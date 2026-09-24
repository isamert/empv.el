;;; empv-audiobookshelf-tests.el --- Tests for the Audiobookshelf backend -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L . -l empv.el -l test/empv-audiobookshelf-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'empv)

(setq empv-display-events nil)

(defmacro empv-abs-test--with-request (responses &rest body)
  "Run BODY with `empv--request' mocked.
RESPONSES is an alist of (URL-REGEXP . RESPONSE).  Every request whose
full URL matches a regexp gets that RESPONSE (a parsed-JSON alist);
callbacks are invoked synchronously.  Requests made are collected into
the variable `requests' (a list of (URL PARAMS HEADERS METHOD DATA)),
available inside BODY."
  (declare (indent 1))
  `(let ((requests '()))
     (cl-letf (((symbol-function 'empv--request)
                (lambda (url &optional params callback)
                  (push (list url params empv--request-headers
                              (bound-and-true-p url-request-method)
                              (bound-and-true-p url-request-data))
                        requests)
                  (let ((response (cdr (seq-find (lambda (it) (string-match-p (car it) url))
                                                 ,responses))))
                    (if callback (funcall callback response) response)))))
       ,@body)))

;;;; Parser

(ert-deftest empv-abs-read-result-null-is-nil ()
  ;; JSON null used to come back as the raw response string (`:null-object
  ;; result'), which broke every `(or .field default)' guard.
  (let ((parsed (empv--read-result "{\"a\":null,\"b\":[null,1],\"c\":false}")))
    (should (null (alist-get 'a parsed)))
    (should (equal (alist-get 'b parsed) '(nil 1)))
    (should (eq (alist-get 'c parsed) :json-false))))

;;;; Task 1: config, request, builders

(ert-deftest empv-abs-request-requires-config ()
  (let ((empv-audiobookshelf-url nil) (empv-audiobookshelf-api-key nil))
    (should-error (empv--audiobookshelf-request "/api/libraries") :type 'user-error)))

(ert-deftest empv-abs-request-builds-url-and-bearer ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY"))
    (empv-abs-test--with-request '(("/api/libraries" . ((libraries . ()))))
      (let ((result (empv--audiobookshelf-request "/api/libraries" :limit 5 :sort "media.metadata.title")))
        (should (equal result '((libraries . ()))))
        (pcase-let ((`(,url ,params ,headers ,_ ,_) (car requests)))
          (should (equal url "https://books.example.com/api/libraries"))
          (should (equal params '((limit . "5") (sort . "media.metadata.title"))))
          (should (equal (cdr (assoc "Authorization" headers)) "Bearer KEY")))))))

(ert-deftest empv-abs-request-async-calls-callback ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (got nil))
    (empv-abs-test--with-request '(("/api/me" . ((id . "u1"))))
      (empv--audiobookshelf-request "/api/me" (lambda (r) (setq got r)))
      (should (equal got '((id . "u1")))))))

(ert-deftest empv-abs-request-json-sets-method-and-body ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY"))
    (empv-abs-test--with-request '(("/api/me/progress/i1" . nil))
      (empv--audiobookshelf-request-json "PATCH" "/api/me/progress/i1" '((currentTime . 12.5)) #'ignore)
      (pcase-let ((`(,url ,_ ,headers ,method ,data) (car requests)))
        (should (equal url "https://books.example.com/api/me/progress/i1"))
        (should (equal method "PATCH"))
        (should (equal (cdr (assoc "Content-Type" headers)) "application/json"))
        (should (equal (cdr (assoc "Authorization" headers)) "Bearer KEY"))
        (should (string-match-p "\"currentTime\":12.5" data))))))

(ert-deftest empv-abs-file-url ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "K/1"))
    (should (equal (empv--audiobookshelf-file-url "/api/items/i1/file/42")
                   "https://books.example.com/api/items/i1/file/42?token=K%2F1"))))

(ert-deftest empv-abs-edl-single-and-multi ()
  (should (equal (empv--audiobookshelf-edl '("https://x/a")) "https://x/a"))
  (should (equal (empv--audiobookshelf-edl '("https://x/a" "https://x/bb"))
                 "edl://!no_chapters;%11%https://x/a;%12%https://x/bb")))

(ert-deftest empv-abs-chapters-file ()
  (should (null (empv--audiobookshelf-write-chapters-file nil "b1")))
  (let ((file (empv--audiobookshelf-write-chapters-file
               '(((id . 0) (start . 0) (end . 2.5) (title . "Intro"))
                 ((id . 1) (start . 2.5) (end . 6) (title . "One; two=three"))
                 ((id . 2) (start . 6) (end . 9) (title . "Poglavje č")))
               "b1")))
    (unwind-protect
        (with-temp-buffer
          (should (string-suffix-p "empv-audiobookshelf-chapters-b1.txt" file))
          (let ((coding-system-for-read 'utf-8))
            (insert-file-contents file))
          (should (equal (buffer-string)
                         (concat ";FFMETADATA1\n"
                                 "[CHAPTER]\nTIMEBASE=1/1000\nSTART=0\nEND=2500\ntitle=Intro\n"
                                 "[CHAPTER]\nTIMEBASE=1/1000\nSTART=2500\nEND=6000\ntitle=One\\; two\\=three\n"
                                 "[CHAPTER]\nTIMEBASE=1/1000\nSTART=6000\nEND=9000\ntitle=Poglavje č\n"))))
      (delete-file file))))

;;;; Task 2: normalization & formatting

(defconst empv-abs-test--book
  '((id . "b1") (libraryId . "l1") (mediaType . "book")
    (media . ((metadata . ((title . "Dune") (authorName . "Frank Herbert")))
              (numTracks . 3) (duration . 7200.5)))))

(defconst empv-abs-test--podcast
  '((id . "p1") (libraryId . "l2") (mediaType . "podcast")
    (media . ((metadata . ((title . "Lex") (author . "Lex F")))
              (numEpisodes . 12)))))

(defconst empv-abs-test--episode
  '((id . "e1") (libraryItemId . "p1") (title . "Ep 1") (publishedAt . 1700000000000)
    (duration . 3600) (audioTrack . ((contentUrl . "/api/items/p1/file/7")))))

(ert-deftest empv-abs-normalize-library ()
  (let ((lib (empv--audiobookshelf-normalize-library '((id . "l1") (name . "Books") (mediaType . "book")))))
    (should (equal (alist-get 'kind lib) 'library))
    (should (equal (alist-get 'type lib) 'audiobookshelf))
    (should (equal (alist-get 'title lib) "Books"))
    (should (equal (alist-get 'mediaType lib) "book"))))

(ert-deftest empv-abs-normalize-book-and-podcast ()
  (let ((book (empv--audiobookshelf-normalize-item empv-abs-test--book))
        (podcast (empv--audiobookshelf-normalize-item empv-abs-test--podcast)))
    (should (equal (alist-get 'kind book) 'book))
    (should (equal (alist-get 'author book) "Frank Herbert"))
    (should (equal (alist-get 'duration book) 7200.5))
    (should (equal (alist-get 'count book) 3))
    (should (equal (alist-get 'kind podcast) 'podcast))
    (should (equal (alist-get 'author podcast) "Lex F"))
    (should (equal (alist-get 'count podcast) 12))))

(ert-deftest empv-abs-normalize-episode ()
  (let ((ep (empv--audiobookshelf-normalize-episode
             empv-abs-test--episode
             (empv--audiobookshelf-normalize-item empv-abs-test--podcast))))
    (should (equal (alist-get 'kind ep) 'episode))
    (should (equal (alist-get 'id ep) "e1"))
    (should (equal (alist-get 'itemId ep) "p1"))
    (should (equal (alist-get 'author ep) "Lex"))
    (should (equal (alist-get 'contentUrl ep) "/api/items/p1/file/7"))))

(ert-deftest empv-abs-normalize-shelf-entity ()
  ;; A podcast shelf entity is the podcast item plus `recentEpisode'.
  (let ((ep (empv--audiobookshelf-normalize-shelf-entity
             (append empv-abs-test--podcast `((recentEpisode . ,empv-abs-test--episode)))))
        (book (empv--audiobookshelf-normalize-shelf-entity empv-abs-test--book)))
    (should (equal (alist-get 'kind ep) 'episode))
    (should (equal (alist-get 'itemId ep) "p1"))
    (should (equal (alist-get 'kind book) 'book))))

(ert-deftest empv-abs-playable-p ()
  ;; Ebook-only books report zero tracks in their minified form.
  (should-not (empv--audiobookshelf-playable-p '((kind . book) (count . 0))))
  (should (empv--audiobookshelf-playable-p '((kind . book) (count . 3))))
  (should (empv--audiobookshelf-playable-p '((kind . book) (count . nil))))
  (should (empv--audiobookshelf-playable-p '((kind . podcast) (count . 0))))
  (should (empv--audiobookshelf-playable-p '((kind . episode) (id . "e1")))))

(ert-deftest empv-abs-format-duration ()
  (should (equal (empv--audiobookshelf-format-duration 7200.5) "2h 00m"))
  (should (equal (empv--audiobookshelf-format-duration 125) "2m"))
  (should (equal (empv--audiobookshelf-format-duration nil) "?")))

(ert-deftest empv-abs-format-candidate ()
  (let* ((book (empv--audiobookshelf-normalize-item empv-abs-test--book))
         (cand (empv--audiobookshelf-format-candidate book)))
    (should (string-match-p "Frank Herbert" cand))
    (should (string-match-p "Dune" cand))
    (should (string-match-p "2h 00m" cand))
    (should (equal (empv--get-text-property cand :item) book))))

(ert-deftest empv-abs-format-candidate-episode-not-downloaded ()
  (let* ((podcast (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
         (downloaded (empv--audiobookshelf-format-candidate
                      (empv--audiobookshelf-normalize-episode empv-abs-test--episode podcast)))
         (missing (empv--audiobookshelf-format-candidate
                   (empv--audiobookshelf-normalize-episode
                    '((id . "e9") (libraryItemId . "p1") (title . "No file") (duration . 60))
                    podcast))))
    (should-not (string-match-p "not downloaded" downloaded))
    (should (string-match-p "No file" missing))
    (should (string-match-p "not downloaded" missing))))

;;;; Task 3: playback

(defconst empv-abs-test--expanded-book
  '((id . "b1") (libraryId . "l1") (mediaType . "book")
    (media . ((metadata . ((title . "Dune") (authorName . "Frank Herbert")))
              (numTracks . 2) (duration . 100.0)
              (tracks . (((index . 1) (startOffset . 0) (duration . 60.0) (contentUrl . "/api/items/b1/file/1"))
                         ((index . 2) (startOffset . 60.0) (duration . 40.0) (contentUrl . "/api/items/b1/file/2"))))
              (chapters . (((id . 0) (start . 0) (end . 50.0) (title . "One"))
                           ((id . 1) (start . 50.0) (end . 100.0) (title . "Two"))))))
    (userMediaProgress . ((currentTime . 42.5) (isFinished . :json-false)))))

(defconst empv-abs-test--expanded-podcast
  '((id . "p1") (libraryId . "l2") (mediaType . "podcast")
    (media . ((metadata . ((title . "Lex") (author . "Lex F")))
              (episodes . (((id . "e1") (libraryItemId . "p1") (title . "Ep 1") (publishedAt . 1700000000000)
                            (duration . 3600) (audioTrack . ((contentUrl . "/api/items/p1/file/7"))))
                           ((id . "e2") (libraryItemId . "p1") (title . "Ep 2") (publishedAt . 1700000001000)
                            (duration . 10))))))))

(ert-deftest empv-abs-progress-start ()
  (should (equal (empv--audiobookshelf-progress-start nil) 0))
  (should (equal (empv--audiobookshelf-progress-start '((currentTime . 12.5) (isFinished . :json-false))) 12.5))
  (should (equal (empv--audiobookshelf-progress-start '((currentTime . 12.5) (isFinished . t))) 0)))

(ert-deftest empv-abs-extract-url-roundtrip ()
  (let* ((empv-audiobookshelf-url "https://books.example.com")
         (empv-audiobookshelf-api-key "KEY")
         (ep (empv--audiobookshelf-normalize-episode
              empv-abs-test--episode
              (empv--audiobookshelf-normalize-item empv-abs-test--podcast)))
         (uri (empv--audiobookshelf-item-extract-url ep '("https://books.example.com/api/items/p1/file/7?token=KEY")))
         (info (empv--extract-empv-metadata-from-path uri)))
    (should (equal (plist-get info :uri) "https://books.example.com/api/items/p1/file/7?token=KEY"))
    (should (eq (plist-get info :audiobookshelf) t))
    (should (equal (plist-get info :id) "p1"))
    (should (equal (plist-get info :episodeId) "e1"))
    (should (equal (plist-get info :duration) 3600))
    (should (string-match-p "Ep 1" (plist-get info :title)))
    ;; A newline in the title must not end the magic-info comment early.
    (let* ((ep (empv--audiobookshelf-normalize-episode
                (append '((title . "Line one\nline two")) empv-abs-test--episode)
                (empv--audiobookshelf-normalize-item empv-abs-test--podcast)))
           (uri (empv--audiobookshelf-item-extract-url ep '("https://books.example.com/api/items/p1/file/7?token=KEY")))
           (info (empv--extract-empv-metadata-from-path uri)))
      (should (equal (plist-get info :id) "p1"))
      (should (string-match-p "Line one line two" (plist-get info :title)))
      (should-not (string-match-p "\n" (plist-get info :title))))))

(ert-deftest empv-abs-resolve-book ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (playable nil))
    (empv-abs-test--with-request `(("/api/items/b1" . ,empv-abs-test--expanded-book))
      (empv--audiobookshelf-resolve (empv--audiobookshelf-normalize-item empv-abs-test--book)
                                    (lambda (p) (setq playable p)))
      (should requests)
      (should (equal (nth 1 (car requests)) '((expanded . "1") (include . "progress"))))
      (should (equal (plist-get playable :start) 42.5))
      (should (string-prefix-p "edl://!no_chapters;%55%https://books.example.com/api/items/b1/file/1?token=KEY;%55%https://books.example.com/api/items/b1/file/2?token=KEY##"
                               (plist-get playable :uri)))
      (let ((file (plist-get playable :chapters-file)))
        (should (file-exists-p file))
        (delete-file file)))))

(ert-deftest empv-abs-resolve-episode ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (playable nil))
    (empv-abs-test--with-request
        `(("/api/items/p1" . ,(append empv-abs-test--expanded-podcast
                                      '((userMediaProgress . ((currentTime . 7.0) (isFinished . :json-false)))))))
      (empv--audiobookshelf-resolve
       (empv--audiobookshelf-normalize-episode empv-abs-test--episode
                                               (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
       (lambda (p) (setq playable p)))
      (should (equal (length requests) 1))
      (should (equal (nth 0 (car requests)) "https://books.example.com/api/items/p1"))
      (should (equal (nth 1 (car requests)) '((expanded . "1") (include . "progress") (episode . "e1"))))
      (should (equal (plist-get playable :start) 7.0))
      (should (null (plist-get playable :chapters-file)))
      (should (string-prefix-p "https://books.example.com/api/items/p1/file/7?token=KEY##" (plist-get playable :uri))))))

(ert-deftest empv-abs-resolve-episode-without-progress ()
  ;; Regression: the per-item progress endpoint 404s for a never-played
  ;; episode, and `empv--request' never calls back on HTTP errors, so
  ;; playback silently never started.  The item endpoint with `episode'
  ;; just omits `userMediaProgress' instead.
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (playable nil))
    (empv-abs-test--with-request `(("/api/items/p1" . ,empv-abs-test--expanded-podcast))
      (empv--audiobookshelf-resolve
       (empv--audiobookshelf-normalize-episode empv-abs-test--episode
                                               (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
       (lambda (p) (setq playable p)))
      (should (equal (nth 0 (car requests)) "https://books.example.com/api/items/p1"))
      (should (equal (plist-get playable :start) 0))
      (should (null (plist-get playable :chapters-file)))
      (should (string-prefix-p "https://books.example.com/api/items/p1/file/7?token=KEY##" (plist-get playable :uri))))))

(ert-deftest empv-abs-resolve-episode-not-downloaded ()
  ;; Episode e2 of the expanded podcast has no `audioTrack'.
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY"))
    (empv-abs-test--with-request `(("/api/items/p1" . ,empv-abs-test--expanded-podcast))
      (should-error
       (empv--audiobookshelf-resolve
        (empv--audiobookshelf-normalize-episode '((id . "e2") (libraryItemId . "p1") (title . "Ep 2"))
                                                (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
        #'ignore)
       :type 'user-error))))

(ert-deftest empv-abs-resolve-episode-refreshes-content-url ()
  ;; A bookmark made before the episode was downloaded has no
  ;; `contentUrl'; the fresh item response supplies it.
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (playable nil))
    (empv-abs-test--with-request `(("/api/items/p1" . ,empv-abs-test--expanded-podcast))
      (empv--audiobookshelf-resolve
       (empv--audiobookshelf-normalize-episode '((id . "e1") (libraryItemId . "p1") (title . "Ep 1"))
                                               (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
       (lambda (p) (setq playable p)))
      (should (string-prefix-p "https://books.example.com/api/items/p1/file/7?token=KEY##" (plist-get playable :uri))))))

(ert-deftest empv-abs-resolve-book-without-audio ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (item (copy-tree empv-abs-test--expanded-book)))
    (setf (alist-get 'tracks (alist-get 'media item)) nil)
    (empv-abs-test--with-request `(("/api/items/b1" . ,item))
      (should-error
       (empv--audiobookshelf-resolve (empv--audiobookshelf-normalize-item empv-abs-test--book) #'ignore)
       :type 'user-error))))

(ert-deftest empv-abs-load-sends-loadfile-with-options ()
  (let ((sent '()))
    (cl-letf (((symbol-function 'empv--running?) (lambda () t))
              ((symbol-function 'empv--send-command)
               (lambda (command &optional callback _event?)
                 (push command sent)
                 (when callback (funcall callback 3)))))
      (empv--audiobookshelf-load '(:uri "https://x/a##(:title \"T\" :audiobookshelf t)" :start 12.5 :chapters-file "/tmp/ch.txt") 'enqueue)
      (should (equal (car sent) '(loadfile "https://x/a##(:title \"T\" :audiobookshelf t)" "append-play" -1 "start=12.5,chapters-file=/tmp/ch.txt")))
      (setq sent '())
      (empv--audiobookshelf-load '(:uri "https://x/a##(:title \"T\" :audiobookshelf t)" :start 0 :chapters-file nil) 'play)
      (should (equal (car (last sent)) '(loadfile "https://x/a##(:title \"T\" :audiobookshelf t)" "append" -1 "start=0"))))))

;;;; Task 4: browsing

(ert-deftest empv-abs-normalize-episode-recent-payload ()
  ;; `recent-episodes' embeds the podcast media object, not a library item.
  (let ((ep (empv--audiobookshelf-normalize-episode
             (append empv-abs-test--episode
                     '((podcast . ((id . "m1") (metadata . ((title . "Lex"))) (numEpisodes . 12)))))
             nil)))
    (should (equal (alist-get 'author ep) "Lex"))
    (should (equal (alist-get 'itemId ep) "p1"))))

(ert-deftest empv-abs-normalize-items-response ()
  (let ((items (empv--audiobookshelf-normalize-items-response
                `((results . (,empv-abs-test--book ,empv-abs-test--podcast)) (total . 2)))))
    (should (equal (mapcar (lambda (it) (alist-get 'kind it)) items) '(book podcast)))))

(defconst empv-abs-test--ebook
  '((id . "eb1") (libraryId . "l1") (mediaType . "book")
    (media . ((metadata . ((title . "Paper only") (authorName . "Someone")))
              (numTracks . 0) (duration . 0)))))

(ert-deftest empv-abs-normalize-items-response-drops-ebooks ()
  (let ((items (empv--audiobookshelf-normalize-items-response
                `((results . (,empv-abs-test--book ,empv-abs-test--ebook ,empv-abs-test--podcast)) (total . 3)))))
    (should (equal (mapcar (lambda (it) (alist-get 'id it)) items) '("b1" "p1")))))

(ert-deftest empv-abs-normalize-search-response ()
  (let ((items (empv--audiobookshelf-normalize-search-response
                `((book . (((libraryItem . ,empv-abs-test--book) (matchKey . "title"))
                           ((libraryItem . ,empv-abs-test--ebook) (matchKey . "title"))))
                  (podcast . (((libraryItem . ,empv-abs-test--podcast))))))))
    (should (equal (mapcar (lambda (it) (alist-get 'id it)) items) '("b1" "p1")))))

(ert-deftest empv-abs-normalize-episodes-newest-first ()
  (let* ((old '((id . "e-old") (libraryItemId . "p1") (title . "Old") (publishedAt . 1)
                (audioTrack . ((contentUrl . "/api/items/p1/file/1")))))
         (new '((id . "e-new") (libraryItemId . "p1") (title . "New") (publishedAt . 2)
                (audioTrack . ((contentUrl . "/api/items/p1/file/2")))))
         (item `((id . "p1") (mediaType . "podcast") (media . ((metadata . ((title . "Lex"))) (episodes . (,old ,new))))))
         (episodes (empv--audiobookshelf-normalize-episodes item (empv--audiobookshelf-normalize-item empv-abs-test--podcast))))
    (should (equal (mapcar (lambda (it) (alist-get 'id it)) episodes) '("e-new" "e-old")))))

(ert-deftest empv-abs-normalize-episodes-null-published-at ()
  (let* ((item (empv--read-result
                "{\"id\":\"p1\",\"mediaType\":\"podcast\",\"media\":{\"metadata\":{\"title\":\"Lex\"},\"episodes\":[{\"id\":\"e1\",\"libraryItemId\":\"p1\",\"title\":\"A\",\"publishedAt\":null,\"audioTrack\":{\"contentUrl\":\"/api/items/p1/file/1\"}},{\"id\":\"e2\",\"libraryItemId\":\"p1\",\"title\":\"B\",\"publishedAt\":5,\"audioTrack\":{\"contentUrl\":\"/api/items/p1/file/2\"}}]}}"))
         (episodes (empv--audiobookshelf-normalize-episodes item (empv--audiobookshelf-normalize-item empv-abs-test--podcast))))
    (should (equal (mapcar (lambda (it) (alist-get 'id it)) episodes) '("e2" "e1")))))

(ert-deftest empv-abs-act-on-library-requests-items ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (shown nil))
    (cl-letf (((symbol-function 'empv--audiobookshelf-result-handler)
               (lambda (_prompt &optional _on-quit) (lambda (items) (setq shown items)))))
      (empv-abs-test--with-request `(("/api/libraries/l1/items" . ((results . (,empv-abs-test--book)))))
        (empv--audiobookshelf-act-on-candidate
         (empv--audiobookshelf-normalize-library '((id . "l1") (name . "Books") (mediaType . "book"))))
        (should (equal (car (car requests)) "https://books.example.com/api/libraries/l1/items"))
        (should (equal (nth 1 (car requests)) '((limit . "0") (sort . "media.metadata.title") (minified . "1"))))
        (should (equal (alist-get 'kind (car shown)) 'book))))))

(ert-deftest empv-abs-act-on-podcast-requests-episodes ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (shown nil))
    (cl-letf (((symbol-function 'empv--audiobookshelf-result-handler)
               (lambda (_prompt &optional _on-quit) (lambda (items) (setq shown items)))))
      (empv-abs-test--with-request
          `(("/api/items/p1" . ((id . "p1") (mediaType . "podcast")
                                (media . ((metadata . ((title . "Lex"))) (episodes . (,empv-abs-test--episode)))))))
        (empv--audiobookshelf-act-on-candidate (empv--audiobookshelf-normalize-item empv-abs-test--podcast))
        (should (equal (nth 1 (car requests)) '((expanded . "1"))))
        (should (equal (alist-get 'kind (car shown)) 'episode))
        (should (equal (alist-get 'author (car shown)) "Lex"))))))

(ert-deftest empv-abs-act-on-book-resolves-and-asks ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (asked nil))
    (cl-letf (((symbol-function 'empv--audiobookshelf-play-or-enqueue) (lambda (p) (setq asked p))))
      (empv-abs-test--with-request `(("/api/items/b1" . ,empv-abs-test--expanded-book))
        (empv--audiobookshelf-act-on-candidate (empv--audiobookshelf-normalize-item empv-abs-test--book))
        (should (equal (plist-get asked :start) 42.5))
        (when-let* ((f (plist-get asked :chapters-file))) (delete-file f))))))

;;;; Task 5: progress sync

(defconst empv-abs-test--book-info '(:title "Dune" :kind book :audiobookshelf t :id "b1" :episodeId nil :duration 100.0 :uri "x"))
(defconst empv-abs-test--episode-info '(:title "Ep" :kind episode :audiobookshelf t :id "p1" :episodeId "e1" :duration 3600 :uri "y"))

(ert-deftest empv-abs-send-progress-endpoints ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY"))
    (empv-abs-test--with-request '(("." . nil))
      (empv--audiobookshelf-send-progress empv-abs-test--book-info 42.0)
      (empv--audiobookshelf-send-progress empv-abs-test--episode-info 7.0)
      (should (equal (car (nth 1 requests)) "https://books.example.com/api/me/progress/b1"))
      (should (equal (car (nth 0 requests)) "https://books.example.com/api/me/progress/p1/e1"))
      (should (equal (nth 3 (nth 0 requests)) "PATCH"))
      (should (string-match-p "\"currentTime\":7.0" (nth 4 (nth 0 requests))))
      (should (string-match-p "\"duration\":3600" (nth 4 (nth 0 requests)))))))

(ert-deftest empv-abs-send-progress-ignores-nil ()
  (empv-abs-test--with-request '(("." . nil))
    (empv--audiobookshelf-send-progress nil 1.0)
    (empv--audiobookshelf-send-progress empv-abs-test--book-info nil)
    (should (null requests))))

(ert-deftest empv-abs-path-change-tracks-and-flushes ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (empv--audiobookshelf-current nil)
        (empv--audiobookshelf-last-time-pos nil)
        (empv--audiobookshelf-sync-timer nil))
    (cl-letf (((symbol-function 'empv--running?) (lambda () t)))
      (empv-abs-test--with-request '(("." . nil))
        (unwind-protect
            (progn
              ;; A non-Audiobookshelf path: nothing tracked, nothing sent.
              (empv--audiobookshelf-handle-path-change "https://radio.example/stream##(:title \"r\" :radio t)")
              (should (null empv--audiobookshelf-current))
              (should (null requests))
              ;; An Audiobookshelf path starts tracking and the timer.
              (empv--audiobookshelf-handle-path-change (apply #'empv--url-with-magic-info "https://x/a" empv-abs-test--book-info))
              (should (equal (plist-get empv--audiobookshelf-current :id) "b1"))
              (should (timerp empv--audiobookshelf-sync-timer))
              (setq empv--audiobookshelf-last-time-pos 33.0)
              ;; Switching item flushes the previous position and stops the timer.
              (empv--audiobookshelf-handle-path-change nil)
              (should (equal (car (car requests)) "https://books.example.com/api/me/progress/b1"))
              (should (string-match-p "\"currentTime\":33.0" (nth 4 (car requests))))
              (should (null empv--audiobookshelf-current))
              (should (null empv--audiobookshelf-sync-timer)))
          (empv--audiobookshelf-stop-timer))))))

(ert-deftest empv-abs-sync-now-queries-mpv ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (empv--audiobookshelf-current empv-abs-test--book-info)
        (empv--audiobookshelf-last-time-pos nil))
    (cl-letf (((symbol-function 'empv--running?) (lambda () t))
              ((symbol-function 'empv--send-command)
               (lambda (command &optional callback _event?)
                 (should (equal command '(get_property time-pos)))
                 (when callback (funcall callback 55.5)))))
      (empv-abs-test--with-request '(("." . nil))
        (empv--audiobookshelf-sync-now)
        (should (equal empv--audiobookshelf-last-time-pos 55.5))
        (should (string-match-p "\"currentTime\":55.5" (nth 4 (car requests))))
        ;; Same position again (paused): nothing new to report.
        (empv--audiobookshelf-sync-now)
        (should (equal (length requests) 1))))))

(ert-deftest empv-abs-end-file-marks-finished ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (empv--audiobookshelf-current empv-abs-test--book-info)
        (empv--audiobookshelf-last-time-pos 90.0))
    (empv-abs-test--with-request '(("." . nil))
      ;; A stop or playlist switch is not a finish.
      (empv--audiobookshelf-handle-end-file '((event . "end-file") (reason . "stop")))
      (should (null requests))
      (empv--audiobookshelf-handle-end-file '((event . "end-file") (reason . "eof")))
      (should (equal (car (car requests)) "https://books.example.com/api/me/progress/b1"))
      (should (string-match-p "\"currentTime\":100.0" (nth 4 (car requests))))
      (should (string-match-p "\"isFinished\":true" (nth 4 (car requests))))
      (should (null empv--audiobookshelf-last-time-pos)))))

(ert-deftest empv-abs-pause-flushes ()
  (let ((flushed 0))
    (cl-letf (((symbol-function 'empv--audiobookshelf-sync-now) (lambda () (cl-incf flushed))))
      (let ((empv--audiobookshelf-current empv-abs-test--book-info))
        (empv--audiobookshelf-handle-pause t)
        (empv--audiobookshelf-handle-pause :json-false)
        (should (equal flushed 1)))
      (let ((empv--audiobookshelf-current nil))
        (empv--audiobookshelf-handle-pause t)
        (should (equal flushed 1))))))

(ert-deftest empv-abs-exit-flushes-progress ()
  (let ((empv-audiobookshelf-url "https://books.example.com")
        (empv-audiobookshelf-api-key "KEY")
        (empv--audiobookshelf-current empv-abs-test--book-info)
        (empv--audiobookshelf-last-time-pos 33.0)
        (empv--audiobookshelf-sync-timer (run-with-timer 3600 nil #'ignore))
        (empv--process nil)
        (empv--network-process nil)
        (empv--ivjs-process nil))
    (empv-abs-test--with-request '(("." . nil))
      (empv-exit)
      (should (equal (car (car requests)) "https://books.example.com/api/me/progress/b1"))
      (should (string-match-p "\"currentTime\":33.0" (nth 4 (car requests))))
      (should (null empv--audiobookshelf-current))
      (should (null empv--audiobookshelf-sync-timer)))))

;;;; Task 6: embark & bookmarks

(ert-deftest empv-abs-bookmark-record ()
  (let ((created nil))
    (cl-letf (((symbol-function 'empv--create-bookmark)
               (lambda (default props) (setq created (cons default props)))))
      (empv-audiobookshelf-bookmark-set
       (empv--audiobookshelf-normalize-episode empv-abs-test--episode
                                               (empv--audiobookshelf-normalize-item empv-abs-test--podcast)))
      (should (equal (car created) "Ep 1"))
      (should (equal (alist-get 'type (cdr created)) 'audiobookshelf))
      (should (equal (alist-get 'kind (cdr created)) 'episode))
      (should (equal (alist-get 'itemId (cdr created)) "p1"))
      (should (equal (alist-get 'contentUrl (cdr created)) "/api/items/p1/file/7")))))

(ert-deftest empv-abs-bookmark-jump-dispatches ()
  (let ((acted nil))
    (cl-letf (((symbol-function 'empv--audiobookshelf-act-on-candidate) (lambda (record &optional _) (setq acted record))))
      (empv-bookmark-jump '("Dune" (type . audiobookshelf) (kind . book) (id . "b1") (title . "Dune")
                            (handler . empv-bookmark-jump)))
      (should (equal (alist-get 'id acted) "b1")))))

(ert-deftest empv-abs-embark-transformer ()
  (let* ((item (empv--audiobookshelf-normalize-item empv-abs-test--book))
         (cand (empv--audiobookshelf-format-candidate item)))
    (should (equal (empv--embark-audiobookshelf-item-transformer 'empv-audiobookshelf-item cand)
                   (cons 'empv-audiobookshelf-item item)))))

(ert-deftest empv-abs-embark-keymap-registered ()
  (require 'embark)
  (should (keymapp empv-audiobookshelf-item-action-map))
  (should (eq (alist-get 'empv-audiobookshelf-item embark-keymap-alist) 'empv-audiobookshelf-item-action-map))
  (should (eq (lookup-key empv-audiobookshelf-item-action-map "p") 'empv-audiobookshelf-play)))

(provide 'empv-audiobookshelf-tests)
;;; empv-audiobookshelf-tests.el ends here
