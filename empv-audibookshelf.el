;;; empv-audibookshelf.el --- Audiobookshelf support for empv -*- lexical-binding: t; -*-

;; ...

;;; Commentary:

;; ...

;;; Code:

(require 'empv)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(defgroup empv-audiobookshelf nil
  "Browse and play media from Audiobookshelf."
  :group 'empv)

(defcustom empv-audiobookshelf-url nil
  "Audiobookshelf server URL, without a trailing slash.
For example: \"https://books.example.com\"."
  :group 'empv-audiobookshelf
  :type '(choice (const :tag "Not configured" nil) string))

(defcustom empv-audiobookshelf-api-key nil
  "Audiobookshelf API key or user access token.
Create one under Settings → API Keys in the Audiobookshelf web UI.  It
is sent as a Bearer token on API calls and as the `token' query
parameter on stream URLs handed to mpv."
  :group 'empv-audiobookshelf
  :type '(choice (const :tag "Not configured" nil) string))

(defcustom empv-audiobookshelf-result-count 50
  "Max number of results fetched per Audiobookshelf search or shelf request."
  :group 'empv-audiobookshelf
  :type 'integer)

(defcustom empv-audiobookshelf-progress-sync-interval 15
  "Seconds between listening-progress updates sent to Audiobookshelf.
Progress is also sent when playback is paused and when the playing item
changes, see `empv-audiobookshelf--handle-path-change'."
  :group 'empv-audiobookshelf
  :type 'integer)

(defvar empv-audiobookshelf--search-history nil)
(defvar empv-audiobookshelf--search-prompt "Audiobookshelf search: ")

;;;; Requests & builders

(defun empv-audiobookshelf--check-config ()
  "Signal a `user-error' unless Audiobookshelf is configured."
  (unless (seq-every-p (lambda (value)
                         (and (stringp value) (not (string-blank-p value))))
                       (list empv-audiobookshelf-url empv-audiobookshelf-api-key))
    (user-error "Please configure `empv-audiobookshelf-url' and `empv-audiobookshelf-api-key' first")))

(defun empv-audiobookshelf--headers (&optional json?)
  "Request headers for Audiobookshelf, with a JSON content type if JSON?."
  (cons (cons "Authorization" (concat "Bearer " empv-audiobookshelf-api-key))
        (when json? '(("Content-Type" . "application/json")))))

(defun empv-audiobookshelf--url (endpoint)
  "Build a URL for Audiobookshelf ENDPOINT, a path starting with a slash."
  (concat (string-remove-suffix "/" empv-audiobookshelf-url) endpoint))

(defun empv-audiobookshelf--wrap-callback (callback)
  "Wrap CALLBACK so it can safely open a minibuffer from a URL callback.
`url-retrieve' invokes callbacks with `inhibit-quit' bound and from a
process filter, so C-g in a `completing-read' would otherwise be
reported as an error."
  (lambda (result)
    (let ((inhibit-quit nil))
      (condition-case nil
          (funcall callback result)
        (quit nil)))))

(defun empv-audiobookshelf--request (endpoint &rest rest)
  "Make a GET request to Audiobookshelf ENDPOINT, like \"/api/libraries\".
REST are URL params as a plist; values are converted to strings and nil
values are dropped.  If the last of REST is a function, the request is
made asynchronously and that function is called with the parsed JSON
response.  Otherwise return the parsed response synchronously.
Request errors are handled by `empv-request'."
  (empv-audiobookshelf--check-config)
  (let* ((last-arg (car (last rest)))
         (callback (and (functionp last-arg) last-arg))
         (params (cl-loop for (key value) on (if callback (butlast rest) rest) by #'cddr
                          when value collect
                          (cons (intern (substring (symbol-name key) 1)) (format "%s" value)))))
    (empv-request (empv-audiobookshelf--url endpoint)
                  :headers (empv-audiobookshelf--headers)
                  :params params
                  :callback (when callback (empv-audiobookshelf--wrap-callback callback)))))

(defun empv-audiobookshelf--request-json (method endpoint data &optional callback)
  "Send DATA (an alist) as a JSON body to Audiobookshelf ENDPOINT with METHOD.
METHOD is an HTTP method string like \"PATCH\".  CALLBACK, if given,
receives the parsed response asynchronously; otherwise the request is
synchronous."
  (empv-audiobookshelf--check-config)
  (empv-request (empv-audiobookshelf--url endpoint)
                :method method
                :data (encode-coding-string (json-serialize data) 'utf-8)
                :headers (empv-audiobookshelf--headers t)
                :callback (when callback (empv-audiobookshelf--wrap-callback callback))))

(defun empv-audiobookshelf--file-url (content-url)
  "Return a streamable URL for CONTENT-URL, an item file path from the API.
CONTENT-URL looks like \"/api/items/ID/file/INO\"; the API key is
appended as the `token' query parameter so that each entry works
stand-alone inside an `edl://' URI."
  (format "%s%stoken=%s"
          (empv-audiobookshelf--url content-url)
          (if (string-search "?" content-url) "&" "?")
          (url-hexify-string empv-audiobookshelf-api-key)))

(defun empv-audiobookshelf--edl (urls)
  "Return one mpv URI that plays URLS back to back.
A single URL is returned as is.  Several URLs are joined into an
`edl://' URI so that mpv sees one continuous file whose timeline matches
the whole-book timeline Audiobookshelf uses for chapters and progress.
Each entry is length-prefixed (%N%) so URL characters need no escaping.
The `!no_chapters' header keeps mpv from making one chapter per segment
titled with the segment URL, which would expose the API token in the
chapter list."
  (if (cdr urls)
      (concat "edl://!no_chapters;"
              (mapconcat (lambda (url) (format "%%%d%%%s" (string-bytes url) url)) urls ";"))
    (car urls)))

(defvar empv-audiobookshelf--chapters-directory nil
  "Private directory holding chapter files for this Emacs session.")

(defun empv-audiobookshelf--cleanup-chapters ()
  "Remove the temporary directory used for Audiobookshelf chapters."
  (when empv-audiobookshelf--chapters-directory
    (delete-directory empv-audiobookshelf--chapters-directory t)
    (setq empv-audiobookshelf--chapters-directory nil)))

(add-hook 'kill-emacs-hook #'empv-audiobookshelf--cleanup-chapters)

(defun empv-audiobookshelf--write-chapters-file (chapters id)
  "Write CHAPTERS of the item ID to an ffmetadata file and return its path.
CHAPTERS is a list of alists with `start', `end' (seconds) and `title',
as found in an item's `media.chapters'.  Return nil when CHAPTERS is
empty.  The file lives in a private temporary directory under a name
derived from ID, so playing an item again overwrites its own file
rather than leaving a new one behind.  It is handed to mpv through its
`chapters-file' option so that `empv-chapter-select' and friends see
the server-side chapters."
  (when chapters
    (unless empv-audiobookshelf--chapters-directory
      (setq empv-audiobookshelf--chapters-directory
            (make-temp-file "empv-audiobookshelf-" t)))
    (let ((file (expand-file-name (concat (secure-hash 'sha256 id) ".ffmeta")
                                  empv-audiobookshelf--chapters-directory))
          (coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert ";FFMETADATA1\n")
        (dolist (chapter chapters)
          (let-alist chapter
            (insert (format "[CHAPTER]\nTIMEBASE=1/1000\nSTART=%d\nEND=%d\ntitle=%s\n"
                            (round (* 1000 .start))
                            (round (* 1000 .end))
                            (thread-last (or .title "")
                                         (replace-regexp-in-string "[\n\r]" " ")
                                         (replace-regexp-in-string "\\([=;#\\\\]\\)" "\\\\\\1")))))))
      file)))

;;;; Items

(defun empv-audiobookshelf--normalize-library (library)
  "Turn an Audiobookshelf LIBRARY object into an empv item."
  (let-alist library
    `((type . audiobookshelf)
      (kind . library)
      (id . ,.id)
      (title . ,.name)
      (mediaType . ,.mediaType))))

(defun empv-audiobookshelf--normalize-item (item)
  "Turn an Audiobookshelf library ITEM (minified or expanded) into an empv item.
Books become `book' items and podcasts `podcast' items."
  (let-alist item
    `((type . audiobookshelf)
      (kind . ,(if (equal .mediaType "podcast") 'podcast 'book))
      (id . ,.id)
      (libraryId . ,.libraryId)
      (title . ,.media.metadata.title)
      (author . ,(or .media.metadata.authorName .media.metadata.author))
      (duration . ,.media.duration)
      (count . ,(or .media.numTracks .media.numEpisodes)))))

(defun empv-audiobookshelf--normalize-episode (episode podcast)
  "Turn an Audiobookshelf podcast EPISODE into an empv `episode' item.
PODCAST is the normalized podcast item the episode belongs to, or nil
when EPISODE carries its own `podcast' object (as `recent-episodes'
results do)."
  (let-alist episode
    `((type . audiobookshelf)
      (kind . episode)
      (id . ,.id)
      (itemId . ,(or .libraryItemId (alist-get 'id podcast)))
      (title . ,.title)
      (author . ,(or (alist-get 'title podcast) .podcast.metadata.title .podcast.media.metadata.title))
      (duration . ,.duration)
      (publishedAt . ,.publishedAt)
      (contentUrl . ,.audioTrack.contentUrl))))

(defun empv-audiobookshelf--playable-p (item)
  "Non-nil unless ITEM is a book known to have no audio files.
Book libraries also hold ebook-only items; their minified form reports
zero tracks, and there is nothing mpv could play.  Items whose track
count is unknown (expanded items do not carry it) are kept."
  (not (and (eq (alist-get 'kind item) 'book)
            (eql (alist-get 'count item) 0))))

(defun empv-audiobookshelf--normalize-shelf-entity (entity)
  "Normalize ENTITY from a personalized shelf.
Podcast entities carry a `recentEpisode'; those become `episode' items,
everything else goes through `empv-audiobookshelf--normalize-item'."
  (let ((item (empv-audiobookshelf--normalize-item entity)))
    (if-let* ((episode (alist-get 'recentEpisode entity)))
        (empv-audiobookshelf--normalize-episode episode item)
      item)))

(defun empv-audiobookshelf--format-duration (seconds)
  "Format SECONDS as \"2h 05m\" or \"42m\"; \"?\" when SECONDS is nil."
  (if (not (numberp seconds))
      "?"
    (let* ((minutes (floor (/ seconds 60)))
           (hours (floor (/ minutes 60))))
      (if (> hours 0)
          (format "%dh %02dm" hours (% minutes 60))
        (format "%dm" minutes)))))

(defun empv-audiobookshelf--format-candidate (cand)
  "Format Audiobookshelf item CAND for `completing-read'."
  (propertize
   (let-alist cand
     (pcase .kind
       ('library (format "%s %s"
                         (propertize .title 'face 'bold)
                         (propertize (format "[%s]" .mediaType) 'face 'italic)))
       ('book (format "%s - %s %s"
                      (propertize (or .author "?") 'face 'italic)
                      (propertize (or .title "?") 'face 'bold)
                      (propertize (format "[%s]" (empv-audiobookshelf--format-duration .duration)) 'face 'italic)))
       ('podcast (format "%s %s"
                         (propertize (or .title "?") 'face 'bold)
                         (propertize (format "[%s episodes]" (or .count 0)) 'face 'italic)))
       ('episode (format "%s - %s %s%s"
                         (propertize (or .author "?") 'face 'italic)
                         (propertize (or .title "?") 'face 'bold)
                         (propertize (format "[%s]" (empv-audiobookshelf--format-duration .duration)) 'face 'italic)
                         (if .contentUrl "" (propertize " [not downloaded]" 'face 'italic))))
       (other (error "empv-audiobookshelf--format-candidate :: No formatter found for: %s" other))))
   'empv-item cand))

;;;; Playback

(defun empv-audiobookshelf--item-extract-url (obj urls)
  "Build the playable URI for Audiobookshelf item OBJ from its track URLS.
The item id, episode id and duration are attached as empv metadata (see
`empv-uri-with-metadata') so progress can be reported later."
  (let ((episode? (eq (alist-get 'kind obj) 'episode)))
    (empv-uri-with-metadata
     ;; Start a new EDL entry so mpv treats the ## metadata as a comment.
     (concat (empv-audiobookshelf--edl urls) (when (cdr urls) ";"))
     :title (replace-regexp-in-string
             "[\n\r]+\\|##" " "
             (or (alist-get 'title obj) "Audiobookshelf"))
     :kind (alist-get 'kind obj)
     :audiobookshelf t
     :id (if episode? (alist-get 'itemId obj) (alist-get 'id obj))
     :episodeId (when episode? (alist-get 'id obj))
     :duration (alist-get 'duration obj))))

(defun empv-audiobookshelf--progress-start (progress)
  "Resume position in seconds from a PROGRESS object.
Return 0 when PROGRESS is nil or the item is marked finished."
  (if (or (null progress) (eq (alist-get 'isFinished progress) t))
      0
    (or (alist-get 'currentTime progress) 0)))

(defun empv-audiobookshelf--resolve (obj callback)
  "Fetch what is needed to play Audiobookshelf item OBJ and call CALLBACK.
CALLBACK receives a playable plist: (:uri URI :start SECONDS
:chapters-file FILE-OR-NIL).  Books and episodes both need one item
request: books for their tracks and chapters, episodes for their audio
file and saved progress.  The episode's progress is requested with the
`episode' parameter, which unlike the per-item progress endpoint does
not 404 when nothing was listened to yet."
  (pcase (alist-get 'kind obj)
    ('book
     (empv-audiobookshelf--request
      (format "/api/items/%s" (alist-get 'id obj))
      :expanded 1 :include "progress"
      (lambda (item)
        (unless item
          (user-error "Audiobookshelf: could not fetch item %s" (alist-get 'id obj)))
        (let-alist item
          (let ((tracks (delq nil (mapcar (lambda (track) (alist-get 'contentUrl track))
                                          .media.tracks))))
            (unless tracks
              (user-error "Audiobookshelf: '%s' has no audio files" (alist-get 'title obj)))
            (funcall
             callback
             (list :uri (empv-audiobookshelf--item-extract-url
                         (cons (cons 'duration (or .media.duration (alist-get 'duration obj))) obj)
                         (mapcar #'empv-audiobookshelf--file-url tracks))
                   :start (empv-audiobookshelf--progress-start .userMediaProgress)
                   :chapters-file (empv-audiobookshelf--write-chapters-file
                                   .media.chapters (alist-get 'id obj)))))))))
    ('episode
     (empv-audiobookshelf--request
      (format "/api/items/%s" (alist-get 'itemId obj))
      :expanded 1 :include "progress" :episode (alist-get 'id obj)
      (lambda (item)
        (unless item
          (user-error "Audiobookshelf: could not fetch item %s" (alist-get 'itemId obj)))
        (let-alist item
          (let* ((episode (seq-find (lambda (ep) (equal (alist-get 'id ep) (alist-get 'id obj)))
                                    .media.episodes))
                 (content-url (if episode
                                  (let-alist episode .audioTrack.contentUrl)
                                (alist-get 'contentUrl obj))))
            (unless content-url
              (user-error "Audiobookshelf: episode '%s' is not downloaded on the server" (alist-get 'title obj)))
            (funcall
             callback
             (list :uri (empv-audiobookshelf--item-extract-url obj (list (empv-audiobookshelf--file-url content-url)))
                   :start (empv-audiobookshelf--progress-start .userMediaProgress)
                   :chapters-file nil)))))))
    (other (user-error "Not playable: %s" other))))

(defun empv-audiobookshelf--load-options (playable)
  "Per-file mpv options string for PLAYABLE: resume point and chapters."
  (string-join
   (delq nil
         (list (format "start=%s" (plist-get playable :start))
               (when-let* ((file (plist-get playable :chapters-file)))
                 (if (string-match-p "[,\"'%]" file)
                     (format "chapters-file=%%%d%%%s" (string-bytes file) file)
                   (format "chapters-file=%s" file)))))
   ","))

(defun empv-audiobookshelf--load (playable action)
  "Load PLAYABLE (see `empv-audiobookshelf--resolve') into mpv.
ACTION is one of `play', `enqueue' or `enqueue-next'.  Unlike
`empv-play', the file is loaded with mpv's per-file options so playback
resumes at the saved position and server chapters are attached."
  (let ((uri (plist-get playable :uri))
        (options (empv-audiobookshelf--load-options playable)))
    (pcase action
      ('play
       (empv-cmd-seq
        ('loadfile (list uri "append" -1 options))
        ('get_property 'playlist-count)
        ('playlist-play-index (1- it))
        ('set_property '(pause :json-false))))
      ('enqueue
       (empv-cmd 'loadfile (list uri "append-play" -1 options)))
      ('enqueue-next
       (empv-cmd 'loadfile (list uri "insert-next-play" -1 options))))
    (empv-display-event
     "%s %s"
     (if (eq action 'play) "Playing" "Enqueued")
     (plist-get (empv-metadata-from-path uri) :title))))

(defun empv-audiobookshelf--play-or-enqueue (playable)
  "Ask what to do with PLAYABLE and do it, like `empv-play-or-enqueue'."
  (empv-select-action _
    "Play" → (empv-audiobookshelf--load playable 'play)
    "Enqueue last" → (empv-audiobookshelf--load playable 'enqueue)
    "Enqueue next" → (empv-audiobookshelf--load playable 'enqueue-next)))

(defun empv-audiobookshelf--act (action obj)
  "Resolve Audiobookshelf item OBJ and load it with ACTION.
Only books and episodes are playable."
  (unless (memq (alist-get 'kind obj) '(book episode))
    (user-error "Not applicable for %s" (alist-get 'kind obj)))
  (empv-audiobookshelf--resolve obj (lambda (playable) (empv-audiobookshelf--load playable action))))

(defun empv-audiobookshelf-play (item)
  "Play the Audiobookshelf ITEM (a book or an episode), resuming where it was left."
  (empv-audiobookshelf--act 'play item))

(defun empv-audiobookshelf-enqueue (item)
  "Enqueue the Audiobookshelf ITEM at the end of the playlist."
  (empv-audiobookshelf--act 'enqueue item))

(defun empv-audiobookshelf-enqueue-next (item)
  "Enqueue the Audiobookshelf ITEM right after the current one."
  (empv-audiobookshelf--act 'enqueue-next item))

;;;; Progress

(defvar empv-audiobookshelf--current nil
  "Metadata plist of the Audiobookshelf item mpv is playing, or nil.
See `empv-audiobookshelf--item-extract-url' for the keys.")

(defvar empv-audiobookshelf--last-time-pos nil
  "Last playback position (seconds) seen for `empv-audiobookshelf--current'.")

(defvar empv-audiobookshelf--sync-timer nil
  "Timer that periodically reports progress while an Audiobookshelf item plays.")

(defun empv-audiobookshelf--progress-endpoint (info)
  "Return the `/api/me/progress' endpoint for the item described by INFO.
INFO is a metadata plist with `:id' and, for episodes, `:episodeId'."
  (if-let* ((episode-id (plist-get info :episodeId)))
      (format "/api/me/progress/%s/%s" (plist-get info :id) episode-id)
    (format "/api/me/progress/%s" (plist-get info :id))))

(defun empv-audiobookshelf--send-progress (info time-pos)
  "Report TIME-POS (seconds) as the listening position of INFO.
INFO is a metadata plist with `:id', `:episodeId' and `:duration'.
Does nothing when either argument is nil."
  (when (and info time-pos)
    (empv-audiobookshelf--request-json
     "PATCH"
     (empv-audiobookshelf--progress-endpoint info)
     (delq nil
           (list (cons 'currentTime (float time-pos))
                 (when (plist-get info :duration)
                   (cons 'duration (plist-get info :duration)))))
     #'ignore)))

(defun empv-audiobookshelf--handle-end-file (event)
  "Mark the current Audiobookshelf item finished when EVENT says it ended.
EVENT is mpv's `end-file' event; only a natural end (reason \"eof\")
counts.  The finished report replaces the pending position, so the
flush on the following path change does not send a stale one."
  (when (and empv-audiobookshelf--current
             (equal (alist-get 'reason event) "eof"))
    (let ((info empv-audiobookshelf--current))
      (setq empv-audiobookshelf--current nil
            empv-audiobookshelf--last-time-pos nil)
      (empv-audiobookshelf--stop-timer)
      (empv-audiobookshelf--request-json
       "PATCH"
       (empv-audiobookshelf--progress-endpoint info)
       (delq nil
             (list (when (plist-get info :duration)
                     (cons 'currentTime (float (plist-get info :duration))))
                   (cons 'isFinished t)))
       #'ignore))))

(defun empv-audiobookshelf--sync-now (&optional force)
  "Ask mpv for its current position and report it if changed or FORCE is non-nil."
  (when (and empv-audiobookshelf--current (empv-running?))
    (let ((info empv-audiobookshelf--current))
      (empv-cmd 'get_property 'time-pos
        ;; The item may have changed while the reply was in flight, and a
        ;; paused player keeps returning the same position.
        (when (and (numberp it) (eq info empv-audiobookshelf--current))
          (when (or force (not (equal it empv-audiobookshelf--last-time-pos)))
            (empv-audiobookshelf--send-progress info it))
          (setq empv-audiobookshelf--last-time-pos it))))))

(defun empv-audiobookshelf--start-timer ()
  "Start the periodic progress timer if it is not running."
  (unless empv-audiobookshelf--sync-timer
    (setq empv-audiobookshelf--sync-timer
          (run-with-timer (max 1 empv-audiobookshelf-progress-sync-interval)
                          (max 1 empv-audiobookshelf-progress-sync-interval)
                          #'empv-audiobookshelf--sync-tick))))

(defun empv-audiobookshelf--stop-timer ()
  "Stop the periodic progress timer."
  (when empv-audiobookshelf--sync-timer
    (cancel-timer empv-audiobookshelf--sync-timer)
    (setq empv-audiobookshelf--sync-timer nil)))

(defun empv-audiobookshelf--sync-tick ()
  "Timer function: report progress, or stop when nothing relevant plays."
  (if (and empv-audiobookshelf--current (empv-running?))
      (empv-audiobookshelf--sync-now)
    (empv-audiobookshelf--stop-timer)))

(defun empv-audiobookshelf--handle-path-change (path)
  "Track whether PATH, mpv's new `path' property, is an Audiobookshelf item.
Flushes the last known position of the previous item, then starts or
stops the progress timer."
  (let ((old-info empv-audiobookshelf--current)
        (old-position empv-audiobookshelf--last-time-pos)
        (info (and (stringp path) (empv-metadata-from-path path))))
    (setq empv-audiobookshelf--current (and (plist-get info :audiobookshelf) info)
          empv-audiobookshelf--last-time-pos nil)
    (if empv-audiobookshelf--current
        (empv-audiobookshelf--start-timer)
      (empv-audiobookshelf--stop-timer))
    (empv-audiobookshelf--send-progress old-info old-position)))

(defun empv-audiobookshelf--handle-pause (paused)
  "Report progress when playback of an Audiobookshelf item is PAUSED."
  (when (and empv-audiobookshelf--current (eq paused t))
    (empv-audiobookshelf--sync-now)))

;;;###autoload
(defun empv-audiobookshelf-sync-progress ()
  "Report the current playback position to Audiobookshelf now."
  (interactive)
  (unless (and empv-audiobookshelf--current (empv-running?))
    (user-error "Not playing an Audiobookshelf item"))
  (empv-audiobookshelf--sync-now t))

(defun empv-audiobookshelf--observe-player ()
  "Register Audiobookshelf progress listeners on the active mpv instance."
  (empv-remove-event 'end-file #'empv-audiobookshelf--handle-end-file)
  (empv-event 'end-file #'empv-audiobookshelf--handle-end-file)
  (empv-observe 'path #'empv-audiobookshelf--handle-path-change)
  (empv-observe 'pause #'empv-audiobookshelf--handle-pause))

(defun empv-audiobookshelf--handle-player-state (state)
  "Flush the last known position when empv reports shutdown via STATE."
  (when (and (eq state 'stopped) (not (empv-running?)))
    (empv-audiobookshelf--handle-path-change nil)))

(add-hook 'empv-init-hook #'empv-audiobookshelf--observe-player)
(add-hook 'empv-player-state-changed-hook #'empv-audiobookshelf--handle-player-state)
;; The add-on can also be loaded after mpv has already started.
(when (empv-running?)
  (empv-audiobookshelf--observe-player))

;;;; Browsing

(defun empv-audiobookshelf--normalize-items-response (response)
  "Normalize a `/api/libraries/ID/items' RESPONSE into empv items.
Ebook-only books are dropped (see `empv-audiobookshelf--playable-p')."
  (seq-filter #'empv-audiobookshelf--playable-p
              (mapcar #'empv-audiobookshelf--normalize-item (alist-get 'results response))))

(defun empv-audiobookshelf--normalize-search-response (response)
  "Normalize a `/api/libraries/ID/search' RESPONSE into empv items.
Books and podcasts arrive under the `book' or `podcast' key, each
wrapped in a `libraryItem'.  Ebook-only books are dropped (see
`empv-audiobookshelf--playable-p')."
  (seq-filter #'empv-audiobookshelf--playable-p
              (mapcar (lambda (hit) (empv-audiobookshelf--normalize-item (alist-get 'libraryItem hit)))
                      (append (alist-get 'book response) (alist-get 'podcast response)))))

(defun empv-audiobookshelf--normalize-episodes (item podcast)
  "Normalize the episodes of expanded podcast ITEM, newest first.
PODCAST is the normalized podcast item."
  (let-alist item
    (thread-last .media.episodes
                 (mapcar (lambda (episode) (empv-audiobookshelf--normalize-episode episode podcast)))
                 (seq-sort-by (lambda (episode) (or (alist-get 'publishedAt episode) 0)) #'>))))

(defun empv-audiobookshelf--item-group (item)
  "Return the completion group for ITEM."
  (capitalize (symbol-name (alist-get 'kind item))))

(defun empv-audiobookshelf--result-handler (prompt &optional on-quit)
  "Return a handler that shows Audiobookshelf items with PROMPT.
The handler takes a list of normalized items.  ON-QUIT is called when
the selector is cancelled; nested selectors use it to go back to the
selector they were opened from."
  (lambda (items)
    (unless items
      (user-error "Audiobookshelf: nothing found"))
    (cl-labels
        ((show-results ()
                       (let ((selected
                              (condition-case nil
                                  (empv-completing-read-object
                                   prompt
                                   items
                                   :formatter #'empv-audiobookshelf--format-candidate
                                   :category 'empv-audiobookshelf-item
                                   :group #'empv-audiobookshelf--item-group
                                   :sort? nil)
                                (quit
                                 (if on-quit
                                     (progn (funcall on-quit) nil)
                                   (signal 'quit nil))))))
                         (when selected
                           (empv-audiobookshelf--act-on-candidate selected #'show-results)))))
      (show-results))))

(defun empv-audiobookshelf--act-on-candidate (selected &optional on-quit)
  "Act on SELECTED Audiobookshelf item.
Libraries and podcasts open a nested selector; books and episodes are
played or enqueued.  ON-QUIT is called when a nested selector is
cancelled."
  (empv--dbg "empv-audiobookshelf--act-on-candidate :: %s" selected)
  (let ((id (alist-get 'id selected))
        (title (alist-get 'title selected)))
    (pcase (alist-get 'kind selected)
      ('library
       (empv-audiobookshelf--request
        (format "/api/libraries/%s/items" id)
        :limit 0 :sort "media.metadata.title" :minified 1
        (lambda (response)
          (funcall (empv-audiobookshelf--result-handler (format "Select from '%s':" title) on-quit)
                   (empv-audiobookshelf--normalize-items-response response)))))
      ('podcast
       (empv-audiobookshelf--request
        (format "/api/items/%s" id)
        :expanded 1
        (lambda (item)
          (funcall (empv-audiobookshelf--result-handler (format "Select episode of '%s':" title) on-quit)
                   (empv-audiobookshelf--normalize-episodes item selected)))))
      ((or 'book 'episode)
       (empv-audiobookshelf--resolve selected #'empv-audiobookshelf--play-or-enqueue))
      (other (error "Not found %s" other)))))

(defun empv-audiobookshelf--select-library (callback &optional media-type)
  "Let the user pick a library and call CALLBACK with it.
MEDIA-TYPE, \"book\" or \"podcast\", restricts the choice.  When only
one library matches it is picked without asking."
  (empv-audiobookshelf--request
   "/api/libraries"
   (lambda (response)
     (let ((libraries
            (seq-filter (lambda (library)
                          (or (null media-type) (equal (alist-get 'mediaType library) media-type)))
                        (mapcar #'empv-audiobookshelf--normalize-library (alist-get 'libraries response)))))
       (pcase (length libraries)
         (0 (user-error "Audiobookshelf: no %slibraries found" (if media-type (concat media-type " ") "")))
         (1 (funcall callback (car libraries)))
         (_ (funcall callback
                     (empv-completing-read-object
                      "Select library:"
                      libraries
                      :formatter #'empv-audiobookshelf--format-candidate
                      :category 'empv-audiobookshelf-item
                      :sort? nil))))))))

(defun empv-audiobookshelf--search-library (library &optional initial)
  "Search LIBRARY, starting with INITIAL input when supplied."
  (let* ((endpoint (format "/api/libraries/%s/search" (alist-get 'id library)))
         (request (lambda (query callback)
                    (empv-audiobookshelf--request
                     endpoint :q query :limit empv-audiobookshelf-result-count
                     (lambda (response)
                       (funcall callback (empv-audiobookshelf--normalize-search-response response))))))
         (selection
          (empv--read-async
           empv-audiobookshelf--search-prompt request
           (lambda (items) (mapcar #'empv-audiobookshelf--format-candidate items))
           :category 'empv-audiobookshelf-item
           :history 'empv-audiobookshelf--search-history
           :require-match t
           :initial initial
           :lookup (lambda (selected candidates input &rest _)
                     (when-let* ((candidate (car (member selected candidates))))
                       (list (get-text-property 0 'empv-item candidate) input)))
           :group (lambda (candidate transform)
                    (if transform candidate
                      (empv-audiobookshelf--item-group
                       (get-text-property 0 'empv-item candidate))))
           :fallback (lambda ()
                       (funcall request
                                (read-string empv-audiobookshelf--search-prompt initial
                                             'empv-audiobookshelf--search-history)
                                (empv-audiobookshelf--result-handler "Search results:"))
                       nil))))
    (when selection
      (empv-audiobookshelf--act-on-candidate
       (car selection)
       (lambda () (empv-audiobookshelf--search-library library (cadr selection)))))))

;;;; Interactive functions

;;;###autoload
(defun empv-audiobookshelf-browse ()
  "Browse an Audiobookshelf library: books, or podcasts and their episodes."
  (interactive)
  (empv-audiobookshelf--select-library #'empv-audiobookshelf--act-on-candidate))

;;;###autoload
(defun empv-audiobookshelf-search ()
  "Search books or podcasts in an Audiobookshelf library."
  (interactive)
  (empv-audiobookshelf--select-library #'empv-audiobookshelf--search-library))

;;;###autoload
(defun empv-audiobookshelf-continue-listening ()
  "Pick up an Audiobookshelf book or episode where you left off.
Shows the `Continue Listening' shelf of a library."
  (interactive)
  (empv-audiobookshelf--select-library
   (lambda (library)
     (empv-audiobookshelf--request
      (format "/api/libraries/%s/personalized" (alist-get 'id library))
      :limit empv-audiobookshelf-result-count
      (lambda (shelves)
        (let ((shelf (seq-find (lambda (shelf) (equal (alist-get 'id shelf) "continue-listening")) shelves)))
          (funcall (empv-audiobookshelf--result-handler "Continue listening:")
                   (seq-filter #'empv-audiobookshelf--playable-p
                               (mapcar #'empv-audiobookshelf--normalize-shelf-entity
                                       (alist-get 'entities shelf))))))))))

;;;###autoload
(defun empv-audiobookshelf-recent-episodes ()
  "List the newest unfinished podcast episodes of an Audiobookshelf library."
  (interactive)
  (empv-audiobookshelf--select-library
   (lambda (library)
     (empv-audiobookshelf--request
      (format "/api/libraries/%s/recent-episodes" (alist-get 'id library))
      :limit empv-audiobookshelf-result-count
      (lambda (response)
        (funcall (empv-audiobookshelf--result-handler "Recent episodes:")
                 (mapcar (lambda (episode) (empv-audiobookshelf--normalize-episode episode nil))
                         (alist-get 'episodes response))))))
   "podcast"))

(defun empv-audiobookshelf-bookmark-set (item)
  "Create an empv bookmark from Audiobookshelf ITEM."
  (empv-bookmark-set item))

;;;; Bookmarks

(defun empv-audiobookshelf--bookmark-record (target)
  "Return bookmark data for Audiobookshelf TARGET, an item or empv URI."
  (let* ((info (and (stringp target) (empv-metadata-from-path target)))
         (item (cond
                ((and (listp target) (eq (alist-get 'type target) 'audiobookshelf))
                 target)
                ((plist-get info :audiobookshelf)
                 (let ((id (plist-get info :id))
                       (episode-id (plist-get info :episodeId)))
                   `((kind . ,(if episode-id 'episode 'book))
                     (id . ,(or episode-id id))
                     (itemId . ,(and episode-id id))
                     (title . ,(plist-get info :title))
                     (duration . ,(plist-get info :duration))))))))
    (when item
      (cons (or (alist-get 'title item) "Audiobookshelf")
            (delq nil
                  (mapcar (lambda (key)
                            (let ((value (alist-get key item)))
                              (when value (cons key value))))
                          '(kind id itemId libraryId title author mediaType
                                 duration count publishedAt contentUrl)))))))

(empv-register-bookmark-type
 'audiobookshelf
 :record #'empv-audiobookshelf--bookmark-record
 :jump #'empv-audiobookshelf--act-on-candidate)

;;;; Embark

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-transformer-alist)

(defun empv-audiobookshelf--embark-item-transformer (type target)
  "Extract the Audiobookshelf item object from TARGET, keeping its TYPE."
  (cons type (get-text-property 0 'empv-item target)))

(with-eval-after-load 'embark
  (defvar-keymap empv-audiobookshelf-item-action-map
    :doc "Action map for Audiobookshelf items, utilized by Embark."
    :parent embark-general-map
    "p" #'empv-audiobookshelf-play
    "e" #'empv-audiobookshelf-enqueue
    "n" #'empv-audiobookshelf-enqueue-next
    "b" #'empv-audiobookshelf-bookmark-set)

  (add-to-list 'embark-keymap-alist
               '(empv-audiobookshelf-item . empv-audiobookshelf-item-action-map))
  (setf (alist-get 'empv-audiobookshelf-item embark-transformer-alist)
        #'empv-audiobookshelf--embark-item-transformer))

(provide 'empv-audibookshelf)
;;; empv-audibookshelf.el ends here
