;;; empv.el --- A multimedia player/manager, YouTube interface -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 4.0.0
;; Homepage: https://github.com/isamert/empv.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1") (s "1.13.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs media player, based on mpv.  More precisely this package
;; provides somewhat comprehensive interface to mpv with bunch of
;; convenient functionality like an embedded radio manager, YouTube
;; interface, local music/video library manager etc.

;; Lots of interactive functions are at your disposal.  To view the
;; most essential ones, type `M-x describe-keymap empv-map`.  It is
;; advised that you bind this keymap to a key for convenience.

;; Additionally, empv has versatile customization options.  For an
;; overview of all customization options, do `M-x customize-group
;; empv`.

;;; Code:

(require 'pp)
(require 'seq)
(require 'map)
(require 'json)
(require 'url)
(require 's)
(require 'consult nil t)
(eval-when-compile
  (require 'subr-x))


;;; Some helpful resources
;; - https://github.com/mpv-player/mpv/blob/master/DOCS/man/input.rst


;;; Customization

(defgroup empv nil
  "A media player for Emacs."
  :group 'multimedia)

(defcustom empv-socket-file (format "%sempv-socket" (temporary-file-directory))
  "Socket file path."
  :type 'string
  :group 'empv)

(defcustom empv-mpv-binary "mpv"
  "MPV binary path."
  :type 'string
  :group 'empv)

(defcustom empv-mpv-args `("--no-video" "--no-terminal" "--idle" ,(concat "--input-ipc-server=" empv-socket-file))
  "Args used while starting mpv.
This should contain --input-ipc-server=`empv-socket-file', also
--idle is recommended for keeping your state."
  :type 'list
  :group 'empv)

(defcustom empv-display-events t
  "Wheter to show events at the bottom of the screen.
Events may include track changes, volume changes etc."
  :type 'boolean
  :group 'empv)

(defcustom empv-invidious-instance nil
  "Invidious instance to interact with.
Invidious is a privacy focused YouTube interface.  See the
following links to find an instance:
- https://api.invidious.io/
- https://docs.invidious.io/Invidious-Instances.md
The URL should end with \"/api/v1\". An example would be
https://invidious-example.com/api/v1"
  :type 'string
  :group 'empv)

(defcustom empv-youtube-use-tabulated-results
  nil
  "Show YouTube results in a tabulated buffer with thumbnails if not nil.
Otherwise simply use `completing-read'.  You can still use
`empv-youtube-tabulated' or `consult-empv-youtube-tabulated'
commands if this variable is nil."
  :type 'boolean
  :group 'empv)

(defcustom empv-youtube-thumbnail-quality "default"
  "Default value for YouTube thumbnail quality.
If it's nil, then downloading thumbnails are disabled."
  :type '(choice
          (const :tag "No thumbnail" nil)
          (const :value "maxres")
          (const :value "maxresdefault")
          (const :value "sddefault")
          (const :value "high")
          (const :value "medium")
          (const :value "default")
          (const :value "start")
          (const :value "middle")
          (const :value "end"))
  :group 'empv)

(defcustom empv-audio-dir (or (getenv "XDG_MUSIC_DIR") "~/Music")
  "The directory (or list of directories) that you keep your music in."
  :type '(choice (directory :tag "Audio directory")
                 (list :tag "List of audio directories"))
  :group 'empv)

(defcustom empv-video-dir (or (getenv "XDG_VIDEOS_DIR") "~/Videos")
  "The directory (or list of directories) that you keep your videos in."
  :type '(choice (directory :tag "Video directory")
                 (list :tag "List of video directories"))
  :group 'empv)

(defcustom empv-playlist-dir (or (getenv "XDG_MUSIC_DIR") "~/Music")
  "The directory that you keep your playlists in."
  :type 'directory
  :group 'empv)

(defcustom empv-radio-channels
  '(("SomaFM - Groove Salad" . "http://www.somafm.com/groovesalad.pls")
    ("SomaFM - Drone Zone" . "http://www.somafm.com/dronezone.pls")
    ("SomaFM - Sonic Universe" . "https://somafm.com/sonicuniverse.pls")
    ("SomaFM - Metal" . "https://somafm.com/metal.pls")
    ("SomaFM - Vaporwaves" . "https://somafm.com/vaporwaves.pls"))
  "List of radio channels -- or any other type of streamable.
Elements should pair in the form of: `(\"Channel name\" . \"stream-address\")'"
  :type '(alist :key-type (string :tag "Channel Name")
                :value-type (string :tag "stream-address"))
  :group 'empv)

(defcustom empv-video-file-extensions '("mkv" "mp4" "avi" "mov")
  "List of video file extensions."
  :type '(repeat (string :tag "Extension"))
  :group 'empv)

(defcustom empv-audio-file-extensions '("mp3" "ogg" "wav" "m4a" "flac" "aac")
  "List of audio file extensions."
  :type '(repeat (string :tag "Extension"))
  :group 'empv)

(defcustom empv-max-directory-search-depth 6
  "Max depth while searching in directories."
  :type 'number
  :group 'empv)

(defcustom empv-base-directory nil
  "Base directory.
Functions that shows directory-selection-prompts starts in this
directory.  nil means starting in `default-directory'."
  :type '(choice directory (const :tag "Default Directory" nil))
  :group 'empv)

(defcustom empv-log-events-to-file nil
  "Log all events to given file.
Supply a path to enable logging.  nil means no logging."
  :type '(choice file (const :tag "No Logging" nil))
  :group 'empv)

(defcustom empv-radio-log-format
  "* #{timestamp} [#{channel-name}] #{track-title}\n#{capture}\n"
  "The format used when `empv-log-current-radio-song-name' is called.
`#{channel-name}', `#{timestamp}' and `#{track-title}' are
replaced with their current values at the time of calling.

#{capture} is replaced with the user-supplied string at the moment."
  :type 'string
  :group 'empv)

(defcustom empv-radio-log-file
  "~/logged-radio-songs.org"
  "The file that is used by `empv-log-current-radio-song-name'."
  :type 'file
  :group 'empv)

(defcustom empv-lyrics-save-automatically nil
  "Save lyrics to audio file automatically when `empv-lyrics' is called."
  :type 'boolean
  :group 'empv)

(defcustom empv-allow-insecure-connections
  nil
  "Allow insecure connections while doing network calls.
This could be useful for being able to use some invidious
instances."
  :type 'boolean
  :group 'empv)

(defcustom empv-action-handler
  'read-multiple-choice
  "Action handler style.
When you select a media to play, you're presented with some
options like: \"Play\", \"Enqueue\" etc. This customization
determines which function is used to show you these actions."
  :type '(choice (const :tag "completing-read" completing-read)
                 (const :tag "read-multiple-choice" read-multiple-choice))
  :group 'empv)

(defcustom empv-init-hook
  '()
  "Functions to run after mpv process is started."
  :type 'hook
  :group 'empv)

(defcustom empv-volume-step
  5
  "Step percentage used in empv-volume-{up,down}."
  :type 'number
  :group 'empv)

(defcustom empv-log-prefix
  "empv :: "
  "Prefix that is shown in the logs."
  :type 'string
  :group 'empv)

(defcustom empv-search-prefix
  "https://html.duckduckgo.com/html/?q="
  "URL that is used for searching the web.
Only used in lyrics related functions."
  :type 'string
  :group 'empv)

(defcustom empv-reset-playback-speed-on-quit
  nil
  "Whether to reset the playback speed when quitting mpv.
This sets the playback speed to 1 when you hit the `q' key while
on video view in mpv.  You need to enable overriding the quit key
for this to work.  See `empv-override-quit-key'."
  :type 'string
  :group 'empv)


;;; Public variables

;;;###autoload
(defvar empv-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Add loop on/off

    (define-key map "o" 'empv-play-or-enqueue)
    (define-key map "f" 'empv-play-file)
    (define-key map "d" 'empv-play-directory)
    (define-key map "v" 'empv-play-video)
    (define-key map "a" 'empv-play-audio)
    (define-key map "i" 'empv-display-current)
    (define-key map "c" 'empv-copy-path)

    (define-key map "t" 'empv-toggle)
    (define-key map "_" 'empv-toggle-video)

    (define-key map "r" 'empv-play-radio)
    (define-key map "R" 'empv-play-random-channel)
    (define-key map "l" 'empv-log-current-radio-song-name)

    (define-key map "[" 'empv-playback-speed-down)
    (define-key map "]" 'empv-playback-speed-up)
    (put 'empv-playback-speed-up 'repeat-map 'empv-map)
    (put 'empv-playback-speed-down 'repeat-map 'empv-map)

    (define-key map "x" 'empv-chapter-select)
    (define-key map "(" 'empv-chapter-prev)
    (define-key map ")" 'empv-chapter-next)

    (define-key map "9" 'empv-volume-down)
    (define-key map "0" 'empv-volume-up)
    (put 'empv-volume-up 'repeat-map 'empv-map)
    (put 'empv-volume-down 'repeat-map 'empv-map)

    (define-key map "p" 'empv-playlist-select)
    (define-key map "s" 'empv-playlist-shuffle)
    (define-key map "C" 'empv-playlist-clear)
    (define-key map "n" 'empv-playlist-next)
    (define-key map "N" 'empv-playlist-prev)
    (put 'empv-playlist-next 'repeat-map 'empv-map)
    (put 'empv-playlist-prev 'repeat-map 'empv-map)

    (define-key map "y" 'empv-youtube)
    (define-key map "Y" 'empv-youtube-last-results)

    (define-key map "q" 'empv-exit)
    map)
  "Keymap for commonly used empv functions.
It is not bound to any key by default.  Some keys are loosely
modelled after default keys of mpv.")


;;; Internal variables

(defconst empv--title-sep "##"
  "Seperator for seperating the URL and it's title.
MPV does not provide a way to get media-title for a given item
in the playlist.  This makes sense, because the item might be a
YouTube link and a network request is required to fetch it's
title.  MPV gathers the media-title while playing the media but it
does not persist it.  So it's not possible to display media's
title even if it's already played before.

empv tries to append media's title at the end of it's URL.  Assume
the following media:

    https://youtu.be/watch?v=X4bgXH3sJ2Q

empv appends `empv--title-sep' and a parsable sexp containing the
media's title etc. to end of the PATH.

    https://youtu.be/watch?v=X4bgXH3sJ2Q##(:title \"Iron Maiden - The Trooper\" :youtube t)

Stuff that starts with # is discarded in the URL while fetching,
so it's a hackish way to store extra information in the
filename.  This only works for URLs, not on local files.  Anyway,
this makes it possible to show media titles instead of plain URLs
whenever we have the title information a priori.

Also see #6 for extra information.")

(defvar empv--media-title-cache
  (make-hash-table :test #'equal)
  "Media title cache.
It's a mapping from a path/uri to media title.  MPV reads the
media-title/metadata of given path only when it starts playing
it.  Whenever this information is read, we put it into cache so
that we can retrieve later to show user the media-title instead
of a raw path whenever possible.  This cache is required because
MPV does not attach media-title information to playlist
items.  See [1] for more information on this.

[1]: https://github.com/mpv-player/mpv/pull/10453")

(defvar empv--process nil)
(defvar empv--network-process nil)
(defvar empv--callback-table (make-hash-table :test 'equal))
(defvar empv--dbg nil)
(defvar empv--last-youtube-candidates '()
  "Latest candidates that are shown in a `completing-read' by empv.
Mainly used by embark actions defined in this package.")
(defvar empv--action-selection-default-title "Action"
  "The text displayed on action selection menus.")
(defvar empv--inspect-buffer-name "*empv inspect*"
  "Buffer name for showing pretty printed results.")
(defvar-local empv--current-file nil
  "Current media file associated with the buffer.")

(defconst empv--playlist-current-indicator (propertize "[CURRENT]" 'face '(:foreground "green"))
  "Simple text to show on the currently playing playlist item.")

(defvar empv--youtube-search-history nil)


;;; Utility

(defun empv-flipcall (fn x y)
  "Flip arguments of given binary function FN and call it with Y and X."
  (funcall fn y x))

(defun empv-seq-init (seq)
  "Return the SEQ without the last element."
  (seq-subseq seq 0 (1- (seq-length seq))))

(defun empv-seq-find-index (fn seq)
  "Return the first index in SEQ for which FN evaluate to non-nil."
  (seq-position seq 'empv-dummy-item (lambda (it _) (funcall fn it))))

(cl-defmacro empv--wait-until-non-nil (place &rest forms)
  "Wait until PLACE is non-nil, after executing FORMS."
  (declare (indent 1))
  `(let (,place (try-count 0))
     ,@forms
     (while (and (not result) (< try-count 500))
       (setq try-count (1+ try-count))
       (sleep-for 0.01))
     result))

(cl-defmacro empv--try-until-non-nil (place &rest forms)
  "Run FORMS until PLACE becomes non-nil."
  (declare (indent 1))
  `(let (,place (try-count 0))
     ,@forms
     (while (and (not result) (< try-count 500))
       (setq try-count (1+ try-count))
       (sleep-for 0.01)
       ,@forms)
     result))

(defun empv--running? ()
  "Return if there is an mpv instance running or not."
  empv--process)

(defun empv--dbg (msg &rest rest)
  "Print MSG with REST if variable `empv--dbg' is non-nil."
  (when empv--dbg
    (apply #'message `(,(format "(empv) %s" msg) ,@rest))))

(defun empv--display-event (msg &rest rest)
  "Print MSG with REST if `empv-display-events' is non-nil."
  (let ((formatted-msg `(,(format "%s%s" empv-log-prefix msg) ,@rest)))
    (when empv-log-events-to-file
      (write-region
       (concat
        (format-time-string "[%F %T] ")
        (apply #'format formatted-msg)
        "\n")
       nil empv-log-events-to-file 'append))
    (when empv-display-events
      (apply #'message formatted-msg))))

(defun empv--read-result (result)
  "Read JSON RESULT into an elisp structre."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string result)
                 (error (empv--dbg "Error while reading JSON :: %s" result) nil))))
    json))

(defvar empv--request-id 0)
(defun empv--new-request-id ()
  "Generate a new unique request id."
  (format "%s" (setq empv--request-id (1+ empv--request-id))))

(defmacro empv--select-action (prompt &rest forms)
  (declare (indent 1))
  (let* ((selection-only? (listp (car forms)))
         (actions
          (if selection-only?
              (mapcar #'list (car forms))
            (seq-partition forms 3)))
         (used-keys '())
         (rmc-choices
          (mapcar
           (lambda (it)
             (list (let ((new (seq-find
                               (lambda (it) (not (memq it used-keys)))
                               (downcase (car it)))))
                     (push new used-keys)
                     new)
                   (downcase (car it))))
           actions))
         (cr-choices (mapcar #'car actions))
         (prompt (if (eq prompt '_) 'empv--action-selection-default-title prompt)))
    `(let ((result (pcase empv-action-handler
                     ('read-multiple-choice (nth 1 (read-multiple-choice ,prompt ',rmc-choices)))
                     ('completing-read (completing-read ,(format "%s: " prompt) ',cr-choices)))))
       ,(if selection-only?
            'result
          `(pcase (downcase result)
             ,@(mapcar (lambda (it) (list (downcase (seq-find #'stringp it)) (car (last it)))) actions))))))

(defun empv--inspect-obj (obj)
  "Inspect the given elisp OBJ."
  (get-buffer-create empv--inspect-buffer-name)
  (let ((print-length nil)
        (print-level nil))
    (pp-display-expression obj empv--inspect-buffer-name))
  (unless (get-buffer-window empv--inspect-buffer-name)
    (switch-to-buffer-other-window empv--inspect-buffer-name)))

(defun empv--clean-uri (it)
  (car (split-string it empv--title-sep)))

(defmacro empv--run (&rest forms)
  "Start if mpv is not running already and then run FORMS."
  `(progn
     (unless (empv--running?)
       (empv-start))
     ,@forms))


;;; Handlers

(defun empv--sentinel (_proc msg)
  "Clean up after connection closes with MSG."
  (empv--dbg "<< sentinel -> %s" msg)
  (empv--display-event "Closed.")
  (empv-exit))

(defvar empv--process-buffer ""
  "Temporary variable to hold the date returned from mpv process.

If the returned result is sufficently long (like a very long
playlist), `empv--filter' might get called with incomplete
JSON.  According to here[1], mpv terminates every result with
`\n', so we simply wait until we see a newline before processing
the result.

[1]: https://github.com/mpv-player/mpv/blob/master/DOCS/man/ipc.rst")

(defun empv--filter (_proc incoming)
  "Filter INCOMING messages from the socket."
  (setq empv--process-buffer (concat empv--process-buffer incoming))
  (when (string-match-p "\n$" incoming)
    (seq-do
     (lambda (it)
       (let* ((json-data (empv--read-result it))
              (id (map-elt json-data 'id))
              (request-id (map-elt json-data 'request_id))
              (callback (map-elt empv--callback-table (format "%s" (or request-id id)))))
         (empv--dbg
          "<< data: %s, request_id: %s, has-cb?: %s"
          json-data
          request-id
          (and callback t))
         ;; Remove it first from callback table, so we don't get into
         ;; a loop if an error occurs on the callback function
         (when (not (plist-get callback :event?))
           (map-delete empv--callback-table request-id))
         (when-let (cb-fn (plist-get callback :fn))
           (ignore-error (quit minibuffer-quit)
             (funcall cb-fn (cdr (assoc 'data json-data)))))))
     (seq-filter
      (lambda (it) (not (string-empty-p it)))
      (seq-map
       #'string-trim
       (split-string empv--process-buffer "\n"))))
    (setq empv--process-buffer "")))


;;; Process primitives

(defun empv--make-process (&optional uri)
  "Create the MPV process with given URI."
  (setq empv--process
        (make-process :name "empv-process"
                      :buffer nil
                      :command (if uri
                                   `(,empv-mpv-binary ,@empv-mpv-args ,uri)
                                 `(,empv-mpv-binary ,@empv-mpv-args)))))

(defun empv--make-network-process ()
  "Create the network process for mpv.
Blocks until mpv process establishes it's socket interface."
  (setq empv--network-process
        (empv--try-until-non-nil result
          (setq result
                (ignore-error file-error
                  (make-network-process :name "empv-network-process"
                                        :family 'local
                                        :service empv-socket-file
                                        :sentinel #'empv--sentinel
                                        :filter #'empv--filter))))))


(defun empv--send-command (command &optional callback event?)
  "Send COMMAND to mpv and call CALLBACK when mpv responds.
If EVENT? is non-nil, then the command is treated as an event
observer and the callback is called everytime that given event
happens."
  (empv--run
   (let* ((request-id (empv--new-request-id))
          (msg (json-encode
                (if event?
                    `((command . (,(seq-first command) ,(string-to-number request-id) ,@(seq-rest command)))
                      (request_id . ,request-id))
                  `((command . ,command)
                    (request_id . ,request-id))))))
     (when callback
       (map-put! empv--callback-table request-id (list :fn callback :event? event?)))
     (process-send-string empv--network-process (format "%s\n" msg))
     (empv--dbg ">> %s" msg)
     request-id)))

(defun empv--send-command-sync (command)
  "Send COMMAND to mpv process and return the result."
  (empv--wait-until-non-nil result
    (empv--send-command command (lambda (x) (setq result x)))))


;;; Essential macros

(defmacro empv--cmd (cmd &optional args &rest forms)
  "Run CMD with ARGS and then call FORMS with the result."
  (let ((cb (when (not (seq-empty-p forms))
              `(lambda (it) (ignore it) ,@forms))))
    `(if (listp ,args)
         (empv--send-command `(,,cmd ,@,args) ,cb)
       (empv--send-command `(,,cmd ,,args) ,cb))))

(defmacro empv--cmd-seq (&rest forms)
  (seq-reduce
   (lambda (acc it) `(empv--cmd ,(nth 0 it) ,(nth 1 it) (,@acc)))
   (reverse forms)
   '()))

(defmacro empv--let-properties (props &rest forms)
  (declare (indent 1))
  `(let* ((prop-count (length ,props))
          (props-alist (make-hash-table :size prop-count))
          (finalized-count 0))
     (seq-do
      (lambda (prop)
        (empv--send-command
         (list "get_property" prop)
         (lambda (result)
           (map-put! props-alist prop result)
           (setq finalized-count (1+ finalized-count))
           (when (eq finalized-count prop-count)
             (let-alist (map-into props-alist 'alist)
               ,@forms)))))
      ,props)))

(defmacro empv--with-media-info (&rest body)
  "Gives you a context containing `.media-title', `.path' `.metadata'"
  `(empv--let-properties '(metadata media-title path)
     (let ((.media-title (empv--create-media-summary-for-notification .metadata .path .media-title)))
       ,@body)))

(defmacro empv--with-video-enabled (&rest forms)
  `(let* ((empv-mpv-args (seq-filter (lambda (it) (not (equal it "--no-video"))) empv-mpv-args))
          (result (progn ,@forms)))
     (empv--cmd
      'get_property 'video
      (when (eq it :json-false)
        (empv-toggle-video)))
     result))

(defmacro empv--transform-property (property fn)
  (declare (indent 1))
  `(empv--send-command
    (list "get_property" ,property)
    (lambda (result)
      (let ((new-val (funcall ,fn result)))
        (empv--send-command (list "set_property" ,property new-val) #'ignore)
        (empv--display-event "%s is set to %s" (capitalize (symbol-name ,property)) new-val)))))


;;; User level helpers

(defun empv-observe (property callback)
  "Observe PROPERTY and call CALLBACK when it does change."
  (empv--send-command `(observe_property ,property) callback t))


;;; Essential functions

;;;###autoload
(defun empv-play-or-enqueue (uri)
  "Play or enqueue the URI based on user input.
URI might be a string or a list of strings."
  (interactive "sEnter an URI to play: ")
  (empv--select-action _
    "Play" → (cond
              ((stringp uri) (empv-play uri))
              ((listp uri) (empv--cmd
                            'stop nil
                            (seq-do #'empv-enqueue uri)
                            (empv-resume))))
    "Enqueue last" → (cond
                      ((stringp uri) (empv-enqueue uri))
                      ((listp uri) (seq-do #'empv-enqueue uri)))
    "Enqueue next" → (cond
                      ((stringp uri) (empv-enqueue-next uri))
                      ((listp uri) (seq-do #'empv-enqueue-next uri)))))

(defun empv--metadata-get (alist main fallback)
  "Get MAIN from ALIST, if it's nill get FALLBACK from ALIST."
  (or (cdr (assoc main alist)) (cdr (assoc fallback alist))))

(defun empv--extract-metadata (data)
  "Extract metadata into a consistent format using DATA."
  `((title  . ,(empv--metadata-get data 'title 'icy-title))
    (album  . ,(empv--metadata-get data 'album 'icy-album))
    (artist . ,(empv--metadata-get data 'artist 'icy-artist))
    (genre  . ,(empv--metadata-get data 'genre 'icy-genre))))

(defun empv--create-media-summary-for-notification (metadata path &optional fallback)
  "Generate a formatted media title like \"Song name - Artist\" from given METADATA."
  (let-alist (empv--extract-metadata metadata)
    (if .title
        (format "%s %s %s"
                (string-trim .title)
                (or (and .artist "-") "")
                (or .artist ""))
      (plist-get (empv--extract-empv-metadata-from-path path fallback) :title))))

(defun empv--handle-metadata-change (data)
  "Display info about the current track using DATA."
  (empv--dbg "handle-metadata-change <> %s" data)
  (empv--let-properties '(media-title path)
    (when .path
      (let ((title (empv--create-media-summary-for-notification data .path .media-title)))
        (empv--display-event "%s" title)
        (puthash (empv--clean-uri .path) title empv--media-title-cache)))))

(defun empv-start (&optional uri)
  "Start mpv using `empv-mpv-command' with given URI."
  (interactive)
  (unless (empv--running?)
    (empv--dbg "Starting MPV.")
    (empv--make-process uri)
    (empv--make-network-process)
    (empv-observe 'metadata #'empv--handle-metadata-change)
    (run-hooks 'empv-init-hook)))

(defun empv--extract-empv-metadata-from-path (path &optional fallback)
  "Extract the metadata encoded in PATH.
Some metadata is encoded into PATH and this function tries to
parse that and returns a form along the lines of:

    \\='(:title \"...\" :uri \"the-real-uri-without-metadata\" :radio nil/t :youtube nil/t)

:TITLE is the human readable name of the path. This function also
checks if is there any cached name for this PATH.

:URI is the PATH but without the encoded metadata. Clean version
of the PATH.

:RADIO indicates if this PATH is a radio stream or not (only true
if invoked by `empv-play-radio' etc.)

:YOUTUBE indicates if this PATH is a YouTube path or not (only
true if invoked by `empv-youtube' family of functions.)

Also see `empv--title-sep' and `empv--media-title-cache'
documentation."
  ;; First try the URL encoded title and then check for the
  ;; media-title cache.  Cache may contain last playing media's title
  ;; for a stream, not the name for the stream itself. URL encoded
  ;; title probably has the stream's title. (At least this is the case
  ;; for radio streams, see `empv-radio-channels' and
  ;; `empv--play-radio-channel')
  (pcase-let* ((`(,uri ,data) (split-string (or path "") empv--title-sep)))
    (if-let (parsed
             (and data
                  (string-prefix-p "(" data)
                  (string-suffix-p ")" data)
                  (ignore-errors (read data))))
        `(,@parsed :uri ,uri)
      (list
       :uri uri
       :title (gethash uri empv--media-title-cache
                       (or fallback (abbreviate-file-name uri)))))))

(cl-defmacro empv--with-empv-metadata (&rest forms)
  "Gives you a context containing `empv-metadata' and execute FORMS.
See `empv--extract-empv-metadata-from-path' documentation to
see what `empv-metadata' object looks like."
  `(empv--let-properties '(path)
     (let ((empv-metadata (empv--extract-empv-metadata-from-path .path)))
       ,@forms)))

(defun empv--format-playlist-item (item)
  "Format given ITEM into a readable item.
INDEX is the place where the item appears in the playlist."
  (format
   "%s%s"
   (or (and (alist-get 'current item)
            (format "%s " empv--playlist-current-indicator)) "")
   (string-trim
    (or (alist-get 'title item)
        (thread-first
          (alist-get 'filename item)
          empv--extract-empv-metadata-from-path
          (plist-get :title))))))

(defmacro empv--playlist-select-item-and (&rest forms)
  "Select a playlist item and then run FORMS with the input.
This function also tries to disable sorting in `completing-read' function."
  `(empv--let-properties '(playlist)
     (let ((item (empv--completing-read-object
                  "Select track: "
                  (seq-map-indexed (lambda (item index) (map-insert item 'index index)) .playlist)
                  :formatter #'empv--format-playlist-item
                  :category 'empv-playlist-item
                  :sort? nil)))
       (ignore item)
       ,@forms)))

;;;###autoload
(cl-defun empv--completing-read-object
    (prompt objects &key (formatter #'identity) category (sort? t) def multiple?)
  "`completing-read' with formatter and sort control.
Applies FORMATTER to every object in OBJECTS and propertizes
candidates with the actual object so that they can be retrieved
later by embark actions.  Also adds category metadata to each
candidate, if given.  PROMPT passed to `completing-read' as is."
  (let* ((object-table
          (make-hash-table :test 'equal :size (length objects)))
         (object-strings
          (mapcar
           (lambda (object)
             (let ((formatted-object (funcall formatter object)))
               (puthash formatted-object object object-table)
               (propertize formatted-object 'empv-item object)))
           objects))
         (selected
          (funcall
           (if multiple? #'completing-read-multiple #'completing-read)
           (format "%s " prompt)
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(when category (cons 'category category))
                   ,@(unless sort?
                       '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
               (complete-with-action
                action object-strings string predicate))))))
    (if multiple?
        (or (mapcar (lambda (it) (gethash it object-table)) selected) def)
      (gethash selected object-table (or def selected)))))


;;; Interactive - Basics

;;;###autoload
(defun empv-play (uri)
  "Play given URI.
Add given URI to end of the current playlist and immediately
switch to it"
  (interactive "sEnter an URI to play: ")
  (when (file-exists-p uri)
    (setq uri (expand-file-name uri)))
  (if (empv--running?)
      (empv--cmd-seq
       ('loadfile (list uri 'append))
       ('get_property 'playlist-count)
       ('playlist-play-index (1- it))
       ('set_property '(pause :json-false)))
    (empv-start uri))
  (when (called-interactively-p 'interactive)
    (empv--display-event "Playing %s" uri)))

(defun empv-seek (target &optional type)
  "Change the playback position according to TARGET.
TYPE is `(\"relative\")' by default.  It can be one of the
following or combination of them (like `(\"absolute\"
\"keyframes\")'):

relative (default)
    Seek relative to current position (a negative value seeks backwards).
absolute
    Seek to a given time (a negative value starts from the end of the file).
absolute-percent
    Seek to a given percent position.
relative-percent
    Seek relative to current position in percent.
keyframes
    Always restart playback at keyframe boundaries (fast).
exact
    Always do exact/hr/precise seeks (slow).

Some examples:

    (empv-seek \"33:27\" \\='(\"absolute\")) -> Jump exactly to 33:27.
    (empv-seek \"40\" \\='(\"relative\"))    -> Seek +40 seconds

See this[1] for more information.

[1]: https://mpv.io/manual/master/#command-interface-seek-%3Ctarget%3E-[%3Cflags%3E]"
  (interactive
   (let ((type (list (empv--select-action "How? " ("relative" "absolute" "relative-percent" "absolute-percent" "keyframes" "exact"))))
         (target (read-string "Target: ")))
     (list target type)))
  (empv--cmd 'seek `(,target ,(string-join (or type '("relative")) "+"))))

;;;###autoload
(defun empv-play-file (path)
  "Play given PATH.
This is just a simple wrapper around `empv-play' that displays
`find-file' dialog if called interactively."
  (interactive "fPlay file: ")
  (empv-play-or-enqueue (expand-file-name path)))

;;;###autoload
(defun empv-play-directory (path)
  "Enqueue given files under PATH.
If called interactively, shows a prompt to select a directory.  By
default this directory is `default-directory'.  If you want this
function to start the prompt in same directory everytime, please
see `empv-base-directory'."
  (interactive
   (list (read-directory-name "Select directory to enqueue: " empv-base-directory nil t)))
  (thread-last
    (empv--find-files path (append empv-audio-file-extensions empv-video-file-extensions) 1)
    (mapcar (lambda (it) (expand-file-name it path)))
    (empv-play-or-enqueue)))

;;;###autoload
(defun empv-resume ()
  "Resume the playback."
  (interactive)
  (empv--cmd 'set_property '(pause :json-false)))

;;;###autoload
(defun empv-pause ()
  "Pause the playback."
  (interactive)
  (empv--cmd 'set_property '(pause t)))

;;;###autoload
(defun empv-toggle ()
  "Toggle the playback."
  (interactive)
  (empv--cmd 'cycle 'pause))

;;;###autoload
(defun empv-current-loop-on ()
  "Turn on loop for current file."
  (interactive)
  (empv--cmd 'set_property '(loop-file inf))
  (empv--display-event "↻ File loop on."))

(defalias 'empv-file-loop-on #'empv-current-loop-on)

;;;###autoload
(defun empv-current-loop-off ()
  "Turn off loop for current file."
  (interactive)
  (empv--cmd 'set_property '(loop-file no))
  (empv--display-event "File loop off."))

(defalias 'empv-file-loop-off #'empv-current-loop-off)

(defun empv-toggle-current-loop ()
  "Turn on or off the loop for current file."
  (interactive)
  (empv--transform-property 'loop-file
    (lambda (it) (if (equal it "inf") 'no 'inf))))

(defalias 'empv-toggle-file-loop #'empv-toggle-current-loop)

;;;###autoload
(defun empv-volume-up ()
  "Up the volume to a max of 100%."
  (interactive)
  (empv--let-properties '(volume-max)
    (empv--transform-property 'volume
      (lambda (current) (min (floor (+ current empv-volume-step)) (or .volume-max 100))))))

;;;###autoload
(defun empv-volume-down ()
  "Down the volume to a min of 0%."
  (interactive)
  (empv--transform-property 'volume (lambda (current) (max (floor (- current empv-volume-step)) 0))))

;;;###autoload
(defun empv-set-volume ()
  "Set the exact volume."
  (interactive)
  (empv--let-properties '(volume-max)
    (empv--transform-property 'volume
      (lambda (current)
        (max 0 (min (or .volume-max 100) (floor (read-number (format "Volume (0-%s, current %s): " (floor (or .volume-max 100)) (floor current))))))))))

;;;###autoload
(defun empv-set-playback-speed ()
  "Set the exact playback speed."
  (interactive)
  (empv--transform-property 'speed
    (lambda (speed)
      (read-string (format "Speed (current %s): " speed)))))

;;;###autoload
(defun empv-playback-speed-down ()
  "Lower the playback speed by `0.25'."
  (interactive)
  (empv--transform-property 'speed (apply-partially #'+ -0.25)))

;;;###autoload
(defun empv-playback-speed-up ()
  "Increase the playback speed by `0.25'."
  (interactive)
  (empv--transform-property 'speed (apply-partially #'+ 0.25)))

;;;###autoload
(defun empv-toggle-video ()
  "Toggle the video display.
You can press \"_\" to hide it again when you are focused on
MPV."
  (interactive)
  (empv--cmd 'cycle 'video))

;;;###autoload
(defun empv-exit ()
  "Shut down mpv."
  (interactive)
  (when empv--process
    (setq empv--process (delete-process empv--process)))
  (when empv--network-process
    (setq empv--network-process (delete-process empv--network-process)))
  (setq empv--callback-table (make-hash-table :test 'equal))
  (setq empv--media-title-cache (make-hash-table :test 'equal)))

;;;###autoload
(defun empv-save-and-exit ()
  "Exit and save the current playing position.
Playing that file later will seek to the previous position on
start.  This is equivalent to doing `Shift + q' on an mpv
window."
  (interactive)
  (empv--cmd
   'quit-watch-later '()
   (empv-exit)))

;;;###autoload
(defun empv-toggle-event-display ()
  "Toggle debug mode."
  (interactive)
  (setq empv-display-events (not empv-display-events))
  (empv--display-event "Display event mode is %s" empv-display-events))

;;;###autoload
(defun empv-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (setq empv--dbg (not empv--dbg))
  (empv--display-event  "Debug mode is %s" empv--dbg))

(defun empv-log-current-radio-song-name (&optional capture?)
  "Log current radio song name with the radio channel name.
The song's are logged into `empv-radio-log-file' with the format
that is defined in `empv-radio-log-format'.

When CAPTURE? is non-nil, also ask user for extra input to save
along with the log."
  (interactive "P")
  (empv--let-properties '(metadata path)
    (when-let ((title (alist-get 'icy-title .metadata))
               (empv-metadata (empv--extract-empv-metadata-from-path .path)))
      (write-region
       (thread-last
         empv-radio-log-format
         (string-replace "#{timestamp}" (format "[%s]" (format-time-string "%F %a %R")))
         (string-replace "#{channel-name}" (plist-get empv-metadata :title))
         (string-replace "#{track-title}" title)
         (string-replace "#{capture}" (if capture? (read-string "Extra notes: ") "")))
       nil empv-radio-log-file 'append)
      (message "%s" title))))


;;; Interactive - Playlist

;;;###autoload
(defun empv-enqueue (uri)
  "Like `empv-play' but add the given URI to end of the playlist."
  (interactive "sEnter an URI to play: ")
  (when (string-prefix-p "~/" uri)
    (setq uri (expand-file-name uri)))
  (empv--cmd 'loadfile `(,uri append-play))
  (empv--display-event "Enqueued %s" uri))

(defalias 'empv-enqueue-last #'empv-enqueue)

(defun empv-enqueue-next (uri)
  "Like `empv-enqueue' but append URI right after current item."
  (interactive "sEnter an URI to play: ")
  (empv--let-properties '(playlist)
    (let ((len (length .playlist))
          (idx (empv-seq-find-index (lambda (it) (alist-get 'current it)) .playlist)))
      (empv-enqueue uri)
      (empv--cmd 'playlist-move `(,len ,(1+ idx))))))

;;;###autoload
(defun empv-playlist-next ()
  "Play next in the playlist."
  (interactive)
  (empv--cmd 'playlist-next))

;;;###autoload
(defun empv-playlist-prev ()
  "Play previous in the playlist."
  (interactive)
  (empv--cmd 'playlist-prev))

;;;###autoload
(defun empv-playlist-clear ()
  "Clear the current playlist."
  (interactive)
  (empv--cmd 'playlist-clear)
  (empv--display-event "Playlist cleared."))

;;;###autoload
(defun empv-playlist-shuffle ()
  "Shuffle the current playlist."
  (interactive)
  (empv--cmd 'playlist-shuffle)
  (empv--display-event "Playlist shuffled."))

;;;###autoload
(defun empv-playlist-select ()
  "Select a playlist entry and play it."
  (interactive)
  (empv--playlist-select-item-and
   (empv-playlist-play item)))

;;;###autoload
(defun empv-playlist-loop-on ()
  "Turn on loop for playlist."
  (interactive)
  (empv--cmd 'set_property '(loop-playlist inf))
  (empv--display-event "↻ Playlist loop on."))

;;;###autoload
(defun empv-playlist-loop-off ()
  "Turn off loop for playlist."
  (interactive)
  (empv--cmd 'set_property '(loop-playlist no))
  (empv--display-event "Playlist loop off."))

(defun empv-toggle-playlist-loop ()
  "Turn on or off the loop for current file."
  (interactive)
  (empv--transform-property 'loop-playlist
    (lambda (it) (if (equal it "inf") 'no 'inf))))

(defun empv--playlist-apply (fn &rest args)
  "Call FN with the current playlist, and the extra ARGS.

Example:
 (empv--playlist-apply
  (lambda (playlist arg1 arg2)
   (message \"Got a playlist with %d songs!\" (length playlist))))"
  (empv--let-properties '(playlist)
    (let ((files (mapcar (lambda (it) (alist-get 'filename it)) .playlist)))
      (apply fn files args))))

(defun empv--playlist-save-to-file (playlist &optional filename)
  "Save the PLAYLIST content to FILENAME."
  (with-temp-buffer
    (insert (mapconcat #'identity playlist "\n"))
    (let* ((pl-name-regex "empv-playlist-\\(?1:[[:digit:]]+\\)\\.m3u")
           (files (directory-files empv-playlist-dir nil "empv-playlist-\\(?1:[[:digit:]]+\\)\\.m3u"))
           (pl-last (when files (file-name-sans-extension (car (last files)))))
           (num (if pl-last (1+ (string-to-number (if (string-match pl-name-regex pl-last) (match-string 1 pl-last) "0"))) 0))
           (filename (or filename (expand-file-name (format "empv-playlist-%d.m3u" num) empv-playlist-dir))))
      (write-file filename))))

;;;###autoload
(defun empv-playlist-save-to-file (&optional filename)
  "Save the current playlist to FILENAME."
  (interactive
   (list
    (let ((fname (read-file-name "Save playlist to: " (file-name-as-directory empv-playlist-dir))))
      (when (and fname (not (string-empty-p fname)))
        fname))))
  (empv--playlist-apply #'empv--playlist-save-to-file filename))


;;; Interactive - Misc

(defun empv--format-clock (it)
  (format "%02d:%02d" (floor (/ it 60)) (% (floor it) 60)))

(defun empv--format-chapter (chapter-list chapter time-pos duration)
  ;; NOTE The documentation says current chapter should be marked in
  ;; chapter-list but it was not in my case. Hence, I use chapter
  ;; property to get which chapter we are currently on.
  (let* ((current-chapter (nth chapter chapter-list))
         (next-chapter (nth (1+ chapter) chapter-list))
         (current-chapter-time-pos (- time-pos (alist-get 'time current-chapter)))
         (current-chapter-duration (- (or (alist-get 'time next-chapter) duration) (alist-get 'time current-chapter))))
    (format
     "(%s: %s, %s of %s (%%%d))"
     (propertize "Chapter" 'face 'italic)
     (propertize (alist-get 'title current-chapter chapter) 'face 'underline)
     (empv--format-clock current-chapter-time-pos)
     (empv--format-clock current-chapter-duration)
     (* (/ current-chapter-time-pos current-chapter-duration) 100))))

;;;###autoload
(defun empv-display-current (arg)
  "Display currently playing item's title and media player state.
If ARG is non-nil, then also put the title to `kill-ring'."
  (interactive "P")
  (empv--let-properties '( playlist-pos-1 playlist-count
                           time-pos percent-pos duration
                           metadata media-title path
                           pause paused-for-cache loop-file loop-playlist
                           chapter chapter-list
                           volume option-info/volume/default-value
                           speed option-info/volume/default-value)
    (let ((title (string-trim (empv--create-media-summary-for-notification .metadata .path .media-title)))
          (state (cond
                  ((eq .paused-for-cache t) (propertize "Buffering..." 'face '(:foreground "yellow")))
                  ((eq .pause t) (propertize "Paused" 'face '(:foreground "grey")))
                  (t (propertize "Playing" 'face '(:foreground "green")))))
          (empv-metadata (empv--extract-empv-metadata-from-path .path)))
      (empv--display-event
       "[%s%s, %s of %s (%d%%), %s/%s%s%s%s%s] %s %s"
       state
       ;; Show a spinning icon near state, indicating that current
       ;; playlist item is in loop.
       (if (eq :json-false .loop-file) "" " ↻")
       (empv--format-clock (or .time-pos 0))
       (empv--format-clock (or .duration 0))
       (or .percent-pos 0)
       .playlist-pos-1
       .playlist-count
       ;; Show a spinning icon near playlist count, indicating that
       ;; current playlist itself is on loop.
       (if (eq :json-false .loop-playlist) "" " ↻")
       ;; If it's a radio being played, show the radio name too
       (if (plist-get empv-metadata :radio)
           (concat ", " (propertize (plist-get empv-metadata :title) 'face 'italic))
         "")
       (if (not (= .volume (or .option-info/volume/default-value 100)))
           (concat ", " (propertize (format "🔊 %s" (floor .volume)) 'face 'bold))
         "")
       (if (not (= .speed (or .option-info/speed/default-value 1)))
           (concat ", " (propertize (format "⏩ %s" .speed) 'face 'bold))
         "")
       (propertize title 'face 'bold)
       (if .chapter
           (empv--format-chapter .chapter-list .chapter .time-pos .duration)
         ""))
      (when arg
        (kill-new title)))))

(defun empv-copy-path ()
  "Copy the path of currently playing item."
  (interactive)
  (empv--let-properties '(path)
    (empv--display-event "URI copied: %s" (empv--clean-uri .path))
    (kill-new (empv--clean-uri .path))))


;;; Interactive - Chapters

(defun empv-chapter-prev ()
  "Seek to previous chapter in current media file."
  (interactive)
  (empv--transform-property 'chapter #'1-))

(defun empv-chapter-next ()
  "Seek to next chapter in current media file."
  (interactive)
  (empv--transform-property 'chapter #'1+))

(defun empv-chapter-select ()
  "Select a chapter to seek in current media file."
  (interactive)
  (empv--let-properties '(chapter chapter-list)
    (let* ((chapters (seq-map-indexed (lambda (it it-index) `(,@it (id . ,it-index))) .chapter-list))
           (result (alist-get
                    'id
                    (empv--completing-read-object
                     "Select chapter: "
                     chapters
                     :formatter (lambda (it)
                                  (let ((id (alist-get 'id it)))
                                    (format "%s %s"
                                            (alist-get 'title it id)
                                            (or (and (= .chapter id) empv--playlist-current-indicator) ""))))
                     :sort? nil))))
      (empv--cmd 'set_property `(chapter ,result)))))


;;; Radio

(defun empv--play-radio-channel (channel &optional ask)
  (let* ((data (list :title (car channel) :url (cdr channel) :radio t))
         (url (format "%s%s%s" (cdr channel) empv--title-sep (prin1-to-string data))))
    (if ask
        (empv-play-or-enqueue url)
      (empv-play url))))

;;;###autoload
(defun empv-play-radio ()
  "Play radio channels."
  (interactive)
  (empv--with-empv-metadata
   (empv--play-radio-channel
    (empv--completing-read-object
     "Channel: "
     empv-radio-channels
     :formatter (lambda (x) (if (equal (cdr x) (plist-get empv-metadata :uri))
                           (format "%s %s" (car x) empv--playlist-current-indicator)
                         (car x)))
     :category 'empv-radio-item)
    t)))

;;;###autoload
(defun empv-play-random-channel ()
  "Play a random radio channel."
  (interactive)
  (let ((channel (thread-last
                   empv-radio-channels
                   (length)
                   (random)
                   (empv-flipcall #'nth empv-radio-channels))))
    (empv--display-event "Playing %s" (car channel))
    (empv--play-radio-channel channel)))


;;; YouTube/Invidious

(defun empv--format-yt-views (view-count)
  (format "%.2sK views" (/ view-count 1000.0)))

(defun empv--format-yt-duration (seconds)
  (let ((hours (/ seconds 3600))
        (minutes (mod (/ seconds 60) 60))
        (remaining-seconds (mod seconds 60)))
    (cond
     ((< hours 1) (format "%02d:%02d" minutes remaining-seconds))
     (t (format "%d:%02d:%02d" hours minutes remaining-seconds)))))

(defun empv--format-yt-item (it)
  "Format IT into `(\"formatted video title\" . it)'."
  (let-alist it
    (pcase (alist-get 'type it)
      ("video" (format "[%s] %s"
                       (propertize (format "%s, %s"
                                           (empv--format-yt-views .viewCount)
                                           (empv--format-yt-duration .lengthSeconds))
                                   'face
                                   'italic)
                       .title))
      ("playlist" (format "[%s] %s"
                          (propertize (format "%s videos by %s"
                                              .videoCount
                                              .author)
                                      'face
                                      'italic)
                          .title)))))

(defun empv--request-format-param (pair)
  "Format given PAIR into a URL parameter."
  (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))

(defun empv--request (url params callback)
  "Send a GET request to given URL and return the response body.
PARAMS should be an alist.  CALLBACK is called with the resulting JSON
object."
  (let* ((url-params (string-join (mapcar #'empv--request-format-param params) "&"))
         (full-url (format "%s?%s" url url-params)))
    (url-retrieve
     full-url
     (lambda (_status)
       (let ((result (progn
                       (goto-char (point-min))
                       (re-search-forward "\n\n" nil t)
                       (delete-region (point-min) (point))
                       (decode-coding-string
                        (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))
         (kill-buffer)
         (funcall callback (empv--read-result result)))))))

(defun empv--request-raw-sync (url)
  "Retrieve URL's body synchronously as string."
  (when (string-empty-p url)
    (error 'wrong-type-argument))
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((result
           (progn
             (goto-char (point-min))
             (re-search-forward "\n\n" nil t)
             (delete-region (point-min) (point))
             (set-buffer-multibyte t)
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (buffer-substring-no-properties (point-min) (point-max)))))
      result)))

;; TODO: Add [NEXT] item to load next page of results
(defun empv--youtube-search (term type callback)
  "Search TERM in YouTube.
TYPE determines what to search for, it's either video or
playlist.  By default it's video.  Call CALLBACK when request
finishes."
  (setq type (symbol-name (or type 'video)))
  (empv--request
   (format "%s/search" empv-invidious-instance)
   `(("q" . ,term)
     ("type" . ,type))
   callback))

(defun empv--youtube-item-extract-link (item)
  "Find and return YouTube url for ITEM."
  (let ((video-id (alist-get 'videoId item))
        (playlist-id (alist-get 'playlistId item)))
    (format
     "https://youtube.com/%s=%s%s%s"
     (if video-id "watch?v" "playlist?list")
     (or video-id playlist-id)
     empv--title-sep
     (prin1-to-string (list :title (alist-get 'title item) :youtube t)))))

(defun empv--youtube (term type)
  "Search TERM in YouTube.
See `empv--youtube-search' for TYPE."
  (let ((use-tabulated empv-youtube-use-tabulated-results))
    (empv--youtube-search
     term type
     (lambda (results)
       (setq empv--last-youtube-candidates results)
       (if use-tabulated
           (empv-youtube-tabulated-last-results)
         (empv-youtube-last-results))))))

;;;###autoload
(defun empv-youtube (term)
  "Search TERM in YouTube videos."
  (interactive (list (empv--yt-suggest "Search in YouTube videos: ")))
  (empv--youtube term 'video))

;;;###autoload
(defun empv-youtube-tabulated (term)
  "Search TERM in YouTube videos.
Show results in a tabulated buffers with thumbnails."
  (interactive (list (empv--yt-suggest "Search in YouTube videos: ")))
  (let ((empv-youtube-use-tabulated-results t))
    (empv--youtube term 'video)))

;;;###autoload
(defun empv-youtube-playlist (term)
  "Search TERM in YouTube playlists."
  (interactive (list (empv--yt-suggest "Search in YouTube videos: ")))
  (empv--youtube term 'playlist))

(defun empv-youtube-show-current-comments ()
  "Show YouTube comments for currently playing (or paused) YouTube video."
  (interactive)
  (empv--let-properties '(path)
    (empv-youtube-show-comments .path)))

(declare-function emojify-mode "emojify")
(defun empv-youtube-show-comments (video-id)
  "Show comments of a YouTube VIDEO-ID in a nicely formatted org buffer.
VIDEO-ID can be either a YouTube URL or just a YouTube ID."
  (interactive "sURL or ID: ")
  (setq video-id (replace-regexp-in-string "^.*v=\\([A-Za-z0-9_-]+\\).*" "\\1" video-id))
  (empv--request
   (format "%s/comments/%s" empv-invidious-instance video-id)
   '()
   (lambda (result)
     (let ((buffer (get-buffer-create (format "*empv-yt-comments-%s*" video-id))))
       (switch-to-buffer-other-window buffer)
       (with-current-buffer buffer
         (erase-buffer)
         (org-mode)
         (when (require 'emojify nil t)
           (emojify-mode))
         (seq-map
          (lambda (comment)
            (let-alist comment
              (insert (format "* %s (👍 %s)%s\n%s\n"
                              .author .likeCount (if .creatorHeart " ❤️" "") .content))))
          (alist-get 'comments result))
         (goto-char (point-min)))))))


;;; Videos and music

(defun empv--find-files-1 (path extensions &optional depth)
  "Find files with given EXTENSIONS under given PATH.
PROMPT is shown when `completing-read' is called."
  (let ((default-directory path))
    (thread-last
      extensions
      (mapcar (lambda (ext) (format "-e '%s' " ext)))
      (string-join)
      (concat (format "fd . --absolute-path --max-depth %s " (or depth empv-max-directory-search-depth)))
      (shell-command-to-string)
      (empv-flipcall #'split-string "\n")
      (empv-seq-init))))

(defun empv--find-files (path extensions &optional depth)
  "Like `empv--find-files-1' but PATH can be a list."
  (if (listp path)
      (seq-mapcat (lambda (it) (empv--find-files-1 it extensions depth)) (seq-filter (lambda (it) (file-directory-p it)) path))
    (empv--find-files-1 path extensions depth)))

(defun empv--select-file (prompt path extensions &optional depth)
  "Select a file interactively under given PATH.

Only searches for files with given EXTENSIONS.
PROMPT is shown to user while selecting.
Limit directory traversal at most DEPTH levels.  By default it's
`empv-max-directory-search-depth'"
  (expand-file-name
   (empv--completing-read-object
    prompt
    (empv--find-files path extensions depth)
    :formatter #'abbreviate-file-name
    :category 'file)))

(defun empv--select-files (prompt path extensions &optional depth)
  "Select files interactively under given PATH.

Only searches for files with given EXTENSIONS.
PROMPT is shown to user while selecting.
Limit directory treversal at most DEPTH levels.  By default it's
`empv-max-directory-search-depth'"
  (seq-map
   (lambda (it) (expand-file-name it path))
   (empv--completing-read-object
    prompt
    (empv--find-files path extensions depth)
    :multiple? t
    :formatter #'abbreviate-file-name
    :category 'file)))

;;;###autoload
(defun empv-play-video ()
  "Interactively select and play a video file from `empv-video-dir'."
  (interactive)
  (empv--with-video-enabled
   (empv-play-or-enqueue
    (empv--select-file "Select a video file" empv-video-dir empv-video-file-extensions))))

;;;###autoload
(defun empv-play-audio ()
  "Interactively select and play an audio file from `empv-audio-dir'."
  (interactive)
  (empv-play-or-enqueue
   (empv--select-file "Select an audio file:" empv-audio-dir empv-audio-file-extensions)))


;;; empv-youtube-results-mode

(defvar empv-youtube-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "P") #'empv-youtube-results-play-current)
    (define-key map (kbd "a") #'empv-youtube-results-enqueue-current)
    (define-key map (kbd "Y") #'empv-youtube-results-copy-current)
    (define-key map (kbd "c") #'empv-youtube-results-show-comments)
    (define-key map (kbd "i") #'empv-youtube-results-inspect)
    (define-key map (kbd "RET") #'empv-youtube-results-play-or-enqueue-current)
    ;; TODO: add quick help for ? binding
    map)
  "Keymap for `empv-youtube-results-mode'.")

(declare-function evil-set-initial-state "evil-core")
(define-derived-mode empv-youtube-results-mode tabulated-list-mode "empv-youtube-results-mode"
  "Major mode for interacting with YouTube results with thumbnails."
  (setq tabulated-list-padding 3)
  (setq tabulated-list-format [("Thumbnail" 20 nil)
                               ("Title" 60 t)
                               ("Length"  10 t)
                               ("Views" 10 t)])
  (when (require 'evil nil t)
    (evil-set-initial-state 'empv-youtube-results-mode 'emacs)))

(defadvice tabulated-list-sort (after empv-tabulated-list-sort-after activate)
  (when (derived-mode-p 'empv-youtube-results-mode)
    (iimage-recenter)))

(defun empv--youtube-show-tabulated-results (candidates)
  (with-current-buffer (get-buffer-create "*empv-yt-results*")
    (empv-youtube-results-mode)
    (setq tabulated-list-entries
          (seq-map-indexed
           (lambda (it index)
             (let* ((video-info (cdr it))
                    (video-title (propertize (alist-get 'title video-info) 'empv-youtube-item it))
                    (video-view (empv--format-yt-views (alist-get 'viewCount video-info)))
                    (video-length (empv--format-yt-duration (alist-get 'lengthSeconds video-info))))
               (list index (vector "<THUMBNAIL>" video-title video-length video-view))))
           candidates))
    (tabulated-list-init-header)
    (when empv-youtube-thumbnail-quality
      (empv--youtube-tabulated-load-thumbnails candidates))
    (tabulated-list-print)
    (back-to-indentation)
    (pop-to-buffer-same-window (current-buffer))))

(defun empv--youtube-tabulated-load-thumbnails (candidates)
  (let ((total-count (length candidates))
        (completed-count 0)
        (buffer (current-buffer)))
    (seq-do-indexed
     (lambda (video index)
       (let* ((video-info (cdr video))
              (video-id (alist-get 'videoId video-info))
              (filename (format
                         (expand-file-name "~/.cache/empv_%s_%s.jpg")
                         video-id
                         empv-youtube-thumbnail-quality))
              (thumb-url (thread-last
                           video-info
                           (alist-get 'videoThumbnails video-info)
                           (seq-find (lambda (thumb)
                                       (equal empv-youtube-thumbnail-quality
                                              (alist-get 'quality thumb))))
                           (cdr)
                           (alist-get 'url)))
              (args (seq-filter
                     #'identity
                     (list
                      (if (file-exists-p filename)
                          "printf" "curl")
                      (if empv-allow-insecure-connections
                          "--insecure" nil)
                      "-L"
                      "-o"
                      filename
                      thumb-url))))
         (empv--dbg "Dowloading thumbnail using: '%s'" args)
         (set-process-sentinel
          (apply #'start-process
                 (format "empv-download-process-%s" video-id)
                 "*empv-thumbnail-downloads*"
                 args)
          (lambda (_ _)
            (empv--dbg "Download finished for image index=%s, url=%s, path=%s" index thumb-url filename)
            (with-current-buffer buffer
              (setf
               (elt (car (alist-get index tabulated-list-entries)) 0)
               (format "[[%s]]" filename))
              (setq completed-count (1+ completed-count))
              (when (eq completed-count total-count)
                (tabulated-list-print)
                (iimage-mode)
                (back-to-indentation)))))))
     candidates)))

(defun empv-youtube-results--current-item ()
  (save-excursion
    (beginning-of-line)
    (tabulated-list-next-column)
    (tabulated-list-next-column)
    (get-text-property (point) 'empv-youtube-item)))

(defun empv-youtube-results--current-video-url ()
  (empv--youtube-item-extract-link (empv-youtube-results--current-item)))

(defun empv-youtube-results-play-current ()
  "Play the currently selected video in `empv-youtube-results-mode'."
  (interactive)
  (empv-play (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-enqueue-current ()
  "Enqueue the currently selected video in `empv-youtube-results-mode'."
  (interactive)
  (empv-enqueue (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-play-or-enqueue-current ()
  "Play or enqueue the currently selected video in `empv-youtube-results-mode'."
  (interactive)
  (empv-play-or-enqueue (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-copy-current ()
  "Copy the URL of the currently selected video in `empv-youtube-results-mode'."
  (interactive)
  (empv-youtube-copy-link (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-show-comments ()
  "Show comments of the currently selected video in `empv-youtube-results-mode'."
  (interactive)
  (empv-youtube-show-comments (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-inspect ()
  "Inspect the currently selected video in `empv-youtube-results-mode'.
This simply shows the data returned by the invidious API in a
nicely formatted buffer."
  (interactive)
  (empv--inspect-obj (empv-youtube-results--current-item)))

(defalias 'empv-youtube-become-tabulated #'empv-youtube-tabulated-last-results)

(defun empv-youtube-tabulated-last-results ()
  "Show last search results in tabulated mode with thumbnails."
  (interactive)
  (empv--youtube-show-tabulated-results empv--last-youtube-candidates))

(defun empv-youtube-last-results ()
  "Show and act on last search results."
  (interactive)
  (ignore-error (quit minibuffer-quit)
    (thread-last
      (empv--completing-read-object
       "YouTube results"
       empv--last-youtube-candidates
       :formatter #'empv--format-yt-item
       :category 'empv-youtube-item
       :sort? nil)
      (empv--youtube-item-extract-link)
      (empv-play-or-enqueue))))


;;; empv utility

(defun empv-override-quit-key ()
  "Override `q' key to \"pause and hide video\" action.

This function overrides the `q' key so that you dont accidentaly
quit mpv, resulting in a loss of your current playlist.

Instead of quitting mpv, it hides the video view (just like the
`_' binding) and pauses the playback.  If you want to hide the
video view, without pausing you can still use `_' key binding.  To
really exit, you can still use `empv-exit' function.

To make this behavior permanant, add the following to your init file:

    (add-hook \\='empv-init-hook #\\='empv-override-quit-key)"
  (empv--cmd
   'keybind `("q" ,(format "set pause yes;%s cycle video"
                           (if empv-reset-playback-speed-on-quit
                               "set speed 1;"
                             "")))))


(defvar org-link-any-re)
(declare-function org-element-property "org")
(declare-function org-element-context "org")
(declare-function shr-url-at-point "shr")
(defun empv-media-at-point ()
  "Return the potential media item at the point.
It may be an absolute filepath, it may be a relative file
path. No guarantees."
  (or
   (when (eq major-mode 'org-mode)
     (ignore-errors (org-element-property :path (org-element-context))))
   (ignore-errors (shr-url-at-point nil))
   (thing-at-point 'url)
   (thing-at-point 'existing-filename)
   (thing-at-point 'filename)))

(defun empv-play-media-at-point ()
  "Play the media at point."
  (interactive)
  (empv-play-or-enqueue (empv-media-at-point)))

(defalias 'empv-play-thing-at-point #'empv-play-media-at-point)


;; Lyrics manager

(defvar empv-lyrics-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'empv-lyrics-save)
    (define-key map (kbd "C-c C-w") #'empv-lyrics-force-web)
    map)
  "Keymap for `empv-lyrics-display-mode'.")

(define-derived-mode empv-lyrics-display-mode text-mode "empv-lyrics-display-mode"
  "Major mode for displaying lyrics of a given song.")

(defun empv--lyrics-display (path title lyrics)
  (with-current-buffer (get-buffer-create "*empv-lyrics*")
    (empv-lyrics-display-mode)
    (erase-buffer)
    (insert lyrics)
    (setq header-line-format (format "Lyrics :: %s" title))
    (setq empv--current-file path)
    (goto-char (point-min))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window (current-buffer)))))

(defun empv--lyrics-make-search-url (song)
  "Generate a search URL for the given SONG."
  (url-encode-url (format "%s%s lyrics" empv-search-prefix song)))

;; TODO Make this async? Not quite sure if it does worth or not
;; This function is completely fucked up *and* it works, most of the time.
(defun empv--lyrics-download (song)
  (ignore-error wrong-type-argument
    (thread-last
      (empv--request-raw-sync (empv--lyrics-make-search-url song))
      ;; Find all the links in the response first
      (s-match-strings-all "https\\(://\\|%3A%2F%2F\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]+[-A-Za-z0-9+&@#/%=~_|]")
      (mapcar #'car)
      (mapcar (lambda (it) (thread-last
                        (string-trim-right it "&amp.*")
                        (string-replace "%3A%2F%2F" "://")
                        (string-replace "%2F" "/")
                        (string-replace "%2D" "-")
                        (string-replace "%2D" "+"))))
      ;; Then find the first sturmgeweiht|azlyrics|genius link
      (seq-find (lambda (it) (s-matches? "^https?://.*\\(sturmgeweiht.de/texte/.*titel\\|flashlyrics.com/lyrics/\\|lyrics.az/.*.html\\|azlyrics.com/lyrics/\\)" it)))
      (url-unhex-string)
      (empv--request-raw-sync)
      (string-replace "" "")
      ;; Replace newlines so that regexes can work
      (string-replace "\n" "<newline>")
      ;; FIXME: The resulting string may be too long and regexes may
      ;; fail due to stack overflow errors
      ((lambda (it)
         (or
          (s-match "<div class=\"inhalt\">\\(.*\\)<a href=\"" it) ;; sturmgeweiht
          (s-match "Sorry about that\\. -->\\(.*\\)<!-- MxM banner -->" it) ;; azlyrics
          ;; (s-match "window.__PRELOADED_STATE__ =.*\\\\\"html\\\\\":\\\\\"\\(.*\\)\\\\\",\\\\\"children\\\\\":" it) ;; genius
          (s-match "x-ref=\"lyric_text\">\\(.*\\)</p>" it) ;; lyrics.az
          (s-match "<div class=\"main-panel-content\".*?>\\(.*\\)<div class=\"sharebar-wrapper\"" it) ;; flashlyrics
          )))
      (nth 1)
      (s-replace-all '(("<br>" . "\n")
                       ("<br/>" . "\n")
                       ("<br />" . "\n")
                       ("\\n" . "\n")
                       ("<div>" . "")
                       ("</div>" . "")
                       ("\\" . "")
                       ("<newline>" . "\n")
                       ("" . "\n")
                       ("\"" . "")
                       ("&#x27;" . "'")
                       ("&#039;" . "'")))
      (s-replace "\n\n" "\n")
      (s-split "\n")
      (mapcar #'s-trim)
      ;; Remove some lines
      (seq-filter (lambda (it) (not (s-contains? "adsbygoogle" it))))
      (s-join "\n")
      (s-trim)
      ;; Clear all remaning html tags
      (replace-regexp-in-string "<[^>]*>" ""))))

(defun empv--lyrics-from-metadata (metadata)
  "Fetch lyrics from METADATA.
Tries to find a key in METADATA that contains \"lyrics\" as there
is no standard key for lyrics."
  (when-let (lyrics-key (seq-find
                         (lambda (key) (string-match-p "lyrics" (symbol-name key)))
                         (map-keys metadata)))
    (map-elt metadata lyrics-key)))

(defun empv--lyrics-on-not-found (song)
  "Prompt the user to search the web for SONG lyrics if they are not found."
  (when (y-or-n-p ">> Lyrics not found.  Do you want to search the web?")
    (browse-url (empv--lyrics-make-search-url song))))

(defun empv-lyrics-force-web ()
  "Fill buffer with lyrics downloaded from web.
This does not save lyrics to file.  Call `empv-lyrics-save' to
really save."
  (interactive nil empv-lyrics-display-mode)
  (empv--with-media-info
   (if-let (lyrics (empv--lyrics-download .media-title))
       (empv--lyrics-display .path .media-title lyrics)
     (empv--lyrics-on-not-found .media-title))))

(defun empv-lyrics-save (file lyrics)
  "Save LYRICS into FILEs ID3 lyrics tag.
If you are in a `*empv-lyrics*' buffer and call this function
interactively, it will automatically update the currently shown
lyrics with the buffers content."
  (interactive
   (list (or empv--current-file (read-file-name "Audio file: "))
         (if (eq major-mode 'empv-lyrics-display-mode)
             (buffer-string)
           (read-string "Lyrics: "))))
  (setq file (expand-file-name file))
  (unless (file-exists-p file)
    (user-error "File not found: '%s'" file))
  (let ((lyrics-file (make-temp-file "empv-lyrics" nil ".txt" lyrics)))
    (set-process-filter
     (start-process "*empv-eyeD3*" nil "eyeD3" "--encoding" "utf8" "--add-lyrics" lyrics-file file)
     (lambda (proc out)
       (empv--dbg "*eyeD3* output: %s" out)
       (if (eq (process-exit-status proc) 0)
           (empv--display-event "Lyrics saved for '%s'" file)
         (error "Failed to save lyrics into '%s'.  exit-code=%s, output=%s"
                file (process-exit-status proc) out))))))

(defun empv-lyrics-current ()
  "Get lyrics for currently playing song.
This tries to extract the lyrics from the file metada first and
if it can't find one then downloads it from the web."
  (interactive)
  (empv--with-media-info
   (if-let (metadata-lyrics (empv--lyrics-from-metadata .metadata))
       (empv--lyrics-display .path .media-title metadata-lyrics)
     (if-let* ((web-lyrics (empv--lyrics-download .media-title)))
         (progn
           (empv--lyrics-display .path .media-title web-lyrics)
           (when (and empv-lyrics-save-automatically (file-exists-p (expand-file-name .path)))
             (empv-lyrics-save .path web-lyrics)))
       (empv--lyrics-on-not-found .media-title)))))

(defun empv-lyrics-show (song)
  "Show lyrics for SONG in a seperate buffer.
This function searches the web for SONG lyrics.  If you want to
get the lyrics for currently playing/paused song, use
`empv-lyrics-current'."
  (interactive "sSong title: ")
  (if-let (lyrics (empv--lyrics-download song))
      (empv--lyrics-display nil song lyrics)
    (empv--lyrics-on-not-found song)))


;; Actions, mainly for embark but used in other places too

(defun empv-playlist-play (item)
  (empv--cmd-seq
   ('playlist-play-index (alist-get 'index item))
   ('set_property '(pause :json-false))))

(defun empv-playlist-remove (item)
  (empv--cmd 'playlist-remove (alist-get 'index item)))

(defun empv-playlist-remove-others (item)
  (empv--cmd 'loadfile (list (alist-get 'filename item) 'replace)))

(defun empv-playlist-move (item)
  (let ((index (alist-get 'index item)))
    (empv--select-action "Move to"
      "Top"    → (empv--cmd 'playlist-move (list index 0))
      "Bottom" → (empv--cmd 'playlist-move (list index 1000))
      "Next"   → (empv--let-properties '(playlist-pos)
                   (empv--cmd 'playlist-move (list index (1+ .playlist-pos))))
      "Index"  → (let ((i (read-number "Index: ")))
                   (empv--cmd 'playlist-move (list index i))))
    (empv--display-event "Moved %s." (abbreviate-file-name (alist-get 'filename item)))))

(defun empv-youtube-copy-link (link)
  (empv--display-event "URI copied: %s" (empv--clean-uri link))
  (kill-new (empv--clean-uri link)))

(defun empv-playlist-copy-path (item)
  (let ((path (alist-get 'filename item)))
    (empv--display-event "URI copied: %s" (empv--clean-uri path))
    (kill-new (empv--clean-uri path))))


;; Embark integration

(defvar embark-file-map)
(defvar embark-keymap-alist)
(defvar embark-url-map)
(defvar embark-post-action-hooks)
(defvar embark-transformer-alist)
(defvar embark-general-map)

(defun empv--embark-youtube-item-transformer (type target)
  "Extract the YouTube URL from TARGET without changing it's TYPE."
  (cons type (empv--youtube-item-extract-link (get-text-property 0 'empv-item target))))

(defun empv--embark-radio-item-transformer (type target)
  "Extract the radio URL from TARGET without changing it's TYPE."
  (cons type (cdr (get-text-property 0 'empv-item target))))

(defun empv--embark-playlist-item-transformer (type target)
  "Extract the item object from TARGET without changing it's TYPE."
  (cons type (get-text-property 0 'empv-item target)))

(with-eval-after-load 'embark
  (defvar-keymap empv-playlist-item-action-map
    :doc "Action map for playlist items, utilized by Embark."
    :parent embark-general-map
    "RET" #'empv-playlist-play
    "p" #'empv-playlist-play
    "y" #'empv-playlist-copy-path
    "m" #'empv-playlist-move
    "r" #'empv-playlist-remove
    "R" #'empv-playlist-remove-others)

  (defvar-keymap empv-radio-item-action-map
    :doc "Action map for radio items, utilized by Embark."
    :parent embark-general-map
    "RET" #'empv-play
    "p" #'empv-play
    "e" #'empv-enqueue
    "n" #'empv-enqueue-next)

  (defvar-keymap empv-youtube-item-action-map
    :doc "Action map for YouTube items, utilized by Embark."
    :parent embark-general-map
    "RET" #'empv-play
    "p" #'empv-play
    "y" #'empv-youtube-copy-link
    "e" #'empv-enqueue
    "n" #'empv-enqueue-next
    "c" #'empv-youtube-show-comments
    "t" #'empv-youtube-become-tabulated)

  (add-to-list 'embark-keymap-alist '(empv-playlist-item . empv-playlist-item-action-map))
  (add-to-list 'embark-keymap-alist '(empv-radio-item . empv-radio-item-action-map))
  (add-to-list 'embark-keymap-alist '(empv-youtube-item . empv-youtube-item-action-map))

  (setf (alist-get 'empv-playlist-item embark-transformer-alist) #'empv--embark-playlist-item-transformer)
  (setf (alist-get 'empv-radio-item embark-transformer-alist) #'empv--embark-radio-item-transformer)
  (setf (alist-get 'empv-youtube-item embark-transformer-alist) #'empv--embark-youtube-item-transformer))

(defun empv-embark-initialize-extra-actions ()
  "Add empv actions like play, enqueue etc. to embark file and url actions.
This might override your changes to `embark-file-map' and
`embark-url-map', if there is any.  It also overrides a few
default embark actions.  Check out this functions definition to
learn more.  Supposed to be used like this:

  (with-eval-after-load \\='embark (empv-embark-initialize-extra-actions))"
  (define-key embark-file-map "p" 'empv-play)
  (define-key embark-file-map "n" 'empv-enqueue-next)
  (define-key embark-file-map "e" 'empv-enqueue) ;; overrides eww-open-file
  (define-key embark-url-map "p" 'empv-play)
  (define-key embark-url-map "e" 'empv-enqueue-next) ;; overrides eww
  (define-key embark-url-map "n" 'empv-enqueue))


;; Consult integration

(declare-function consult--read "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--async-split-initial "consult")
(declare-function consult--async-sink "consult")
(declare-function consult--async-refresh-immediate "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--async-split "consult")

(defun empv--consult-get-input-with-suggestions (prompt)
  "Get an input from user, using YouTube search suggestions.
PROMPT is passed to `completing-read' as-is."
  (consult--read
   (empv--consult-yt-search-generator)
   :prompt prompt
   :category 'empv-youtube
   :lookup (lambda (selected &rest _)
             (string-trim-left selected (consult--async-split-initial "")))
   :initial (consult--async-split-initial "")
   :sort nil
   :history 'empv--youtube-search-history
   :require-match nil))

(defun empv--consult-yt-search-generator ()
  (thread-first
    (consult--async-sink)
    (consult--async-refresh-immediate)
    (empv--consult-async-search)
    (consult--async-throttle)
    (consult--async-split)))

(defun empv--consult-async-search (next)
  "Async search provider for `consult-empv'.
This gets the suggestions based on the current action and returns
results to consult using NEXT."
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when (not (string-empty-p (string-trim action)))
         (empv--request
          (format "%s/search/suggestions" empv-invidious-instance)
          `(("q" . ,action))
          (lambda (result)
            (funcall next 'flush)
            (when result
              (funcall next (alist-get 'suggestions result)))))))
      (_ (funcall next action)))))

(defun empv--yt-suggest (prompt)
  (if (require 'consult nil t)
      (empv--consult-get-input-with-suggestions prompt)
    (read-string prompt nil 'empv--youtube-search-history)))



(provide 'empv)
;;; empv.el ends here
