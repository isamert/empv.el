;;; empv.el --- An Emacs interface for MPV -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamert@protonmail.com>
;; Version: 0.6
;; Homepage: https://github.com/isamert/empv.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

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

;; An Emacs interface for MPV

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'seq)
(require 'map)
(require 'json)
(require 'url)

(defgroup empv nil
  "A media player for Emacs"
  :group 'multimedia)

(defcustom empv-socket-file "/tmp/empv-socket"
  "Socket file path."
  :type 'string
  :group 'empv)

(defcustom empv-mpv-binary "mpv"
  "MPV binary path."
  :type 'string
  :group 'empv)

(defcustom empv-mpv-args `("--no-video" "--no-terminal" "--idle" ,(concat "--input-ipc-server=" empv-socket-file))
  "Args used while starting mpv.  This should contain --input-ipc-server=<empv-socket-file>, also --idle is recommended for keeping your state."
  :type 'list
  :group 'empv)

(defcustom empv-display-events t
  "Wheter to show events (like track changed, stopped etc.) at the bottom of the screen."
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
Otherwise simply use `completing-read'. You can still use
`empv-youtube-tabulated' or `consult-empv-youtube-tabulated'
commands if this variable is `nil'."
  :type 'boolean
  :group 'empv)

(defcustom empv-youtube-thumbnail-quality "default"
  "Default value for YouTube thumbnail quality."
  :type 'string
  :group 'empv)

(defcustom empv-audio-dir (or (getenv "$XDG_MUSIC_DIR") "~/Music")
  "The directory that you keep your music in."
  :type 'string
  :group 'empv)

(defcustom empv-video-dir (or (getenv "$XDG_VIDEOS_DIR") "~/Videos")
  "The directory that you keep your videos in."
  :type 'string
  :group 'empv)

(defcustom empv-radio-channels
  '(("SomaFM - Groove Salad" . "http://www.somafm.com/groovesalad.pls")
    ("SomaFM - Drone Zone" . "http://www.somafm.com/dronezone.pls")
    ("SomaFM - Sonic Universe" . "https://somafm.com/sonicuniverse.pls")
    ("SomaFM - Metal" . "https://somafm.com/metal.pls")
    ("SomaFM - Vaporwaves" . "https://somafm.com/vaporwaves.pls"))
  "List of radio channels -- or any other type of streamable.
Elements should pair in the form of: `(\"Channel name\" . \"stream-address\")'"
  :type 'list
  :group 'empv)

(defcustom empv-video-file-extensions '("mkv" "mp4" "avi" "mov")
  "List of video file extensions."
  :type 'list
  :group 'empv)

(defcustom empv-audio-file-extensions '("mp3" "ogg" "wav" "m4a" "flac" "aac")
  "List of video file extensions."
  :type 'list
  :group 'empv)

(defcustom empv-max-directory-search-depth 6
  "Max depth while searching in directories."
  :type 'number
  :group 'empv)

(defcustom empv-base-directory nil
  "Base directory.
Functions that shows directory-selection-prompts starts in this
directory.  nil means starting in `default-directory'."
  :type 'string
  :group 'empv)

(defcustom empv-log-events-to-file nil
  "Log all events to given file.
Supply a path to enable logging.  nil means no logging."
  :type 'string
  :group 'empv)

(defcustom empv-radio-log-format
  "* #{timestamp} [#{channel-name}] #{track-title}\n"
  "The format used when `empv-log-current-radio-song-name' is called.
`#{channel-name}', `#{timestamp}' and `#{track-title}' are
replaced with their current values at the time of calling."
  :type 'string
  :group 'empv)

(defcustom empv-radio-log-file
  "~/logged-radio-songs.org"
  "The file that is used by `empv-log-current-radio-song-name'."
  :type 'string
  :group 'empv)

(defcustom empv-allow-insecure-connections
  nil
  "Allow insecure connections (expired certificates etc.) while
doing network calls. This could be useful for being able to use
some invidious instances."
  :type 'boolean
  :group 'empv)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar empv-current-radio-channel
  nil
  "Currently playing radio channel.
The format is `(channel name . channel address)'.  This variable
does not clean itself up, it's possible that currently no radio
is playing but this variable is still holds some channel.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar empv--process nil)
(defvar empv--network-process nil)
(defvar empv--callback-table (make-hash-table :test 'equal))
(defvar empv--dbg nil)
(defvar empv--last-candidates '()
  "Latest candidates that are shown in a `completing-read' by empv.
Mainly used by embark actions defined in this package.")
(defvar empv--youtube-last-type nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv-flipcall (fn x y)
  "Flip arguments of given binary function FN and call it with Y and X."
  (funcall fn y x))

(defun empv-seq-init (seq)
  "Return the SEQ without the last element."
  (seq-subseq seq 0 (1- (seq-length seq))))

(defun empv--running? ()
  "Return if there is an mpv instance running or not."
  empv--process)

(defun empv--dbg (msg &rest rest)
  "Print MSG with REST if `empv--dbg' is non-nil."
  (when empv--dbg
    (apply 'message `(,(format "(empv) %s" msg) ,@rest))))

(defun empv--display-event (msg &rest rest)
  "Print MSG with REST if `empv-display-events' is non-nil."
  (let ((formatted-msg `(,(format "empv :: %s" msg) ,@rest)))
    (when empv-log-events-to-file
      (write-region
       (concat
        (format-time-string "[%Y-%m-%d %H:%M:%S] ")
        (apply 'format formatted-msg)
        "\n")
       nil empv-log-events-to-file 'append))
    (when empv-display-events
      (apply 'message formatted-msg))))

(defun empv--read-result (result)
  "Read JSON RESULT into an elisp structre."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string result)
                 (error (empv--dbg "Error while reading JSON :: %s" result) nil))))
    json))

(defun empv--new-request-id ()
  "Generate a new unique request id."
  (format "%s" (random)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--sentinel (_proc msg)
  "Clean up after connection closes with MSG."
  (empv--dbg "<< sentinel -> %s" msg)
  (empv--display-event "Closed.")
  (empv-exit))

(defun empv--filter (_proc incoming)
  "Filter INCOMING messages from the socket."
  (seq-do
   (lambda (it)
     (let* ((json-data (empv--read-result it))
            (id (map-elt json-data 'id))
            (request-id (map-elt json-data 'request_id))
            (callback (map-elt empv--callback-table (format "%s" (or request-id id))))
            (cb-result (and callback (funcall callback (cdr (assoc 'data json-data))))))
       (when cb-result
         (empv--dbg "<> Removing callback.")
         (map-delete empv--callback-table request-id))
       (empv--dbg
        "<< data: %s, request_id: %s, has-cb?: %s, remove-cb? :%s"
        json-data
        request-id
        (and callback t)
        cb-result)))
   (seq-filter
    (lambda (it) (not (string-empty-p it)))
    (seq-map
     #'string-trim
     (split-string incoming "\n")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process primitives
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--make-network-process ()
  "Create the network process for mpv."
  (setq empv--network-process
        (make-network-process :name "empv-network-process"
                              :family 'local
                              :service empv-socket-file
                              :sentinel #'empv--sentinel
                              :filter #'empv--filter)))

(defun empv--make-process (&optional uri)
  "Create the MPV process with given URI."
  (setq empv--process
        (make-process :name "empv-process"
                      :buffer nil
                      :command (if uri
                                   `(,empv-mpv-binary ,@empv-mpv-args ,uri)
                                 `(,empv-mpv-binary ,@empv-mpv-args)))))

(defun empv--send-command (command callback &optional event?)
  "Send COMMAND to mpv and call CALLBACK when mpv responds.
If EVENT? is non-nil, then the command is treated as an event
observer and the callback is called everytime that given event
happens."
  (when (empv--running?)
    (let* ((request-id (empv--new-request-id))
           (msg (json-encode
                 (if event?
                     `((command . (,(seq-first command) ,(string-to-number request-id) ,@(seq-rest command)))
                       (request_id . ,request-id))
                   `((command . ,command)
                     (request_id . ,request-id))))))
      (map-put! empv--callback-table request-id callback)
      (process-send-string empv--network-process (format "%s\n" msg))
      (empv--dbg ">> %s" msg)
      request-id)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential macros
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro empv--cmd (cmd &optional args &rest forms)
  "Run CMD with ARGS and then call FORMS with the result."
  `(if (listp ,args)
       (empv--send-command
        `(,,cmd ,@,args) (lambda (it) (ignore it) ,@forms t))
     (empv--send-command
      `(,,cmd ,,args) (lambda (it) (ignore it) ,@forms t))))

(defmacro empv--cmd-seq (&rest forms)
  (seq-reduce
   (lambda (acc it) `(empv--cmd ,(nth 0 it) ,(nth 1 it) (,@acc)))
   (reverse forms)
   '()))

(defmacro empv--observe (property &rest forms)
  "Observe PROPERTY and call FORMS when it does chage."
  `(empv--send-command
    '(observe_property ,property)
    (lambda (it)
      ,@forms
      nil)
    t))

(defmacro empv--run (&rest forms)
  "Start if mpv is not running already and then run FORMS."
  `(progn
     (when (not (empv--running?))
       (empv-start))
     ,@forms))

(defmacro empv--let-properties (props &rest forms)
  (declare (indent 1))
  `(let ((props-alist (make-hash-table))
         (prop-count (length ,props))
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--play-or-enqueue (uri)
  "Play or enqueue the URI based on user input.
URI might be a string or a list of strings."
  (interactive)
  (pcase (completing-read "Select action: " '("Play" "Enqueue"))
    ("Play"
     (cond
      ((stringp uri) (empv-play uri))
      ((listp uri) (empv--run
                    (empv--cmd
                     'stop nil
                     (seq-do #'empv-enqueue uri)
                     (empv-resume))))))
    ("Enqueue"
     (cond
      ((stringp uri) (empv-enqueue uri))
      ((listp uri) (seq-do #'empv-enqueue uri))))))

(defun empv--metadata-get (alist main fallback)
  "Get MAIN from ALIST, if it's nill get FALLBACK from ALIST."
  (or (cdr (assoc main alist)) (cdr (assoc fallback alist))))

(defun empv--extract-metadata (data)
  "Extract metadata into a consistent format using DATA."
  `((title  . ,(empv--metadata-get data 'title 'icy-title))
    (album  . ,(empv--metadata-get data 'album 'icy-album))
    (artist . ,(empv--metadata-get data 'artist 'icy-artist))
    (genre  . ,(empv--metadata-get data 'genre 'icy-genre))))

(defun empv--create-media-summary-for-notification (metadata)
  "Generate a formatted media title like \"Song name - Artist\" from given METADATA."
  (let-alist (empv--extract-metadata metadata)
    (when .title
      (format "%s %s %s"
              .title
              (or (and .artist "-") "")
              (or .artist "")))))

(defun empv--handle-metadata-change (data)
  "Display info about the current track using DATA."
  (empv--dbg "handle-metadata-change <> %s" data)
  (if-let ((media-title (empv--create-media-summary-for-notification data)))
      (empv--display-event "%s" media-title)
    (empv--let-properties '(media-title)
      (empv--display-event "%s" .media-title))))

(defun empv-start (&optional uri)
  "Start mpv using `empv-mpv-command' with given URI."
  (interactive)
  (when (not (empv--running?))
    (empv--dbg "Starting MPV.")
    (empv--make-process uri)
    ;; TODO: remove sleep
    (sleep-for 1.5)
    (empv--make-network-process)
    (empv--observe metadata (empv--handle-metadata-change it))))

(defun empv--format-playlist-item (item index)
  "Format given ITEM into a readable item.
INDEX is the place where the item appears in the playlist."
  (format
   "%s - %s%s"
   index
   (cdr (or (assoc 'title item) (assoc 'filename item)))
   (or (and (cdr (assoc 'current item)) " [CURRENT]") "")))

(defmacro empv--playlist-select-item-and (&rest forms)
  "Select a playlist item and then run FORMS with the input.
This function also tries to disable sorting in `completing-read' function."
  `(empv--run
    (empv--let-properties '(playlist)
      (let ((item (empv--completing-read
                   "Select track: "
                   (seq-map-indexed #'empv--format-playlist-item .playlist))))
        (ignore item)
        ,@forms))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive - Basics
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun empv-play (uri)
  "Play given URI. Add given URI to end of the current playlist and
immediately switch to it"
  (interactive "sEnter an URI to play: ")
  (if (empv--running?)
      (empv--cmd-seq
       ('loadfile (list uri 'append))
       ('get_property 'playlist-count)
       ('playlist-play-index (1- it))
       ('set_property '(pause :json-false)))
    (empv-start uri))
  (empv--display-event "Playing %s" uri))

(defun empv-resume ()
  "Resume the playback."
  (interactive)
  (empv--cmd 'set_property '(pause :json-false)))

;;;###autoload
(defun empv-play-file (path)
  "Play given PATH.
This is just a simple wrapper around `empv-play' that displays
`find-file' dialog if called interactively."
  (interactive "fPlay file: ")
  (empv--play-or-enqueue (expand-file-name path)))

;;;###autoload
(defun empv-play-directory (path)
  "Enqueue given files under PATH.
If called interactively, shows a prompt to select a directory.  By
default this directory is `default-directory'.  If you want this
function to start the prompt in same directory everytime, please
see `empv-base-directory'."
  (interactive
   (list (read-directory-name "Select directory to enqueue: " empv-base-directory nil t)))
  (thread-last (empv--find-files path (append empv-audio-file-extensions empv-video-file-extensions) 1)
    (mapcar (lambda (it) (expand-file-name it path)))
    (empv--play-or-enqueue)))

;;;###autoload
(defun empv-pause ()
  "Pause the playback."
  (interactive)
  (empv--cmd 'set_property '(pause t)))

;;;###autoload
(defun empv-toggle ()
  "Toggle the playback."
  (interactive)
  (empv--run
   (empv--cmd 'cycle 'pause)))

;;;###autoload
(defun empv-current-loop-on ()
  "Turn on loop for current file"
  (interactive)
  (empv--cmd 'set_property '(loop-file inf)))

;;;###autoload
(defun empv-current-loop-off ()
  "Turn off loop for current file"
  (interactive)
  (empv--cmd 'set_property '(loop-file no)))

;;;###autoload
(defun empv-volume-up ()
  "Up the volume to a max of 100%"
  (interactive)
  (empv--cmd-seq
   ('get_property 'volume)
	 ('set_property `(volume ,(min (+ it 5) 100)))))

;;;###autoload
(defun empv-volume-down ()
  "Down the volume to a min of 0%"
  (interactive)
  (empv--cmd-seq
   ('get_property 'volume)
	 ('set_property `(volume ,(max (- it 5) 0)))))

(defun empv-set-volume ()
  "Set the exact volume."
  (interactive)
  (empv--let-properties '(volume)
    (let* ((current (string-trim-right (number-to-string .volume) ".0"))
           (in (read-string (format "Volume (0-100, current %s): " current))))
      (empv--cmd 'set_property `(volume ,in)))))

;;;###autoload
(defun empv-toggle-video ()
  "Toggle the video display.
You can press \"_\" to hide it again when you are focused on
MPV."
  (interactive)
  (empv--run
   (empv--cmd 'cycle 'video)))

;;;###autoload
(defun empv-exit ()
  "Shut down mpv."
  (interactive)
  (when empv--process
    (setq empv--process (delete-process empv--process)))
  (when empv--network-process
    (setq empv--network-process (delete-process empv--network-process)))
  (setq empv--callback-table (make-hash-table :test 'equal)))

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

(defun empv-log-current-radio-song-name ()
  "Log current radio song name with the radio channel name.
The song's are logged into `empv-radio-log-file' with the format
that is defined in `empv-radio-log-format'."
  (interactive)
  (empv--let-properties '(metadata)
    (when-let ((title (alist-get 'icy-title .metadata)))
      (write-region
       (thread-last
         empv-radio-log-format
         (string-replace "#{timestamp}" (format "[%s]" (format-time-string "%Y-%m-%d %a %H:%M")))
         (string-replace "#{channel-name}" (car empv-current-radio-channel))
         (string-replace "#{track-title}" title))
       nil empv-radio-log-file 'append)
      (message "%s" title))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive - Playlist
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun empv-enqueue (uri)
  "Like `empv-play' but add the given URI to end of the playlist."
  (interactive "sEnter an URI to play: ")
  (empv--run
   (empv--cmd 'loadfile `(,uri append-play))
   (empv--display-event "Enqueued %s" uri)))

;;;###autoload
(defun empv-playlist-next ()
  "Play next in the playlist."
  (interactive)
  (empv--run
   (empv--cmd 'playlist-next)))

;;;###autoload
(defun empv-playlist-prev ()
  "Play previous in the playlist."
  (interactive)
  (empv--run
   (empv--cmd 'playlist-prev)))

;;;###autoload
(defun empv-playlist-clear ()
  "Clear the current playlist."
  (interactive)
  (empv--run
   (empv--cmd 'playlist-clear)))

;;;###autoload
(defun empv-playlist-shuffle ()
  "Shuffle the current playlist."
  (interactive)
  (empv--run
   (empv--cmd 'playlist-shuffle)))

;;;###autoload
(defun empv-playlist-select ()
  "Select a playlist entry and play it."
  (interactive)
  (empv--playlist-select-item-and
   (empv--cmd-seq
    ('playlist-play-index (car (split-string item " ")))
    ('set_property '(pause :json-false)))))

;;;###autoload
(defun empv-playlist-loop-on ()
  "Turn on loop for playlist"
  (interactive)
  (empv--cmd 'set_property '(loop-playlist inf)))

;;;###autoload
(defun empv-current-loop-off ()
  "Turn off loop for playlist"
  (interactive)
  (empv--cmd 'set_property '(loop-playlist no)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive - Misc
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun empv-display-current (arg)
  "Display currently playing item's title and media player state.
If ARG is non-nil, then also put the title to `kill-ring'."
  (interactive "P")
  (empv--let-properties '(playlist-pos-1 playlist-count percent-pos metadata media-title pause paused-for-cache)
    (let ((title (or (empv--create-media-summary-for-notification .metadata) .media-title))
          (state (cond
                  ((eq .paused-for-cache t) "Buffering...")
                  ((eq .pause t) "Paused")
                  (t "Playing"))))
      (empv--display-event "[%s] %s (%d%%, %s/%s)" state title (or .percent-pos 0) .playlist-pos-1 .playlist-count)
      (when arg
        (kill-new title)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Radio
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--play-radio-channel (channel &optional ask)
  (setq empv-current-radio-channel channel)
  ;; See #6
  (let ((url (format "%s# %s" (cdr channel) (car channel))))
    (if ask
        (empv--play-or-enqueue url)
      (empv-play url))))

(defun empv--get-radio-url ()
  "Get a radio channel URL from the user."
  (assoc-string
   (empv--completing-read "Channel: " empv-radio-channels :sort t)
   empv-radio-channels))

;;;###autoload
(defun empv-play-radio ()
  "Play radio channels."
  (interactive)
  (empv--play-radio-channel (empv--get-radio-url) t))

;;;###autoload
(defun empv-play-random-channel ()
  "Play a random radio channel."
  (interactive)
  (let ((channel (thread-last empv-radio-channels
                   (length)
                   (random)
                   (empv-flipcall #'nth empv-radio-channels))))
    (empv--display-event "Playing %s" (car channel))
    (empv--play-radio-channel channel)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YouTube/Invidious
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--format-yt-item (it)
  "Format IT into `(\"formatted video title\" . it)'."
  (let-alist it
    (cons
     (pcase (alist-get 'type it)
       ("video" (format "[%s] %s"
                        (propertize (format "%.2sK views, %.2s mins"
                                            (/ .viewCount 1000.0)
                                            (/ .lengthSeconds 60.0))
                                    'face
                                    'italic)
                        .title))
       ("playlist" (format "[%s] %s"
                           (propertize (format "%s videos by %s"
                                               .videoCount
                                               .author)
                                       'face
                                       'italic)
                           .title)))
     it)))

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
       (let ((result (decode-coding-string (buffer-string) 'utf-8)))
         (kill-buffer)
         (funcall callback (empv--read-result (or (cadr (split-string result "\n\n")) "{}"))))))))

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
   (lambda (request-result)
     (funcall
      callback
      (seq-map #'empv--format-yt-item request-result)))))

(defun empv--youtube-process-result (results type selected)
  "Find and return youtube url for SELECTED item of TYPE in RESULTS."
  (let ((is-video (eq type 'video))
        (info (cdr (assoc-string selected results))))
    ;; See #6
    (format "https://youtu.be/%s=%s# %s"
	          (if is-video "watch?v" "playlist?list")
            (alist-get (if is-video 'videoId 'playlistId) info)
	          (alist-get 'title info))))

(cl-defun empv--completing-read (prompt candidates &key category sort)
  "`completing-read' wrapper.

It uses `consult--read' if it's available or fallsback to
`completing-read'.  Using `consult--read' enables the use of
embark actions through the CATEGORY.  CANDIDATES and PROMPT are
required."
  (let ((selectrum-should-sort sort))
    (setq empv--last-candidates candidates)
    (if (require 'consult nil 'noerror)
        (consult--read
         candidates
         :prompt prompt
         :category category
         :sort sort)
      (completing-read prompt candidates))))

(defun empv--youtube (term type)
  "Search TERM in YouTube.
See `empv--youtube-search' for TYPE."
  (let ((use-tabulated empv-youtube-use-tabulated-results))
    (setq empv--youtube-last-type type)
    (empv--youtube-search
     term type
     (lambda (results)
       (setq empv--last-candidates results)
       (if use-tabulated
           (empv-youtube-tabulated-last-results)
         (empv-youtube-last-results))))))

(defun empv--youtube-multiple (term type)
  "Like `empv--youtube' but use `completing-read-multiple'.
See `empv--youtube' for TERM and TYPE."
  (empv--youtube-search
   term type
   (lambda (results)
     (thread-last (completing-read-multiple (format "YouTube results for '%s': " term) results)
       (seq-map (lambda (it) (empv--youtube-process-result results type it)))
       (empv--play-or-enqueue)))))

;;;###autoload
(defun empv-youtube (term)
  "Search TERM in YouTube videos."
  (interactive "sSearch in YouTube videos: ")
  (empv--youtube term 'video))

;;;###autoload
(defun empv-youtube-tabulated (term)
  "Search TERM in YouTube videos.
Show results in a tabulated buffers with thumbnails."
  (interactive "sSearch in YouTube videos: ")
  (let ((empv-youtube-use-tabulated-results t))
    (empv--youtube term 'video)))

;;;###autoload
(defun empv-youtube-multiple (term)
  "Search TERM in YouTube videos."
  (interactive "sSearch in YouTube videos: ")
  (empv--youtube-multiple term 'video))

;;;###autoload
(defun empv-youtube-playlist (term)
  "Search TERM in YouTube playlists."
  (interactive "sSearch in YouTube playlists: ")
  (empv--youtube term 'playlist))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Videos and music
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empv--find-files (path extensions &optional depth)
  "Find files with given EXTENSIONS under given PATH.
PROMPT is shown when `completing-read' is called."
  (let ((default-directory path))
    (thread-last extensions
      (mapcar (lambda (ext) (format "-e '%s' " ext)))
      (string-join)
      (concat (format "fd . -d %s " (or depth empv-max-directory-search-depth)))
      (shell-command-to-string)
      (empv-flipcall #'split-string "\n")
      (empv-seq-init))))

(defun empv--select-file (prompt path extensions &optional depth)
  "Select a file interactively under given PATH.

Only searches for files with given EXTENSIONS.
PROMPT is shown to user while selecting.
Limit directory treversal at most DEPTH levels.  By default it's
`empv-max-directory-search-depth'"
  (thread-last (empv--find-files path extensions depth)
    (empv--completing-read prompt)
    (empv-flipcall #'expand-file-name path)))

(defun empv--select-files (prompt path extensions &optional depth)
  "Select files interactively under given PATH.

Only searches for files with given EXTENSIONS.
PROMPT is shown to user while selecting.
Limit directory treversal at most DEPTH levels.  By default it's
`empv-max-directory-search-depth'"
  (thread-last (empv--find-files path extensions depth)
    (completing-read-multiple prompt)
    (seq-map (lambda (it) (expand-file-name it path)))))

;;;###autoload
(defun empv-play-video ()
  "Interactively select and play a video file from `empv-video-dir'."
  (interactive)
  (empv--play-or-enqueue
   (empv--select-file "Select a video file: " empv-video-dir empv-video-file-extensions)))

;;;###autoload
(defun empv-play-video-multiple ()
  "Interactively select and play video file(s) from `empv-video-dir'."
  (interactive)
  (empv--play-or-enqueue
   (empv--select-files "Select a video file(s): " empv-video-dir empv-video-file-extensions)))

;;;###autoload
(defun empv-play-audio ()
  "Interactively select and play an audio file from `empv-audio-dir'."
  (interactive)
  (empv--play-or-enqueue
   (empv--select-file "Select an audio file: " empv-audio-dir empv-audio-file-extensions)))

;;;###autoload
(defun empv-play-audio-multiple ()
  "Interactively select and play an audio file from `empv-audio-dir'."
  (interactive)
  (empv--play-or-enqueue
   (empv--select-files "Select an audio file: " empv-audio-dir empv-audio-file-extensions)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; empv-youtube-results-mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar empv-youtube-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'empv-youtube-results-play-current)
    (define-key map (kbd "a") #'empv-youtube-results-enqueue-current)
    (define-key map (kbd "Y") #'empv-youtube-results-copy-current)
    (define-key map (kbd "RET") #'empv-youtube-results-play-or-enqueue-current)
    map)
  "Keymap for `empv-youtube-results-mode'.")

(define-derived-mode empv-youtube-results-mode tabulated-list-mode "empv-youtube-results-mode"
  "Major mode for interacting with YouTube results with thumbnails."
  (setq tabulated-list-padding 3)
  (setq tabulated-list-format [("Thumbnail" 20 nil)
                               ("Title" 60 t)
                               ("Length"  10 t)
                               ("Views" 10 t)]))

(defadvice tabulated-list-sort (after empv-tabulated-list-sort-after activate)
  (when (derived-mode-p 'empv-youtube-results-mode)
    (iimage-recenter)))

(defun empv--youtube-show-tabulated-results (candidates)
  (let ((buffer (get-buffer-create "*empv-yt-results*"))
        (total-count (length candidates))
        (completed-count 0))
    (with-current-buffer buffer
      (empv-youtube-results-mode)
      (setq tabulated-list-entries
            (seq-map-indexed
             (lambda (it index)
               (let* ((video-info (cdr it))
                      (video-title (alist-get 'title video-info))
                      (video-view (format "%0.2fk views" (/ (alist-get 'viewCount video-info) 1000.0)))
                      (video-length (format "%0.2f mins" (/ (alist-get 'lengthSeconds video-info) 60.0))))
                 (list index (vector "<THUMBNAIL>" video-title video-length video-view))))
             candidates))
      (tabulated-list-init-header))
    (seq-do-indexed
     (lambda (video index)
       (let* ((video-info (cdr video))
              (video-id (alist-get 'videoId video-info))
              (filename (format
                         (expand-file-name "~/.cache/empv_%s_%s.jpg")
                         video-id
                         empv-youtube-thumbnail-quality)))
         (thread-last video-info
           (alist-get 'videoThumbnails video-info)
           (seq-find (lambda (thumb)
                       (equal empv-youtube-thumbnail-quality
                              (alist-get 'quality thumb))))
           (cdr)
           (alist-get 'url)
           (start-process
            (format "empv-download-process-%s" video-id)
            "*empv-thumbnail-downloads*"
            (if (file-exists-p filename) "printf" "curl")
            (if empv-allow-insecure-connections "--insecure" "")
            "-o"
            filename)
           (empv-flipcall
            #'set-process-sentinel
            (lambda (p e)
              (with-current-buffer buffer
                (setf
                 (elt (car (alist-get index tabulated-list-entries)) 0)
                 (format "[[%s]]" filename))
                (setq completed-count (1+ completed-count))
                (when (eq completed-count total-count)
                  (tabulated-list-print)
                  (iimage-mode)
                  (back-to-indentation))))))))
     candidates)
    (pop-to-buffer-same-window buffer)))

(defun empv-youtube-results--current-video-url ()
  (empv--youtube-process-result
   empv--last-candidates
   'video
   (car (nth (tabulated-list-get-id) empv--last-candidates))))

(defun empv-youtube-results-play-current ()
  (interactive)
  (empv-play (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-enqueue-current ()
  (interactive)
  (empv-enqueue (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-play-or-enqueue-current ()
  (interactive)
  (empv--play-or-enqueue (empv-youtube-results--current-video-url)))

(defun empv-youtube-results-copy-current ()
  (interactive)
  (let ((it (empv-youtube-results--current-video-url)))
    (kill-new it)
    (message "Copied %s!" it)))

(defun empv-youtube-tabulated-last-results ()
  "Show last search results in tabulated mode with thumbnails."
  (interactive)
  (empv--youtube-show-tabulated-results empv--last-candidates))

(defun empv-youtube-last-results ()
  "Show last search results."
  (interactive)
  (thread-last (empv--completing-read
                (format "YouTube results: ")
                empv--last-candidates
                :category 'empv-youtube)
    (empv--youtube-process-result empv--last-candidates empv--youtube-last-type)
    (empv--play-or-enqueue)))

(provide 'empv)
;;; empv.el ends here
