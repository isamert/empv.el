;;; empv.el --- A multimedia player/manager, YouTube interface -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 5.1.0
;; Homepage: https://github.com/isamert/empv.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1") (s "1.13.0") (compat "29.1.4.4"))

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
;; interface, Subsonic/Navidrome client, local music/video library
;; manager etc.

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
(require 'compat)
(require 'text-property-search)
(require 'bookmark)
(eval-when-compile
  (require 'subr-x))

;;;; Development

;; * mpv
;;
;; - https://github.com/mpv-player/mpv/blob/master/DOCS/man/input.rst
;;
;; * Subsonic
;;
;; - https://www.subsonic.org/pages/api.jsp
;; - https://git.sr.ht/~amk/subsonic.el
;;
;; * Invidious
;;
;; - https://docs.invidious.io/api/
;;
;; * Testing
;;
;; There are tests, a few...  In the comments of some functions.  You
;; can use `doctest' package to run them.  Not very useful tests
;; though.

;;;; Constants

(defconst empv-thumbnail-placeholder "<THUMBNAIL>")

;;;; Customization

(defgroup empv nil
  "A media player for Emacs."
  :group 'multimedia)

(defcustom empv-socket-file (format "%sempv-socket" (temporary-file-directory))
  "Socket file path."
  :type 'string
  :group 'empv)

(defcustom empv-fd-binary "fd"
  "FD path."
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
  :type '(repeat (string :tag "Command line option"))
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

(defcustom empv-invidious-request-headers nil
  "Alist of custom HTTP headers to include in Invidious API requests.
For example:
    \\='((\"X-My-Header\" . \"Value\")
         (\"Authorization\" . \"Bearer token\"))"
  :type '(alist :key-type string :value-type string)
  :group 'empv)

(defcustom empv-youtube-use-tabulated-results
  nil
  "Show YouTube results in a tabulated buffer with thumbnails if not nil.
Otherwise simply use `completing-read'.  You can still use
`empv-youtube-tabulated' command if this variable is nil."
  :type 'boolean
  :group 'empv)

(defcustom empv-youtube-thumbnail-props `(:max-width 240 :max-height 180)
  "Image properties for thumbnails.
See Info node `(elisp) Image Descriptors', specifically the PROPS
part.  With this, you can manipulate the size of the thumbnails."
  :version "4.3.0"
  :type '(plist)
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
                 (repeat (directory :tag "Audio directory")))
  :group 'empv)

(defcustom empv-video-dir (or (getenv "XDG_VIDEOS_DIR") "~/Videos")
  "The directory (or list of directories) that you keep your videos in."
  :type '(choice (directory :tag "Video directory")
                 (repeat (directory :tag "Video directory")))
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
  :type 'boolean
  :group 'empv)

(defcustom empv-display-current-format
  "[#{state}#{item-loop-indicator}, #{time-pos} of #{duration} (#{percent-pos}%), #{playlist-pos}/#{playlist-count}#{playlist-loop-indicator}#{radio}#{volume}#{speed}] #{title} #{chapter}"
  "Format of the message displayed when `empv-display-current' is called.

This is a string representing the format or this can be an alist
from file format to format string, like:

    \\='((\"hls\" . \"#{state} #{title} ...\")
      (\"mp3\" . \"#{state} #{title} ...\")
      ;; Multiple formats can be defined like this:
      (\"mkv,mp4\" . \"#{state} #{title} ...\")
      ;; Catch-all case:
      (t . \"#{state} #{title} ...\"))

So that you can use different formats for different file types.

Here are the template strings that you can utilize in the format
string:

- #{title} - Shows the title of the current media.

- #{chapter} - Shows the current chapter of the media file being
   played.

- #{state} - Shows one one of the following: Playing, Paused,
   Buffered...

- #{time-pos} - Shows the current time of the media being played
   in MM:SS format.

- #{duration} - Shows the total duration of the current media in
   MM:SS format.

- #{percent-pos} - Shows the total completed percentage of the
   current media.

- #{playlist-pos} - Shows the current media's position in the
   playlist.

- #{playlist-count} - Shows the total item count in the current
   playlist.

- #{item-loop-indicator} - Shows ↻ if the current file is on
   loop.

- #{playlist-loop-indicator} - Shows ↻ if the current playlist is
   on loop.

- #{radio} - Shows the current radio channel being played, if
   there is any.

- #{volume} - Shows the current volume level in percentage, if
   it's different from 100%.

- #{speed} - Shows the current volume level in percentage, if
   it's different from 100%."
  :type '(choice (string :tag "Template format for all file types")
                 (alist :key-type (string :tag "File format (like hls, mp3, mpv etc.)")
                        :value-type (string :tag "Template format")))
  :group 'empv)

(defcustom empv-youtube-tabulated-video-headers
  `(("Thumbnail" 15 nil ,empv-thumbnail-placeholder)
    ("Title" 50 t .title)
    ("Length" 15 t .lengthSeconds)
    ("Views" 15 (lambda (a b) (< (alist-get 'viewCount a 0) (alist-get 'viewCount b 0))) .viewCountText)
    ("Author" 15 t .author)
    ("Published" 15 (lambda (a b) (< (alist-get 'published a 0) (alist-get 'published b 0))) .publishedText))
  "Headers to display in YouTube tabulated search result.

By setting this variable, you can customize what the show inside
YouTube tabulated search results.  By default \"Thumbnail, Title,
Length, Views, Author, Published\" are shown.  You can simply
delete or add to this list show extra or less information.  This
variable is in the following form:

    (HEADER-DEFINITION ...)

A HEADER-DEFINITION is a list in the following form:

    (TITLE COLUMN-LENGTH SORTABLE? ACCESSOR)

- TITLE is simply the text shown in the header portion (a string).

- COLUMN-LENGTH is the length of this column in the table (a number).

- SORTABLE? defines if this column is sortable or not (t or nil).  It
  can also be a function that takes

    (INVIDIOUS-RESPONSE1 INVIDIOUS-RESPONSE2)

  as parameters.

- ACCESSOR is a JSON-PATH like symbol that defines how to get the value
  of this column.  Or it can be a function that takes one argument, an
  alist containing video information.

For example, to show how many views are there for a video, you can use
the following HEADER-DEFINITION:

    \\='(\"Views\" 15 t .viewCountText)

alternatively (the function variant):

    \\='(\"Views\" 15 t (lambda (video) (alist-get 'viewCountText video)))

This will create an 15 char-wide column named \"Views\" and it
will get the viewCountText value from the Invidious response to
display it as the value in a row.

You can inspect a YouTube search result to learn it's
JSON-PATH. To do so, do a tabulated search, focus on a
video/playlist and then hit `i', or do
\\[empv-youtube-results-inspect], and this will show you the raw
Invidious response that is used to build the search result table.
If the value you are looking for is inside another object, you
can use a JSON-PATH like the following: .a.b.c"
  :version "4.2.0"
  :type '(repeat
          (list (string :tag "Name of the column")
                (integer :tag "Length of the column")
                (choice (boolean :tag "Is column sortable?")
                        (function :tag "Sort function"))
                (choice (symbol :tag "The path of the property in the response json")
                        (string :tag "Constant text to show"))))
  :group 'empv)

(defcustom empv-youtube-tabulated-playlist-headers
  `(("Thumbnail" 20 nil ,empv-thumbnail-placeholder)
    ("Title" 60 t .title)
    ("Author" 15 t .author)
    ("Video Count" 13 t .videoCount))
  "Like `empv-youtube-tabulated-video-headers' but for playlist search results."
  :version "4.2.0"
  :type '(repeat
          (list (string :tag "Name of the column")
                (integer :tag "Length of the column")
                (choice (boolean :tag "Is column sortable?")
                        (function :tag "Sort function"))
                (choice (symbol :tag "The path of the property in the response json")
                        (string :tag "Constant text to show"))))
  :group 'empv)

(defcustom empv-youtube-tabulated-channel-headers
  `(("Thumbnail" 20 nil ,empv-thumbnail-placeholder)
    ("Channel" 30 t .author)
    ("Sub Count" 15 t .subCount)
    ("Video Count" 15 t .videoCount)
    ("Verified?" 10 t .authorVerified)
    ("Auto Gen?" 15  t .autoGenerated))
  "Like `empv-youtube-tabulated-video-headers' but for playlist search results."
  :version "5.2.0"
  :type '(repeat
          (list (string :tag "Name of the column")
                (integer :tag "Length of the column")
                (choice (boolean :tag "Is column sortable?")
                        (function :tag "Sort function"))
                (choice (symbol :tag "The path of the property in the response json")
                        (string :tag "Constant text to show"))))
  :group 'empv)

(defcustom empv-youtube-tabulated-new-entries-hook
  '()
  "Functions to be called when new entries are added to tabulated YouTube results.
Each function is called with set of set of results being appended
to the buffer.  The list contains the response from Invidious.
For example, to print all video titles whenever they are being
added, you can do the following:

  (add-hook
   \\='empv-youtube-tabulated-new-entries-hook
   #\\='(lambda (entries &rest _)
       (seq-do (lambda (it) (message (alist-get \\='title it))) entries)))"
  :version "4.3.0"
  :type 'hook
  :group 'empv)

(defcustom empv-use-consult-if-possible t
  "Use consult for some flows like YouTube search suggestions if it's installed.
Setting this variable to t does not affect anything if you
haven't installed consult."
  :version "4.4.0"
  :type 'boolean
  :group 'empv)

(defcustom empv-ytdl-binary "yt-dlp"
  "`ytdl' binary path."
  :version "4.5.0"
  :type 'string
  :group 'empv)

(defcustom empv-ytdl-download-options '("--extract-audio" "--audio-format=mp3" "--embed-metadata" "--embed-thumbnail")
  "Options passed to yt-dlp program while calling `empv-youtube-download'.
Also see `empv-youtube-ytdl-binary'."
  :version "4.5.0"
  :type '(repeat (string :tag "Command line option"))
  :group 'empv)

(defcustom empv-subsonic-username nil
  "Subsonic user name."
  :group 'empv
  :type 'string
  :version "4.9.0")

(defcustom empv-subsonic-password nil
  "Subsonic password."
  :group 'empv
  :type 'string
  :version "4.9.0")

(defcustom empv-subsonic-url nil
  "Subsonic URL."
  :group 'empv
  :type 'string
  :version "4.9.0")

(defcustom empv-subsonic-result-count "50"
  "Max Subsonic result count.
This only applies if the endpoint has a count or size parameter.  For
example, `empv-subsonic-artists' will return all artists no matter what
this value is.

Maximum possible value is 500. "
  :type 'string
  :group 'empv
  :version "4.9.0")

;;;; Public variables

(defvar empv-media-title nil
  "Formatted title of the current or most recently played media.
If available, chapter info is also included.")

(defcustom empv-media-title-changed-hook '()
  "Functions to run when current media title is changed.
Functions are called with one argument, the value of
`empv-media-title'."
  :type 'hook
  :group 'empv
  :version "5.1.0")

(defvar empv-player-state nil
  "Current media player state.
It's in one of these states: \\='paused, \\='playing, \\='caching or
\\='stopped.")

(defcustom empv-player-state-changed-hook '()
  "Functions to run when current media player state is changed.
Functions are called with one argument, the value of
`empv-player-state'."
  :type 'hook
  :group 'empv
  :version "5.1.0")

;;;###autoload
(defvar empv-map
  (let ((map (make-sparse-keymap)))
    ;; Play/pause etc.
    (define-key map "o" 'empv-play-or-enqueue)
    (define-key map "f" 'empv-play-file)
    (define-key map "d" 'empv-play-directory)
    (define-key map "v" 'empv-play-video)
    (define-key map "a" 'empv-play-audio)

    ;; Utility
    (define-key map "i" 'empv-display-current)
    (define-key map "c" 'empv-copy-path)

    ;; Toggling
    (define-key map "8" 'empv-toggle-current-loop)
    (define-key map "e" 'empv-toggle-event-display)

    ;; Video
    (define-key map "t" 'empv-toggle)
    (define-key map "_" 'empv-toggle-video)

    ;; Radio
    (define-key map "r" 'empv-play-radio)
    (define-key map "R" 'empv-play-random-channel)
    (define-key map "l" 'empv-log-current-radio-song-name)

    ;; Speed
    (define-key map "[" 'empv-playback-speed-down)
    (define-key map "]" 'empv-playback-speed-up)
    (put 'empv-playback-speed-up 'repeat-map 'empv-map)
    (put 'empv-playback-speed-down 'repeat-map 'empv-map)

    ;; Chapters
    (define-key map "x" 'empv-chapter-select)
    (define-key map "(" 'empv-chapter-prev)
    (define-key map ")" 'empv-chapter-next)

    ;; Volume
    (define-key map "9" 'empv-volume-down)
    (define-key map "0" 'empv-volume-up)
    (put 'empv-volume-up 'repeat-map 'empv-map)
    (put 'empv-volume-down 'repeat-map 'empv-map)

    ;; Playlist
    (define-key map "p" 'empv-playlist-select)
    (define-key map "s" 'empv-playlist-shuffle)
    (define-key map "C" 'empv-playlist-clear)
    (define-key map "n" 'empv-playlist-next)
    (define-key map "N" 'empv-playlist-prev)
    (define-key map "L" 'empv-playlist-load-from-file)
    (put 'empv-playlist-next 'repeat-map 'empv-map)
    (put 'empv-playlist-prev 'repeat-map 'empv-map)

    ;; Youtube
    (define-key map "y" 'empv-youtube)
    (define-key map "Y" 'empv-youtube-last-results)

    (define-key map "q" 'empv-exit)
    map)
  "Keymap for commonly used empv functions.
It is not bound to any key by default.  Some keys are loosely
modeled after default keys of mpv.")

;;;; Internal variables

(defconst empv--title-sep "##"
  "Separator for separating the URL and it's title.
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
(defvar empv--last-youtube-search nil
  "Latest YouTube search, an `empv--yt-search' struct.")
(defvar empv--action-selection-default-title "Action"
  "The text displayed on action selection menus.")
(defvar empv--inspect-buffer-name "*empv inspect*"
  "Buffer name for showing pretty printed results.")
(defvar-local empv--current-file nil
  "Current media file associated with the buffer.")

(defconst empv--playlist-current-indicator (propertize "[CURRENT]" 'face '(:foreground "green"))
  "Simple text to show on the currently playing playlist item.")

(defvar-local empv--buffer-youtube-search nil
  "YouTube search for this buffer, see `empv--yt-search'.")

(defvar empv--youtube-search-history nil)

;;;; Utility: Lists

(defun empv--flipcall (fn x y)
  "Flip arguments of given binary function FN and call it with Y and X."
  (funcall fn y x))

(defun empv--seq-init (seq)
  "Return the SEQ without the last element."
  (seq-subseq seq 0 (1- (seq-length seq))))

(defun empv--seq-last (seq)
  "Return the last element of SEQ."
  (seq-elt seq (1- (seq-length seq))))

(defun empv--seq-find-index (fn seq)
  "Return the first index in SEQ for which FN evaluate to non-nil."
  (seq-position seq 'empv-dummy-item (lambda (it _) (funcall fn it))))

(defun empv--random-string (len)
  "Create a random string that is LEN chars long."
  (let* ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (charset-len (length charset)))
    (apply #'string (mapcar (lambda (_x) (elt charset (random charset-len))) (number-sequence 0 len)))))

(defun empv--alist-path-get-helper (path alist)
  "Get the value associated with a specific PATH in ALIST.

>> (let ((alist `((a . ((b . ((c . d)))))))
         (path `(a b c)))
    (empv--alist-path-get-helper path alist))
=> d"
  (if (eq (length path) 1)
      (alist-get (car path) alist)
    (empv--alist-path-get-helper (seq-drop path 1) (alist-get (car path) alist))))

(defun empv--alist-path-get (path alist)
  "Get the value associated with a specific PATH in ALIST.

>> (let ((alist `((a . ((b . ((c . d)))))))
         (path `.a.b.c))
    (empv--alist-path-get path alist))
=> d"
  (empv--alist-path-get-helper (mapcar #'intern (string-split (symbol-name path) "\\." t)) alist))

(defun empv--plist-to-alist (plist)
  "Convert PLIST to an alist.
Taken from transient.el.

>> (empv--plist-to-alist (quote (:a 1 :b 2 :c (3 4 5))))
=> ((a . 1) (b . 2) (c . (3 4 5)))"
  (let (alist)
    (while plist
      (push (cons (let* ((symbol (pop plist))
                         (name (symbol-name symbol)))
                    (if (eq (aref name 0) ?:)
                        (intern (substring name 1))
                      symbol))
                  (pop plist))
            alist))
    (nreverse alist)))

(defun empv--to-keyword (it)
  "Convert IT to a :keyword.
IT can be a symbol or string.

>> (empv--to-keyword (quote hello))
=> :hello

>> (empv--to-keyword \"hey\")
=> :hey"
  (thread-last
    (cond
     ((stringp it) it)
     ((symbolp it) (symbol-name it))
     (t (error "Trying to convert %s to symbol" it)))
    (string-remove-prefix ":")
    (concat ":")
    (downcase)
    (intern)))

(defun empv--alist-to-plist (alist)
  "Convert ALIST to a plist.

>> (empv--alist-to-plist (quote ((a . 1) (b . 2) (c . (3 4 5)))))
=> (:a 1 :b 2 :c (3 4 5))"
  (let (plist)
    (while alist
      (let ((el (car alist)))
        (setq plist (cons (cdr el) (cons (empv--to-keyword (car el)) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

;;;; Utility: Execution

(cl-defmacro empv--wait-until-non-nil (place &rest forms)
  "Wait until PLACE is non-nil, after executing FORMS."
  (declare (indent 1))
  `(let (,place (try-count 0))
     ,@forms
     (while (and (not ,place) (< try-count 500))
       (setq try-count (1+ try-count))
       (sleep-for 0.01))
     ,place))

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

;;;; Utility: empv

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
  "Read JSON RESULT into an elisp structure."
  (condition-case nil
      (json-parse-string
       result
       :object-type 'alist
       :array-type 'list
       :false-object :json-false
       :null-object result)
    (error (empv--dbg "Error while reading JSON :: %s" result) nil)))

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

(defmacro empv--run (&rest forms)
  "Start if mpv is not running already and then run FORMS."
  `(progn
     (unless (empv--running?)
       (empv-start))
     ,@forms))

(defun empv--with-text-properties (str &rest props)
  (let ((str-copy (copy-sequence str)))
    (add-text-properties
     0 1
     (seq-mapcat
      #'identity
      (seq-map
       (lambda (prop) (cons (intern (concat "empv-" (string-trim-left (symbol-name (car prop)) ":"))) (cdr prop)))
       (seq-partition props 2)))
     str-copy)
    str-copy))

(defun empv--get-text-property (str prop)
  (get-text-property 0 (intern (concat "empv-" (string-trim-left (symbol-name prop) ":"))) str))

;;;; Utility: Url/Path/Metadata

(defun empv--tramp-to-sftp-uri (uri)
  "Convert URI to something that MPV can stream.
URI is in TRAMP format.  If URI does not point to a remote resource,
return as-is.

NOTE: Only supports SSH/SSHX methods on tramp."
  (if (not (file-remote-p uri))
      uri
    (let ((method (file-remote-p uri 'method))
          (user (file-remote-p uri 'user))
          (host (file-remote-p uri 'host))
          (localname (file-remote-p uri 'localname)))
      (if (not (member method (list "ssh" "sshx")))
          (user-error "empv: method %s is not supported for playback." method))
      (format "sftp://%s%s"
              (if user
                  (format "%s@%s" user host)
                host)
              localname))))

(defun empv--clean-uri (it)
  (car (split-string it empv--title-sep)))

(defun empv--url-with-magic-info (url &rest info)
  "Return URL, but with magic info attached into it.
Magic info is just a PLIST (the INFO) that is added end of the URL, see
`empv--title-sep' for more details."
  (format
   "%s%s%s"
   url
   empv--title-sep
   (prin1-to-string info)))

(defun empv--extract-empv-metadata-from-path (path &optional fallback)
  "Extract the metadata encoded in PATH.
Some metadata is encoded into PATH and this function tries to
parse that and returns a form along the lines of:

    \\='(:title \"...\" :uri \"the-real-uri-without-metadata\"
         :radio nil/t :youtube nil/t)

:title is the human readable name of the path.  This function also
checks if is there any cached name for this PATH.

:uri is the PATH but without the encoded metadata.  Clean version
of the PATH.

:radio indicates if this PATH is a radio stream or not (only true
if invoked by `empv-play-radio' etc.)

:subsonic indicates that this PATH is a Subsonic stream.

:youtube indicates if this PATH is a YouTube path or not (only
true if invoked by `empv-youtube' family of functions.)

:author gives you the YouTube channel name or not (only true if invoked
by `empv-youtube' family of functions.)

:authorId gives you the YouTube channel id or not (only true if invoked
by `empv-youtube' family of functions.)

Names of these properties are inherited from Invidious response to
ensure consistency. See `empv--youtube-item-extract-link' function for
further details.

Use FALLBACK as fallback title in case nothing in URL is found.

Also see `empv--title-sep' and `empv--media-title-cache'
documentation."
  ;; First try the URL encoded title and then check for the
  ;; media-title cache.  Cache may contain last playing media's title
  ;; for a stream, not the name for the stream itself. URL encoded
  ;; title probably has the stream's title. (At least this is the case
  ;; for radio streams, see `empv-radio-channels' and
  ;; `empv--radio-item-extract-link')
  (pcase-let* ((`(,uri ,data) (split-string (or path "") empv--title-sep)))
    (if-let* ((parsed
               (and data
                    (string-prefix-p "(" data)
                    (string-suffix-p ")" data)
                    (ignore-errors (read data)))))
        `(,@parsed :uri ,uri)
      (list
       :uri uri
       :title (gethash uri empv--media-title-cache
                       (or fallback (abbreviate-file-name
                                     (if (file-remote-p uri)
                                         (file-remote-p uri 'localname)
                                       uri))))))))

;;;; Handlers

(defun empv--sentinel (_proc msg)
  "Clean up after connection closes with MSG."
  (empv--dbg "<< sentinel -> %s" msg)
  (empv--display-event "Closed.")
  (empv-exit))

(defvar empv--process-buffer ""
  "Temporary variable to hold the date returned from mpv process.

If the returned result is sufficiently long (like a very long
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
              (event-name (map-elt json-data 'event))
              (callback (map-elt empv--callback-table (format "%s" (or request-id id event-name)))))
         (empv--dbg
          "<< data: %s, request_id: %s, has-cb?: %s"
          json-data
          request-id
          (and callback t))
         ;; Remove it first from callback table, so we don't get into
         ;; a loop if an error occurs on the callback function
         (when (not (plist-get callback :event?))
           (map-delete empv--callback-table request-id))
         (when-let* ((cb-fn (plist-get callback :fn)))
           (ignore-error (quit minibuffer-quit)
             (if (or request-id id)
                 (funcall cb-fn (cdr (assoc 'data json-data)))
               (funcall cb-fn json-data))))))
     (seq-filter
      (lambda (it) (not (string-empty-p it)))
      (seq-map
       #'string-trim
       (split-string empv--process-buffer "\n"))))
    (setq empv--process-buffer "")))

;;;; Process primitives

(defun empv--make-process (&rest uris)
  "Create the MPV process with given URIs."
  (setq empv--process
        (make-process :name "empv-process"
                      :buffer nil
                      :command (if uris
                                   `(,empv-mpv-binary ,@empv-mpv-args ,@uris)
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
                      (request_id . ,(string-to-number request-id)))
                  `((command . ,command)
                    (request_id . ,(string-to-number request-id)))))))
     (when callback
       (map-put! empv--callback-table request-id (list :fn callback :event? event?)))
     (process-send-string empv--network-process (format "%s\n" msg))
     (empv--dbg ">> %s" msg)
     request-id)))

(defun empv--send-command-sync (command)
  "Send COMMAND to mpv process and return the result."
  (let ((result nil))
    (empv--wait-until-non-nil finished
      (empv--send-command command (lambda (x)
                                    (setq result x)
                                    (setq finished t))))
    result))

;;;; Essential macros

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
  "Gives you a context containing `.media-title', `.path' `.metadata'.
Executes BODY with this context."
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
        (empv--display-event
         "%s is %s"
         (capitalize (symbol-name ,property))
         (pcase new-val
           (:json-false "false")
           ('t "true")
           ;; Some properties use 0/1 to indicate enabled/disabled but this causes messages like:
           ;; "Volume is disabled" instead of "Volume is 0" (which is kind of true)
           ;; "Volume is enabled" instead of "Volume is 1" (Bearable)
           ;; I can fix this by taking an optional message transformer but I don't feel like doing it right now.
           (0 "disabled")
           (1 "enabled")
           (other other)))))))

;;;; User level helpers

(defun empv-observe (property callback)
  "Observe PROPERTY and call CALLBACK when it does change.
See the mpv property list:
https://github.com/mpv-player/mpv/blob/master/DOCS/man/input.rst#property-list"
  (declare (indent 1))
  (empv--send-command `(observe_property ,property) callback t))

(defun empv-event (event callback)
  "Every time mpv fires an EVENT, call CALLBACK.
EVENT is a symbol representing the event name, see list of
events: https://mpv.io/manual/stable/#list-of-events"
  (map-put! empv--callback-table (symbol-name event) (list :fn callback :event? t)))

;;;; Some callbacks

(defun empv--set-player-state (&rest _)
  (cl-flet ((run-hooks
             ()
             (seq-each (lambda (x) (funcall x empv-player-state)) empv-player-state-changed-hook)))
    (if (empv--running?)
        (empv--let-properties '(paused-for-cache pause playlist-pos)
          (let ((empv-metadata (empv--extract-empv-metadata-from-path .path)))
            (setq empv-player-state
                  (cond
                   ((< .playlist-pos 0) 'stopped)
                   ((eq .paused-for-cache t) 'caching)
                   ((eq .pause t) 'paused)
                   (t 'playing))))
          (run-hooks))
      (setq empv-player-state 'stopped)
      (run-hooks))))

(defun empv--set-media-title (title)
  (if (empv--running?)
      (setq empv-media-title title)
    (setq empv-media-title nil))
  (seq-each (lambda (x) (funcall x empv-media-title)) empv-media-title-changed-hook))

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
  "Generate a formatted media title like \"Song name - Artist\" from given METADATA.
Use FALLBACK as fallback title in case song/artist name not found.
PATH is the path of the media file."
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
  (empv--let-properties '(media-title path chapter chapter-metadata metadata)
    (when .path
      (let ((title (string-trim (empv--create-media-summary-for-notification .metadata .path .media-title))))
        (puthash (empv--clean-uri .path) title empv--media-title-cache)
        (empv--set-media-title (concat title (if (and .chapter (> .chapter -1))
                                                 (format " (%s)" (alist-get 'title .chapter-metadata))
                                               "")))
        (empv--display-event "%s" empv-media-title)))))

;;;; Essential functions

;;;###autoload
(defun empv-play-or-enqueue (uri)
  "Play or enqueue the URI based on user input.
URI might be a string or a list of strings."
  (interactive "sEnter an URI to play: ")
  (empv--select-action _
    "Play" → (empv-play uri)
    "Enqueue last" → (empv-enqueue uri)
    "Enqueue next" → (empv-enqueue-next uri)))

(defun empv-start (&rest uris)
  "Start mpv using `empv-mpv-command' with given URIS."
  (interactive)
  (unless (empv--running?)
    (empv--dbg "Starting MPV.")
    (apply #'empv--make-process uris)
    (empv--make-network-process)
    (empv-observe 'metadata #'empv--handle-metadata-change)
    (empv-observe 'chapter-metadata #'empv--handle-metadata-change)
    (empv-observe 'pause #'empv--set-player-state)
    (empv-observe 'paused-for-cache #'empv--set-player-state)
    (empv-observe 'playlist-count #'empv--set-player-state)
    (run-hooks 'empv-init-hook)))

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

(cl-defun empv--completing-read-object
    (prompt objects &key (formatter #'identity) category group (sort? t) def multiple?)
  "`completing-read' with formatter and sort control.
Applies FORMATTER to every object in OBJECTS and propertizes
candidates with the actual object so that they can be retrieved
later by embark actions.

Also adds CATEGORY as metadata to each candidate, if given.

PROMPT passed to `completing-read' as is.

GROUP is the grouping function called with the each object and should
return a group string."
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
                   ,(when group
                      (cons 'group-function
                            (lambda (cand transform)
                              (if transform
                                  cand
                                (funcall group (gethash cand object-table def))))))
                   ,@(unless sort?
                       '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
               (complete-with-action
                action object-strings string predicate))))))
    (if multiple?
        (or (mapcar (lambda (it) (gethash it object-table)) selected) def)
      (gethash selected object-table (or def selected)))))

;;;; Interactive - Basics

;;;###autoload
(defun empv-play (uri)
  "Play given URI.
Add given URI to end of the current playlist and immediately switch to
it.  URI also might be a list of URIs.  In that case all items are
enqueued and the first one starts playing."
  (interactive "sEnter an URI to play: ")
  (if (listp uri)
      (empv--cmd
       'stop nil
       (seq-do #'empv-enqueue uri)
       (empv-resume))
    (when (file-exists-p uri)
      (setq uri (expand-file-name uri)))
    (setq uri (empv--tramp-to-sftp-uri uri))
    (if (empv--running?)
        (empv--cmd-seq
         ('loadfile (list uri 'append))
         ('get_property 'playlist-count)
         ('playlist-play-index (1- it))
         ('set_property '(pause :json-false)))
      (empv-start uri))
    (when (called-interactively-p 'interactive)
      (empv--display-event "Playing %s" uri))))

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
  (empv--transform-property 'pause
    (lambda (it)
      (pcase it
        ('t :json-false)
        (_ t)))))

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
  (empv--transform-property 'video
    (lambda (it)
      (pcase it
        (1 0)
        (_ (empv--cmd 'set_property '(force-window immediate))
           1))))
  (empv--cmd 'set_property '(force-window no)))

;;;###autoload
(defun empv-exit ()
  "Shut down mpv."
  (interactive)
  (when empv--process
    (setq empv--process (delete-process empv--process)))
  (when empv--network-process
    (setq empv--network-process (delete-process empv--network-process)))
  (setq empv--callback-table (make-hash-table :test 'equal))
  (setq empv--media-title-cache (make-hash-table :test 'equal))
  (empv--set-media-title nil)
  (empv--set-player-state nil))

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
  "Toggle displaying events.
See `empv-display-events' for details."
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
    (when-let* ((title (alist-get 'icy-title .metadata))
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

;;;; Interactive - Playlist

;;;###autoload
(defun empv-enqueue (uri)
  "Like `empv-play' but add the given URI to end of the playlist.
URI might be a list of URIs, then they are all enqueued in order."
  (interactive "sEnter an URI to play: ")
  (if (listp uri)
      (seq-do #'empv-enqueue uri)
    (when (string-prefix-p "~/" uri)
      (setq uri (expand-file-name uri)))
    (setq uri (empv--tramp-to-sftp-uri uri))
    (empv--cmd 'loadfile `(,uri append-play))
    (empv--display-event "Enqueued %s" uri)))

(defalias 'empv-enqueue-last #'empv-enqueue)

(defun empv-enqueue-next (uri)
  "Like `empv-enqueue' but append URI right after current item."
  (interactive "sEnter an URI to play: ")
  (if (listp uri)
      (seq-do #'empv-enqueue-next uri)
    (empv--let-properties '(playlist)
      (let ((len (length .playlist))
            (idx (empv--seq-find-index (lambda (it) (alist-get 'current it)) .playlist)))
        (empv-enqueue uri)
        (empv--cmd 'playlist-move `(,len ,(1+ idx)))))))

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
    (let ((fname (read-file-name "Save playlist to (extension must be .m3u): " (file-name-as-directory empv-playlist-dir) "")))
      (when (and fname (not (string-empty-p fname)))
        fname))))
  (empv--playlist-apply #'empv--playlist-save-to-file filename))

;;;###autoload
(defun empv-playlist-load-from-file ()
  "Load a playlist from `empv-playlist-dir'."
  (interactive)
  (empv-play-or-enqueue
   (empv--select-file "Select a playlist file:" empv-playlist-dir '("m3u"))))

;;;; Interactive - Misc

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
     "(%s: %s, %s of %s (%d%%))"
     (propertize "Chapter" 'face 'italic)
     (propertize (alist-get 'title current-chapter chapter) 'face 'underline)
     (empv--format-clock current-chapter-time-pos)
     (empv--format-clock current-chapter-duration)
     (* (/ current-chapter-time-pos current-chapter-duration) 100))))

;;;###autoload
(defun empv-display-current (arg)
  "Display currently playing item's title and media player state.
If ARG is non-nil, then also put the title to `kill-ring'.

The display format is determined by the
`empv-display-current-format' variable, see it's documentation"
  (interactive "P")
  (empv--let-properties '(playlist-pos-1
                          playlist-count
                          time-pos percent-pos duration
                          metadata media-title path
                          pause paused-for-cache loop-file loop-playlist
                          chapter chapter-list
                          volume option-info/volume/default-value
                          speed option-info/volume/default-value
                          file-format)
    (let ((title (string-trim (empv--create-media-summary-for-notification .metadata .path .media-title)))
          (state (cond
                  ((eq .paused-for-cache t) (propertize "Buffering..." 'face '(:foreground "gold")))
                  ((eq .pause t) (propertize "Paused" 'face '(:foreground "grey")))
                  (t (propertize "Playing" 'face '(:foreground "green")))))
          (empv-metadata (empv--extract-empv-metadata-from-path .path))
          (empv-display-events t))
      (empv--display-event
       "%s"
       (s-replace-all
        `(("#{state}" . ,state)
          ("#{item-loop-indicator}" . ,(if (eq :json-false .loop-file) "" " ↻"))
          ("#{time-pos}" . ,(format "%s" (empv--format-clock (or .time-pos 0))))
          ("#{duration}" . ,(format "%s" (empv--format-clock (or .duration 0))))
          ("#{percent-pos}" . ,(format "%d" (or .percent-pos 0)))
          ("#{playlist-pos}" . ,(format "%s" .playlist-pos-1))
          ("#{playlist-count}" . ,(format "%s" .playlist-count))
          ("#{playlist-loop-indicator}" . ,(if (eq :json-false .loop-playlist) "" " ↻"))
          ("#{radio}" . ,(if (plist-get empv-metadata :radio)
                             (concat ", " (propertize (plist-get empv-metadata :title) 'face 'italic))
                           ""))
          ("#{volume}" . ,(if (not (= .volume (or .option-info/volume/default-value 100)))
                              (concat ", " (propertize (format "🔊 %s" (floor .volume)) 'face 'bold))
                            ""))
          ("#{speed}" . ,(if (not (= .speed (or .option-info/speed/default-value 1)))
                             (concat ", " (propertize (format "⏩ %s" .speed) 'face 'bold))
                           ""))
          ("#{title}" . ,(propertize title 'face 'bold))
          ("#{chapter}" . ,(if .chapter
                               (empv--format-chapter .chapter-list .chapter .time-pos .duration)
                             "")))
        (if (stringp empv-display-current-format)
            empv-display-current-format
          (cdr (or (seq-find (lambda (it)
                               (when (stringp (car it))
                                 (seq-some
                                  (lambda (format) (s-contains? format (or .file-format "")))
                                  (s-split "," (car it)))))
                             empv-display-current-format)
                   (assoc 't empv-display-current-format))))))
      (when arg
        (kill-new title)))))

(defun empv-copy-path ()
  "Copy the path of currently playing item."
  (interactive)
  (empv--let-properties '(path)
    (if (not .path)
        (user-error "Nothing is currently playing")
      (empv--display-event "URI copied: %s" (empv--clean-uri .path))
      (kill-new (empv--clean-uri .path)))))

;;;; Interactive - Chapters

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

;;;; Radio

(defun empv--radio-item-extract-link (channel)
  (empv--url-with-magic-info
   (cdr channel)
   :title (car channel) :radio t))

;;;###autoload
(defun empv-play-radio ()
  "Play radio channels."
  (interactive)
  (let ((favorites (mapcar
                    (apply-partially #'alist-get 'uri)
                    (empv--get-bookmarks))))
    (empv--with-empv-metadata
     (empv-play-or-enqueue
      (empv--radio-item-extract-link
       (empv--completing-read-object
        "Channel: "
        empv-radio-channels
        :formatter (lambda (x) (if (equal (cdr x) (plist-get empv-metadata :uri))
                              (format "%s %s" (car x) empv--playlist-current-indicator)
                            (car x)))
        :group (when favorites
                 (lambda (x)
                   (if (member (cdr x) favorites)
                       "Favorites"
                     "Other")))
        :category 'empv-radio-item))))))

;;;###autoload
(defun empv-play-random-channel ()
  "Play a random radio channel."
  (interactive)
  (let ((channel (thread-last
                   empv-radio-channels
                   (length)
                   (random)
                   (empv--flipcall #'nth empv-radio-channels))))
    (empv--display-event "Playing %s" (car channel))
    (empv-play (empv--radio-item-extract-link channel))))

;;;; Videos and music

(defun empv--find-files-1 (path extensions &optional depth)
  "Find files with given EXTENSIONS under given PATH.
PROMPT is shown when `completing-read' is called."
  (let ((default-directory path)
        (is-remote (file-remote-p path)))
    (thread-last
      extensions
      (mapcar (lambda (ext) (format "-e '%s' " ext)))
      (string-join)
      (concat (format "%s . --absolute-path --max-depth %s -c never "
                      empv-fd-binary
                      (or depth empv-max-directory-search-depth)))
      (shell-command-to-string)
      (empv--flipcall #'split-string "\n")
      (empv--seq-init)
      (mapcar (lambda (s) (if is-remote (concat is-remote s) s))))))

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
    (empv--select-file "Select a video file:" empv-video-dir empv-video-file-extensions))))

;;;###autoload
(defun empv-play-audio ()
  "Interactively select and play an audio file from `empv-audio-dir'."
  (interactive)
  (empv-play-or-enqueue
   (empv--select-file "Select an audio file:" empv-audio-dir empv-audio-file-extensions)))

;;;; Consult integration

(declare-function consult--read "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-refresh "consult")
(declare-function consult--async-indicator "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--async-split "consult")

(defun empv--consult-async-generator (request mapper)
  (lambda (next)
    (lambda (action)
      (pcase action
        ((pred stringp)
         (when (not (string-empty-p (string-trim action)))
           (funcall
            request
            action
            (lambda (result)
              (funcall next 'flush)
              (funcall next (funcall mapper result))))))
        (_ (funcall next action))))))

(defun empv--consult-async-wrapper (async)
  (consult--async-pipeline
   (consult--async-split)
   (consult--async-throttle)
   async
   (consult--async-indicator)
   (consult--async-refresh)))

(defun empv--use-consult? ()
  (and empv-use-consult-if-possible (require 'consult nil t)))

;;;; Request utils

(defun empv--request-format-param (pair)
  "Format given PAIR into a URL parameter."
  (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))

(defun empv--build-url (url params)
  "Build a url with URL and url PARAMS."
  (let ((url-params
         (string-join
          (thread-last
            params
            (seq-filter #'cdr)
            (mapcar #'empv--request-format-param))
          "&")))
    (format "%s%s"
            url
            (if (s-blank? url-params)
                ""
              (concat "?" url-params)))))

(defun empv--request (url &optional params callback)
  "Send a GET request to given URL and return the response body.
PARAMS should be an alist.  CALLBACK is called with the resulting JSON
object.  If no callback is given, request is made synchronously and
resulting object is returned."
  (let* ((full-url (empv--build-url url params))
         (default-headers '(("Accept" . "*/*")))
         (custom-headers
          (when (and empv-invidious-instance
                     (string-prefix-p empv-invidious-instance full-url))
            empv-invidious-request-headers))
         (url-request-extra-headers
          (append custom-headers default-headers))
         (handler
          (lambda (status)
            (empv--dbg "empv--request(%s, %s) → error: %s" url params (plist-get status :error))
            (let ((headers (progn
                             (goto-char (point-min))
                             (re-search-forward "\n\n" nil t)
                             (buffer-substring-no-properties (point-min) (point))))
                  (body (decode-coding-string
                         (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
              (empv--dbg "empv--request(%s, %s) → headers: %s, body: %s" url params headers body)
              (kill-buffer)
              (let ((parsed (empv--read-result body)))
                (if callback
                    (funcall callback parsed)
                  parsed))))))
    (if callback
        (url-retrieve full-url handler)
      (with-current-buffer (url-retrieve-synchronously full-url)
        (funcall handler :ok)))))

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

;;;; YouTube/Invidious

;; I don't have good feelings about `tabulated-list-mode'.
;; Nevertheless it's not an excuse for shitty code.  Behold!

;;;;; Interactice

;;;###autoload
(defun empv-youtube (term)
  "Search TERM in YouTube videos."
  (interactive (list (empv--youtube-suggest "Search in YouTube videos: ")))
  (empv--youtube term 'video))

(defalias 'empv-youtube-search #'empv-youtube)

(defun empv-youtube-last-results ()
  "Show and act on last search results."
  (interactive)
  (ignore-error (quit minibuffer-quit)
    (let ((selected (empv--completing-read-object
                     "YouTube results"
                     (empv--yt-search-results empv--last-youtube-search)
                     :formatter #'empv--format-yt-item
                     :category 'empv-youtube-item
                     :sort? nil)))
      ;; Also see `empv-youtube-results-play-or-enqueue-current',
      ;; similar behavior for the tabulated results mode.
      (pcase (empv--yt-search-type empv--last-youtube-search)
        ('channel (empv-youtube-show-channel-videos (alist-get 'authorId selected)))
        (_other (empv-play-or-enqueue (empv--youtube-item-extract-link selected)))))))

(defun empv-youtube-tabulated-last-results ()
  "Show last search results in tabulated mode with thumbnails."
  (interactive)
  (if empv--last-youtube-search
      (empv--youtube-show-tabulated-results empv--last-youtube-search)
    (user-error "Last YouTube search is empty")))

(defalias 'empv-youtube-become-tabulated #'empv-youtube-tabulated-last-results)

;;;###autoload
(defun empv-youtube-playlist (term)
  "Search TERM in YouTube playlists."
  (interactive (list (empv--youtube-suggest "Search in YouTube playlists: ")))
  (empv--youtube term 'playlist))

(defalias 'empv-youtube-playlist-search #'empv-youtube-playlist)

;;;###autoload
(defun empv-youtube-channel (term)
  "Search TERM in YouTube playlists."
  (interactive (list (empv--youtube-suggest "Search in YouTube channels: ")))
  (empv--youtube term 'channel))

(defalias 'empv-youtube-channel-search #'empv-youtube-channel)

;;;###autoload
(defun empv-youtube-show-current-comments ()
  "Show YouTube comments for currently playing (or paused) YouTube video."
  (interactive)
  (empv--let-properties '(path)
    (empv-youtube-show-comments .path)))

(declare-function emojify-mode "emojify")

;;;###autoload
(defun empv-youtube-show-comments (video-id &optional video-info)
  "Show comments of a YouTube VIDEO-ID in a nicely formatted org buffer.
VIDEO-ID can be either a YouTube URL or just a YouTube ID.  If
VIDEO-INFO is non-nil, then add video metadata to beginning of the
buffer."
  (interactive "sURL or ID: ")
  (unless video-info
    (when-let* ((metadata (empv--extract-empv-metadata-from-path video-id))
                (_ (plist-get metadata :youtube)))
      (setq video-info (empv--plist-to-alist metadata))))
  (setq video-id (replace-regexp-in-string "^.*v=\\([A-Za-z0-9_-]+\\).*" "\\1" video-id))
  (empv--request
   (format "%s/comments/%s" empv-invidious-instance video-id)
   '()
   (lambda (result)
     (let ((buffer (get-buffer-create (format "*empv-yt-comments: %s %s*"
                                              (alist-get 'title video-info)
                                              video-id))))
       (switch-to-buffer-other-window buffer)
       (with-current-buffer buffer
         (erase-buffer)
         (org-mode)
         (when (require 'emojify nil t)
           (emojify-mode))
         (when video-info
           (let-alist video-info
             (insert (format "#+title: \"%s\" comments\n\n" .title))
             (unless (s-blank? .description)
               (insert .description "\n\n"))
             (insert (format "- View Count :: %s
- Published :: %s
- Author :: [[elisp:(empv-youtube-show-channel-videos \"%s\")][%s]]"
                             .viewCountText
                             .publishedText
                             .authorId
                             .author))
             (insert "\n\n-----\n")))
         (seq-map
          (lambda (comment)
            (let-alist comment
              (insert (format "* %s (👍 %s)%s\n%s\n"
                              .author .likeCount (if .creatorHeart " ❤️" "") .content))))
          (alist-get 'comments result))
         (goto-char (point-min)))))))

(defun empv-toggle-youtube-tabulated-results ()
  "Toggle YouTube results display between tabulated mode and completing-read."
  (interactive)
  (setq empv-youtube-use-tabulated-results (not empv-youtube-use-tabulated-results))
  (empv--display-event
   "YouTube results will be shown in %s."
   (if empv-youtube-use-tabulated-results "tabulated mode" "completing-read")))

;;;;; YouTube download

;; TODO: Add a simple interface for manipulating `empv-ytdl-download-options'.
(defun empv-youtube-download (link &optional path callback)
  "Download given YouTube LINK to PATH.
If PATH is nil, then ask interactively.  Call CALLBACK after
download finishes with the path downloaded.

By default it downloads as MP3 file, please see
`empv-ytdl-download-options' to change this behavior."
  (interactive "sLink: ")
  (let* ((url (empv--clean-uri link))
         (title (or (plist-get (empv--extract-empv-metadata-from-path link) :title) ""))
         ;; For some reason, embark triggers this
         (use-dialog-box nil)
         (where (or path
                    (read-file-name
                     "Download to: "
                     (if (stringp empv-audio-dir)
                         empv-audio-dir
                       (car empv-audio-dir))
                     nil nil
                     (concat
                      (string-clean-whitespace title)
                      "."
                      ;; Try to detect the format from the
                      ;; options. Otherwise simply default to .mp4.
                      ;; Not the greatest solution but should cover
                      ;; most cases.  Hopefully this part will be
                      ;; removed completely when a UI for download is
                      ;; shown in the future.
                      (or (ignore-errors
                            (cadr
                             (s-split
                              "="
                              (seq-find
                               (lambda (it)
                                 (s-matches? "^--\\(audio-format\\|merge-output-format\\)" it))
                               empv-ytdl-download-options))))
                          "mp4")))))
         (default-directory (file-name-directory where))
         (buffer (generate-new-buffer " *empv-yt-dlp*")))
    (set-process-sentinel
     (apply
      #'start-process
      (buffer-name buffer) buffer
      empv-ytdl-binary url
      `(,@empv-ytdl-download-options "--output" ,(file-name-nondirectory (directory-file-name where))))
     (lambda (proc _)
       (if (eq (process-exit-status proc) 0)
           (progn
             (if (functionp callback)
                 (funcall callback where)
               (progn
                 (message "Downloaded: %s to %s" url where)
                 (kill-new where))
               (empv-play-or-enqueue where)))
         (empv--display-event "Failed to download: %s. See buffer %s for details." url (buffer-name buffer)))))))

;;;;; empv-youtube-results-mode
;;;;;; Mode

(cl-defstruct (empv--yt-search (:constructor empv--make-yt-search)
                               (:copier nil))
  ;; All
  (buffer nil)
  (query nil :type 'string)
  (page 1 :type 'number)
  (type nil :type '(member 'video 'playlist 'channel))
  (kind nil :type '(member 'search 'channel-videos))
  (results '() :type 'list :documentation "List of search results as returned by Invidious.")
  ;; Channel videos
  (channel-id nil :type 'string)
  (sort-by nil :type '(member 'newest 'popular))
  (continuation nil :type 'string :documentation "A continuation token to get the next chunk of items."))

(defun empv--yt-search-generate-buffer-name (search)
  (format "*empv-youtube-%s: %s*"
          (empv--yt-search-kind search)
          (empv--yt-search-query search)))

(defun empv--yt-search-get-buffer-create (search)
  "Get existing one or create buffer for SEARCH."
  (or (empv--yt-search-buffer search)
      (get-buffer-create
       (empv--yt-search-generate-buffer-name search))))

(defvar empv-youtube-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "h") #'tabulated-list-previous-column)
    (define-key map (kbd "l") #'tabulated-list-next-column)
    (define-key map (kbd "P") #'empv-youtube-results-play-current)
    (define-key map (kbd "a") #'empv-youtube-results-enqueue-current)
    (define-key map (kbd "Y") #'empv-youtube-results-copy-current)
    (define-key map (kbd "c") #'empv-youtube-results-show-comments)
    (define-key map (kbd "C") #'empv-youtube-results-open-current-channel)
    (define-key map (kbd "i") #'empv-youtube-results-inspect)
    (define-key map (kbd "m") #'empv-youtube-results-load-more)
    (define-key map (kbd "d") #'empv-youtube-results-download-current)
    (define-key map (kbd "RET") #'empv-youtube-results-play-or-enqueue-current)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `empv-youtube-results-mode'.")

(define-derived-mode empv-youtube-results-mode tabulated-list-mode "empv-youtube-results-mode"
  "Major mode for interacting with YouTube results with thumbnails."
  (setq tabulated-list-padding 3)
  ;; Enable help-at-pt so that video title is fully shown without
  ;; being truncated
  (setq-local help-at-pt-display-when-idle t)
  (setq-local help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

;;;;;; Table printing

(defun empv--youtube-show-tabulated-results (search)
  (let ((buffer (empv--yt-search-get-buffer-create search)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'empv-youtube-results-mode)
        (empv-youtube-results-mode))
      (pop-to-buffer-same-window (current-buffer))
      (setq empv--buffer-youtube-search search)
      (empv--youtube-tabulated-entries-put search (empv--yt-search-results search)))))

(defun empv--youtube-tabulated-entries-put (search candidates &optional append?)
  "Display CANDIDATES in the YouTube results buffer for SEARCH.
If APPEND? is non-nil, add the CANDIDATES to the end of the
table.

This function should called within a `empv-youtube-results-mode'
buffer."
  (let* ((headers (pcase (empv--yt-search-type search)
                    ('video empv-youtube-tabulated-video-headers)
                    ('playlist empv-youtube-tabulated-playlist-headers)
                    ('channel empv-youtube-tabulated-channel-headers)))
         (thumbnail-column (empv--seq-find-index (lambda (it) (equal empv-thumbnail-placeholder (nth 3 it))) headers)))
    (unless tabulated-list-format
      (empv--youtube-format-tabulated-list headers))
    (let* ((offset (if append? (length tabulated-list-entries) 0))
           (new-entries (seq-map-indexed
                         (lambda (it index)
                           (list (+ offset index) (empv--youtube-results-mode-format-entry headers it)))
                         candidates)))
      (setq tabulated-list-entries
            (if append?
                (append tabulated-list-entries new-entries)
              new-entries))
      (tabulated-list-print t)
      (back-to-indentation)
      (when (and thumbnail-column empv-youtube-thumbnail-quality)
        (empv--youtube-tabulated-load-thumbnails
         candidates
         thumbnail-column
         offset))
      (seq-do (lambda (fn) (funcall fn new-entries)) empv-youtube-tabulated-new-entries-hook))))

(defun empv--youtube-format-tabulated-list (headers)
  (setq
   tabulated-list-format
   (seq-into
    (seq-map
     (lambda (it)
       (let ((name (nth 0 it))
             (len (nth 1 it))
             (sort (let ((sort (nth 2 it)))
                     (if (functionp sort)
                         (lambda (x y)
                           (funcall
                            sort
                            (nth (car x) (empv--yt-search-results empv--buffer-youtube-search))
                            (nth (car y) (empv--yt-search-results empv--buffer-youtube-search))))
                       sort))))
         (list name len sort)))
     headers)
    'vector))
  (tabulated-list-init-header))

(defun empv--youtube-results-mode-format-entry (headers it)
  (seq-into
   (seq-map
    (lambda (col)
      (let-alist (cdr it)
        (propertize
         (pcase (nth 3 col)
           ;; I truncate the title (and `other' below) manually because
           ;; when tabulated-list-mode does it, some columns gets
           ;; misaligned for some reason
           ((and (pred functionp) fn) (funcall fn (cdr it)))
           ('.title (propertize
                     (s-truncate (- (nth 1 col) 1) .title "…")
                     'help-echo .title))
           ('.lengthSeconds (empv--format-yt-duration .lengthSeconds))
           ('.viewCount (empv--format-yt-views .viewCount))
           ('.autoGenerated (if (eq .autoGenerated :json-false) "No" "Yes"))
           ('.authorVerified (if (eq .authorVerified :json-false) "No" "Yes"))
           ((pred (equal empv-thumbnail-placeholder)) (propertize empv-thumbnail-placeholder 'help-echo .title))
           (other (let ((value (or (empv--alist-path-get other (cdr it)) "N/A")))
                    (propertize
                     (s-truncate (- (nth 1 col) 1) (format "%s" value)  "…")
                     'help-echo value))))
         'empv-youtube-item it)))
    headers)
   'vector))

(defun empv--youtube-find-reasonable-thumbnail (thumb-list)
  "Find a thumb from THUMB-LIST, according to `empv-youtube-thumbnail-quality'.

>> (let ((empv-youtube-thumbnail-quality \"maxresdefault\"))
     (empv--youtube-find-reasonable-thumbnail
      (quote (((quality . \"maxres\")
               (url . \"https://some-invidious-address.com/maxres.jpg\")
               (width . 1280) (height . 720))
              ((quality . \"maxresdefault\")
               (url . \"https://some-invidious-address.com/maxresdefault.jpg\")
               (width . 1280) (height . 720))
              ((quality . \"high\")
               (url . \"https://some-invidious-address.com/hqdefault.jpg\")
               (width . 480) (height . 360))))))
=> \"https://some-invidious-address.com/maxresdefault.jpg\"

>> (let ((empv-youtube-thumbnail-quality \"maxresdefault\"))
     (empv--youtube-find-reasonable-thumbnail
      (quote (((url
                . \"//yt3.googleusercontent.com/something1\")
               (width . 32) (height . 32))
              ((url
                . \"//yt3.googleusercontent.com/something5\")
               (width . 176) (height . 176))))))
=> \"https://yt3.googleusercontent.com/something5\""
  (let ((thumb (thread-last
                 ;; For video thumbnails, there is `quality' and for channels, there is width/height.
                 (or
                  ;; First, try to find a video thumbnail
                  (seq-find
                   (lambda (thumb)
                     (equal empv-youtube-thumbnail-quality
                            (alist-get 'quality thumb)))
                   thumb-list)
                  ;; Otherwise, try to find one by position (they are generally in ascending order)
                  (pcase empv-youtube-thumbnail-quality
                    ((or "maxres" "maxresdefault") (car (last thumb-list)))
                    (_ (nth 2 thumb-list)))
                  (car thumb-list))
                 (alist-get 'url))))
    (if (s-prefix? "//" thumb)
        (concat "https:" thumb)
      thumb)))

(defun empv--youtube-tabulated-load-thumbnails (candidates thumbnail-col-index &optional index-offset)
  (let ((total-count (length candidates))
        (completed-count 0)
        (buffer (current-buffer)))
    (seq-do-indexed
     (lambda (video index)
       (let* ((info (cdr video))
              (id (or (alist-get 'videoId info)
                      (alist-get 'playlistId info)
                      (alist-get 'authorId info)))
              (filename (format
                         (expand-file-name "~/.cache/empv_%s_%s.jpg")
                         id
                         empv-youtube-thumbnail-quality))
              (thumb-url (or
                          (alist-get 'playlistThumbnail info)
                          (empv--youtube-find-reasonable-thumbnail (alist-get 'videoThumbnails info))
                          (empv--youtube-find-reasonable-thumbnail (alist-get 'authorThumbnails info))))
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
                 (format "empv-download-process-%s" id)
                 " *empv-thumbnail-downloads*"
                 args)
          (lambda (_ _)
            (empv--dbg "Download finished for image index=%s, url=%s, path=%s" index thumb-url filename)
            (with-current-buffer buffer
              (setf
               (elt (car (alist-get (+ (or index-offset 0) index) tabulated-list-entries)) thumbnail-col-index)
               (cons 'image `(:type ,(empv--get-image-type filename) :file ,filename ,@empv-youtube-thumbnail-props)))
              (setq completed-count (1+ completed-count))
              (when (eq completed-count total-count)
                (tabulated-list-print t)
                (back-to-indentation)))))))
     candidates)))

(defun empv--get-image-type (file)
  "Return image type of FILE.
It is based on file extension.  See Info node `(elisp) Image Formats' for
supported formats."
  ;; (if (string-prefix-p
  ;;      "\x89PNG\r\n\x1a\n"
  ;;      (with-temp-buffer
  ;;        (insert-file-contents-literally file nil 0 10)
  ;;        (buffer-string)))
  ;;     'png
  ;;   'jpeg)
  (pcase (intern (file-name-extension file))
    ('jpg 'jpeg)
    (other other)))

(defun empv-youtube-results--current-item ()
  (save-excursion
    (beginning-of-line)
    (prop-match-value
     (text-property-search-forward 'empv-youtube-item))))

(defun empv-youtube-results--current-item-url ()
  (empv--youtube-item-extract-link (empv-youtube-results--current-item)))

;;;;;; Interactive functions

(defmacro empv--youtube-result-dispatch-type (&rest cases)
  "Act according to current result type.
It might be one of `empv--yt-search-type', see `empv--yt-search'.

CASES are in the form of (<type> <expr>).

A variable representing currently hovered search result named
`current-item' is introduced to the scope."
  `(let ((current-item (empv-youtube-results--current-item)))
     (ignore current-item)
     (pcase (empv--yt-search-type empv--buffer-youtube-search)
       ,@(mapcar (lambda (case) (list (car case) (cadr case))) (seq-partition cases 2)))))

(defun empv-youtube-results-play-current ()
  "Play the currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv-play (empv-youtube-results--current-item-url)))

(defun empv-youtube-results-enqueue-current ()
  "Enqueue the currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv-enqueue (empv-youtube-results--current-item-url)))

(defun empv-youtube-results-play-or-enqueue-current ()
  "Play or enqueue the currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv--youtube-result-dispatch-type
   'channel (empv-youtube-results-open-current-channel)
   _others (empv-play-or-enqueue (empv-youtube-results--current-item-url))))

(defun empv-youtube-results-copy-current ()
  "Copy the URL of the currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv-youtube-copy-link (empv-youtube-results--current-item-url)))

(defun empv-youtube-results-show-comments ()
  "Show comments of the currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv--youtube-result-dispatch-type
   'channel (user-error "No comments for channels")
   _others (empv-youtube-show-comments
            (empv-youtube-results--current-item-url)
            (empv-youtube-results--current-item))))

(defun empv-youtube-results-download-current ()
  "Download currently selected item in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv-youtube-download
   (empv-youtube-results--current-item-url)
   nil
   (lambda (file) (empv--display-event "Download completed: %s" file))))

(defun empv-youtube-results-inspect ()
  "Inspect the currently selected video in `empv-youtube-results-mode'.
This simply shows the data returned by the invidious API in a
nicely formatted buffer."
  (interactive nil empv-youtube-results-mode)
  (empv--inspect-obj (empv-youtube-results--current-item)))

(defun empv-youtube-results-open-current-channel ()
  "Open selected video's channel videos in `empv-youtube-results-mode'."
  (interactive nil empv-youtube-results-mode)
  (empv-youtube-show-channel-videos (alist-get 'authorId (empv-youtube-results--current-item))))

(defun empv-youtube-results-load-more ()
  "Load the next page of results."
  (interactive nil empv-youtube-results-mode)
  (let* ((buff (current-buffer))
         (kind (empv--yt-search-kind empv--buffer-youtube-search))
         (callback (lambda (results)
                     (with-current-buffer buff
                       ;; Increase the current page
                       (let ((old-results (empv--yt-search-results empv--buffer-youtube-search)))
                         (setf
                          (empv--yt-search-page empv--buffer-youtube-search) (1+ (empv--yt-search-page empv--buffer-youtube-search))
                          (empv--yt-search-results empv--buffer-youtube-search) (append old-results results)))
                       (setq empv--last-youtube-search empv--buffer-youtube-search)
                       (empv--youtube-tabulated-entries-put empv--buffer-youtube-search results :append)))))
    (pcase kind
      ('search
       (empv--youtube-search
        (empv--yt-search-query empv--buffer-youtube-search)
        (empv--yt-search-type empv--buffer-youtube-search)
        (1+ (empv--yt-search-page empv--buffer-youtube-search))
        callback))
      ('channel-videos
       (empv--youtube-channel-videos
        (empv--yt-search-channel-id empv--buffer-youtube-search)
        (empv--yt-search-sort-by empv--buffer-youtube-search)
        (empv--yt-search-continuation empv--buffer-youtube-search)
        (lambda (result)
          (funcall callback (alist-get 'videos result))
          (setf (empv--yt-search-continuation empv--buffer-youtube-search) (alist-get 'continuation result))))))))

;;;;; Channel stuff

;;;###autoload
(defun empv-youtube-show-channel-videos (channel-id &optional sort-by)
  (interactive "sChannel ID or URL: ")
  ;; Extract channel id from the url?
  (when-let* ((match (s-match "\\/channel\\/\\([^/?]+\\)" channel-id)))
    (setq channel-id (nth 1 match)))
  ;; Extract the channelid id from empv enriched url
  (when-let* ((metadata (empv--extract-empv-metadata-from-path channel-id))
              (id (plist-get metadata :authorId)))
    (setq channel-id id))
  (let ((sort-by (or sort-by
                     (empv--select-action "Sort videos of channel by: "
                       "Popular" → 'popular
                       "Newest" → 'newest))))
    (empv--youtube-channel-videos
     channel-id
     sort-by
     nil
     (lambda (result)
       (let ((channel-name (or (thread-last
                                 result
                                 (alist-get 'videos)
                                 (car)
                                 (alist-get 'author))
                               channel-id)))
         (setq empv--last-youtube-search
               (empv--make-yt-search
                :query channel-name
                :channel-id channel-id
                :type 'video
                :kind 'channel-videos
                :page 1
                :sort-by sort-by
                :continuation (alist-get 'continuation result)
                :results (alist-get 'videos result))))
       (if empv-youtube-use-tabulated-results
           (empv-youtube-tabulated-last-results)
         (empv-youtube-last-results))))))

;;;;; Utilities

;;;;;; Formatters

(defun empv--human-readable-number (num)
  "Format NUM into a human-readable string with K, M, B suffixes."
  (cond
   ((< num 1000) (format "%d" num))
   ((< num 1000000) (format "%.1fK" (/ num 1000.0)))
   ((< num 1000000000) (format "%.1fM" (/ num 1000000.0)))
   (t (format "%.1fB" (/ num 1000000000.0)))))

(defun empv--format-yt-views (view-count)
  (concat (empv--human-readable-number view-count) " views"))

(defun empv--format-yt-subcount (sub-count)
  (concat (empv--human-readable-number sub-count) " subs"))

(defun empv--format-yt-duration (seconds)
  (let ((hours (/ seconds 3600))
        (minutes (mod (/ seconds 60) 60))
        (remaining-seconds (mod seconds 60)))
    (cond
     ((< hours 1) (format "%02d:%02d" minutes remaining-seconds))
     (t (format "%d:%02d:%02d" hours minutes remaining-seconds)))))

(defun empv--format-yt-item (it)
  "Format IT into a type specific text."
  (let-alist it
    (pcase (alist-get 'type it)
      ("video" (concat
                (s-pad-right
                 25
                 " "
                 (concat
                  "["
                  (propertize (format "%s, %s"
                                      (empv--format-yt-views .viewCount)
                                      (empv--format-yt-duration .lengthSeconds))
                              'face
                              'italic)
                  "]"))
                "| "
                .title))
      ("playlist" (concat
                   (s-pad-right
                    50 " "
                    (propertize (format "%s videos by %s" .videoCount .author)
                                'face
                                'italic))
                   " | "
                   .title))
      ("channel"
       (concat
        (s-pad-right 50 " " (format "%s%s [%s]"
                                    .author
                                    (if (eq .authorVerified t) " ✓" "")
                                    (empv--format-yt-subcount .subCount)) )
        " -- "
        (s-truncate 100 (propertize (or .description "") 'face 'italic)))))))

;;;;;; Request

(defun empv--youtube-search (term type page callback)
  "Search TERM in YouTube.
TYPE determines what to search for, it's either video or
playlist.  By default it's video.  Call CALLBACK when request
finishes."
  (setq type (symbol-name (or type 'video)))
  (empv--request
   (format "%s/search" empv-invidious-instance)
   `(("q" . ,term)
     ("page" . ,(number-to-string page))
     ("type" . ,type))
   (lambda (results)
     ;; For some reason, even if we set the type to "video", "channel"
     ;; type results may appear in the returned data from
     ;; Invidious. Filtering it out here so that it does not interfere
     ;; with tabulated-list building logic etc..
     (funcall
      callback
      (seq-filter
       (lambda (it)
         (equal type (alist-get 'type it type)))
       results)))))

(defun empv--youtube-channel-videos (channel-id sort-by continuation callback)
  "Get videos for CHANNEL-ID sorted by SORT-BY.
SORT-BY can be one of \\='newest, \\='popular or \\='oldest (Broken as
of 10/2022 on Invidious).  Default to newest.

CONTINUATION is the continuation token.  CALLBACK is called with results
parameter."
  (empv--request
   (format "%s/channels/%s/videos" empv-invidious-instance channel-id)
   `(("sort_by" . ,(or (symbol-name sort-by) "newest"))
     ("continuation" . ,continuation))
   callback))

(defun empv--youtube-item-extract-link (item)
  "Find and return YouTube url for ITEM."
  (let ((video-id (alist-get 'videoId item))
        (playlist-id (alist-get 'playlistId item))
        (author-id (alist-get 'authorId item)))
    (let-alist item
      (empv--url-with-magic-info
       (concat "https://youtube.com/"
               (cond
                (video-id (concat "watch?v=" video-id))
                (playlist-id (concat "playlist?list=" playlist-id))
                (author-id (concat "channel/" author-id))))
       :title (or .title .author)
       :youtube t
       :authorId .authorId
       :author .author))))

(defun empv--youtube (term type)
  "Search TERM in YouTube.
See `empv--youtube-search' for TYPE."
  (empv--youtube-search
   term type 1
   (lambda (results)
     (setq empv--last-youtube-search
           (empv--make-yt-search
            :query term
            :type type
            :results results
            :kind 'search))
     (if empv-youtube-use-tabulated-results
         (empv-youtube-tabulated-last-results)
       (empv-youtube-last-results)))))

;;;;; Consult integration

(defvar consult-async-split-style)

(defun empv--consult-get-input-with-suggestions (prompt)
  "Get an input from user, using YouTube search suggestions.
PROMPT is passed to `completing-read' as-is."
  (let ((consult-async-split-style 'none))
    (consult--read
     (empv--consult-async-generator
      (lambda (action on-result)
        (empv--request
         (format "%s/search/suggestions" empv-invidious-instance)
         `(("q" . ,action))
         on-result))
      (lambda (result)
        (alist-get 'suggestions result)))
     :prompt prompt
     :category 'empv-youtube-suggestion-item
     :sort nil
     :history 'empv--youtube-search-history
     :require-match nil
     :async-wrap #'empv--consult-async-wrapper)))

(defun empv--youtube-suggest (prompt)
  (unless empv-invidious-instance
    (user-error "Please configure `empv-invidious-instance'"))
  (if (empv--use-consult?)
      (empv--consult-get-input-with-suggestions prompt)
    (read-string prompt nil 'empv--youtube-search-history)))

;;;; Subsonic

(defvar empv--subsonic-client-name "emacs-empv")
(defvar empv--subsonic-api-version "1.16.0")
(defvar empv--subsonic-salt-length 10)
(defvar empv--subsonic-search-prompt "Subsonic search: ")

;;;;; Requests & builders

(defun empv--subsonic-build-url (endpoint &rest params)
  (let* ((salt (empv--random-string empv--subsonic-salt-length))
         (token (md5 (concat empv-subsonic-password salt))))
    (empv--build-url
     (concat empv-subsonic-url "/rest/" endpoint)
     `(("u" . ,empv-subsonic-username)
       ("s" . ,salt)
       ("t" . ,token)
       ("v" . ,empv--subsonic-api-version)
       ("c" . ,empv--subsonic-client-name)
       ("f" . "json")
       ,@(empv--plist-to-alist params)))))

(defun empv--subsonic-item-extract-url (object)
  "Build streamable url for given Subsonic OBJECT."
  (empv--url-with-magic-info
   (empv--subsonic-build-url "stream.view" :id (alist-get 'id object))
   :title (substring-no-properties (empv--subsonic-format-candidate object))
   :kind (alist-get 'kind object)
   :subsonic t))

(defun empv--subsonic-request (endpoint &rest rest)
  "Make a request to ENDPOINT.
REST are URL params.  If last of REST is a function, then it is used as
the callback function.  Otherwise the request is made synchronously and
resulting object is returned.

This function does not return subsonic responses verbatim.  It processes
them so that responses are easier to work with."
  (when (seq-some #'s-blank? (list empv-subsonic-url empv-subsonic-username empv-subsonic-password))
    (user-error "Please configure Subsonic first"))
  (let* ((callback (when-let* ((last (empv--seq-last rest))
                               (_ (functionp last)))
                     last))
         (params (if callback
                     (ignore-errors (empv--seq-init rest))
                   rest))
         (handler
          (lambda (result)
            ;; I know this is horrible but Subsonic responses are much worse.
            (let-alist result
              (unless (equal "ok" .subsonic-response.status)
                (empv--dbg "empv--subsonic-request failed :: endpoint=%s, rest=%s, response=%s" endpoint rest result)
                (error "Request to subsonic failed."))
              (let ((result (or
                             (alist-get
                              (thread-last
                                endpoint
                                (s-split "\\.")
                                (car)
                                (s-chop-prefix "get")
                                (s-lower-camel-case)
                                (intern))
                              .subsonic-response)
                             ;; FIXME: This does not look good
                             ;; This is the only non-conforming result object.
                             .subsonic-response.searchResult3)))
                (let ((info '())
                      (results '())
                      (result-fields '(artist album song playlist genre)))
                  ;; TODO: Document what is going on here
                  (seq-do
                   (lambda (key)
                     (let ((val (alist-get key result)))
                       (if (listp val)
                           (setq results
                                 (append results
                                         (seq-map
                                          (lambda (x)
                                            (thread-first
                                              x
                                              (map-insert 'kind key)
                                              (map-insert 'type 'subsonic)))
                                          val)))
                         (setq info (map-insert info key val)))))
                   result-fields)
                  (seq-do
                   (lambda (key)
                     (setq info (map-insert info key (alist-get key result))))
                   (seq-difference (map-keys result) result-fields))
                  ;; Also handle /getIndexes and /getArtists responses and flatten them
                  (when-let* ((index (alist-get 'index result)))
                    (setq
                     results
                     (seq-mapcat
                      (lambda (index)
                        (let ((index-name (alist-get 'name index)))
                          (seq-map
                           (lambda (val)
                             (thread-first
                               val
                               (map-insert 'indexName index-name)
                               (map-insert 'kind 'artist)))
                           (alist-get 'artist index))))
                      index)))
                  (let ((all `(,@info (results . ,results))))
                    (if callback
                        (funcall callback all)
                      all))))))))
    (if callback
        (empv--request (apply #'empv--subsonic-build-url endpoint params) nil handler)
      (funcall handler (empv--request (apply #'empv--subsonic-build-url endpoint params) nil)))))

(defun empv--subsonic-format-candidate (cand)
  (empv--with-text-properties
   (pcase (alist-get 'kind cand)
     ('artist (alist-get 'name cand))
     ('album (format
              "%s - %s"
              (propertize (alist-get 'artist cand) 'face 'italic)
              (propertize (alist-get 'name cand) 'face 'bold)))
     ('song (format
             "%s - %s"
             (propertize (alist-get 'artist cand) 'face 'italic)
             (propertize (alist-get 'title cand) 'face 'bold)))
     ('genre (format
              "%s %s"
              (propertize (alist-get 'value cand) 'face 'bold)
              (propertize (format "[%s songs]" (alist-get 'songCount cand)) 'face 'italic)))
     (other (error "empv--subsonic-format-candidate :: No formatter found for: %s" other)))
   :item cand))

(defun empv--subsonic-result-handler (prompt)
  (lambda (results)
    (setq results (alist-get 'results results))
    (empv--subsonic-act-on-candidate
     (empv--completing-read-object
      prompt
      results
      :formatter #'empv--subsonic-format-candidate
      :category 'empv-subsonic-item
      :group (lambda (object)
               (or (alist-get 'indexName object)
                   (s-titleize (symbol-name (alist-get 'kind object)))))
      :sort? nil))))

(defun empv--subsonic-consult-search ()
  (empv--subsonic-act-on-candidate
   (consult--read
    (empv--consult-async-generator
     (lambda (action on-result)
       (empv--subsonic-request
        "search3.view"
        :query action
        :artistCount empv-subsonic-result-count
        :albumCount empv-subsonic-result-count
        :songCount empv-subsonic-result-count
        on-result))
     (lambda (result)
       (mapcar #'empv--subsonic-format-candidate (alist-get 'results result))))
    :prompt empv--subsonic-search-prompt
    :category 'empv-subsonic-item
    :lookup (lambda (selected candidates &rest _)
              (empv--get-text-property (car (member selected candidates)) :item))
    :sort nil
    :group
    (lambda (cand transform)
      (if transform
          cand
        (s-titleize (symbol-name (alist-get 'kind (empv--get-text-property cand :item))))))
    :history 'empv--subsonic-search-history
    ;; TODO: :narrow ...?
    :require-match t
    :async-wrap #'empv--consult-async-wrapper)))

(defun empv--subsonic-completing-read-search ()
  (empv--subsonic-request
   "search3.view"
   :query (read-string "Query: " nil 'empv--subsonic-search-history)
   :artistCount empv-subsonic-result-count
   :albumCount empv-subsonic-result-count
   :songCount empv-subsonic-result-count
   (empv--subsonic-result-handler empv--subsonic-search-prompt)))

(defun empv--subsonic-act-on-candidate (selected)
  (empv--dbg "empv--subsonic-act-on-candidate :: %s" selected)
  (let* ((id (alist-get 'id selected)))
    (pcase (alist-get 'kind selected)
      ('song
       (empv-play-or-enqueue
        (empv--subsonic-item-extract-url selected)))
      ('album
       (empv--subsonic-request
        "getAlbum.view"
        :id id
        (empv--subsonic-result-handler (format "Select song from '%s':" (alist-get 'name selected)))))
      ('artist
       (empv--subsonic-request
        "getArtist.view"
        :id id
        (empv--subsonic-result-handler (format "Select song of '%s':" (alist-get 'name selected)))))
      ('genre
       (empv--subsonic-request
        "getSongsByGenre.view"
        :genre (alist-get 'value selected)
        :count empv-subsonic-result-count
        (empv--subsonic-result-handler (format "Select song from '%s' genre:" (alist-get 'value selected)))))
      (other (error "Not found %s" other)))))

(defun empv--subsonic-select-genre-sync ()
  "Select a genre using `completing-read', sync."
  (alist-get
   'value
   (funcall
    (empv--subsonic-result-handler "Select genre:")
    (empv--subsonic-request "getGenres.view"))))

;;;;; Interactive functions

;;;###autoload
(defun empv-subsonic-search ()
  "Search for artists/songs/albums in Subsonic."
  (interactive)
  (if (empv--use-consult?)
      (empv--subsonic-consult-search)
    (empv--subsonic-completing-read-search)))

;;;###autoload
(defun empv-subsonic-artists ()
  "List all Subsonic artists and act."
  (interactive)
  (empv--subsonic-request
   "getArtists.view"
   (empv--subsonic-result-handler "Select artist:")))

;;;###autoload
(defun empv-subsonic-songs ()
  "List songs by a filter and act."
  (interactive)
  (let ((handler (empv--subsonic-result-handler "Select song:")))
    (empv--select-action "Get songs: "
      "Random" →
      (empv--subsonic-request
       "getRandomSongs.view"
       :size empv-subsonic-result-count
       handler)
      "Random by genre" →
      (empv--subsonic-request
       "getRandomSongs.view"
       :size empv-subsonic-result-count
       :genre (empv--subsonic-select-genre-sync) handler)
      "By genre" →
      (empv--subsonic-request
       "getSongsByGenre.view"
       :count empv-subsonic-result-count
       :genre (empv--subsonic-select-genre-sync) handler))))

;;;###autoload
(defun empv-subsonic-albums ()
  "List albums by a filter and act."
  (interactive)
  (let ((get-type (empv--select-action "Get albums: "
                    "Random" → "random"
                    "Recently played" → "recent"
                    "Newest" → "newest"
                    "Frequently played" → "frequent"
                    "Starred" → "starred"
                    "By genre" → "byGenre")))
    (empv--subsonic-request
     "getAlbumList2.view"
     :type get-type
     :genre (when (equal get-type "byGenre")
              (empv--subsonic-select-genre-sync))
     :size empv-subsonic-result-count
     (empv--subsonic-result-handler "Select album:"))))

;;;;; Embark actions for subsonic

;; Not every Subsonic item corresponds to a playable url.  So we
;; define wrappers for empv-{play,enqueue} here for Subsonic results
;; which are used in `defvar-keymap'.

(defun empv--subsonic-act (action obj)
  "Do ACTION given Subsonic OBJ.
If OBJ is a single song, then do ACTION on it.  If OBJ is an album, then
do ACTION on all songs of given album.  If OBJ is something else, then
error out."
  (pcase (alist-get 'kind obj)
    ('album
     (empv--subsonic-request
      "getAlbum.view"
      :id (alist-get 'id obj)
      (lambda (album)
        (funcall
         (or action #'empv-play)
         (mapcar #'empv--subsonic-item-extract-url (alist-get 'results album))))))
    ('song (funcall action (empv--subsonic-item-extract-url obj)))
    (other (user-error "Not applicable for %s" other))))

(defun empv-subsonic-play (subsonic-result)
  "Play the SUBSONIC-RESULT."
  (empv--subsonic-act #'empv-play subsonic-result))

(defun empv-subsonic-enqueue (subsonic-result)
  "Enqueue the SUBSONIC-RESULT."
  (empv--subsonic-act #'empv-enqueue subsonic-result))

(defun empv-subsonic-enqueue-next (subsonic-result)
  "Enqueue the SUBSONIC-RESULT after current item."
  (empv--subsonic-act #'empv-enqueue-next subsonic-result))

;; NOTE: It is not a good idea to store the constructed playable
;; subsonic URL as the bookmark because it contains a token. Also it
;; doesn't work well with other Subsonic kinds (apart from songs; like
;; albums, artists etc.).
(defun empv-subsonic-bookmark-set (subsonic-result)
  "Create an empv bookmark from SUBSONIC-RESULT."
  (empv-bookmark-set subsonic-result))

;;;; Bookmarks integration

(defconst empv--bookmark-known-mode-list '(empv-youtube-results-mode))

(defun empv-bookmark-set (&optional target)
  "Create an empv bookmark from TARGET.
TARGET is either a buffer (like `empv-youtube-results-mode' buffer) or a
URL.  If invoked by empv itself, URL generally contains all the metadata
required to build the bookmark, see `empv--title-sep' for details."
  (interactive (unless (member major-mode empv--bookmark-known-mode-list)
       (list (read-string "URL: "))))
  (cond
   ((and (listp target) (eq (alist-get 'type target) 'subsonic))
    (let ((name (alist-get 'name target))
          (title (alist-get 'title target))
          (value (alist-get 'value target)))
      (empv--create-bookmark
       (or title value name)
       (seq-filter
        #'identity
        `((type . subsonic)
          (kind . ,(alist-get 'kind target))
          ,(assoc 'name target)
          ,(assoc 'title target)
          ,(assoc 'value target)
          ,(assoc 'artist target)
          ,(assoc 'songCount target)
          ,(assoc 'id target))))))
   ((stringp target)
    (let ((info (empv--extract-empv-metadata-from-path target)))
      (empv--create-bookmark
       (plist-get info :title)
       (append
        `((type . uri))
        (empv--plist-to-alist info)))))
   ((derived-mode-p 'empv-youtube-results-mode)
    (empv--create-bookmark
     (string-replace "*" "" (empv--yt-search-generate-buffer-name empv--buffer-youtube-search))
     `((type . ,(empv--yt-search-type empv--buffer-youtube-search))
       (kind . ,(empv--yt-search-kind empv--buffer-youtube-search))
       (query . ,(empv--yt-search-query empv--buffer-youtube-search))
       (channel-id . ,(empv--yt-search-channel-id empv--buffer-youtube-search))
       (sort-by . ,(empv--yt-search-sort-by empv--buffer-youtube-search)))))
   (:otherwise (user-error "This mode is not supported by empv-bookmarks: %s" major-mode))))

;;;###autoload
(defun empv-bookmark-jump (bookmark)
  "Jump to empv BOOKMARK."
  (interactive (list
      (progn
        (assoc
         (completing-read
          "Bookmark: "
          (or (empv--get-bookmarks)
              (user-error "No empv bookmarks found"))
          nil t nil 'bookmark-history)
         bookmark-alist))))
  (let ((type (bookmark-prop-get bookmark 'type))
        (kind (bookmark-prop-get bookmark 'kind))
        (query (bookmark-prop-get bookmark 'query))
        (record (bookmark-get-bookmark-record bookmark)))
    (pcase (list type kind)
      (`(uri ,_)
       (let ((data (empv--alist-to-plist record)))
         (empv-play
          (apply
           #'empv--url-with-magic-info
           (plist-get data :uri)
           data))))
      (`(subsonic ,_kind)
       (empv--subsonic-act-on-candidate record))
      ;; Rest is for matching YouTube bookmarks
      ;; FIXME: this type & kind is just mess for YouTube.  Probably
      ;; going to break people's bookmarks when I rework those.  Maybe
      ;; I can add a small migrator when I do that.
      ('(video search) (empv-youtube (bookmark-prop-get bookmark 'query)))
      ('(video channel-videos) (empv-youtube-show-channel-videos
                                (bookmark-prop-get bookmark 'channel-id)
                                (bookmark-prop-get bookmark 'sort-by)))
      ('(playlist search) (empv-youtube-playlist (bookmark-prop-get bookmark 'query)))
      ;; TODO: We don't have a way of showing playlist videos right now in empv
      ('(channel search) (empv-youtube-channel query)))))


(defvar empv--bookmark-name-prefix "empv :: ")

(defun empv--create-bookmark (default props)
  (let* ((name (read-from-minibuffer "Bookmark name: " (concat empv--bookmark-name-prefix default) nil nil 'bookmark-history default))
         (bookmark-make-record-function
          (lambda ()
            (setq bookmark-current-bookmark nil)
            `(,name
              ,@props
              (handler . ,#'empv-bookmark-jump)))))
    (bookmark-set name)
    (message "Stored bookmark: %s" name)))

(defun empv--get-bookmarks ()
  (bookmark-maybe-load-default-file)
  (seq-filter
   (lambda (it)
     (eq (bookmark-prop-get it 'handler) #'empv-bookmark-jump))
   bookmark-alist))

;;;; empv utility

(defun empv-override-quit-key ()
  "Override `q' key to \"pause and hide video\" action.

This function overrides the `q' key so that you don't accidentally quit
mpv, resulting in a loss of your current playlist.

Instead of quitting mpv, it hides the video view (just like the `_'
binding) and pauses the playback.  If you want to hide the video view,
without pausing you can still use `_' key binding.  To really exit, you
can still use `empv-exit' function.

To make this behavior permanant, add the following to your init file:

    (add-hook \\='empv-init-hook #\\='empv-override-quit-key)

Also see `empv-reset-playback-speed-on-quit' for resetting playback
speed to 1 after quitting the video view."
  (empv--cmd
   'keybind `("q" ,(format "set pause yes; %s set force-window no; cycle video;"
                           (if empv-reset-playback-speed-on-quit
                               "set speed 1;"
                             "")))))

(defvar org-link-any-re)
(declare-function org-element-property "org")
(declare-function org-element-context "org")
(declare-function shr-url-at-point "shr")
(declare-function dired-get-marked-files "dired")
(defun empv-media-at-point ()
  "Return the potential media item at the point.
It may be an absolute filepath, it may be a relative file
path.  No guarantees."
  (or
   (when (derived-mode-p 'dired-mode)
     (dired-get-marked-files))
   (when (derived-mode-p 'org-mode)
     (ignore-errors (org-element-property :path (org-element-context))))
   (when (derived-mode-p 'empv-youtube-results-mode)
     (empv-youtube-results--current-item-url))
   (ignore-errors (shr-url-at-point nil))
   (thing-at-point 'url)
   (thing-at-point 'existing-filename)
   (thing-at-point 'filename)))

(defun empv-play-media-at-point ()
  "Play the media at point."
  (interactive)
  (empv-play-or-enqueue (empv-media-at-point)))

(defalias 'empv-play-thing-at-point #'empv-play-media-at-point)

;;;; Lyrics manager

(defvar empv-lyrics-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'empv-lyrics-save)
    (define-key map (kbd "C-c C-w") #'empv-lyrics-force-web)
    map)
  "Keymap for `empv-lyrics-display-mode'.")

(define-derived-mode empv-lyrics-display-mode text-mode "empv-lyrics-display-mode"
  "Major mode for displaying lyrics of a given song.")

(defvar custom-button)

(cl-defun empv--lyrics-display (path title lyrics &key url)
  (with-current-buffer (get-buffer-create "*empv-lyrics*")
    (empv-lyrics-display-mode)
    (erase-buffer)
    (insert lyrics)
    (setq header-line-format (format "Lyrics :: %s" title))
    (setq empv--current-file path)
    (insert "\n\n")
    (when url
      (goto-char (point-max))
      (insert-button
       "Source"
       'action
       (lambda (_button)
         (browse-url url))
       'face custom-button
       'follow-link t)
      (when (and path (file-exists-p (expand-file-name path)))
        (insert " ")
        (insert-button
         "Save to file metadata"
         'action
         (lambda (_button)
           (call-interactively #'empv-lyrics-save))
         'face custom-button
         'follow-link t))
      (insert " "))
    (insert-button
     "Search web"
     'action
     (lambda (_button)
       (browse-url (empv--lyrics-make-search-url title)))
     'face custom-button
     'follow-link t)
    (goto-char (point-min))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window (current-buffer)))))

(defun empv--lyrics-make-search-url (song)
  "Generate a search URL for the given SONG."
  (url-encode-url (format "%s%s lyrics" empv-search-prefix song)))

(defun empv--lyrics-download (song)
  "Get lyrics for SONG from web and return a pair of source url and lyrics.
Also see `empv-search-prefix'."
  (let (url lyrics)
    (ignore-error wrong-type-argument
      (thread-last
        (empv--request-raw-sync (empv--lyrics-make-search-url song))
        ;; Find all the links in the response first
        (s-match-strings-all "https\\(://\\|%3A%2F%2F\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]+[-A-Za-z0-9+&@#/%=~_|]")
        (mapcar #'car)
        (mapcar
         (lambda (it)
           (thread-last
             (string-trim-right it "&amp.*")
             (string-replace "%3A%2F%2F" "://")
             (string-replace "%2F" "/")
             (string-replace "%2D" "-")
             (string-replace "%2D" "+"))))
        ;; Then find the first sturmgeweiht|azlyrics|genius link
        ;; FIXME: Sort found URLs by their reliability first?
        (seq-find (lambda (it) (s-matches? "^https?://.*\\(sturmgeweiht.de/texte/.*titel\\|flashlyrics.com/lyrics/\\|lyrics.az/.*.html\\|azlyrics.com/lyrics/\\|genius.com\\)" it)))
        (url-unhex-string)
        (funcall (lambda (it) (setq url it) it))
        (empv--request-raw-sync)
        (string-replace "" "")
        ;; Replace newlines so that regexes can work
        (string-replace "\n" "<newline>")
        ;; FIXME: The resulting string may be too long and regexes may
        ;; fail due to stack overflow errors
        (funcall
         (lambda (it)
           (or
            ;; sturmgeweiht
            (s-match "<div class=\"inhalt\">\\(.*\\)<a href=\"" it)
            ;; azlyrics
            (s-match "Sorry about that\\. -->\\(.*\\)<!-- MxM banner -->" it)
            ;; lyrics.az
            (s-match "x-ref=\"lyric_text\">\\(.*\\)</p>" it)
            ;; flashlyrics
            (s-match "<div class=\"main-panel-content\".*?>\\(.*\\)<div class=\"sharebar-wrapper\"" it)
            ;; genius
            (s-match "data-lyrics-container=\"true\"\\(.*?\\)How to Format Lyrics" it))))
        (nth 1)
        (s-replace-all
         '(("<br>" . "\n")
           ("<br/>" . "\n")
           ("<br />" . "\n")
           ("\\n" . "\n")
           ("<div>" . "")
           ("</div>" . "")
           ("\\" . "")
           ("<newline>" . "\n")
           ("" . "\n")
           ("\"" . "")
           ("&quot;" . "\"")
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
        (replace-regexp-in-string "<[^>]*>" "")
        (setq lyrics)))
    (list url lyrics)))

(defun empv--lyrics-from-metadata (metadata)
  "Fetch lyrics from METADATA.
Tries to find a key in METADATA that contains \"lyrics\" as there
is no standard key for lyrics."
  (when-let* ((lyrics-key (seq-find
                           (lambda (key) (string-match-p "lyrics" (symbol-name key)))
                           (map-keys metadata))))
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
   (pcase-let ((`(,url ,lyrics) (empv--lyrics-download .media-title)))
     (if lyrics
         (empv--lyrics-display nil .media-title lyrics :url url)
       (empv--lyrics-on-not-found .media-title)))))

(defun empv-lyrics-save (file lyrics)
  "Save LYRICS into FILEs ID3 lyrics tag.
If you are in a `*empv-lyrics*' buffer and call this function
interactively, it will automatically update the currently shown
lyrics with the buffers content."
  (interactive
   (list (or empv--current-file (read-file-name "Audio file: "))
         (if (derived-mode-p 'empv-lyrics-display-mode)
             (buffer-string)
           (read-string "Lyrics: "))))
  (setq file (expand-file-name file))
  (unless (file-exists-p file)
    (user-error "File not found: '%s'" file))
  (let ((lyrics-file (make-temp-file "empv-lyrics" nil ".txt" lyrics)))
    (set-process-filter
     (start-process " *empv-eyeD3*" nil "eyeD3" "--encoding" "utf8" "--add-lyrics" lyrics-file file)
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
   (if-let* ((metadata-lyrics (empv--lyrics-from-metadata .metadata)))
       (empv--lyrics-display .path .media-title metadata-lyrics)
     (pcase-let ((`(,url ,web-lyrics) (empv--lyrics-download .media-title)))
       (if (not web-lyrics)
           (empv--lyrics-on-not-found .media-title)
         (empv--lyrics-display .path .media-title web-lyrics :url url)
         (when (and empv-lyrics-save-automatically (file-exists-p (expand-file-name .path)))
           (empv-lyrics-save .path web-lyrics)))))))

(defun empv-lyrics-show (song)
  "Show lyrics for SONG in a separate buffer.
This function searches the web for SONG lyrics.  If you want to
get the lyrics for currently playing/paused song, use
`empv-lyrics-current'."
  (interactive "sSong title: ")
  (pcase-let ((`(,url ,lyrics) (empv--lyrics-download song)))
    (if lyrics
        (empv--lyrics-display nil song lyrics :url url)
      (empv--lyrics-on-not-found song))))

;;;; Actions, mainly for embark but used in other places too

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

;;;; Embark integration

(defvar embark-file-map)
(defvar embark-keymap-alist)
(defvar embark-url-map)
(defvar embark-post-action-hooks)
(defvar embark-transformer-alist)
(defvar embark-general-map)

(defun empv--embark-youtube-item-transformer (type target)
  "Extract the YouTube URL from TARGET without changing it's TYPE."
  (cons type (empv--youtube-item-extract-link (get-text-property 0 'empv-item target))))

(defun empv--embark-subsonic-item-transformer (type target)
  (cons type (empv--get-text-property target :item)))

(defun empv--embark-radio-item-transformer (type target)
  "Extract the radio URL from TARGET without changing it's TYPE."
  (cons type (empv--radio-item-extract-link (get-text-property 0 'empv-item target))))

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
    "n" #'empv-enqueue-next
    "b" #'empv-bookmark-set)

  (defvar-keymap empv-subsonic-item-action-map
    :doc "Action map for Subsonic items, utilized by Embark."
    :parent embark-general-map
    "p" #'empv-subsonic-play
    "e" #'empv-subsonic-enqueue
    "n" #'empv-subsonic-enqueue-next
    "b" #'empv-subsonic-bookmark-set)

  (defvar-keymap empv-youtube-item-action-map
    :doc "Action map for YouTube items, utilized by Embark."
    :parent embark-general-map
    "RET" #'empv-play
    "p" #'empv-play
    "y" #'empv-youtube-copy-link
    "d" #'empv-youtube-download
    "e" #'empv-enqueue
    "n" #'empv-enqueue-next
    "c" #'empv-youtube-show-comments
    "C" #'empv-youtube-show-channel-videos
    "t" #'empv-youtube-become-tabulated
    "b" #'empv-bookmark-set)

  (add-to-list 'embark-keymap-alist '(empv-playlist-item . empv-playlist-item-action-map))
  (add-to-list 'embark-keymap-alist '(empv-radio-item . empv-radio-item-action-map))
  (add-to-list 'embark-keymap-alist '(empv-youtube-item . empv-youtube-item-action-map))
  (add-to-list 'embark-keymap-alist '(empv-subsonic-item . empv-subsonic-item-action-map))

  (setf (alist-get 'empv-playlist-item embark-transformer-alist) #'empv--embark-playlist-item-transformer)
  (setf (alist-get 'empv-radio-item embark-transformer-alist) #'empv--embark-radio-item-transformer)
  (setf (alist-get 'empv-youtube-item embark-transformer-alist) #'empv--embark-youtube-item-transformer)
  (setf (alist-get 'empv-subsonic-item embark-transformer-alist) #'empv--embark-subsonic-item-transformer))

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

;;;; Hydra extension

(with-eval-after-load 'hydra
  ;; See the discussion at[1] for why this is wrapped in an `eval'
  ;; call.
  ;; [1]: https://github.com/melpa/melpa/pull/9423
  (eval
   '(defhydra empv-hydra nil
      "EMPV Hydra."
      ("o" #'empv-play-or-enqueue "play or enqueue"                         :column "Play")
      ("f" #'empv-play-file "play file"                                     :column "Play")
      ("d" #'empv-play-directory "play directory"                           :column "Play")
      ("v" #'empv-play-video "play video"                                   :column "Play")
      ("a" #'empv-play-audio "play audio"                                   :column "Play")
      ("q" #'empv-exit "exit"                                               :column "Play" :exit t)
      ("Q" #'empv-save-and-exit "save and exit"                             :column "Play" :exit t)
      ("[" #'empv-playback-speed-down "playback speed down"                 :column "Playback")
      ("]" #'empv-playback-speed-up "playback speed up"                     :column "Playback")
      ("0" #'empv-volume-up "volume up"                                     :column "Playback")
      ("9" #'empv-volume-down "volume down"                                 :column "Playback")
      ("(" #'empv-chapter-prev "chapter prev"                               :column "Playback")
      (")" #'empv-chapter-next "chapter next"                               :column "Playback")
      ("x" #'empv-chapter-select "chapter select"                           :column "Playback")
      ("p" #'empv-playlist-select "playlist select"                         :column "Playlist")
      ("L" #'empv-playlist-load-from-file "playlist load"                   :column "Playlist")
      ("s" #'empv-playlist-shuffle "playlist shuffle"                       :column "Playlist")
      ("C" #'empv-playlist-clear "playlist clear"                           :column "Playlist")
      ("n" #'empv-playlist-next "playlist next"                             :column "Playlist")
      ("N" #'empv-playlist-prev "playlist prev"                             :column "Playlist")
      ("r" #'empv-play-radio "play radio"                                   :column "Remote Play")
      ("R" #'empv-play-random-channel "play random channel"                 :column "Remote Play")
      ("l" #'empv-log-current-radio-song-name "log current radio song name" :column "Remote Play")
      ("y" #'empv-youtube "youtube"                                         :column "Remote Play")
      ("Y" #'empv-youtube-last-results "youtube last results"               :column "Remote Play")
      ("t" #'empv-toggle "toggle"                                           :column "Toggle")
      ("_" #'empv-toggle-video "toggle video"                               :column "Toggle")
      ("8" #'empv-toggle-current-loop "toggle current loop"                 :column "Toggle")
      ("e" #'empv-toggle-event-display "toggle event display"               :column "Toggle")
      ("i" #'empv-display-current "display current"                         :column "Utility")
      ("c" #'empv-copy-path "copy path"                                     :column "Utility"))))

(provide 'empv)
;;; empv.el ends here
