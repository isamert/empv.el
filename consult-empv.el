;;; consult-empv.el --- Consult dependent functions for empv -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamert@protonmail.com>
;; Version: 0.1
;; Homepage: https://github.com/isamert/empv
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (consult "0.5"))

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

;; Consult dependent functions for empv

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'empv)
(require 'consult)

(defun consult-empv--get-input-with-suggestions ()
  "Get an input from user, using YouTube search suggestions."
  (consult--read
   (consult-empv-yt--search-generator)
   :prompt "Search in YouTube videos: "
   :category 'empv-youtube
   :lookup (lambda (_ candidates cand)
             (or (consult--lookup-member nil candidates cand)
                 (string-trim-left cand (consult--async-split-initial ""))))
   :initial (consult--async-split-initial "")
   :sort nil
   :require-match nil))

(defun consult-empv-yt--search-generator ()
  "Generate an async search closure for TYPE and FILTER."
  (thread-first (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult-empv--async-search)
    (consult--async-throttle)
    (consult--async-split)))

(defun consult-empv--async-search (next)
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

;;;###autoload
(defun consult-empv-youtube ()
  "Search in YouTube videos with interactive suggestions using `consult' and `empv'."
  (interactive)
  (empv--youtube (consult-empv--get-input-with-suggestions) 'video))

;;;###autoload
(defun consult-empv-youtube ()
  "Search in YouTube videos with interactive suggestions using `consult' and `empv'."
  (interactive)
  (empv--youtube (consult-empv--get-input-with-suggestions) 'video))

;;;###autoload
(defun consult-empv-youtube-tabulated ()
  "Search in YouTube videos with interactive suggestions using `consult' and `empv'.
Show results in a tabulated buffers with thumbnails."
  (interactive)
  (let ((empv-youtube-use-tabulated-results t))
    (empv--youtube (consult-empv--get-input-with-suggestions) 'video)))

;;;###autoload
(defun consult-empv-youtube-multiple ()
  "Search in YouTube videos with interactive suggestions using `consult' and `empv'."
  (interactive)
  (empv--youtube-multiple (consult-empv--get-input-with-suggestions) 'video))

;;;###autoload
(defun consult-empv-youtube-playlist ()
  "Search in YouTube playlists with interactive suggestions using `consult' and `empv'."
  (interactive)
  (empv--youtube (consult-empv--get-input-with-suggestions) 'playlist))

;;;###autoload
(defun consult-empv-youtube-playlist-multiple ()
  "Search in YouTube playlists with interactive suggestions using `consult' and `empv'."
  (interactive)
  (empv--youtube-multiple (consult-empv--get-input-with-suggestions) 'playlist))

(defun empv-embark-copy-youtube-link (key)
  (let ((link (empv--youtube-process-result empv--last-candidates empv--youtube-last-type key)))
    (kill-new link)
    (empv--display-event "Youtube link copied into your kill-ring: %s" link)))

(embark-define-keymap empv-embark-youtube-result-actions
  "Actions for empv YouTube results."
  ("y" empv-embark-copy-youtube-link)
  ("a" empv-embark-enqueue-youtube))

(add-to-list 'embark-keymap-alist '(empv-youtube . empv-embark-youtube-result-actions))

(provide 'consult-empv)
;;; consult-empv.el ends here
