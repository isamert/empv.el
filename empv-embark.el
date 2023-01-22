;;; empv-embark.el --- Embark extensions for empv.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 1.0.0
;; Homepage: https://github.com/isamert/empv.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1"))

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

;; Embark extensions for empv.el.

;; Simply call `empv-embark-initialize' function and you are
;; set. Other functions are provided for more granular control and not
;; needed most of the time.

;;; Code:

(require 'embark)
(require 'empv)

(defun empv-embark-initialize ()
  "Initialize embark support for empv.
There are several other `empv-embark-initialize-*' functions,
this function simply calls all of them.  If you want more control
over how this package manipulates embark actions, use the
functions called by this function."
  (empv-embark-initialize-radio-actions)
  (empv-embark-initialize-playlist-actions)
  (empv-embark-initialize-youtube-actions)
  (empv-embark-initialize-file-actions))

(defun empv-embark-initialize-playlist-actions ()
  "Initialize embark actions for empv playlist items."
  (embark-define-keymap empv-embark-playlist-item-actions
    "Actions for playlist items."
    ("p" empv-playlist-play)
    ("y" empv-playlist-copy-path)
    ("m" empv-playlist-move)
    ("r" empv-playlist-remove)
    ("R" empv-playlist-remove-others))
  (add-to-list 'embark-keymap-alist '(empv-playlist-item . empv-embark-playlist-item-actions))
  (setf (alist-get 'empv-playlist-item embark-transformer-alist) #'empv--embark-playlist-item-transformer))

(defun empv-embark-initialize-radio-actions ()
  "Initialize embark actions for empv radio items."
  (embark-define-keymap empv-embark-radio-item-actions
    "Actions for radio channels."
    ("e" empv-enqueue)
    ("n" empv-enqueue-next)
    ("p" empv-play))
  (add-to-list 'embark-keymap-alist '(empv-radio-item . empv-embark-radio-item-actions))
  (setf (alist-get 'empv-radio-item embark-transformer-alist) #'empv--embark-radio-item-transformer))

(defun empv-embark-initialize-youtube-actions ()
  "Initialize embark actions for empv YouTube items."
  (embark-define-keymap empv-embark-youtube-item-actions
    "Actions for YouTube results."
    ("y" empv-youtube-copy-link)
    ("e" empv-enqueue)
    ("n" empv-enqueue-next)
    ("p" empv-play)
    ("c" empv-youtube-show-comments))
  (add-to-list 'embark-keymap-alist '(empv-youtube-item . empv-embark-youtube-item-actions))
  (setf (alist-get 'empv-youtube-item embark-transformer-alist) #'empv--embark-youtube-item-transformer))

(defun empv-embark-initialize-file-actions ()
  "Add empv actions like play, enqueue etc. to embark file actions.
This might override your changes to `embark-file-map' if there is
any.  It also overrides a few default embark actions.  Check out
this functions definition to learn more."
  (define-key embark-file-map "p" 'empv-play)
  (define-key embark-file-map "n" 'empv-enqueue-next)
  (define-key embark-file-map "e" 'empv-enqueue) ;; overrides eww-open-file
  (define-key embark-url-map "p" 'empv-play)
  (define-key embark-url-map "e" 'empv-enqueue-next) ;; overrides eww
  (define-key embark-url-map "n" 'empv-enqueue))

(provide 'empv-embark)
;;; empv-embark.el ends here
