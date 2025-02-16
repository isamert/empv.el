;;; empv-hydra.el --- A hydra for empv - optional extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aryadev Chavali

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>, Aryadev Chavali <aryadev@aryadevchavali.com>
;; Version: 4.10.1
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

;; An extension to empv, providing a Hydra (see
;; <https://github.com/abo-abo/hydra>) based on the bindings from `empv-map'.
;; This is not provided by default when loading empv - it must be loaded if you
;; would like to use it.

;;; Code:

(require 'empv)
(require 'hydra)

(defhydra empv-hydra nil
  "EMPV Hydra:"
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
  ("c" #'empv-copy-path "copy path"                                     :column "Utility"))

(provide 'empv-hydra)
;;; empv-hydra.el ends here
