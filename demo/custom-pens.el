;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>

;; This file is not part of GNU Emacs.

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


(org-remark-create "memorize"
                   '(:foreground "white" :underline "black")
                   '(CATEGORY "exam"))

(org-remark-create "magenta"
                   '(modus-themes-nuanced-magenta))

(org-remark-create "typo"
                   '(:underline (:color "#8f0075" :style wave))
                   '(help-echo "Fix the typo"))
