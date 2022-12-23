;;; org-remark-eww.el --- Enable Org-remark for EWW -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Vedang Manerikar <ved.manerikar@gmail.com>
;;         Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 23 December 2022
;; Last modified: 23 December 2022
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, note-taking, marginal-notes, wp

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

;;; Commentary:
;;  This file is part of org-remark.

;;  This feature to support for EWW was originally added by Vedang
;;  Manerika with commit 5e4b27ar feat: Support taking annotations in eww
;;  buffers.  nobit has refactored it as a separate file.

;;; Code:

(require 'eww)
;; Silence compiler
(defvar org-remark-global-tracking-mode)

(defun org-remark-eww-global-tracking-mode ()
  (if org-remark-global-tracking-mode
      (add-hook 'eww-after-render-hook #'org-remark-auto-on)
    (remove-hook 'eww-after-render-hook #'org-remark-auto-on))
  (add-to-list 'org-remark-source-find-file-name-functions
               #'org-remark-eww-find-file-name))

(defun org-remark-eww-find-file-name ()
  (if (eq major-mode 'eww-mode)
      (let ((url-parsed (url-generic-parse-url (eww-current-url))))
        (concat (url-host url-parsed) (url-filename url-parsed)))))

(provide 'org-remark-eww)
;;; org-remark-eww.el ends here
