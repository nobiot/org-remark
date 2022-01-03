;;; org-remark-global-tracking.el --- Track files with highlights & annotations -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Last modified: 03 January 2022
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, marginal-notes

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
;;  This file is part of org-remark

;;; Code:

(declare-function org-remark-mode "org-remark")

(defcustom org-remark-tracking-file
  (locate-user-emacs-file ".org-remark-tracking" nil)
  "File name where the files `org-remark' tracks is saved.
When `org-remark-global-tracking-mode' is active, opening a file
saved in `org-remark-tracking-file' automatically loads highlights."
  :group 'org-remark
  :type 'file)

(defvar org-remark-tracking-file-loaded nil)

(defvar org-remark-files-tracked nil)

;;;###autoload
(define-minor-mode org-remark-global-tracking-mode
  "Track files saved in `org-remark-tracking-file'.
When opening any of them, automatically activates `org-remark-mode'
locally for the file opened."
  :init-value nil
  :lighter " ormk-auto"
  :global t
  (cond
   (org-remark-global-tracking-mode
    ;; Activate
    (when (and (not org-remark-tracking-file-loaded)
	       (file-exists-p org-remark-tracking-file))
      (org-remark-tracking-load))
    (add-hook 'find-file-hook #'org-remark-tracking-auto-on)
    (add-hook 'kill-emacs-hook #'org-remark-tracking-save))
   (t
    ;; Deactivate
    (setq org-remark-files-tracked nil)
    (setq org-remark-tracking-file-loaded nil)
    (remove-hook 'find-file-hook #'org-remark-tracking-auto-on)
    (remove-hook 'kill-emacs-hook #'org-remark-tracking-save))))

;;;; Private Functions

(defun org-remark-tracking-auto-on ()
  "Activate `org-remark-mode' when file is being tracked.
The files being tracked are loaded on to
`org-remark-files-tracked'.  Refer to
`org-remark-tracking-load'."
  (when (and org-remark-files-tracked
	     (member (abbreviate-file-name (buffer-file-name))
		     org-remark-files-tracked))
    (unless (featurep 'org-remark) (require 'org-remark))
    (org-remark-mode +1)))

(defun org-remark-tracking-load ()
  "Load files being tracked from `org-remark-tracking-file'.
It has one filename each line.  The filename is obtrained
`abbreviated-file-names'.  This function reloads the content of
the file regardless if it is already done in this Emacs session
or not."
  (with-temp-buffer
    (condition-case nil
	(progn
	  (insert-file-contents org-remark-tracking-file)
	  (setq org-remark-files-tracked
		(split-string (buffer-string) "\n"))
          (setq org-remark-tracking-file-loaded t)))))

(defun org-remark-tracking-save ()
  "Save files being tracked in `org-remark-tracking-file'.
Files with marginal notes are tracked with variable
`org-remark-files-tracked'."
  (interactive)
  (when org-remark-files-tracked
    (with-temp-file org-remark-tracking-file
      (insert (mapconcat 'identity org-remark-files-tracked "\n")))))

(provide 'org-remark-global-tracking)

;;; org-remark-global-tracking.el ends here
