;;; org-remark-global-tracking.el --- Track files and auto-activate Org-remark -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Last modified: 16 January 2022
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, marginal notes

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
  (abbreviate-file-name
   (expand-file-name ".org-remark-tracking" user-emacs-directory))
  "File name where the files `org-remark' tracks is saved.
When `org-remark-global-tracking-mode' is active, opening a file
saved in `org-remark-tracking-file' automatically loads highlights."
  :group 'org-remark
  :type 'file)

(defvar org-remark-files-tracked nil
  "List of files being tracked by `org-remark-global-tracking-mode'.")

;;;###autoload
(define-minor-mode org-remark-global-tracking-mode
  "Track files saved in `org-remark-tracking-file'.
When opening any of them, automatically activates `org-remark-mode'
locally for the file opened."
  :init-value nil
  :lighter " ormk-auto"
  :global t
  :group 'org-remark
  (cond
   (org-remark-global-tracking-mode
    ;; Activate
    ;; Prioritise the new `org-remark-tracking-file' over the legacy one
    (when-let (tracking-file (or (when (file-exists-p
                                        org-remark-tracking-file)
                                   org-remark-tracking-file)
                                 (when (file-exists-p
                                        (org-remark-legacy-tracking-file-get))
                                   (org-remark-legacy-tracking-file-get))))
      (org-remark-tracking-load tracking-file))
    ;; `org-remark-tracking-save' should be added to kill hook even when no
    ;; tracking file existed before -- this would indicate first time use of
    ;; tracking; the files tracked in the memory needs to persist in the file.
    (add-hook 'find-file-hook #'org-remark-tracking-auto-on)
    (add-hook 'kill-emacs-hook #'org-remark-tracking-save))
   (t
    ;; Deactivate
    (setq org-remark-files-tracked nil)
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

(defun org-remark-tracking-load (tracking-file)
  "Load files being tracked from TRACKING-FILE.
It has one filename each line.  The filename is obtrained with
`abbreviated-file-names'.  This function reloads the content of
the file regardless if it is already done in this Emacs session
or not."
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents tracking-file)
          (setq org-remark-files-tracked
                (split-string (buffer-string) "\n"))))))

(defun org-remark-tracking-save ()
  "Save files being tracked in `org-remark-tracking-file'.
Files with marginal notes are tracked with variable
`org-remark-files-tracked'."
  (interactive)
  (when org-remark-files-tracked
    ;; Save to the new Org-remark tracking file. No need to keep the old file any
    ;; longer, ignore the legacy file path.
    (with-temp-file org-remark-tracking-file
      (insert (mapconcat 'identity org-remark-files-tracked "\n")))))

(defun org-remark-legacy-tracking-file-get ()
  "Return the path to the legacy tracking file.
This function is used to automate conversion from the legacy
Org-marginalia to the new Org-remark.  For this purpose, this
function assumes the user has not customised the default tracking
file name \".org-marginalia-tracking\" placed their
`user-emacs-directory'.  If personalized, it is reasonable to
expect the user is able to to also customize
`org-remark-tracking-file'."
  (abbreviate-file-name (expand-file-name
                         ".org-marginalia-tracking"
                         user-emacs-directory)))

(provide 'org-remark-global-tracking)

;;; org-remark-global-tracking.el ends here
