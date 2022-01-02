;;; org-hana-global-tracking.el --- Track files with highlights & annotations -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-hana
;; Last modified: 02 January 2022
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
;;  This file is part of org-hana

;;; Code:

(declare-function org-hana-mode "org-hana")

(defcustom org-hana-tracking-file
  (locate-user-emacs-file ".org-hana-tracking" nil)
  "File name where the files `org-hana' tracks is saved.
When `org-hana-global-tracking-mode' is active, opening a file
saved in `org-hana-tracking-file' automatically loads highlights."
  :group 'org-hana
  :type 'file)

(defvar org-hana-tracking-file-loaded nil)

(defvar org-hana-files-tracked nil)

;;;###autoload
(define-minor-mode org-hana-global-tracking-mode
  "Track files saved in `org-hana-tracking-file'.
When opening any of them, automatically activates `org-hana-mode'
locally for the file opened."
  :init-value nil
  :group 'org-hana
  :lighter " ❦-tracking"
  :global t
  (cond
   (org-hana-global-tracking-mode
    ;; Activate
    (when (and (not org-hana-tracking-file-loaded)
	       (file-exists-p org-hana-tracking-file))
      (org-hana-tracking-load))
    (add-hook 'find-file-hook #'org-hana-tracking-auto-on)
    (add-hook 'kill-emacs-hook #'org-hana-tracking-save))
   (t
    ;; Deactivate
    (setq org-hana-files-tracked nil)
    (setq org-hana-tracking-file-loaded nil)
    (remove-hook 'find-file-hook #'org-hana-tracking-auto-on)
    (remove-hook 'kill-emacs-hook #'org-hana-tracking-save))))

;;;; Private Functions

(defun org-hana-tracking-auto-on ()
  "Activate `org-hana-mode' when file is being tracked.
The files being tracked are loaded on to
`org-hana-files-tracked'.  Refer to
`org-hana-tracking-load'."
  (when (and org-hana-files-tracked
	     (member (abbreviate-file-name (buffer-file-name))
		     org-hana-files-tracked))
    (unless (featurep 'org-hana) (require 'org-hana))
    (org-hana-mode +1)))

(defun org-hana-tracking-load ()
  "Load files being tracked from `org-hana-tracking-file'.
It has one filename each line.  The filename is obtrained
`abbreviated-file-names'.  This function reloads the content of
the file regardless if it is already done in this Emacs session
or not."
  (with-temp-buffer
    (condition-case nil
	(progn
	  (insert-file-contents org-hana-tracking-file)
	  (setq org-hana-files-tracked
		(split-string (buffer-string) "\n"))
          (setq org-hana-tracking-file-loaded t)))))

(defun org-hana-tracking-save ()
  "Save files being tracked in `org-hana-tracking-file'.
Files with marginal notes are tracked with variable
`org-hana-files-tracked'."
  (interactive)
  (when org-hana-files-tracked
    (with-temp-file org-hana-tracking-file
      (insert (mapconcat 'identity org-hana-files-tracked "\n")))))

(provide 'org-hana-global-tracking)

;;; org-hana-global-tracking.el ends here
