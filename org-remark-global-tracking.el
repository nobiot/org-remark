;;; org-remark-global-tracking.el --- Track files and auto-activate Org-remark -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 15 August 2021
;; Last modified: 13 February 2022
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

(defvaralias 'org-remark-notes-file-path 'org-remark-notes-file-name)

(make-obsolete-variable
 'org-remark-notes-file-path 'org-remark-notes-file-name "0.2.0")

(defcustom org-remark-notes-file-name "marginalia.org"
  "Name of the file where we store highlights and marginal notes.
It can be either a string or function.

If it is a string, it should be a file name to the marginal notes
file.  The default is \"marginalia.org\".  The default will
result in one marginal notes file per directory.  Ensure that it
is an Org file.

If it is a function, the default function is
`org-remark-notes-file-name-function'.  It returns a file name
like this: \"FILE-notes.org\" by adding \"-notes.org\" as a
suffix to the file name without the extension."
  :group 'org-remark
  :safe #'stringp
  :type '(choice
          (file "marginalia.org")
          (function org-remark-notes-file-name-function)))

;;;###autoload
(define-minor-mode org-remark-global-tracking-mode
  "Automatically activates local minor mode `org-remark-mode'.
When this global minor mode is active, a function added to
`find-file-hook' will look for a marginal notes file for the file
as defined by `org-remark-notes-file-path'.  If it is found and
readable, the function automatically activates `org-remark'."
  :init-value nil
  :lighter " ormk-auto"
  :global t
  :group 'org-remark
  (cond
   (org-remark-global-tracking-mode
    ;; Activate
    (add-hook 'find-file-hook #'org-remark-auto-on))
   (t
    ;; Deactivate
    (remove-hook 'find-file-hook #'org-remark-auto-on))))

(defun org-remark-notes-file-name-function ()
  "Return a marginal notes file name for the current buffer.

This is the default function for the customizing variable
`org-remark-notes-file-name' for its function option.

When the current buffer is visiting a file, the name of marginal
notes file will be \"FILE-notes.org\", adding \"-notes.org\" as a
suffix to the file name without the extension."
  (concat (file-name-sans-extension (buffer-file-name)) "-notes.org"))

(defalias
  'org-remark-notes-file-path-function 'org-remark-notes-file-name-function)

(make-obsolete
 'org-remark-notes-file-path-function 'org-remark-notes-file-name-function "0.2.0" )

;;;; Private Functions

(defun org-remark-auto-on ()
  "Automatically activates `org-remark-mode' for current buffer when relevant.
This function is meant to be added to `find-file-hook' by
`org-remark-global-tracking-mode'."
  (when-let (notes-file (org-remark-notes-get-file-name))
    (when (file-readable-p notes-file)
      (unless (featurep 'org-remark) (require 'org-remark))
      (org-remark-mode +1))))

(defun org-remark-notes-get-file-name ()
  "Return the name of marginal notes file for current buffer."
  (if (functionp org-remark-notes-file-name)
      (funcall org-remark-notes-file-name)
    ;; If not function, assume string and return it as the file path.
    org-remark-notes-file-name))

(provide 'org-remark-global-tracking)

;;; org-remark-global-tracking.el ends here
