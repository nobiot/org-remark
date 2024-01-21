;;; org-remark-global-tracking.el --- Track files and auto-activate Org-remark -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 15 August 2021
;; Last modified: 2024-01-21T202245
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
suffix to the file name without the extension. When the buffer is
not visiting a file (e.g. a website via EWW), the file name will
be \"marginalia.org\" in your `user-emacs-directory'."
  :group 'org-remark
  :safe #'stringp
  :type '(choice
          (file "marginalia.org")
          (function org-remark-notes-file-name-function)))

(defcustom org-remark-highlight-link-to-source-functions nil
  "Abnormal hook called to create a link to source in notes file.
Each one is called with FILENAME as an argument."
  :group 'org-remark
  :type '(repeat function))

(defvar org-remark-source-find-file-name-functions nil
  "List of functions to get the source file name.
It is an abnormal hook run with no argument and each function
must return a file-name-equvalent as a string that uniquely
identifies the source.  The hook is run when `buffer-file-name`
in source buffer returns nil, meaning the source buffer is not
visiting a file.

Meant to be set by extensions such as `org-remark-eww'")

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
  (if org-remark-global-tracking-mode
    ;; Enable
      (add-hook 'find-file-hook #'org-remark-auto-on)
    ;; Disable
    (remove-hook 'find-file-hook #'org-remark-auto-on)))

;;; Functions
(defun org-remark-notes-file-name-function ()
  "Return a marginal notes file name for the current buffer.

This is the default function for the customizing variable
`org-remark-notes-file-name' for its function option.

When the current buffer is visiting a file, the name of marginal
notes file will be \"FILE-notes.org\", adding \"-notes.org\" as a
suffix to the file name without the extension.

If the current buffer is not visiting a file, the file name will
be marginalia.org in your `user-emacs-directory'.  If this file
name is not suitable, either override the function or set the
user option to use your own custom function."
  (if buffer-file-name
      (let ((source-filename (org-remark-source-find-file-name)))
        (when (and (stringp source-filename)
                   (file-exists-p source-filename))
          (concat (file-name-sans-extension
                   (file-name-nondirectory source-filename))
                  "-notes.org")))
    ;; If buffer is not visiting a file, we use the default file name.
    ;; If this file name is not suitable, either override the function
    ;; or set the user option to a custom function.
    (expand-file-name "marginalia.org" user-emacs-directory)))

(defalias
  'org-remark-notes-file-path-function
  'org-remark-notes-file-name-function)

(make-obsolete
 'org-remark-notes-file-path-function
 'org-remark-notes-file-name-function "0.2.0" )

;;;; Private Functions

(defun org-remark-auto-on ()
  "Automatically activates `org-remark-mode' for current buffer when relevant.
This function is meant to be added to `find-file-hook' by
`org-remark-global-tracking-mode'."
  (when-let (notes-file (org-remark-notes-get-file-name))
    (when (file-readable-p notes-file)
      (unless (featurep 'org-remark) (require 'org-remark))
      (org-remark-mode +1))))

(cl-defgeneric org-remark-notes-get-file-name ()
  "Return the name of marginal notes file for current buffer.")

(cl-defmethod org-remark-notes-get-file-name ()
  "Return the name of marginal notes file for current buffer.
This method is major modes derived from `text-mode'."
  (if (functionp org-remark-notes-file-name)
      (funcall org-remark-notes-file-name)
    (if buffer-file-name org-remark-notes-file-name
      (expand-file-name org-remark-notes-file-name user-emacs-directory))))

(defun org-remark-source-find-file-name ()
  "Return the filename for the source buffer.
We use this filename to identify the source buffer in all
operations related to marginal notes.
Assumes that we are currently in the source buffer."
  (let ((filename (or (and (not (derived-mode-p 'special-mode))
                           buffer-file-name)
                      (run-hook-with-args-until-success
                       'org-remark-source-find-file-name-functions))))
    filename))

(provide 'org-remark-global-tracking)

;;; org-remark-global-tracking.el ends here
