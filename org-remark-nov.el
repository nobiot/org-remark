;;; org-remark-nov.el --- Compatibility with nov-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 9 January 2023
;; Last modified: 21 January 2024
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, note-taking, marginal-notes, wp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  This feature would not have been possible without the collaboration
;;  with Tan Yin Hoe (GitHub user sati-bodhi).
;;
;;  This file is an Org-remark extension to add highlights and
;;  annotation support for EPUB books rendered by nov.el [1]. Org-remark
;;  does not automatically install nov.el. Users are expected to obtain
;;  it separately, for example, via MELPA.
;;
;;  The extension can be enabled globally with `org-remark-nov-mode'.
;;  You will need to use it in conjunction with
;;  `org-remark-global-tracking-mode' and `org-remark-mode'. For more,
;;  refer to the following Info node:
;;
;; - Info node `(org-remark) Installation'
;; - Info node `(org-remark) Getting Started'
;;
;;  [1]: <https://depp.brause.cc/nov.el/>

;;; Code:

(if (locate-library "nov") (require 'nov)
  (error "Org-remark: package `nov' is missing"))
(require 'org-remark-global-tracking)
(declare-function org-remark-highlights-load "org-remark")
(declare-function org-store-link "ol")
(defvar org-remark-prop-source-file)
;; To silence flymake
(defvar nov-file-name)
(defvar nov-documents)
(defvar nov-documents-index)
(defvar nov-metadata)

;;;###autoload
(define-minor-mode org-remark-nov-mode
  "Enable Org-remark to work with `nov-mode' for eub."
  :global t
  :group 'org-remark-nov
  (if org-remark-nov-mode
      ;; Enable
      (progn
        (add-hook 'org-remark-source-find-file-name-functions
                  #'org-remark-nov-get-epub-source)
        (add-hook 'org-remark-highlight-link-to-source-functions
                  #'org-remark-nov-link)
        ;; When users turn the page (document in nov-mode's terminology)
        ;; `nov-mode' will erase the current buffer and render the new
        ;; document content in the same buffer. This means the
        ;; highlights currently displayed get removed; the ones for the
        ;; new document need to be loaded document after `nov-mode'
        ;; renders the new document.
        (add-hook 'nov-post-html-render-hook #'org-remark-highlights-load))
    ;; Disable
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'org-remark-nov-get-epub-source)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'org-remark-nov-link)
    (remove-hook 'nov-post-html-render-hook #'org-remark-highlights-load)))

(cl-defmethod org-remark-notes-get-file-name (&context (major-mode nov-mode))
  "Return the name of marginal notes file for current buffer.
This method is for `nov-mode' MAJOR-MODE."
  (let ((filename
         (cond (;; if `org-remark-notes-file-name' is a user's custom function, use it as is.
                (and (functionp org-remark-notes-file-name)
                     (not (eq org-remark-notes-file-name #'org-remark-notes-file-name-function)))
                (funcall org-remark-notes-file-name))
               ;; if it is a default function, then do something different for nov.el.
               ((functionp org-remark-notes-file-name)
                (concat (file-name-sans-extension nov-file-name)
                  "-notes.org"))
               ;; the only other case is a string. Assume the default
               ;; marginalia.org but it should be the same for custom
               ;; string.
               (t
                org-remark-notes-file-name))))
    ;; Even if `org-remark-notes-file-name' is a user-defined custom
    ;; function, check if it is relative. If so, make it absolute by
    ;; adding the directory path to the epub file. The purpose is to
    ;; avoid the directory to be the temp file that nov.el creates for
    ;; the html file to be rendered.
    (unless (file-name-absolute-p filename)
      ;; major mode is nov and in the document buffer.
      (setq filename (expand-file-name filename (file-name-directory nov-file-name))))
    filename))

(defun org-remark-nov-get-epub-source ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (concat
     (file-name-nondirectory nov-file-name)
     "/"
     (file-name-base (cdr (aref nov-documents nov-documents-index))))))

(defun org-remark-nov-get-epub-document-title ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (let ((temp-filename (cdr (aref nov-documents nov-documents-index))))
      (file-name-base temp-filename))))

(defun org-remark-nov-link (_filname _point)
  "Return \"nov:\" link with current point in `nov-mode' buffer.

This function only works when the mode is `nov-mode'.

Assume the point is on the highlight in source epub document
buffer."
  (when (eq major-mode 'nov-mode)
    (org-store-link nil)))

(cl-defmethod org-remark-highlight-get-constructors (&context (major-mode nov-mode))
  "Construct lists for creating MAJOR-MODE specific hierarchy.
Return the value in a alist like this:

   (SOURCE-FILENAME-FN TITLE-FN PROP-TO-FIND)"
  (let* ((headline-1 (list
                      ;; SOURCE-FILENAME-FN
                      (lambda () nov-file-name)
                      ;; TITLE-FN
                      (lambda () (cdr (assoc 'title nov-metadata)))
                      ;; PROP-TO-FIND
                      "org-remark-nov-file"))
         (headline-2 (list
                      ;; SOURCE-FILENAME-FN
                      #'org-remark-nov-get-epub-source
                      ;; TITLE-FN
                      #'org-remark-nov-get-epub-document-title
                      ;; PROP-TO-FIND
                      org-remark-prop-source-file))
         (headline-constructors (list headline-1 headline-2)))
    headline-constructors))

(provide 'org-remark-nov)
;;; org-remark-nov.el ends here
