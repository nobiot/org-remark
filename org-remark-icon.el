;;; org-remark-icon.el --- Enable Org-roam to use icons -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 29 July 2023
;; Last modified: 23 March 2024
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

;;; Code:
(require 'cl-macs)
;; Silence compiler
(defvar org-remark-default-feature-modes)

(defgroup org-remark-icon nil
  "Enable `org-remark' to display glyph/icon indicators."
  :group 'org-remark
  :prefix "org-remark-icon"
  :link '(url-link :tag "GitHub" "https://github.com/nobiot/org-remark"))

(defcustom org-remark-icon-notes "(*)"
  "String to be displayed when notes exist for a given highlight.

You can set a function to this user option. In this case, the
function must take one argument, which is FACE. FACE can be a
named face (a symbol), or an anonymous face (plist of face
attributes). The function can ignore them and set its own face
and/or text-property to the string. This means you can return a
string with a display property to show an SVG icon instead of the
underlying string.

Nil means no icon is to be displayed.

If you wants to use image icons (e.g. SVG image icon created with
package `icons', available Emacs 29.1 or higher), you're limited
to a single character with no space before and after the
character. This limitation does not apply to string of characters
without images, but it is generally assumed that the the value
set to this customizing variable will be a short string (e.g 3
characters long with a pair of parentheses before and after a
single character, such as the default value.)"
  :safe #'stringp
  :type '(choice
          (string "(*)")
          (function)))

(defcustom org-remark-icon-position-adjusted "(d)"
  "String to be displayed when a highlight position adjusted.

You can set a function to this user option. In this case, the
function must take one argument, which is FACE. FACE can be a
named face (a symbol), or an anonymous face (plist of face
attributes). The function can ignore them and set its own face
and/or text-property to the string. This means you can return a
string with a display property to show an SVG icon instead of the
underlying string.

Nil means no icon is to be displayed.

If you wants to use image icons (e.g. SVG image icon created with
package `icons', available Emacs 29.1 or higher), you're limited
to a single character with no space before and after the
character. This limitation does not apply to string of characters
without images, but it is generally assumed that the the value
set to this customizing variable will be a short string (e.g 3
characters long with a pair of parentheses before and after a
single character, such as the default value."
  :safe #'stringp
  :type '(choice
          (string "(d)")
          (function)))

;; Register a mode for automatic enablement at the same time as
;; `org-remark-mode'.
(add-to-list 'org-remark-default-feature-modes #'org-remark-icon-mode)

;;;###autoload
(define-minor-mode org-remark-icon-mode
  "Enable Org-remark to display icons.
The icons currently available are defined in `org-remark-icons'."
  :global nil
  :group 'org-remark
  (if org-remark-icon-mode
      ;; Enable
      (progn
        (add-hook 'org-remark-highlights-toggle-hide-functions
                  #'org-remark-icon-toggle-hide nil :local)
        (add-hook 'org-remark-highlights-toggle-show-functions
                  #'org-remark-icon-toggle-show :local)
        ;; Add-icons should be the last function because other functions may do
        ;; something relevant for an icon -- e.g. adjust-positon."
        (add-hook 'org-remark-highlights-after-load-functions
                  #'org-remark-highlights-add-icons-maybe 80 :local))
    ;; Disable
    (remove-hook 'org-remark-highlights-toggle-hide-functions
                 #'org-remark-icon-toggle-hide :local)
    (remove-hook 'org-remark-highlights-toggle-show-functions
                 #'org-remark-icon-toggle-show :local)
    (remove-hook 'org-remark-highlights-after-load-functions
                 #'org-remark-highlights-add-icons-maybe :local)))

(defvar org-remark-icons
  (list
   (list 'notes
         #'org-remark-icon-notes-p
         nil)
   (list 'position-adjusted
         #'org-remark-icon-position-adjusted-p
         'org-remark-highlighter-warning))
  "List of icons enabled.
It is an alist. Each element is a list of this form:
 (ICON-NAME PREDICATE DEFAULT-FACE)

ICON-NAME must be a symbol such as \\='notes\\=' and
\\='position-adjusted\\='. They are used as a suffix to be added to
\\='org-remark-icon-\\=' to form a customizing variable such as
`org-remark-icon-notes' and `org-remark-icon-position-adjusted'.

PREDICATE must be a function that accepts one argument OV, which
is the highlight overlay. If PREDICATE returns non-nil, the icon
for ICON-NAME will be added to the highlight.

DEFAULT FACE must be a named face. It is optinal and can be nil.")

(defun org-remark-icon-notes-p (ov)
  (and org-remark-icon-notes
       (overlay-get ov '*org-remark-note-body)))

(defun org-remark-icon-position-adjusted-p (ov)
  (and org-remark-icon-position-adjusted
       (overlay-get ov '*org-remark-position-adjusted)))

(defun org-remark-icon-toggle-hide (highlight)
  (overlay-put highlight '*org-remark-icons (overlay-get highlight 'after-string))
  (overlay-put highlight 'after-string nil))

(defun org-remark-icon-toggle-show (highlight)
  (overlay-put highlight 'after-string (overlay-get highlight '*org-remark-icons))
  (overlay-put highlight '*org-remark-icons nil))

(defun org-remark-highlights-add-icons-maybe (overlays _notes-buf)
  "Add icons to OVERLAYS.
Each overlay is a highlight."
  (dolist (ov overlays)
    ;; icons added to line highlighters differently from normal ones.
    (cl-flet ((add-icon-maybe (icon)
                (cl-destructuring-bind
                    (icon-name pred default-face) icon
                  (when (funcall pred ov)
                    (org-remark-icon-propertize icon-name ov default-face)))))
      (let ((icon-string
             ;; The third arg of `mapconcat' is not optional in Emacs 28 or lower.
             (mapconcat #'add-icon-maybe org-remark-icons nil)))
        ;; `mapconcat' returns "" when all function calls for SEQUENCE
        ;; return nil, I guess to guarantee the result is a string
        (when (and icon-string
                   (not (string= icon-string "")))
          (org-remark-icon-overlay-put
           ov icon-string
           (overlay-get ov 'org-remark-type)))))))

(cl-defgeneric org-remark-icon-overlay-put (_ov _icon-string _org-remark-type)
  "Default method to deal with icon.
This is used when a method specific \\='org-remark-type\\=' not
implemented."
  (ignore))

(cl-defmethod org-remark-icon-overlay-put (ov icon-string (_org-remark-type (eql nil)))
  (overlay-put ov 'after-string icon-string))

(defun org-remark-icon-propertize (icon-name highlight default-face)
  "Return a propertized string.

ICON-NAME is a symbol such as \\='notes\\=' and
\\='position-adjusted\\='. They are used as a suffix to be added
to \\='org-remark-icon-\\=' to form an ICON, which is a
customizing variable such as `org-remark-icon-notes' and
`org-remark-icon-position-adjusted'.

HIGHLIGHT is the current highlight overlay being worked on. It is
useful to obtain its face to add the matching face to the
icon (HIGHLIGHT-FACE).

DEFAULT-FACE is the default face for the ICON. It can be nil, in
which case the face of the HIGHLIGHT should be used. This
depends on the value of ICON.

ICON is a customizing variable, it can be set to a string. In
this case, the DEFAULT-FACE is used when available; if not,
HIGHLIGHT-FACE. ICON can also be a function. In this case, three
arguments are pass to it: ICON-NAME, HIGHLIGHT-FACE, and
DEFAULT-FACE. It is up to the function whether or not to use any
of them. All it needs to do is to return a string that represents
an icon, typically propertized with a face."
  (let ((icon (symbol-value (intern (concat "org-remark-icon-"
                                            (symbol-name icon-name)))))
        (highlight-face (org-remark-icon-highlight-get-face
                         highlight
                         (overlay-get highlight 'org-remark-type)))
        (default-face default-face))
    (if (functionp icon)
        (funcall icon icon-name highlight-face default-face)
      (propertize icon 'face (if default-face default-face highlight-face)))))

(cl-defgeneric org-remark-icon-highlight-get-face (highlight _org-remark-type)
  "Return the face of the HIGHLIGHT overlay.
This is default method for range-highlights."
  (overlay-get highlight 'face))


(provide 'org-remark-icon)
;;; org-remark-icon.el ends here
