;;; org-remark-info.el --- Support Org-roam with Info-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 16 July 2023
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
;;  This file is an Org-remark extension to add highlights and
;;  annotation support for Info-mode.
;;
;;  The extension can be enabled globally with `org-remark-info-mode'
;;
;;  Highlights will include the links to the original with using
;;  `ol-info' (Org mode's info: link type). By default, it opens the
;;  node within the same window as the Org buffer, which is not
;;  convenient for the purpose of Org-remark. You can configure Emacs to
;;  open info: link into another window. See an example of such a
;;  customization below:
;;
;;    (setopt switch-to-buffer-obey-display-actions t)
;;    (add-to-list 'display-buffer-alist
;;               '("\*info\*.*"
;;                 (display-buffer-reuse-mode-window)
;;                 (mode . Info-mode)))
;;
;;  You will need to use `org-remark-info-mode' in conjunction with
;;  `org-remark-global-tracking-mode' and `org-remark-mode'. For more,
;;  refer to the following Info node:
;;
;; - Info node `(org-remark) Installation'
;; - Info node `(org-remark) Getting Started'


;;; Code:

(require 'ol-info)
(require 'info)
(require 'org-remark-global-tracking)
(defvar org-remark-prop-source-file)
(defvar org-remark-mode)
(declare-function org-remark-highlights-load "org-remark")
(declare-function org-remark-mode "org-remark")

;;;###autoload
(define-minor-mode org-remark-info-mode
  "Enable Org-remark to work with `Info-mode' for Info documentation reader."
  :global t
  :group 'org-remark-info
  (if org-remark-info-mode
      ;; Enable
      (progn
          (add-hook 'org-remark-source-find-file-name-functions
                    #'org-remark-info-get-node)
          (add-hook 'org-remark-highlight-link-to-source-functions
                    #'org-remark-info-link)
          (advice-add #'Info-find-node :after #'org-remark-info-highlights-load)
          (advice-add #'Info-search :after #'org-remark-info-highlights-load))
    ;; Disable
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'org-remark-info-get-node)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'org-remark-info-link)
    (advice-remove #'Info-find-node #'org-remark-info-highlights-load)
    (advice-remove #'Info-search #'org-remark-info-highlights-load)))

(defun org-remark-info-highlights-load (&rest _args)
  "Wrapper for `org-remark-highlights-load'.
It is necessary as this function is intended to be used as part
of advice for `Info-goto-node', which gets arguments passed to
it. `org-remark-highlights-load' should be called with no
arguments for the purpose of `org-remark-info-mode'."
  ;; Enabling `org-remark-mode' runs `org-remark-highlights-load', which
  ;; would result in duplicating the highlights. As this function should
  ;; be run only once for initial load or only once for subsequent
  ;; re-load. This `if' statement is to differentiate the initial load
  ;; when no Info node has been opened from subsequent reloads when the
  ;; user moves to another Info node. In addition, `featurep' is used
  ;; because variable `org-remark-mode' may not have been loaded yet to
  ;; avoid symbol void.
  (if (or (not (featurep 'org-remark))
          (not org-remark-mode))
      (org-remark-mode +1)
    (org-remark-highlights-load)))

(defun org-remark-info-get-node ()
  "Return the current Info file/node."
  (when (eq major-mode 'Info-mode)
    (concat (file-name-nondirectory Info-current-file)
            "/"
            Info-current-node)))

(defun org-remark-info-link (_filname _point)
  "Return \"info:\" link with current point in `Info-mode' buffer.
This function only works when the mode is `Info-mode'.
Assume the point is on the highlight in source Info document
buffer and `ol-info' is loaded. The latter is necessary for
`org-store-link' to work wiht Info buffer."
  (when (eq major-mode 'Info-mode)
    (org-store-link nil nil)))

(cl-defmethod org-remark-highlight-get-constructors (&context (major-mode Info-mode))
  "Construct lists for creating MAJOR-MODE specific hierarchy.
This method is for `Info-mode'. Return the value in a alist like
this:
   (SOURCE-FILENAME-FN TITLE-FN PROP-TO-FIND)"
  (let* ((headline-1 (list
                      ;; SOURCE-FILENAME-FN
                      ;; Don't include the full directory path for Info
                      ;; node. This may change when Emacs version or
                      ;; package version changes.
                      (lambda () (file-name-nondirectory Info-current-file))
                      ;; TITLE-FN
                      (lambda () (file-name-nondirectory Info-current-file))
                      ;; PROP-TO-FIND
                      "org-remark-info-file"))
         (headline-2 (list
                      ;; SOURCE-FILENAME-FN
                      #'org-remark-info-get-node
                      ;; TITLE-FN
                      (lambda () Info-current-node)
                      ;; PROP-TO-FIND
                      org-remark-prop-source-file))
         (headline-constructors (list headline-1 headline-2)))
    headline-constructors))

(provide 'org-remark-info)
;;; org-remark-info.el ends here
