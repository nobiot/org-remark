;;; org-remark-line.el --- Enable Org-roam to highlight a line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 01 August 2023
;; Last modified: 10 August 2023
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

(require 'org-remark)

(defface org-remark-line-highlighter
  '((((class color) (min-colors 88) (background light))
     :foreground "#dbba3f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e2d980")
    (t
     :inherit highlight))
  "Face for the default line highlighter pen.")

(defvar org-remark-line-icon "*")

(defvar org-remark-line-heading-title-max-length 40)

(defvar org-remark-line-ellipsis "…")

(defvar-local org-remark-line-delayed-put-overlay-functions '()
  "List of lambda functions that add a highlight.
We need to delay adding highlight overlays until window is
available for the current buffer. When the buffer visits a
filefor the first time, the window has not been created before
`org-remark-highlight-mark-overlay' is called.")

;;;###autoload
(define-minor-mode org-remark-line-mode
  "Enable Org-remark to highlight and annotate the whole line."
  :global nil
  :group 'org-remark
  (if org-remark-line-mode
      ;; Enable
      (progn
        ;; Depth is deeper than the default one for range highlight.
        ;; This is to prioritize it over line-highlight when the fomer
        ;; is at point and yet on the same line of another
        ;; line-highlight.
        (add-hook 'org-remark-find-dwim-functions
                  #'org-remark-line-find 80 :local)
        (add-hook 'window-size-change-functions
                 #'org-remark-line-set-window-margins nil :local)
        ;;(org-remark-line-set-buffer-windows))
        )
    (remove-hook 'org-remark-find-dwim-functions #'org-remark-line-find :local)
    (remove-hook 'window-size-change-functions
                 #'org-remark-line-set-window-margins :local)))

(defun org-remark-line-set-buffer-windows ()
  "
Adapted from Olivetti mode"
  (mapc #'org-remark-line-set-window-margins
        (get-buffer-window-list nil nil 'visible)))

(defun org-remark-line-set-window-margins (window-or-frame)
  "Set the margins of current window that displays current buffer.
Return a cons of the form (LEFT-WIDTH . RIGHT-WIDTH). If a
marginal area does not exist, its width will be returned as nil."
  (when (and (windowp window-or-frame) org-remark-line-mode)
    ;;(message "size change used with a window argument")
    (when org-remark-line-delayed-put-overlay-functions
      (dolist (fn (reverse org-remark-line-delayed-put-overlay-functions))
        (funcall fn))
      (setq org-remark-line-delayed-put-overlay-functions nil))
    (cl-destructuring-bind (left-width . right-width) (window-margins)
      (when (or (eq left-width nil) (< left-width 3))
        (setq left-margin-width 3)
        (set-window-buffer (get-buffer-window) (current-buffer) 'keep-margins)
        (set-window-margins nil 3))
      (window-margins))))

(defun org-remark-line-pos-bol (pos)
  "Return the beginning of the line position for POS."
  (save-excursion
    (goto-char pos)
    (pos-bol)))

(defun org-remark-line-highlight-p (highlight)
  "Return t if HIGHLIGHT is one for the line.
HIGHLIGHT is an overlay."
  (eql 'line (overlay-get highlight 'org-remark-type)))

(defun org-remark-line-find (&optional point)
  "Return the line-highight (overlay) of the current line.
When POINT is passed, one for the line it belongs to. If there
are multiple line-hilights, return the car of the list returned
by `overlays-in'."
  (let* ((point (or point (point)))
         (bol (org-remark-line-pos-bol point))
         (highlights (overlays-in bol bol)))
    (seq-find #'org-remark-line-highlight-p highlights)))

(org-remark-create "line"
                   'org-remark-line-highlighter
                   '(org-remark-type line))

(cl-defmethod org-remark-beg-end ((org-remark-type (eql 'line)))
    (let ((bol (org-remark-line-pos-bol (point))))
      (list bol bol)))

(cl-defmethod org-remark-highlight-mark-overlay (ov face (org-remark-type (eql 'line)))
  "Put FACE and other necessary properties to the highlight OV.
This is a method for highlights of ORG-REMARK-TYPE \\='line\\='."
  (if (get-buffer-window)
      ;; When revert-buffer is called, the window is already available
      ;; but window size won't change.
      (org-remark-line-highlight-overlay-put ov face)
    (push (lambda ()
            (org-remark-line-highlight-overlay-put ov face))
        org-remark-line-delayed-put-overlay-functions)))

(defun org-remark-line-highlight-overlay-put (ov face &optional string)
  (let* ((face (or face 'org-remark-line-highlighter))
         ;;(left-margin (car (org-remark-line-set-window-margins
         ;;                   (get-buffer-window))))
         ;;(left-margin 3)
         ;;(spaces (- left-margin 1))
         (string (or string
                     (with-temp-buffer ;;(insert-char ?\s spaces)
                                       (insert org-remark-line-icon)
                                       (buffer-string)))))
    (overlay-put ov 'before-string (propertize "! " 'display
                                               `((margin left-margin)
                                                 ,(propertize string 'face face))))
    (overlay-put ov 'insert-in-front-hooks (list 'org-remark-line-highlight-modified))
    ov))

(defun org-remark-line-highlight-modified (ov after-p beg end &optional length)
  "This is good! Move the overlay to follow the point when ENTER in the line."
  (when after-p
    (save-excursion (goto-char beg)
                    (when (looking-at "\n")
                      (move-overlay ov (1+ beg) (1+ beg))))))

(cl-defmethod org-remark-highlight-headline-text (ov (org-remark-type (eql 'line)))
  "Return the first x characters of the line.
If the line is shorter than x, then up to the newline char."
  (let ((line-text (buffer-substring-no-properties
                    (overlay-start ov) (pos-eol))))
    (if (or (eq line-text nil)
            (string= line-text ""))
        "Empty line highlight"
      (setq line-text (string-trim-left line-text))
      (if (length<  line-text
                    (1+ org-remark-line-heading-title-max-length))
          line-text
        (concat (substring line-text 0 org-remark-line-heading-title-max-length)
                org-remark-line-ellipsis)))))

(cl-defmethod org-remark-highlights-adjust-positions-p ((org-remark-type (eql 'line)))
  "
For line-highlights, adjust-positions is not relevant."
  nil)

(cl-defmethod org-remark-highlights-housekeep-delete-p (_ov
                                                        (org-remark-type (eql 'line)))
  "Always return nil when ORG-REMARK-TYPE is \\='line\\='.
Line-highlights are designed to be zero length with the start and
end of overlay being identical."
  nil)

(cl-defmethod org-remark-highlights-housekeep-per-type (ov
                                                        (org-remark-type (eql 'line)))
  "Ensure line-highlight OV is always at the beginning of line."
  ;; if `pos-bol' is used to move, you can actually get the highlight to
  ;; always follow the point, keeping the original place unless you
  ;; directly change the notes. That's not really an intutive behaviour,
  ;; though in some cases, it imay be useful.
  (let* ((ov-start (overlay-start ov))
         (ov-line-bol (org-remark-line-pos-bol ov-start)))
    (unless (= ov-start ov-line-bol)
      (move-overlay ov ov-line-bol ov-line-bol))))

(defun org-remark-line-icon-overlay-put (ov icon-string)
  ;; If the icon-string has a display properties, assume it is an icon image
  (let ((display-prop (get-text-property 0 'display icon-string)))
    (cond (display-prop
           (let* ((display-prop (list '(margin left-margin) display-prop))
                  (icon-string (propertize " " 'display display-prop)))
             (setq icon-string (propertize icon-string
                                           'face 'org-remark-line-highlighter))
             (overlay-put ov 'before-string icon-string)))
          (icon-string
           (setq icon-string (propertize icon-string
                                         'face 'org-remark-line-highlighter))
           (org-remark-line-highlight-overlay-put ov
                                                  'org-remark-line-highlighter
                                                  icon-string))
          (t (ignore)))))

(cl-defmethod org-remark-icon-overlay-put (ov icon-string (org-remark-type (eql 'line)))
  (if (get-buffer-window)
      (org-remark-line-icon-overlay-put ov icon-string)
    (push
     (lambda () (org-remark-line-icon-overlay-put ov icon-string))
     org-remark-line-delayed-put-overlay-functions)))

(provide 'org-remark-line)
;;; org-remark-line.el ends here
