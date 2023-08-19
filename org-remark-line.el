;;; org-remark-line.el --- Enable Org-roam to highlight a line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 01 August 2023
;; Last modified: 19 August 2023
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

(defvar org-remark-line-icon " * ")

(defvar org-remark-line-heading-title-max-length 40)

(defvar org-remark-line-ellipsis "…")

(defvar org-remark-line-minimum-margin-width 3)

(defvar org-remark-line-margin-padding 1
  "Padding between the main text area the icon on the margin")

(defvar-local org-remark-line-margins-original '()
  "Original window margin width values.
It is the original margins returned by function `window-margins'
in cons cell (or nil) before function
`org-remark-line-set-window-margins' set margins.")

(defvar-local org-remark-line-margins-set-p nil
  "State if margins are set by `org-remark-line' for current buffer.")

;;;###autoload
(define-minor-mode org-remark-line-mode
  "Enable Org-remark to highlight and annotate the whole line."
  :global nil
  :group 'org-remark
  (if org-remark-line-mode
      ;; Enable
      (progn
        ;; Depth is deeper than the default one for range-highlight.
        ;; This is to prioritize it over line-highlight when the fomer
        ;; is at point and yet on the same line of another
        ;; line-highlight.
        (add-hook 'org-remark-find-dwim-functions
                  #'org-remark-line-find 80 :local)
        ;; olivetti sets DEPTH to t (=90). We need go lower priority than it
        (add-hook 'window-size-change-functions
                  #'org-remark-line-set-window-margins 95 :local)
        ;; Need to reload to cater to margin changes done by `olivetti'.
        (add-hook 'window-size-change-functions
                  #'org-remark-highlights-load 96 :local)
        (org-remark-line-set-window-margins))
    (remove-hook 'org-remark-find-dwim-functions #'org-remark-line-find :local)
    (remove-hook 'window-size-change-functions
                 #'org-remark-line-set-window-margins :local)
    (remove-hook 'window-size-change-functions
                 #'org-remark-highlights-load :local)
    (when org-remark-line-margins-set-p
      (setq left-margin-width (car org-remark-line-margins-original))
      (setq right-margin-width (cdr org-remark-line-margins-original))
      (set-window-margins nil left-margin-width right-margin-width)
      (set-window-buffer (get-buffer-window) (current-buffer) nil)
      (setq org-remark-line-margins-set-p nil))))

(defun org-remark-line-set-window-margins (&optional window)
  "Set the margins of current window that displays current buffer.
Return a cons of the form (LEFT-WIDTH . RIGHT-WIDTH). If a
marginal area does not exist, its width will be returned as nil."
  (let ((window (or window (get-buffer-window))))
    (when (and (windowp window) (not (window-minibuffer-p window)))
      (cl-destructuring-bind (left-width . right-width) (window-margins)
        ;; TODO make this part compatible with right margin
        (unless org-remark-line-margins-set-p
          (setq org-remark-line-margins-original (window-margins))
          (setq org-remark-line-margins-set-p t))
        (if (or (eq left-width nil) (< left-width
                                       org-remark-line-minimum-margin-width))
            (progn
              (setq left-margin-width org-remark-line-minimum-margin-width)
              (setq right-margin-width org-remark-line-minimum-margin-width))
          (setq left-margin-width left-width)
          (setq right-margin-width right-width))
        ;; For `set-window-margins' window should be specified.
        ;; Howerver, `set-window-buffer' should get nil for window.
        ;; Otherwise, the minibuffer also gets the margins. It's a
        ;; little tricky behaviour. Both functions seem to be required.
        ;; The former changes the current window's margin display
        ;; immediately. The latter makes the margin widths the default
        ;; for future, when window gets split, etc.
        (set-window-margins window left-margin-width right-margin-width)
        (set-window-buffer nil (current-buffer) 'keep-margins)
        (window-margins)))))

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
                   `org-remark-line-highlighter
                   `(org-remark-type line))

(cl-defmethod org-remark-beg-end ((_org-remark-type (eql 'line)))
    (let ((bol (org-remark-line-pos-bol (point))))
      (list bol bol)))

(cl-defmethod org-remark-highlight-make-overlay (beg end face (_org-remark-type (eql 'line)))
  "Make and return a highlight overlay for line-highlight.
Return nil when no window is created for current buffer."
  (when (get-buffer-window)
    (unless org-remark-line-mode (org-remark-line-mode +1))
    (let* ((face (or face 'org-remark-line-highlighter))
           ;; We need to be sure where the minimum-margin-width is set to the buffer
           ;; TODO rigth margin
           (left-margin (or (car (window-margins)) left-margin-width))
           ;; TODO we might want another string per line-highlight pen
           (string (with-temp-buffer
                     (insert org-remark-line-icon)
                     (buffer-string)))
           (string-length (length string))
           (spaces-base-length (- left-margin
                                  (+ string-length org-remark-line-margin-padding)))
           (spaces-length (if (> spaces-base-length 0) spaces-base-length 0))
           (spaces (with-temp-buffer (insert-char ?\s spaces-length)
                                     (buffer-string)))
           (spacer-ov (make-overlay beg end nil :front-advance))
           (ov (make-overlay beg end nil :front-advance)))
      ;; Add a spacing overlay before the line-highlight overlay but we
      ;; only need one of these; remove it if one already exits
      (remove-overlays (overlay-start spacer-ov) (overlay-end spacer-ov)
                       'category 'org-remark-spacer)
      (overlay-put spacer-ov 'before-string (propertize " "
                                                        'display
                                                        `((margin left-margin)
                                                          ,spaces)))
      (overlay-put spacer-ov 'category 'org-remark-spacer)
      (overlay-put spacer-ov 'insert-in-front-hooks
                   (list 'org-remark-line-highlight-modified))
      ;; line-highlight overlay
      (overlay-put ov 'before-string (propertize " " 'display
                                                 `((margin left-margin)
                                                   ,(propertize string 'face face))))
      (overlay-put ov 'insert-in-front-hooks (list 'org-remark-line-highlight-modified))
      ov)))

(defun org-remark-line-highlight-find-spacer (pos)
  (let ((highlights (overlays-in pos pos)))
    (seq-find (lambda (ov)
                (eql 'org-remark-spacer (overlay-get ov 'category)))
              highlights)))

(defun org-remark-line-highlight-modified (ov after-p beg _end &optional _length)
  "Move the overlay to follow the point when ENTER in the line."
  (when after-p
    (save-excursion (goto-char beg)
                    (when (looking-at "\n")
                      ;; Spacer needs to move before the hightlight to keep the sequence.
                      (let ((spacer (org-remark-line-highlight-find-spacer beg)))
                        (when spacer (move-overlay spacer (1+ beg) (1+ beg))))
                      (move-overlay ov (1+ beg) (1+ beg))))))

(cl-defmethod org-remark-highlight-headline-text (ov (_org-remark-type (eql 'line)))
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

(cl-defmethod org-remark-highlights-adjust-positions-p ((_org-remark-type (eql 'line)))
  "
For line-highlights, adjust-positions is not relevant."
  nil)

(cl-defmethod org-remark-highlights-housekeep-delete-p (_ov (_org-remark-type (eql 'line)))
  "Always return nil when ORG-REMARK-TYPE is \\='line\\='.
Line-highlights are designed to be zero length with the start and
end of overlay being identical."
  nil)

(cl-defmethod org-remark-highlights-housekeep-per-type (ov (_org-remark-type (eql 'line)))
  "Ensure line-highlight OV is always at the beginning of line."
  ;; if `pos-bol' is used to move, you can actually get the highlight to
  ;; always follow the point, keeping the original place unless you
  ;; directly change the notes. That's not really an intutive behaviour,
  ;; though in some cases, it imay be useful.
  ;; (if (not (overlay-start ov)) (delete-overlay ov)
  (let* ((ov-start (overlay-start ov))
         (ov-line-bol (org-remark-line-pos-bol ov-start)))
    (unless (= ov-start ov-line-bol)
      (move-overlay ov ov-line-bol ov-line-bol))))

(cl-defmethod org-remark-icon-overlay-put (ov icon-string (_org-remark-type (eql 'line)))
  "
Return nil when no window is created for current buffer."
  (when (get-buffer-window)
    ;; If the icon-string has a display properties, assume it is an icon image
    (let ((display-prop (get-text-property 0 'display icon-string)))
      (cond (display-prop
             (let* ((display-prop (list '(margin left-margin) display-prop))
                    (icon-string (propertize " " 'display display-prop)))
              ;; (setq icon-string (propertize icon-string
              ;;                               'face 'org-remark-line-highlighter))
               (overlay-put ov 'before-string icon-string)))
            (icon-string
             (let ((icon-string icon-string))
                    ;;(propertize icon-string
                    ;;                        'face 'org-remark-line-highlighter)))
               (overlay-put ov 'before-string (propertize " " 'display (list '(margin left-margin) icon-string)))))
            (t (ignore))))))


(cl-defmethod org-remark-icon-highlight-get-face (highlight (_org-remark-type (eql 'line)))
  "Return the face of the line-highilght in a margin."
  (get-text-property 0 'face
                     (cadr (get-text-property 0 'display
                                              (overlay-get highlight 'before-string)))))

(provide 'org-remark-line)
;;; org-remark-line.el ends here
