;;; org-remark-line.el --- Enable Org-roam to highlight a line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 01 August 2023
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

;;; Code:

(require 'org-remark)

(defgroup org-remark-line nil
  "Enable`org-remark' to highlight and annotate whole lines."
  :group 'org-remark
  :prefix "org-remark-line"
  :link '(url-link :tag "GitHub" "https://github.com/nobiot/org-remark"))

(defcustom org-remark-line-icon " * "
  "Glyph displayed on the margin to indicate the line-highlight.
If you wants to use image icons (e.g. SVG image icon created with
package `icons', available Emacs 29.1 or higher), you're limited
to a single character with no space before and after the
character. This limitation does not apply to string of characters
without images, but it is generally assumed that the the value
set to this customizing variable will be a short string (e.g 3
characters long with a padding spaces before and after a single
character, such as the default value.)"
  :local t
  :type 'string
  :safe 'stringp)

(defcustom org-remark-line-minimum-margin-width 3
  "Margin width in a natural number.
It can be a single number or a cons cell of two. When it is a
single number, both the left and right margin widths will be the
same. When this customizing variable is a cons cell, the format
is as follows: (LEFT-MARGIN-WIDTH . RIGHT-MARGIN-WIDTH)."
  :local t
  :type '(choice
          (natnum :tag "Minimum margin width for both left and right margins" 3)
          (cons :tag "Left and right margin widths" natnum natnum)))

(defcustom org-remark-line-margin-padding 1
  "Padding between the main text area the glyph/icon on the margin."
  :local t
  :type 'natnum)

(defcustom org-remark-line-margin-side 'left-margin
  "The side of margin to display line highlights.
Left or right can be chosen."
  :local t
  :type '(radio
          (const :tag "Left margin" left-margin)
          (const :tag "Right margin" right-margin)))

(defcustom org-remark-line-heading-title-max-length 40
  "Maximum length of string included as the highlight title."
  :type 'natnum)

(defcustom org-remark-line-ellipsis "…"
  "Ellipsis used when the highlight title is longer than maximum.
The maximum is set in `org-remark-line-heading-title-max-length'."
  :type 'string
  :safe 'stringp)

(defface org-remark-line-highlighter
  '((((class color) (min-colors 88) (background light))
     :foreground "#dbba3f" :inherit default)
    (((class color) (min-colors 88) (background dark))
     :foreground "#e2d980" :inherit default)
    (t
     :inherit highlight))
  "Face for the default line highlighter pen.")

(defvar-local org-remark-line-minimum-left-margin-width nil
  "Computed minimum `left-margin' width.")

(defvar-local org-remark-line-minimum-right-margin-width nil
  "Computed minimum `right-margin' width.")

(defvar-local org-remark-line-margins-original '()
  "Original window margin width values.
It is the original margins returned by function `window-margins'
in cons cell (or nil) before function
`org-remark-line-set-window-margins' set margins.")

(defvar-local org-remark-line-margins-set-p nil
  "Status indicating if margins are set by `org-remark-line'.")

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
                  #'org-remark-line-highlights-redraw 96 :local)
        (add-hook 'org-remark-highlight-other-props-functions
                  #'org-remark-line-prop-line-number-get)
        (org-remark-line-set-window-margins))
    ;; Disable
    (remove-hook 'org-remark-find-dwim-functions #'org-remark-line-find :local)
    (remove-hook 'window-size-change-functions
                 #'org-remark-line-set-window-margins :local)
    (remove-hook 'window-size-change-functions
                 #'org-remark-line-highlights-redraw :local)
    (remove-hook 'org-remark-highlight-other-props-functions
                 #'org-remark-line-prop-line-number-get)
    (when org-remark-line-margins-set-p
      (setq left-margin-width (car org-remark-line-margins-original))
      (setq right-margin-width (cdr org-remark-line-margins-original))
      (set-window-margins nil left-margin-width right-margin-width)
      (set-window-buffer (get-buffer-window) (current-buffer) nil)
      (setq org-remark-line-minimum-left-margin-width nil)
      (setq org-remark-line-minimum-right-margin-width nil)
      (setq org-remark-line-margins-set-p nil))))

;; Default line-highlighter pen

;;;###autoload
(defun org-remark-mark-line (_beg _end &optional _id _mode)
  "Apply face to the region selected by BEG and END.
Dummy function definition to let autoload work.
The actual implementation is added when this library is loaded
and macro `org-remark-create' creates the actual function.")

(org-remark-create "line"
                   `org-remark-line-highlighter
                   `(org-remark-type line))

(defun org-remark-line-set-window-margins (&optional window)
  "Set the margins of WINDOW or window that displays current buffer.
Return a cons of the form (LEFT-WIDTH . RIGHT-WIDTH). If a
marginal area does not exist, return nil."
  (let ((window (or window (get-buffer-window))))
    (when (and (windowp window) (not (window-minibuffer-p window)))
      (cl-destructuring-bind (left-width . right-width) (window-margins)
        (unless org-remark-line-margins-set-p
          (setq org-remark-line-margins-original (window-margins))
          (setq org-remark-line-margins-set-p t)
          (setq org-remark-line-minimum-left-margin-width
                (+ (if (numberp org-remark-line-minimum-margin-width)
                       org-remark-line-minimum-margin-width
                     (car org-remark-line-minimum-margin-width))
                   org-remark-line-margin-padding))
          (setq org-remark-line-minimum-right-margin-width
                (+ (if (numberp org-remark-line-minimum-margin-width)
                       org-remark-line-minimum-margin-width
                     (cdr org-remark-line-minimum-margin-width))
                   org-remark-line-margin-padding)))
        (if (or (eq left-width nil) (< left-width
                                       org-remark-line-minimum-left-margin-width))
            (setq left-margin-width org-remark-line-minimum-left-margin-width)
          (setq left-margin-width left-width))
        (if (or (eq right-width nil) (< right-width
                                        org-remark-line-minimum-right-margin-width))
            (setq right-margin-width org-remark-line-minimum-right-margin-width)
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

(defun org-remark-line-prop-line-number-get (highlight)
  "Return the line number for HIGHLIGHT overlay.
This happens only when HIGHLIGHT is a line-highlight."
  (when (org-remark-line-highlight-p highlight)
    (list 'org-remark-line-number
          (number-to-string (org-current-line (overlay-start highlight))))))

(cl-defmethod org-remark-beg-end ((_org-remark-type (eql 'line)))
  "Return beg and end for ORG-REMARK-TYPE line."
    (let ((bol (org-remark-line-pos-bol (point))))
      (list bol bol)))

(defun org-remark-line-make-spacer-overlay (pos)
  "Return a spacer overlay at POS."
  (let* ((left-margin (or (car (window-margins)) left-margin-width))
         ;;(right-margin (or (cdr (window-margins)) right-margin-width))
         (string-length (length org-remark-line-icon))
         (spaces-base-length (if (eql org-remark-line-margin-side 'right-margin)
                                 org-remark-line-margin-padding
                               (- left-margin
                                  (+ string-length org-remark-line-margin-padding))))
         (spaces-length (if (> spaces-base-length 0) spaces-base-length 0))
         (spacer-ov (make-overlay pos pos nil :front-advance)))
    ;; Add a spacing overlay before the line-highlight overlay but we
    ;; only need one of these; remove it if one already exits
    (remove-overlays (overlay-start spacer-ov) (overlay-end spacer-ov)
                     'category 'org-remark-spacer)
    (overlay-put spacer-ov 'before-string
                 (propertize " "
                             'display
                             `((margin ,org-remark-line-margin-side)
                               (space . (:width ,spaces-length)))))
    (overlay-put spacer-ov 'category 'org-remark-spacer)
    spacer-ov))

(defun org-remark-line-highlights-redraw (&optional window)
  "Redraw line-highlights to adjust the spaces/padding.
When WINDOW is nil, this function gets window that current buffer is displayed."
  (let ((window (or window (get-buffer-window))))
    (when (and (windowp window) (not (window-minibuffer-p window)))
      (org-with-wide-buffer
       (let ((highlights
              (seq-filter (lambda (ov) (eql 'line (overlay-get ov 'org-remark-type)))
                          org-remark-highlights)))
         (dolist (ov highlights)
           (let* ((beg (overlay-start ov))
                  (spacer-ov (org-remark-line-make-spacer-overlay beg))
                  (copied-highlight (copy-overlay ov))
                  (display-props
                   (get-text-property 0 'display (overlay-get copied-highlight 'before-string))))
             (setf (car display-props) `(margin ,org-remark-line-margin-side))
             (push copied-highlight org-remark-highlights)
             (copy-overlay spacer-ov)
             (delete-overlay ov)
             (org-remark-highlights-housekeep)
             (org-remark-highlights-sort))))))))

(defun org-remark-line-highlight-propertize (ov icon-string)
  "Propertize ICON-STRING and add it to OV."
  ;; If the icon-string has a display properties, assume it is an icon image
  (let ((display-prop (get-text-property 0 'display icon-string)))
    (cond (display-prop ; svg-based icon
           (let* ((display-prop (list `(margin ,org-remark-line-margin-side) display-prop))
                   ;; TODO margin needs to be calculated
                   ;; (list `(margin ,org-remark-line-margin-side)
                   ;;       (append display-prop '(:margin (10 . 0))))))
                  (icon-face (get-text-property 0 'face icon-string))
                  (icon-string (propertize " " 'display display-prop)))
             (when icon-face
               (setq icon-string (propertize icon-string 'face icon-face)))
             (overlay-put ov 'before-string icon-string)))
          (icon-string ; text/string-based icon
           (let ((icon-string icon-string))
             (overlay-put
              ov
              'before-string
              (propertize
               " " 'display
               (list `(margin ,org-remark-line-margin-side) icon-string)))))
          (t (ignore)))))

(cl-defmethod org-remark-highlight-make-overlay (beg end face (_org-remark-type (eql 'line)))
  "Make and return a highlight overlay in BEG END for line-highlight.
This function adds FACE to line icon string. If FACE is nil, this
function uses default `org-remark-line-highlighter'. Return nil
when no window is created for current buffer."
  (when (get-buffer-window)
    (unless org-remark-line-mode (org-remark-line-mode +1))
    (let* ((face (or face 'org-remark-line-highlighter))
           (string (propertize org-remark-line-icon 'face face))
           (spacer-ov (org-remark-line-make-spacer-overlay beg))
           (ov (make-overlay beg end nil :front-advance)))
      ;; line-highlight overlay
      (org-remark-line-highlight-propertize ov string)
      ;; Let highlight overlay to take care of the spacer movement
      (overlay-put ov 'insert-in-front-hooks (list 'org-remark-line-highlight-modified))
      ;; Copy spacer overlay. It is put after the line-highlight to
      ;; limit and reset the face added by the line-highlight back to
      ;; default. This is especially done for RTL languages and when the
      ;; face include a background color different from that of default.
      ;; Without it, the background color goes all the way to the end of
      ;; the right margin.
      (copy-overlay spacer-ov)
      ov)))

(defun org-remark-line-highlight-find-spacers (pos)
  "Find the two spacers for POS."
  (let ((highlights (overlays-in pos pos)))
    (seq-filter (lambda (ov)
                  (eql 'org-remark-spacer (overlay-get ov 'category)))
                highlights)))

(defun org-remark-line-highlight-modified (ov after-p beg _end &optional _length)
  "Move spacers and lighlight OV to follow the point.
Without this function, the line-highlighter mark does not move
when the user press RET to add a newline at the beginning of the
line-highlight. This is unintuitive for the user.

This function is meant to be added to insert-in-front-hooks of
the overlay that represents line-highlight. It must be called
AFTER-P is non-nil and move BEG to one position forward."
  (when after-p
    (save-excursion (goto-char beg)
                    (when (looking-at "\n")
                      ;; The sequence must be 1. spacer; 2. highlight 3. spacer
                      (let ((spacers (org-remark-line-highlight-find-spacers beg)))
                        (when spacers
                          (move-overlay (pop spacers) (1+ beg) (1+ beg)))
                        (move-overlay ov (1+ beg) (1+ beg))
                        (when spacers
                          (move-overlay (pop spacers) (1+ beg) (1+ beg))))))))

(cl-defmethod org-remark-highlight-headline-text (ov (_org-remark-type (eql 'line)))
  "Return the first N characters of the highlighted line OV.
N is customized with `org-remark-line-heading-title-max-length'.
If the line starts with any space or tab, they will be trimmed.
If the line (after trimming) is shorter than N, then this
function will include the charcters up to the newline char.

In addition, if the text happens to be empty, the function uses
\"Empty line highlight\" as the fallback; headlines with no title
is not considered valid for the purpose of `org-remark' and thus
risks unexpected results (mostly the highlight skipped when
loading highlights)."
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
  "Return t if adjust-positions feature is relevant.
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
  (when (overlay-buffer ov)
    (let* ((ov-start (overlay-start ov))
           (ov-line-bol (org-remark-line-pos-bol ov-start)))
      (unless (= ov-start ov-line-bol)
        (move-overlay ov ov-line-bol ov-line-bol)))))

(cl-defmethod org-remark-icon-overlay-put (ov icon-string (_org-remark-type (eql 'line)))
  "Add ICON-STRING to OV.
Each overlay is a highlight. Return nil when no window is created
for current buffer."
  (when (get-buffer-window)
    (org-remark-line-highlight-propertize ov icon-string)))

(cl-defmethod org-remark-icon-highlight-get-face (highlight (_org-remark-type (eql 'line)))
  "Return the face of HIGHLIGHT in margin for line-highlight."
  (let* ((before-string (overlay-get highlight 'before-string))
         (face (get-text-property 0 'face before-string)))
    ;; When the highlight already is an SVG icon, face is in the display
    ;; property of before-string
    (unless face
      (let ((display-string
             (cadr (get-text-property 0 'display before-string))))
        (when (stringp display-string)
          (setq face (get-text-property 0 'face display-string)))))
    face))

(provide 'org-remark-line)
;;; org-remark-line.el ends here
