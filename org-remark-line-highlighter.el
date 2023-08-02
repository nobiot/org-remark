;;; org-remark-line.el --- Enable Org-roam to highlight a line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 01 August 2023
;; Last modified: 02 August 2023
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

;;(require 'org-remark)

(defun org-remark-line-pos-bol (pos)
  "Return the beginning of the line position for POS."
  (save-excursion
    (goto-char pos)
    (pos-bol)))

(defun org-remark-line-highlight-p (highlight)
  "Return t if HIGHLIGHT is one for the line.
HIGHLIGHT is an overlay."
  (string= "line" (overlay-get highlight 'org-remark-type)))

(defun org-remark-line-find (&optional point)
  "Return the line-highight (overlay) of the current line.
When POINT is passed, one for the line it belongs to. If there
are multiple line-hilights, return the car of the list returned
by `overlays-in'."
  (let* ((point (or point (point)))
         (bol (org-remark-line-pos-bol point))
         (highlights (overlays-in bol bol)))
    (seq-find #'org-remark-line-highlight-p highlights)))

(add-hook 'org-remark-find-dwim-functions #'org-remark-line-find)

(add-hook 'window-size-change-functions
          #'(lambda (&rest args)
              (set-window-margins nil 2)))

(defun test/overlay-put-line-highlight (ov)
  (let* ((left-margin (or (car (window-margins))
                          ;; when nil = no margin, set to 1
                          (progn (set-window-margins nil 2)
                                 2)))
         (spaces (- left-margin 2))
         (string (with-temp-buffer (insert-char ?\s spaces)
                                   (insert "•")
                                   (buffer-string))))
    (overlay-put ov 'before-string (propertize "! " 'display
                                               `((margin left-margin)
                                                 ,(propertize string 'face 'modus-themes-diff-refine-removed))))
    ;;(overlay-put ov 'category "org-remark-line") ;; need to fix property add logic
    ov))

(defun org-remark-line-highlight-modified (ov after-p beg end &optional length)
  "This is good! Move the overlay to follow the point when ENTER in the line."
  (when after-p
    (save-excursion (goto-char beg)
                    (when (looking-at "\n")
                      (move-overlay ov (1+ beg) (1+ beg))))))

;;;###autoload
(defun org-remark-mark-line (beg end &optional id mode)
  (interactive (org-remark-beg-end 'line))
  (org-remark-highlight-mark beg end id mode  ;; LINE line function different
                             ;; LINE needs to be the suffix of a
                             ;; function: `org-remark-mark-'
                             "line" nil ;; LINE important to put
                             ;; the suffix of the label
                             ;; to call this correct function
                             (list 'org-remark-type 'line)))

;; (add-hook 'org-remark-beg-end-dwim-functions #'org-remark-line-beg-end)
;; This is not a good solution. The normal highlight gets zero length with this.
;; It needs to be "type" differentiated by a defmethod, etc.

;; (remove-hook 'org-remark-beg-end-dwim-functions #'org-remark-line-beg-end)

;; (defun org-remark-line-beg-end ()
;;   "Return a list of beg and end both being the bol."
;;   (let ((bol (org-remark-line-pos-bol (point))))
;;     (list bol bol)))

(cl-defmethod org-remark-beg-end ((org-remark-type (eql 'line)))
    (let ((bol (org-remark-line-pos-bol (point))))
      (list bol bol)))

(cl-defmethod org-remark-highlight-mark-overlay (ov face (org-remark-type (eql 'line)))
  (test/overlay-put-line-highlight ov) ;; LINE
  (overlay-put ov 'insert-in-front-hooks (list 'org-remark-line-highlight-modified)))

;; (defun org-remark-line-highlight-mark
;;     (beg end &optional id mode label face properties)
;;   "Apply the FACE to the whole line that contains BEG."
;;   ;; Ensure to turn on the local minor mode
;;   (unless org-remark-mode (org-remark-mode +1))
;;   ;; When highlights are toggled hidden, only the new one gets highlighted in
;;   ;; the wrong toggle state.
;;   (when org-remark-highlights-hidden (org-remark-highlights-show))
;;   (let ((ov (make-overlay beg end nil :front-advance))
;;         ;; UUID is too long; does not have to be the full length
;;         (id (if id id (substring (org-id-uuid) 0 8)))
;;         (filename (org-remark-source-find-file-name)))
;;     (if (not filename)
;;         (message (format "org-remark: Highlights not saved.\
;;  This buffer (%s) is not supported" (symbol-name major-mode)))
;;       (org-with-wide-buffer
;;        (org-remark-highlight-mark-overlay ov face 'line)
;;        (while properties
;;          (let ((prop (pop properties))
;;                (val (pop properties)))
;;            (overlay-put ov prop val)))
;;        (when label (overlay-put ov 'org-remark-label label))
;;        (overlay-put ov 'org-remark-id id)
;;        ;; Keep track of the overlay in a local variable. It's a list that is
;;        ;; guaranteed to contain only org-remark overlays as opposed to the one
;;        ;; returned by `overlay-lists' that lists all overlays.
;;        (push ov org-remark-highlights)
;;        ;; for mode, nil and :change result in saving the highlight.  :load
;;        ;; bypasses save.
;;        (unless (eq mode :load)
;;          (let* ((notes-buf (find-file-noselect
;;                             (org-remark-notes-get-file-name)))
;;                 (source-buf (current-buffer))
;;                 ;; Get props for create and change modes
;;                 (notes-props
;;                  (org-remark-highlight-add ov source-buf notes-buf)))
;;            (when notes-props
;;              (org-remark-highlight-put-props ov notes-props))
;;            ;; Save the notes buffer when not loading
;;           (unless (eq notes-buf (current-buffer))
;;                        (with-current-buffer notes-buf (save-buffer))))))
;;       (deactivate-mark)
;;       (org-remark-highlights-housekeep)
;;       (org-remark-highlights-sort)
;;       (setq org-remark-source-setup-done t)
;;       ;; Return overlay
;;       ov)))

(provide 'org-remark-line)
;;; org-remark-line.el ends here
