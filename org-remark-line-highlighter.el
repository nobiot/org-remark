;;; org-remark-line.el --- Enable Org-roam to highlight a line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Created: 01 August 2023
;; Last modified: 01 August 2023
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

(defun my/test-margin-left ()
  (interactive)
  (let* ((ov (make-overlay (line-beginning-position) (line-beginning-position)))
         (left-margin (or (car (window-margins))
                          ;; when nil = no margin, set to 1
                          (progn (set-window-margins nil 2)
                                 2)))
         (spaces (- left-margin 2))
         (string (with-temp-buffer (insert-char ?\s spaces)
                                   (insert "‣ ")
                                   (buffer-string))))
    (overlay-put ov 'before-string (propertize "! " 'display
                                               `((margin left-margin)
                                                 ,(propertize string 'face 'modus-themes-markup-code))))))

(defun test/get-beginning-of-line (pos)
  "Return the beginning of the line position for POS."
  (save-excursion
    (goto-char pos)
    (pos-bol)))


(defun test/overlay-put-line-highlight (ov)
  (let* ((left-margin (or (car (window-margins))
                          ;; when nil = no margin, set to 1
                          (progn (set-window-margins nil 2)
                                 2)))
         (spaces (- left-margin 2))
         (string (with-temp-buffer (insert-char ?\s spaces)
                                   (insert " ")
                                   (buffer-string))))
    (overlay-put ov 'before-string (propertize "! " 'display
                                               `((margin left-margin)
                                                 ,(propertize string 'face 'modus-themes-diff-refine-removed))))
    ;;(overlay-put ov 'category "org-remark-line") ;; need to fix property add logic
    ov))

;;;###autoload
(defun org-remark-mark-line (beg end &optional id mode)
  (interactive (org-remark-region-or-word))
  (org-remark-line-highlight-mark beg end id mode
                                  "line" nil ;; LINE needs to be the suffix of a function: `org-remark-mark-'
                                  (list 'org-remark-type "line")))

(defun org-remark-line-highlight-mark
    (beg end &optional id mode label face properties)
  "Apply the FACE to the whole line that contains BEG."
  ;; Ensure to turn on the local minor mode
  (unless org-remark-mode (org-remark-mode +1))
  ;; When highlights are toggled hidden, only the new one gets highlighted in
  ;; the wrong toggle state.
  (when org-remark-highlights-hidden (org-remark-highlights-show))
  (let ((ov (make-overlay (test/get-beginning-of-line beg) (test/get-beginning-of-line beg))) ;; LINE without :front-advance
        ;; UUID is too long; does not have to be the full length
        (id (if id id (substring (org-id-uuid) 0 8)))
        (filename (org-remark-source-find-file-name)))
    (if (not filename)
        (message (format "org-remark: Highlights not saved.\
 This buffer (%s) is not supported" (symbol-name major-mode)))
      (org-with-wide-buffer
       ;;(overlay-put ov 'face (if face face 'org-remark-highlighter)) ;; LINE
       (test/overlay-put-line-highlight ov) ;; LINE
       (while properties ;; LINE add prop to indicate it is a line highlighter
         (let ((prop (pop properties))
               (val (pop properties)))
           (overlay-put ov prop val)))
       (when label (overlay-put ov 'org-remark-label label)) ;; LINE put a label for line (allow variations)
       (overlay-put ov 'org-remark-id id)
       ;; Keep track of the overlay in a local variable. It's a list that is
       ;; guaranteed to contain only org-remark overlays as opposed to the one
       ;; returned by `overlay-lists' that lists all overlays.
       (push ov org-remark-highlights)
       ;; for mode, nil and :change result in saving the highlight.  :load
       ;; bypasses save.
       (unless (eq mode :load)
         (let* ((notes-buf (find-file-noselect
                            (org-remark-notes-get-file-name)))
                (source-buf (current-buffer))
                ;; Get props for create and change modes
                (notes-props
                 (org-remark-highlight-add ov source-buf notes-buf)))
           (when notes-props
             (org-remark-highlight-put-props ov notes-props))
           ;; Save the notes buffer when not loading
          (unless (eq notes-buf (current-buffer))
                       (with-current-buffer notes-buf (save-buffer)))))) ;; LINE. save-buffer triggers something that deletes this highilght
      (deactivate-mark)
      (org-remark-highlights-housekeep) ;;LINE is a zero width overlay! Need to escape them.
      (org-remark-highlights-sort)
      (setq org-remark-source-setup-done t)
      ;; Return overlay
      ov)))

(provide 'org-remark-line)
;;; org-remark-line.el ends here
