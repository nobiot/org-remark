;;; org-remark-nov.el --- Compatibility with nov-mode -*- lexical-binding: t; -*-

;; URL: https://github.com/nobiot/org-remark
;; Created: 9 January 2023
;; Last modified: 12 January 2023

;;; Commentary:

;;; Code:

(require 'nov)
;;(declare-function org-remark-auto-on "org-remark-global-tracking")
(declare-function org-remark-highlights-load "org-remark")
(defvar org-remark-notes-headline-functions)

;; TODO add a minor mode to let user switch these hooks on and off.
(add-hook 'org-remark-source-find-file-name-functions #'org-remark-get-epub-source)
(add-hook 'org-remark-highlight-link-to-source-functions #'org-remark-nov-link)
;; When users turn the page (document in nov-mode's terminology)
;; `nov-mode' will erase the current buffer and render the new document
;; content in the same buffer.  This means the highlights currently
;; displayed get removed; the ones for the new document need to be
;; loaded document after `nov-mode' renders the new document.
(add-hook 'nov-post-html-render-hook #'org-remark-highlights-load)
(add-hook 'org-remark-highlights-after-load-hook
          #'org-remark-nov-highlight-adjust-positions)
(add-to-list 'org-remark-notes-headline-functions
  '(nov-mode . ((1 . org-remark-nov-highlight-add-book-headline-maybe)
                (2 . org-remark-highlight-add-source-headline-maybe))))

(defun org-remark-get-epub-source ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (concat
     (file-name-nondirectory nov-file-name)
     "/"
     (file-name-base (cdr (aref nov-documents nov-documents-index))))))

(defun org-remark-nov-link (_filname)
  "Return \"nov:\" link with current point in `nov-mode' buffer.

This function only works when the mode is `nov-mode'.

Assume the point is on the highlight in source epub document
buffer."
  (when (eq major-mode 'nov-mode)
    (org-store-link nil)))

(defun org-remark-nov-highlight-add-book-headline-maybe (level source-buf notes-buf)
  "Add a book headline if not present in NOTES-BUF for epub file.
Return the point of beginning of book headline regardless of it
being newly added or already present.

LEVEL is the headline level when one is to be added.

SOURCE-BUF is a `nov-mode' buffer visiting a document within an
epub file.

Assume the current buffer is NOTES-BUF."
  (let (filename title)
    (with-current-buffer source-buf
      ;; The nov variables are only locally set in the source buffer.
      (setq filename nov-file-name
            title (cdr (assoc 'title nov-metadata))))
    ;; Back in the notes buffer, return the point of the beginning of
    ;; the headline
    (with-current-buffer notes-buf
      (or (org-find-property "org-remark-nov-file" filename)
          (progn
            ;; If the book entry does not exist, create one at the bottom of notes buffer
            (goto-char (point-max))
            ;; Ensure to be in the beginning of line to add a new headline
            (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
            (insert-char (string-to-char "*") level)
            ;; Title is assumed to present but the program won't fail even if it is nil.
            (insert (concat " " title "\n"))
            (org-set-property "org-remark-nov-file" filename)
            (org-back-to-heading) (point))))))

(defun org-remark-nov-highlight-adjust-positions (overlays _notes-buf)
  (dolist (ov overlays)
    (let ((highlight-text (overlay-get ov '*org-remark-original-text)))
      (when highlight-text (test/move-highlight ov highlight-text)))))

(provide 'org-remark-nov)
;;; org-remark-nov.el ends here
