;; -*- lexical-binding: t; -*-

;; compatibility with org-noter
(defun org-remark-get-epub-source ()
  "Returns the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (concat
     (file-name-nondirectory nov-file-name)
     "/"
     (file-name-base (cdr (aref nov-documents nov-documents-index))))))

(defun org-remark-nov-link (_filname)
  ;; Assume the point is on the highlight in source epub buffer.
  (when (eq major-mode 'nov-mode)
    (org-store-link nil)))

(add-hook 'org-remark-source-find-file-name-functions #'org-remark-get-epub-source)
(add-hook 'org-remark-highlight-link-to-source-functions #'org-remark-nov-link)
(add-hook 'nov-post-html-render-hook #'org-remark-highlights-load)

(add-to-list 'org-remark-notes-create-entry-functions
  '(nov-mode . ((1 . org-remark-nov-highlight-save-book-entry)
                (2 . org-remark-highlight-save-file-entry))))

(defun org-remark-nov-highlight-save-book-entry (level source-buf _notes-buf)
  ".
Assume the current buffer is in the notes buffer."
  (let (filename title)
    (with-current-buffer source-buf
      (setq filename nov-file-name
            title (cdr (assoc 'title nov-metadata))))
    (or (org-find-property
         "org-remark-nov-file" filename)
        (progn
          ;; If file-headline does not exist, create one at the bottom
          (goto-char (point-max))
          ;; Ensure to be in the beginning of line to add a new headline
          (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
          (insert-char (string-to-char "*") level)
          (insert (concat " " title "\n"))
          (org-set-property "org-remark-nov-file" filename)
          (point)))))

;;; TODO move this test function to the user manual as a sample
(defun test/simple-headline (level source-buf _notes-buf)
  (let (filename)
    (with-current-buffer source-buf
      (setq filename (org-remark-source-get-file-name
                      (org-remark-source-find-file-name))))
    (or (org-find-property
         "org-remark-file-name" filename)
        (progn
          ;; If file-headline does not exist, create one at the bottom
          (goto-char (point-max))
          ;; Ensure to be in the beginning of line to add a new headline
          (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
          (insert-char (string-to-char "*") level)
          (insert (concat " " "title" "\n"))
          (org-set-property "org-remark-file-name" filename)
          (point)))))

(provide 'org-remark-nov)
;;; org-remark-nov.el ends here
