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

;;(defun org-remark-housekeep-before-render ()
;;  (setq org-remark-highlights nil))
;;(add-hook 'nov-pre-html-render-hook #'org-remark-housekeep-before-render)
