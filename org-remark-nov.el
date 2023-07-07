;;; org-remark-nov.el --- Compatibility with nov-mode -*- lexical-binding: t; -*-

;; URL: https://github.com/nobiot/org-remark
;; Created: 9 January 2023
;; Last modified: 07 July 2023

;;; Commentary:

;;; Code:

(require 'nov)
;;(require 'org-remark-global-tracking)
(declare-function org-remark-highlights-load "org-remark")
;;(defvar org-remark-notes-headline-functions)
(require 'org-remark) ;; require to recognize `org-remark-notes-headline-functions'

;;;###autoload
(define-minor-mode org-remark-nov-mode
  "Enable Org-remark to work with `nov-mode' for eub."
  :global t
  :group 'org-remark
  (if org-remark-nov-mode
      ;; Enable
      (progn
        (add-hook 'org-remark-source-find-file-name-functions
                  #'org-remark-get-epub-source)
        (add-hook 'org-remark-highlight-link-to-source-functions #'org-remark-nov-link)
        (add-hook 'org-remark-highlight-link-to-source-functions
                  #'org-remark-nov-link)
        ;; When users turn the page (document in nov-mode's terminology)
        ;; `nov-mode' will erase the current buffer and render the new document
        ;; content in the same buffer.  This means the highlights currently
        ;; displayed get removed; the ones for the new document need to be
        ;; loaded document after `nov-mode' renders the new document.
        (add-hook 'nov-post-html-render-hook #'org-remark-highlights-load))
    ;; Disable
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'org-remark-get-epub-source)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'org-remark-nov-link)
    (remove-hook 'org-remark-highlight-link-to-source-functions
              #'org-remark-nov-link)
    (remove-hook 'nov-post-html-render-hook #'org-remark-highlights-load)))

(cl-defmethod org-remark-notes-get-file-name-for-mode (&context (major-mode nov-mode))
  "For nov.el mode, do something"
  (let ((filename
         (cond (;; if `org-remark-notes-file-name' is a user's custom function, use it as is.
                (and (functionp org-remark-notes-file-name)
                     (not (eq org-remark-notes-file-name #'org-remark-notes-file-name-function)))
                (funcall org-remark-notes-file-name))
               ;; if it is a default function, then do something different for nov.el.
               ((functionp org-remark-notes-file-name)
                (concat (file-name-sans-extension nov-file-name)
                  "-notes.org"))
               ;; the only other case is a string. Assume the default
               ;; marginalia.org but it should be the same for custom
               ;; string.
               (t
                org-remark-notes-file-name))))
    ;; Even if `org-remark-notes-file-name' is a user-defined custom
    ;; function, check if it is relative. If so, make it absolute by
    ;; adding the directory path to the epub file. The purpose is to
    ;; avoid the directory to be the temp file that nov.el creates for
    ;; the html file to be rendered.
    (unless (file-name-absolute-p filename)
      ;; major mode is nov and in the document buffer.
      (setq filename (expand-file-name filename (file-name-directory nov-file-name))))
    filename))

(defun org-remark-get-epub-source ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (concat
     (file-name-nondirectory nov-file-name)
     "/"
     (file-name-base (cdr (aref nov-documents nov-documents-index))))))

(defun org-remark-nov-get-epub-document-title ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    (let ((temp-filename (cdr (aref nov-documents nov-documents-index))))
      (file-name-base temp-filename))))

;; (defun org-remark-get-epub-filename ()
;;   "Return the path of the epub source from which the present session is initiated."
;;   (when (eq major-mode 'nov-mode)
;;     nov-file-name))

(defun org-remark-nov-link (_filname _point)
  "Return \"nov:\" link with current point in `nov-mode' buffer.

This function only works when the mode is `nov-mode'.

Assume the point is on the highlight in source epub document
buffer."
  (when (eq major-mode 'nov-mode)
    (org-store-link nil)))

(defun test/org-remark-nov-highlight-headlines (_level source-buf notes-buf)
  (org-remark-nov-highlight-new-headline-maybe source-buf notes-buf))

(cl-defmethod org-remark-highlight-get-constructors (&context (major-mode nov-mode))
  "Dev needs to define a mode-specific headline constructors.
`(level source-filename-fn title-fn prop-to-find)`'"
  (let* ((headline-1 (list 1
                           (lambda () nov-file-name)
                           (lambda () (cdr (assoc 'title nov-metadata)))
                           "org-remark-nov-file"))
         (headline-2 (list 2
                           #'org-remark-get-epub-source
                           #'org-remark-nov-get-epub-document-title
                           org-remark-prop-source-file))
         (headline-constructors (list headline-1 headline-2)))
    headline-constructors))

(defun org-remark-nov-highlight-new-headline-maybe (source-buf notes-buf)
  (let* ((headline-1 (list 1
                           (lambda () nov-file-name)
                           (lambda () (cdr (assoc 'title nov-metadata)))
                           "org-remark-nov-file"))
         (headline-2 (list 2
                           #'org-remark-get-epub-source
                           #'org-remark-nov-get-epub-document-title
                           org-remark-prop-source-file))
         (headline-constructs (list headline-1 headline-2)))
    (org-remark-highlight-new-headline-maybe headline-constructs source-buf notes-buf)))

;; navigate from notes to document
(defun test/find-nov-file-buffer ()
  (interactive)
  (when-let* ((pos (point))
              (base-buf (or (buffer-base-buffer) (current-buffer)))
              (link (with-current-buffer base-buf
                      (org-entry-get pos "org-remark-link")))
              (path (with-temp-buffer
                      (insert link) (beginning-of-buffer)
                      (org-element-property :path (org-element-context))))
              (file (if (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path) ;; nov only
                        (match-string 1 path)                                       ;; nov only
                      (error "Invalid nov.el link")))                               ;; nov only
              (index (string-to-number (match-string 2 path)))                      ;; nov only
              (point (string-to-number (match-string 3 path)))                      ;; nov only
              (source-buffers (with-current-buffer base-buf
                                org-remark-notes-source-buffers))
              (epub-buffer (seq-find
                            (lambda (buf) (and (buffer-live-p buf)
                                               (with-current-buffer buf
                                                 (string= file nov-file-name))))    ;; nov only
                            source-buffers)))
    (pop-to-buffer epub-buffer)
    ;; If FILE is nil, the current buffer is used.
    (nov--find-file nil index point)))

(provide 'org-remark-nov)
;;; org-remark-nov.el ends here
