;;; org-remark-nov.el --- Compatibility with nov-mode -*- lexical-binding: t; -*-

;; URL: https://github.com/nobiot/org-remark
;; Created: 9 January 2023
;; Last modified: 24 June 2023

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
        (add-hook 'nov-post-html-render-hook #'org-remark-highlights-load)
        (add-to-list 'org-remark-notes-headline-functions
                     '(nov-mode . ((1 . org-remark-nov-highlight-add-book-headline-maybe)
                                   (2 . org-remark-nov-add-source-headline-maybe)))))
    ;; Disable
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'org-remark-get-epub-filename)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'org-remark-nov-link)
    (remove-hook 'org-remark-highlight-link-to-source-functions
              #'org-remark-nov-link)
    (remove-hook 'nov-post-html-render-hook #'org-remark-highlights-load)
    (setq org-remark-notes-headline-functions
          (assq-delete-all 'nov-mode org-remark-notes-headline-functions))))

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
    (let ((document-info-pair (aref nov-documents nov-documents-index)))
      (concat
       (symbol-name (car document-info-pair)) ;; id
       "/"
       (file-name-base (cdr document-info-pair)))))) ;; temp file

(defun org-remark-get-epub-filename ()
  "Return the path of the epub source from which the present session is initiated."
  (when (eq major-mode 'nov-mode)
    nov-file-name))

(defun org-remark-nov-link (_filname _point)
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

(defun org-remark-nov-add-source-headline-maybe (level source-buf notes-buf)
  "Add a new source headline if not yet present in NOTES-BUF.
Return the point of beginning of source headline regardless of it
being newly added or already present.

SOURCE-BUF is the source buffer that contains highlights.

Assume the current buffer is NOTES-BUF."
  (let (source-name title)
    (with-current-buffer source-buf
      (setq source-name (org-remark-get-epub-source))
      (setq title (org-remark-nov-get-epub-document-title)))
    (with-current-buffer notes-buf
      ;; Return the beginning point of the headline. Create if not present
      (or (org-find-property
           org-remark-prop-source-file source-name)
          (org-remark-new-headline
           level title (list org-remark-prop-source-file source-name))))))

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
