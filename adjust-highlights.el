;; Load works. but need one for sync. Need to re-think
;; ' and ’ are different in regex of course.

;; This is probably not very good for text that you change; and change the highlights.
;; if you change it, this will bring it back to the "original".
(defun test/move-highlight (highlight text)
  (let* ((beg (overlay-start highlight))
         (end (overlay-end highlight))
         (paragraph-beg)(paragraph-end)
         ;; Cater to the case when the text is divided by a \n
         ;; The regexp must look for space or \n
         (text (replace-regexp-in-string " " "\[ \n\]" text)))
    (org-with-wide-buffer
     (unless (string= (buffer-substring beg end) text)
       ;; Look at one paragraph ahead as it is possible that the
       ;; position has been displaced across a paragraph
       (goto-char beg) (backward-paragraph 2) (setq paragraph-beg (point))
       (goto-char beg) (forward-paragraph 2) (setq paragraph-end (point))
       (goto-char paragraph-beg)
       ;; Search from the beginning of the previous paragraph to the end
       ;; of next paragraph relative to the begining of the highlight
       ;; overlay; this way, you don't need to look forward and backward
       ;; separately.
       (when (re-search-forward text paragraph-end :noerror)
         (move-overlay highlight (match-beginning 0) (match-end 0)))))))

;; navigate from notes to document
(defun test/find-nov-file-buffer ()
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
