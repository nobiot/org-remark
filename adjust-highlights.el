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
       (goto-char beg) (backward-paragraph 2) (setq paragraph-beg (point))
       (goto-char beg) (forward-paragraph 2) (setq paragraph-end (point))
       (goto-char (point-min))
       (when (or (re-search-forward text paragraph-end :noerror)
                 (re-search-backward text paragraph-beg :noerror))
         (move-overlay highlight (match-beginning 0) (match-end 0)))))))
