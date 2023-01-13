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
