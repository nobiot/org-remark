;;; Commentary:

;;; Code

(declare-function org-marginalia-mode 'org-marginalia)

(defgroup org-marginalia-global-tracking nil
  "Write margin notes (marginalia) for any text file in a
separate Org file"
  :group 'org-marginalia
  :prefix "org-marginalia-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-marginalia"))

(defcustom org-marginalia-tracking-file
  (locate-user-emacs-file ".org-marginalia-tracking" nil)
  "File name where the files `org-marginalia' tracks is saved.
When `org-marginalia-global-tracking-mode' is active, opening a file
saved in `org-marginalia-tracking-file' automatically loads highlights."
  :group 'org-marginalia-global-tracking
  :type 'file)

(defvar org-marginalia-tracking-file-loaded nil)

(defvar org-marginalia-files-tracked nil)

;;;###autoload
(define-minor-mode org-marginalia-global-tracking-mode
  "Global mode. When enabled, check files saved in
`org-marginalia-tracking-file' and opening them automatically
activates `org-marginalia-mode' locally for the file opened."
  :init-value nil
  :lighter " marginalia-tracking"
  :global t
  (cond
   (org-marginalia-global-tracking-mode
    ;; Activate
    (when (and (not org-marginalia-tracking-file-loaded)
	       (file-exists-p org-marginalia-tracking-file))
      (org-marginalia-tracking-load))
    (add-hook 'find-file-hook #'org-marginalia-tracking-auto-on)
    (add-hook 'kill-emacs-hook #'org-marginalia-tracking-save))
   (t
    ;; dactivate
    (setq org-marginalia-files-tracked nil)
    (setq org-marginalia-tracking-file-loaded nil)
    (remove-hook 'find-file-hook #'org-marginalia-tracking-auto-on)
    (remove-hook 'kill-emacs-hook #'org-marginalia-tracking-save))))

(defun org-marginalia-tracking-auto-on ()
  "."
  (when (and org-marginalia-files-tracked
	     (member (abbreviate-file-name (buffer-file-name))
		     org-marginalia-files-tracked))
    (unless (featurep 'org-marginalia) (require 'org-marginalia))
    (org-marginalia-mode +1)))

(defun org-marginalia-tracking-load ()
  ".
Each line.  It loads regardless of `org-marginalia-tracking-file';
if already loaded this function reloads."
  (with-temp-buffer
    (condition-case nil
	(progn
	  (insert-file-contents org-marginalia-tracking-file)
	  (setq org-marginalia-files-tracked
		(split-string (buffer-string) "\n"))
          (setq org-marginalia-tracking-file-loaded t)))))

(defun org-marginalia-tracking-save ()
  "."
  (interactive)
  (when org-marginalia-files-tracked
    (with-temp-file org-marginalia-tracking-file
      (insert (mapconcat 'identity org-marginalia-files-tracked "\n")))))

(provide 'org-marginalia-global-tracking)

;;; org-marginalia-global-tracking.el ends here
