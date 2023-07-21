;;; org-remark-info.el --- Support Org-roam with Info-mode -*- lexical-binding: t; -*-

;; URL: https://github.com/nobiot/org-remark
;; Created: 16 January 2023
;; Last modified: 21 July 2023

;;; Commentary:

;;; Code:

(require 'ol-info)
(require 'info)
(require 'org-remark-global-tracking)
(defvar org-remark-prop-source-file)
(declare-function org-remark-highlights-load "org-remark")

;;;###autoload
(define-minor-mode org-remark-info-mode
  "Enable Org-remark to work with `Info-mode' for Info documentation reader."
  :global t
  :group 'org-remark
  (if org-remark-info-mode
      ;; Enable
      (progn
          (add-hook 'org-remark-source-find-file-name-functions
                    #'org-remark-info-get-node)
          (add-hook 'org-remark-highlight-link-to-source-functions
                    #'org-remark-info-link)
          (advice-add #'Info-find-node :after #'org-remark-info-highlights-load))
    ;; Disable
    (remove-hook 'org-remark-source-find-file-name-functions
                 #'org-remark-info-get-node)
    (remove-hook 'org-remark-highlight-link-to-source-functions
                 #'org-remark-info-link)
    (advice-remove #'Info-find-node #'org-remark-info-highlights-load)))

(defun org-remark-info-highlights-load (&rest _args)
  "Wrapper for `org-remark-highlights-load'.
It is necessary as this function is intended to be used as part
of advice for `Info-goto-node', which gets arguments passed to
it. `org-remark-highlights-load' should be called with no
arguments for the purpose of `org-remark-info-mode'."
  (org-remark-highlights-load))

(defun org-remark-info-get-node ()
  "Return the current Info file/node."
  (when (eq major-mode 'Info-mode)
    (concat (file-name-nondirectory Info-current-file)
            "/"
            Info-current-node)))

(defun org-remark-info-link (_filname _point)
  "Return \"info:\" link with current point in `Info-mode' buffer.

This function only works when the mode is `Info-mode'.

Assume the point is on the highlight in source Info document
buffer and `ol-info' is loaded. The latter is necessary for
`org-store-link' to work wiht Info buffer."
  (when (eq major-mode 'Info-mode)
    (org-store-link nil nil)))

(cl-defmethod org-remark-highlight-get-constructors (&context (major-mode Info-mode))
  "Construct lists for creating MAJOR-MODE specific hierarchy.

This method is for `Info-mode'.

Return the value in a alist like this:

   (SOURCE-FILENAME-FN TITLE-FN PROP-TO-FIND)"
  (let* ((headline-1 (list
                      ;; SOURCE-FILENAME-FN
                      ;; Don't include the full directory path for Info
                      ;; node. This may change when Emacs version or
                      ;; package version changes.
                      (lambda () (file-name-nondirectory Info-current-file))
                      ;; TITLE-FN
                      (lambda () (file-name-nondirectory Info-current-file))
                      ;; PROP-TO-FIND
                      "org-remark-info-file"))
         (headline-2 (list
                      ;; SOURCE-FILENAME-FN
                      #'org-remark-info-get-node
                      ;; TITLE-FN
                      (lambda () Info-current-node)
                      ;; PROP-TO-FIND
                      org-remark-prop-source-file))
         (headline-constructors (list headline-1 headline-2)))
    headline-constructors))

(provide 'org-remark-info)
;;; org-remark-info.el ends here
