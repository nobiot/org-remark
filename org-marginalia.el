;;; org-marginalia.el --- highlight & annotate       -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-marginalia
;; Version: 0.0.6
;; Last Modified: 2021-08-18
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, margin-notes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you write margin notes (marginalia) for any text file in
;; a separate Org file.

;; Refer to README.org and docstring of variables and functions.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-marginalia-global-tracking)
(declare-function org-collect-keywords 'org)

;;;; Customization

(defgroup org-marginalia nil
  "Write margin notes (marginalia) for any text file in a
separate Org file"
  :group 'org
  :prefix "org-marginalia-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-marginalia"))

(defface org-marginalia-highlighter
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#001904")
    (t
     :inherit highlight))
  "Face for highlighters."
  :group 'org-marginalia)

(defcustom org-marginalia-notes-file-path "marginalia.org"
  "Specify the file path for the marginalia.org file.
The default is \"./marginalia.org\", thus one marginalia file per directory.
Ensure that it is an Org file."
  :type 'file
  :group 'org-marginalia)

(defcustom org-marginalia-use-org-id t
  "Define if Org-marginalia use Org-ID to link back to the main note."
  :type 'boolean
  :group 'org-marginalia)

;;;; Variables

(defvar-local org-marginalia-loaded nil)

(defvar-local org-marginalia-highlights '()
  "Keep track of all the highlights.
It is a local variable and is a list of multiple highlights.
Each element is an overlay representing a highlighted text region.

On save-buffer each highlight will be persisted in the marginalia file
(defined by `org-marginalia-notes-file-path').")

(defvar org-marginalia-last-notes-buffer nil
  "Stores the cloned indirect buffer for the margin notes.
It is meant to exist only one of these in each Emacs session.")

;; Const for the names of properties in Org Mode
(defconst org-marginalia-prop-id "marginalia-id")
(defconst org-marginalia-prop-source-file "marginalia-source-file")
(defconst org-marginalia-prop-source-beg "marginalia-source-beg")
(defconst org-marginalia-prop-source-end "marginalia-source-end")

;;; Macros
(defmacro org-marginalia-make-pen (label face &rest properties)
  "Create a user-defined highlight function.
LABEL is the name of the highlight. The function will be called
`ov-highlight-LABEL', and it will apply FACE to the selected
region. FACE can be an anonymous face, or a function that returns
one. PROPERTIES is a list of symbols and properties. If the
property is a function, it will be evaluated. The function takes
no arguments."
  `(defun ,(intern (format "org-marginalia-mark-%s" label)) (beg end &optional id)
     ,(format "Apply the face %S to the region selected by BEG and END" face)
     (interactive "r")
     ;; (flyspell-delete-region-overlays beg end)
     (unless org-marginalia-mode (org-marginalia-mode +1))
     ;; UUID is too long; does not have to be the full length
     (when (not id) (setq id (substring (org-id-uuid) 0 8)))
     ;; Add highlight to the text
     (org-with-wide-buffer
      (let ((face ,face)
	    (properties (quote ,properties))
            (ov (make-overlay beg end nil 'FRONT-ADVANCE)))
        (overlay-put ov 'face face)
        (while properties
	  (setq prop (pop properties)
		val (pop properties))
	  (overlay-put ov prop val))
        (overlay-put ov 'org-marginalia-id id)
        ;; Keep track it in a local variable. It's a list overlays, guranteed to
        ;; contain only marginalia overlays as opposed to the one returned by
        ;; `overlay-lists'
        ;; TODO Do we need to consider this for overlay?
        ;; `set-marker-insertion-type' to
        ;; set the type t is necessary to move the cursor in sync with the
        ;; font-lock-face property of the text property.
        (push ov org-marginalia-highlights)
        ;; Adding overlay does not set the buffer modified.
        ;; It's more fluid with save operation.
        ;; You cannot use `undo' to undo highlighter.
        (deactivate-mark)
        (unless (buffer-modified-p) (set-buffer-modified-p t))))
     (org-marginalia-housekeep)
     (org-marginalia-sort-highlights-list)))

(org-marginalia-make-pen "yellow" '(:background "Yellow") "category" "important" "org-marginalia-label" "yellow")

;;;; Commands

;;;###autoload
(define-minor-mode org-marginalia-mode
    "Highlight text, write margin notes for any text file in Org Mode.
This is a local minor-mode.

On activation, it loads your saved highlights from the marginalia
file and enables automatic saving of highlights.

The automatic saving is achieved via function
`org-marginalia-save' added to `after-save-hook'.

On deactivation, it removes all the overlays and stops tracking
the highlights in this buffer by setting variable
`org-marginalia-highlights' to nil. Be careful of behavior, if
you still wish to retain the locations of highlights.

It is recommended to use `org-marginalia-toggle' if you wish to
temporarily hide highlights in the current buffer. It keeps
`org-marginalia-highlights' unchanged.

While the tracking of highlights is stopped,
editing the buffer will likely result in mismatch between the
saved highlights' locations and the current buffer's text
content.

Highlights tracked by variable `org-marginalia-highlights' cannot
persist when you kill the buffer or quit Emacs. When you
re-launch Emacs and visit the same file, ensure to turn on
`org-marginalia-mode' to load the highlights from the marginalia
file. `org-marginalia-global-tracking-mode' can automate this.

\\{org-marginalia-mode-map}"
    :init-value nil
    :lighter " marginalia"
    :global nil
    :keymap (let ((map (make-sparse-keymap)))
              map)
    (cond
     (org-marginalia-mode
      ;; Activate
      (org-marginalia-load)
      (add-hook 'after-save-hook #'org-marginalia-save nil t)
      (add-hook 'kill-buffer-hook #'org-marginalia-tracking-save nil t))
     (t
      ;; Deactivate
      (when org-marginalia-highlights
	(dolist (highlight org-marginalia-highlights)
	  (delete-overlay highlight)))
      (setq org-marginalia-highlights nil)
      (setq org-marginalia-loaded nil)
      (org-marginalia-tracking-save)
      (remove-hook 'after-save-hook #'org-marginalia-save t)
      (remove-hook 'kill-buffer-hook #'org-marginalia-tracking-save t))))

;;;###autoload
(defun org-marginalia-mark (beg end &optional id)
  "Highlight the selected region (BEG and END).
When used interactively. it will generate a new ID, always
assuming it is a new highlighted text region, and start tracking
the highlight's location, so that you can edit the text around.

It will not create a marginalia entry yet. Save the current
buffer or call `org-marginalia-save' to create a new entry (it is
automatic with `after-save-hook').

When this function is called from Elisp, ID can be optionally
passed. If so, no new ID gets generated.

Every highlighted text region in the current buffer is tracked by
local variable `org-marginalia-highlights'. The highlights are
sorted in the ascending order; this is a property of the variable
used for `org-marginalia-next' and `org-marginalia-prev'."
  (interactive "r")
  ;; Ensure to turn on the local minor mode
  (unless org-marginalia-mode (org-marginalia-mode +1))
  ;; UUID is too long; does not have to be the full length
  (when (not id) (setq id (substring (org-id-uuid) 0 8)))
  ;; Add highlight to the text
  (org-with-wide-buffer
   (let ((ov (make-overlay beg end nil 'FRONT-ADVANCE)))
     (overlay-put ov 'face 'org-marginalia-highlighter)
     (overlay-put ov 'org-marginalia-id id)
     ;; Keep track it in a local variable. It's a list overlays, guranteed to
     ;; contain only marginalia overlays as opposed to the one returned by
     ;; `overlay-lists'

     ;; TODO Do we need to consider this for overlay?
     ;; `set-marker-insertion-type' to
     ;; set the type t is necessary to move the cursor in sync with the
     ;; font-lock-face property of the text property.
     (push ov org-marginalia-highlights)
     ;; Adding overlay does not set the buffer modified.
     ;; It's more fluid with save operation.
     ;; You cannot use `undo' to undo highlighter.
     (deactivate-mark)
     (unless (buffer-modified-p) (set-buffer-modified-p t))))
  (org-marginalia-housekeep)
  (org-marginalia-sort-highlights-list))

;;;###autoload
(defun org-marginalia-load ()
  "Open the marginalia file and load the saved highlights onto current buffer.
If there is no margin notes for it, it will output a message in
the echo.

Highlights tracked locally by variable
`org-marginalia-highlights' cannot persist when you kill the
buffer or quit Emacs. When you re-launch Emacs, ensure to turn on
`org-marginalia-mode' to load the highlights.
`org-marginalia-global-tracking-mode' can automate this."
  (interactive)
  ;; Open the marginalia file
  ;; Read all the positions
  (unless org-marginalia-mode (org-marginalia-mode +1))
  (unless org-marginalia-loaded
    (when-let* ((filename (buffer-file-name))
		(margin-buf (find-file-noselect org-marginalia-notes-file-path))
		(source-path (abbreviate-file-name filename)))
      ;; Get hilights: each highlighlight is stored as an alist
      ;; (id beg . end)
      ;; TODO check if there is any relevant notes for the current file
      (let ((highlights '()))
	(with-current-buffer margin-buf
          (org-with-wide-buffer
           (let ((heading (org-find-property
			   org-marginalia-prop-source-file source-path)))
             (if (not heading)
		 (message "No marginalia written for %s." source-path)
               (goto-char (org-find-property
			   org-marginalia-prop-source-file source-path))
               ;; Narrow to only subtree for a single file
               ;; `org-find-property' ensures that it is the beginning of H1
               (org-narrow-to-subtree)
               ;; It's important that the headline levels are fixed
               ;; H1: File
               ;; H2: Higlighted region (each one has a dedicated H2 subtree)
               (while (not (org-next-visible-heading 1))
		 (when-let ((id (org-entry-get (point) "marginalia-id"))
                            (beg (string-to-number
				  (org-entry-get (point)
						 "marginalia-source-beg")))
                            (end (string-to-number
				  (org-entry-get (point)
						 "marginalia-source-end"))))
                   (push (cons id (cons beg end)) highlights)))))))
	;; Back to the current buffer
	;; Look highilights and add highlights to the current buffer
	(dolist (highlight highlights)
          (let ((id (car highlight))
		(beg (car (cdr highlight)))
		(end (cdr (cdr highlight))))
            (org-marginalia-mark beg end id)))))
    ;; Tracking
    (when org-marginalia-global-tracking-mode
      (add-to-list 'org-marginalia-files-tracked
		   (abbreviate-file-name (buffer-file-name))))
    (setq org-marginalia-loaded t)))

(defun org-marginalia-save ()
  "Save all the highlights tracked in current buffer to marginalia file.
The marginalia file is defined in `org-marginalia-notes-file-path' variable.

This funcion is automatically called when you save the buffer. This is
achieved via `after-save-hook' (added via `org-marginalia-mode' when you
activate the minor mode).

`org-marginalia-highlights' is the local variable that tracks every highlight
in the current buffer. Each highlight is represented by an overlay."

  (interactive)
  (let* ((filename (buffer-file-name))
         (source-path (abbreviate-file-name filename))
         (title (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                    (file-name-sans-extension
		     (file-name-nondirectory (buffer-file-name))))))
    (org-marginalia-housekeep)
    (org-marginalia-sort-highlights-list)
    (dolist (h org-marginalia-highlights)
      (let ((orgid (and org-marginalia-use-org-id
			(org-entry-get (overlay-start h) "ID" 'INHERIT))))
	(org-marginalia-save-single-highlight h title source-path orgid)))
    ;; Tracking
    (when org-marginalia-global-tracking-mode
      (add-to-list 'org-marginalia-files-tracked
		   (abbreviate-file-name (buffer-file-name))))))

(defun org-marginalia-open (point)
  "Open the margin notes at POINT, narrowed to the relevant headline.
It creates a cloned indirect buffer of the marginalia file
\(`org-marginalia-notes-file-path'\). You can edit the margin notes as a
normal Org file. Once you have done editing, you can simply save and close the
buffer (kill or close the window) as per your normal workflow.

This package ensures that there is only one cloned buffer for marginalia by
tracking it."
  (interactive "d")
  (when (buffer-live-p org-marginalia-last-notes-buffer)
    (kill-buffer org-marginalia-last-notes-buffer))
  (when-let ((id (get-char-property point 'org-marginalia-id))
             (ibuf (make-indirect-buffer
                    (find-file-noselect org-marginalia-notes-file-path)
		    "*marginalia*" 'clone)))
    (setq org-marginalia-last-notes-buffer ibuf)
    (org-switch-to-buffer-other-window ibuf)
    (widen)(goto-char (point-min))
    (when (org-find-property org-marginalia-prop-id id)
      (goto-char (org-find-property org-marginalia-prop-id id))
      (org-narrow-to-subtree))))

(defun org-marginalia-remove (point &optional arg)
  "Remove the highlight at POINT.
It will remove the highlight and the properties from the
marginalia, but will keep the headline and notes. This is to
ensure to keep any notes you might have written intact.

You can let this command delete the entire heading subtree, along
with the notes you have written, for the highlight by pass a
universal argument with \\[universal-argument] (ARG). If you have
done so by error, you could still `undo' it in the marginalia
buffer"
  (interactive "d\nP")
  ;; TODO There may be multple overlays
  (when-let* ((id (get-char-property point 'org-marginalia-id)))
    ;; Remove the highlight overlay and id
    (dolist (ov (overlays-at (point)))
      ;; Remove the element in the variable org-marginalia-highlights
      (when (overlay-get ov 'org-marginalia-id)
	(delete ov org-marginalia-highlights)
	(delete-overlay ov)))
    (org-marginalia-sort-highlights-list)
    ;; Update the marginalia note file accordingly
    (org-marginalia-remove-marginalia id arg)
    t))

(defun org-marginalia-next ()
  "Look at the current point and move to the next highlight, if any.
If there is none below the point but there is a highlight in the
buffer, go back to the first one.

After the point has moved to the next highlight, this command
lets you move further by re-entering only the last letter like
this example:

   C-n \] \] \] \] \] \(assuming this command is bound to C-n\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-marginalia-prev', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (if (not org-marginalia-highlights)
      (progn (message "No highlights present in this buffer.") nil)
    (let ((p (org-marginalia-find-next-highlight)))
      (if p (progn
              (goto-char p)
              ;; Setup the overriding keymap.
              (unless overriding-terminal-local-map
                (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
                      (map (cdr org-marginalia-mode-map)))
                  (when (< 0 (length prefix-keys))
                    (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
                    (setq map (cdr-safe map))
                    (when (keymapp map) (set-transient-map map t)))))
              t)
        (message "Nothing done. No more visible highlights exist") nil))))

(defun org-marginalia-prev ()
  "Look at the current point, and move to the previous highlight, if any.
If there is none above the point, but there is a highlight in the
buffer, go back to the last one.

After the point has moved to the previous highlight, this command
lets you move further by re-entering only the last letter like
this example:

   C-n \[ \[ \[ \[ \[ \(assuming this command is bound to C-n \[\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-marginalia-next', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (if (not org-marginalia-highlights)
      (progn (message "No highlights present in this buffer.") nil)
    (let ((p (org-marginalia-find-prev-highlight)))
      (if p (progn
              (goto-char p)
              ;; Setup the overriding keymap.
              (unless overriding-terminal-local-map
                (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
                      (map (cdr org-marginalia-mode-map)))
                  (when (< 0 (length prefix-keys))
                    (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
                    (setq map (cdr-safe map))
                    (when (keymapp map) (set-transient-map map t)))))
              t)
        (message "Nothing done. No more visible highlights exist") nil))))

(defun org-marginalia-toggle ()
  "Toggle showing/hiding of highlighters in current buffer.
It only affects the display of the highlighters. Their locations
are still kept tracked; upon buffer-save the correct locations
are still recorded in the marginalia file."
  (interactive)
  (when-let ((highlights org-marginalia-highlights))
    ;; Check the first highlight in the buffer
    ;; If it's hidden, all hidden. Show them.
    ;; If not, all shown. Hide them.
    (if-let* ((beg (overlay-start (nth 0 highlights)))
              (hidden-p (get-char-property beg 'org-marginalia-hidden)))
        (org-marginalia-show)
      (org-marginalia-hide))
    t))

;;;; Functions

;;;;; Private

(defun org-marginalia-save-single-highlight (highlight title path orgid)
  "Save a single HIGHLIGHT in the marginalia file with properties.
The marginalia file is specified by PATH. If headline with
the same ID already exists, update it based on the new highlight
position and highlighted text as TITLE. If it is a new highlight,
create a new headline at the end of the buffer.

ORGID can be passed to this function. If user option
`org-marginalia-use-org-id' is non-nil, this function will create
a link back to the source via an Org-ID link instead of the
normal file link.

When a new marginalia file is created and
`org-marginalia-use-org-id' is non-nil, this function adds ID
property to the file level. This is mainly to support Org-roam's
backlink feature for marginalia files."
  (let* ((beg (overlay-start highlight))
         (end (overlay-end highlight))
	 (id (overlay-get highlight 'org-marginalia-id))
         ;;`org-with-wide-buffer is a macro that should work for non-Org file'
         (text (org-with-wide-buffer (buffer-substring-no-properties beg end)))
         (props (overlay-properties highlight))
         (note-props nil))
    (while props
      (let ((p (pop props))
            (v (pop props)))
        (when (and (stringp p)
                   (or (string-equal "category" (downcase p))
                       (and (>= (length p) 15)
                            (string-equal "org-marginalia-" (downcase (substring p 0 15))))))
          (push v note-props)
          (push p note-props))))
    ;; TODO Want to add a check if save is applicable here.
    (with-current-buffer (find-file-noselect org-marginalia-notes-file-path)
      ;; If it is a new empty marginalia file
      (when (and (org-marginalia-empty-buffer-p) org-marginalia-use-org-id)
	(org-id-get-create))
      (org-with-wide-buffer
       (let ((file-headline (org-find-property
			     org-marginalia-prop-source-file path))
             (id-headline (org-find-property org-marginalia-prop-id id)))
         (unless file-headline
           ;; If file-headline does not exist, create one at the bottom
           (goto-char (point-max))
           ;; Ensure to be in the beginning of line to add a new headline
           (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
           (insert (concat "* " title "\n"))
           (org-set-property org-marginalia-prop-source-file path))
         (cond (id-headline
                (goto-char id-headline)
                ;; Update the existing headline and position properties
                (org-edit-headline text)
                (org-marginalia-notes-set-properties nil beg end note-props)
                (org-set-property org-marginalia-prop-source-beg
				  (number-to-string beg))
                (org-set-property org-marginalia-prop-source-end
				  (number-to-string end)))
               (t ;; No headline with the ID property. Create one
                (when-let ((p (org-find-property
			       org-marginalia-prop-source-file path)))
                  (goto-char p))
                (org-narrow-to-subtree)
                (goto-char (point-max))
                ;; Ensure to be in the beginning of line to add a new headline
                (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
                ;; Create a headline
                ;; Add a properties
                (insert (concat "** " text "\n"))
                (org-marginalia-notes-set-properties id beg end note-props)
		(if (and org-marginalia-use-org-id orgid)
		    (insert (concat "[[id:" orgid "]" "[" title "]]"))
		  (insert (concat "[[file:" path "]" "[" title "]]")))))))
      (when (buffer-modified-p) (save-buffer) t))))

(defun org-marginalia-notes-set-properties (id beg end &optional props)
  "."
  (when id (org-set-property org-marginalia-prop-id id))
  (org-set-property org-marginalia-prop-source-beg
		    (number-to-string beg))
  (org-set-property org-marginalia-prop-source-end
		    (number-to-string end))
  (while props
    ;; Upcase for the property names for Org It seems CATEGORY needs to be
    ;; uppercase for sparse tree search to work properly.
    (let ((p (upcase (pop props)))
          (v (pop props)))
      (org-set-property p v))))

(defun org-marginalia-list-highlights-positions (&optional reverse)
  "Return list of beg points of highlights in this buffer.
By default, the list is in ascending order.
If REVERSE is non-nil, return list in the descending order.

It also checks if the position is visible or not. Return only
visible ones.

If none, return nil."
  (when org-marginalia-highlights
    (let ((list org-marginalia-highlights))
      (setq list (mapcar
                  (lambda (h)
                    (let ((p (overlay-start h)))
                      ;; Checking if the p is visible or not
                      (if (or
                           (> p (point-max))
                           (< p (point-min))
                           ;; When the highlight is wihtin a visible folded
                           ;; area, this function returns 'outline
                           (org-invisible-p p))
                          nil p)))
                  list))
      (setq list (remove nil list))
      (when list
        (if reverse (reverse list) list)))))

(defun org-marginalia-sort-highlights-list ()
  "Utility function to sort `org-marginalia-sort-highlights'.
It checks if there is any element exists for `org-marginalia-highlights'.
Instead of receiving it as an arg, it assumes its existence. It
also distructively updates `org-marginalia-highlights'.
It returns t when sorting is done."
  (when org-marginalia-highlights
    (setq org-marginalia-highlights
	  (seq-sort-by (lambda (ov) (overlay-start ov))
		       #'<
		       org-marginalia-highlights))
    t))

(defun org-marginalia-find-next-highlight ()
  "Return the beg point of the next highlight.
Look through `org-marginalia-highlights' list."
  (when-let ((points (org-marginalia-list-highlights-positions)))
      ;; Find the first occurance of p > (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-marginalia-highlights' is sorted in the ascending order (it is).
    (seq-find (lambda (p) (> p (point))) points (nth 0 points))))

(defun org-marginalia-find-prev-highlight ()
  "Return the beg point of the previous highlight.
Look through `org-marginalia-highlights' list (in descending order)."
  (when-let ((points (org-marginalia-list-highlights-positions 'reverse)))
      ;; Find the first occurance of p < (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-marginalia-highlights' is sorted in the descending order .
    (seq-find (lambda (p) (< p (point))) points (nth 0 points))))

(defun org-marginalia-hide ()
  "Hide highlighters.
It will remove the font-lock-face of all the highlights, and add
'org-marginalia-hidden property with value 't. It does not check the current
hidden state, thus not interactive. Use `org-marginalia-toggle'
command to manually toggle the show/hide state."
  (when-let ((highlights org-marginalia-highlights))
    (dolist (highlight highlights)
        (overlay-put highlight 'face nil)
        (overlay-put highlight 'org-marginalia-hidden t))
    t))

(defun org-marginalia-show ()
  "Show highlights.
It adds the font-lock-face to all the highlighted text regions.
It does not check the current hidden state, thus not interactive.
Use `org-marginalia-toggle' command to manually toggle the
show/hide state."
  (when-let ((highlights org-marginalia-highlights))
    (dolist (highlight highlights)
        (overlay-put highlight 'org-marginalia-hidden nil)
        (overlay-put highlight 'face 'org-marginalia-highlighter))
    t))

(defun org-marginalia-remove-marginalia (id &optional delete-notes)
  "Remove marginalia entry for the ID for the current buffer.
By default, it deletes only the properties of the entry keeping
the headline intact. You can pass DELETE-NOTES and delete the all
notes of the entry."
  (with-current-buffer (find-file-noselect org-marginalia-notes-file-path)
      (org-with-wide-buffer
       (when-let ((id-headline (org-find-property org-marginalia-prop-id id)))
         (goto-char id-headline)
	 (org-narrow-to-subtree)
         (org-delete-property org-marginalia-prop-id)
         (org-delete-property org-marginalia-prop-source-beg)
         (org-delete-property org-marginalia-prop-source-end)
         (when delete-notes
           ;; TODO I would love to add the y-n prompt if there is any notes written
           (delete-region (point-min)(point-max))
           (message "Deleted the marginal notes."))
	 (when (buffer-modified-p) (save-buffer))))
      t))

(defun org-marginalia-housekeep ()
  "Housekeep the internal variable `org-marginalia-highlights'.
This is a private function; housekeep is automatically done on
save.

Case 1. Both start and end of an overlay are identical

        This should not happen when you manually mark a text
        region. A typical cause of this case is when you delete a
        region that contains a highlight overlay.

Case 2. The overlay points to no buffer

        This case happens when overlay is deleted by
        `overlay-delete' but the variable not cleared."
  (dolist (ov org-marginalia-highlights)
    ;; Both start and end of an overlay are indentical; this should not happen
    ;; when you manually mark a text region. A typical cause of this case is
    ;; when you delete a region that contains a highlight overlay.
    (when (and (overlay-buffer ov)
	       (= (overlay-start ov) (overlay-end ov)))
      (org-marginalia-remove-marginalia (overlay-get ov 'org-marginalia-id))
      (delete-overlay ov))
    (unless (overlay-buffer ov)
      (setq org-marginalia-highlights (delete ov org-marginalia-highlights))))
  t)

(defun org-marginalia-empty-buffer-p ()
  "Return non-nil when the current buffer is empty."
  (save-excursion
    (goto-char (point-max))
    (= 1 (point))))

;;;; Footer

(provide 'org-marginalia)

;;; org-marginalia.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; eval: (setq-local org-marginalia-notes-file-path "README.org")
;; End:
