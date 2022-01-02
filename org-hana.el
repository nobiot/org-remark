;;; org-hana.el --- Highlight & ANnotate Any text file (HANA) -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-hana
;; Version: 0.0.7
;; Last modified: 02 January 2022
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, marginal-notes

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

;; This package lets you highlight and annote any text file in a separate Org
;; file.  Refer to README.org and docstring for detail.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-hana-global-tracking)
(declare-function org-collect-keywords "org")

;;;; Customization

(defgroup org-hana nil
  "Highlight and annote any text file in a separate Org file."
  :group 'org
  :prefix "org-hana-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-hana"))

(defface org-hana-highlighter
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#001904")
    (t
     :inherit highlight))
  "Face for the default highlighter pen.")

(defcustom org-hana-notes-file-path "margin-notes.org"
  "Specify the file path to store the location of highlights and annotations.
The default is one file per directory.  Ensure that it is an Org
file."
  :type 'file)

(defcustom org-hana-use-org-id t
  "Define if Org-HANA use Org-ID to link back to the main note."
  :type 'boolean)

;;;; Variables

(defvar-local org-hana-loaded nil
  "Indicate if hightlights have been loaded onto current buffer.")

(defvar-local org-hana-highlights '()
  "Keep track of all the highlights in current buffer.
It is a local variable and is a list of overlays.  Each overlay
represents a highlighted text region.

On `save-buffer' each highlight will be save in the notes file at
`org-hana-notes-file-path'.")

(defvar org-hana-last-notes-buffer nil
  "Stores the cloned indirect buffer visting the notes file.
It is meant to exist only one of these in each Emacs session.")

;; Const for the names of properties in Org Mode
;; Kept for backward compatiblity reasons
(defconst org-hana-prop-id "marginalia-id")
(defconst org-hana-prop-source-file "marginalia-source-file")
(defconst org-hana-prop-source-beg "marginalia-source-beg")
(defconst org-hana-prop-source-end "marginalia-source-end")

;;;; Commands

;;;###autoload
(define-minor-mode org-hana-mode
    "Highlight text, write margin notes for any text file in Org Mode.
This is a local minor-mode.

On activation, it loads your saved highlights from the marginalia
file and enables automatic saving of highlights.

The automatic saving is achieved via function
`org-hana-save' added to `after-save-hook'.

On deactivation, it removes all the overlays and stops tracking
the highlights in this buffer by setting variable
`org-hana-highlights' to nil.  Be careful of behavior, if
you still wish to retain the locations of highlights.

It is recommended to use `org-hana-toggle' if you wish to
temporarily hide highlights in the current buffer.  It keeps
`org-hana-highlights' unchanged.

While the tracking of highlights is stopped,
editing the buffer will likely result in mismatch between the
saved highlights' locations and the current buffer's text
content.

Highlights tracked by variable `org-hana-highlights' cannot
persist when you kill the buffer or quit Emacs.  When you
re-launch Emacs and visit the same file, ensure to turn on
`org-hana-mode' to load the highlights from the marginalia
file.  `org-hana-global-tracking-mode' can automate this.

\\{org-hana-mode-map}"
    :init-value nil
    :lighter " ❦"
    :global nil
    :keymap (let ((map (make-sparse-keymap)))
              map)
    (cond
     (org-hana-mode
      ;; Activate
      (org-hana-load)
      (add-hook 'after-save-hook #'org-hana-save nil t)
      (add-hook 'kill-buffer-hook #'org-hana-tracking-save nil t))
     (t
      ;; Deactivate
      (when org-hana-highlights
	(dolist (highlight org-hana-highlights)
	  (delete-overlay highlight)))
      (setq org-hana-highlights nil)
      (setq org-hana-loaded nil)
      (org-hana-tracking-save)
      (remove-hook 'after-save-hook #'org-hana-save t)
      (remove-hook 'kill-buffer-hook #'org-hana-tracking-save t))))

;;; `org-hana-create-pen' macro lets you create commands for different highlighter pens
;;; Org-HANA provides three default ones. See below after `org-hana-create-pen'
(defmacro org-hana-create-pen (&optional label face properties)
  "Create a user-defined highlighter function.
LABEL is the name of the highlighter.  The function will be called
`org-hana-mark-LABEL', or, when LABEL is nil, the default
`org-hana-mark'.

The highlighter function will apply FACE to the selected region.
FACE can be an anonymous face.  When it is nil, this macro uses
the default face `org-hana-highlight'.

PROPERTIES is a list of pairs of a symbol and value.  Each
highlighted text region will have a corresponding Org headline in
the notes file, and it can have properties from the highlighter
pen.  To do this, prefix property names with \"org-hana-\" or use
\"CATEGORY\"."
  `(defun ,(intern (or (when label (format "org-hana-mark-%s" label))
                       "org-hana-mark"))
       (beg end &optional id)
     ,(format "Apply the following face to the region selected by BEG and END.
%s

Following properties are also added to the notes file:
%S

When this function is used interactively. it will generate a new
ID, always assuming it is a new highlighted text region, and
start tracking the highlight's location, so that you can edit the
text around.

It will not create a marginalia entry yet. Save the current
buffer or call `org-hana-save' to create a new entry (it is
automatic with `after-save-hook').

When this function is called from Elisp, ID can be optionally
passed. If so, no new ID gets generated.

Every highlighted text region in the current buffer is tracked by
local variable `org-hana-highlights'. The highlights are
sorted in the ascending order; this is a property of the variable
used for `org-hana-next' and `org-hana-prev'."
              (or face "`org-hana-highlight'") properties)
     (interactive "r")
     (org-hana-highlight beg end ,label ,face ,properties id)))

;; Don't use category (symbol) as a property -- it's a special one of text
;; properties. If you use it, the value also need to be a symbol; otherwise, you
;; will get an error. You can use CATEGORY (symbol and all uppercase).

(org-hana-create-pen) ;; create the default mark function with default face
                      ;; `org-hana-highlight' with no properties.
(org-hana-create-pen "orange"
                           '(:underline (:color "dark red" :style wave) :background "coral" :weight bold)
                           '(CATEGORY "must"))
(org-hana-create-pen "yellow"
                           '(:underline "gold" :background "lemon chiffon") '(CATEGORY "important"))

;;;###autoload
(defun org-hana-load ()
  "Visit `org-hana-notes-file' & load the saved highlights onto current buffer.
If there is no highligths or annotations for current buffer,
output a message in the echo.

Highlights tracked locally by variable `org-hana-highlights'
cannot persist when you kill current buffer or quit Emacs.  It is
recommended to set `org-hana-global-tracking-mode' in your
configuration.  It automatically turns on `org-hana-mode', which
runs `org-hana-load' for current buffer.

Otherwise, do not forget to turn on `org-hana-mode' manually to
load the highlights"
  (interactive)
  (unless org-hana-mode (org-hana-mode +1))
  (unless org-hana-loaded
    (when-let* ((filename (buffer-file-name))
		(notes-buf (find-file-noselect org-hana-notes-file-path))
		(source-path (abbreviate-file-name filename)))
      ;; Get hilights: each highlighlight is stored as an alist
      ;; TODO check if there is any relevant notes for the current file
      (let ((highlights '()))
	(with-current-buffer notes-buf
          (org-with-wide-buffer
           (let ((heading (org-find-property
			   org-hana-prop-source-file source-path)))
             (if (not heading)
		 (message "No highlights or annotations found for %s." source-path)
               (goto-char (org-find-property
			   org-hana-prop-source-file source-path))
               ;; Narrow to only subtree for a single file.  `org-find-property'
               ;; ensures that it is the beginning of a headline
               (org-narrow-to-subtree)
               ;; It's important that the headline levels are fixed
               ;; H1: File
               ;; H2: Higlighted region (each one has a dedicated H2 subtree)
               (while (not (org-next-visible-heading 1))
                 ;; The `or' for backward compatibility.  The consts are no
                 ;; longer used in the current version
		 (when-let ((id (or
                                 (org-entry-get (point) "org-hana-id")
                                 (org-entry-get (point) org-hana-prop-id)))
                            (beg (string-to-number
                                  (or
                                   (org-entry-get (point)
					          "org-hana-source-beg")
			           (org-entry-get (point)
					          org-hana-prop-source-beg))))
                            (end (string-to-number
                                  (or
                                   (org-entry-get (point)
					          "org-hana-source-end")
			           (org-entry-get (point)
					          org-hana-prop-source-end)))))
                   (push (list id
                               (cons beg end)
                               (org-entry-get (point) "org-hana-label"))
                         highlights)))))))
	;; Back to the current buffer
	;; Loop highilights and add them to the current buffer
        ;; Each highlight is a list in the following structure:
        ;;
        ;;     (id (beg . end) label)
        ;;
	(dolist (highlight highlights)
          (let ((id (car highlight))
		(beg (caadr highlight))
		(end (cdadr highlight))
                (label (caddr highlight)))
            (let ((fn (intern (concat "org-hana-mark-" label))))
              (unless (functionp fn) (setq fn #'org-hana-mark))
              (funcall fn beg end id))))))
    ;; Tracking
    (when org-hana-global-tracking-mode
      (add-to-list 'org-hana-files-tracked
		   (abbreviate-file-name (buffer-file-name))))
    (setq org-hana-loaded t)))

(defun org-hana-save ()
  "Save all the highlights tracked in current buffer to notes file.
Variable`org-hana-notes-file-path' defines the file path.

This funcion is automatically called when you save the current
buffer via `after-save-hook'.  This function is added to it by
function `org-hana-mode' when you activate the minor mode.

When `org-hana-global-tracking-mode' is on, this function also
adds current buffer to variable `org-hana-files-tracked' so that
next time you visit this file, `org-hana-mode' can be
automatically turned on to load the highlights.

`org-hana-highlights' is the local variable that tracks every highlight
in the current buffer.  Each highlight is represented by an overlay."
  (interactive)
  (let* ((filename (buffer-file-name))
         (source-path (abbreviate-file-name filename))
         (title (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                    (file-name-sans-extension
		     (file-name-nondirectory (buffer-file-name))))))
    (org-hana-housekeep)
    (org-hana-sort-highlights-list)
    (dolist (h org-hana-highlights)
      (let ((orgid (and org-hana-use-org-id
			(org-entry-get (overlay-start h) "ID" 'INHERIT))))
	(org-hana-save-single-highlight h title source-path orgid)))
    ;; Tracking
    (when org-hana-global-tracking-mode
      (add-to-list 'org-hana-files-tracked
		   (abbreviate-file-name (buffer-file-name))))))

(defun org-hana-open (point)
  "Open hightlight and annocation at POINT, narrowed to the relevant headline.
It creates a cloned indirect buffer of the notes file
\(`org-hana-notes-file-path'\).  You can edit notes file as a in
a normal Org file.  Once you have done editing, you can simply
save and kill the buffer.

This package ensures that there is only one cloned buffer for
notes file by tracking it."
  (interactive "d")
  (when (buffer-live-p org-hana-last-notes-buffer)
    (kill-buffer org-hana-last-notes-buffer))
  (when-let ((id (get-char-property point 'org-hana-id))
             (ibuf (make-indirect-buffer
                    (find-file-noselect org-hana-notes-file-path)
		    "*marginal notes*" 'clone)))
    (setq org-hana-last-notes-buffer ibuf)
    (org-switch-to-buffer-other-window ibuf)
    (widen)(goto-char (point-min))
    (when (org-find-property org-hana-prop-id id)
      (goto-char (org-find-property org-hana-prop-id id))
      (org-narrow-to-subtree))))

(defun org-hana-remove (point &optional arg)
  "Remove the highlight at POINT.
It will remove the highlight and the properties from the
marginalia, but will keep the headline and notes.  This is to
ensure to keep any notes you might have written intact.

You can let this command delete the entire heading subtree for
the highlight, along with the annotations you have written, pass
a universal argument with \\[universal-argument] \(ARG\).  If you
have done so by error, you could still `undo' it in the notes
buffer, but not in the current buffer as adding and removing overlays
are not part of the undo tree."
  (interactive "d\nP")
  ;; TODO There may be multiple overlays
  (when-let* ((id (get-char-property point 'org-hana-id)))
    ;; Remove the highlight overlay and id
    (dolist (ov (overlays-at (point)))
      ;; Remove the element in the variable org-hana-highlights
      (when (overlay-get ov 'org-hana-id)
	(delete ov org-hana-highlights)
	(delete-overlay ov)))
    (org-hana-housekeep)
    (org-hana-sort-highlights-list)
    ;; Update the notes file accordingly
    (org-hana-remove-single-highlight id arg)
    t))

(defun org-hana-next ()
  "Move to the next highlight, if any.
If there is none below the point but there is a highlight in the
buffer, cycle back to the first one.

After the point has moved to the next highlight, this command
lets you move further by re-entering only the last letter like
this example:

   C-n \] \] \] \] \] \(assuming this command is bound to C-n \]\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-hana-prev', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (if (not org-hana-highlights)
      (progn (message "No highlights present in this buffer.") nil)
    (let ((p (org-hana-find-next-highlight)))
      (if p (progn
              (goto-char p)
              ;; Setup the overriding keymap.
              (unless overriding-terminal-local-map
                (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
                      (map (cdr org-hana-mode-map)))
                  (when (< 0 (length prefix-keys))
                    (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
                    (setq map (cdr-safe map))
                    (when (keymapp map) (set-transient-map map t)))))
              t)
        (message "Nothing done. No more visible highlights exist") nil))))

(defun org-hana-prev ()
  "Move to the previous highlight, if any.
If there is none above the point, but there is a highlight in the
buffer, cycle back to the last one.

After the point has moved to the previous highlight, this command
lets you move further by re-entering only the last letter like
this example:

   C-n \[ \[ \[ \[ \[ \(assuming this command is bound to C-n \[\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-hana-next', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (if (not org-hana-highlights)
      (progn (message "No highlights present in this buffer.") nil)
    (let ((p (org-hana-find-prev-highlight)))
      (if p (progn
              (goto-char p)
              ;; Setup the overriding keymap.
              (unless overriding-terminal-local-map
                (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
                      (map (cdr org-hana-mode-map)))
                  (when (< 0 (length prefix-keys))
                    (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
                    (setq map (cdr-safe map))
                    (when (keymapp map) (set-transient-map map t)))))
              t)
        (message "Nothing done. No more visible highlights exist") nil))))

(defun org-hana-toggle ()
  "Toggle showing/hiding of highlighters in current buffer.
It only affects the display of the highlighters.  Their locations
are still kept tracked; upon buffer-save the correct locations
are still recorded in the marginalia file."
  (interactive)
  (when-let ((highlights org-hana-highlights))
    ;; Check the first highlight in the buffer
    ;; If it's hidden, all hidden. Show them.
    ;; If not, all shown. Hide them.
    (if-let* ((beg (overlay-start (nth 0 highlights)))
              (hidden-p (get-char-property beg 'org-hana-hidden)))
        (org-hana-show)
      (org-hana-hide))
    t))

;;;; Functions

;;;;; Private

(defun org-hana-highlight (beg end label face properties &optional id)
  "Highlight the selected region between BEG and END.
This function performs the main work for the command created via
`org-hana-create-pen'.

Create a user-defined highlighter function.
LABEL is the name of the highlighter.  The function will be called
`org-hana-mark-LABEL', or, when LABEL is nil, the default
`org-hana-mark'.

The highlighter function will apply FACE to the selected
region.  FACE can be an anonymous face.  When it is nil, this
macro uses the default face `org-hana-highlight'.

PROPERTIES is a list of pairs of a symbol and value.  Each
highlighted text region will have a corresponding Org headline in
the notes file, and it can have properties from the highlighter
pen.  To do this, prefix property names with \"org-hana-\" or use
\"CATEGORY\".

When this function is called from Elisp, ID can be optionally
passed. If so, no new ID gets generated."
  ;; Ensure to turn on the local minor mode
  (unless org-hana-mode (org-hana-mode +1))
  ;; UUID is too long; does not have to be the full length
  (when (not id) (setq id (substring (org-id-uuid) 0 8)))
  ;; Add highlight to the text
  (org-with-wide-buffer
   (let ((ov (make-overlay beg end nil 'FRONT-ADVANCE)))
     (overlay-put ov 'face (if face face 'org-hana-highlighter))
     (while properties
       (let ((prop (pop properties))
             (val (pop properties)))
         (overlay-put ov prop val)))
     (when label (overlay-put ov 'org-hana-label label))
     (overlay-put ov 'org-hana-id id)
     ;; Keep track of the overlay in a local variable. It's a list that is
     ;; guranteed to contain only org-hana overlays as opposed to the one
     ;; returned by `overlay-lists' that lists any overlays.
     (push ov org-hana-highlights)
     ;; Adding overlay to the buffer does not set the buffer modified. You
     ;; cannot use `undo' to undo highlighter, either.
     (deactivate-mark)
     (unless (buffer-modified-p) (restore-buffer-modified-p t))))
  (org-hana-housekeep)
  (org-hana-sort-highlights-list))

(defun org-hana-save-single-highlight (highlight title path orgid)
  "Save a single HIGHLIGHT in the notes file with properties.
The notes file is specified by PATH.

For the first highlight for current buffer, it will create a new
H1 headline for it.

If it is a new highlight, create a new H2 headline with the TITLE
as its headline text at the end of the H1 headline for the
current buffer.

If headline with the same ID already exists, update its position,
while keep the headline text intact, because the user might have
changed it to their needs.

ORGID can be passed to this function.  If user option
`org-hana-use-org-id' is non-nil, this function will create a
link back to the source via an Org-ID link with using ORGID
instead of the normal file link.

When a new notes file is created and `org-hana-use-org-id' is
non-nil, this function adds ID property to the file level.  This
can be helpful with other packages such as Org-roam's backlink
feature."
  (let* ((beg (overlay-start highlight))
         (end (overlay-end highlight))
	 (id (overlay-get highlight 'org-hana-id))
         ;;`org-with-wide-buffer is a macro that should work for non-Org file'
         (text (org-with-wide-buffer (buffer-substring-no-properties beg end)))
         (props (overlay-properties highlight)))
    ;; TODO Want to add a check if save is applicable here.
    (with-current-buffer (find-file-noselect org-hana-notes-file-path)
      ;; If it is a new empty marginalia file
      (when (and (org-hana-empty-buffer-p) org-hana-use-org-id)
	(org-id-get-create))
      (org-with-wide-buffer
       (let ((file-headline (or (org-find-property
			         org-hana-prop-source-file path)
                                (progn
                                  ;; If file-headline does not exist, create one at the bottom
                                  (goto-char (point-max))
                                  ;; Ensure to be in the beginning of line to add a new headline
                                  (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
                                  (insert (concat "* " title "\n"))
                                  (org-set-property org-hana-prop-source-file path)
                                  (org-up-heading-safe) (point))))
             (id-headline (org-find-property org-hana-prop-id id)))
         (if id-headline
             (progn
               (goto-char id-headline)
                ;; Update the existing headline and position properties
                ;; Don't update the headline text when it already exists
                ;; Let the user decide how to manage the headlines
                ;; (org-edit-headline text)
               (org-hana-notes-set-properties nil beg end props))
           ;; No headline with the marginal notes ID property. Create a new one
           ;; at the end of the file's entry
           (goto-char file-headline)
           (org-narrow-to-subtree)
           (goto-char (point-max))
           ;; Ensure to be in the beginning of line to add a new headline
           (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
           ;; Create a headline
           ;; Add a properties
           (insert (concat "** " text "\n"))
           (org-hana-notes-set-properties id beg end props)
	   (if (and org-hana-use-org-id orgid)
	       (insert (concat "[[id:" orgid "]" "[" title "]]"))
	     (insert (concat "[[file:" path "]" "[" title "]]"))))))
      (when (buffer-modified-p) (save-buffer) t))))

(defun org-hana-notes-set-properties (id beg end &optional props)
  "Set properties for the headline in the notes file.
Return t.

Minimal properties are:

- org-hana-id :: ID
- org-hana-source-beg :: BEG
- org-hana-source-end :: END

For PROPS, if the property name is CATEGORY \(case-sENsiTive\) or
prefixed with org-hana- set them to to headline's property
drawer."
  (when id (org-set-property org-hana-prop-id id))
  (org-set-property org-hana-prop-source-beg
		    (number-to-string beg))
  (org-set-property org-hana-prop-source-end
		    (number-to-string end))
  (while props
    (let ((p (pop props))
          (v (pop props)))
      (when (symbolp p) (setq p (symbol-name p)))
      (when (or (string-equal "CATEGORY" (upcase p))
                (and (>= (length p) 15)
                     (string-equal "org-hana-" (downcase (substring p 0 15)))))
        (org-set-property p v))))
  t)

(defun org-hana-list-highlights-positions (&optional reverse)
  "Return list of beg points of highlights in this buffer.
By default, the list is in ascending order.
If REVERSE is non-nil, return list in the descending order.

It also checks if the position is visible or not.  Return only
visible ones.

If none, return nil."
  (when org-hana-highlights
    (let ((list org-hana-highlights))
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

(defun org-hana-sort-highlights-list ()
  "Utility function to sort `org-hana-sort-highlights'.
It checks if there is any element exists for `org-hana-highlights'.
Instead of receiving it as an arg, it assumes its existence.  It
also distructively updates `org-hana-highlights'.
It returns t when sorting is done."
  (when org-hana-highlights
    (setq org-hana-highlights
	  (seq-sort-by (lambda (ov) (overlay-start ov))
		       #'<
		       org-hana-highlights))
    t))

(defun org-hana-find-next-highlight ()
  "Return the beg point of the next highlight.
Look through `org-hana-highlights' list."
  (when-let ((points (org-hana-list-highlights-positions)))
      ;; Find the first occurance of p > (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-hana-highlights' is sorted in the ascending order (it is).
    (seq-find (lambda (p) (> p (point))) points (nth 0 points))))

(defun org-hana-find-prev-highlight ()
  "Return the beg point of the previous highlight.
Look through `org-hana-highlights' list (in descending order)."
  (when-let ((points (org-hana-list-highlights-positions 'reverse)))
      ;; Find the first occurance of p < (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-hana-highlights' is sorted in the descending order .
    (seq-find (lambda (p) (< p (point))) points (nth 0 points))))

(defun org-hana-hide ()
  "Hide highlights.
This function removes the font-lock-face of all the highlights,
and add org-hana-hidden property with value t. It does not
check the current hidden state, thus not interactive.  Use
`org-hana-toggle' command to manually toggle the show/hide
state."
  (when-let ((highlights org-hana-highlights))
    (dolist (highlight highlights)
      (overlay-put highlight 'face nil)
      (overlay-put highlight 'org-hana-hidden t))
    t))

(defun org-hana-show ()
  "Show highlights.
This function adds the font-lock-face to all the highlighted text
regions.  It does not check the current hidden state, thus not
interactive.  Use `org-hana-toggle' command to manually toggle
the show/hide state."
  (when-let ((highlights org-hana-highlights))
    (dolist (highlight highlights)
      (overlay-put highlight 'org-hana-hidden nil)
      ;; TODO it does not work wtih new pens
      (overlay-put highlight 'face 'org-hana-highlighter))
    t))

(defun org-hana-remove-single-highlight (id &optional delete-notes)
  "Remove the highlight entry for ID for current buffer.
By default, it deletes only the properties of the entry keeping
the headline intact.  You can pass DELETE-NOTES and delete the
all notes of the entry."
  (with-current-buffer (find-file-noselect org-hana-notes-file-path)
      (org-with-wide-buffer
       (when-let ((id-headline (org-find-property org-hana-prop-id id)))
         (goto-char id-headline)
	 (org-narrow-to-subtree)
         (dolist (prop (org-entry-properties))
           (when (string-prefix-p "org-hana-" (downcase (car prop)))
             (org-delete-property (car prop))))
         ;; Backward compatible
         (org-delete-property org-hana-prop-id)
         (org-delete-property org-hana-prop-source-beg)
         (org-delete-property org-hana-prop-source-end)
         (when delete-notes
           ;; TODO I would love to add the y-n prompt if there is any notes written
           (delete-region (point-min)(point-max))
           (message "Deleted the marginal notes."))
	 (when (buffer-modified-p) (save-buffer))))
      t))

(defun org-hana-housekeep ()
  "Housekeep the internal variable `org-hana-highlights'.
This is a private function; housekeep is automatically done on
mark, save, and remove -- before sort-highlights.

Case 1. Both start and end of an overlay are identical

        This should not happen when you manually mark a text
        region.  A typical cause of this case is when you delete a
        region that contains a highlight overlay.

Case 2. The overlay points to no buffer

        This case happens when overlay is deleted by
        `overlay-delete' but the variable not cleared."
  (dolist (ov org-hana-highlights)
    ;; Both start and end of an overlay are indentical; this should not happen
    ;; when you manually mark a text region. A typical cause of this case is
    ;; when you delete a region that contains a highlight overlay.
    (when (and (overlay-buffer ov)
	       (= (overlay-start ov) (overlay-end ov)))
      (org-hana-remove-single-highlight (overlay-get ov 'org-hana-id))
      (delete-overlay ov))
    (unless (overlay-buffer ov)
      (setq org-hana-highlights (delete ov org-hana-highlights))))
  t)

(defun org-hana-empty-buffer-p ()
  "Return non-nil when the current buffer is empty."
  (save-excursion
    (goto-char (point-max))
    (= 1 (point))))

;;;; Footer

(provide 'org-hana)

;;; org-hana.el ends here

;; Local Variables:
;; eval: (setq-local org-hana-notes-file-path "README.org")
;; End:
