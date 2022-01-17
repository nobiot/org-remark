;;; org-remark.el --- Highlight & annotate text file -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Version: 0.1.0
;; Created: 22 December 2020
;; Last modified: 17 January 2022
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

;; This package lets you highlight and annotate any text file with using Org
;; mode.  Refer to README.org and docstring for detail.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-remark-global-tracking)
(declare-function org-remark-convert-legacy-data "org-remark-convert-legacy")


;;;; Customization

(defgroup org-remark nil
  "Highlight and annotate any text file with using Org mode."
  :group 'org
  :prefix "org-remark-"
  :link '(url-link :tag "GitHub" "https://github.com/nobiot/org-remark"))

(defface org-remark-highlighter
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#001904")
    (t
     :inherit highlight))
  "Face for the default highlighter pen.")

(defcustom org-remark-create-default-pen-set t
  "When non-nil, Org-remark creates default pen set.
Set to nil if you prefer for it not to.
`org-remark-mark' is always created as a fall-back"
  :type 'boolean)

(defcustom org-remark-notes-file-path "marginalia.org"
  "Specify the file path to store the location of highlights and write annotations.
The default is one file per directory.  Ensure that it is an Org
file."
  :type 'file)

(defcustom org-remark-notes-display-buffer-action
  `((display-buffer-in-side-window)
    (side . left)
    (slot . 1)
    (dedicated . t))
  "Define how Org-remark opens the notes buffer.
The default is to use a dedicated side-window on the left.  It is
an action list for `display-buffer'.  Refer to its documentation
for more detail and expected elements of the list."
  :type display-buffer--action-custom-type)

(defcustom org-remark-notes-buffer-name "*marginal notes*"
  "Define the buffer name of the marginal notes.
`org-remark-open' creates an indirect clone buffer with this
name."
  :type 'string)

(defcustom org-remark-use-org-id nil
  "Define if Org-remark use Org-ID to link back to the main note."
  :type 'boolean)


;;;; Variables

(defvar-local org-remark-loaded nil
  "Indicate if highlights have been loaded onto current buffer.")

(defvar-local org-remark-highlights '()
  "Keep track of all the highlights in current buffer.
It is a local variable and is a list of overlays.  Each overlay
represents a highlighted text region.

On `save-buffer' each highlight will be save in the notes file at
`org-remark-notes-file-path'.")

(defvar org-remark-last-notes-buffer nil
  "Stores the cloned indirect buffer visiting the notes file.
It is meant to exist only one of these in each Emacs session.")

(defvar org-remark-available-pens nil)

;; Const for the names of properties in Org Mode
(defconst org-remark-prop-id "org-remark-id")
(defconst org-remark-prop-source-file "org-remark-file")
(defconst org-remark-prop-source-beg "org-remark-beg")
(defconst org-remark-prop-source-end "org-remark-end")


;;;; Macros to create user-defined highlighter pen functions

(defmacro org-remark-create (label &optional face properties)
  "Create and register new highlighter pen functions.

The newly created pen function will be registered to variable
`org-remark-available-pens'.  It is used by `org-remark-change'
as a selection list.

LABEL is the name of the highlighter and mandatoryy.  The function
will be named `org-remark-mark-LABEL'.

The highlighter pen function will apply FACE to the selected region.
FACE can be an anonymous face.  When FACE is nil, this macro uses
the default face `org-remark-highlighter'.

PROPERTIES is a plist of pairs of a symbol and value.  Each
highlighted text region will have a corresponding Org headline in
the notes file, and it can have additional properties in the
property drawer from the highlighter pen.  To do this, prefix
property names with \"org-remark-\" or use \"CATEGORY\"."
  (if (not label) `(user-error "org-remark-create: Label is missing")
    `(progn
       (add-to-list 'org-remark-available-pens
                    (intern (or (when ,label (format "org-remark-mark-%s" ,label))
                                "org-remark-mark")))
       (defun ,(intern (format "org-remark-mark-%s" label))
           (beg end &optional id load-only)
         ,(format "Apply the following face to the region selected by BEG and END.

%s

Following overlay properties will be added to the highlighted
text region:

%S

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region, and Org-remark will start tracking the highlight's
location in the current buffer.

When this function is called from Elisp, ID can be optionally
passed, indicating to Org-remark that it is an existing
highlight.  In this case, no new ID gets generated.

When LOAD-ONLY is non-nil, this function does not save the
highlight in the marginal notes file.  This is meant to be for
`org-remark-load'."
                  (or face "`org-remark-highlighter'") properties)
         (interactive "r")
         (org-remark-mark beg end id load-only ,label ,face ,properties)))))

;; Don't use category (symbol) as a property -- it's a special one of text
;; properties. If you use it, the value also need to be a symbol; otherwise, you
;; will get an error. You can use CATEGORY (symbol and all uppercase).
(when org-remark-create-default-pen-set
  ;; Create default pen set.
  ;; They are rather meant to be a starter pack and examples
  ;; They can be overridden, or set `org-remark-create-default-pen-set' to nil
  ;; so that Org-remark will not create them.
  (org-remark-create "red-line"
                     '(:underline (:color "dark red" :style wave))
                     '(CATEGORY "review" help-echo "Review this"))
  (org-remark-create "yellow"
                     '(:underline "gold" :background "lemon chiffon") '(CATEGORY "important")))


;;;; Commands

;;;###autoload
(define-minor-mode org-remark-mode
    "Highlight and annotate any text file with using Org mode.
This is a local minor-mode.

On activation, it loads your saved highlights from the notes file
and enables automatic saving of highlights thereafter.

The automatic saving is achieved via function
`org-remark-save' added to `after-save-hook'.

On deactivation, it removes all the overlays and stops tracking
the highlights in this buffer by setting variable
`org-remark-highlights' to nil.  Be careful of behavior, if
you still wish to retain the locations of highlights.

It is recommended to use `org-remark-toggle' if you wish to
temporarily hide highlights in the current buffer.  It keeps
`org-remark-highlights' unchanged.

While the tracking of highlights is stopped,
editing the buffer will likely result in mismatch between the
saved highlights' locations and the current buffer's text
content.

Highlights tracked by variable `org-remark-highlights' cannot
persist when you kill the buffer or quit Emacs.  When you
re-launch Emacs and visit the same file, ensure to turn on
`org-remark-mode' to load the highlights from the marginalia
file.  `org-remark-global-tracking-mode' automates this.  It is
recommended to turn it on as part of Emacs initialization.

\\{org-remark-mode-map}"
    :init-value nil
    :lighter " ormk"
    :global nil
    :keymap (let ((map (make-sparse-keymap)))
              map)
    (cond
     (org-remark-mode
      ;; Activate
      (org-remark-load)
      (add-hook 'after-save-hook #'org-remark-save nil t)
      (add-hook 'kill-buffer-hook #'org-remark-tracking-save nil t))
     (t
      ;; Deactivate
      (when org-remark-highlights
        (dolist (highlight org-remark-highlights)
          (delete-overlay highlight)))
      (setq org-remark-highlights nil)
      (setq org-remark-loaded nil)
      (org-remark-tracking-save)
      (remove-hook 'after-save-hook #'org-remark-save t)
      (remove-hook 'kill-buffer-hook #'org-remark-tracking-save t))))

(add-to-list 'org-remark-available-pens #'org-remark-mark)
;;;###autoload
(defun org-remark-mark (beg end &optional id load-only label face properties)
  "Apply the FACE to the region selected by BEG and END.

This function will apply FACE to the selected region.  When it is
nil, this function will use the default face `org-remark-highlighter'

This function will add LABEL and PROPERTIES as overlay
properties. PROPERTIES is a plist of pairs of a symbol and value.

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region, and Org-remark will start tracking the highlight's
location in the current buffer.

When this function is called from Elisp, ID can be optionally
passed, indicating to Org-remark that it is an existing
highlight.  In this case, no new ID gets generated.

A Org headline entry for the highlght will be created in the
marginal notes file specified by `org-remark-notes-file-path'. If
the file does not exist yet, it will be created.

When LOAD-ONLY is non-nil, this function will not save the
highlight in the marginal notes file.  This is meant to be for
`org-remark-load'."
  (interactive "r")
  ;; Ensure to turn on the local minor mode
  (unless org-remark-mode (org-remark-mode +1))
  ;; UUID is too long; does not have to be the full length
  (when (not id) (setq id (substring (org-id-uuid) 0 8)))
  ;; Add highlight to the text
  (org-with-wide-buffer
   (let ((ov (make-overlay beg end nil 'FRONT-ADVANCE)))
     (overlay-put ov 'face (if face face 'org-remark-highlighter))
     (while properties
       (let ((prop (pop properties))
             (val (pop properties)))
         (overlay-put ov prop val)))
     (when label (overlay-put ov 'org-remark-label label))
     (overlay-put ov 'org-remark-id id)
     ;; Keep track of the overlay in a local variable. It's a list that is
     ;; guaranteed to contain only org-remark overlays as opposed to the one
     ;; returned by `overlay-lists' that lists any overlays.
     (push ov org-remark-highlights)
     ;; Adding overlay to the buffer does not set the buffer modified. You
     ;; cannot use `undo' to undo highlights, either.
     (unless load-only
       (org-remark-single-highlight-save (buffer-file-name)
                                         beg end
                                         (overlay-properties ov)
                                         (org-remark-single-highlight-get-title)))
     (deactivate-mark)))
  (org-remark-housekeep)
  (org-remark-highlights-sort))

(defun org-remark-load ()
  "Visit `org-remark-notes-file' & load the saved highlights onto current buffer.
If there is no highlights or annotations for current buffer,
output a message in the echo.

Highlights tracked locally by variable `org-remark-highlights'
cannot persist when you kill current buffer or quit Emacs.  It is
recommended to set `org-remark-global-tracking-mode' in your
configuration.  It automatically turns on `org-remark-mode', which
runs `org-remark-load' for current buffer when you open it.

Otherwise, do not forget to turn on `org-remark-mode' manually to
load the highlights"
  (interactive)
  (unless org-remark-mode (org-remark-mode +1))
  (unless org-remark-loaded
    ;; Loop highlights and add them to the current buffer
    (dolist (highlight (org-remark-highlights-get))
      (let ((id (car highlight))
            (beg (caadr highlight))
            (end (cdadr highlight))
            (label (caddr highlight)))
        (let ((fn (intern (concat "org-remark-mark-" label))))
          (unless (functionp fn) (setq fn #'org-remark-mark))
          (funcall fn beg end id 'load-only))))
    (setq org-remark-loaded t))
  ;; Tracking
  (org-remark-notes-track-file (buffer-file-name)))

(defun org-remark-save ()
  "Save all the highlights tracked in current buffer to notes file.
Variable`org-remark-notes-file-path' defines the file path.

This function is automatically called when you save the current
buffer via `after-save-hook'.

When `org-remark-global-tracking-mode' is on, this function also
adds current buffer to variable `org-remark-files-tracked' so that
next time you visit this file, `org-remark-mode' can be
automatically turned on to load the highlights.

`org-remark-highlights' is the local variable that tracks every highlight
in the current buffer.  Each highlight is represented by an overlay."
  (interactive)
  (org-remark-housekeep)
  (org-remark-highlights-sort)
  (let ((path (buffer-file-name)))
    (dolist (h org-remark-highlights)
      (let ((beg (overlay-start h))
            (end (overlay-end h))
            (props (overlay-properties h)))
        (org-remark-single-highlight-save path beg end props)))
    ;; Tracking
    (org-remark-notes-track-file path)))

(defun org-remark-open (point &optional view-only)
  "Open marginal notes file for highlight at POINT.
The marginal notes will be narrowed to the relevant headline to
show only the highlight at point.

This function creates a cloned indirect buffer of the marginal
notes file \(`org-remark-notes-file-path'\).  You can edit
marginal notes file as a normal Org file.  Once you have done
editing, you can simply save and kill the buffer or keep it
around.

The marginal notes file gets displayed by the action defined by
`org-remark-notes-display-buffer-action' (by default in a side
window in the left of the current frame), narrowed to the
relevant headline.

You can customize the name of the marginal notes buffer with
`org-remark-notes-buffer-name'.

By default, the cursor will go to the marginal notes buffer for
further editing.  When VIEW-ONLY is non-nil \(e.g. by passing a
universal argument with \\[universal-argument]\), you can display
the marginal notes buffer with the cursour remaining in the
current buffer.

This function ensures that there is only one cloned buffer for
notes file by tracking it."
  (interactive "d\nP")
  (when (buffer-live-p org-remark-last-notes-buffer)
      (kill-buffer org-remark-last-notes-buffer))
  (when-let ((id (get-char-property point 'org-remark-id))
             (ibuf (make-indirect-buffer
                    (find-file-noselect org-remark-notes-file-path)
                    org-remark-notes-buffer-name 'clone)))
    (setq org-remark-last-notes-buffer ibuf)
    (with-current-buffer ibuf
      (when-let (p (org-find-property org-remark-prop-id id))
        (widen)(goto-char p)(org-narrow-to-subtree)))
    (display-buffer ibuf org-remark-notes-display-buffer-action)
    (unless view-only (select-window (get-buffer-window ibuf)))))

(defun org-remark-view (point)
  "View marginal notes for highlight at POINT.
The marginal notes file gets displayed by the action defined by
`org-remark-notes-display-buffer-action' (by default in a side
window in the left of the current frame), narrowed to the
relevant headline.  The cursor remains in the current buffer.

Also see the documentation of `org-remark-open'."
  (interactive "d")
  (org-remark-open point :view-only))

(defun org-remark-next ()
  "Move to the next highlight, if any.
If there is none below the point but there is a highlight in the
buffer, cycle back to the first one.

After the point has moved to the next highlight, this command
lets you move further by re-entering only the last letter like
this example:v

   C-n \] \] \] \] \] \(assuming this command is bound to C-n \]\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-remark-prev', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (org-remark-next-or-prev :next))

(defun org-remark-prev ()
  "Move to the previous highlight, if any.
If there is none above the point, but there is a highlight in the
buffer, cycle back to the last one.

After the point has moved to the previous highlight, this command
lets you move further by re-entering only the last letter like
this example:

   C-n \[ \[ \[ \[ \[ \(assuming this command is bound to C-n \[\)

This is achieved by transient map with `set-transient-map'.

If you have the same prefix for `org-remark-next', you can combine it in
the sequence like so:

   C-n \] \] \] \[ \["
  (interactive)
  (org-remark-next-or-prev))

(defun org-remark-view-next ()
  "Move the cursor to the next highlight and view its marginal notes."
  (interactive)
  (org-remark-next)(org-remark-view (point)))

(defun org-remark-view-prev ()
  "Move the cursor to the previous highlight and view its marginal notes."
  (interactive)
  (org-remark-prev)(org-remark-view (point)))

(defun org-remark-toggle ()
  "Toggle showing/hiding of highlights in current buffer.
This function only affects the display of the highlights.  Their
locations are still kept tracked.  On buffer-save the correct
locations are to be saved in the marginal notes file when the
highlights are hidden, thus it is recommended to use this
function, instead of `org-remark-mode', if you would just like to
hide the highlights."
  (interactive)
  (when-let ((highlights org-remark-highlights))
    ;; Check the first highlight in the buffer
    ;; If it's hidden, all hidden. Show them.
    ;; If not, all shown. Hide them.
    (if-let* ((beg (overlay-start (nth 0 highlights)))
              (hidden-p (get-char-property beg 'org-remark-hidden)))
        (org-remark-show)
      (org-remark-hide))
    t))

(defun org-remark-change (&optional pen)
  "Change the highlight at point to PEN.
This function will show you a list of available pens to choose
from."
  (interactive)
  (when-let* ((ov (org-remark-overlay-find))
              (id (overlay-get ov 'org-remark-id))
              (beg (overlay-start ov))
              (end (overlay-end ov)))
    ;; FIXME read list of pens
    ;; when create, add to list
    (let ((new-pen (if pen pen
                     (intern
                      (completing-read "Which pen?:" org-remark-available-pens)))))
      (delete-overlay ov)
      (funcall new-pen beg end id))))

(defun org-remark-remove (point &optional arg)
  "Remove the highlight at POINT.
It will remove the highlight and the properties from the
marginalia, but will keep the headline and annotations.  This is
to ensure to keep any notes you might have written intact.

You can let this command delete the entire heading subtree for
the highlight, along with the annotations you have written, by
passing a universal argument with \\[universal-argument] \(ARG\).
If you have done so by error, you could still `undo' it in the
marginal notes buffer, but not in the current buffer as adding
and removing overlays are not part of the undo tree."
  (interactive "d\nP")
  ;; TODO There may be multiple overlays
  (when-let* ((id (get-char-property point 'org-remark-id)))
    ;; Remove the highlight overlay and id
    (dolist (ov (overlays-at (point)))
      ;; Remove the element in the variable org-remark-highlights
      (when (overlay-get ov 'org-remark-id)
        (delete ov org-remark-highlights)
        (delete-overlay ov)))
    (org-remark-housekeep)
    (org-remark-highlights-sort)
    ;; Update the notes file accordingly
    (org-remark-single-highlight-remove id arg)
    t))


;;;; Internal Functions

(defun org-remark-next-or-prev (&optional next)
  "Move cursor to the next or previous highlight if any.
NEXT must be either non-nil or nil.
When non-nil it's for the next; for nil, prev.

This function is internal only and meant to be used by interctive
commands such as `org-remark-next' and `org-remark-prev'.

Return t if the cursor has moved to next/prev.
Return nil if not after a message."
  (if (not org-remark-highlights)
      (progn (message "No highlights present in the buffer") nil)
    (let ((p (if next (org-remark-find-next-highlight)
               (org-remark-find-prev-highlight))))
      (if p (progn
              (goto-char p)
              ;; Setup the overriding keymap.
              (unless overriding-terminal-local-map
                (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
                      (map (cdr org-remark-mode-map)))
                  (when (< 0 (length prefix-keys))
                    (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
                    (setq map (cdr-safe map))
                    (when (keymapp map) (set-transient-map map t)))))
              t)
        (message "No visible highlights present in the buffer")
        nil))))

(defun org-remark-overlay-find ()
  "Return one org-remark overlay at point.
If there are more than one, return CAR of the list."
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'org-remark-id)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    (car found)))

(defun org-remark-single-highlight-get-title ()
  "Return the title of the current buffer.
Utility function to work with a single highlight overlay."
  (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                    (file-name-sans-extension
                     (file-name-nondirectory (buffer-file-name)))))

(defun org-remark-single-highlight-get-org-id (point)
  "Return Org-ID closest to POINT.
This function does this only when `org-remark-use-org-id' is
non-nil.  Returns nil otherwise, or when no Org-ID is found."
  (and org-remark-use-org-id
       (org-entry-get point "ID" :inherit)))

(defun org-remark-single-highlight-save (path beg end props &optional title)
  "Save a single HIGHLIGHT in the marginal notes file.

Return t.

PATH specifies the source/main file with which the marginal notes
file is associated.

BEG and END specify the range of the highlight being saved.  It
is the highlight overlay's start and end.

PROPS are the highlight overlay's properties.  Not all the
properties will be added as headline properties.  Refer to
`org-remark-notes-set-properties'.

For the first highlight of the current buffer, this function will
create a new H1 headline for it at the bottom of the marginal
notes buffer with TITLE as its headline text.

If it is a new highlight, this function will create a new H2
headline with the highlighted text as its headline text at the
end of the H1 headline for the current buffer.

If headline with the same ID already exists, update its position
and other \"org-remark-*\" properties (CATEGORY is the exception
and gets updated as well) from the highlight overlay.  For
update, the headline text will be kept intact, because the user
might have changed it to their needs.

This function will also add a normal file link as property
\"org-remark-lilnk\" of the H2 headline entry back to the current
buffer with serach option \"::line-number\".

ORGID can be passed to this function.  If user option
`org-remark-use-org-id' is non-nil, this function will add an
Org-ID link in the body text of the headline, linking back to the
source with using ORGID.

When a new marginal notes file is to be created and
`org-remark-use-org-id' is non-nil, this function will also add
an Org-ID property to the file level.  This can be helpful with
other packages such as Org-roam's backlink feature."
  ;;`org-with-wide-buffer is a macro that should work for non-Org file'
  (let* ((path (org-remark-source-path path))
         (id (plist-get props 'org-remark-id))
         (text (org-with-wide-buffer (buffer-substring-no-properties beg end)))
         (orgid (org-remark-single-highlight-get-org-id beg))
         ;; FIXME current-line - it's not always at point
         (line-num (org-current-line beg)))
    ;; TODO Want to add a check if save is applicable here.
    (with-current-buffer (find-file-noselect org-remark-notes-file-path)
      ;; If it is a new empty marginalia file
      (when (and (org-remark-empty-buffer-p) org-remark-use-org-id)
        (org-id-get-create))
      (when (featurep 'org-remark-convert-legacy) (org-remark-convert-legacy-data))
      (org-with-wide-buffer
       (let ((file-headline (or (org-find-property
                                 org-remark-prop-source-file path)
                                (progn
                                  ;; If file-headline does not exist, create one at the bottom
                                  (goto-char (point-max))
                                  ;; Ensure to be in the beginning of line to add a new headline
                                  (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
                                  (insert (concat "* " title "\n"))
                                  (org-set-property org-remark-prop-source-file path)
                                  (org-up-heading-safe) (point))))
             (id-headline (org-find-property org-remark-prop-id id)))
         ;; Add org-remark-link with updated line-num as a property
         (plist-put props "org-remark-link" (concat
                                             "[[file:"
                                             path
                                             (when line-num (format "::%d" line-num))
                                             "]]"))
         (if id-headline
             (progn
               (goto-char id-headline)
               ;; Update the existing headline and position properties
               ;; Don't update the headline text when it already exists
               ;; Let the user decide how to manage the headlines
               ;; (org-edit-headline text)
               ;; FIXME update the line-num in a normal link if any
               (org-remark-notes-set-properties beg end props))
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
           (org-remark-notes-set-properties beg end props)
           (when (and orgid org-remark-use-org-id)
               (insert (concat "[[id:" orgid "]" "[" title "]]"))))))
      (when (buffer-modified-p) (save-buffer))
      t)))

(defun org-remark-single-highlight-remove (id &optional delete-notes)
  "Remove the highlight entry for ID for current buffer.
By default, it deletes only the properties of the entry keeping
the headline intact.  You can pass DELETE-NOTES and delete the
all notes of the entry.

Return t if an entry is removed or deleted."
  (with-current-buffer (find-file-noselect org-remark-notes-file-path)
      (org-with-wide-buffer
       (when-let ((id-headline (org-find-property org-remark-prop-id id)))
         (goto-char id-headline)
         (org-narrow-to-subtree)
         (dolist (prop (org-entry-properties))
           (when (string-prefix-p "org-remark-" (downcase (car prop)))
             (org-delete-property (car prop))))
         ;; Backward compatible
         (org-delete-property org-remark-prop-id)
         (org-delete-property org-remark-prop-source-beg)
         (org-delete-property org-remark-prop-source-end)
         (when delete-notes
           ;; TODO I would love to add the y-n prompt if there is any notes written
           (delete-region (point-min)(point-max))
           (message "Deleted the marginal notes entry"))
         (when (buffer-modified-p) (save-buffer))))
      t))

(defun org-remark-notes-set-properties (beg end &optional props)
  "Set properties for the headline in the notes file.
Return t.

Minimal properties are:

- org-remark-id :: ID
- org-remark-source-beg :: BEG
- org-remark-source-end :: END

For PROPS, if the property name is CATEGORY \(case-sensitive\) or
prefixed with \"org-remark-\" set them to to headline's property
drawer."
  (org-set-property org-remark-prop-source-beg
                    (number-to-string beg))
  (org-set-property org-remark-prop-source-end
                    (number-to-string end))
  (while props
    (let ((p (pop props))
          (v (pop props)))
      (when (symbolp p) (setq p (symbol-name p)))
      (when (or (string-equal "CATEGORY" (upcase p))
                (and (> (length p) 11)
                     (string-equal "org-remark-" (downcase (substring p 0 11)))))
        (org-set-property p v))))
  t)

(defun org-remark-highlights-get ()
  "Return a list of highlights from `org-remark-notes-file-path'.
Each highlight is a list in the following structure:
    (id (beg . end) label)"
  (when-let ((notes-buf (find-file-noselect org-remark-notes-file-path))
             (source-path (org-remark-source-path (buffer-file-name))))
    ;; TODO check if there is any relevant notes for the current file
    (let ((highlights))
      (with-current-buffer notes-buf
        (when (featurep 'org-remark-convert-legacy)
          (org-remark-convert-legacy-data))
        (org-with-wide-buffer
         (let ((heading (org-find-property
                         org-remark-prop-source-file source-path)))
           (if (not heading)
               (message "No highlights or annotations found for %s."
                        source-path)
             (goto-char heading)
             ;; Narrow to only subtree for a single file.  `org-find-property'
             ;; ensures that it is the beginning of a headline
             (org-narrow-to-subtree)
             ;; It's important that the headline levels are fixed
             ;; H1: File
             ;; H2: Highlighted region (each one has a dedicated H2 subtree)
             (while (not (org-next-visible-heading 1))
               (when-let ((id (org-entry-get (point) org-remark-prop-id))
                          (beg (string-to-number
                                (org-entry-get (point)
                                               org-remark-prop-source-beg)))
                          (end (string-to-number
                                (org-entry-get (point)
                                               org-remark-prop-source-end))))
                 (push (list id
                             (cons beg end)
                             (org-entry-get (point) "org-remark-label"))
                       highlights))))
           highlights))))))

(defun org-remark-highlights-get-positions (&optional reverse)
  "Return list of the beggining point of all visible highlights in this buffer.
By default, the list is in ascending order.  If REVERSE is
non-nil, return list in the descending order.

This function also checks if the position is visible or not.
Return only visible ones.

If none, return nil."
  (when org-remark-highlights
    (let ((list org-remark-highlights))
      (setq list (mapcar
                  (lambda (h)
                    (let ((p (overlay-start h)))
                      ;; Checking if the p is visible or not
                      (if (or
                           (> p (point-max))
                           (< p (point-min))
                           ;; When the highlight is within a visible folded
                           ;; area, this function returns 'outline
                           (org-invisible-p p))
                          nil p)))
                  list))
      (setq list (remove nil list))
      (when list
        (if reverse (reverse list) list)))))

(defun org-remark-highlights-sort ()
  "Utility function to sort `org-remark-highlights'.
It checks if there is any element exists for `org-remark-highlights'.
Instead of receiving it as an arg, it assumes its existence.  It
also destructively updates `org-remark-highlights'.
It returns t when sorting is done."
  (when org-remark-highlights
    (setq org-remark-highlights
          (seq-sort-by (lambda (ov) (overlay-start ov))
                       #'<
                       org-remark-highlights))
    t))

(defun org-remark-find-next-highlight ()
  "Return the beg point of the next highlight.
Look through `org-remark-highlights' list."
  (when-let ((points (org-remark-highlights-get-positions)))
      ;; Find the first occurrence of p > (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-remark-highlights' is sorted in the ascending order (it is).
    (seq-find (lambda (p) (> p (point))) points (nth 0 points))))

(defun org-remark-find-prev-highlight ()
  "Return the beg point of the previous highlight.
Look through `org-remark-highlights' list (in descending order)."
  (when-let ((points (org-remark-highlights-get-positions 'reverse)))
      ;; Find the first occurrence of p < (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `org-remark-highlights' is sorted in the descending order .
    (seq-find (lambda (p) (< p (point))) points (nth 0 points))))

(defun org-remark-hide ()
  "Hide highlights.
This function removes the font-lock-face of all the highlights,
and add org-remark-hidden property with value t. It does not
check the current hidden state, thus not interactive.  Use
`org-remark-toggle' command to manually toggle the show/hide
state."
  (when-let ((highlights org-remark-highlights))
    (dolist (highlight highlights)
      (overlay-put highlight 'org-remark-face (overlay-get highlight 'face))
      (overlay-put highlight 'face nil)
      (overlay-put highlight 'org-remark-hidden t))
    t))

(defun org-remark-show ()
  "Show highlights.
This function adds the font-lock-face to all the highlighted text
regions.  It does not check the current hidden state, thus not
interactive.  Use `org-remark-toggle' command to manually toggle
the show/hide state."
  (when-let ((highlights org-remark-highlights))
    (dolist (highlight highlights)
      (overlay-put highlight 'org-remark-hidden nil)
      ;; TODO it does not work with new pens
      (overlay-put highlight 'face (overlay-get highlight 'org-remark-face)))
    t))

(defun org-remark-housekeep ()
  "Housekeep the internal variable `org-remark-highlights'.

Return t.

This is a private function; housekeep is automatically done on
mark, save, and remove -- before sort-highlights.

Case 1. Both start and end of an overlay are identical

        This should not happen when you manually mark a text
        region.  A typical cause of this case is when you delete a
        region that contains a highlight overlay.

Case 2. The overlay points to no buffer

        This case happens when overlay is deleted by
        `overlay-delete' but the variable not cleared."
  (dolist (ov org-remark-highlights)
    ;; Both start and end of an overlay are identical; this should not happen
    ;; when you manually mark a text region. A typical cause of this case is
    ;; when you delete a region that contains a highlight overlay.
    (when (and (overlay-buffer ov)
               (= (overlay-start ov) (overlay-end ov)))
      (org-remark-single-highlight-remove (overlay-get ov 'org-remark-id))
      (delete-overlay ov))
    (unless (overlay-buffer ov)
      (setq org-remark-highlights (delete ov org-remark-highlights))))
  t)

(defun org-remark-source-path (path)
  "Covert PATH either to absolute or relative for marginal notes files.
Returns the standardized path.  Currently, it's only a place
holder and uses `abbreviate-file-name' to return an absolute
path."
  ;; TODO
  ;; A place holder for enhancemnet after the release of v1.0.0
  ;; Potentially support relative path.
  ;; No capacity to test this properly at the moment.
  ;;
  ;; (if org-remark-notes-relative-directory
  ;;     (funcall org-remark-notes-path-function path org-remark-notes-relative-directory)
  ;;   (funcall org-remark-notes-path-function path)))
  (abbreviate-file-name path))

(defun org-remark-notes-track-file (path)
  "Add PATH to `org-remark-files-tracked' when relevant.
It works only when `org-remark-global-tracking-mode' is on.  For
the global tracking purpose, the path must be an absolute path."
  (when org-remark-global-tracking-mode
    (add-to-list 'org-remark-files-tracked
                 (abbreviate-file-name path))))

(defun org-remark-empty-buffer-p ()
  "Return t when the current buffer is empty."
  (when (= 0 (buffer-size)) t))


;;;; Footer

(provide 'org-remark)

;;; org-remark.el ends here

;; Local Variables:
;; eval: (setq-local org-remark-notes-file-path "README.org")
;; End:
