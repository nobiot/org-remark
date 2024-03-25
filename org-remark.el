;;; org-remark.el --- Highlight & annotate text, Info, EPUB, EWW -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Version: 1.2.2
;; Created: 22 December 2020
;; Last modified: 25 March 2024
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, note-taking, marginal-notes, wp,

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

;; This package lets you highlight and annotate any text file with using
;; Org mode.  For usage, refer to the user manual available as an Info
;; node by evaluating (info "org remark")

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-remark-global-tracking)
(declare-function org-remark-convert-legacy-data "org-remark-convert-legacy")


;;;; Customization

(defgroup org-remark nil
  "Highlight and annotate any text files with using Org mode."
  :group 'org
  :prefix "org-remark-"
  :link '(url-link :tag "GitHub" "https://github.com/nobiot/org-remark"))

(defface org-remark-highlighter
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#1d3c25")
    (t
     :inherit highlight))
  "Face for the default highlighter pen.")

(defface org-remark-highlighter-warning
  '((((class color) (min-colors 88) (background light))
     :foreground "#604000" :background "#fff29a")
    (((class color) (min-colors 88) (background dark))
     :foreground: "#e2d980" :background "#693200")
    (t
     :inherit warning))
  "Face for highlighter warning.
For example, it is used by the characters that indicate the
location of the highlight has been automatically adjusted from its
original.")

(defcustom org-remark-create-default-pen-set t
  "When non-nil, Org-remark creates default pen set.
Set to nil if you prefer for it not to."
  :type 'boolean)

(defcustom org-remark-notes-display-buffer-action
  `((display-buffer-in-side-window)
    (side . left)
    (slot . 1))
  "Buffer display action that Org-remark uses to open marginal notes buffer.

The default is to use a side-window on the left.

Org-remark uses `pop-to-buffer', which passes this display action
list to `display-buffer'.  Refer to its documentation for more
detail and expected elements of the list."
  :type display-buffer--action-custom-type)

(defcustom org-remark-notes-buffer-name "*marginal notes*"
  "Buffer name of the marginal notes buffer.
`org-remark-open' and `org-remark-visit' create an indirect clone
buffer with this name."
  :type 'string)

(defcustom org-remark-notes-auto-delete nil
  "How Org-remark removes entries when user deletes highlights.
This controls the behavior of Org-remark when the user deletes
the highlight in the source (for example, when deleting a whole
line including a highlight). The default behavior is to remove
only the properties in the notes buffer and keeping the headline
title and any notes in the entry (with using
`org-remark-remove').

If this behavior leads to cluttering the marginal notes org file,
you can set this customizing variable to \\=':auto-delete\\='.
With this option, Org-remark will delete the entire entry when it
contains no notes without a prompt asking for confirmation. This
is the same behavior as passing a single `universal-argument'
\(\\[universal-argument]) to`org-remark-delete' or double `universal-argument'
\(\\[universal-argument] \\[universal-argument]) to `org-remark-remove'."
  :type '(radio
          (const :tag "Keep entries (default)" nil)
          (const :tag "Delete entries automatically when no notes exist"
                 :auto-delete)))

(defvaralias
  'org-remark-source-path-function 'org-remark-source-file-name)

(make-obsolete-variable
 'org-remark-source-path-function 'org-remark-source-file-name "0.2.0")

(defcustom org-remark-source-file-name #'file-relative-name
  "Function that returns the file name to point back at the source file.

The function is called with a single argument: the absolute file
name of source file.  The `default-directory' is temporarily set
to the directory where the marginal notes file resides.

This means that when the \"Relative file name\" option is
selected, the source file name recorded in the marginal notes
file will be relative to it."
  :type '(choice
          (const :tag "Relative file name" file-relative-name)
          (const :tag "Abbreviated absolute file name" abbreviate-file-name)
          (function :tag "Other function")))

(defcustom org-remark-use-org-id nil
  "When non-nil, Org-remark adds an Org-ID link to marginal notes.
The link points at the relevant Org-ID in the source file.
Org-remark does not create this ID, which needs to be added
manually or some other function to either the headline or file."
  :type 'boolean)

(defcustom org-remark-open-hook nil
  "Hook run when a note buffer is opened/visited.
The current buffer is the note buffer."
  :type 'hook)

(defcustom org-remark-highlights-after-load-functions
  '(org-remark-highlights-adjust-positions)
  "Abnormal hook run after `org-remark-highlights-load'.
It is run with OVERLAYS and NOTES-BUF as arguments. OVERLAYS are
highlights. It is run with the source buffer as current buffer."
  :type 'hook)


;;;; Variables

(defvar org-remark-default-features '(org-remark-icon org-remark-line)
  "Extension features to be enabled by default.
It is suggested to keep them as the default, but you can choose to disable them")

(defvar org-remark-default-feature-modes '()
  "Extension minor modes to be enabled together with `org-remark-mode'.
These minor modes should be registered to this variable by the
respective feature where required. As an example, see
`org-remark-line'.")

(defvar org-remark-find-dwim-functions nil
  "Functions to find the highlight overlays.
These functions should be registered to this variable by the
respective feature where required. As an example, see
`org-remark-line'.")

(defvar org-remark-last-notes-buffer nil
  "The cloned indirect buffer visiting the notes file.
It is meant to exist only one of these in each Emacs session.")

(defvar org-remark-available-pens (list #'org-remark-mark)
  "A list of pens available.
Each pen is a function. Users can create a new custom pen with
using `org-remark-create', which automatically add a new pen
function this list. It is used by `org-remark-change' as a
selection list.")

(defvar-local org-remark-highlights '()
  "All the highlights in current source buffer.
It is a local variable and is a list of overlays.  Each overlay
represents a highlighted text region.

On `save-buffer' each highlight will be saved in the notes file
returned by `org-remark-notes-get-file-name'.")

(defvar-local org-remark-highlights-hidden nil
  "Hidden/shown state of the highlights in current source buffer.")

(defvar-local org-remark-notes-source-buffers '()
  "List of source buffers that have loaded current notes buffer.
Each notes buffer locally keeps track of the source buffers that
have loaded notes from itself.  This list is used when automatic
sync is triggered in `after-save-buffer' of the notes buffer, as
not all the sources may be open.  Buffers in this list may be
killed so that this needs to be checked with `buffer-live-p'.")

(defvar-local org-remark-source-setup-done nil
  "Local indicator that sync with notes buffer is set up.")

(defvar-local org-remark-highlights-toggle-hide-functions nil
  "Functions to be called when toggling to hide highlights.
Each function is called with one argument HIGHLIGHT, which is an
overlay that shows the highlight. It also stores properties to
control visibility such as \\=':face\\='.

This variable is an abnormal hook and is intended to be used to
add additional controls for the overlay properties.")

(defvar-local org-remark-highlights-toggle-show-functions nil
  "Functions to be called when toggling to show highlights.
Each function is called with one argument HIGHLIGHT, which is an
overlay that shows the highlight. It also stores properties to
control visibility such as \\=':face\\='.

This variable is an abnormal hook and is intended to be used to
add additional controls for the overlay properties")

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

LABEL is the name of the highlighter and mandatory.  The function
will be named `org-remark-mark-LABEL'.

The highlighter pen function will apply FACE to the selected region.
FACE can be an anonymous face.  When FACE is nil, this macro uses
the default face `org-remark-highlighter'.

PROPERTIES is a plist of pairs of a symbol and value.  Each
highlighted text region will have a corresponding Org headline in
the notes file, and it can have additional properties in the
property drawer from the highlighter pen.  To do this, prefix
property names with \"org-remark-\" or use \"CATEGORY\"."
  (if (or (not label) (stringp label)
          (user-error "org-remark-create: Label is missing or not string"))
      (let ((org-remark-type
             `(quote ,(plist-get (eval properties) 'org-remark-type))))
        `(progn
           ;; Define custom pen function
           (defun ,(intern (format "org-remark-mark-%s" label))
               (beg end &optional id mode)
             ,(format "Apply the following face to the region selected by BEG and END.

%s

Following overlay properties will be added to the highlighted
text region:

%S

Return the overlay.

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region, and Org-remark will start tracking the highlight's
location in the current buffer.

When this function is called from Elisp, ID can be optionally
passed, indicating to Org-remark that it is an existing
highlight.  In this case, no new ID gets generated.

When the pen itself defines the help-echo property, it will have
the priority over the excerpt of the marginal notes."
                      (or face "`org-remark-highlighter'") properties)
             (interactive (org-remark-beg-end ,org-remark-type))
             (org-remark-highlight-mark beg end id mode ,label ,face ,properties))

           ;; Register to `org-remark-available-pens'
           (add-to-list 'org-remark-available-pens
                        (intern (format "org-remark-mark-%s" ,label)))

           ;; Add function prop This is for `org-remark-change' to show
           ;; only the pens of the same type
           (when 'org-remark-type (function-put
                                   (intern (format "org-remark-mark-%s" ,label))
                                   'org-remark-type
                                   ,org-remark-type))

           ;; Add the custom pen function to the minor-mode menu
           (define-key-after org-remark-pen-map
             [,(intern (format "org-remark-mark-%s" label))]
             '(menu-item ,(format "%s pen" label) ,(intern (format "org-remark-mark-%s" label))))

           ;; Add the custom pen change function to the minor-mode menu
           (define-key-after org-remark-change-pen-map
             [,(intern (format "org-remark-change-to-%s" label))]
             '(menu-item ,(format "%s pen" label)
                         (lambda ()
                           (interactive)
                           (org-remark-change
                            #',(intern (format "org-remark-mark-%s" label))))
                         :enable (org-remark-pen-same-type-at-point-p ,org-remark-type)))))))


;;;; Minor mode

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
      (dolist (feature org-remark-default-features)
        (unless (featurep feature) (require feature nil 'noerror)))
      (dolist (feature-mode org-remark-default-feature-modes)
        (when (functionp feature-mode) (funcall feature-mode +1)))
      (org-remark-highlights-load)
      (add-hook 'org-remark-find-dwim-functions
                #'org-remark-find-overlay-at-point nil :local)
      (add-hook 'after-save-hook #'org-remark-save nil :local)
      (add-hook 'org-remark-highlight-link-to-source-functions
                #'org-remark-highlight-link-to-source-default 80)
      (add-hook 'after-revert-hook #'org-remark-highlights-load 80 :local)
      (add-hook 'clone-buffer-hook #'org-remark-highlights-load 80 :local))
     (t
      ;; Deactivate
      (when org-remark-highlights
        (dolist (highlight org-remark-highlights)
          (delete-overlay highlight)))
      (setq org-remark-highlights nil)
      (dolist (feature-mode org-remark-default-feature-modes)
        (funcall feature-mode -1))
      (remove-hook 'org-remark-find-dwim-functions
                #'org-remark-find-overlay-at-point :local)
      (remove-hook 'after-save-hook #'org-remark-save t)
      (remove-hook 'org-remark-highlight-link-to-source-functions
                   #'org-remark-highlight-link-to-source-default)
      (remove-hook 'after-revert-hook #'org-remark-highlights-load :local)
      (remove-hook 'clone-buffer-hook #'org-remark-highlights-load :local))))


;;;; Org-remark Menu
(defvar org-remark-menu-map
  (make-sparse-keymap "Org-remark"))

(define-key-after org-remark-menu-map
  [org-remark-open]
  '(menu-item "Open" org-remark-open
              :help "Display and move to marginal notes for highlight at point"
              :enable (org-remark-find-dwim)))

(define-key-after org-remark-menu-map
  [org-remark-view]
  '(menu-item "View" org-remark-view
              :help "Display marginal notes for highlight at point; stay in current buffer"
              :enable (org-remark-find-dwim)))

(define-key-after org-remark-menu-map
  [org-remark-view-next]
  '(menu-item "View next" org-remark-view-next
              :enable org-remark-highlights))

(define-key-after org-remark-menu-map
  [org-remark-view-prev]
  '(menu-item "View previous" org-remark-view-prev
              :enable org-remark-highlights))

(define-key-after org-remark-menu-map
  [org-remark-toggle]
  '(menu-item "Toggle" org-remark-toggle
              :help "Toggle showing/hiding of highlights in current buffer"
              :enable org-remark-highlights))

(define-key-after org-remark-menu-map
  [org-remark-remove]
  '(menu-item "Remove" org-remark-remove
              :help "Remove highlight at point, keeping the marginal notes entry"
              :enable (org-remark-find-dwim)))

(define-key-after org-remark-menu-map
  [org-remark-delete]
  '(menu-item "Delete" org-remark-delete
              :help "Delete highlight at point and the marginal notes entry"
              :enable (org-remark-find-dwim)))

;; Make pen functions menu
(defvar org-remark-pen-map
  (make-sparse-keymap "Org-remark-mark"))

(define-key-after org-remark-pen-map
  [org-remark-mark]
  '(menu-item "default pen" org-remark-mark))

;; Make change pen menu
(defvar org-remark-change-pen-map
  (make-sparse-keymap "Org-remark-change"))

(define-key-after org-remark-change-pen-map
  [org-remark-change]
  '(menu-item "default pen"
              (lambda ()
                (interactive)
                (org-remark-change #'org-remark-mark))
              :enable (org-remark-pen-same-type-at-point-p nil)))

;; Add change menu to the parent menu
(define-key-after org-remark-menu-map
  [org-remark-change-pens]
  `(menu-item "Change to..." ,org-remark-change-pen-map
        :enable (org-remark-find-dwim)
  'org-remark-toggle))

;; Add pen menu to the parent menu
(define-key org-remark-menu-map
            [org-remark-pens]
            (list 'menu-item "Highlight with..." org-remark-pen-map))

;; Add all to the main menu
(define-key org-remark-mode-map
            [menu-bar org-remark]
            (list 'menu-item "Org-remark" org-remark-menu-map))

(defun org-remark-pen-same-type-at-point-p (org-remark-type)
  "Return t if the highlight's type is the same as ORG-REMARK-TYPE."
  (eql org-remark-type
       (overlay-get (org-remark-find-dwim (point)) 'org-remark-type)))


;;;; Commands

;;;###autoload
(defun org-remark-mark (beg end &optional id mode)
  "Apply face `org-remark-highlighter' to the region between BEG and END.

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region.

Return the highlight overlay.

A Org headline entry for the highlight will be created in the
marginal notes file specified by
`org-remark-notes-get-file-name'.  If the file does not exist
yet, it will be created.

When this function is called from Elisp, ID can be
optionally passed, indicating to Org-remark that it is to load an
existing highlight.  In this case, no new ID gets generated and
the highlight saved again, avoiding the unnecessary round-trip
back to the database.

MODE is also an argument which can be passed from Elisp.  It
determines whether or not highlight is to be saved in the
marginal notes file.  The expected values are nil, :load and
:change."
  (interactive (org-remark-beg-end nil)) ;; passing org-remark-type nil
    ;; FIXME
    ;; Adding "nil" is different to removing a prop
    ;; This will do for now
  (org-remark-highlight-mark beg end id mode
                             nil nil
                             (list 'org-remark-label "nil")))

(when org-remark-create-default-pen-set
  ;; Create default pen set.
  (org-remark-create "red-line"
                     `(:underline (:color "dark red" :style wave))
                     `(CATEGORY "review" help-echo "Review this"))
  (org-remark-create "yellow"
                     `(:underline "gold2")
                     `(CATEGORY "important")))

(defun org-remark-save ()
  "Save all the highlights tracked in current buffer to notes buffer.

This function is automatically called when you save the current
buffer via `after-save-hook'.

`org-remark-highlights' is the local variable that tracks every highlight
in the current buffer.  Each highlight is an overlay."
  (interactive)
  (org-remark-highlights-housekeep)
  (org-remark-highlights-sort)
  (let ((notes-buf (find-file-noselect (org-remark-notes-get-file-name)))
        (source-buf (or (buffer-base-buffer) (current-buffer))))
    (dolist (h org-remark-highlights)
      (org-remark-highlight-add h source-buf notes-buf))
    ;;; Avoid saving the notes buffer if it is the same as the source buffer
    (if (eq source-buf notes-buf)
        (set-buffer-modified-p nil)
      (with-current-buffer notes-buf
        (save-buffer)))))

(defun org-remark-open (point &optional view-only)
  "Open marginal notes file for highlight at POINT.
The marginal notes will be narrowed to the relevant headline to
show only the highlight at point.

This function creates a cloned indirect buffer for the marginal
notes file. You can edit it as a normal Org buffer. Once you have
done editing, you can simply save and kill the buffer or keep it
around. Org-remark ensures that there is only one cloned buffer
for notes file by tracking it.

The marginal notes file gets displayed by the action defined by
`org-remark-notes-display-buffer-action' (by default in a left
side window of the current frame), narrowed to the relevant
headline.

You can customize the name of the marginal notes buffer with
`org-remark-notes-buffer-name'.

By default, the cursor will go to the marginal notes buffer for
further editing. When VIEW-ONLY is \\=':view-only\\=' \(e.g.
Elisp program to pass the value), you can view the marginal notes
buffer with the cursor remaining in the current buffer.

You can open the marginal notes buffer associated with the
current buffer with `find-file' when the notes file exist and if
there is no highlight at point. Passing a single universal
argument with \\[universal-argument]\) also lets you open the
notes buffer in this way.

If you pass any other values to VIEW-ONLY, this function behaves
in the way as passing \\=':view-only\\=' to it and simply let you
view the marginal notes in a cloned indirect buffer in the
side-window (as defined by user option
`org-remark-notes-display-buffer-action')."
  (interactive "d\nP")
  (let ((ov (org-remark-find-dwim point)))
    ;; If C-u is used or the cursor is not on a highlight, we don't want
    ;; to open in a normal way but open the margnal notes buffer with
    ;; find-file.
    (if (or (eql (prefix-numeric-value current-prefix-arg) 4)
            ;; :view-only should not open the marginal notes buffer
            (and (null ov) (not (eql view-only :view-only))))
        (let ((notes-file (org-remark-notes-get-file-name)))
          (when (file-exists-p notes-file) (find-file notes-file)))
      ;; Open marginal notes normally as an indirect buffer in a side
      ;; window.
      (when-let*
          ((ov ov) ;; OV must be present here.
           (id (overlay-get ov 'org-remark-id))
           (ibuf (org-remark-notes-buffer-get-or-create))
           (cbuf (current-buffer)))
        (pop-to-buffer ibuf org-remark-notes-display-buffer-action)
        (widen)
        (when-let (p (org-find-property org-remark-prop-id id))
          ;; Somehow recenter is needed when a highlight is deleted and move to a
          ;; previous highlight.  Otherwise, the cursor is too low to show the
          ;; entire entry.  It looks like there is no entry.
          (goto-char p) (org-narrow-to-subtree) (org-end-of-meta-data t) (recenter))
        ;; Run hook with the current-buffer being the note's buffer
        (run-hooks 'org-remark-open-hook)
        ;; Avoid error when buffer-action is set to display a new frame
        (when view-only
          (select-window (get-buffer-window cbuf)))))))

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
this example:

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
If you would like to hide/show the highlights in the current
buffer, it is recommended to use this command instead of
`org-remark-mode'.  This command only affects the display of the
highlights and their locations are still kept tracked.  Toggling
off `org-remark-mode' stops this tracking completely, which will
likely result in inconsistency between the marginal notes file
and the current source buffer."
  (interactive)
  (if org-remark-highlights-hidden
      (org-remark-highlights-show)
    (org-remark-highlights-hide))
  t)

(defun org-remark-change (&optional pen)
  "Change the highlight at point to PEN.
This function will show you a list of available pens to choose
from."
  (interactive)
  (if-let* ((ov (org-remark-find-dwim))
            (id (overlay-get ov 'org-remark-id))
            (beg (overlay-start ov))
            (end (overlay-end ov)))
      (let* ((available-pens (seq-filter
                              (lambda (pen-fn)
                                (let ((type (overlay-get ov 'org-remark-type)))
                                  (eql type (function-get pen-fn 'org-remark-type))))
                              org-remark-available-pens))
             (new-pen
              (if pen pen
                (intern
                 ;; To guard against minibuffer quit error when
                 ;; the user quit without selecting any pen.
                 (unwind-protect
                     (completing-read "Which pen?:"
                                      available-pens))))))
        (org-remark-highlight-clear ov)
        (funcall new-pen beg end id :change))
    ;; if ov or any other variables are not found
    (message "No highlight here.")))

(defun org-remark-remove (point &optional delete)
  "Remove the highlight at POINT.
By default, it will remove the highlight from the source buffer
and the properties of entry from the marginal notes buffer, but
will keep the headline title and any notes in it. This is to
ensure to keep any notes you might have written intact.

Optionally, you can let this command delete the entire heading
subtree for the highlight along with the notes you have written,
by passing universal argument in DELETE. For deletion, this
command differentiates a single or double universal arguments as
follows:

- \\[universal-argument]

  This is the same behavior as function `org-remark-delete'.

  Look for notes in the entry. If there is any, the side-window
  will show them and a prompt will ask the user for confirmation.
  The function will delete the entry only when the user confirms
  with \\='y\\='. When \\='n\\=', it will only remove the entry's
  properties.

  If there are no notes in the entry, the command will delete the
  entry without the prompt.

- \\[universal-argument] \\[universal-argument]

  This is automatic deletion. This command will delete the entry
  without asking the user when there is no notes in the entry. If
  there are any notes, only the entry's properties will be
  removed. This is the same behavior as passing a single
  `universal-argument' to function `org-remark-delete'.

If you have removed or deleted a highlight by error, you can
still `undo' it in the marginal notes buffer and not in the
current buffer. This is because adding and removing overlays are
not part of the undo tree. You can undo the deletion in the
marginal notes buffer and then save it to sync the highlight back
in the source."
  (interactive "d\nP")
  (let* ((ov (org-remark-find-dwim point))
         (id (overlay-get ov 'org-remark-id)))
    (when (and ov id)
      ;; Remove the highlight overlay and id. If there is more than one,
      ;; remove only one. It should be last-in-first-out in general but
      ;; overlays functions don't guarantee it (when delete
      ;; (org-remark-open point :view-only))
      (org-remark-highlight-clear ov)
      ;; Update the notes file accordingly
      (org-remark-notes-remove id delete)
      (org-remark-highlights-housekeep)
      (org-remark-highlights-sort)
      t)))

(defun org-remark-delete (point &optional arg)
  "Delete the highlight at POINT and marginal notes for it.

This function will prompt for confirmation if there is any notes
present in the highlight's entry in the marginal notes buffer.
When it is not displayed in the current frame, it will be
temporarily displayed together with the prompt for the user to
see the notes to help with confirmation.

If there are no notes, this function will not prompt for
confirmation and will remove the highlight in the source buffer
and delete the entry in the marginal notes buffer.

This is the same behavior as passing a single `universal-argument'
to function `org-remark-remove'.

Optionally, you can pass `universal-argument' to this function
with ARG and it will behave as follows.

- \\[universal-argument]

  This is automatic deletion. Delete the entry without asking the
  user when there is no notes in the entry. If there are any
  notes, remove the entry's properties only. This is the same
  behavior as passing double universal-arguments to function
  `org-remark-remove'.

If you have removed or deleted a highlight by error, you can
still `undo' it in the marginal notes buffer and not in the
current buffer. This is because adding and removing overlays are
not part of the undo tree. You can undo the deletion in the
marginal notes buffer and then save it to sync the highlight
back in the source."
  (interactive "d\nP")
  (let ((delete (if (eql 4 (prefix-numeric-value arg))
                          '(16) ;; make it universal-arg x 2
                        :delete)))
    (org-remark-remove point delete)))


;;;; Private Functions

;;;;; org-remark-find
;;    Find a highlight (e.g. next/prev or overlay)

(defun org-remark-next-or-prev (&optional next)
  "Move cursor to the next or previous highlight if any.
When NEXT is non-nil, move to the next; for nil, to the previous.

This function is internal only and meant to be used by interactive
commands such as `org-remark-next' and `org-remark-prev'.

Return t if the cursor has moved to next/prev.
Return nil if not and outputs a message in the echo."
  (org-remark-highlights-housekeep)
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

(defun org-remark-find-dwim (&optional point)
  "Return one highlight overlay for the context.

It is a generic wrapper function to get and return as what the
context requires. This is achieved via abnormal hook that passed
the POINT as a single argument.

The highligh to be returned can be the range-highlight at point.
POINT is optional and if not passed, the current point is used.
It can also be a line-highlight for the line, which is a zero
length overlay put to the beginning of the line. For the latter,
the user's point can be anywhere."
  (or (run-hook-with-args-until-success
       'org-remark-find-dwim-functions point)
      ;; Fallback
      (org-remark-find-overlay-at-point point)))

(defun org-remark-find-overlay-at-point (&optional point)
  "Return one org-remark overlay at POINT.
When point is nil, use the current point.
If there are more than one, return CAR of the list."
  (let* ((pt (or point (point)))
         (overlays (overlays-at pt))
         found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'org-remark-id)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    (car found)))

(defun org-remark-find-overlay-in (beg end &optional id)
  "Return one org-remark overlay between BEG and END.
If there are more than one, return CAR of the list.
Optionally ID can be passed to find the exact ID match."
  (let* ((overlays (overlays-in beg end))
         found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'org-remark-id)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    (when id (setq found
                   (seq-filter
                    (lambda (ov)
                      (equal (overlay-get ov 'org-remark-id) id))
                    found)))
    (car found)))


;;;;; org-remark-highlight
;;   Private functions that work on a single highlight.  A highlight is an
;;   overlay placed on a a part of text.  With using an analogy of pens
;;   and books, a highlight is the mark you make over a part of a book
;;   with a highlighter pen or marker.
;;
;;   As highlights are overlays placed on the source buffer, the
;;   functions here mostly assume the current buffer is the source
;;   buffer.

(cl-defgeneric org-remark-highlight-make-overlay (_beg _end _face _org-remark-type)
  "Make overlay and return it.
Put FACE and other necessary properties to the highlight OV"
  (ignore))

(cl-defmethod org-remark-highlight-make-overlay (beg end face
                                                     (_org-remark-type (eql nil)))
  "Make overlay BEG END and add FACE to it.
If FACE is nil, this function uses defaul face `org-remark-highlighter'.
This is a method for highlights of default ORG-REMARK-TYPE, that
is for a character range."
  (let ((ov (make-overlay beg end nil :front-advance)))
    (overlay-put ov 'face (if face face 'org-remark-highlighter))
    ov))

(defun org-remark-highlight-mark
    (beg end &optional id mode label face properties)
  "Apply the FACE to the region selected by BEG and END.

This function will apply FACE to the selected region.  When it is
nil, this function will use the default face `org-remark-highlighter'

This function will add LABEL and PROPERTIES as overlay
properties.  PROPERTIES is a plist of pairs of a symbol and value.

Return the highlight overlay.

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region, and Org-remark will start tracking the highlight's
location in the current buffer.

MODE determines whether or not highlight is to be saved in the
marginal notes file.  The expected values are nil, :load and
:change.

An Org headline entry for the highlight will be created in the
marginal notes file specified by `org-remark-notes-get-file-name'.
If the file does not exist yet, it will be created.

When this function is called from Elisp, ID can be optionally
passed, indicating to Org-remark that it is to load an existing
highlight.  In this case, no new ID gets generated and the
highlight will not be saved again, avoiding the unnecessary
round-trip back to the notes file."
  ;; Ensure to turn on the local minor mode
  (unless org-remark-mode (org-remark-mode +1))
  ;; When highlights are toggled hidden, only the new one gets highlighted in
  ;; the wrong toggle state.
  (when org-remark-highlights-hidden (org-remark-highlights-show))
  (org-with-wide-buffer
   (let* ((org-remark-type (plist-get properties 'org-remark-type))
          (ov (org-remark-highlight-make-overlay beg end face org-remark-type))
          ;;(make-overlay beg end nil :front-advance))
          ;; UUID is too long; does not have to be the full length
          (id (if id id (substring (org-id-uuid) 0 8)))
          (filename (org-remark-source-find-file-name)))
     (if (not filename)
         (message (format "org-remark: Highlights not saved.\
 This buffer (%s) is not supported" (symbol-name major-mode)))
       ;; OV may not be created for line-highlights when the user opens
       ;; the buffer for the first time, as the window may not have been
       ;; created to display the buffer yet. This is necessary for the
       ;; margin width to be calculated.
       (when ov
         (while properties
           (let ((prop (pop properties))
                 (val (pop properties)))
             (overlay-put ov prop val)))
         (when label (overlay-put ov 'org-remark-label label))
         (overlay-put ov 'org-remark-id id)
         ;; Keep track of the overlay in a local variable. It's a list that is
         ;; guaranteed to contain only org-remark overlays as opposed to the one
         ;; returned by `overlay-lists' that lists all overlays.
         (push ov org-remark-highlights)
         ;; for mode, nil and :change result in saving the highlight.  :load
         ;; bypasses save.
         (unless (eq mode :load)
           (let* ((notes-buf (find-file-noselect
                              (org-remark-notes-get-file-name)))
                  (source-buf (current-buffer))
                  ;; Get props for create and change modes
                  (notes-props
                   (org-remark-highlight-add ov source-buf notes-buf)))
             (when notes-props
               (org-remark-highlight-put-props ov notes-props))
             ;; Save the notes buffer when not loading
             (unless (eq notes-buf (current-buffer))
               ;; Force tiggering the update save for :change:operation.
               ;; The line-icons do not get updated because :change: to
               ;; the same pen does not involve buffer modificaiton and
               ;; thus the sync does not get triggered to update icons.
               (with-current-buffer notes-buf
                 (unless (buffer-modified-p) (restore-buffer-modified-p t))
                 (save-buffer))))))
       (deactivate-mark)
       (org-remark-highlights-housekeep)
       (org-remark-highlights-sort)
       (setq org-remark-source-setup-done t)
       ;; Return overlay
       ov))))

(defun org-remark-highlight-get-title ()
  "Return the title of the source buffer.
The title is either the title keyword for an Org buffer, or the
file name of the source buffer.  When the source is not a
file (e.g. a website), it is its file name equivalent, such as
the URL for a website.

This function assumes
the current buffer is the source buffer.

Utility function to work with a single highlight overlay."
  (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
      (let* ((full-name (org-remark-source-find-file-name))
             (filename (if (and (string= "" (file-name-nondirectory full-name))
                                (string-match "[\/]+\\'" full-name))
                           ;; The name ends with a / (possibly a URL).
                           ;; Trim all the slashes at the end of the
                           ;; name.
                           (replace-match "" t t full-name)
                         full-name)))
        (if (or (null filename) (string= "" filename))
            (error "Could not extract highlight title")
            (file-name-sans-extension (file-name-nondirectory filename))))))

(defun org-remark-highlight-get-org-id (point)
  "Return Org-ID closest to POINT of the source buffer.
This function does this only when `org-remark-use-org-id' is
non-nil.  Returns nil otherwise, or when no Org-ID is found.

This function assumes the current buffer is the source buffer."
  (and org-remark-use-org-id
       (org-entry-get point "ID" :inherit)))

(define-obsolete-function-alias
  'org-remark-highlight-save 'org-remark-highlight-add "1.2.0"
  "Save a single HIGHLIGHT in the marginal notes file. We no longer
save the notes file to disk; hence the name change")

(cl-defgeneric org-remark-highlight-get-constructors ()
  "Construct lists for creating MAJOR-MODE specific hierarchy.

This is the default one. Return the value in a alist like this:

   (SOURCE-FILENAME-FN TITLE-FN PROP-TO-FIND)"
  (let* ((headline-1 (list
                      ;; SOURCE-FILENAME-FN
                      (lambda ()
                        (org-remark-source-get-file-name
                         (org-remark-source-find-file-name)))
                      ;; TITLE-FN
                      #'org-remark-highlight-get-title
                      ;; PROP-TO-FIND
                      org-remark-prop-source-file))
         (headline-constructors (list headline-1)))
    headline-constructors))

(defun org-remark-highlight-add (overlay source-buf notes-buf)
  "Add a single HIGHLIGHT in the marginal notes file.

Return the highlight's data properties list (TODO refer to ...).

FILENAME is the name of source file with which the marginal notes
buffer is associated.  When the source buffer does not visit a
file (e.g. a website), it is the source buffer's file name
equivalent, such as the URL.

OVERLAY is the highlight being saved.

SOURCE-BUF is the buffer of the source.

NOTES-BUF is the notes buffer for the source.

For the first highlight of the source buffer, this function will
create a new H1 headline for it at the bottom of the marginal
notes buffer with TITLE as its headline text.

When called for a new highlight that is unsaved in the marginal
notes file, this function will create a new H2 headline with the
highlighted text as its headline text at the end of the H1
headline for the source buffer.

If a headline with the same ID already exists, update its
position and properties named \"org-remark-*\" and CATEGORY from
the highlight overlay.  For update, the headline text will be
kept intact, because the user might have changed it to their
needs.

This function will also add a normal file link as property
\"org-remark-link\" of the H2 headline entry, pointing back to
the source file with search option \"::line-number\", or for
non-file sources, calls `run-hook-with-args-until-success' for
`org-remark-highlight-link-to-source-functions' with FILENAME as
the argument.

ORGID can be passed to this function.  If user option
`org-remark-use-org-id' is non-nil, this function will add an
Org-ID link in the body text of the headline, linking back to the
source with using ORGID.

When the current source buffer is not set up for sync with notes,
this function calls `org-remark-notes-setup' to prepare the notes
buffer for automatic sync."
  (let ((notes-props nil)
        ;; Does this have to be explicitly in with-current buffer clause?
        (headline-constructors (with-current-buffer source-buf
                                 (org-remark-highlight-get-constructors))))
    (with-current-buffer notes-buf
      (org-with-wide-buffer
       ;; Different major-mode extension may have different structure of notes file
       ;; e.g. nov.el file: 1. source file; 2. book; 3 highlight
       ;;      text file:   1. source file; 2. highlight
       ;; Note the lowest level is always the highlight (common). And
       ;; the top level is the "source" -- the file or URL, etc.
       (cl-loop for index from 1
                for (filename-fn title-fn prop-to-find) in headline-constructors
                ;; This variable "point" is set in order to be returned at
                ;; the end of the loop.
                with point = 1
                do (let (filename title)
                     (with-current-buffer source-buf
                       (setq filename (funcall filename-fn))
                       (setq title (funcall title-fn)))
                     (with-current-buffer notes-buf
                       (goto-char point)
                       (setq point
                             (or (org-find-property
                                  prop-to-find filename)
                                 (org-remark-notes-new-headline
                                  index title (list prop-to-find filename))))))
                ;; Add the hightlight/note nodes after the headline loop.
                finally (progn
                          ;; need to move to the point at the end
                          ;; of loop
                          (goto-char point)
                          ;; Highlight Headline is common to all major-mode extensions
                          (setq notes-props
                                (org-remark-highlight-add-or-update-highlight-headline
                                 overlay source-buf notes-buf))))))
    ;;; Set up notes buffer for sync for the source buffer
    (with-current-buffer source-buf
      (unless org-remark-source-setup-done
        (org-remark-notes-setup notes-buf source-buf)))
    ;;; Return notes-props
    notes-props))

(cl-defgeneric org-remark-highlight-headline-text (_ov _org-remark-type)
  "Return title text of highlight.")

(cl-defmethod org-remark-highlight-headline-text (ov (_org-remark-type (eql nil)))
  "Return title text of highlight OV of default type.
Assume it is called within `org-with-wide-buffer' of the source."
  (replace-regexp-in-string
   "\n" " "
   (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))

(defvar org-remark-highlight-other-props-functions nil
  "Abnormal hook to be run when adding or updating headline.
It is called with one argument HIGHLIGHT, which is the overlay
that represents the current highlight being worked on. The
function is run with source buffer as the current buffer.")

(defun org-remark-highlight-collect-other-props (highlight)
  "Return other properties for HIGHLIGHT.
Assume to be run in the source buffer."
  (let ((props nil))
    (dolist (fn org-remark-highlight-other-props-functions props)
      (let ((plist (funcall fn highlight)))
        (when plist
          (setq props (append props plist)))))))

(defun org-remark-highlight-add-or-update-highlight-headline (highlight source-buf notes-buf)
  "Add a new HIGHLIGHT headlne to the NOTES-BUF or update it.
Return notes-props as a property list.

HIGHLIGHT is an overlay from the SOURCE-BUF.

Assume the current buffer is NOTES-BUF and point is placed on the
beginning of source-headline, which should be one level up."
  ;; Add org-remark-link with updated line-num as a property
  (let (title beg end props id text filename link orgid org-remark-type other-props)
    (with-current-buffer source-buf
      (setq title (org-remark-highlight-get-title)
            beg (overlay-start highlight)
            end (overlay-end highlight)
            props (overlay-properties highlight)
            id (plist-get props 'org-remark-id)
            org-remark-type (overlay-get highlight 'org-remark-type)
            text (org-with-wide-buffer
                  (org-remark-highlight-headline-text highlight org-remark-type))
            filename (org-remark-source-get-file-name
                      (org-remark-source-find-file-name))
            link (run-hook-with-args-until-success
                  'org-remark-highlight-link-to-source-functions filename beg)
            orgid (org-remark-highlight-get-org-id beg)
            other-props (org-remark-highlight-collect-other-props highlight))
      ;; TODO ugly to add the beg end after setq above
      (plist-put props org-remark-prop-source-beg (number-to-string beg))
      (plist-put props org-remark-prop-source-end (number-to-string end))
      (when link (plist-put props "org-remark-link" link))
      (when other-props (setq props (append props other-props))))
    ;;; Make it explicit that we are now in the notes-buf, though it is
    ;;; functionally redundant.
    (with-current-buffer notes-buf
      (let ((highlight-headline (org-find-property org-remark-prop-id id))
            ;; Assume point is at the beginning of the parent headline
            (level (1+ (org-current-level))))
        (if highlight-headline
            (progn
              (goto-char highlight-headline)
              ;; Update the existing headline and position properties
              ;; Don't update the headline text when it already exists.
              ;; Let the user decide how to manage the headlines
              ;; (org-edit-headline text)
              (org-remark-notes-set-properties props))
          ;; No headline with the marginal notes ID property. Create a new one
          ;; at the end of the file's entry
          (org-narrow-to-subtree)
          (goto-char (point-max))
          ;; Ensure to be in the beginning of line to add a new headline
          (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
          ;; Create a headline
          ;; Add a properties
          (insert (concat (insert-char (string-to-char "*") level)
                          " " text "\n"))
          ;; org-remark-original-text should be added only when this
          ;; headline is created. No update afterwards
          (plist-put props "org-remark-original-text" text)
          (org-remark-notes-set-properties props)
          (when (and orgid org-remark-use-org-id)
            (insert (concat "[[id:" orgid "]" "[" title "]]"))))
        (list :body (org-remark-notes-get-body)
              :original-text text)))))

(defun org-remark-highlight-load (highlight)
  "Load a single HIGHLIGHT to the source buffer.

Return highlight overlay that has been loaded on the source
buffer.

Assume the current buffer is the source buffer."
  (let* ((id (plist-get highlight :id))
         (location (plist-get highlight :location))
         (beg (car location))
         (end (cdr location))
         (label (plist-get highlight :label))
         (ov nil)
         (props (plist-get highlight :props)))
    (let ((fn (intern (concat "org-remark-mark-" label))))
      (unless (functionp fn) (setq fn #'org-remark-mark))
      (setq ov (funcall fn beg end id :load))
      (when ov
        (org-remark-highlight-put-props ov props))
      ;; Return highlight overlay
      ov)))

(defun org-remark-highlight-put-props (highlight plist)
  "Put PLIST from notes Org props to HIGHLIGHT overlay properties."
  ;; TODO Generalize the part that updates properties.
  ;; :body should not be the fixed property.
  ;; '(:text (val . fn) :prop1 (val . fn) :prop2 (val .fn))
  ;; (dolist list)
  (unless (overlay-get highlight 'help-echo)
    (overlay-put highlight 'help-echo (plist-get plist :body)))
  (overlay-put highlight '*org-remark-note-body
               (plist-get plist :body))
  (overlay-put highlight '*org-remark-original-text
               (plist-get plist :original-text)))

(defun org-remark-highlight-clear (overlay)
  "Clear a single highlight OVERLAY.
It is a utility function to take care of both variabel
`org-remark-highlights' and a highlight OVERLAY at the same time."
  (setq org-remark-highlights (delete overlay org-remark-highlights))
  (delete-overlay overlay))

(defun org-remark-highlight-adjust-position-after-load (highlight text)
  "Adjust the position of a HIGHLIGHT overlay after loaded.
It searches for TEXT, which should be the original text of the highlight."
  ;; Load works. but need one for sync. Need to re-think
  ;; ' and ’ are different in regex of course.

  ;; This is probably not very good for text that you change; and change the highlights.
  ;; if you change it, this will bring it back to the "original".
  (let* ((beg (overlay-start highlight))
         (end (overlay-end highlight))
         (paragraph-beg)(paragraph-end))
    (org-with-wide-buffer
     (unless (org-remark-string= (buffer-substring-no-properties beg end) text)
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
         (move-overlay highlight (match-beginning 0) (match-end 0)))
       ;; Add property to indicate that the position has somehow been
       ;; adjusted. Even if the new location could not be found,
       ;; indicate that the fact that string should have moved.
       (overlay-put highlight '*org-remark-position-adjusted t)))))

(defun org-remark-highlight-link-to-source-default (filename point)
  "Return Org link string for FILENAME & POINT for a highlight.
POINT is used to compute the line number.

Default function for `org-remark-highlight-link-to-source-functions'."
  (if buffer-file-name
      (let ((line-num (org-current-line point)))
        (concat "[[file:" filename
                (when line-num (format "::%d" line-num)) "]]"))))

;;;;; org-remark-notes
;;    Private functions that work on marginal notes buffer (notes
;;    buffer).  Notes buffer visits an Org file, which serves as plain
;;    text database whose main purpose is to persist the location of
;;    highlights and body text for them if any. Each highlight can also
;;    hold properties related to Org-remark.  Their names are prefixed
;;    with "org-remark".  The exception to this is the Org's standard
;;    CATEGORY, which can be set as a property for Org-remark's custom
;;    highlighter pens.

;;    The tree structure of the Org file database can be flexible and
;;    defined per major mode by their respective extension. Examples
;;    include `org-remark-nov' and `org-remark-info'. Org-remark search
;;    specific Org properties to determine where to store newly created
;;    highlights. :org-remark-file: and :org-remark-id: are the most
;;    important properties that Org-remark search.

;;    - The default tree structure:

;;        H1: File (:org-remark-file:) for non-file visiting buffer, file
;;            name equivalent such as URL for a website

;;        H2: Highlight (:org-remark-id:) each one has a dedicated H2
;;            subtree

;;   - The tree structure for `org-remark-nov'

;;        H1: Book (:org-remark-nov-file:) The EPUB file, which
;;            is the zip file.

;;        H2: Chapter (:org-remark-file:) Each HTML file contained in
;;            the EPUB file

;;        H3: Highlight (:org-remark-id:)

;;    The highlight (:org-remark-id:) is the lowest level of the tree,
;;    whose parent/ancestor must be a node that has :org-remark-file:
;;    property.

;;    In general, -notes-* functions assume the current buffer is the
;;    notes buffer.  One remark on a subtlety: Org-remark clones an
;;    indirect buffer of notes buffer; this is meant to be user
;;    convenience. Users might interact with either the indirect buffer
;;    or directly with the base buffer.  For automatic sync
;;    functionality, Org-remark interacts directly with the base buffer.

(defun org-remark-notes-new-headline (level title props)
  "Add a new headline in the note buffer.

This function assumes that the point is in the notes buffer.

LEVEL is the headline level to be added. TITLE is the headline
title. PROPS is the alist of properties to be added to the headline.

Return the point of begining of current heading."
  ;; If file-headline does not exist, create one at the bottom
  (unless (= level 1)
    ;; If top node, narrowing headline results in level 2 being
    ;; prepended. By not narrowing at level 1, the new level 2
    ;; headings will be appended at the bottom of the buffer.
    (org-narrow-to-subtree))
  (goto-char (point-max))
  ;; Ensure to be in the beginning of line to add a new headline
  (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
  (insert-char (string-to-char "*") level)
  (insert (concat " " title "\n"))
  (org-remark-notes-set-properties props)
  (org-back-to-heading) (point))

(defun org-remark-notes-remove (id &optional delete)
  "Remove the note entry for highlight ID.
Return t.

By default, this function only removes the properties of the
entry, keeping the headline title and any notes in it intact.

You can pass DELETE to delete the entire entry. Elisp can pass
the following value to differentiate the deletion behavior (for
example, with commands `org-remark-remove' or
`org-remark-delete'):

- :auto-delete or a list that has a single element 16, that is
  \\='(16)\\='. The latter value is generated when the user uses
  a command with \\[universal-argument] \\[universal-argument] ::

  Delete the entry without asking the user when there is no notes
  in the entry. If there are any notes, remove the entry's
  properties only.

- Any other non-nil value ::

  Look for notes in the entry. If there is any, ask the user for
  confirmation. Delete the entire entry only when the user
  confirms with \\='y\\='. When \\='n\\=', remove the entry's
  properties only.

  If there are no notes, do not prompt for confirmation and
  delete the entry in the marginal notes buffer."
  (let* ((ibuf (org-remark-notes-buffer-get-or-create))
         (window (get-buffer-window ibuf)))
    (with-current-buffer ibuf
      (org-with-wide-buffer
       (when-let ((id-headline (org-find-property org-remark-prop-id id)))
         (goto-char id-headline)
         (org-narrow-to-subtree)
         (dolist (prop (org-entry-properties))
           (when (string-prefix-p "org-remark-" (downcase (car prop)))
             (org-delete-property (car prop))))
         (when delete
           ;; CATEGORY prop won't be deleted. Move to line after props
           (org-end-of-meta-data t)
           (let ((delete (if (eql 16 (prefix-numeric-value delete))
                             :auto-delete
                           delete))
                 (notes-exist-p (looking-at "."))
                 (ok-to-delete-p nil))
             (cond
              ;; Deletion Case 1. No notes. Auto-delete. OK to Delete
              ((and (not notes-exist-p) (eql delete :auto-delete))
               (setq ok-to-delete-p t))
              ;; Deletion Case 2. No notes. Normal delete. OK to Delete
              ((and (not notes-exist-p) (not (eql delete :auto-delete)))
               (setq ok-to-delete-p t))
              ;; Deletion Case 3. Notes exist. Auto-delete. Keep.
              ((and notes-exist-p (eql delete :auto-delete))
               (setq ok-to-delete-p nil))
              ;; Deletion Case 4. Notes exist. Normal delete. Prompt Y/N
              ((and notes-exist-p (not (eql delete :auto-delete)))
               ;; default behavior: when notes exist, ask the user
               (display-buffer ibuf
                               org-remark-notes-display-buffer-action)
               (setq ok-to-delete-p
                     (y-or-n-p "Highlight removed but notes exist.  \
Do you also want to delete the notes?"))))
             (when ok-to-delete-p
               (delete-region (point-min)(point-max))
               (message "Deleted the marginal notes entry"))))))
      (when (buffer-modified-p) (save-buffer))
      ;; Quit the marginal notes indirect buffer if it was not there
      ;; before the remove/delete -- go back to the original state.
      (when-let (ibuf-window (get-buffer-window ibuf))
        (unless window (quit-window nil ibuf-window ))))
    t))

(defun org-remark-notes-buffer-get-or-create ()
  "Return marginal notes buffer. Create one if it does not exist.
It's a cloned indirect buffer of a buffer visiting the marginal
notes file of the current buffer. This function ensures there is
only one of the marginal notes buffer per session."
  ;; Compare the target marginal notes buffer and current marginal notes buffer.
  ;; For the latter, we need the base buffer of an indirect buffer.
  (let ((cbuf (find-file-noselect (org-remark-notes-get-file-name)))
        (ibuf (when (buffer-live-p org-remark-last-notes-buffer)
                org-remark-last-notes-buffer)))
    (unless (eq (buffer-base-buffer ibuf) cbuf)
      ;; Fixed the issue of killing the main buffer when there is no
      ;; indirect buffer created yet
      (when ibuf (kill-buffer ibuf))
      (setq ibuf (make-indirect-buffer cbuf org-remark-notes-buffer-name
                                       :clone)))
    ;; set the variable and return the indirect buffer
    (setq org-remark-last-notes-buffer ibuf)))

(defun org-remark-notes-set-properties (props)
  "Set properties for the headline in the notes file.
Return t.

Minimal properties are:

- org-remark-id :: ID
- org-remark-source-beg :: BEG
- org-remark-source-end :: END

And the following are also reserved for Org-remark:
- org-remark-link

For PROPS, if the property name is CATEGORY \(case-sensitive\) or
prefixed with \"org-remark-\" set them to to headline's property
drawer.

In order to avoid adding org-remark-* overlay properties to Org
properties, add prefix \"*\"."
  ;; Delete property.  Prefer `org-entry-delete' over
  ;; `org-delete-property' as the former is silent.
  (org-entry-delete nil "CATEGORY")
  ;; TODO generalize org-entry-delete for other props so that they can
  ;; be deleted when the value previously existed and now being deleted.

  (while props
    (let ((p (pop props))
          (v (pop props)))
      (when (symbolp p) (setq p (symbol-name p)))
      (when (symbolp v) (setq v (symbol-name v)))
      (when (or (string-equal "CATEGORY" (upcase p))
                (and (> (length p) 11)
                     (string-equal "org-remark-" (downcase (substring p 0 11)))))
        (org-set-property p v))))
  t)

(defun org-remark-notes-get-body ()
  "Return the text body of a highlight in the notes buffer."
  (let ((full-text
         (save-excursion
           (org-end-of-meta-data :full)
           (if
               ;; handle empty annotation
               ;; (org-end-of-meta-data :full) took us to next org heading):
               (or (looking-at org-heading-regexp)
                   (eobp)) ;; end of buffer
               nil ;; no body text for the annotation
             (buffer-substring-no-properties
              (point)
              (org-end-of-subtree))))))
    ;; TODO Consider customizing var for the max length 200
    (if (< 200 (length full-text))
        (substring-no-properties full-text 0 200)
      full-text)))

(defun org-remark-notes-setup (notes-buf source-buf)
  "Set up NOTES-BUF and SOURCE-BUF for sync.

Note that this function adds some local variables only to the
base-buffer of the notes and not to the indirect buffer."
  (let ((base-buf (or (buffer-base-buffer notes-buf) notes-buf)))
    (with-current-buffer base-buf
      (unless (member source-buf org-remark-notes-source-buffers)
        (cl-pushnew source-buf org-remark-notes-source-buffers)
        (add-hook 'after-save-hook #'org-remark-notes-sync-with-source nil :local))))
  (with-current-buffer source-buf
    (setq org-remark-source-setup-done t)))

(defun org-remark-notes-housekeep ()
 "Remove killed buffers from `org-remark-notes-source-buffers'."
 (setq org-remark-notes-source-buffers
       (seq-filter #'buffer-live-p org-remark-notes-source-buffers)))

(defun org-remark-notes-sync-with-source ()
  "Update sources from the current notes buffer.
This function iterates through `org-remark-notes-source-buffers'
in the base buffer of the notes.

It is meant to be used in `after-save-hook'."
  ;;; Assume the current buffer is either the indirect or notes buffer
  ;;; in question.  In order for the `after-save-hook' to correctly
  ;;; triggers notes sync, we need to get the base buffer if the note
  ;;; buffer being saved is an indirect one.
  (let ((notes-buffer (or (buffer-base-buffer) (current-buffer))))
    (with-current-buffer notes-buffer
      (org-remark-notes-housekeep)
      (dolist (source-buf org-remark-notes-source-buffers)
        (with-current-buffer source-buf
          (org-remark-highlights-load :update))))
    t))


;;;;; org-remark-highlights
;;    Work on all the highlights in the current buffer

(defun org-remark-highlights-get (notes-buf)
  "Return a list of highlights from NOTES-BUF.
The file name is returned by `org-remark-notes-get-file-name'.
It is assumed that the current buffer is source buffer.  Each
highlight is a property list in the following properties:
    (:id ID :location (BEG . END) :label LABEL :props (PROPERTIES)"
  ;; Set source-file-name first, as `find-file-noselect' will set the
  ;; current-buffer to source-file-name. Issue #39 FIXME: A way to make
  ;; this sequence agnostic is preferred, if there is a function that
  ;; visit file but not set the current buffer
  (when-let ((source-file-name (org-remark-source-get-file-name
                                (org-remark-source-find-file-name)))
             (notes-buf notes-buf))
    (let ((highlights))
      (with-current-buffer notes-buf
        (when (featurep 'org-remark-convert-legacy)
          (org-remark-convert-legacy-data))
        (org-with-wide-buffer
         (let ((heading (org-find-property
                         org-remark-prop-source-file source-file-name)))
           (if (not heading)
               (message "No highlights or annotations found for %s."
                        source-file-name)
             (goto-char heading)
             ;; Narrow to only subtree for a single file.  `org-find-property'
             ;; ensures that it is the beginning of a headline
             (org-narrow-to-subtree)
             (org-show-children)
             ;; Headline levels now can be dynamically changed via
             ;; highlight-constructors.
             (while (org-at-heading-p (org-next-visible-heading 1))
               (let ((id (org-entry-get (point) org-remark-prop-id))
                     (beg (org-entry-get (point)
                                         org-remark-prop-source-beg))
                     (end (org-entry-get (point)
                                         org-remark-prop-source-end))
                     (body (org-remark-notes-get-body)))
                 ;; beg and end must exist. If either is nil
                 ;; `string-to-number' errors
                 (when (and id beg end)
                   (setq beg (string-to-number beg))
                   (setq end (string-to-number end))
                   (push (list :id id
                               :location (cons beg end)
                               :label    (org-entry-get (point) "org-remark-label")
                               :props    (list
                                          :original-text
                                          (org-entry-get (point) "org-remark-original-text")
                                          :body body))
                         highlights)))))
           highlights))))))

(defun org-remark-highlights-delay-load (window)
  "Delay load until WINDOW for current buffer is created."
  (when (windowp window)
    (remove-hook 'window-state-change-functions
                 #'org-remark-highlights-delay-load 'local)
    (org-remark-highlights-load)))

;;;###autoload
(defun org-remark-highlights-load (&optional update)
  "Visit notes file & load the saved highlights onto current buffer.
If there is no highlights or annotations for current buffer,
output a message in the echo.

Non-nil value for UPDATE is passed for the notes-source sync
process."
  (if (not (get-buffer-window))
      (add-hook 'window-state-change-functions
                #'org-remark-highlights-delay-load 95 'local)
    ;; Some major modes such as nov.el reuse the current buffer, deleting
    ;; the buffer content and insert a different file's content. In this
    ;; case, obsolete highlight overlays linger when you switch from one
    ;; file to another. Thus, in order to update the highlight overlays we
    ;; need to begin loading by clearing them first. This way, we avoid
    ;; duplicate of the same highlight.
    (org-remark-highlights-clear)
    ;; Loop highlights and add them to the current buffer
    (let (overlays) ;; highlight overlays
      (when-let* ((notes-filename (org-remark-notes-get-file-name))
                  (default-dir default-directory)
                  (notes-buf (or (find-buffer-visiting notes-filename)
                                 (find-file-noselect notes-filename)))
                  (source-buf (current-buffer)))
        (with-demoted-errors
            "Org-remark: error during loading highlights: %S"
          ;; Load highlights with demoted errors -- this makes the loading
          ;; robust against errors in loading.
          (dolist (highlight (org-remark-highlights-get notes-buf))
            (let ((ov (org-remark-highlight-load highlight)))
              (when ov (push ov overlays))))
          (unless update (org-remark-notes-setup notes-buf source-buf))
          (if overlays
              (progn (run-hook-with-args 'org-remark-highlights-after-load-functions
                                         overlays notes-buf)
                     ;; Return t
                     t)
            ;; if there is no overlays loaded, return nil
            nil))))))

(defun org-remark-highlights-clear ()
  "Delete all highlights in the buffer.

This function also set `org-remark-highlights' to nil."
  (setq org-remark-highlights nil)
  (org-with-wide-buffer
   (dolist (ov (overlays-in (point-min) (point-max)))
     (when (overlay-get ov 'org-remark-id)
       (delete-overlay ov)))))

(defun org-remark-highlights-get-positions (&optional reverse)
  "Return list of the beginning point of all visible highlights in this buffer.
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

(defun org-remark-highlights-hide ()
  "Hide highlights when toggling their visibility.
This function removes the font-lock-face of all the highlights,
and add *org-remark-hidden property with value t. It does not
check the current hidden state, thus not interactive.  Use
`org-remark-toggle' command to manually toggle the show/hide
state."
  (when-let ((highlights org-remark-highlights))
    (dolist (highlight highlights)
      ;; Faces
      (overlay-put highlight '*org-remark-face (overlay-get highlight 'face))
      (overlay-put highlight 'face nil)
      (overlay-put highlight '*org-remark-hidden t)
      (run-hook-with-args 'org-remark-highlights-toggle-hide-functions highlight))
    (setq org-remark-highlights-hidden t)))

(defun org-remark-highlights-show ()
  "Show highlights when toggling their visibility.
This function adds the font-lock-face to all the highlighted text
regions.  It does not check the current hidden state, thus not
interactive.  Use `org-remark-toggle' command to manually toggle
the show/hide state."
  (when-let ((highlights org-remark-highlights))
    (dolist (highlight highlights)
      ;; Faces
      (overlay-put highlight '*org-remark-hidden nil)
      (overlay-put highlight 'face (overlay-get highlight '*org-remark-face))
      (run-hook-with-args 'org-remark-highlights-toggle-show-functions highlight))
    (setq org-remark-highlights-hidden nil)))

(defun org-remark-highlights-housekeep ()
  "House keep the internal variable `org-remark-highlights'.

Return t.

This is a private function; house keep is automatically done on
mark, save, and remove -- before sort-highlights.

Case 1. Both start and end of an overlay are identical _and_ it's
        not a line-highlight, which is designed to be zero length
        with the start and end identical

        This should not happen when you manually mark a text
        region.  A typical cause of this case is when you delete a
        region that contains a highlight overlay.

        This also happens when EWW reloads the buffer or
        re-renders any part of the buffer.  This is because it
        removes overlays on re-render by calling
        `remove-overlays'.

Case 2. The overlay points to no buffer

        This case happens when overlay is deleted by
        `overlay-delete' but the variable not cleared."
  (dolist (ov org-remark-highlights)
    ;; Both start and end of an overlay are identical; this should not
    ;; happen when you manually mark a text region. A typical cause of
    ;; this case is when you delete a region that contains a highlight
    ;; overlay. This also happens when EWW reloads the buffer or
    ;; re-renders any part of the buffer. This is because it removes
    ;; overlays on re-render by calling `remove-overlays', which edits
    ;; the overlay-start and overlay-end properties. To guard against
    ;; this, we check if the buffer is write-able and only remove the
    ;; annotation when it is.
    (let ((org-remark-type (overlay-get ov 'org-remark-type)))
      (when (and (overlay-buffer ov)
                 (= (overlay-start ov) (overlay-end ov))
                 (org-remark-highlights-housekeep-delete-p
                  ov org-remark-type))
        ;; When the buffer is writable and visitnig a file to change it.
        ;; That is, a "normal" buffer. If it is writable and yet derived
        ;; from a special mode, we consider the case to be in the
        ;; rendering process of the mode, and thus do not want to put into
        ;; the bin as part of housekeeping.
        (when (and (not buffer-read-only)
                   (not (derived-mode-p 'special-mode)))
          ;; Buffer-size 0 happens for a package like nov.el. It erases
          ;; the buffer (size 0) and renders a new page in the same
          ;; buffer. In this case, buffer is writable.
          ;;
          ;; TODO Relying on the current major mode being derived from
          ;; special-mode may not be the best.
          ;; Removing the notes here is meant to be to automatically remove
          ;; notes when you delete a region that contains a higlight
          ;; overlay.
          (let ((id (overlay-get ov 'org-remark-id))
                (arg org-remark-notes-auto-delete))
            (org-remark-notes-remove id arg)))
        (delete-overlay ov))
      ;; Before deleting `org-remark-highlights', add a handler per
      ;; org-remark-type
      (org-with-wide-buffer
       (org-remark-highlights-housekeep-per-type ov org-remark-type))
      ;; Update `org-remark-highlights' by removing the deleted overlays
      (unless (overlay-buffer ov)
        (setq org-remark-highlights (delete ov org-remark-highlights)))))
  t)

(cl-defgeneric org-remark-highlights-housekeep-delete-p (_ov _org-remark-type)
  "Additional predicate to delete during housekeep.
Default is always t. Implement method specific to
\\='\org-remark-type\=' and return nil the highlight must be
kept.

The current buffer is source buffer."
  t)

(cl-defgeneric org-remark-highlights-housekeep-per-type (_ov _org-remark-type)
  "Housekeep highlights per type."
  (ignore))

(defun org-remark-highlights-adjust-positions (overlays _notes-buf)
  "Run dolist and delgate the actual adjustment to another function.

OVERLAYS are highlights.

Check the original text property exits and not the same as the
current highlighted text.

Meant to be set to `org-remark-highlights-after-load-functions' by mode-specific
extensions."
  (dolist (ov overlays)
    (let ((highlight-text (overlay-get ov '*org-remark-original-text)))
      ;; Check that the original text exists AND it is different to the
      ;; current text
      (when
          (and (org-remark-highlights-adjust-positions-p (overlay-get ov 'org-remark-type))
               highlight-text
               (not (org-remark-string=
                     highlight-text
                     (buffer-substring-no-properties
                      (overlay-start ov) (overlay-end ov)))))
        (org-remark-highlight-adjust-position-after-load
         ov highlight-text)))))

(cl-defgeneric org-remark-highlights-adjust-positions-p (_org-remark-type)
  "Return t if adjust-positions feature is relevant.
Default is t and evaluated per ORG-REMARK-TYPE."
  t)


;;;;; Other utilities
(defun org-remark-source-get-file-name (filename)
  "Convert FILENAME either to absolute or relative for marginal notes files.
Returns the standardized filename.

The current buffer is assumed to be visiting the source file.

FILENAME should be an absolute file name of the source file.

If FILENAME is nil, return nil."
  ;; Get the default-directory of the notes
  (when filename ; fix #23
    (with-current-buffer (find-file-noselect (org-remark-notes-get-file-name))
      (funcall org-remark-source-file-name filename))))

(cl-defgeneric org-remark-beg-end (_org-remark-type)
  "Return beg and end for default ORG-REMARK-TYPE."
  (org-remark-region-or-word))

(defun org-remark-region-or-word ()
  "Return beg and end of the active region or of the word at point.
It is meant to be used within `interactive' in place for \"r\"
key.  The \"r\" key outputs an error when no mark is set. This
function extends the behavior and looks for the word at point"
  (let ((beg (mark))
        (end (point))
        (word (bounds-of-thing-at-point 'word)))
    ;; Use word's bounds when there is no active mark or one of beg/end is
    ;; missing. The latter can happen when there is no mark is set yet.
    (unless mark-active (setq beg (car word) end (cdr word)))
    ;; Check beg end is required as the cursor may be on an empty point with no
    ;; word under it.
    (if (and beg end)
        (progn
          (when (> beg end)
            (let ((large beg))
              (setq beg end
                    end large)))
          (list beg end))
      (user-error "No region selected and the cursor is not on a word"))))

(defun org-remark-string= (s1 s2)
  "Like `string=' but remove newlines and spaces before compare.
Return t if S1 and S2 are an identical string."
  (string=
   ;; Cater to the case when the text is divided by a newline
   ;; character \n. Remove all spaces and newline chars
   ;; before comparing the strings.
   (replace-regexp-in-string "[\n ]" "" s1)
   (replace-regexp-in-string "[\n ]" "" s2)))


;;;; Footer

(provide 'org-remark)

;;; org-remark.el ends here
