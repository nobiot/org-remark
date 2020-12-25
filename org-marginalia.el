;;; org-marginalia.el --- Highlight text, write margin notes for any text file in Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-marginalia
;; Version: 0.0.3
;; Last Modified: 2020-12-25
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

;;;; Installation

;;;;; MELPA

;; This package is not available on MELPA. Manual installation required.

;;;;; Manual

;; Install these required packages:

;; Ensure to have Org Mode 9.4 or later (tested on 9.4).  This package uses
;; `org-collect-keywords', which does not exist in an earlier version.

;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'org-marginalia)

;;;; Usage

;; - =org-marginalia-mode= ::
;; Org-marginalia is a local minor mode. Toggle it on/off with using
;; =org-marginalia-mode=. On activating, it loads your saved highlighters from
;; the marginalia file, and enables automatic saving of highlighters. The
;; automatic saving is achieved via function =om/save= added to
;; =after-save-hook=.

;; - =om/mark= (C-c n m by default) ::
;; Select a region of text, and call =om/mark= (bound to =C-c n m= by default)
;; to highlight the region. It will generate a new ID, and start tracking the
;; location -- so you can edit text around the marked text. Do not copy and
;; paste as it will disappear and it is a bit tricky to recover the
;; highlighter. To create a new margin note entry in themarginalia file, save the buffer.

;; - =om/save= ::
;; By default, Org-marginalia creates or updates the highlighter's location
;; and text inside automatically in the marginalia file. You can manually call
;; =om/save= to manually do it (automatic process also call this command).

;; - =om/open= (C-c o by default) ::
;; Move your cursor on the highlighted text, and call =om/open= to open the
;; relevant margin notes in a separate window. Your cursor should move to the
;; marginalia buffer narrowed to the relevant margin notes entry. You can edit
;; the margin notes as a normal Org file. Once you have done editing, you can
;; simply save and close the buffer (kill or close the window) as per your
;; normal workflow. Technically, the marginalia buffer is a cloned indirect
;; buffer of the marginalia file.

;; - =om/load= ::
;; This command open the marginalia file and load the saved highlights onto
;; current buffer. If there is no margin notes for it, it will output a
;; message in the echo. Highlights tracked locally by this packages cannot
;; persist when you kill the buffer, or quit Emacs. When you re-launch Emacs,
;; ensure to turn on =org-marginalia-mode= to load the highlights. Load is
;; automatically done when you activate the minor mode.

;; - =om/remove= ::
;; This command removes the highlight at point. It will remove the highlight,
;; and remove the properties from the marginalia, but will keep the headline
;; and notes in tact.

;; - =om/next= (C-c n ] by default) ::
;; Move to the next highlight if any. If there is none below the cursor, and
;; there is a highlight above, loop back to the top one.

;; - =om/prev= (C-c n [ by default) ::
;; Move to the previous highlight if any. If there is none above the cursor,
;; and there is a highlight below, loop back to the bottom one.

;; - =om/toggle= ::
;; Toggle showing/hiding of highlighters in current buffer. It only affects
;; the display of the highlighters. When hidden, highlights' locations are
;; still kept tracked; thus, upon buffer-save the correct locations are still
;; recorded in the marginalia file.

;;;; Customizing

;; - Highlighter face can be changed via `om/highlighter'
;; - Marginalia file is defined with `om/notes-file-path'

;;;; Known Limitations

;; - Turning off minor mode does not turn off the highlighters
;;   This should not do any harm, but if you are distracted, kill the buffer
;;   (not revert) and visit it again. Toggling show/hide of highlighters is
;;   something I - would like to add later.

;; - Copy & pasting loses highlights
;;   You could manually recover it by adjusting the properties in the
;;   marginalia file.

;;;; Credits

;; To create this package, I was inspired by the following packages. I did not
;; copy any part of them, but borrowed some ideas from them -- e.g. saving the
;; margin notes in a separate file.

;; - [[https://github.com/jkitchin/ov-highlight][ov-highlight]]
;;   John Kitchin's (author of Org-ref). Great UX for markers with hydra.
;;   Saves the marker info and comments directly within the Org file as Base64
;;   encoded string. It uses overlays.

;; - [[https://github.com/bastibe/annotate.el][Annotate.el]]
;;   Bastian Bechtold's (author of Org-journal). Unique display of annotations
;;   right next to (or on top of) the text. It seems to be designed for very
;;   short annotations, and perhaps for code review (programming practice); I
;;   have seen recent issues reported when used with variable-pitch fonts
;;   (prose).

;; - [[https://github.com/tkf/org-mode/blob/master/contrib/lisp/org-annotate-file.el][Org-annotate-file]]
;;   Part of Org's contrib library. It seems to be designed to annotate a
;;   whole file in a separate Org file, rather than specific text items

;; - [[https://github.com/IdoMagal/ipa.el][InPlaceAnnotations (ipa-mode)]]
;;   It looks similar to Annotate.el above

;;; Code:

;;;; Requirements

(require 'org)
(declare-function org-id-uuid 'org-id)
(declare-function org-collect-keywords 'org)

;;;; Customization

(defgroup org-marginalia nil
  "Write margin notes (marginalia) for any text file in a
separate Org file"
  :group 'org
  :prefix "om/"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-marginalia"))

(defface om/highlighter
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#001904"))
  "Face for highlighters."
  :group 'org-marginalia)

(defcustom om/notes-file-path "marginalia.org"
  "Specify the file path for the marginalia.org file.
The default is \"./marginalia.org\", thus one marginalia file per directory.
Ensure that it is an Org file."
  :type 'string
  :group 'org-marginalia)

;;;; Variables

(defvar-local om/highlights '()
  "Keep track of all the highlights.
It is a local variable, and is a list of multiple highlights.
Each highlight is an alist of this structure:

   (id beg-marker . end-marker)

On save-buffer each highlight will be persisted in the marginalia file
(defined by `om/notes-file-path').")

(defvar om/last-notes-buffer nil
  "Stores the cloned indirect buffer for the margin notes.
It is meant to exist only one of these in each Emacs session.")

;; Const for the names of properties in Org Mode
(defconst om/prop-id "marginalia-id")
(defconst om/prop-source-file "marginalia-source-file")
(defconst om/prop-source-beg "marginalia-source-beg")
(defconst om/prop-source-end "marginalia-source-end")

;;;; Commands

;;;###autoload
(define-minor-mode org-marginalia-mode
    "Highlight text, write margin notes for any text file in Org Mode.

This command toggles Org-marginalia local minor mode. On
activation, it loads your saved highlights from the marginalia
file, and enables automatic saving of highlights.

The automatic saving is achieved via function `om/save' added
to `after-save-hook'.

Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
    :init-value nil
    :lighter " marginalia"
    :global nil
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c n o") #'om/open)
              (define-key map (kbd "C-c m") #'om/mark)
              (define-key map (kbd "C-c n ]") #'om/next)
              (define-key map (kbd "C-c n [") #'om/prev)
              map)
    (cond
     (org-marginalia-mode
      ;; Activate
      (om/load)
      (add-hook 'after-save-hook #'om/save nil t))
     (t
      ;; Deactivate
      (remove-hook 'after-save-hook #'om/save t))))

;;;###autoload
(defun om/mark (beg end &optional id)
  "Highlight the selected region (BEG and END).
When used interactively. it will generate a new ID, always
assuming it is a new highlighted text region, and start tracking
the higlight's location, so that you can edit the text around.

It will not create a marginalia entry yet. Call `om/save' to
create a new entry (it is automatic with `after-save-hook').

When this function is called from Elisp, ID can be optionally
passed. If so, no new ID gets generated.

Every highlighted texts in the local buffer is tracked by
`om/highlights' local variable. The highlght is sorted by the
beginning point in the ascending; this is useful for `om/next'
and `om/prev'."
  (interactive "r")
  ;; UUID is too long; does not have to be the full length
  (when (not id) (setq id (substring (org-id-uuid) 0 8)))
  ;; Add highlight to the text
  (org-with-wide-buffer
   (add-text-properties beg end '(font-lock-face om/highlighter))
   (add-text-properties beg end (list 'om/id id)))
  ;; Keep track in a local variable It's alist; don't forget the dot
  ;;   (beg . end)
  ;; The dot "." is imporant to make the car/cdr "getter" interface clean.
  ;; Also, `set-marker-insertion-type' to set the type t is necessary to move
  ;; the cursor in sync with the font-lock-face property of the text property.
  (push (cons id
          (cons (om/make-highlight-marker beg) (om/make-highlight-marker end)))
        om/highlights)
  (om/sort-highlights-list))

;;;###autoload
(defun om/save ()
  "Save all the highlights tracked in current buffer to marginalia file.
The marginalia file is defined in `om/notes-file-path' variable.

This funcion is automatically called when you save the buffer.
This is achieved via `after-save-hook' (added via
`org-marginalia-mode' when you activate the minor mode).

`om/highlights' is the local variable that tracks every highlight in the
current buffer. Each highlight is represented by this data structure:

   (id beg-marker . end-marker)"
  (interactive)
  (let* ((filename (buffer-file-name))
         (source-path (abbreviate-file-name filename))
         (title (or (car (cdr (assoc "TITLE" (org-collect-keywords '("TITLE")))))
                    (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
    (dolist (highlight om/highlights)
      (om/save-single-highlight highlight title source-path))))

;;;###autoload
(defun om/open (point)
  "Open the margin notes at POINT, narrowed to the relevant headline.
It creates a cloned indirect buffer of the marginalia file
\(`om/notes-file-path'\). You can edit the margin notes as a normal Org file.
Once you have done editing, you can simply save and close the buffer (kill or
close the window) as per your normal workflow.

This package ensures that there is only one cloned buffer for marginalia by
tracking it."
  (interactive "d")
  (when (buffer-live-p om/last-notes-buffer)
    (kill-buffer om/last-notes-buffer))
  (when-let ((id (get-char-property point 'om/id))
             (ibuf (make-indirect-buffer
                   (find-file-noselect om/notes-file-path) "*marginalia*" 'clone)))
    (setq om/last-notes-buffer ibuf)
    (org-switch-to-buffer-other-window ibuf)
    (widen)(goto-char (point-min))
    (when (org-find-property om/prop-id id)
      (goto-char (org-find-property om/prop-id id))
      (org-narrow-to-subtree))))

;;;###autoload
(defun om/load ()
  "Open the marginalia file and load the saved highlights onto current buffer.
If there is no margin notes for it, it will output a message in
the echo.

Highlights tracked locally by this packages cannot persist when
you kill the buffer, or quit Emacs. When you re-launch Emacs,
ensure to turn on `org-marginalia-mode' to load the highlights.
Load is automatically done when you activate the minor mode."
  (interactive)
  ;; Open the marginalia file
  ;; Read all the positions
  (when-let* ((filename (buffer-file-name))
              (margin-buf (find-file-noselect om/notes-file-path))
              (source-path (abbreviate-file-name filename)))
    ;; Get hilights: each highlighlight is stored as an alist
    ;; (id beg . end)
    ;; TODO check if there is any relevant notes for the current file
    (let ((highlights '()))
      (with-current-buffer margin-buf
        (org-with-wide-buffer
         (let ((heading (org-find-property om/prop-source-file source-path)))
           (if (not heading) (message "No marginalia written for %s." source-path)
             (goto-char (org-find-property om/prop-source-file source-path))
             ;; Narrow to only subtree for a single file
             ;; `org-find-property' ensures that it is the beginning of H1
             (org-narrow-to-subtree)
             ;; It's important that the headline levels are fixed
             ;; H1: File
             ;; H2: Higlighted region (each one has a dedicated H2 subtree)
             (while (not (org-next-visible-heading 1))
               (when-let ((id (car (org--property-local-values "marginalia-id" nil)))
                          (beg (string-to-number (car (org--property-local-values "marginalia-source-beg" nil))))
                          (end (string-to-number (car (org--property-local-values "marginalia-source-end" nil)))))
                 (push (cons id (cons beg end)) highlights)))))))
      ;; Back to the current buffer
      ;; Look highilights and add highlights to the current buffer
      (dolist (highlight highlights)
        (let ((id (car highlight))
              (beg (car (cdr highlight)))
              (end (cdr (cdr highlight))))
          (om/mark beg end id))))))

;;;###autoload
(defun om/remove (point)
  "Remove the highlight at POINT.
It will remove the highlight, and remove the properties from the
marginalia, but will keep the headline and notes."
  (interactive "d")
  (when-let* ((id (get-char-property point 'om/id))
              (mks (cdr (assoc id om/highlights))))
    ;; Remove the highlight text prop and id
    (remove-list-of-text-properties (marker-position (car mks)) (marker-position (cdr mks)) '(om/id font-lock-face))
    ;; Remove the element in the variable om/highlights
    (setq om/highlights (assoc-delete-all id om/highlights))
    (om/sort-highlights-list)
    ;; Update the marginalia note file accordingly
    (with-current-buffer (find-file-noselect om/notes-file-path)
      (org-with-wide-buffer
       (when-let ((id-headline (org-find-property om/prop-id id)))
         (goto-char id-headline)
         (org-delete-property om/prop-id)
         (org-delete-property om/prop-source-beg)
         (org-delete-property om/prop-source-end))))))

(defun om/next ()
  "Look at the current point, and move to the next highlight, if any.
If there is none below the point, but there is a highlight in the
buffer, go back to the first one."
  (interactive)
  (if (not om/highlights)
      (progn (message "No highlights present in this buffer.") nil)
    (if (om/find-next-highlight)
        (progn (goto-char (om/find-next-highlight)) t)
      (message "Nothing done. No more visible highlights exist") nil)))

(defun om/prev ()
  "Look at the current point, and move to the previous highlight, if any.
If there is none above the point, but there is a highlight in the
buffer, go back to the last one."
  (interactive)
  (if (not om/highlights)
      (progn (message "No highlights present in this buffer.")
             nil)
    (if (om/find-prev-highlight)
        (progn (goto-char (om/find-prev-highlight)) t)
      (message "Nothing done. No more visible highlights exist") nil)))

(defun om/toggle ()
  "Toggle showing/hiding of highlighters in current buffer.
It only affects the display of the highlighters. The locations
are still kept tracked; thus, upon buffer-save the correct
locations are still recorded in the marginalia file."
  (interactive)
  (when-let ((highlights om/highlights))
    ;; Check the first highlight in the buffer
    ;; If it's hidden, all hidden. Show them.
    ;; If not, all shown. Hide them.
    (if-let* ((beg (car (cdr (nth 0 highlights))))
              (hidden-p (get-char-property beg 'om/hidden)))
        (om/show)
      (om/hide))
    t))

;;;; Functions

;;;;; Private

(defun om/save-single-highlight (highlight title source-path)
  "Save a single HIGHLIGHT in the marginalia file with properties.
The marginalia file is specified by SOURCE-PATH. If headline with
the same ID already exists, update it based on the new highlight
position and highlighted text as TITLE. If it is a new highlight,
creat a new headline at the end of the buffer."
  (let* ((pos (cdr highlight))
         (beg (marker-position (car pos)))
         (end (marker-position (cdr pos)))
         ;;`org-with-wide-buffer is a macro that should work for non-Org file'
         (text (org-with-wide-buffer (buffer-substring-no-properties beg end))))
    ;; TODO Want to add a check if save is applicable here.
    (with-current-buffer (find-file-noselect om/notes-file-path)
      (org-with-wide-buffer
       (let ((file-headline (org-find-property om/prop-source-file source-path))
             (id-headline (org-find-property om/prop-id (car highlight))))
         (unless file-headline
           ;; If file-headline does not exist, create one at the bottom
           (goto-char (point-max))
           ;; Ensure to be in the beginning of line to add a new headline
           (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
           (insert (concat "* " title "\n"))
           (org-set-property om/prop-source-file source-path))
         (cond (id-headline
                (goto-char id-headline)
                ;; Update the existing headline and position properties
                (org-edit-headline text)
                (org-set-property om/prop-source-beg (number-to-string beg))
                (org-set-property om/prop-source-end (number-to-string end)))
               (t ;; No headline with the ID property. Create one
                (when-let ((p (org-find-property om/prop-source-file source-path)))
                  (goto-char p))
                (org-narrow-to-subtree)
                (goto-char (point-max))
                ;; Ensure to be in the beginning of line to add a new headline
                (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
                ;; Create a headline
                ;; Add a properties
                (insert (concat "** " text "\n"))
                (org-set-property om/prop-id (car highlight))
                (org-set-property om/prop-source-beg (number-to-string beg))
                (org-set-property om/prop-source-end (number-to-string end))
                (insert (concat "[[file:" source-path "]" "[" title "]]"))))))
      (when (buffer-modified-p) (save-buffer)))))

(defun om/make-highlight-marker (point)
  "Return marker of the insertion-type t for POINT.
The insertion-type is important in order for the highlight
position (beg and end points) in sync with the highlighted text
properties."
  (let ((marker (set-marker (make-marker) point)))
    (set-marker-insertion-type marker t)
    marker))

(defun om/list-highlights-positions (&optional reverse)
  "Return list of beg points of highlights in this buffer.
By default, the list is in ascending order.
If REVERSE is non-nil, return list in the descending order.

It also checks if the position is visible or not. Returns only
visible ones.

If none, return nil."
  (when om/highlights
    (let ((list om/highlights))
      (setq list (mapcar
                  (lambda (h)
                    (let ((p (marker-position (car (cdr h)))))
                      ;; Checking if the p is visible or not
                      (if (or
                           (> p (point-max))
                           ;; When the highlight is wihtin a visible folded
                           ;; area, this function returns 'outline
                           (org-invisible-p p))
                          nil p)))
                  list))
      (setq list (remove nil list))
      (when list
        (if reverse (reverse list) list)))))

(defun om/sort-highlights-list ()
  "Utility function to sort `om/sort-highlights'."
  (when om/highlights
    (setq om/highlights (seq-sort-by (lambda (s) (car (cdr s))) #'< om/highlights))))

(defun om/find-next-highlight ()
  "Return the beg point of the next highlight.
Look through `om/highlights' list."
  (when-let ((points (om/list-highlights-positions)))
      ;; Find the first occurance of p > (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `om/highlights' is sorted in the ascending order (it is).
    (seq-find (lambda (p) (> p (point))) points (nth 0 points))))

(defun om/find-prev-highlight ()
  "Return the beg point of the previous highlight.
Look through `om/highlights' list (in descending order)."
  (when-let ((points (om/list-highlights-positions 'reverse)))
      ;; Find the first occurance of p < (point). If none, this means all the
      ;; points occur before the current point. Take the first one. Assume
      ;; `om/highlights' is sorted in the descending order .
    (seq-find (lambda (p) (< p (point))) points (nth 0 points))))

(defun om/hide ()
  "Hide highlighters.
It will remove the font-lock-face of all the highlights, and add
'om/hidden property with value 't. It does not check the current
hidden state, thus not interactive. Use `om/toggle-display'
command to manually toggle the show/hide state."
  (when-let ((highlights om/highlights))
    (dolist (highlight highlights)
      (let ((beg (car (cdr highlight)))
            (end (cdr (cdr highlight))))
        (remove-list-of-text-properties beg end '(font-lock-face))
        (add-text-properties beg end (list 'om/hidden t))))
    t))

(defun om/show ()
  "Show highlighters.
It adds the font-lock-face to all the highlighted text regions.
It does not check the current hidden state, thus not interactive.
Use `om/toggle-display' command to manually toggle the show/hide
state."
  (when-let ((highlights om/highlights))
    (dolist (highlight highlights)
      (let ((beg (car (cdr highlight)))
            (end (cdr (cdr highlight))))
        (remove-list-of-text-properties beg end '(om/hidden))
        (add-text-properties beg end '(font-lock-face om/highlighter))))
    t))

;;;; Footer

(provide 'org-marginalia)

;;; org-marginalia.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; require-final-newline: t
;; sentence-end-double-space: nil
;; eval: (setq-local om/notes-file-path "README.org")
;; eval: (if (find-library "org-marginalia")(progn (require 'org-marginalia)(org-marginalia-mode 1)))
;; End:
