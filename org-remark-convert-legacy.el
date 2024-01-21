;;; org-remark-convert-legacy.el --- Convert legacy Org-marginalia files to Org-remark -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-remark
;; Last modified: 21 January 2024
;; Created: 16 January 2022
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, marginal notes

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
;;  This file is part of Org-remark and contains a feature that helps users of
;;  Org-marginalia (former name of Org-remark) convert their legacy
;;  marginalia.org files to those compatible with Org-remark.
;;
;;  (require 'org-remark-convert-legacy-data) for `org-remark' to use
;;  `org-remark-convert-legacy-data' function to automatically convert legacy
;;  data on save and load.  Alternatively, use the same function as an
;;  interactive command on a marginalia.org buffer that contains legacy
;;  Org-marginalia data.

;;; Code:

(require 'org)
(require 'org-remark)

(defun org-remark-convert-legacy-data ()
  "Convert the legacy Org-marginalia properties to those for Org-remark.

You can call this function interactively to convert the current
buffer.  It also gets automatically triggered when you save or
load Org-remark marginal notes file if
`org-remark-convert-legacy' user option is non-nil.

This function checks whether or not there is at least one legacy entry with
property \"marginalia-source-file\" in the current buffer.

If one found, this function will:

1. Create a backup copy with the filename \"<current-file-name>_archive\"
2. Convert all \"marginalia-*\" properties to \"org-remark-*\" equivalents

- marginalia-source-file -> org-remark-file
- marginalia-id          -> org-remark-id
- marginalia-source-beg  -> org-remark-beg
- marginalia-source-end  -> org-remark-end

This assumes that all the \"marginalia-*\" properties were used
solely by Org-marginalia."
  (interactive)
  (org-with-wide-buffer
   ;; Check that there is at least one legacy entry in the current buffer
   (goto-char (point-min))
   (when (save-excursion (org-find-property "marginalia-source-file"))
     ;; Do the process only when there is at least one entry
     ;; Create a backup copy
     (let ((filename (abbreviate-file-name
                      (concat (buffer-file-name) "_archive"))))
       (write-region (point-min) (point-max) filename)
       (message (format "org-remark: created backup file %s" filename)))
     ;; Scan the whole marginal notes file
     (while (not (org-next-visible-heading 1))
       (when-let (source-file (org-entry-get (point) "marginalia-source-file"))
         (org-delete-property "marginalia-source-file")
         (org-set-property org-remark-prop-source-file source-file))
       (when-let ((id (org-entry-get (point) "marginalia-id"))
                  (beg (string-to-number
                        (org-entry-get (point)
                                       "marginalia-source-beg")))
                  (end (string-to-number
                        (org-entry-get (point)
                                       "marginalia-source-end"))))
         (org-delete-property "marginalia-id")
         (org-delete-property "marginalia-source-beg")
         (org-delete-property "marginalia-source-end")
         (org-set-property org-remark-prop-id id)
         (let ((props '()))
           (plist-put props org-remark-prop-source-beg (number-to-string beg))
           (plist-put props org-remark-prop-source-end (number-to-string end))
           (org-remark-notes-set-properties props))))
     (goto-char (point-min))
     (message (format "org-remark: Legacy \"marginalia-*\" properties updated for %s"
                      (abbreviate-file-name (buffer-file-name))))
     t)))

(provide 'org-remark-convert-legacy)

;;; org-remark-convert-legacy.el ends here
