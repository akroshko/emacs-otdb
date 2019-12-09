;;; otdb-table.el --- Create a database using an org-mode table and
;;; calculate similar to a spreadsheet.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20191209
;; URL: https://github.com/akroshko/emacs-otdb
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;;
;; Use this file with the command "emacs -q --load
;; otdb-sample-init.el".  See the included README.md file for more
;; information on this package.
;;
;; Features that might be required by this library:
;;
;; Standard Emacs features, to be documented specificly later.  Also
;; requires features from https://github.com/akroshko/cic-emacs-common,
;; using (require 'cic-emacs-common) is sufficient.
;;
;;; Code:

(require 'xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database/table keys
;; modes, macros, and utility commands

(defun otdb-table-skeleton-map (map)
  ;; TODO: placeholder, non-existant function
  (define-key map (kbd "s-d !") 'otdb-table-agenda-attention)
  (define-key map (kbd "s-d *") 'otdb-table-recalculate)
  ;; TODO: placeholder, non-existant function
  (define-key map (kbd "s-d +") 'otdb-recipe-push-to-agenda)
  ;; update the "agenda" with the key
  (define-key map (kbd "s-d a") 'otdb-table-agenda-check-add-key)
  ;; goto key in main database
  (define-key map (kbd "s-d d") 'otdb-table-goto-key-in-database)
  ;; XXXX: this makes sense to do on a new row or checklist item, or to complete something
  (define-key map (kbd "s-d i") 'otdb-table-insert-key)
  ;; TODO: why is this commented out
  ;; (define-key map (kbd "s-d u") 'otdb-table-update-key-in-database)
  ;; TODO: find occurences in database and collections
  ;; (define-key map (kbd "s-d o") 'otdb-table-occurences-key)
  ;; put key into main database, ask for key, update key at point if necessary
  (define-key map (kbd "s-d p") 'otdb-table-put-key-in-database)
  (define-key map (kbd "s-d s") 'otdb-table-calc-in-special-buffer)
  ;; update key from main database
  ;; TODO want to be able to go and pop back
  ;; update the "agenda" with the key
  (define-key map (kbd "s-d u") 'otdb-table-agenda-uncheck-key)
  ;; TODO: figure out better key scheme
  (define-key map (kbd "H-t") 'otdb-table-set-toggle-check-line)
  (define-key map (kbd "H-T") 'otdb-table-invalid-toggle-check-line)
  ;; TODO: I use these for other things now.... but good on laptops
  ;;       change somewhere?
  ;; (define-key map (kbd "H-<up>") 'otdb-table-increment-line)
  ;; (define-key map (kbd "H-<down>") 'otdb-table-decrement-line)
  ;; (define-key map (kbd "H-M-<up>") nil)
  ;; (define-key map (kbd "H-M-<down>") nil)
  (define-key map (kbd "H-m") 'otdb-toggle-tablet-mode)
  map)


(defun otdb-table-detect ()
  "Detect whether we are in a buffer with otdb-tables.  Users should modify this file to meet their file structure.
May eventually be generalized a little better."
  (let (;; (current-directory (file-name-base (directory-file-name default-directory)))
        (current-buffer (current-buffer)))
    (cond ((with-current-buffer-min current-buffer
             (re-search-forward "^\\*.*:recipe:" nil t))
           'recipe)
          ((with-current-buffer-min current-buffer
             (re-search-forward "^\\*.*:gear:" nil t))
           'backpacking)
          (t
           nil))))

(defun otdb-table-calc-in-special-buffer ()
  "Calculate in a special buffer."
  ;; TODO: try this out
  (interactive)
  (cond ((eq (otdb-table-detect) 'backpacking)
         (otdb-gear-calc-in-special-buffer-all))
        ((eq (otdb-table-detect) 'recipe)
         (otdb-recipe-calc-in-special-buffer-all))))

(defun otdb-toggle-tablet-mode ()
  "A mode where the otdb-table-mode buffer is read-only, except
for the effects of certain commands. These special commands are
generally for checking off items."
  (interactive)
  (when (otdb-table-buffer-p)
    (if otdb-table-tablet-mode
        (progn
          (face-remap-remove-relative otdb-old-modeline-color)
          (face-remap-remove-relative otdb-old-modeline-color-inactive)
          (setq otdb-table-tablet-mode nil)
          ;; TODO: want to make sure buffer stays read-only if it has been made so for other reasons?
          (read-only-mode -1))
      (progn
        (setq-local otdb-old-modeline-color          (face-remap-add-relative 'mode-line          :background "green"))
        (setq-local otdb-old-modeline-color-inactive (face-remap-add-relative 'mode-line-inactive :background "red"))
        (setq otdb-table-tablet-mode t)
        (read-only-mode t)))))

(defun otdb-table-buffer-p ()
  "Check if we are in an buffer otdb-table can be functional."
  (and (derived-mode-p 'org-mode)
       (save-excursion (goto-char (point-min))
                       ;; assume two spaces in front of TBLEL
                       (re-search-forward "^  #\\+TBLEL:" nil t))
       t))

;; TODO: better detect here
(defun otdb-setup-hook ()
  "Put into appropriate mode if an appropriate otdb file is
detected."
  (when (otdb-table-buffer-p)
    (let ((otdb-detect (otdb-table-detect))
          (current-filename (ignore-errors buffer-file-name)))
      (cond ((eq otdb-detect 'backpacking)
             (otdb-gear-mode))
            ((eq otdb-detect 'recipe)
             (otdb-recipe-mode))))))

(add-hook 'find-file-hook 'otdb-setup-hook)

;; TODO: is there a better way to do this?
(defmacro otdb-table-inhibit-read-only (&rest body)
  "Macro to inhibit read-only for certain commands when using
otdb tablet mode."
  `(when (and (boundp 'otdb-table-tablet-mode) (otdb-table-buffer-p))
     (if otdb-table-tablet-mode
         (progn
           (toggle-read-only -1)
           ,@body
           (toggle-read-only t))
       (progn
         ,@body))
     t))

(defun otdb-table-inhibit-read-only-advice (orig-fun &rest args)
  "Advice to help inhibit inhibit read-only for some existing
commands when using otdb tablet mode."
  (let (return-value)
    (if (and (boundp 'otdb-table-tablet-mode) otdb-table-tablet-mode)
        (progn
          (toggle-read-only -1)
          (setq return-value (apply orig-fun args))
          (toggle-read-only t))
      (setq return-value (apply orig-fun args)))
    return-value))

;; ensure C-c C-c works in tablet mode
(advice-add 'org-ctrl-c-ctrl-c :around #'otdb-table-inhibit-read-only-advice)
(advice-add 'tblel-eval        :around #'otdb-table-inhibit-read-only-advice)

(defvar otdb-table-collections-cache
  nil
  "Variable to store list of collections (the org-mode tables
  acting like spreadsheets).")

(defvar otdb-table-database-cache
  nil
  "Cache of the database table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table variables that can be bound
(defun otdb-table-reset-cache ()
  "Reset the cache of information stored.  This is generally done
at the beginning of commands."
  (setq otdb-table-collections-cache nil
        otdb-table-database-cache    nil))

(defun otdb-table-update (arg database heading collection-files lookup-function insert-function message-buffer)
  "Update otdb tables in current context.

DATABASE and HEADING gives the database.  COLLECTION-FILES gives
the files representing collections.  LOOKUP-FUNCTION and
INSERT-FUNCTION are helper functions.  MESSAGE-BUFFER is the
buffer where messages are put."
  (otdb-table-inhibit-read-only
   (cond ((equal arg '(4))
          (save-excursion
            (point-min)
            (org-table-map-tables (lambda () (otdb-table-update nil database heading collection-files lookup-function insert-function message-buffer)))))
         ((equal arg '(16))
          (dolist (collection-file collection-files)
            (with-current-file-transient-min collection-file
              (otdb-table-update '(4) database heading collection-files lookup-function insert-function message-buffer))))
         ((equal arg '(64))
          (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer)
          (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer)
          (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer))
         (t
          (let ((table-filename buffer-file-name)
                (table-lisp (cic:org-table-to-lisp-no-separators))
                (table-heading (save-excursion (org-back-to-heading) (cic:get-headline-text (cic:get-current-line))))
                looked-up)
            ;; writing table-lookup functions is good
            (setq looked-up (funcall lookup-function table-lisp))
            ;; get columns from org-table-lisp
            (funcall insert-function table-filename table-heading looked-up))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some commands and associated functions for dealing with the check column
;; TODO: some are unused for now, but keep

(defun otdb-table-set-line (the-column &optional the-char)
  "Set the column corresponding to THE-COLUMN for the current row.
Works even in otdb-table table mode."
  (unless the-char
    (setq the-char "X"))
  (otdb-table-inhibit-read-only
   (when (otdb-table-buffer-p)
     (when (and (org-table-check-inside-data-field t) (> (org-table-current-line) 1))
       (let ((the-column (otdb-table-char-find-column the-column)))
         ;; find appropraite column in header and record
         (when the-column
           (org-table-put nil the-column the-char)))
       ;; TODO: only align if interactive
       (org-table-align)))))

(defun otdb-table-set-check-line ()
  "Mark the check column of the current table row.  Works even
in otdb-table table mode."
  (interactive)
  (otdb-table-set-line "X"))

(defun otdb-table-unset-line (the-column)
  "Unset the column corresponding to THE-COLUMN for the current
row.  Works even in otdb-table table mode."
  (otdb-table-inhibit-read-only
   (when (and (otdb-table-buffer-p) (org-table-check-inside-data-field t) (> (org-table-current-line) 1))
     (let ((the-column (otdb-table-char-find-column the-column)))
       ;; find X column in header and record
       (when the-column
         (org-table-put nil the-column " ")))
     ;; TODO: only align if interactive
     (org-table-align))))

(defun otdb-table-unset-check-line ()
  "Unmark the check column of the current table row.  Works even
in otdb-table table mode."
  (interactive)
  (otdb-table-unset-line "X"))

(defun otdb-table-invalid-check-line ()
  "Unmark the check column of the current table row as
invalid.  Works even in otdb-table table mode."
  (interactive)
  (otdb-table-set-line "X" "-"))

(defun otdb-table-set-toggle-line (the-column &optional the-char)
  "Toggle the column corresponding to THE-COLUMN for the current row.
Works even in otdb-table table mode."
  (interactive)
  (unless the-char
    (setq the-char "X"))
  ;; TODO: add proper save-excursion
  (otdb-table-inhibit-read-only
   (when (otdb-table-buffer-p)
     (org-table-goto-column 1)
     (when (and (org-at-table-p) (org-table-check-inside-data-field t) (> (org-table-current-line) 1))
       ;; find X column in header and record
       (let* ((x-column (otdb-table-char-find-column the-column))
              (x-column-value (s-trim-full-no-properties (org-table-get nil x-column))))
         (when (and x-column (or (string= x-column-value the-char)
                                 (string= x-column-value "")))
           (if (string= x-column-value "")
               (org-table-put nil x-column the-char)
             (org-table-put nil x-column " "))))
       ;; TODO: only align if interactive
       (org-table-align)))))

(defun otdb-table-set-toggle-check-line ()
  "Toggle the check column of the current table row.  Works even
in otdb-table table mode."
  (interactive)
  (otdb-table-set-toggle-line "X"))

(defun otdb-table-invalid-toggle-check-line ()
  "Toggle the mark column of the current table row as
valid/invalid.  Works even in otdb-table table mode."
  (interactive)
  (otdb-table-set-toggle-line "X" "-"))

(defun otdb-table-set-toggle-cost-line ()
  "Toggle the mark column of the current table row with a \"C\"
  for cost.  Works even in otdb-table table mode."
  (interactive)
  (otdb-table-set-toggle-line "X" "C"))

;; TODO: this is probably non-functional and needs updating
(defun otdb-table-decrement-line (&optional arg)
  "Decrement check on current line in otdb-table."
  (interactive "P")
  (otdb-table-inhibit-read-only
   (cond ((equal arg '(4))
          (otdb-table-increment-line arg))
         ((null arg)
          (otdb-table-increment-line -1))
         (t
          (otdb-table-increment-line (- arg))))))

;; TODO: this is probably non-functional and needs updating
(defun otdb-table-increment-line (&optional arg)
  "Increment check on current line in otdb-table."
  (interactive "P")
  ;; TODO: add proper save-excursion
  ;; TODO: toggles becoming more difficult
  (let ((the-char "X"))
    (otdb-table-inhibit-read-only
     (when (otdb-table-buffer-p)
       (org-table-goto-column 1)
       (when (and (org-at-table-p)
                  (org-table-check-inside-data-field t)
                  (> (org-table-current-line) 1))
         ;; find X column in header and record
         (let* ((x-column (otdb-table-char-find-column the-char))
                (x-column-value (s-trim-full-no-properties (org-table-get nil x-column))))
           (when (and (x-column
                       (or
                        (string= x-column-value "")
                        (cic:string-integer-p x-column-value))))
             ;; if current value goes to zero
             ;; get number corresponding to arg?
             ;; TODO: reverse arg properly if
             (let ((change (cond ((equal arg '(4))
                                  nil)
                                 (arg
                                  arg)
                                 (t
                                  1))))
               (org-table-put nil x-column (otdb-table-increment-string x-column-value change)))))
         ;; TODO: only align if interactive
         (org-table-align))))))

(defun otdb-table-increment-string (string change)
  "Appropriately increment an integer string."
  ;; TODO: This is more general than just otdb-table
  (if change
      (cond ((string= string "")
             (number-to-string change))
            (t
             (let ((new-string (number-to-string (+ (string-to-number string) change))))
               (if (string= new-string "0")
                   ""
                 new-string))))
    ""))

(defun otdb-table-char-find-column (column-char)
  "Find if and where a column with the single character name of
name COLUMN-CHAR exists in the org-table at point."
  ;; TODO: This is more general than just otdb-table
  (when (org-at-table-p)
    ;; found converting to lisp-table was easiest way to do this
    (let ((lisp-table (cic:org-table-to-lisp-no-separators))
          (current-column 1)
          found-column)
      (dolist (lisp-element (car lisp-table))
        (when (and
               (not found-column)
               (string= column-char (s-trim-full-no-properties lisp-element)))
          (setq found-column current-column))
        (setq current-column (1+ current-column)))
      found-column)))

;; TODO: deduplication with above function
(defun otdb-table-lisp-char-find-column (lisp-table column-char)
  "Find if and where a column with the single character name of
name COLUMN-CHAR exists in the org-table in Lisp representation
given by LISP-TABLE."
  ;; found converting to lisp-table was easiest way to do this
  (let ((current-column 1)
        found-column)
    (dolist (lisp-element (car lisp-table))
      (when (and
             (not found-column)
             (string= column-char (s-trim-full-no-properties lisp-element)))
        (setq found-column current-column))
      ;; TODO: change this to zero index
      (setq current-column (1+ current-column)))
    found-column))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other?

(defun otdb-table-recalculate (&optional arg)
  "Recalculate an otdb table.  ARG gets passed to the function
otdb-table-update and may cause multiple tables in multiple files
to be updated."
  (interactive "P")
  (otdb-table-reset-cache)
  (otdb-table-inhibit-read-only
   (let ((table-detect (otdb-table-detect)))
     (cond ((eq table-detect 'recipe)
            (otdb-table-update arg
                               (otdb-recipe-get-variable 'otdb-recipe-database)
                               (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                               (otdb-recipe-get-variable 'otdb-recipe-files)
                               'otdb-recipe-lookup-function
                               'otdb-recipe-insert-function
                               (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
           ((eq table-detect 'backpacking)
            (otdb-table-update arg
                               otdb-gear-database
                               otdb-gear-database-headline
                               otdb-gear-collection-files
                               'otdb-gear-lookup-function
                               'otdb-gear-insert-function
                               otdb-gear-message-buffer))
           (t
            (error "Not in valid file!"))))))

(defun otdb-table-number (value)
  "Read from string VALUE and convert it to a number.  This
function trims whitespace and excludes things such as units."
  ;; match numbers, trim whitespace
  (when value
    ;; deal with fractions
    (when (string-match "[0-9.]" value)
      (if (string-match "/" value)
          (let ((space-split (split-string value " "))
                (slash-part nil)
                (slash-split nil)
                first
                second)
            ;; find the part with the slash
            (dolist (part space-split)
              (when (or (not slash-part) (string-match "/" part))
                (when first
                  (setq second t))
                (unless second
                  (setq first t))
                (setq slash-part part)))
            (setq slash-split (split-string slash-part "/"))
            (if second
                (+ (string-to-float (elt space-split 0)) (/ (cic:string-to-float (elt slash-split 0))
                                                            (cic:string-to-float (elt slash-split 1))))
              (/ (cic:string-to-float (elt slash-split 0))
                 (cic:string-to-float (elt slash-split 1)))))
        (cic:string-to-float value)))))

(defun otdb-table-unit (value)
  "Read from string VALUE and get the unit as a string."
  ;; match just the letters, trim any whitespace
  (let (unit-string)
    (and (string-match "\\([A-Za-z]+\\)" value)
         (setq unit-string (match-string 1 value)))
    unit-string))

(defun otdb-table-unit-type (quantity-entry)
  "Detect the type of units (weight or volume) from the string
QUANTITY-ENTRY."
  ;; TODO: need exact word string match to avoid l<-->lb confusion
  ;; TODO: use the weight and volume tables instead
  (if quantity-entry
      (cond ((or
              (string-match "kg" quantity-entry)
              (string-match "lb" quantity-entry)
              (string-match "g" quantity-entry))
             'weight)
            ((or
              (string-match "L" quantity-entry)
              (string-match "oz" quantity-entry)
              (string-match "cup" quantity-entry)
              (string-match "tbsp" quantity-entry)
              (string-match "tsp" quantity-entry)
              (string-match "ml" quantity-entry))
             'volume))
    nil))

(defun otdb-table-unit-conversion (unit-type ingredient-unit to-unit)
  "Convert units of type UNIT-TYPE from INGREDIEINT-UNIT units to
TO-UNIT units."
  (if ingredient-unit
      (let ((unit-table (if (eq unit-type 'weight)
                            otdb-table-weight-table
                          otdb-table-volume-table)))
        (eval (cadr (assoc to-unit (cadr (assoc ingredient-unit unit-table))))))
    1.0))

;; TODO: autogenerate these alists or do better than this hard-coded table

;; TODO: dig up some of my references and add them here
(defconst otdb-table-weight-table
  '(("kg" (("kg" 1.0)
           ("lb" (/ 1.0 0.45359237))
           ("g" 1000.0)))
    ("lb" (("kg" 0.45359237)
           ("lb" 1.0)
           ("g" 453.59237)))
    ("g" (("kg" 0.001)
          ("lb" (/ 1.0 453.59237))
          ("g" 1.0))))
  "Conversion alist for weight units.")

(defconst otdb-table-volume-table
  '(("L" (("L" 1.0)
          ("cup" (/ 1000.0 240.0))
          ("oz" (/ 1000.0 30.0))
          ("tbsp" (/ 1000.0 15.0))
          ("tsp" (/ 1000.0 5.0))
          ("ml" 1000.0)))
    ;; for the purposes of otdb, oz's are volume/fluid ounces only
    ;; TODO: if weight oz's are eventually desired, use more focused units (like avoirdupois or troy ounces)
    ("oz" (("L" (/ 30.0 1000.0))
           ("cup" (/ 1.0 8.0))
           ("oz" 1.0)
           ("tbsp" 2.0)
           ("tsp" 6.0)
           ("ml" 30.0)))
    ("cup" (("L" (/ 240.0 1000.0))
            ("cup" 1.0)
            ("oz" 8.0)
            ("tbsp" 16.0)
            ("tsp" 48.0)
            ("ml" 240.0)))
    ("tbsp" (("L" (/ 15.0 1000.0))
             ("cup" (/ 1.0 16.0))
             ("oz" (/ 1.0 2.0))
             ("tbsp" 1.0)
             ("tsp" 3.0)
             ("ml" 15.0)))
    ("tsp" (("L" (/ 3.0 1000.0))
            ("cup" (/ 1.0 48.0))
            ("oz" (/ 1.0 6.0))
            ("tbsp" (/ 1.0 3.0))
            ("tsp" 1.0)
            ("ml" 5.0)))
    ("ml" (("L" (/ 1.0 1000.0))
           ("cup" (/ 1.0 240.0))
           ("oz" (/ 1.0 30.0))
           ("tbsp" (/ 1.0 15.0))
           ("tsp" (/ 1.0 5.0))
           ("ml" 1.0))))
  "Conversion alist for volume units.")

;; TODO: probably broken
(defun otdb-table-get-key-at-point ()
  "Get the KEY from the current table, works for both collections and databases.
TODO: Fix some of the hardcoding here.
TODO: probably want an error if not at proper table"
  (let (key
        (line (cic:get-current-line))
        (column 2)
        (fname-nondirectory (file-name-nondirectory buffer-file-name)))
    (when (or
           (equal fname-nondirectory "food-database.org")
           (equal fname-nondirectory "gear-database.org"))
      (setq column 1))
    (cond ((org-at-table-p)
           ;; get first column of current row
           (setq key (s-trim-full (org-table-get nil column))))
          ((cic:org-list-p line)
           (when (string-match cic:list-regexp line)
             (setq key (match-string 3 line))))
          (t
           (error "Not in valid file!")))
    (s-trim-full-no-properties key)))

(defun otdb-table-insert-key-database (new-key)
  "Insert NEW-KEY into the database.
TODO: document further and remove hardcoding."
  ;; XXXX assumes proper checks have already been made before it
  ;; modifies database
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           ;; TODO: will need to change for multiple files
           (with-current-file-transient-org-table (otdb-recipe-get-variable 'otdb-recipe-database)
                                                  (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                                                  ;; go to last row
                                                  (cic:org-table-last-row)
                                                  ;; insert the new key
                                                  (org-table-insert-row '(4))
                                                  (insert new-key)
                                                  (org-table-align)))
          ((eq table-detect 'backpacking)
           (with-current-file-transient-org-table otdb-gear-database otdb-gear-database-headline
                                                  ;; go to last row
                                                  (cic:org-table-last-row)
                                                  ;; insert the new key
                                                  (org-table-insert-row '(4))
                                                  (insert new-key)
                                                  (org-table-align)))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-insert-key-at-point (new-key)
  "Insert a key at point.
TODO: document further and remove hardcoding."
  (let ((line (cic:get-current-line)))
    (cond ((org-at-table-p)
           ;; insert new-key into column 1
           (org-table-put nil 2 new-key))
          ((cic:org-plain-list-p line)
           (move-beginning-of-line 1)
           (re-search-forward "[-+*]" nil t)
           (cic:kill-line-elisp)
           ;; insert new-key
           (insert (concat " " new-key))
           (org-table-align))
          ((cic:org-checkbox-p line)
           ;; go to end of checkbox
           (move-beginning-of-line 1)
           (search-forward "" nil t)
           ;; kill till end of line
           (cic:kill-line-elisp)
           ;; insert new-key
           (insert (concat " " new-key)))
          (t
           (error "Not in valid file!")))))

(setq otdb-recipe-key-history nil
      otdb-gear-key-history   nil)
(defun otdb-table-agenda-check-add-key ()
  "Add a key from a database to an agenda."
  (interactive)
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           ;; check context to determine, or select if context cannot be determined
           (otdb-recipe-add-check))
          ((eq table-detect 'backpacking)
           (error "Not in valid file!"))
          (t
           (error "Not in valid file!")))))

;; TODO: appears broken
(defun otdb-table-goto-key-in-database (&optional arg)
  "Goto a key in a database.
XXXX: ARG does nothing for now."
  (interactive "P")
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (let ((ingredient-list (append
                                   (otdb-recipe-get-ingredients)
                                   (otdb-recipe-get-recipes))))
             (cic:goto-location
              ;; TODO want to find recipes too
              (otdb-recipe-find-ingredient
               (completing-read "Ingredient: " ingredient-list nil nil (otdb-table-get-key-at-point) 'otdb-recipe-key-history)))))
          ((eq table-detect 'backpacking)
           (let ((item-list (otdb-gear-get-items)))
             (cic:goto-location
              ;; TODO want to find collections too
              (otdb-gear-find-item
               (completing-read "Item: " item-list nil nil (otdb-table-get-key-at-point) 'otdb-gear-key-history)))))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-insert-key (&optional arg)
  "Insert a key into a database.
TODO: Document usage further."
  (interactive "P")
  (otdb-table-reset-cache)
  (otdb-table-inhibit-read-only
   (let ((table-detect (otdb-table-detect)))
     (cond ((eq table-detect 'recipe)
            (let* ((line (cic:get-current-line))
                   (completion-list (if (equal arg '(4))
                                        (otdb-recipe-get-ingredients)
                                      (append (otdb-recipe-get-ingredients) (otdb-recipe-get-recipes))))
                   (ingredient (completing-read "Ingredient: " completion-list nil nil (otdb-table-get-key-at-point) 'otdb-recipe-key-history)))
              (cond ((derived-mode-p 'org-mode)
                     ;; add a new checkbox
                     (otdb-table-insert-key-at-point ingredient))
                    (t
                     (error "Not in valid file!")))))
           ((eq table-detect 'backpacking)
            (let* ((line (cic:get-current-line))
                   (completion-list (if (equal arg '(4))
                                        (otdb-gear-get-items)
                                      (append (otdb-gear-get-items) (otdb-gear-get-collections))))
                   (item (completing-read "Items: " completion-list nil nil (otdb-table-get-key-at-point) 'otdb-gear-key-history)))
              (cond ((derived-mode-p 'org-mode)
                     (otdb-table-insert-key-at-point item))
                    (t
                     (error "Not in valid file!"))))))))
  (org-table-align))

;; TODO: select file
(defun otdb-table-put-key-in-database ()
  "Put a new key into a database based on point.
TODO: Document usage further."
  (interactive)
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           ;; get the key at point, don't complete from ingredients
           ;; this function is generally run after it is determined there are no ingredients
           (let ((at-point (otdb-table-get-key-at-point))
                 new-key
                 (ingredient-list (otdb-recipe-get-ingredients)))
             (setq new-key (completing-read "Ingredient: " ingredient-list nil nil at-point))
             ;; TODO make sure key is not already in database
             (if (not (member new-key ingredient-list))
                 (progn
                   (unless (equal new-key at-point)
                     (otdb-table-get-key-at-point new-key))
                   ;; adds a row to the database
                   (otdb-table-insert-key-database new-key))
               (cic:mpp-echo (concat new-key " already in database!") (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))))
          ((eq table-detect 'backpacking)
           ;;
           ;;
           (let ((at-point (otdb-table-get-key-at-point))
                 new-key
                 (item-list (otdb-gear-get-items)))
             (setq new-key (completing-read "Item: " item-list nil nil at-point))
             ;; TODO make sure key is not already in database
             (if (not (member new-key item-list))
                 (progn
                   (unless (equal new-key at-point)
                     (otdb-table-get-key-at-point new-key))
                   ;; add a new row to the database
                   (otdb-table-insert-key-database new-key))
               (cic:mpp-echo (concat new-key " already in database!") (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-agenda-uncheck-key ()
  "Uncheck a database key from an agenda."
  (interactive)
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (otdb-recipe-uncheck))
          ((eq table-detect 'backpacking)
           (error "Not in valid file!"))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-update-key-in-database ()
  "Update a database key everywhere.
TODO: Needs further documentation."
  (interactive)
  (otdb-table-reset-cache)
  (error)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (let ((at-point (otdb-table-get-key-at-point))
                 (ingredient-list (otdb-recipe-get-ingredients)))
             (otdb-table-insert-database
              (completing-read "Ingredient: " ingredient-list nil nil nil 'otdb-table-key-history))))
          ((eq table-detect 'backpacking)
           (error "Not in valid file!"))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-lisp-row-check (lisp-row index)
  "Check that the INDEX of a particular Lisp table-row LISP-ROW
is a non-zero float"
  (ignore-errors (/= (cic:string-to-float-empty-zero (elt lisp-row index)) 0.0)))

(defun otdb-table-lisp-row-float (lisp-row index)
  "From LISP-ROW get the float from INDEX."
  (cic:string-to-float-empty-zero (replace-regexp-in-string "\\$" "" (elt lisp-row index))))

(defun otdb-table-format-number-nil (num decimal-places)
  "Format a number NUM with DECIMAL-PLACES or return an empty
string for nil."
  (if num
      (format (concat "%." (number-to-string decimal-places) "f") num)
    ""))

(defun otdb-table-format-number-zero (num decimal-places)
  "Format a number NUM with DECIMAL-PLACES or return zero string
for nil."
  (if num
      (format (concat "%." (number-to-string decimal-places) "f") num)
    "0.0"))

(defun otdb-table-item-row-multiple (database table-name key-list &optional column)
  "Look up multiple ingredient rows in DATABASE file with heading
TABLE-NAME and keys KEY-LIST in column COLUMN."
  ;; get multiple things out
  ;; return a list of rows indexed by key
  (let (database-files
        lisp-table
        found-rows-alist)
    (if otdb-table-database-cache
        (setq lisp-table otdb-table-database-cache)
      (if (otdb-table-data-sanity database table-name)
          (error "Database not sane!")
        (progn
          (setq database-files (cic:ensure-list database))
          (dolist (database-file database-files)
            (with-current-file-transient-org-table database-file table-name
                                                   (setq lisp-table (append lisp-table (cic:org-table-to-lisp-no-separators)))))
          (setq otdb-table-database-cache lisp-table))))
    (dolist (row lisp-table)
      ;; when column is a member
      (let ((column-stripped (s-trim-full-no-properties (elt row (- column 1)))))
        (when (member column-stripped key-list)
          (push (list column-stripped (cic:org-table-assoc lisp-table column-stripped column)) found-rows-alist))))
    found-rows-alist))

;; TODO: convert collections into proper database table
(defun otdb-table-data-sanity (database table-name)
  ""
  ;; check for sanity of database
  ;; check that heading exists, number of columns of each database table, that there is a header
  (let ((table-detect (otdb-table-detect))
        column-widths
        lisp-table
        big-lisp-table
        key-list
        collection-keys
        all-keys
        dups
        message-buffer)
    (unless table-detect
      (error "Not in appropriate place!!!"))
    (cond ((eq table-detect 'recipe)
           (setq message-buffer (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((eq table-detect 'backpacking)
           (setq message-buffer otdb-gear-message-buffer)))
    (setq database-files (cic:ensure-list database))
    (dolist (database-file database-files)
      ;; TODO: heading not found? do I need it at all?
      (with-current-file-transient-org-table database-file table-name
                                             (setq lisp-table (org-table-to-lisp))
                                             (setq big-lisp-table (append big-lisp-table (cdr (cic:org-table-to-lisp-no-separators))))
                                             (if (and
                                                  (eq (elt lisp-table 0) 'hline)
                                                  (eq (elt lisp-table 2) 'hline))
                                                 (setq column-widths (append column-widths (list (length (elt lisp-table 1)))))
                                               (message (concat "Incorrect header in " database-file "!")))))
    ;; if column widths not equal
    (unless (equal (length (delete-dups column-widths)) 1)
      (error "Incorrect column widths!"))
    ;; check for duplicate keys in big-lisp-table
    (setq key-list (mapcar (lambda (e) (downcase (s-trim-full (car e)))) big-lisp-table))
    (setq dups (cic:get-list-duplicates key-list))
    (when (> (length dups) 0)
      (error (concat "Duplicate data keys: " (pp-to-string dups)) message-buffer))
    ;; TODO: need more universal function?, this is a stupid hack for now
    (cond ((eq table-detect 'recipe)
           (setq collection-keys (mapcar (lambda (e) (downcase (s-trim-full e))) (otdb-recipe-get-recipes))))
          ((eq table-detect 'backpacking)
           (setq collection-keys (mapcar (lambda (e) (downcase (s-trim-full e))) (otdb-gear-get-collections)))))
    (setq dups (cic:get-list-duplicates collection-keys))
    (when (> (length dups) 0)
      (error (concat "Duplicate collection keys: " (pp-to-string dups)) message-buffer))
    (setq all-keys (append key-list collection-keys))
    (setq dups (cic:get-list-duplicates all-keys))
    (when (> (length dups) 0)
      (error (concat "Duplicate keys between collections and database: " (pp-to-string dups)) message-buffer))
    ;; TODO: check for possibly conflicting keys in collections by checking case
    ;;       take list of keys from all collections and database, check for duplicates, downcase, duplicates should be the same
    ))

;; (otdb-table-parse-char-columns (cic:org-table-to-lisp-no-separators))
(defun otdb-table-parse-char-columns (lisp-table)
  "Get the columns that have a single character heading from
LISP-TABLE."
  (let ((top-row (car lisp-table))
        (bottom-rows (cdr lisp-table))
        (single-columns nil)
        (count 0))
    ;; get list of single-char column
    (dolist (lisp-element top-row)
      (when (and (equal (length lisp-element) 1) (string-match "[[:alpha:]]" lisp-element))
        (push (list count lisp-element) single-columns))
      (setq count (1+ count)))
    (nreverse single-columns)))

;; TODO: faked for now, need to deal with numbers
;; (otdb-table-check-current-row-lisp (elt (cic:org-table-to-lisp-no-separators) 4) "(or C X)" (otdb-table-parse-char-columns lisp-table))
;; (otdb-table-check-current-row-lisp (elt (cic:org-table-to-lisp-no-separators) 4) "X"        (otdb-table-parse-char-columns (cic:org-table-to-lisp-no-separators)))
;; (defun otdb-table-check-current-row-lisp (lisp-row eval-expression char-columns)
;;   (let ((let-form))
;;     (dolist (char-column char-columns)
;;       (push (list (intern (cadr char-column)) (and (not (equal ""  (s-trim-full (elt lisp-row (car char-column))))))) let-form))
;;     (setq let-form (nreverse let-form))
;;     (setq form (list 'let let-form (car (read-from-string eval-expression))))
;;     (eval form)))

(defun otdb-table-check-current-row-lisp (lisp-row char-columns the-character)
  (let (found)
    (dolist (char-column char-columns)
      (when (and (equal (s-trim-full (cadr char-column)) "X")
                 (equal (s-trim-full (elt  lisp-row (car char-column))) the-character))
        (setq found t)))
    found))

(defun otdb-table-check-invalid-current-row-lisp (lisp-row char-columns)
  (let (invalid)
    (dolist (char-column char-columns)
      (when (and (equal (s-trim-full (cadr char-column)) "X")
                 (equal (s-trim-full (elt  lisp-row (car char-column))) "-"))
        (setq invalid t)))
    invalid))

(defun otdb-table-tag-pattern-match (tags-pattern tags)
  "Match a set of TAGS to a TAGS-PATTERN (list of tags).

Match if all tags in TAGS-PATTERN are present or do not match if
one or more tags in TAGS-PATTERN indicated by !<<tag>> is
present."
  (let* ((tag-pattern-list (split-string (s-trim-full tags-pattern) ","))
         (tag-pattern-list-false (remove-if-not (lambda (e) (and (string-match "^!" e) e)) tag-pattern-list))
         (tag-pattern-list-false-strip (mapcar (lambda (e) (substring e 1)) tag-pattern-list-false))
         (tag-pattern-list-true (cl-set-difference tag-pattern-list tag-pattern-list-false))
         (tag-list (split-string (s-trim-full tags) ",")))
    (and (cl-intersection tag-list tag-pattern-list-true :test 'equal)
         (not (cl-intersection tag-list tag-pattern-list-false-strip :test 'equal)))))

(provide 'otdb-table)
