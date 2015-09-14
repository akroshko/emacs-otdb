;;; otdb-table.el --- Create a database using an org-mode table and
;;; calculate things as a spreadsheet.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20150904
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
;; requires features from https://github.com/akroshko/emacs-stdlib,
;; using (require 'emacs-stdlib) is sufficient.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit conversion is done in a way to be be easy for now, either
;; twice-calculate or lookup in org-table
;; http://en.wikipedia.org/wiki/Conversion_of_units#Length
;; http://whatscookingamerica.net/Q-A/equiv.htm
;; http://www.cookipedia.co.uk/recipes_wiki/Conversions
;; http://joythebaker.com/2013/11/baking-101-im-still-a-baker-even-though-i-use-measuring-cups-not-a-kitchen-scale/
;; http://www.bakepedia.com/tipsandtricks/choosing-dry-measuring-cups-wisely/
;; use conversion from King Arthur flour.... 240ml cup
;; http://www.kingarthurflour.com/shop/items/measuring-cups-set-of-7

(require 'xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database/table keys
(define-minor-mode otdb-mode
  ;; TODO: better way than global trees, trigger on appopriate files
  :global t
  ;; :lighter "Org-mode table database."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-d !") 'otdb-table-agenda-attention)
            (define-key map (kbd "s-d *") 'otdb-table-recalculate)
            (define-key map (kbd "s-d +") 'otdb-recipe-agenda-push-groceries)
            ;; update the "agenda" with the key
            (define-key map (kbd "s-d a") 'otdb-table-agenda-check-add-key)
            ;; goto key in main database
            (define-key map (kbd "s-d d") 'otdb-table-goto-key-in-database)
            ;; XXXX this makes sense to do on a new row or checklist item, or to complete something
            (define-key map (kbd "s-d i") 'otdb-table-insert-key)
            ;; (define-key map (kbd "s-d u") 'otdb-table-update-key-in-database)
            (define-key map (kbd "s-d j") 'otdb-table-agenda-jump)
            ;; TODO: find occurences in database and collections
            ;; (define-key map (kbd "s-d o") 'otdb-table-occurences-key)
            ;; put key into main database, ask for key, update key at point if necessary
            (define-key map (kbd "s-d p") 'otdb-table-put-key-in-database)
            ;; update key from main database
            ;; TODO want to be able to go and pop back
            ;; update the "agenda" with the key
            (define-key map (kbd "s-d u") 'otdb-table-agenda-uncheck-key)
            map))
(otdb-mode 1)
(defun otdb-setup-minibuffer-hook ()
  (otdb-mode 0))
(add-hook 'minibuffer-setup-hook 'otdb-setup-minibuffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table variables that can be bound
(defun otdb-table-reset-cache ()
  "Reset the cache of information stored, because tables and
databases are not edited during calculations.
TODO: Make less application-specific."
  (setq otdb-recipe-recipes-cache nil)
  (setq otdb-recipe-database-cache nil)
  (setq otdb-gear-database-cache nil)
  (setq otdb-gear-collections-cache nil))

(defun otdb-table-update (arg database heading collection-files lookup-function insert-function message-buffer)
  "Update otdb tables in current context.

DATABASE and HEADING gives the database.  COLLECTION-FILES gives
the collection of files.  LOOKUP-FUNCTION and INSERT-FUNCTION are
helper functions.  MESSAGE-BUFFER gives messages."
  (cond ((equal arg '(4))
         (save-excursion
           (point-min)
           (org-table-map-tables (lambda () (otdb-table-update nil database heading collection-files lookup-function insert-function message-buffer)))))
        ((equal arg '(16))
         (dolist (collection-file collection-files)
           (with-current-file-min collection-file
             (otdb-table-update '(4) database heading collection-files lookup-function insert-function message-buffer))))
        ((equal arg '(64))
         (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer)
         (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer)
         (otdb-table-update '(16) database heading collection-files lookup-function insert-function message-buffer))
        (t
         (let ((table-filename (buffer-file-name))
               (table-lisp (cic:org-table-to-lisp-no-separators))
               (table-heading (save-excursion (org-back-to-heading) (cic:get-headline-text (cic:get-current-line))))
               looked-up)
           ;; writing table-lookup functions is good
           (setq looked-up (funcall lookup-function table-lisp))
           ;; get columns from org-table-lisp
           (funcall insert-function table-filename table-heading looked-up)))))

(defun otdb-table-number (value)
  "Read from string VALUE and convert to a number."
  ;; match numbers, strip whitespace
  (when value
    ;; deal with fractions
    (when (string-match "[0-9.]" value)
      (if (string-match "/" value)
          (progn
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
                  (+ (string-to-float (elt space-split 0)) (/ (string-to-float (elt slash-split 0))
                                                              (string-to-float (elt slash-split 1))))
                (/ (string-to-float (elt slash-split 0))
                   (string-to-float (elt slash-split 1))))))
        (progn
          (string-to-float value))))))

(defun otdb-table-unit (value)
  "Read from string VALUE and get the unit as a string."
  ;; match just the letters, strip any whitespace
  (let (unit-string)
    (and (string-match "\\([A-Za-z]+\\)" value)
         (setq unit-string (match-string 1 value)))
    unit-string))

(defun otdb-table-unit-type (quantity-entry)
  "Detect the type of units (weight or volume) from the string
QUANTITY-ENTRY."
  ;; TODO need exact word string match to avoid l<-->lb confusion
  ;; TODO use the weight and volume tables instead
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

;; figure out if we are dealing with weight/volume
;; figure out the from and to units
;; look up the from and to conversion, convert

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
  "Conversion table for weight units.")

(defconst otdb-table-volume-table
  '(("L" (("L" 1.0)
          ("cup" (/ 1000.0 240.0))
          ("oz" (/ 1000.0 30.0))
          ("tbsp" (/ 1000.0 15.0))
          ("tsp" (/ 1000.0 5.0))
          ("ml" 1000.0)))
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
  "Conversion table for volume units.")


(defun otdb-table-get-key-at-point ()
  "Get the KEY from the current table, works for both collections and databases.
TODO: Fix some of the hardcoding here.
TODO: probably want an error if not at proper table
TODO: how to handle multiple databases"
  (let (key
        (line (cic:get-current-line))
        (column 2))
    (when (or
           (equal (file-name-nondirectory (buffer-file-name)) "food-database.org")
           (equal (file-name-nondirectory (buffer-file-name)) "gear-database.org"))
      (setq column 1))
    (cond ((org-at-table-p)
           ;; get first column of current row
           (setq key (strip-full (org-table-get nil column))))
          ((cic:org-list-p line)
           (when (string-match cic:emacs-stdlib-list-regexp line)
             (setq key (match-string 3 line))))
          (t
           (error "Not in valid file!")))
    (strip-full-no-properties key)))

(defun otdb-table-insert-key-database (new-key)
  "Insert NEW-KEY into the database.
TODO: document further and remove hardcoding."
  ;; XXXX assumes proper checks have already been made before it
  ;; modifies database
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (with-current-file-org-table (otdb-recipe-get-variable 'otdb-recipe-database)
                                        (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                                        ;; go to last row
                                        (cic:org-table-last-row)
                                        ;; insert the new key
                                        (org-table-insert-row '(4))
                                        (insert new-key)
                                        (org-table-align)))
          ((eq table-detect 'backpacking)
           (with-current-file-org-table otdb-gear-database otdb-gear-database-headline
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
           (search-forward-regexp "[-+*]" nil t)
           (kill-line)
           ;; insert new-key
           (insert (concat " " new-key))
           (org-table-align))
          ((cic:org-checkbox-p line)
           ;; go to end of checkbox
           (move-beginning-of-line 1)
           (search-forward "" nil t)
           ;; kill till end of line
           (kill-line)
           ;; insert new-key
           (insert (concat " " new-key)))
          (t
           (error "Not in valid file!")))))

(setq otdb-recipe-key-history nil)
(setq otdb-gear-key-history nil)
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
             (goto-location
              ;; TODO want to find recipes too
              (otdb-recipe-find-ingredient
               (completing-read "Ingredient: " ingredient-list nil nil (otdb-table-get-key-at-point) 'otdb-recipe-key-history)))))
          ((eq table-detect 'backpacking)
           (let ((item-list (otdb-gear-get-items)))
             (goto-location
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
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (let* ((line (cic:get-current-line))
                  (completion-list (if (equal arg '(4))
                                       (otdb-recipe-get-ingredients)
                                     (append (otdb-recipe-get-ingredients) (otdb-recipe-get-recipes))))
                  (ingredient (completing-read "Ingredient: " completion-list nil nil (otdb-table-get-key-at-point) 'otdb-recipe-key-history)))
             (cond
              ((eq major-mode 'org-mode)
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
             (cond
              ((eq major-mode 'org-mode)
               (otdb-table-insert-key-at-point item))
              (t
               (error "Not in valid file!")))))))
  (org-table-align))

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
                   (when (not (equal new-key at-point))
                     (otdb-table-get-key-at-point new-key))
                   ;; adds a row to the database
                   (otdb-table-insert-key-database new-key))
               (mpp-echo (concat new-key " already in database!") (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))))
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
                   (when (not (equal new-key at-point))
                     (otdb-table-get-key-at-point new-key))
                   ;; add a new row to the database
                   (otdb-table-insert-key-database new-key))
               (mpp-echo (concat new-key " already in database!") (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-recalculate (&optional arg)
  "Recalculate a database.  ARG gets passed to
otdb-table-update."
  (interactive "P")
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (otdb-table-update arg
                             (otdb-recipe-get-variable 'otdb-recipe-database)
                             (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                             (otdb-recipe-get-variable 'otdb-recipe-files)
                             'otdb-recipe-lookup-function 'otdb-recipe-insert-function (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((eq table-detect 'backpacking)
           (otdb-table-update arg
                             otdb-gear-database
                             otdb-gear-database-headline
                             otdb-gear-collection-files
                             'otdb-gear-lookup-function
                             'otdb-gear-insert-function
                             otdb-gear-message-buffer))
          (t
           (error "Not in valid file!")))))

(defun otdb-table-agenda-attention ()
  "Call attention in agenda to a key in the table for things such
as price checking.  Can be used whenever values related to an
item are in need of checking."
  (interactive)
  (otdb-table-reset-cache)
  (let ((table-detect (otdb-table-detect)))
    (cond ((eq table-detect 'recipe)
           (otdb-recipe-agenda-price-check))
          ((eq table-detect 'backpacking)
           (error "Not in valid file!"))
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

(provide 'otdb-table)
