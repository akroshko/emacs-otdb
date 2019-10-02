;;; otdb-recipe.el --- Allows easy entry of nutritional information
;;; into org-mode tables and calculation of costs and macronutrient
;;; amounts.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20190429
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

;; TODO: non-functional but here for completeness
(defvar otdb-recipe-item-pattern
  nil
  "Contains the regex pattern for filtering special calculations.")

;; TODO: non-functional but here for completeness
(defvar otdb-recipe-item-last-pattern
  nil
  "Contains the last regex pattern for filtering special
  calculations.")

(defvar otdb-recipe-item-tags
  nil
  ;; TODO: need a better interface
  "Contains the tags used for filtering special calculations.")

(defvar otdb-recipe-item-last-tags
  nil
  "Contains the last tags used for filtering special
  calculations.")

(defconst otdb-recipe-quantity-column
  0
  "The table column that contains the quantity.")

(defconst otdb-recipe-item-column
  1
  "The table column that contains the item.")

(defconst otdb-recipe-database-item-column
  0
  "The table column in the database that contains the item.")

(defconst otdb-recipe-database-package-weight-column
  1
  "The table column in the database that contains the package weight.")

(defconst otdb-recipe-database-package-volume-column
  2
  "The table column in the database that contains the package volume.")

(defconst otdb-recipe-database-cost-column
  3
  "The table column in the database that contains the cost.")

(defconst otdb-recipe-database-serving-weight-column
  4
  "The table column in the database that contains the weight.")

(defconst otdb-recipe-database-serving-volume-column
  5
  "The table column in the database that contains the volume.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for different collection of recipe files
;; XXXX: needs to be here
(defun otdb-recipe-get-variable (lookup-variable)
  "Helper function to lookup different otdb-recipe variables
depending on context."
  (let ((current-filename (ignore-errors buffer-file-name)))
    ;; use the standard version
    (let ((normal-recipe-files otdb-recipe-normal-alist))
      (cdr (assoc lookup-variable normal-recipe-files)))))

(defun otdb-recipe-reset-filters ()
  "Reset all specified filters."
  (interactive)
  (setq otdb-recipe-item-last-pattern nil
        otdb-recipe-item-pattern      nil
        otdb-recipe-item-last-tags    nil
        otdb-recipe-item-tags         nil))

(defun otdb-recipe-menu-tags ()
  "Create dynamic menus to reflect the current value of
otdb-recipe-item-tags."
  (cons (cond (otdb-recipe-item-tags
               (concat "Disable (otdb-recipe-item-tags): "   (pp-to-string otdb-recipe-item-tags)))
              (otdb-recipe-item-last-tags
               (concat "Re-enable (otdb-recipe-item-tags): " (pp-to-string otdb-recipe-item-last-tags)))
              (t
               "Empty (otdb-recipe-item-tags): "))
        (lambda ()
          (interactive)
          (if otdb-recipe-item-tags
              (setq otdb-recipe-item-last-tags otdb-recipe-item-tags
                    otdb-recipe-item-tags      nil)
            (setq otdb-recipe-item-tags otdb-recipe-item-last-tags)))))

(defun otdb-recipe-menu-item-pattern ()
  "Create dynamic menus to reflect the current value of
otdb-recipe-item-pattern."
  (cons (cond (otdb-recipe-item-pattern
               (concat "Disable (otdb-recipe-item-pattern): "   (pp-to-string otdb-recipe-item-pattern)))
              ((and otdb-recipe-item-pattern otdb-recipe-item-last-pattern)
               (concat "Re-enable (otdb-recipe-item-pattern): " (pp-to-string otdb-recipe-item-pattern)))
              (t
               "Empty (otdb-recipe-item-pattern): "))
        (lambda ()
          (interactive)
          (if otdb-recipe-item-pattern
              (setq otdb-recipe-item-last-pattern otdb-recipe-item-pattern
                    otdb-recipe-item-pattern      nil)
            (setq otdb-recipe-item-pattern otdb-recipe-item-last-pattern)))))

(defun otdb-recipe-menu-files (map otdb-recipe-menu)
  "Create dynamic menus pointing to the recipe files and
databases."
  (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-collections)              (cons "Recipe files" (make-sparse-keymap "recipe files")))
  (when (boundp 'otdb-recipe-normal-alist)
    (dolist (collection (cic:ensure-list (otdb-recipe-get-variable 'otdb-recipe-files)))
      (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-collections (make-symbol collection)) (cons collection (cic:make-file-finder collection)))))
  ;; https://stackoverflow.com/questions/9966279/how-to-dynamically-define-a-menu-item-what-is-the-thing-in-square-braces
  ;; TODO: hope this always works out properly, might have issue if databases change
  ;;       does not update dynamically at the moment
  (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-databases)                (cons "Recipe databases" (make-sparse-keymap "recipe databases")))
  (when (boundp 'otdb-recipe-normal-alist)
    (dolist (database (cic:ensure-list (otdb-recipe-get-variable 'otdb-recipe-database)))
      (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-databases (make-symbol database)) (cons database (cic:make-file-finder database))))))

(defun otdb-recipe-update-menu ()
  "Run from a hook to update the otdb-recipe-mode menu."
  (when (derived-mode-p 'otdb-recipe-mode)
    (define-key otdb-recipe-mode-map (vector 'menu-bar 'otdb-recipe-menu 'item-pattern) (otdb-recipe-menu-item-pattern))
    (define-key otdb-recipe-mode-map (vector 'menu-bar 'otdb-recipe-menu 'item-tag)     (otdb-recipe-menu-tags))))

(defvar otdb-recipe-mode-map
  (let ((map (make-sparse-keymap)))
    (setq map (otdb-table-skeleton-map map))
    (define-key map [menu-bar otdb-recipe-menu]                                        (cons "otdb-recipe" (make-sparse-keymap "otdb-recipe")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu)                               (cons "otdb-recipe" (make-sparse-keymap "otdb-recipe")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'reset-filters)                '("Reset recipe filters" otdb-recipe-reset-filters))
    ;; TODO: generate these from alist
    (otdb-recipe-menu-files map 'otdb-recipe-menu)
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'separator4) '("--"))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'export-recipe-multiple)       '("Export recipe multiple" . otdb-recipe-export-multiple))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'export-recipe)                '("Export recipe"          . otdb-recipe-export))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'separator3) '("--"))
    ;; TODO: change for tags more like
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-patterns)                (cons "Recipe ingredient patterns" (make-sparse-keymap "recipe ingredient patterns")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-patterns 'spice)         (cons "Spice"                      'nil-command))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-patterns 'packaging)     (cons "Packaging"                  'nil-command))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-pattern)                 (otdb-recipe-menu-item-pattern))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-tags)                    (cons "Ingredient tags" (make-sparse-keymap "ingredient tags patterns")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-tags 'spice)             (cons "Spice"     (command-with-args 'set otdb-recipe-item-tags "spice")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-tags 'packaging)         (cons "Packaging" (command-with-args 'set otdb-recipe-item-tags "packaging")))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'item-tag)                     (otdb-recipe-menu-tags))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'separator2) '("--"))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'toggle-check-invalid)         '("Toggle column X as invalid" . otdb-table-invalid-toggle-check-line))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'toggle-check-cost)            '("Toggle column X as \"C\""   . otdb-table-set-toggle-cost-line))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'toggle-check)                 '("Toggle column X"            . otdb-table-set-toggle-check-line))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'calc-special-command-pattern) '("Use special buffer for calculating specified pattern" . otdb-recipe-calc-in-special-buffer-pattern))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'calc-special-command-tagged)  '("Use special buffer for calculating selected tags"     . otdb-recipe-calc-in-special-buffer-tag))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'calc-special-command-cost)    '("use special buffer for calculating cost"              . otdb-recipe-calc-in-special-buffer-cost))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'calc-special-command-checked) '("use special buffer for calculating checked"           . otdb-recipe-calc-in-special-buffer-checked))
    (define-key map (vector 'menu-bar 'otdb-recipe-menu 'calc-special-command-all)     '(menu-item "Use special buffer for calculating all"     otdb-recipe-calc-in-special-buffer-all
                                                                                                   :keys "s-d s"))
    map)
  "The key map for otdb-recipe-mode.")

(add-hook 'menu-bar-update-hook 'otdb-recipe-update-menu)

(define-derived-mode otdb-recipe-mode org-mode "org-table database recipe mode"
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))

(defvar otdb-recipe-mode-map
  (let ((map (make-sparse-keymap)))
    (otdb-table-skeleton-map map))
  "The keymap for otdb-recipe-mode.")

;; check and add to shopping list
(defun otdb-recipe-add-check ()
  "Check a box in the shopping list based on current item near
point or entered item."
  ;; TODO want to be able to insert item without checking, best way???
  ;; TODO want to sort groceries
  (interactive)
  (let* ((default-item (ignore-errors (otdb-table-get-key-at-point)))
         (shopping-list-checked (otdb-recipe-get-shopping 'checked))
         (shopping-list-unchecked (otdb-recipe-get-shopping 'unchecked))
         ;; XXXX not all of these may be
         (full-list (append shopping-list-checked shopping-list-unchecked))
         shopping-headlines
         selected-item
         unmatched-item
         prompt
         (headline-prompt ""))
    (if (not (cic:full-string-p default-item))
        (setq prompt "Grocery item to add: ")
      (setq prompt (concat "Grocery item to add (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (unless (cic:full-string-p (s-trim-full selected-item))
      (setq selected-item default-item))
    (cond ((member selected-item shopping-list-checked)
           (cic:mpp-echo (format "Grocery item %s already checked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((member selected-item shopping-list-unchecked)
           ;; uncheck
           ;; TODO: make sure only one appropriate box is checked
           ;;       reset any things needed by save-excursion, etc.
           (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                             (when (string-match "^Grocery.*" headline-name)
                               ;; find in subtree
                               (save-excursion
                                 (org-narrow-to-subtree)
                                 (goto-char (point-min))
                                 (when (re-search-forward (concat cic:checkbox-regexp " " selected-item) nil t)
                                   (org-toggle-checkbox))
                                 (widen))))
           (cic:mpp-echo (format "Grocery item %s successfully checked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          (t
           (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                             (when (string-match "^Grocery.*" headline-name)
                               (push headline-name shopping-headlines)))
           (setq shopping-headlines (nreverse shopping-headlines))
           (dotimes (headline-number (length shopping-headlines))
             (setq headline-prompt (concat headline-prompt "(" (number-to-string headline-number) ") " (nth headline-number shopping-headlines) "\n")))
           (setq headline-prompt (concat  headline-prompt "(q) Quit without adding\n"))
           (let ((headline-counter 0))
             (setq unmatched-item (read-char-choice headline-prompt (append (mapcar (lambda (i)
                                                                                      (setq headline-counter (1+ headline-counter))
                                                                                      ;; (make-symbol (concat "?" (number-to-string (- headline-counter 1))))
                                                                                      (+ 48 (1- headline-counter)))
                                                                                    shopping-headlines)
                                                                            '(?q)))))
           ;; create the prompt and read them (use q)
           ;; add a checkbox if necessary
           (cond ((not (string-match "[A-Za-z]" (char-to-string unmatched-item)))
                  ;; find headline and add at the end
                  (with-current-file-transient-headline (otdb-recipe-get-variable 'otdb-recipe-agenda) (nth (- unmatched-item 48) shopping-headlines)
                                                        (outline-hide-subtree)
                                                        (org-show-subtree)
                                                        (org-end-of-subtree)
                                                        (cic:org-insert-indent-list-item)
                                                        (insert (concat "[X] " selected-item))
                                                        (basic-save-buffer)))
                 ((equal unmatched-item (string-to-char "q"))
                  ;; just finish
                  ))))))

(add-hook 'otdb-recipe-mode-hook 'otdb-recipe-mode-init)
(defun otdb-recipe-mode-init ()
  "Initialize otdb-recipe-mode with some extra functionality."
  (when (derived-mode-p 'otdb-recipe-mode) (functionp 'hl-line-mode)
        (hl-line-mode 1)))

(defun otdb-recipe-uncheck ()
  "Uncheck a box in the shopping list based on current item near
point or entered item."
  (interactive)
  (let* ((default-item (ignore-errors (otdb-table-get-key-at-point)))
         (shopping-list-checked (otdb-recipe-get-shopping 'checked))
         (shopping-list-unchecked (otdb-recipe-get-shopping 'unchecked))
         (full-list (append shopping-list-checked shopping-list-unchecked))
         selected-item
         prompt)
    (if (not (cic:full-string-p default-item))
        (setq prompt "Shopping item to remove: ")
      (setq prompt (concat "Shopping item to remove (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (unless (cic:full-string-p (s-trim-full selected-item))
      (setq selected-item default-item))
    (cond ((not (member selected-item full-list))
           (cic:mpp-echo (format "Shopping item %s not found.  Nothing to remove!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((member selected-item shopping-list-checked)
           (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                             (when (string-match "^Grocery.*" headline-name)
                               ;; find in subtree
                               (save-excursion
                                 (org-narrow-to-subtree)
                                 (goto-char (point-min))
                                 (when (ignore-errors (re-search-forward (concat cic:checkbox-regexp " " selected-item)))
                                   (org-toggle-checkbox))
                                 (widen))))
           (cic:mpp-echo (format "Shopping item %s successfully unchecked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((member selected-supply supply-list-unchecked)
           (cic:mpp-echo (format "Shopping item %s already unchecked!" selected-supply) (otdb-recipe-get-variable 'otdb-recipe-message-buffer))))))

(defun otdb-recipe-get-shopping (&optional checktype)
  "Get the list of items to shop for, optionally with CHECKTYPE."
  (let (line
        matched-text
        shopping-list
        (shopping-file (otdb-recipe-get-variable 'otdb-recipe-agenda)))
    (cond ((eq checktype 'checked)
           (setq checkbox-regexp cic:checkbox-checked-regexp))
          ((eq checktype 'unchecked)
           (setq checkbox-regexp cic:checkbox-unchecked-regexp))
          (t
           (setq checkbox-regexp cic:checkbox-regexp)))
    (do-org-headlines shopping-file headline-name headline-subtree
                      (when (string-match "Grocery.*" headline-name)
                        (do-org-list-items shopping-file headline-name item-line
                                           (when (string-match checkbox-regexp item-line)
                                             (setq matched-text  (match-string 3 item-line))
                                             (push (s-trim-full-no-properties matched-text) shopping-list)))))
    (setq dups (cic:get-list-duplicates shopping-list))
    (when (> (length dups) 0)
      (cic:mpp-echo (concat "Duplicate groceries: " (pp-to-string dups)) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
    (nreverse shopping-list)))

(defun otdb-recipe-get-calories-protein-fat-weight-volume-cost (recipe)
  "Get the totals from a particular RECIPE. Generally to use in a
another recipe."
  (let ((recipe-location (otdb-recipe-find recipe)))
    (with-current-file-transient (car recipe-location)
      (goto-char (cadr recipe-location))
      (cic:org-find-table)
      (cic:org-table-last-row)
      ;; TODO replace with configurable alist
      (list (string-to-number (org-table-get nil 4))
            (string-to-number (org-table-get nil 5))
            (string-to-number (org-table-get nil 6))
            (string-to-number (org-table-get nil 13))
            (string-to-number (org-table-get nil 14))
            (string-to-number (org-table-get nil 7))
            ""))))

;; find recipes
(defun otdb-recipe-find (recipe)
  "Find RECIPE and return the location.
TODO return location at beginning of line"
  ;; have an "exact" way of doing this
  (let (location)
    (dolist (recipe-file (otdb-recipe-get-variable 'otdb-recipe-files))
      (with-current-file-transient-min recipe-file
        (let ((found (when  (re-search-forward (concat "^\* " recipe " :recipe:") nil t)
                       (beginning-of-line)
                       (point))))
          (when found
            (setq location (list recipe-file found))))))
    location))

(defun otdb-recipe-get-recipes ()
  "Get the full list of recipes."
  (if otdb-table-collections-cache
      otdb-table-collections-cache
    (let (table
          table-name
          recipe-list)
      (dolist (recipe-file (otdb-recipe-get-variable 'otdb-recipe-files))
        (do-org-tables recipe-file table-name table
                       (when (string-match "\\(.*\\) :recipe:" table-name)
                         (push (match-string 1 table-name) recipe-list))))
      (setq otdb-table-collections-cache recipe-list)
      recipe-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recipes
(defun otdb-recipe-calories-protein-fat-weight (ingredient-row quantity column to-unit)
  "Return the nutritional values of QUANTITY from database row
INGREDIENT-ROW from COLUMN converting to units TO-UNIT.  Does the
right thing based on information available."
  ;; TODO: combine with very similar cost function
  (let* ((ingredient-package-weight (elt ingredient-row otdb-recipe-database-package-weight-column))
         (ingredient-package-volume (elt ingredient-row otdb-recipe-database-package-volume-column))
         (ingredient-serving-weight (elt ingredient-row otdb-recipe-database-serving-weight-column))
         (ingredient-serving-volume (elt ingredient-row otdb-recipe-database-serving-volume-column))
         ;; used to convert within servings
         ;; TODO: should to-quantity be called to-servings
         (unit-type (otdb-table-unit-type (otdb-table-unit quantity)))
         (to-quantity (cond ((eq unit-type 'weight)
                             ingredient-serving-weight)
                            ((eq unit-type 'volume)
                             ingredient-serving-volume)
                            ;; TODO want to know what to do when quantity is dimensionless, e.g., KD has no volume serving
                            (t
                             ingredient-serving-volume))))
    (cond
     ((and (not unit-type)
           (cic:full-string-p (elt ingredient-row otdb-recipe-database-serving-volume-column))
           (not (otdb-table-unit-type (elt ingredient-row otdb-recipe-database-serving-volume-column))))
      ;; dimensionless units are volume
      (*
       (/
        (otdb-table-number quantity)
        (otdb-table-number ingredient-serving-volume))
       (cic:string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (cic:full-string-p ingredient-package-volume)
           (cic:full-string-p ingredient-serving-volume)
           (otdb-table-unit-type ingredient-serving-volume))
      (* (/ (* (otdb-table-number quantity)
               (otdb-table-number ingredient-package-volume))
            (otdb-table-number ingredient-serving-volume))
         (otdb-table-unit-conversion 'volume (otdb-table-unit ingredient-package-volume) (otdb-table-unit ingredient-serving-volume))
         (cic:string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (cic:full-string-p ingredient-package-weight)
           (cic:full-string-p ingredient-serving-weight)
           (otdb-table-unit-type ingredient-serving-weight)
           (cic:full-string-p ingredient-package-volume)
           (not (otdb-table-unit-type ingredient-package-volume)))
      (* (otdb-table-number quantity)
         (/ (/ (otdb-table-number ingredient-package-weight)
               (otdb-table-number ingredient-package-volume))
            (otdb-table-number ingredient-serving-weight))
         (otdb-table-unit-conversion 'weight (otdb-table-unit ingredient-package-weight) (otdb-table-unit ingredient-serving-weight))
         (cic:string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (cic:full-string-p ingredient-package-weight)
           (cic:full-string-p ingredient-serving-weight)
           (otdb-table-unit-type ingredient-serving-weight))
      (* (/ (* (otdb-table-number quantity)
               (otdb-table-number ingredient-package-weight))
            (otdb-table-number ingredient-serving-weight))
         (otdb-table-unit-conversion 'weight (otdb-table-unit ingredient-package-weight) (otdb-table-unit ingredient-serving-weight))
         (cic:string-to-float (elt ingredient-row column))))
     (t
      (*
       (/ (otdb-table-number quantity)
          (otdb-table-number to-quantity))
       (otdb-table-unit-conversion unit-type (otdb-table-unit quantity) (otdb-table-unit to-quantity))
       (cic:string-to-float (elt ingredient-row column)))))))

(defun otdb-recipe-cost-row (ingredient-row quantity)
  "Return the cost based on the QUANTITY in INGREDIENT-ROW.
Tries to do the right thing with different types of units."
  (let* ((ingredient-package-weight (elt ingredient-row otdb-recipe-database-package-weight-column))
         (ingredient-package-volume (elt ingredient-row otdb-recipe-database-package-volume-column))
         (ingredient-cost           (elt ingredient-row otdb-recipe-database-cost-column))
         (ingredient-serving-weight (elt ingredient-row otdb-recipe-database-serving-weight-column))
         (ingredient-serving-volume (elt ingredient-row otdb-recipe-database-serving-volume-column))
         (unit-type (otdb-table-unit-type (otdb-table-unit quantity))))
    ;; figure out if we have both weight units for cost or not
    (cond ((eq unit-type 'weight)
           (cond ((not (cic:full-string-p ingredient-package-weight))
                  (*
                   ;; quantity
                   (otdb-table-number quantity)
                   ;; cost-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit ingredient-package-weight))
                   ;; cost-quantity / cost)
                   (/ (otdb-table-number ingredient-cost)
                      (otdb-table-number ingredient-package-weight))))
                 ;; all weight
                 ((and (cic:full-string-p ingredient-package-weight)
                       (cic:full-string-p ingredient-serving-weight))
                  (*
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number ingredient-package-weight))
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit quantity)
                    (otdb-table-unit ingredient-package-weight))
                   (otdb-table-number ingredient-cost)))
                 (t
                  ;; quantity / serving-quantity
                  (*
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number ingredient-serving-weight))
                   ;;  serving-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit ingredient-serving-weight))
                   ;; serving-quantity(alt)
                   (otdb-table-number ingredient-serving-volume)
                   ;;  serving-units(alt)/cost-units
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit ingredient-serving-volume)
                    (otdb-table-unit ingredient-package-volume))
                   ;;  cost-quantity/cost
                   (/
                    (cic:string-to-float ingredient-cost)
                    (otdb-table-number ingredient-package-volume))))))
          ((eq unit-type 'volume)
           (cond ((cic:full-string-p ingredient-package-volume)
                  (*
                   ;; quantity
                   (otdb-table-number quantity)
                   ;; cost-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit ingredient-package-volume))
                   ;; cost-quantity / cost)
                   (/ (otdb-table-number ingredient-cost)
                      (otdb-table-number ingredient-package-volume))))
                 (t
                  (*
                   ;; quantity / serving-quantity
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number ingredient-serving-volume))
                   ;;  serving-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit ingredient-serving-volume))
                   ;; serving-quantity(alt)/serving-quantity
                   (otdb-table-number ingredient-serving-weight)
                   ;;  serving-units(alt)/cost-units
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit ingredient-serving-weight)
                    (otdb-table-unit ingredient-package-weight))
                   ;;  cost-quantity/cost
                   (/
                    (cic:string-to-float ingredient-cost)
                    (otdb-table-number ingredient-package-weight))))))
          (t
           (cond ((not (cic:full-string-p (otdb-table-unit ingredient-package-volume)))
                  (if (and (not unit-type) (not (cic:full-string-p ingredient-package-volume)))
                      (otdb-table-number ingredient-cost)
                    (*
                     (/
                      (otdb-table-number quantity)
                      (otdb-table-number ingredient-package-volume))
                     (otdb-table-number ingredient-cost))))
                 (t
                  (*
                   (otdb-table-number quantity)
                   (otdb-table-number ingredient-cost))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tables

(defun otdb-recipe-lookup-function (row-list)
  "Helper function for otdb-table-update to lookup information
for ROW-LIST from a particular recipe.
Test on an actual table with (otdb-recipe-lookup-function (cic:org-table-to-lisp-no-separators))"
  (let (database-row-alist
        calories-protein-fat-weight-volume-cost-list
        key-list
        quantity-alist
        recipe-calories-protein-fat-weight-volume-cost-list
        (recipe-list (otdb-recipe-get-recipes)))
    ;; get list of keys to lookup
    (dolist (row (cdr row-list))
      ;; get the key if applicable
      (if (member (s-trim-full-no-properties (elt row otdb-recipe-item-column)) recipe-list)
          (setq recipe-calories-protein-fat-weight-volume-cost-list
                (let* ((row-quantity (elt row otdb-recipe-quantity-column))
                       (row-item (elt row otdb-recipe-item-column))
                       (ccl (otdb-recipe-get-calories-protein-fat-weight-volume-cost row-item)))
                  (push (list (s-trim-full-no-properties row-item)
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 0)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 1)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 2)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 3)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 4)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 5)))
                              (ignore-errors (* (otdb-table-number row-quantity) (elt ccl 6)))
                              "")
                        recipe-calories-protein-fat-weight-volume-cost-list)))
        (progn
          (push (list (s-trim-full-no-properties (elt row otdb-recipe-item-column))
                      (s-trim-full-no-properties (elt row otdb-recipe-quantity-column)))
                quantity-alist)
          (push (s-trim-full-no-properties (elt row otdb-recipe-item-column)) key-list))))
    ;; get the database rows
    (setq database-row-alist (otdb-table-item-row-multiple
                              (otdb-recipe-get-variable 'otdb-recipe-database)
                              (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                              key-list 1))
    ;; calculate both cost and other things
    (dolist (row-alist database-row-alist)
      (let ((row (cadr row-alist))
            (quantity (cadr (assoc (car row-alist) quantity-alist))))
        (let ((calories
               (when (cic:full-string-p (elt row 6))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 6 nil))))
              (protein
               (when (cic:full-string-p (elt row 7))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 7 nil))))
              (fat
               (when (cic:full-string-p (elt row 8))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 8 nil))))
              (weight
               (if (cic:full-string-p (elt row 9))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity 9 nil))
                 (when (cic:full-string-p (elt row 4))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity 4 nil)))))
              (volume
               (when (cic:full-string-p (elt row 10))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 10 nil))))
              (tags
               (when (cic:full-string-p (elt row 13))
                 (elt row 13)))
              (cost (ignore-errors
                      (otdb-recipe-cost-row row quantity))))
          (push (list (car row-alist)
                      calories
                      protein
                      fat
                      weight
                      volume
                      cost
                      tags)
                calories-protein-fat-weight-volume-cost-list))))
    (append recipe-calories-protein-fat-weight-volume-cost-list calories-protein-fat-weight-volume-cost-list)))

(defun otdb-recipe-insert-function (recipe-filename recipe-heading calories-protein-fat-weight-volume-cost-list)
  "Helper function for otdb-table-update to insert information
into a recipe.  The recipe is RECIPE-HEADING in RECIPE-FILENAME
with information to be inserted of
CALORIES-PROTEIN-FAT-WEIGHT-VOLUME-COST-LIST."
  (let (new-ingredient
        new-calories
        new-protein
        new-weight
        new-volume
        new-cost
        new-tags
        (count 1))
    (do-org-table-rows recipe-filename recipe-heading row
                       (setq new-ingredient (s-trim-full-no-properties (elt row otdb-recipe-item-column)))
                       (unless (equal count 1)
                         (setq new-calories (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 1)
                               new-protein  (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 2)
                               new-fat      (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 3)
                               new-weight   (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 4)
                               new-volume   (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 5)
                               new-cost     (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 6)
                               new-tags     (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 7))
                         (if (not new-calories)
                             (org-table-put count 4 "")
                           (org-table-put count 4 (format "%.1f" new-calories)))
                         (if (not new-protein)
                             (org-table-put count 5 "")
                           (org-table-put count 5 (format "%.1f" new-protein)))
                         (if (not new-fat)
                             (org-table-put count 6 "")
                           (org-table-put count 6 (format "%.1f" new-fat)))
                         (if (not new-weight)
                             (org-table-put count 13 "")
                           (org-table-put count 13 (format "%.3f" new-weight)))
                         (if (not new-volume)
                             (org-table-put count 14 "")
                           (org-table-put count 14 (format "%.3f" new-volume)))
                         (if (not new-cost)
                             (org-table-put count 7 "")
                           (org-table-put count 7 (format "%.3f" new-cost)))
                         (if (not new-tags)
                             (org-table-put count 15 "")
                           (org-table-put count 15 new-tags)))
                       (setq count (1+ count)))
    (tblel-eval)))

;; TODO: appears broken
(defun otdb-recipe-find-ingredient (ingredient)
  "Find the location of INGREDIENT ingredient in database (or
recipe)."
  ;; TODO this will probably become a pretty general table lookup function for databases
  (if (member ingredient (otdb-recipe-get-recipes))
      (otdb-recipe-find ingredient)
    (cic:org-table-lookup-location (otdb-recipe-get-variable 'otdb-recipe-database)
                                   (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                                   ingredient 1)))

;; TODO: appears to only be called from other broken functions
(defun otdb-recipe-get-ingredients ()
  "Get list of all ingredients from the database."
  ;; TODO: will need to be modified for multiple files
  (let ((ingredients)
        (dups))
    (dolist (database (otdb-recipe-get-variable 'otdb-recipe-database))
      (setq ingredients (append ingredients (cic:org-table-get-keys database (otdb-recipe-get-variable 'otdb-recipe-database-headline)))))
    (setq dups (cic:get-list-duplicates ingredients))
    (when (> (length dups) 0)
      (cic:mpp-echo (concat "Duplicate ingredients: " (pp-to-string dups)) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
    ingredients))

;; TODO: will need to modified for multiple files
;; TODO: does not appear to be used....
(defun otdb-recipe-ingredient-row (ingredient)
  "Look up row of INGREDIENT ingredient in the database."
  ;; XXXX assume on top of table
  ;; XXXX assume Ingredient.... etc. header is first
  (cic:org-table-lookup-row (otdb-recipe-get-variable 'otdb-recipe-database)
                            ;; TODO: this variable will have to be modified for multiple files
                            (otdb-recipe-get-variable 'otdb-recipe-database-headline)
                            ingredient))

(defun otdb-recipe-export-multiple (&optional otdb-recipe-temp-directory)
  "Export each component recipe in a table containing recipes to a pdf file in ~/tmp."
  (interactive)
  (require 'ox-publish)
  ;; loop over rows in current table
  (unless otdb-recipe-temp-directory
    (setq otdb-recipe-temp-directory "~/tmp/"))
  (save-excursion
    (org-back-to-heading)
    (let ((heading-name-collection (save-excursion
                                     (org-back-to-heading)
                                     ;; strip off after first colon
                                     (s-trim-full (car (split-string (cic:get-headline-text (cic:get-current-line)) ":")))))
          (current-file buffer-file-name)
          (first-row t))
      (with-current-file-transient (concat otdb-recipe-temp-directory heading-name-collection ".org")
        (erase-buffer)
        (let (current-recipe-subtree
              (tmp-buffer-string ""))
          (do-org-table-rows current-file heading-name-collection current-row
                             (if first-row
                                 (setq first-row nil)
                               ;; ingredient should be empty if on invalid non-first row
                               (when (cic:is-not-empty-string-nil (elt current-row otdb-recipe-item-column))
                                 ;; lookup recipe and get subtree from there
                                 ;; do process to add one recipe to buffer, get this into a function
                                 (let ((recipe-location (otdb-recipe-find (elt current-row otdb-recipe-item-column))))
                                   (with-current-file-transient (car recipe-location)
                                     (goto-char (cadr recipe-location))
                                     (org-mark-subtree)
                                     (setq current-recipe-subtree (buffer-substring (region-beginning) (region-end))
                                           tmp-buffer-string      (concat tmp-buffer-string (otdb-recipe-add-tmp-buffer current-recipe-subtree))))))))
          (insert tmp-buffer-string)
          (goto-char (point-min))
          (otdb-recipe-add-latex-header heading-name-collection)
          (basic-save-buffer)
          (org-latex-export-to-pdf))))))

(defun otdb-recipe-export (&optional otdb-recipe-temp-directory)
  "Export the single recipe in a table containing recipes to a
pdf file in ~/tmp."
  (interactive)
  (require 'ox-publish)
  ;; TODO: requires a trailing slash if set elsewhere
  (unless otdb-recipe-temp-directory
    (setq otdb-recipe-temp-directory "~/tmp/"))
  (save-excursion
    ;; get current table
    (org-mark-subtree)
    (when (string-match ":recipe:" (save-excursion
                                     (org-back-to-heading)
                                     (cic:get-current-line)))
        (let ((heading-name (save-excursion
                              (org-back-to-heading)
                              ;; strip off after first colon
                              (s-trim-full (car (split-string (cic:get-headline-text (cic:get-current-line)) ":")))))
              (current-recipe-subtree (buffer-substring (region-beginning) (region-end))))
          (message (concat "Exporting recipe: "  heading-name))
          ;; put table in temporary file named after headline
          (with-current-file-transient (concat otdb-recipe-temp-directory heading-name ".org")
            (erase-buffer)
            (insert (otdb-recipe-add-tmp-buffer current-recipe-subtree))
            (goto-char (point-min))
            ;; put in the header
            (otdb-recipe-add-latex-header)
            (basic-save-buffer)
            (org-latex-export-to-pdf))))))

(defun otdb-recipe-add-latex-header (&optional title)
  "Add a LaTeX header to the temporary org-mode file for export
with an optional TITLE."
  (insert "#+AUTHOR:\n")
  (insert "#+DATE:\n")
  (insert "#+TITLE:\n")
  (insert "#+OPTIONS: toc:nil num:nil\n")
  (insert "#+LATEX_CLASS_OPTIONS: [landscape,10pt]\n")
  (insert "#+LATEX_HEADER: \\usepackage{nopageno}\n")
  (insert "#+LATEX_HEADER: \\usepackage{geometry}\n")
  (insert "#+LATEX_HEADER: \\newgeometry{landscape,left=1.0cm,right=0.2cm}\n")
  (when title
    (insert "#+BEGIN_LATEX\n")
    (insert "\\begin{center}\n")
    (insert "\\textbf{\\Huge " title "}\n")
    (insert "\\end{center}\n")
    (insert "#+END_LATEX\n")))

(defun otdb-recipe-add-tmp-buffer (current-recipe-subtree)
  "Add a recipe form CURRENT-RECIPE-SUBTREE to a temporary
buffer, then return the string after processing.  Generally
deletes volume, weights, and any comments."
  (with-temp-buffer
    (insert current-recipe-subtree)
    (goto-char (point-min))
    ;; do I find comments to delete???
    (when (search-forward "#+BEGIN_COMMENT" nil t)
      (beginning-of-line)
      ;; delete to end of buffer
      (kill-region (point) (point-max)))
    (goto-char (point-min))
    ;; get rid of tags?
    (when (string-match ":" (cic:get-current-line))
      (search-forward ":" nil t)
      (backward-char)
      (cic:kill-line-elisp))
    ;; add in latex attributes
    (goto-char (point-min))
    (cic:org-find-table)
    (insert "#+ATTR_LATEX: :center nil\n")
    ;; get rid of the last two columns
    (cic:org-find-table)
    (forward-line)
    (org-table-goto-column 13)
    (org-table-delete-column)
    (org-table-delete-column)
    (goto-char (point-min))
  (buffer-substring (point-min) (point-max))))

(defun otdb-recipe-calc-recipe (lisp-table lisp-table-no-seperators)
  "Calculated an updated lisp table from the LISP-TABLE and
LISP-TABLE-NO-SEPERATORS corresponding to a recipe."
  (let ((calories 0)
        (protein 0)
        (fat 0)
        (cost 0)
        (cost-calories-column)
        (cost-calories 0)
        (cost-protein-column)
        (cost-protein 0)
        (percent-carb-column)
        (percent-carb 0)
        (percent-protein-column)
        (percent-protein 0)
        (percent-fat-column)
        (percent-fat 0)
        (weight 0)
        (volume 0)
        (char-columns (otdb-table-parse-char-columns lisp-table-no-seperators))
        (new-lisp-table (list (car lisp-table-no-seperators))))
    ;; add up the directly summable columns
    (dolist (lisp-row (butlast (cdr lisp-table-no-seperators)))
      ;; only skip non-intermediate calculations
      (unless (otdb-table-check-invalid-current-row-lisp lisp-row char-columns)
        (setq calories (+ calories (otdb-table-lisp-row-float lisp-row 3))
              protein  (+ protein (otdb-table-lisp-row-float lisp-row 4))
              fat      (+ fat (otdb-table-lisp-row-float lisp-row 5))
              cost     (+ cost (otdb-table-lisp-row-float lisp-row 6))
              ;; do the appropriate sums from the added up columns
              ;; control for case when calories are zero
              weight   (+ weight (otdb-table-lisp-row-float lisp-row 12))
              volume   (+ volume (otdb-table-lisp-row-float lisp-row 13))))
      (if (otdb-table-lisp-row-check lisp-row 6)
          (progn
            (if (otdb-table-lisp-row-check lisp-row 3)
                (push (/ (otdb-table-lisp-row-float lisp-row 6) (/ (otdb-table-lisp-row-float lisp-row 3) 1000.0)) cost-calories-column)
              (push nil cost-calories-column))
            (if (otdb-table-lisp-row-check lisp-row 4)
                (push (/ (otdb-table-lisp-row-float lisp-row 6) (/ (otdb-table-lisp-row-float lisp-row 4) 100.0)) cost-protein-column)
              (push nil cost-protein-column)))
        (progn
          (push nil cost-calories-column)
          (push nil cost-protein-column)))
      (if (otdb-table-lisp-row-check lisp-row 3)
          (let ((row-calories (otdb-table-lisp-row-float lisp-row 3))
                (row-protein (otdb-table-lisp-row-float lisp-row 4))
                (row-fat (otdb-table-lisp-row-float lisp-row 5)))
            (push (* 100.0 (/ (- row-calories (+ (* 4.0 row-protein) (* 9.0 row-fat))) row-calories)) percent-carb-column)
            (push (* 100.0 (/ (* 4.0 row-protein) row-calories)) percent-protein-column)
            (push (* 100.0 (/ (* 9.0 row-fat) row-calories)) percent-fat-column))
        (progn
          (push nil percent-carb-column)
          (push nil percent-protein-column)
          (push nil percent-fat-column))))
    (setq cost-calories-column   (nreverse cost-calories-column)
          cost-protein-column    (nreverse cost-protein-column)
          percent-carb-column    (nreverse percent-carb-column)
          percent-protein-column (nreverse percent-protein-column)
          percent-fat-column     (nreverse percent-fat-column))
    (when (/= calories 0.0)
      (setq cost-calories   (/ cost (/ calories 1000.0))
            percent-carb    (* 100.0 (/ (- calories (+ (* 4.0 protein) (* 9.0 fat))) calories))
            percent-protein (* 100.0 (/ (* 4.0 protein) calories))
            percent-fat     (* 100.0 (/ (* 9.0 fat) calories))))
    (when (/= protein 0.0)
      (setq cost-protein (/ cost (/ protein 100.0))))
    ;; insert into last row
    (dolist (current-lisp-row (cdr (butlast lisp-table-no-seperators)))
      ;; TODO: this nconc can probably be eliminated
      (push (nconc
             (subseq current-lisp-row 0 7)
             (list
              (otdb-table-format-number-nil (pop cost-calories-column) 3)
              (otdb-table-format-number-nil (pop cost-protein-column) 3)
              (otdb-table-format-number-nil (pop percent-carb-column) 3)
              (otdb-table-format-number-nil (pop percent-protein-column) 3)
              (otdb-table-format-number-nil (pop percent-fat-column) 3))
             (nthcdr 12 current-lisp-row))
            new-lisp-table))
    (push (list
           (caar (last lisp-table-no-seperators))
           ""
           ""
           (otdb-table-format-number-zero calories 1)
           (otdb-table-format-number-zero protein 1)
           (otdb-table-format-number-zero fat 1)
           (otdb-table-format-number-zero cost 2)
           (otdb-table-format-number-zero cost-calories 3)
           (otdb-table-format-number-zero cost-protein 3)
           (otdb-table-format-number-zero percent-carb 3)
           (otdb-table-format-number-zero percent-protein 3)
           (otdb-table-format-number-zero percent-fat 3)
           (otdb-table-format-number-zero weight 2)
           (otdb-table-format-number-zero volume 2))
          new-lisp-table)
    (push (nthcdr 13 (last lisp-table-no-seperators)) new-lisp-table)
    (setq new-lisp-table (nreverse new-lisp-table))))

(defun otdb-recipe-calc-in-special-buffer-all ()
  "Recursively calculate all items from current recipe in special
temporary buffer."
  (interactive)
  (otdb-recipe-calc-in-special-buffer 'all))

(defun otdb-recipe-calc-in-special-buffer-check ()
  "Recursively calculate checked items from current recipe in
special temporary buffer."
  (interactive)
  (otdb-recipe-calc-in-special-buffer 'check))

(defun otdb-recipe-calc-in-special-buffer-cost ()
  "Recursively calculate items marked cost from current recipe in
special temporary buffer."
  (interactive)
  (otdb-recipe-calc-in-special-buffer 'cost))

(defun otdb-recipe-calc-in-special-buffer-tag ()
  "Recursively calculate items with specified tags from current
recipe in special temporary buffer."
  (interactive)
  (otdb-recipe-calc-in-special-buffer 'tag))

(defun otdb-recipe-calc-in-special-buffer-pattern ()
  "Recursively calculate items matching specified patterns from
current recipe in special temporary buffer."
  (interactive)
  (otdb-recipe-calc-in-special-buffer 'pattern))

(defun otdb-recipe-calc-in-special-buffer (calculation-type)
  "Command to create a temporary buffer for a recipe that is
recursively calculated and often filtered in some way based on
CALCULATION-TYPE."
  (interactive)
  (let ((the-new-buffer (let (the-new-buffer-2)
                          (when (eq (otdb-table-detect) 'recipe)
                            (cond (otdb-recipe-item-pattern
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-recipe-pattern--" otdb-recipe-item-pattern "--" (cic:datestamp-current-time) "*"))))
                                  (otdb-recipe-item-tags
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-recipe-tags--" otdb-recipe-item-tags "--" (cic:datestamp-current-time) "*"))))
                                  (t
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-recipe--" (cic:datestamp-current-time) "*")))))
                            (with-current-buffer the-new-buffer-2
                              (org-mode)
                              (otdb-recipe-mode)
                              (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
                              (insert "  | Quantity | Ingredient | Note | Cal | Pro | Fat | $ | $/kCal | $/100g pro | % carb | % pro | % fat | Weight | Volume | Tags | X |\n")
                              (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n"))
                            the-new-buffer-2))))
    (otdb-recipe-calc-special (cic:org-table-to-lisp-no-separators) the-new-buffer 1 calculation-type)
    (with-current-buffer the-new-buffer
      (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
      (insert "  | 1        |            |      |     |     |     |   |        |            |        |       |       |        |        |      |   |\n")
      (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
      (insert "  #+TBLEL: otdb-recipe-calc-recipe\n")
      (forward-line -2)
      (org-table-align)
      (beginning-of-line)
      (tblel-eval)
      (goto-char (point-min)))
    (switch-to-buffer the-new-buffer)))

(defun otdb-recipe-calc-special (lisp-table current-temporary-buffer &optional quantity calculation-type)
  "Do a special calculation on the current recipe specified by
LISP-TABLE in CURRENT-TEMPORARY-BUFFER with QUANTITY potentially
different than 1 and filtered by CALCULATION-TYPE."
  (let (current-collection-name
        (char-columns (otdb-table-parse-char-columns lisp-table))
        lisp-row-quantity
        count)
    (unless quantity
      (setq quantity 1))
    ;; find all the current rows
    (dolist (lisp-row (butlast (cdr lisp-table)))
      (let ((recipe-location (otdb-recipe-find (elt lisp-row otdb-recipe-item-column))))
        (cond (recipe-location
               (unless (otdb-table-check-invalid-current-row-lisp lisp-row char-columns)
                 (with-current-file-transient-min (car recipe-location)
                   ;; TODO: open everything up?
                   (goto-char (cadr recipe-location))
                   (cic:org-find-table)
                   ;; advance to table
                   (otdb-recipe-calc-special (cic:org-table-to-lisp-no-separators) current-temporary-buffer (* quantity (otdb-table-number (elt lisp-row otdb-recipe-quantity-column))) calculation-type))))
              ((or
                (eq calculation-type 'all)
                (and (eq calculation-type 'check)   (otdb-table-check-current-row-lisp lisp-row char-columns "X"))
                (and (eq calculation-type 'cost)    (otdb-table-check-current-row-lisp lisp-row char-columns "C"))
                (and (eq calculation-type 'tag)     (and otdb-recipe-item-tags    (string-match otdb-recipe-item-tags    (elt lisp-row otdb-recipe-check-column))))
                (and (eq calculation-type 'pattern) (and otdb-recipe-item-pattern (string-match otdb-recipe-item-pattern (elt lisp-row otdb-recipe-item-column)))))
               (with-current-buffer current-temporary-buffer
                 (if (/= quantity 1)
                     (progn
                       (setq lisp-row-quantity nil
                             count             0)
                       (dolist (e lisp-row)
                         (if (member count '(0 3 4 5 6 12 13))
                             (push (otdb-recipe-multiply-preserve e quantity) lisp-row-quantity)
                           (push e lisp-row-quantity))
                         (setq count (1+ count)))
                       (setq lisp-row-quantity (nreverse lisp-row-quantity))
                       (insert (concat " | " (mapconcat 'identity lisp-row-quantity " | ") "\n")))
                   (insert (concat " | " (mapconcat 'identity lisp-row " | ") "\n"))))))))))

(defun otdb-recipe-multiply-preserve (thestring quantity)
  "Multiply THESTRING by QUANTITY and preserve units."
  (if (otdb-table-number thestring)
      (let* ((thequantity (otdb-table-number thestring))
             (theunit (otdb-table-unit thestring)))
        (concat (number-to-string (* quantity thequantity)) theunit))
    thestring))

;; TODO: eventually get rid of ignore-errors, makes impossible to
;; debug
(defun otdb-table-tblel-calorie-protein-cost-table (lisp-table lisp-table-no-seperators &rest tblel-args)
  "Table tblel function for TBLFM to calculate cost per 1000
calories in the database and cost per 100g protein."
  (dolist (therow (nthcdr 1 lisp-table-no-seperators))
    (let* ((package-weight   (elt therow 1))
           (package-volume   (elt therow 2))
           (package-cost     (elt therow 3))
           (serving-weight   (elt therow 4))
           (serving-volume   (elt therow 5))
           (serving-calories (elt therow 6))
           (serving-protein  (elt therow 7))
           ;; see comments in otdb-recipe-database-calorie-costs
           (calorie-cost (cic:number-to-string-nan (ignore-errors (cond ((and (cic:full-string-p package-weight) (cic:full-string-p serving-weight))
                                                                         (let ((factor (otdb-table-unit-conversion 'weight (otdb-table-unit package-weight) (otdb-table-unit serving-weight))))
                                                                           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-weight) (otdb-table-number serving-weight)) (otdb-table-number serving-calories))) 1000)))
                                                                        ((and (cic:full-string-p package-volume) (cic:full-string-p serving-volume))
                                                                         (let ((factor (otdb-table-unit-conversion 'volume (otdb-table-unit package-volume) (otdb-table-unit serving-volume))))
                                                                           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-volume) (otdb-table-number serving-volume)) (otdb-table-number serving-calories))) 1000)))
                                                                        (t
                                                                         nil)))))
           ;; see comments in otdb-recipe-database-protein-costs
           (protein-cost (cic:number-to-string-nan (ignore-errors (cond ((and (cic:full-string-p package-weight) (cic:full-string-p serving-weight))
                                                                         (let ((factor (otdb-table-unit-conversion 'weight (otdb-table-unit package-weight) (otdb-table-unit serving-weight))))
                                                                           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-weight) (otdb-table-number serving-weight)) (otdb-table-number serving-protein))) 100)))
                                                                        ((and (cic:full-string-p package-volume) (cic:full-string-p serving-volume))
                                                                         (let ((factor (otdb-table-unit-conversion 'volume (otdb-table-unit package-volume) (otdb-table-unit serving-volume))))
                                                                           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-volume) (otdb-table-number serving-volume)) (otdb-table-number serving-protein))) 100)))
                                                                        (t
                                                                         nil))))))
      ;; add to row
      (setcar (nthcdr 11 therow) calorie-cost)
      (setcar (nthcdr 12 therow) protein-cost)))
  lisp-table-no-seperators)

(provide 'otdb-recipe)
