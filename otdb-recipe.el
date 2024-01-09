;;; otdb-recipe.el --- Allows easy entry of nutritional information
;;; into org-mode tables and calculation of costs and macronutrient
;;; amounts.
;;
;; Copyright (C) 2015-2023, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <boreal6502@gmail.com>
;; Created: Sun Apr 5, 2015
;; Version: 20231116
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

(require 'cl)
(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions for different collections of recipe files

(defun otdb-recipe-mode-hook--init ()
  "Initialize otdb-recipe-mode with some extra functionality."
  (when (and (derived-mode-p 'otdb-recipe-mode) (functionp 'hl-line-mode))
    (hl-line-mode 1)))
(add-hook 'otdb-recipe-mode-hook 'otdb-recipe-mode-hook--init)

(defun otdb-recipe-get-variable (ctx lookup-variable)
  "Helper function to lookup different otdb-recipe variables
depending on context."
  (let ((current-filename (ignore-errors buffer-file-name))
        (normal-recipe-files ctx))
    (cdr (assoc lookup-variable normal-recipe-files))))
;; otdb-recipe-normal-alist

(defun otdb-recipe-reset-filters ()
  "Reset all specified filters."
  (interactive)
  (setq otdb-recipe-item-last-pattern nil
        otdb-recipe-item-pattern      nil
        otdb-recipe-item-last-tags    nil
        otdb-recipe-item-tags         nil))

(define-derived-mode otdb-recipe-mode org-mode "org-table database recipe mode"
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))

(defvar otdb-recipe-mode-map
  (let ((map (make-sparse-keymap)))
    (otdb-table-skeleton-map map))
  "The keymap for otdb-recipe-mode.")

(defun otdb-recipe-get-calories-protein-fat-weight-volume-cost (ctx recipe)
  "Get the totals from a particular RECIPE. Generally to use in a
another recipe."
  (let ((recipe-location (otdb-recipe-find ctx recipe)))
    (with-current-file-transient (car recipe-location)
      (goto-char (cadr recipe-location))
      (cic:org-find-table)
      ;; (cic:org-table-last-row)
      (let* ((table-lisp (cic:org-table-to-lisp-no-separators))
             (last-row (mapcar 'substring-no-properties (car (last table-lisp)))))
        (mapcar (lambda (e)
                  (list (car e) (string-to-number (nth (cdr e) last-row))))
                ;; TODO can probably refactor into a constant "column pairs"
                (list (cons 'calories (otdb-column ctx 'calories))
                      (cons 'protein (otdb-column ctx 'protein))
                      (cons 'fat (otdb-column ctx 'fat))
                      (cons 'weight (otdb-column ctx 'weight))
                      (cons 'volume (otdb-column ctx 'volume))
                      (cons 'cost (otdb-column ctx 'cost))))))))

;; find recipes
(defun otdb-recipe-find (ctx recipe)
  "Find RECIPE and return the location.
TODO return location at beginning of line"
  ;; have an "exact" way of doing this
  (let (location)
    (dolist (recipe-file (otdb-ctx ctx 'collection-files))
      (with-current-file-transient-min recipe-file
        (let ((found (when  (re-search-forward (concat "^\* " recipe " :recipe:") nil t)
                       (line-beginning-position))))
          (when found
            (setq location (list recipe-file found))))))
    location))

(defun otdb-recipe-get-recipes (ctx)
  "Get the full list of recipes."
  (if otdb-table-collections-cache
      otdb-table-collections-cache
    (let (table
          table-name
          recipe-list)
      (dolist (recipe-file (otdb-ctx ctx 'collection-files))
        (do-org-tables recipe-file table-name table
                       (save-match-data
                         (when (string-match "\\(.*\\) :recipe:" table-name)
                           (push (match-string 1 table-name) recipe-list)))))
      (setq otdb-table-collections-cache recipe-list)
      recipe-list)))

(defun otdb-recipe-complete (ctx)
  "Select and complete a recipe name, then go to it."
  (interactive)
  (let* ((recipe-list (otdb-recipe-get-recipes ctx))
         (recipe (completing-read "Recipe: " recipe-list nil t))
         (recipe-location (otdb-recipe-find ctx recipe)))
    (find-file (car recipe-location))
    (goto-char (cadr recipe-location))
    (outline-show-subtree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recipes
(defun otdb-recipe-calories-protein-fat-weight (ingredient-row quantity column to-unit)
  "Return the nutritional values of QUANTITY from database row
INGREDIENT-ROW from COLUMN converting to units TO-UNIT.  Does the
right thing based on information available."
  ;; TODO: combine with very similar cost function
  (let* ((ingredient-package-weight (otdb-database-column-value ctx 'package-weight ingredient-row))
         (ingredient-package-volume (otdb-database-column-value ctx 'package-volume ingredient-row))
         (ingredient-serving-weight (otdb-database-column-value ctx 'serving-weight ingredient-row))
         (ingredient-serving-volume (otdb-database-column-value ctx 'serving-volume ingredient-row))
         (package-volume-p (cic:full-string-p ingredient-package-volume))
         (serving-volume-p (cic:full-string-p ingredient-serving-volume))
         (package-weight-p (cic:full-string-p ingredient-package-weight))
         (serving-weight-p (cic:full-string-p ingredient-serving-weight))
         ;; used to convert within servings
         ;; TODO: should to-quantity be called to-servings
         (package-volume-number (otdb-table-number ingredient-package-volume))
         (package-volume-unit (otdb-table-unit ingredient-package-volume))
         (package-weight-number (otdb-table-number ingredient-package-weight))
         (package-weight-unit (otdb-table-unit ingredient-package-weight))
         (quantity-number (otdb-table-number quantity))
         (quantity-unit (otdb-table-unit quantity))
         (serving-volume-number (otdb-table-number ingredient-serving-volume))
         (serving-volume-unit (otdb-table-unit ingredient-serving-volume))
         (serving-weight-number (otdb-table-number ingredient-serving-weight))
         (serving-weight-unit (otdb-table-unit ingredient-serving-weight))
         (unit-type (otdb-table-unit-type quantity-unit))
         (to-quantity (cl-case unit-type
                        (weight
                         ingredient-serving-weight)
                        (volume
                         ingredient-serving-volume)
                        ;; TODO want to know what to do when quantity is dimensionless, e.g., KD has no volume serving
                        (t
                         ingredient-serving-volume))))
    (cond
     ((and (not unit-type)
           (cic:full-string-p (nth (otdb-database-column ctx 'serving-volume) ingredient-row))
           (not (otdb-table-unit-type (nth (otdb-database-column ctx 'serving-volume) ingredient-row))))
      ;; dimensionless units are volume
      (*
       (/
        quantity-number
        serving-volume-number)
       (cic:string-to-float (nth column ingredient-row))))
     ((and (not unit-type)
           package-volume-p
           serving-volume-p
           (otdb-table-unit-type ingredient-serving-volume))
      (* (/ (* quantity-number
               package-volume-number)
            serving-volume-number)
         (otdb-table-unit-conversion 'volume package-volume-unit serving-volume-unit)
         (cic:string-to-float (nth column ingredient-row))))
     ((and (not unit-type)
           package-weight-p
           serving-weight-p
           (otdb-table-unit-type ingredient-serving-weight)
           package-volume-p
           (not (otdb-table-unit-type ingredient-package-volume)))
      (* quantity-number
         (/ (/ package-weight-number
               package-volume-number)
            serving-weight-number)
         (otdb-table-unit-conversion 'weight package-weight-unit serving-weight-unit)
         (cic:string-to-float (nth column ingredient-row))))
     ((and (not unit-type)
           package-weight-p
           serving-weight-p
           (otdb-table-unit-type ingredient-serving-weight))
      (* (/ (* quantity-number
               package-weight-number)
            serving-weight-number)
         (otdb-table-unit-conversion 'weight package-weight-unit serving-weight-unit)
         (cic:string-to-float (nth column ingredient-row))))
     (t
      (*
       (/ quantity-number
          (otdb-table-number to-quantity))
       (otdb-table-unit-conversion unit-type quantity-unit (otdb-table-unit to-quantity))
       (cic:string-to-float (nth column ingredient-row)))))))

(defun otdb-recipe-cost-row (ingredient-row quantity)
  "Return the cost based on the QUANTITY in INGREDIENT-ROW.
Tries to do the right thing with different types of units."
  (let* ((ingredient-package-weight (otdb-database-column-value ctx 'package-weight ingredient-row))
         (ingredient-package-volume (otdb-database-column-value ctx 'package-volume ingredient-row))
         (ingredient-cost           (otdb-database-column-value ctx 'cost ingredient-row))
         (ingredient-serving-weight (otdb-database-column-value ctx 'serving-weight ingredient-row))
         (ingredient-serving-volume (otdb-database-column-value ctx 'serving-volume ingredient-row))
         (package-volume-p (cic:full-string-p ingredient-package-volume))
         (package-weight-p (cic:full-string-p ingredient-package-weight))
         (serving-weight-p (cic:full-string-p ingredient-serving-weight))
         (cost-number (otdb-table-number ingredient-cost))
         (package-volume-number (otdb-table-number ingredient-package-volume))
         (package-volume-unit (otdb-table-unit ingredient-package-volume))
         (package-weight-number (otdb-table-number ingredient-package-weight))
         (package-weight-unit (otdb-table-unit ingredient-package-weight))
         (quantity-number (otdb-table-number quantity))
         (quantity-unit (otdb-table-unit quantity))
         (serving-volume-number (otdb-table-number ingredient-serving-volume))
         (serving-volume-unit (otdb-table-unit ingredient-serving-volume))
         (serving-weight-number (otdb-table-number ingredient-serving-weight))
         (serving-weight-unit (otdb-table-unit ingredient-serving-weight))
         (unit-type (otdb-table-unit-type quantity-unit)))
    ;; figure out if we have both weight units for cost or not
    (cl-case unit-type
      (weight
       (cond ((not package-weight-p)
              (*
               ;; quantity
               quantity-number
               ;; cost-numbers / quantity-units
               (otdb-table-unit-conversion
                'volume
                quantity-unit
                package-weight-unit)
               ;; cost-quantity / cost)
               (/ cost-number
                  package-weight-number)))
             ;; all weight
             ((and package-weight-p
                   serving-weight-p)
              (*
               (/
                quantity-number
                package-weight-number)
               (otdb-table-unit-conversion
                'weight
                quantity-unit
                package-weight-unit)
               cost-number))
             (t
              ;; quantity / serving-quantity
              (*
               (/
                quantity-number
                serving-weight-number)
               ;;  serving-units / quantity-units
               (otdb-table-unit-conversion
                'volume
                quantity-unit
                serving-weight-unit)
               ;; serving-quantity(alt)
               serving-volume-number
               ;;  serving-units(alt)/cost-numbers
               (otdb-table-unit-conversion
                'weight
                serving-volume-unit
                package-volume-unit)
               ;;  cost-quantity/cost
               (/
                (cic:string-to-float ingredient-cost)
                package-volume-number)))))
      (volume
       (cond (package-volume-p
              (*
               ;; quantity
               quantity-number
               ;; cost-numbers / quantity-units
               (otdb-table-unit-conversion
                'volume
                quantity-unit
                package-volume-unit)
               ;; cost-quantity / cost)
               (/ cost-number
                  package-volume-number)))
             (t
              (*
               ;; quantity / serving-quantity
               (/
                quantity-number
                serving-volume-number)
               ;;  serving-units / quantity-units
               (otdb-table-unit-conversion
                'volume
                quantity-unit
                serving-volume-unit)
               ;; serving-quantity(alt)/serving-quantity
               serving-weight-number
               ;;  serving-units(alt)/cost-numbers
               (otdb-table-unit-conversion
                'weight
                serving-weight-unit
                package-weight-unit)
               ;;  cost-quantity/cost
               (/
                (cic:string-to-float ingredient-cost)
                package-weight-number)))))
      (t
       (cond ((not (cic:full-string-p package-volume-unit))
              (if (and (not unit-type) (not package-volume-p))
                  cost-number
                (*
                 (/
                  quantity-number
                  package-volume-number)
                 cost-number)))
             (t
              (*
               quantity-number
               cost-number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tables

(defun otdb-recipe-lookup-function (ctx row-list)
  "Helper function for otdb-table-update to lookup information
for ROW-LIST from a particular recipe.
Test on an actual table with (otdb-recipe-lookup-function (cic:org-table-to-lisp-no-separators))"
  (let (database-row-alist
        calories-protein-fat-weight-volume-cost-list
        key-list
        quantity-alist
        recipe-calories-protein-fat-weight-volume-cost-list
        (recipe-list (otdb-recipe-get-recipes ctx)))
    ;; get list of keys to lookup
    (dolist (row (cdr row-list))
      ;; get the key if applicable
      (if (member (s-trim-full-no-properties (otdb-column-value ctx 'item row)) recipe-list)
          (setq recipe-calories-protein-fat-weight-volume-cost-list
                (let* ((row-quantity (otdb-column-value ctx 'quantity row))
                       (row-item (otdb-column-value ctx 'item row))
                       (ccl (otdb-recipe-get-calories-protein-fat-weight-volume-cost ctx row-item)))
                  (push (append
                         (list (s-trim-full-no-properties row-item))
                         (mapcar (lambda (e)
                                   (list e (ignore-errors (* (otdb-table-number row-quantity) (cadr (assoc e ccl))))))
                                 '(calories protein fat weight volume cost))
                         (list ""))
                        recipe-calories-protein-fat-weight-volume-cost-list)))
        (progn
          (push (list (s-trim-full-no-properties (otdb-column-value ctx 'item row))
                      (s-trim-full-no-properties (otdb-column-value ctx 'quantity row)))
                quantity-alist)
          (push (s-trim-full-no-properties (otdb-column-value ctx 'item row)) key-list))))
    ;; get the database rows
    (setq database-row-alist (otdb-table-item-row-multiple
                              (otdb-ctx ctx 'database)
                              (otdb-ctx ctx 'database-headline)
                              key-list 1))
    ;; calculate both cost and other things
    (dolist (row-alist database-row-alist)
      (let ((row (cadr row-alist))
            (quantity (cadr (assoc (car row-alist) quantity-alist))))
        (let ((calories
               (when (cic:full-string-p (otdb-database-column-value ctx 'serving-calories row))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity (otdb-database-column ctx 'serving-calories) nil))))
              (protein
               (when (cic:full-string-p (otdb-database-column-value ctx 'serving-protein row))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity (otdb-database-column ctx 'serving-protein) nil))))
              (fat
               (when (cic:full-string-p (otdb-database-column-value ctx 'serving-fat row))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity (otdb-database-column ctx 'serving-fat) nil))))
              (weight
               (if (cic:full-string-p (otdb-database-column-value ctx 'serving-weight row))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity (otdb-database-column ctx 'serving-weight) nil))
                 (when (cic:full-string-p (nth 4 row))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity quantity (otdb-database-column ctx 'serving-weight) nil)))))
              (volume
               (when (cic:full-string-p (otdb-database-column-value ctx 'serving-volume row))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity (otdb-database-column ctx 'serving-volume) nil))))
              (tags
               (when (cic:full-string-p (otdb-database-column-value ctx 'serving-tags row))
                 (nth 13 row)))
              (cost (ignore-errors
                      (otdb-recipe-cost-row row quantity))))
          (push (list (car row-alist)
                      (list 'calories calories)
                      (list 'protein protein)
                      (list 'fat fat)
                      (list 'weight weight)
                      (list 'volume volume)
                      (list 'cost cost)
                      (list 'tags tags))
                calories-protein-fat-weight-volume-cost-list))))
    (append recipe-calories-protein-fat-weight-volume-cost-list calories-protein-fat-weight-volume-cost-list)))

(defun otdb-recipe-insert-function (ctx recipe-filename recipe-heading calories-protein-fat-weight-volume-cost-list)
  "Helper function for otdb-table-update to insert information
into a recipe.  The recipe is RECIPE-HEADING in RECIPE-FILENAME
with information to be inserted of
CALORIES-PROTEIN-FAT-WEIGHT-VOLUME-COST-LIST."
  (let ((count 1))
    (do-org-table-rows recipe-filename recipe-heading row
                       (setq new-ingredient (s-trim-full-no-properties (otdb-column-value ctx 'item row)))
                       (unless (equal count 1)
                         (let* ((ingredient-list (assoc new-ingredient calories-protein-fat-weight-volume-cost-list))
                                (new-calories (cadr (assoc 'calories ingredient-list)))
                                (new-protein  (cadr (assoc 'protein  ingredient-list)))
                                (new-fat      (cadr (assoc 'fat      ingredient-list)))
                                (new-weight   (cadr (assoc 'weight   ingredient-list)))
                                (new-volume   (cadr (assoc 'volume   ingredient-list)))
                                (new-cost     (cadr (assoc 'cost     ingredient-list)))
                                (new-tags     (cadr (assoc 'tags     ingredient-list))))
                           (if (not new-calories)
                               (org-table-put count (+ (otdb-column ctx 'calories) 1) "" )
                             (org-table-put count (+ (otdb-column ctx 'calories) 1) (format "%.1f" new-calories)))
                           (if (not new-protein)
                               (org-table-put count (+ (otdb-column ctx 'protein) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'protein) 1) (format "%.1f" new-protein)))
                           (if (not new-fat)
                               (org-table-put count (+ (otdb-column ctx 'fat) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'fat) 1) (format "%.1f" new-fat)))
                           (if (not new-weight)
                               (org-table-put count (+ (otdb-column ctx 'weight) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'weight) 1) (format "%.3f" new-weight)))
                           (if (not new-volume)
                               (org-table-put count (+ (otdb-column ctx 'volume) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'volume) 1)(format "%.3f" new-volume)))
                           (if (not new-cost)
                               (org-table-put count (+ (otdb-column ctx 'cost) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'cost) 1) (format "%.3f" new-cost)))
                           (if (not new-tags)
                               (org-table-put count (+ (otdb-column ctx 'tags) 1) "")
                             (org-table-put count (+ (otdb-column ctx 'tags) 1) new-tags))))
                       (setq count (1+ count)))
    (tblel-eval)))

;; TODO: appears broken
(defun otdb-recipe-find-ingredient (ctx ingredient)
  "Find the location of INGREDIENT ingredient in database (or
recipe)."
  ;; TODO this will probably become a pretty general table lookup function for databases
  (if (member ingredient (otdb-recipe-get-recipes ctx))
      (otdb-recipe-find ctx ingredient)
    (cic:org-table-lookup-location (otdb-ctx ctx 'database)
                                   (otdb-ctx ctx 'database-headline)
                                   ingredient 1)))

;; TODO: appears to only be called from other broken functions
(defun otdb-recipe-get-ingredients (ctx)
  "Get list of all ingredients from the database."
  ;; TODO: will need to be modified for multiple files
  (let (ingredients
        dups)
    (dolist (database (otdb-ctx ctx 'database))
      (setq ingredients (append ingredients (cic:org-table-get-keys database (otdb-ctx ctx 'database-headline)))))
    (setq dups (cic:get-list-duplicates ingredients))
    (when (> (length dups) 0)
      (cic:mpp-echo (concat "Duplicate ingredients: " (pp-to-string dups)) (otdb-ctx ctx 'message-buffer)))
    ingredients))

;; TODO: will need to modified for multiple files
;; TODO: does not appear to be used....
(defun otdb-recipe-ingredient-row (ctx ingredient)
  "Look up row of INGREDIENT ingredient in the database."
  ;; XXXX assume on top of table
  ;; XXXX assume Ingredient.... etc. header is first
  (cic:org-table-lookup-row (otdb-ctx ctx 'database)
                            ;; TODO: this variable will have to be modified for multiple files
                            (otdb-ctx ctx 'database-headline)
                            ingredient))

(defun otdb-recipe-export-multiple (&optional ctx otdb-recipe-temp-directory)
  "Export each component recipe in a table containing recipes to a pdf file in ~/tmp/otdb-recipes"
  (interactive)
  (require 'ox-publish)
  ;; loop over rows in current table
  (unless otdb-recipe-temp-directory
    (setq otdb-recipe-temp-directory "~/tmp/otdb-recipes/")
    (make-directory otdb-recipe-temp-directory t))
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
                               (when (cic:is-not-empty-string-nil (otdb-column-value ctx 'item current-row))
                                 ;; lookup recipe and get subtree from there
                                 ;; do process to add one recipe to buffer, get this into a function
                                 (let ((recipe-location (otdb-recipe-find ctx (otdb-column-value ctx 'item current-row))))
                                   (with-current-file-transient (car recipe-location)
                                     (goto-char (cadr recipe-location))
                                     (org-mark-subtree)
                                     (setq current-recipe-subtree (buffer-substring (region-beginning) (region-end))
                                           tmp-buffer-string      (concat tmp-buffer-string (otdb-recipe-create-tmp-org current-recipe-subtree 'no-header))))))))
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
    (setq otdb-recipe-temp-directory "~/tmp/otdb-recipes/"))
  (make-directory otdb-recipe-temp-directory t)
  (save-excursion
    ;; get current table
    (org-mark-subtree)
    ;; TODO: unmark
    (when (string-match-p ":recipe:" (save-excursion
                                       (org-back-to-heading)
                                       (cic:get-current-line)))
      (let ((heading-name (save-excursion
                            (org-back-to-heading)
                            ;; strip off after first colon
                            (s-trim-full (car (split-string (cic:get-headline-text (cic:get-current-line)) ":")))))
            (current-recipe-subtree (buffer-substring (region-beginning) (region-end)))
            (org-export-with-broken-links t))
        (message (concat "Exporting recipe: "  heading-name))
        ;; put table in temporary file named after headline
        (with-current-file-transient-literal (concat otdb-recipe-temp-directory heading-name ".org")
          (erase-buffer)
          (insert (otdb-recipe-create-tmp-org current-recipe-subtree))
          (basic-save-buffer)
          (goto-char (point-min))
          (org-latex-export-to-pdf))))))

(defun otdb-recipe-add-latex-header (&optional title)
  "Add a LaTeX header to the temporary org-mode file for export
with an optional TITLE."
  (insert "#+AUTHOR:\n"
          "#+DATE:\n"
          "#+TITLE:\n"
          "#+OPTIONS: toc:nil num:nil\n"
          "#+LATEX_CLASS_OPTIONS: [landscape,10pt]\n"
          "#+LATEX_HEADER: \\usepackage{nopageno}\n"
          "#+LATEX_HEADER: \\usepackage{geometry}\n"
          "#+LATEX_HEADER: \\newgeometry{landscape,left=1.0cm,right=0.2cm}\n")
  (when title
    (insert "#+BEGIN_LATEX\n"
            "\\begin{center}\n"
            "\\textbf{\\Huge " title "}\n"
            "\\end{center}\n"
            "#+END_LATEX\n")))

(defun otdb-recipe-create-tmp-org (current-recipe-subtree &optional no-header)
  "Add a recipe form CURRENT-RECIPE-SUBTREE to a temporary
buffer, then return the string after processing.  Generally
deletes volume, weights, and any comments."
  ;; split into parts and process
  (let (header
        table
        table-elisp
        footer
        new-header
        new-table
        new-footer)
    (with-temp-buffer
      (insert current-recipe-subtree)
      (goto-char (point-min))
      (cic:org-find-table)
      (beginning-of-line)
      (setq header (buffer-substring-no-properties (point-min) (org-table-begin)))
      (setq table (buffer-substring-no-properties (org-table-begin) (org-table-end)))
      (setq footer (buffer-substring-no-properties (org-table-end) (point-max)))
      (setq table-elisp (copy-sequence (org-table-to-lisp table))))
    (setq table-elisp (mapcar (lambda (e)
                                (if (eq e 'hline)
                                    e
                                  (append (cl-subseq e 0 12) (subseq e 14))))
                              table-elisp))
    (with-temp-buffer
      (setq new-header (with-temp-buffer
                         (insert header)
                         (goto-char (point-min))
                         (when (string-match-p ":" (cic:get-current-line))
                           (search-forward ":" nil t)
                           (backward-char)
                           (cic:kill-line-elisp))
                         (goto-char (point-min))
                         (unless no-header
                           (otdb-recipe-add-latex-header))
                         (buffer-substring-no-properties (point-min) (point-max))))
      (insert new-header)
      (setq new-table (with-temp-buffer
                        (insert "#+ATTR_LATEX: :center nil\n"
                                (cic:org-table-create-from-elisp table-elisp))
                        (goto-char (point-min))
                        (search-forward "|---")
                        (forward-line)
                        (search-forward "| ")
                        (org-table-align)
                        (buffer-substring-no-properties (point-min) (point-max))))
      (insert new-table)
      (setq new-footer (with-temp-buffer
                         (insert footer)
                         (goto-char (point-min))
                         (when (search-forward "#+BEGIN_COMMENT" nil t)
                           (beginning-of-line)
                           ;; delete to end of buffer
                           (cic:otdb-delete-until-end-of-buffer))
                         (goto-char (point-min))
                         ;; delete all subheadings
                         (when (org-goto-first-child)
                           (cic:otdb-delete-until-end-of-buffer))
                         (buffer-substring-no-properties (point-min) (point-max))))
      (insert new-footer)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun cic:org-table-create-from-elisp (elisp-table)
  "Create an org table (in text form) from ELISP-TABLE (in lisp
form)."
  (with-temp-buffer
    (dolist (row elisp-table)
      (if (eq row 'hline)
          (insert "|------|\n")
        (progn
          (insert "| ")
          (dolist (cell row)
            (insert cell
                    " |"))
          (insert "\n"))))
    (goto-char (point-min))
    (goto-char (org-table-begin))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cic:otdb-delete-until-end-of-buffer ()
  "Delete until end of buffer, but do not touch any kill rings or
clipboards."
  (let (kill-ring
        kill-whole-line
        kill-ring-yank-pointer
        (save-interprogram-paste-before-kill nil)
        (interprogram-cut-function nil))
    (delete-region (point) (point-max))))

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
        (setq calories (+ calories (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'calories)))
              protein  (+ protein (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'protein)))
              fat      (+ fat (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'fat)))
              cost     (+ cost (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'cost)))
              ;; do the appropriate sums from the added up columns
              ;; control for case when calories are zero
              weight   (+ weight (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'weight)))
              volume   (+ volume (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'volume)))))
      (if (otdb-table-lisp-row-check lisp-row (otdb-column ctx 'cost))
          (progn
            (if (otdb-table-lisp-row-check lisp-row (otdb-column ctx 'calories))
                (push (/ (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'cost))
                         (/ (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'calories)) 1000.0))
                      cost-calories-column)
              (push nil cost-calories-column))
            (if (otdb-table-lisp-row-check lisp-row (otdb-column ctx 'protein))
                (push (/ (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'cost))
                         (/ (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'protein)) 100.0))
                      cost-protein-column)
              (push nil cost-protein-column)))
        (progn
          (push nil cost-calories-column)
          (push nil cost-protein-column)))
      (if (otdb-table-lisp-row-check lisp-row 3)
          (let ((row-calories (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'calories)))
                (row-protein (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'protein)))
                (row-fat (otdb-table-lisp-row-float lisp-row (otdb-column ctx 'fat))))
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
             (cl-subseq current-lisp-row 0 7)
             (list
              (otdb-table-format-number-nil (pop cost-calories-column) (otdb-precision ctx 'cost-calories))
              (otdb-table-format-number-nil (pop cost-protein-column) (otdb-precision ctx 'cost-protein))
              (otdb-table-format-number-nil (pop percent-carb-column) (otdb-precision ctx 'percent-carb))
              (otdb-table-format-number-nil (pop percent-protein-column) (otdb-precision ctx 'percent-protein))
              (otdb-table-format-number-nil (pop percent-fat-column) (otdb-precision ctx 'percent-fat)))
             ;; TODO: fix this 12
             (nthcdr 12 current-lisp-row))
            new-lisp-table))
    (push (list
           (caar (last lisp-table-no-seperators))
           ""
           ""
           (otdb-table-format-number-zero calories (otdb-precision ctx 'calories))
           (otdb-table-format-number-zero protein (otdb-precision ctx 'protein))
           (otdb-table-format-number-zero fat (otdb-precision ctx 'fat))
           (otdb-table-format-number-zero cost (otdb-precision ctx 'cost))
           (otdb-table-format-number-zero cost-calories (otdb-precision ctx 'cost-calories))
           (otdb-table-format-number-zero cost-protein (otdb-precision ctx 'cost-protein))
           (otdb-table-format-number-zero percent-carb (otdb-precision ctx 'percent-carb))
           (otdb-table-format-number-zero percent-protein (otdb-precision ctx 'percent-protein))
           (otdb-table-format-number-zero percent-fat (otdb-precision ctx 'percent-fat))
           (otdb-table-format-number-zero weight (otdb-precision ctx 'weight))
           (otdb-table-format-number-zero volume (otdb-precision ctx 'volume)))
          new-lisp-table)
    ;; TODO: fix this 13
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

(defun otdb-recipe-calc-special (ctx lisp-table current-temporary-buffer &optional quantity calculation-type)
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
      (let ((recipe-location (otdb-recipe-find ctx (otdb-column-value ctx 'item lisp-row))))
        (cond (recipe-location
               (unless (otdb-table-check-invalid-current-row-lisp lisp-row char-columns)
                 (with-current-file-transient-min (car recipe-location)
                   ;; TODO: open everything up?
                   (goto-char (cadr recipe-location))
                   (cic:org-find-table)
                   ;; advance to table
                   (otdb-recipe-calc-special (cic:org-table-to-lisp-no-separators)
                                             current-temporary-buffer
                                             (* quantity (otdb-table-number (otdb-column-value ctx 'quantity lisp-row)))
                                             calculation-type))))
              ((or
                (eq calculation-type 'all)
                (and (eq calculation-type 'check)
                     (otdb-table-check-current-row-lisp lisp-row char-columns "X"))
                (and (eq calculation-type 'cost)
                     (otdb-table-check-current-row-lisp lisp-row char-columns "C"))
                (and (eq calculation-type 'tag)
                     otdb-recipe-item-tags
                     (string-match-p otdb-recipe-item-tags    (nth otdb-recipe-check-column lisp-row)))
                (and (eq calculation-type 'pattern)
                     otdb-recipe-item-pattern
                     (string-match-p otdb-recipe-item-pattern (otdb-column-value ctx 'item lisp-row))))
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
    (let* ((package-weight   (nth (otdb-database-column ctx 'package-weight) therow))
           (package-volume   (nth (otdb-database-column ctx 'package-volume) therow))
           (package-cost     (nth (otdb-database-column ctx 'cost) therow))
           (serving-weight   (nth (otdb-database-column ctx 'serving-weight) therow))
           (serving-volume   (nth (otdb-database-column ctx 'serving-volume) therow))
           (serving-calories (nth (otdb-database-column ctx 'serving-calories) therow))
           (serving-protein  (nth (otdb-database-column ctx 'serving-protein) therow))
           (package-cost-number (otdb-table-number package-cost))
           (package-volume-number (otdb-table-number package-volume))
           (package-volume-unit (otdb-table-unit package-volume))
           (package-weight-number (otdb-table-number package-weight))
           (package-weight-unit (otdb-table-unit package-weight))
           (serving-calories-number (otdb-table-number serving-calories))
           (serving-protein-number (otdb-table-number serving-protein))
           (serving-volume-number (otdb-table-number serving-volume))
           (serving-volume-unit (otdb-table-unit serving-volume))
           (serving-weight-number (otdb-table-number serving-weight))
           (serving-weight-unit (otdb-table-unit serving-weight))
           ;; see comments in otdb-recipe-database-calorie-costs
           (calorie-cost (cic:number-to-string-nan (ignore-errors (cond ((and (cic:full-string-p package-weight) (cic:full-string-p serving-weight))
                                                                         (let ((factor (otdb-table-unit-conversion 'weight package-weight-unit serving-weight-unit)))
                                                                           (* (/ package-cost-number (* factor (/ package-weight-number serving-weight-number) serving-calories-number)) 1000)))
                                                                        ((and (cic:full-string-p package-volume) (cic:full-string-p serving-volume))
                                                                         (let ((factor (otdb-table-unit-conversion 'volume package-volume-unit serving-volume-unit)))
                                                                           (* (/ package-cost-number (* factor (/ package-volume-number serving-volume-number) serving-calories-number)) 1000)))
                                                                        (t
                                                                         nil)))))
           ;; see comments in otdb-recipe-database-protein-costs
           (protein-cost (cic:number-to-string-nan (ignore-errors (cond ((and (cic:full-string-p package-weight) (cic:full-string-p serving-weight))
                                                                         (let ((factor (otdb-table-unit-conversion 'weight package-weight-unit serving-weight-unit)))
                                                                           (* (/ package-cost-number (* factor (/ package-weight-number serving-weight-number) serving-protein-number)) 100)))
                                                                        ((and (cic:full-string-p package-volume) (cic:full-string-p serving-volume))
                                                                         (let ((factor (otdb-table-unit-conversion 'volume package-volume-unit serving-volume-unit)))
                                                                           (* (/ package-cost-number (* factor (/ package-volume-number serving-volume-number) serving-protein-number)) 100)))
                                                                        (t
                                                                         nil))))))
      ;; add to row
      (setcar (nthcdr 11 therow) calorie-cost)
      (setcar (nthcdr 12 therow) protein-cost)))
  lisp-table-no-seperators)

(provide 'otdb-recipe)
