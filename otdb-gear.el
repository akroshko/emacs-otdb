;;; otdb-gear.el --- For calculating the weights of backpacking (or other) gear.
;;
;; Copyright (C) 2015-2023, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <boreal6502@gmail.com>
;; Created: Sun Apr 5, 2015
;; Version: 20130801
;; URL: https://github.com/akroshko/emacs-otdb
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
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
;; Use this file with "emacs -q --load otdb-sample-init.el".  See
;; the included README.md file for more information on this package.
;;
;; Features that might be required by this library:
;;
;; Standard Emacs features, to be documented specifically later.  Also
;; using (require 'cic-emacs-common) is sufficient.
;;
;;; Code:

;; XXXX: I'm Canadian and like small weights in grams and large
;; weights in pounds.
(defvar otdb-gear-weight-units
  'lb-g
  "Use 'kg for kilograms, 'lb for pounds, and 'lb-g for lbs for
  larger weights and g for smaller weights..")
;; (setq otdb-gear-weight-units 'lb)
;; (setq otdb-gear-weight-units 'kg)

(defvar otdb-gear-item-pattern
  nil
  "Contains the regex pattern for filtering special calculations.")

(defvar otdb-gear-item-last-pattern
  nil
  "Contains the last regex pattern for filtering special
  calculations.")

(defvar otdb-gear-item-tags
  nil
  "Hold set of tags")

(defvar otdb-gear-item-last-tags
  nil
  "Hold the last set of tags")

(defun otdb-gear-reset-filters ()
  "Reset all specified filters."
  (interactive)
  (setq otdb-gear-item-last-pattern nil
        otdb-gear-item-pattern nil
        otdb-gear-item-last-tags nil
        otdb-gear-item-tags nil))

(defun otdb-gear-menu-tags ()
  "Create dynamic menus to reflect the current value of
otdb-gear-item-tags."
  (cons (cond (otdb-gear-item-tags
               (concat "Disable (otdb-gear-item-tags): "   (pp-to-string otdb-gear-item-tags)))
              (otdb-gear-item-last-tags
               (concat "Re-enable (otdb-gear-item-tags): " (pp-to-string otdb-gear-item-last-tags)))
              (t
               "Empty (otdb-gear-item-tags): "))
        (lambda ()
          (interactive)
          (if otdb-gear-item-tags
              (progn
                (setq otdb-gear-item-last-tags otdb-gear-item-tags
                      otdb-gear-item-tags      nil))
            (setq otdb-gear-item-tags otdb-gear-item-last-tags)))))

(defun otdb-gear-menu-item-pattern ()
  "Create dynamic menus to reflect the current value of
otdb-gear-item-pattern."
  (cons (cond (otdb-gear-item-pattern
               (concat "Disable (otdb-gear-item-pattern): "   (pp-to-string otdb-gear-item-pattern)))
              (otdb-gear-item-last-pattern
               (concat "Re-enable (otdb-gear-item-pattern): " (pp-to-string otdb-gear-item-last-pattern)))
              (t
               "Empty (otdb-gear-item-pattern): "))
        (lambda ()
          (interactive)
          (if otdb-gear-item-pattern
              (progn
                (setq otdb-gear-item-last-pattern otdb-gear-item-pattern
                      otdb-gear-item-pattern      nil))
            (setq otdb-gear-item-pattern otdb-gear-item-last-pattern)))))

(defun otdb-gear-menu-files (map)
  "Create dynamic menus pointing to the gear files and
databases."
  (define-key map [menu-bar otdb-gear-menu gear-collections]        (cons "Gear collection files" (make-sparse-keymap "gear collection files")))
  ;; TODO: does not update dynamically at the moment
  (when (boundp 'otdb-gear-collection-files)
    (dolist (collection (cic:ensure-list otdb-gear-collection-files))
      (define-key map (vector 'menu-bar 'otdb-gear-menu 'gear-collections (make-symbol collection)) (cons collection (cic:make-file-finder collection)))))
  (define-key map [menu-bar otdb-gear-menu gear-databases]          (cons "Gear database files" (make-sparse-keymap "gear database files")))
  (when (boundp 'otdb-gear-database)
    (dolist (database (cic:ensure-list otdb-gear-database))
      (define-key map (vector 'menu-bar 'otdb-gear-menu 'gear-databases (make-symbol database)) (cons database (cic:make-file-finder database))))))

;; (defun otdb-gear-mode-map ()
;;   "Function to make the menu map for otdb-gear-mode."
;;   (let ((map (make-sparse-keymap)))
;;     (setq map (otdb-table-skeleton-map map))
;;     (define-key map [menu-bar otdb-gear-menu]                              (cons "otdb-gear" (make-sparse-keymap "otdb-gear")))
;;     (define-key map [menu-bar otdb-gear-menu reset-filters]                '("Reset gear filters" . otdb-gear-reset-filters))
;;     ;; TODO: generate these from alist
;;     (otdb-gear-menu-files map)
;;     (define-key map [menu-bar otdb-gear-menu separator2] '("--"))
;;     ;; put these in reverse order they are displayed
;;     ;; TODO: menu to jump to database(s)
;;     (define-key map [menu-bar otdb-gear-menu item-patterns]                (cons "Gear item patterns" (make-sparse-keymap "gear item patterns")))
;;     ;; TODO: make these actually work
;;     (define-key map [menu-bar otdb-gear-menu item-patterns clothing]       (cons "Clothing"  'nil-command))
;;     (define-key map [menu-bar otdb-gear-menu item-patterns packaging]      (cons "Packaging" 'nil-command))
;;     (define-key map [menu-bar otdb-gear-menu item-pattern]                 (otdb-gear-menu-item-pattern))
;;     (define-key map [menu-bar otdb-gear-menu item-tags]                    (cons "Gear tags"   (make-sparse-keymap "gear tags patterns")))
;;     (define-key map [menu-bar otdb-gear-menu item-tags clothing]           (cons "Clothing"    (command-with-args 'set otdb-gear-item-tags "clothing")))
;;     (define-key map [menu-bar otdb-gear-menu item-tags consumable]         (cons "Consumable"  (command-with-args 'set otdb-gear-item-tags "consumable")))
;;     (define-key map [menu-bar otdb-gear-menu item-tags electronics]        (cons "Electronics" (command-with-args 'set otdb-gear-item-tags "electronics")))
;;     (define-key map [menu-bar otdb-gear-menu item-tags container]          (cons "Container"   (command-with-args 'set otdb-gear-item-tags "container")))
;;     (define-key map [menu-bar otdb-gear-menu item-tag]                     (otdb-gear-menu-tags))
;;     (define-key map [menu-bar otdb-gear-menu separator3] '("--"))
;;     (define-key map [menu-bar otdb-gear-menu toggle-check-invalid]         '("Toggle column X as invalid" . otdb-table-invalid-toggle-check-line))
;;     (define-key map [menu-bar otdb-gear-menu toggle-check-cost]            '("Toggle column X as \"C\""   . otdb-table-set-toggle-cost-line))
;;     (define-key map [menu-bar otdb-gear-menu toggle-check]                 '("Toggle column X"            . otdb-table-set-toggle-check-line))
;;     (define-key map [menu-bar otdb-gear-menu calc-special-command-pattern] '("Use special buffer for calculating specified pattern" . otdb-gear-calc-in-special-buffer-pattern))
;;     (define-key map [menu-bar otdb-gear-menu calc-special-command-tagged]  '("Use special buffer for calculating selected tags"     . otdb-gear-calc-in-special-buffer-tag))
;;     (define-key map [menu-bar otdb-gear-menu calc-special-command-cost]    '("Use special buffer for calculating cost"              . otdb-gear-calc-in-special-buffer-cost))
;;     (define-key map [menu-bar otdb-gear-menu calc-special-command-checked] '("Use special buffer for calculating checked"           . otdb-gear-calc-in-special-buffer-checked))
;;     (define-key map [menu-bar otdb-gear-menu calc-special-command-all]     '(menu-item "Use special buffer for calculating all"     otdb-gear-calc-in-special-buffer-all
;;                                                                                        :keys "s-d s"))
;;     map))
;; (setq otdb-gear-mode-map (otdb-gear-mode-map))

(defun otdb-menu-bar-update-hook--update-menu ()
  "Run from a hook to update the otdb-gear-mode menu."
  (when (derived-mode-p 'otdb-gear-mode)
    (define-key otdb-gear-mode-map [menu-bar otdb-gear-menu item-pattern] (otdb-gear-menu-item-pattern))
    (define-key otdb-gear-mode-map [menu-bar otdb-gear-menu item-tag]     (otdb-gear-menu-tags))))
(add-hook 'menu-bar-update-hook 'otdb-menu-bar-update-hook--update-menu)

;; (defvar otdb-gear-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (otdb-table-skeleton-map map))
;;   "The key map for otdb-gear-mode.")

(define-derived-mode otdb-gear-mode org-mode "org-table database gear mode"
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))


(defun otdb-gear-mode-hook--init ()
  (when (and (derived-mode-p 'otdb-gear-mode) (functionp 'hl-line-mode))
    (hl-line-mode 1)))
(add-hook 'otdb-gear-mode-hook 'otdb-gear-mode-hook--init)

(defun otdb-gear-lookup-function (row-list)
  "Helper function for otdb-table-update to lookup information
for ROW-LIST from a particular collection."
  (let (key-list
        database-row-alist
        weight-cost-list
        quantity-alist
        (collection-list (otdb-gear-get-collections))
        collection-weight-cost-list)
    (dolist (row (cdr row-list))
      (let ((current-item (elt row otdb-gear-item-column))
            (current-quantity (elt row otdb-gear-quantity-column)))
        (if (member (s-trim-full-no-properties current-item) collection-list)
            (let ((wcl (otdb-gear-get-collection-weight-cost current-item (string-to-number current-quantity))))
              (push (list (s-trim-full-no-properties current-item) (car wcl) (cadr wcl)) collection-weight-cost-list))
          (progn
            (push (list (s-trim-full-no-properties current-item)
                        (s-trim-full-no-properties current-quantity))
                  quantity-alist)
            (push (s-trim-full-no-properties current-item)
                  key-list)))))
    (setq database-row-alist (otdb-table-item-row-multiple otdb-gear-database otdb-gear-database-headline key-list 1))
    ;; calculate cost and weight
    (dolist (row-alist database-row-alist)
      (let ((row (cadr row-alist))
            (quantity (cadr (assoc (car row-alist) quantity-alist))))
        (let ((weight (ignore-errors
                        (otdb-gear-get-weight-database-row row quantity)))
              (cost (ignore-errors
                      (otdb-gear-get-cost-database-row row quantity)))
              ;; TODO: unhardcode this
              (tags (ignore-errors (elt row 3))))
          (push (list (car row-alist)
                      weight
                      cost
                      tags)
                weight-cost-list))))
    (append collection-weight-cost-list weight-cost-list)))

(defconst otdb-gear-quantity-column
  0
  "The table column that contains the quantity.")

(defconst otdb-gear-item-column
  1
  "The table column that contains the item.")

(defconst otdb-gear-weight-column
  3
  "The table column that contains weight.")

(defconst otdb-gear-cost-column
  4
  "The table column that contains cost")

(defconst otdb-gear-tags-column
  5
  "The table column that contains tags.")

(defconst otdb-gear-database-weight-column
  1
  "The table column in the database that contains the item weight.")

(defconst otdb-gear-database-cost-column
  2
  "The table column in the database that contains the item cost.")

(defconst otdb-gear-database-tags-column
  3
  "The table column in the database that contains the item tags.")

(defun otdb-gear-insert-function (collection-filename collection-heading weight-cost-list)
  "Helper function for otdb-table-update to insert information
into a recipe.  The recipe is COLLECTION-HEADING in
COLLECTION-FILENAME with information to be inserted of
WEIGHT-COST-LIST."
  (let (new-item
        new-weight
        new-cost
        new-tags
        (count 1))
    ;; TODO: this could be generalized a bit better
    ;;       headings are good, but so are standards
    (do-org-table-rows collection-filename collection-heading row
                       ;; XXXX: for some reason do-org-table-rows pops a null row at the end
                       (when row
                         (setq new-item (s-trim-full-no-properties (elt row otdb-gear-item-column)))
                         (unless (equal count 1)
                           (setq new-weight (elt (assoc new-item weight-cost-list) 1)
                                 new-cost   (elt (assoc new-item weight-cost-list) 2)
                                 new-tags   (elt (assoc new-item weight-cost-list) 3))
                           (if (not new-weight)
                               (org-table-put count (+ otdb-gear-weight-column 1) "")
                             (org-table-put count (+ otdb-gear-weight-column 1) (otdb-gear-weight-string new-weight)))
                           (if (not new-cost)
                               (org-table-put count (+ otdb-gear-cost-column 1) "")
                             (org-table-put count (+ otdb-gear-cost-column 1) (otdb-gear-cost-string new-cost)))
                           (if (not new-tags)
                               (org-table-put count (+ otdb-gear-tags-column 1) "")
                             (org-table-put count (+ otdb-gear-tags-column 1) new-tags)))
                         (setq count (1+ count))))
    (tblel-eval)))

(defun otdb-gear-weight-string (weight)
  "Convert the WEIGHT into a proper string."
  (if (numberp weight)
    (cond ((eq otdb-gear-weight-units 'kg)
           (format "%.4f kg" (float weight)))
          ((eq otdb-gear-weight-units 'lb)
           (format "%.4f lb" (float weight)))
          ((eq otdb-gear-weight-units 'lb-g)
           (if (>= (* (float weight) (otdb-table-unit-conversion 'weight "g" "lb")) 2.0)
               (format "%.4flb" (* (otdb-table-unit-conversion 'weight "g" "lb") (float weight)))
             (format "%.1fg" (float weight))))
          (t
           error "Unit not properly defined."))
    ""))

(defun otdb-gear-cost-string (cost)
  "Convert the COST into a proper string."
  (if (numberp cost)
      (format "$%.3f" (float cost))
    ""))

(defun otdb-gear-get-weight (row)
  "Get the weight of key in the ROW."
  (let* ((current-item (elt row otdb-gear-item-column))
         (current-quantity (elt row otdb-gear-quantity-column))
         (collection-list (otdb-gear-get-collections)))
    (if (member (s-trim-full current-item) collection-list)
        (/ (car (otdb-gear-get-collection-weight-cost current-item (string-to-number current-quantity))) (otdb-table-number current-quantity))
      (otdb-gear-get-weight-database current-item current-quantity))))

(defun otdb-gear-get-weight-database (item quantity)
  "Get the weight of QUANTITY of the ITEM from the database."
  (let* ((database-row (otdb-gear-database-row item)))
    (otdb-gear-get-weight-database-row database-row quantity)))

(defun otdb-gear-get-weight-database-row (database-row quantity)
  "Get the weight of QUANTITY from database row DATABASE-ROW."
  (let ((item-weight (elt database-row otdb-gear-database-weight-column)))
   (* (otdb-table-number quantity)
      (otdb-table-number item-weight)
      (cond ((eq otdb-gear-weight-units 'kg)
             (otdb-table-unit-conversion 'weight (otdb-table-unit item-weight) "kg"))
            ((eq otdb-gear-weight-units 'lb)
             (otdb-table-unit-conversion 'weight (otdb-table-unit item-weight) "lb"))
            ((eq otdb-gear-weight-units 'lb-g)
             (otdb-table-unit-conversion 'weight (otdb-table-unit item-weight) "g"))))))

(defun otdb-gear-get-cost (row)
  "Get the cost of the item.  No idea if this is actually a valid thing."
  (let* ((collection-list (otdb-gear-get-collections)))
    (if (member (s-trim-full (elt row otdb-gear-item-column)) collection-list)
        (/ (cadr (otdb-gear-get-collection-weight-cost (elt row otdb-gear-item-column) (string-to-number (elt row otdb-gear-quantity-column)))) (otdb-table-number (elt row otdb-gear-quantity-column)))
      (otdb-gear-get-cost-database (elt row otdb-gear-item-column) (elt row otdb-gear-quantity-column)))))

(defun otdb-gear-get-cost-database (item quantity)
  "Get the cost of QUANITTY of ITEM from the database."
  (let* ((database-row (otdb-gear-database-row item)))
    (* (otdb-table-number quantity) (otdb-table-number (elt database-row otdb-gear-database-cost-column)))))

(defun otdb-gear-get-cost-database-row (database-row quantity)
  "Get the cost of the QUANTITY of the item from the database row
DATABASE-ROW."
  (* (otdb-table-number quantity) (otdb-table-number (elt database-row otdb-gear-database-cost-column))))

(defun otdb-gear-get-collection-weight-cost (collection quantity)
  "Get the weight and cost from gear collection COLLECTION."
  (let ((collection-location (otdb-gear-find-collection collection)))
    (with-current-file-transient (car collection-location)
      (goto-char (cadr collection-location))
      (cic:org-find-table)
      (cic:org-table-last-row)
      (list
       (*
        quantity
        (string-to-number (org-table-get nil (+ otdb-gear-weight-column 1)))
        (cond ((eq otdb-gear-weight-units 'kg)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil (+ otdb-gear-weight-column 1))) "kg"))
              ((eq otdb-gear-weight-units 'lb)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil (+ otdb-gear-weight-column 1))) "lb"))
              ((eq otdb-gear-weight-units 'lb-g)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil (+ otdb-gear-weight-column 1))) "g"))))
       (* quantity (string-to-number (replace-regexp-in-string "\\$" "" (org-table-get nil (+ otdb-gear-cost-column 1)))))))))

(defun otdb-gear-find-item (item)
  "Find the location of the ITEM."
  (cic:org-table-lookup-location otdb-gear-database otdb-gear-database-headline item 1))

(defun otdb-gear-get-items ()
  "Get list of all gear items from the database."
  ;; TODO: where is this used???
  (let (items)
    (dolist (database otdb-gear-database)
      (setq items (append items (cic:org-table-get-keys database otdb-gear-database-headline))))
    items))

(defun otdb-gear-find-collection (collection)
  "Get the location of gear collection COLLECTION."
  (let (location)
    (dolist (collection-file otdb-gear-collection-files)
      (with-current-file-transient-min collection-file
        (let ((found (when (re-search-forward (concat "^\* " collection " :gear:") nil t)
                       (line-beginning-position))))
          (when found
            (setq location (list collection-file found))))))
    location))

(defun otdb-gear-get-collections ()
  "Get list of all gear collections."
  (if otdb-table-collections-cache
      otdb-table-collections-cache
    (let (table
          table-name
          collection-list)
      (dolist (gear-file otdb-gear-collection-files)
        (do-org-tables gear-file table-name table
                       (save-match-data
                         (when (string-match "\\(.*\\) :gear:" table-name)
                           (push (match-string 1 table-name) collection-list)))))
      (setq otdb-table-collections-cache collection-list)
      collection-list)))

(defun otdb-gear-database-row (item)
  "Get the database row corresponding to gear item ITEM."
  (cic:org-table-lookup-row otdb-gear-database otdb-gear-database-headline item))

;; (otdb-gear-calc-gear (org-table-to-lisp) (cic:org-table-to-lisp-no-separators))
(defun otdb-gear-calc-gear (lisp-table lisp-table-no-seperators)
  "Calculated an updated lisp table from the LISP-TABLE-NO-SEPERATORS
corresponding to a gear collection."
  (let ((weight 0)
        (cost 0)
        (cummulative-weight 0.0)
        cummulative-weight-list
        cummulative-ignore
        (last-cummulative t)
        (new-lisp-table (list (elt lisp-table 1)))
        ;; (new-lisp-table (butlast lisp-table-no-seperators))
        (last-row (car (last lisp-table-no-seperators)))
        (char-columns (otdb-table-parse-char-columns lisp-table-no-seperators))
        new-last-row)
    (dolist (lisp-row (cddr (butlast lisp-table 2)))
      (cond ((eq lisp-row 'hline)
             (setq cummulative-weight-list (append (butlast cummulative-weight-list) (list cummulative-weight))
                   cummulative-weight      0.0)
             t)
            (t
             ;; TODO: no marks here for now, maybe only do "X" mark
             ;; XXXX: do not need to include quantities because they are calculated on insertion
             (unless (otdb-table-check-invalid-current-row-lisp lisp-row char-columns)
               (cond ((or (eq otdb-gear-weight-units 'lb)
                          (eq otdb-gear-weight-units 'kg))
                      ;; TODO: add here too....
                      (let* ((current-weight (otdb-table-lisp-row-float lisp-row otdb-gear-weight-column))
                             (current-weight-converted (* current-weight
                                                          (otdb-table-unit-conversion 'weight (otdb-table-unit (elt lisp-row (- otdb-gear-weight-column 1))) "g"))))
                        (setq weight                  (+ weight current-weight)
                              cummulative-weight      (+ current-weight-converted cummulative-weight)
                              cummulative-weight-list (append cummulative-weight-list (cons nil nil)))))
                     ((eq otdb-gear-weight-units 'lb-g)
                      ;; otherwise make sure weight is in grams
                      (let ((current-weight (* (otdb-table-lisp-row-float lisp-row otdb-gear-weight-column)
                                               (otdb-table-unit-conversion 'weight (otdb-table-unit (elt lisp-row otdb-gear-weight-column)) "g"))))
                        (setq weight                  (+ weight current-weight)
                              cummulative-weight      (+ current-weight cummulative-weight)
                              cummulative-weight-list (append cummulative-weight-list (cons nil nil))))))
               (setq cost (+ cost (otdb-table-lisp-row-float lisp-row otdb-gear-cost-column)))))))
    (pop cummulative-weight-list)
    (when (<= (length (remove-if 'null cummulative-weight-list)) 1)
      (setq cummulative-ignore t))
    (dolist (lisp-row (cddr (butlast lisp-table 2)))
      ;; TODO:   if there are adjacent, then ignore, in loop though
      (cond ((eq lisp-row 'hline)
             t)
            (t
             (let ((the-cummulative (pop cummulative-weight-list)))
               ;; TODO: clear out anything already in brackets
               (let ((item-weight (elt lisp-row otdb-gear-weight-column)))
                 (push (nconc
                        (cl-subseq lisp-row 0 3)
                        (list (cond ((and (not cummulative-ignore) (not last-cummulative) the-cummulative)
                                     (setq last-cummulative t)
                                     (concat
                                      (s-trim-full (replace-regexp-in-string "(.*)" "" item-weight))
                                      " ("
                                      (otdb-gear-weight-string the-cummulative)
                                      ")"))
                                    ((and (not cummulative-ignore) last-cummulative the-cummulative)
                                     (setq last-cummulative t)
                                     (s-trim-full (replace-regexp-in-string "(.*)" "" item-weight)))
                                    (t
                                     (setq last-cummulative nil)
                                     (s-trim-full (replace-regexp-in-string "(.*)" "" item-weight)))))
                        (cl-subseq lisp-row 4 7))
                       new-lisp-table))))))
    ;; TODO: go through and make new version of table, but with cummulative...
    ;; insert into last row
    (setq new-last-row (nconc
                        (list
                         (elt last-row otdb-gear-quantity-column)
                         (elt last-row otdb-gear-item-column)
                         ""
                         (otdb-gear-weight-string weight)
                         (otdb-gear-cost-string cost))
                        ;; TODO: not great, really want only do for single character headers
                        (mapcar (lambda (e) "") (nthcdr otdb-gear-tags-column last-row))))
    ;; TODO: add in text indicating char-column if necessary
    (push new-last-row new-lisp-table)
    (setq new-lisp-table (nreverse new-lisp-table))))

(defun otdb-gear-calc-in-special-buffer-all ()
  "Recursively calculate all items from current table in special
temporary buffer."
  (interactive)
  (otdb-gear-calc-in-special-buffer 'all))

(defun otdb-gear-calc-in-special-buffer-check ()
  "Recursively calculate checked items from current table in
special temporary buffer."
  (interactive)
  (otdb-gear-calc-in-special-buffer 'check))

(defun otdb-gear-calc-in-special-buffer-cost ()
  "Recursively calculate items marked cost from current table in
special temporary buffer."
  (interactive)
  (otdb-gear-calc-in-special-buffer 'cost))

(defun otdb-gear-calc-in-special-buffer-tag ()
  "Recursively calculate items with specified tags from current
table in special temporary buffer."
  (interactive)
  (otdb-gear-calc-in-special-buffer 'tag))

(defun otdb-gear-calc-in-special-buffer-pattern ()
  "Recursively calculate items matching specified patterns from
current table in special temporary buffer."
  (interactive)
  (otdb-gear-calc-in-special-buffer 'pattern))

(defun otdb-gear-calc-in-special-buffer (calculation-type)
  "Command to recursively recalculate current table in a special
temporary buffer and filter by CALCULATION-TYPE."
  (let ((the-new-buffer (let (the-new-buffer-2)
                          (when (eq (otdb-table-detect) 'backpacking)
                            (cond ((eq calculation-type 'pattern)
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-gear-pattern--" otdb-gear-item-pattern "--" (cic:datestamp-current-time) "*"))))
                                  ((eq calculation-type 'tag)
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-gear-tags--" otdb-gear-item-tags "--" (cic:datestamp-current-time) "*"))))
                                  (t
                                   (setq the-new-buffer-2 (generate-new-buffer (concat "*otdb-gear--" (cic:datestamp-current-time) "*")))))
                            (with-current-buffer the-new-buffer-2
                              (org-mode)
                              (otdb-gear-mode)
                              (insert (concat
                                       "  |----------+------+---+--------+------+------+------|\n"
                                       "  | Quantity | Item | X | Weight | Cost | Tags | Note |\n"
                                       "  |----------+------+---+--------+------+------+------|\n")))
                            the-new-buffer-2))))
    (otdb-gear-calc-special (cic:org-table-to-lisp-no-separators) the-new-buffer calculation-type)
    (with-current-buffer the-new-buffer
      (insert (concat
               "  |----------+------+--------+------+------+------+---+\n"
               "  | 1        |      |        |      |      |      |   |\n"
               "  |----------+------+--------+------+------+------+---+\n"
               "  #+TBLEL: otdb-gear-calc-gear\n"))
      (forward-line -2)
      (org-table-align)
      (beginning-of-line)
      (tblel-eval)
      (goto-char (point-min)))
    (switch-to-buffer the-new-buffer)))

(defun otdb-gear-calc-special (lisp-table current-temporary-buffer calculation-type)
  "Recursively calculate the LISP-TABLE in
CURRENT-TEMPORARY-BUFFER and filter by CALCULATION-TYPE."
  (let (current-collection-name
        (char-columns (otdb-table-parse-char-columns lisp-table)))
    ;; loop over the rows find all the current rows
    (dolist (lisp-row (butlast (cdr lisp-table)))
      ;; find buffer location of the referenced collection in LISP-TABLE
      ;; a nil if things are not good
      ;; TODO: raise error if totally invalid
      (let ((collection-location (otdb-gear-find-collection (elt lisp-row otdb-gear-item-column))))
        ;; if current row is a collection-location, everything else falls through
        (cond (collection-location
               ;; TODO: figure this out, make sure I'm really only checking for invalid, ???
               (unless (otdb-table-check-invalid-current-row-lisp lisp-row char-columns)
                 ;; find the collection
                 (with-current-file-transient-min (car collection-location)
                   ;; TODO: open everything up?
                   (goto-char (cadr collection-location))
                   (cic:org-find-table)
                   ;; advance to table
                   (otdb-gear-calc-special (cic:org-table-to-lisp-no-separators) current-temporary-buffer calculation-type))))
              ((or
                (eq calculation-type 'all)
                (and (eq calculation-type 'check)   (otdb-table-check-current-row-lisp lisp-row char-columns "X"))
                (and (eq calculation-type 'cost)    (otdb-table-check-current-row-lisp lisp-row char-columns "C"))
                (and (eq calculation-type 'tag)     (and otdb-gear-item-tags (otdb-table-tag-pattern-match otdb-gear-item-tags (elt lisp-row otdb-gear-tags-column)))  )
                (and (eq calculation-type 'pattern) (and otdb-gear-item-pattern (string-match-p otdb-gear-item-pattern (elt lisp-row otdb-gear-item-column)))))
               (with-current-buffer current-temporary-buffer
                 (insert (concat " | " (mapconcat 'identity lisp-row " | ") "\n")))))))))

(provide 'otdb-gear)
