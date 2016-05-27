;;; otdb-gear.el --- For calculating the weights of backpacking (or other) gear.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20160525
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
;; Use this file with "emacs -q --load otdb-sample-init.el".  See
;; the included README.md file for more information on this package.
;;
;; Features that might be required by this library:
;;
;; Standard Emacs features, to be documented specifically later.  Also
;; requires features from https://github.com/akroshko/emacs-stdlib,
;; using (require 'emacs-stdlib) is sufficient.
;;
;;; Code:

;; TODO: document better on next round

(defconst otdb-gear-types
  '("gear"))

;; XXXX: I'm Canadian and like small weights in grams and large
;; weights in pounds.
;; (setq otdb-gear-weight-units 'lb)
;; (setq otdb-gear-weight-units 'kg)
;; (setq otdb-gear-weight-units 'lb-g)
(defvar otdb-gear-weight-units
  'lb-g
  "Use 'kg for kilograms, 'lb for pounds, and 'lb-g for lbs for
  larger weights and g for smaller weights..")

(defvar otdb-gear-mode-map
  nil
  "Keymap for otdb-gear.")

;; XXXX: if above global flag is set, only calculate non-nil and non-invalid for select columns
;; TODO: need key and/or menu item to toggle this variable
(defvar otdb-gear-column-mark
  nil
  "Set to string \"X\" for check and string \"C\" for cost.")

(defvar otdb-gear-item-pattern
  nil
  ;; TODO: need a better interface
  "Set to pattern needed for filtering results.")

(defvar otdb-gear-item-last-pattern
  nil
  ;; TODO: need a better interface
  "Holds the last pattern needed for filtering results.")

(defvar otdb-gear-item-tags
  nil
  "Hold set of tags")

(defvar otdb-gear-item-last-tags
  nil
  "Hold the last set of tags")

(defun otdb-gear-menu-item-pattern ()
  "Set menu item to reflect current value of otdb-gear-item-pattern."
  (cons (cond (otdb-gear-item-pattern
               (concat "Disable (otdb-gear-item-pattern): " (pp-to-string otdb-gear-item-pattern)))
              (otdb-gear-item-last-pattern
               (concat "Re-enable (otdb-gear-item-pattern): " (pp-to-string otdb-gear-item-last-pattern)))
              (t
               "Empty (otdb-gear-item-pattern): "))
        (lambda ()
          (interactive)
          (if otdb-gear-item-pattern
              (progn
                (setq otdb-gear-item-last-pattern otdb-gear-item-pattern)
                (setq otdb-gear-item-pattern nil))
            (progn
              (setq otdb-gear-item-pattern otdb-gear-item-last-pattern))))))

(defun otdb-gear-menu-tags ()
  "Set menu item to reflect current value of otdb-gear-item-tags."
  (cons (cond (otdb-gear-item-tags
               (concat "Disable (otdb-gear-item-tags): " (pp-to-string otdb-gear-item-tags)))
              (otdb-gear-item-last-tags
               (concat "Re-enable (otdb-gear-item-tags): " (pp-to-string otdb-gear-item-last-tags)))
              (t
               "Empty (otdb-gear-item-tags): "))
        (lambda ()
          (interactive)
          (if otdb-gear-item-tags
              (progn
                (setq otdb-gear-item-last-tags otdb-gear-item-tags)
                (setq otdb-gear-item-tags nil))
            (progn
              (setq otdb-gear-item-tags otdb-gear-item-last-tags))))))

(defun otdb-gear-reset-filters ()
  (interactive)
  ;; XXXX: resetting clears all
  (setq otdb-gear-item-last-pattern nil)
  (setq otdb-gear-item-pattern nil)
  (setq otdb-gear-item-last-tags nil)
  (setq otdb-gear-item-tags nil)
  (setq otdb-gear-column-mark nil))

(defvar otdb-gear-read-column-mark-history
  nil
  "The history of column mark inputs.")

(defun otdb-gear-read-column-mark ()
  "Read and change the column mark."
  (interactive)
  (let ((thestring (read-string (concat "Column mark expression " (pp-to-string otdb-gear-column-mark) ": ") nil otdb-gear-read-column-mark-history otdb-gear-column-mark)))
    (if (cic:is-not-empty-string-nil thestring)
        (setq otdb-gear-column-mark thestring)
      nil)))

(defun otdb-gear-menu-column-mark ()
  (cons (concat "Change column mark: " (pp-to-string otdb-gear-column-mark)) 'otdb-gear-read-column-mark))

(defun otdb-gear-menu-files (map)
  (define-key map [menu-bar otdb-gear-menu gear-collections]        (cons "Gear collection files" (make-sparse-keymap "gear collection files")))
  ;; TODO: does not update dynamically at the moment
  (dolist (collection (cic:ensure-list otdb-gear-collection-files))
    (define-key map (vector 'menu-bar 'otdb-gear-menu 'gear-collections collection) (cons collection (cic:make-file-finder collection))))
  (define-key map [menu-bar otdb-gear-menu gear-databases]          (cons "Gear database files" (make-sparse-keymap "gear database files")))
  (dolist (database (cic:ensure-list otdb-gear-database))
    (define-key map (vector 'menu-bar 'otdb-gear-menu 'gear-databases database) (cons database (cic:make-file-finder database)))))

(defun otdb-gear-mode-map ()
  (let ((map (make-sparse-keymap)))
    (otdb-table-skeleton-map map)
    (define-key map [menu-bar otdb-gear-menu]                         (cons "otdb-gear" (make-sparse-keymap "otdb-gear")))
    (define-key map [menu-bar otdb-gear-menu reset-filters]           (cons "Reset gear filters" 'otdb-gear-reset-filters))
    ;; TODO: generate these from alist
    (otdb-gear-menu-files map)
    (define-key map [menu-bar otdb-gear-menu separator2] '("--"))
    (define-key map [menu-bar otdb-gear-menu item-tags]               (cons "Gear tags" (make-sparse-keymap "gear tags patterns")))
    (define-key map [menu-bar otdb-gear-menu item-tags clothing]      (cons "Clothing"  (lambda () (interactive)
                                                                                          (setq otdb-gear-item-tags "clothing"))))
    (define-key map [menu-bar otdb-gear-menu item-tags consumable]    (cons "Consumable" (lambda () (interactive)
                                                                                           (setq otdb-gear-item-tags "consumable"))))
    (define-key map [menu-bar otdb-gear-menu item-tags container]     (cons "Container" (lambda () (interactive)
                                                                                          (setq otdb-gear-item-tags "container"))))
    ;; put these in reverse order they are displayed
    ;; TODO: menu to jump to database(s)
    (define-key map [menu-bar otdb-gear-menu item-patterns]           (cons "Gear item patterns" (make-sparse-keymap "gear item patterns")))
    ;; TODO: make these actually work
    (define-key map [menu-bar otdb-gear-menu item-patterns clothing]  (cons "Clothing" (lambda () (interactive)
                                                                                         nil)))
    (define-key map [menu-bar otdb-gear-menu item-patterns packaging] (cons "Packaging" (lambda () (interactive)
                                                                                          nil)))
    (define-key map [menu-bar otdb-gear-menu item-tag]                (otdb-gear-menu-tags))
    (define-key map [menu-bar otdb-gear-menu item-pattern]            (otdb-gear-menu-item-pattern))
    (define-key map [menu-bar otdb-gear-menu calc-special-command]    '(menu-item "Use special buffer for calculation" (lambda () (interactive) (otdb-gear-calc-special-command))
                                                                                  :keys "s-d s"))
    (define-key map [menu-bar otdb-gear-menu separator3] '("--"))
    (define-key map [menu-bar otdb-gear-menu column-mark-cost]        '(menu-item "Toggle column mark cost (C)" (lambda () (interactive) (setq otdb-gear-column-mark "C"))
                                                                             :button (:toggle . (equal otdb-gear-column-mark "C"))))
    (define-key map [menu-bar otdb-gear-menu column-mark-not-check]   '(menu-item "Toggle column mark check (not X)" (lambda () (interactive) (setq otdb-gear-column-mark "(not X)"))
                                                                             :button (:toggle . (equal otdb-gear-column-mark "(not X)"))))
    (define-key map [menu-bar otdb-gear-menu column-mark-check]       '(menu-item "Toggle column mark check (X)" (lambda () (interactive) (setq otdb-gear-column-mark "X"))
                                                                             :button (:toggle . (equal otdb-gear-column-mark "X"))))
    (define-key map [menu-bar otdb-gear-menu column-mark-nil]         '(menu-item "Toggle column mark empty" (lambda () (interactive) (setq otdb-gear-column-mark nil))
                                                                             :button (:toggle . (equal otdb-gear-column-mark nil))))
    (define-key map [menu-bar otdb-gear-menu column-mark]             (otdb-gear-menu-column-mark))
    (define-key map [menu-bar otdb-gear-menu toggle-check-invalid]    '("Toggle (X) invalid" . otdb-table-invalid-toggle-check-line))
    (define-key map [menu-bar otdb-gear-menu toggle-check]            '("Toggle (X)" . otdb-table-set-toggle-check-line))
    (otdb-table-skeleton-menu-map map 'otdb-gear-menu)
    map))
(setq otdb-gear-mode-map (otdb-gear-mode-map))

(defun otdb-gear-update-menu ()
  ;; TODO: check mode first
  (define-key otdb-gear-mode-map [menu-bar otdb-gear-menu column-mark]  (otdb-gear-menu-column-mark))
  (define-key otdb-gear-mode-map [menu-bar otdb-gear-menu item-pattern] (otdb-gear-menu-item-pattern))
  (define-key otdb-gear-mode-map [menu-bar otdb-gear-menu item-tag]     (otdb-gear-menu-tags)))

(add-hook 'menu-bar-update-hook 'otdb-gear-update-menu)

(define-minor-mode otdb-gear-mode
  :global nil
  :lighter " otdb-gear"
  :keymap otdb-gear-mode-map
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))

;; (add-hook 'otdb-gear-mode-hook 'otdb-gear-mode-init)
;; (defun otdb-gear-mode-init ()
;;   (when (functionp 'hl-line-mode)
;;     (hl-line-mode 1)))

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
      (if (member (strip-full-no-properties (elt row 1)) collection-list)
          (progn
            (let ((wcl (otdb-gear-get-collection-weight-cost (elt row 1))))
              (setq collection-weight-cost-list (cons (list (strip-full-no-properties (elt row 1)) (car wcl) (cadr wcl)) collection-weight-cost-list))))
        (progn
          (setq quantity-alist (cons (list (strip-full-no-properties (elt row 1))
                                           (strip-full-no-properties (elt row 0)))
                                     quantity-alist))
          (setq key-list (cons (strip-full-no-properties (elt row 1)) key-list)))))
    (setq database-row-alist (otdb-table-item-row-multiple otdb-gear-database otdb-gear-database-headline key-list 1))
    ;; calculate cost and weight
    (dolist (row-alist database-row-alist)
      (let ((row (cadr row-alist))
            (quantity (cadr (assoc (car row-alist) quantity-alist))))
        (let ((weight (ignore-errors
                        (otdb-gear-get-weight-database-row row quantity)))
              (cost (ignore-errors
                      (otdb-gear-get-cost-database-row row quantity)))
              (tags (ignore-errors (elt row 3))))
          (setq weight-cost-list (cons (list (car row-alist)
                                             weight
                                             cost
                                             tags)
                                       weight-cost-list)))))
    (append collection-weight-cost-list weight-cost-list)))

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
                       (setq new-item (strip-full-no-properties (elt row 1)))
                       (when (not (equal count 1))
                         (setq new-weight (elt (assoc new-item weight-cost-list) 1))
                         (setq new-cost (elt (assoc new-item weight-cost-list) 2))
                         (setq new-tags (elt (assoc new-item weight-cost-list) 3))
                         (if (not new-weight)
                             (org-table-put count 3 "")
                           (org-table-put count 3 (otdb-gear-weight-string new-weight)))
                         (if (not new-cost)
                             (org-table-put count 4 "")
                           (org-table-put count 4 (otdb-gear-cost-string new-cost)))
                         (if (not new-tags)
                             (org-table-put count 5 "")
                           (org-table-put count 5 new-tags)))
                       (setq count (1+ count)))
    (cic:org-table-eval-tblel)))

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
  (let* ((collection-list (otdb-gear-get-collections)))
    (if (member (strip-full (elt row 1)) collection-list)
        (/ (car (otdb-gear-get-collection-weight-cost (elt row 1))) (otdb-table-number (elt row 0)))
      (otdb-gear-get-weight-database (elt row 1) (elt row 0)))))

(defun otdb-gear-get-weight-database (item quantity)
  "Get the weight of QUANTITY of the ITEM from the database."
  (let* ((database-row (otdb-gear-database-row item)))
    (otdb-gear-get-weight-database-row database-row quantity)))

(defun otdb-gear-get-weight-database-row (database-row quantity)
  "Get the weight of QUANTITY from database row DATABASE-ROW."
  (* (otdb-table-number quantity)
     (otdb-table-number (elt database-row 1))
     (cond ((eq otdb-gear-weight-units 'kg)
            (otdb-table-unit-conversion 'weight (otdb-table-unit (elt database-row 1)) "kg"))
           ((eq otdb-gear-weight-units 'lb)
            (otdb-table-unit-conversion 'weight (otdb-table-unit (elt database-row 1)) "lb"))
           ((eq otdb-gear-weight-units 'lb-g)
            (otdb-table-unit-conversion 'weight (otdb-table-unit (elt database-row 1)) "g")))))

(defun otdb-gear-get-cost (row)
  "Get the cost of the item.  No idea if this is actually a valid thing."
  (let* ((collection-list (otdb-gear-get-collections)))
    (if (member (strip-full (elt row 1)) collection-list)
        (/ (cadr (otdb-gear-get-collection-weight-cost (elt row 1))) (otdb-table-number (elt row 0)))
      (otdb-gear-get-cost-database (elt row 1) (elt row 0)))))

(defun otdb-gear-get-cost-database (item quantity)
  "Get the cost of QUANITTY of ITEM from the database."
  (let* ((database-row (otdb-gear-database-row item)))
    (* (otdb-table-number quantity) (otdb-table-number (elt database-row 2)))))

(defun otdb-gear-get-cost-database-row (database-row quantity)
  "Get the cost of the QUANTITY of the item from the database row
DATABASE-ROW."
  (* (otdb-table-number quantity) (otdb-table-number (elt database-row 2))))

(defun otdb-gear-get-collection-weight-cost (collection)
  "Get the weight and cost from gear collection COLLECTION."
  (let ((collection-location (otdb-gear-find-collection collection)))
    (with-current-file (car collection-location)
      (goto-char (cadr collection-location))
      (cic:org-find-table)
      (cic:org-table-last-row)
      (list
       (*
        (string-to-number (org-table-get nil 3))
        (cond ((eq otdb-gear-weight-units 'kg)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil 3)) "kg"))
              ((eq otdb-gear-weight-units 'lb)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil 3)) "lb"))
              ((eq otdb-gear-weight-units 'lb-g)
               (otdb-table-unit-conversion 'weight (otdb-table-unit (org-table-get nil 3)) "g"))))
       (string-to-number (replace-regexp-in-string "\\$" "" (org-table-get nil 4)))))))

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
      (with-current-file-min collection-file
        (let ((found (progn
                       (when (re-search-forward (concat "^\* " collection " :gear:") nil t)
                         (beginning-of-line)
                         (point)))))
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
                       (when (string-match "\\(.*\\) :gear:" table-name)
                         (setq collection-list (cons (match-string 1 table-name) collection-list)))))
      (setq otdb-table-collections-cache collection-list)
      collection-list)))

(defun otdb-gear-database-row (item)
  "Get the database row corresponding to gear item ITEM."
  (cic:org-table-lookup-row otdb-gear-database otdb-gear-database-headline item))

(defun otdb-gear-calc-gear (lisp-table)
  "Calculated an updated lisp table from the LISP-TABLE
corresponding to a gear collection."
  (let ((weight 0)
        (cost 0)
        (new-lisp-table (butlast lisp-table))
        (last-row (car (last lisp-table)))
        ;; (char-column (otdb-table-lisp-char-find-column lisp-table otdb-gear-column-mark))
        (char-columns (otdb-table-parse-char-columns lisp-table))
        new-last-row)
    (dolist (lisp-row (butlast (cdr lisp-table)))
      ;; TODO: no marks here for now, maybe only do "X" mark
      ;; (and otdb-gear-column-mark (not (otdb-table-check-current-row-lisp lisp-row otdb-gear-column-mark char-columns)))
      (unless (otdb-table-check-invalid-current-row-lisp lisp-row otdb-gear-column-mark char-columns)
        (cond ((or (eq otdb-gear-weight-units 'lb)
                   (eq otdb-gear-weight-units 'kg))
               (setq weight (+ weight (* (string-to-number (elt lisp-row 0))  (otdb-table-lisp-row-float lisp-row 2)))))
              ((eq otdb-gear-weight-units 'lb-g)
               ;; otherwise make sure weight is in grams
               (setq weight (+ weight (* (string-to-number (elt lisp-row 0))
                                         (* (otdb-table-lisp-row-float lisp-row 2)
                                            (otdb-table-unit-conversion 'weight (otdb-table-unit (elt lisp-row 2)) "g")))))))
        (setq cost (+ cost (* (string-to-number (elt lisp-row 0)) (otdb-table-lisp-row-float lisp-row 3))))))
    ;; insert into last row
    ;; TODO: make uniform with ???
    (setq new-last-row (list
                        (nconc
                         (list
                          (elt last-row 0)
                          (elt last-row 1)
                          (otdb-gear-weight-string weight)
                          (otdb-gear-cost-string cost))
                         ;; TODO: not great, really want only do for single character headers
                         (mapcar (lambda (e) "") (nthcdr 4 last-row)))))
    ;; TODO: add in text indicating char-column if necessary
    (setq new-lisp-table
          (nconc
           new-lisp-table
           new-last-row))
    new-lisp-table))

(defun otdb-gear-create-temporary-buffer ()
  ;; TODO: create type of buffer too
  (let (the-new-buffer)
    (when (eq (otdb-table-detect) 'backpacking)
      (cond (otdb-gear-item-pattern
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-gear-pattern--" otdb-gear-item-pattern "--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*"))))
            (otdb-gear-item-tags
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-gear-tags--" otdb-gear-item-tags "--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*"))))
            (t
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-gear--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*")))))
      (with-current-buffer the-new-buffer
        (org-mode)
        (otdb-gear-mode)
        (insert "  |----------+------+--------+------+------+------+---+---|\n")
        (insert "  | Quantity | Item | Weight | Cost | Tags | Note | X | C |\n")
        (insert "  |----------+------+--------+------+------+------+---+---|\n"))
      the-new-buffer)))

(defun otdb-gear-calc-special-command ()
  (interactive)
  (let ((the-new-buffer (otdb-gear-create-temporary-buffer)))
    (otdb-gear-calc-special (cic:org-table-to-lisp-no-separators) the-new-buffer)
    (with-current-buffer the-new-buffer
      (insert "  |----------+------+--------+------+------+------+---+---|\n")
      (insert "  | 1        |      |        |      |      |      |   |   |\n")
      (insert "  |----------+------+--------+------+------+------+---+---|\n")
      (insert "  #+TBLEL: otdb-gear-calc-gear\n")
      (forward-line -2)
      (org-table-align)
      (beginning-of-line)
      (cic:org-table-eval-tblel)
      (goto-char (point-min))
)    (switch-to-buffer the-new-buffer)))

(defun otdb-gear-calc-special (lisp-table current-temporary-buffer)
  (let ((current-collection-name)
        (char-columns (otdb-table-parse-char-columns lisp-table)))
    ;; find all the current rows
    (dolist (lisp-row (butlast (cdr lisp-table)))
      ;; raise error if invalid
      (let ((collection-location (otdb-gear-find-collection (elt lisp-row 1))))
        ;; make sure I excludee invalid too
        (unless (or
                 (otdb-table-check-invalid-current-row-lisp lisp-row otdb-gear-column-mark char-columns)
                 ;; TODO: this not is confusing
                 (and otdb-gear-column-mark (not (otdb-table-check-current-row-lisp lisp-row otdb-gear-column-mark char-columns)))
                 ;; XXXX: allow continuing if thing is a collection of gear that does not match
                 ;;       items only
                 (and otdb-gear-item-pattern
                      (not (otdb-gear-find-collection (elt lisp-row 1)))
                      (not (string-match otdb-gear-item-pattern (elt lisp-row 1))))
                 ;; TODO: tag pattern match here
                 (and otdb-gear-item-tags
                      (not (otdb-gear-find-collection (elt lisp-row 1)))
                      (not (otdb-table-tag-pattern-match otdb-gear-item-tags (elt lisp-row 4)))))
          (unless collection-location
            (with-current-buffer current-temporary-buffer
              (insert (concat "  | " (mapconcat 'identity lisp-row " | ") "\n")))))
        (when (and collection-location (not (otdb-table-check-invalid-current-row-lisp lisp-row otdb-gear-column-mark char-columns)))
          (save-excursion
            ;; find the collection
            (with-current-file-min (car collection-location)
              ;; TODO: open everything up?
              (goto-char (cadr collection-location))
              (cic:org-find-table)
              ;; advance to table
              (otdb-gear-calc-special (cic:org-table-to-lisp-no-separators) current-temporary-buffer))))))))

(provide 'otdb-gear)
