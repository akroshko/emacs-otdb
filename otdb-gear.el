;;; otdb-gear.el --- For calculating the weights of backpacking (or other) gear.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20160130
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
                      (otdb-gear-get-cost-database-row row quantity))))
          (setq weight-cost-list (cons (list (car row-alist)
                                             weight
                                             cost)
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
        (count 1))
    (do-org-table-rows collection-filename collection-heading row
                       (setq new-item (strip-full-no-properties (elt row 1)))
                       (when (not (equal count 1))
                         (setq new-weight (elt (assoc new-item weight-cost-list) 1))
                         (setq new-cost (elt (assoc new-item weight-cost-list) 2))
                         (if (not new-weight)
                             (org-table-put count 3 "")
                           (org-table-put count 3 (otdb-gear-weight-string new-weight)))
                         (if (not new-cost)
                             (org-table-put count 4 "")
                           (org-table-put count 4 (otdb-gear-cost-string new-cost))))
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
       (string-to-number (org-table-get nil 4))))))

(defun otdb-gear-find-item (item)
  "Find the location of the ITEM."
  (cic:org-table-lookup-location otdb-gear-database otdb-gear-database-headline item 1))

(defun otdb-gear-get-items ()
  "Get list of all gear items from the database."
  ;; TODO: where is this used???
  (let ((items (cic:org-table-get-keys otdb-gear-database otdb-gear-database-headline)))
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

;; XXXX: if above global flag is set, only calculate non-nil and non-invalid for select columns
;; TODO: need key and/or menu item to toggle this variable
(defvar otdb-gear-column-mark
  nil
  "Set to string \"X\" for check and string \"C\" for consumable.")

(defun otdb-gear-calc-gear (lisp-table)
  "Calculated an updated lisp table from the LISP-TABLE
corresponding to a gear collection."
  (let ((weight 0)
        (cost 0)
        (new-lisp-table (butlast lisp-table))
        (last-row (car (last lisp-table)))
        (char-column (otdb-table-lisp-char-find-column lisp-table otdb-gear-column-mark))
        new-last-row)
    (when char-column
      (setq char-column (- char-column 1)))
    (dolist (lisp-row (butlast (cdr lisp-table)))
      (unless (and char-column (or (string= (strip-full (elt lisp-row char-column)) "")
                                   (string= (strip-full (elt lisp-row char-column)) "-")))
        (cond ((or (eq otdb-gear-weight-units 'lb)
                   (eq otdb-gear-weight-units 'kg))
               (setq weight (+ weight (otdb-table-lisp-row-float lisp-row 2))))
              ((eq otdb-gear-weight-units 'lb-g)
               ;; otherwise make sure weight is in grams
               (setq weight (+ weight (* (otdb-table-lisp-row-float lisp-row 2)
                                         (otdb-table-unit-conversion 'weight (otdb-table-unit (elt lisp-row 2)) "g")))))))
      (setq cost (+ cost (otdb-table-lisp-row-float lisp-row 3))))
    ;; insert into last row
    (setq new-last-row (list
                        (nconc
                         (list
                          (elt last-row 0)
                          (elt last-row 1)
                          (otdb-gear-weight-string weight)
                          (otdb-gear-cost-string cost))
                         ;; TODO: not great, really want only do for single character headers
                         (mapcar (lambda (e) "") (nthcdr 4 last-row)))))
    ;; add in char-column if necessary
    (when char-column
      (setcar (nthcdr char-column (car new-last-row)) otdb-gear-column-mark))
    (setq new-lisp-table
          (nconc
           new-lisp-table
           new-last-row))
    new-lisp-table))

(provide 'otdb-gear)
