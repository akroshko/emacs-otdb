;;; otdb-gear.el --- For calculating the weights of backpacking (or other) gear.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20150522
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

;; TODO: have cost only for expendables, or optional cost for other things
;; TODO: need to decide on unit... kgs or grams best, convert at to lbs at end if desired

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
    (setq database-row-alist (otdb-gear-item-row-multiple otdb-gear-database otdb-gear-database-headline key-list 1))
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
                           (org-table-put count 3 (format "%.4f" new-weight)))
                         (if (not new-cost)
                             (org-table-put count 4 "")
                           (org-table-put count 4 (format "%.4f" new-cost))))
                       (setq count (+ count 1)))
    (org-table-align)
    (org-table-recalculate (quote (16)))))

(defvar otdb-gear-database-cache
  nil
  "Cache of the gear database table.")

(defun otdb-gear-item-row-multiple (database table-name key-list &optional column)
  "Look up multiple ingredient rows in DATABASE file with heading
TABLE-NAME and keys KEY-LIST in column COLUMN.
TODO: can I make this function universal between gear and recipe?"
  ;; TODO this defun is duplicate between gear and recipe, not good!
  (let (lisp-table
        found-rows-alist)
    (if otdb-gear-database-cache
        (setq lisp-table otdb-gear-database-cache)
      (with-current-file-org-table database table-name
                                   (setq lisp-table (apk:org-table-to-lisp-no-separators))
                                   (setq otdb-gear-database-cache lisp-table)))
    (dolist (row lisp-table)
      ;; when column is a member
      (let ((column-stripped (strip-full-no-properties (elt row (- column 1)))))
        (when (member column-stripped key-list)
          (setq found-rows-alist (cons (list column-stripped (apk:org-table-assoc lisp-table column-stripped column)) found-rows-alist)))))
    found-rows-alist))

(defun otdb-gear-get-weight (row)
  "Get the weight of key in the ROW."
  (let* ((collection-list (otdb-gear-get-collections)))
    (if (member (strip-full (elt row 1)) collection-list)
        (/ (car (otdb-gear-get-collection-weight-cost (elt row 1))) (otdb-table-number (elt row 0)))
      (otdb-gear-get-weight-database (elt row 1) (elt row 0)))))

(defun otdb-gear-get-weight-database (item quantity)
  "Get the weight of QUANTITY of the ITEM from the database."
  (let* ((database-row (otdb-gear-database-row item)))
    (* (otdb-table-number quantity)
       (otdb-table-number (elt database-row 1))
       (otdb-table-unit-conversion 'weight (otdb-table-unit (elt database-row 1)) "kg"))))

(defun otdb-gear-get-weight-database-row (database-row quantity)
  "Get the weight of QUANTITY from database row DATABASE-ROW."
  (* (otdb-table-number quantity)
     (otdb-table-number (elt database-row 1))
     (otdb-table-unit-conversion 'weight (otdb-table-unit (elt database-row 1)) "kg")))

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
      (apk:org-find-table)
      (apk:org-table-last-row)
      (list (string-to-number (org-table-get nil 3)) (string-to-number (org-table-get nil 4))))))

(defun otdb-gear-find-item (item)
  "Find the location of the ITEM."
  (apk:org-table-lookup-location otdb-gear-database otdb-gear-database-headline item 1))

(defun otdb-gear-get-items ()
  "Get list of all gear items from the database."
  (let* ((items (apk:org-table-get-keys otdb-gear-database otdb-gear-database-headline))
         (dups (apk:get-list-duplicates items)))
    (when (> (length dups) 0)
      (mpp-echo (concat "Duplicate items: " (pp-to-string dups) otdb-gear-message-buffer)))
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

(defvar otdb-gear-collections-cache
  nil
  "Variable to store list of recipes.")

(defun otdb-gear-get-collections ()
  "Get list of all gear collections."
  (if otdb-gear-collections-cache
      otdb-gear-collections-cache
    (let (table
          table-name
          collection-list)
      (dolist (gear-file otdb-gear-collection-files)
        (do-org-tables gear-file table-name table
                       (when (string-match "\\(.*\\) :gear:" table-name)
                         (setq collection-list (cons (match-string 1 table-name) collection-list)))))
      (setq dups (apk:get-list-duplicates collection-list))
      (when (> (length dups) 0)
        (mpp-echo (concat "Duplicate collections: " (pp-to-string dups)) otdb-gear-message-buffer))
      (setq otdb-gear-collections-cache collection-list)
      collection-list)))

(defun otdb-gear-database-row (item)
  "Get the database row corresponding to gear item ITEM."
  (apk:org-table-lookup-row otdb-gear-database otdb-gear-database-headline item))

(provide 'otdb-gear)
