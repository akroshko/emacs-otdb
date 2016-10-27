;;; otdb-recipe.el --- Allows easy entry of nutritional information
;;; into org-mode tables and calculation of costs and macronutrient
;;; amounts.
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

(defvar otdb-recipe-mode-map
  nil
  "Keymap for otdb-recipe.")

(defvar otdb-recipe-backpacking-mode-map
  nil
  "Keymap for otdb-recipe backpacking stuff.")

(defvar otdb-recipe-column-mark
  nil
  "Set to string \"X\" for check and string \"C\" for cost.")

;; TODO: non-functional but here for completeness
(defvar otdb-recipe-item-pattern
  nil
  ;; TODO: need a better interface
  "Set to pattern needed for filtering results.")

;; TODO: non-functional but here for completeness
(defvar otdb-recipe-item-last-pattern
  nil
  ;; TODO: need a better interface
  "Holds the last pattern needed for filtering results.")

(defvar otdb-recipe-item-tags
  nil
  ;; TODO: need a better interface
  "Hold a set of tags.")

(defvar otdb-recipe-item-last-tags
  nil
  "Hold the last set of tags")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for different collection of recipe files
;; XXXX: needs to be here
(defun otdb-recipe-get-variable (lookup-variable &optional force)
  "Helper function to lookup different otdb-recipe variables
depending on context.  FORCE only forces backpacking recipes for
now."
  (let ((current-filename (ignore-errors (buffer-file-name))))
    (cond ((or (member current-filename (cdr (assoc 'otdb-recipe-files otdb-recipe-backpacking-alist))) (eq force 'backpacking))
           ;; use the backpacking version
           (let ((backpacking-recipe-files otdb-recipe-backpacking-alist))
             (cdr (assoc lookup-variable backpacking-recipe-files))))
          (t
           ;; use the standard version
           (let ((normal-recipe-files otdb-recipe-normal-alist))
             (cdr (assoc lookup-variable normal-recipe-files)))))))

(defvar otdb-recipe-read-column-mark-history
  nil
  "The history column mark inputs.")

(defun otdb-recipe-read-column-mark ()
  (interactive)
  (let ((thestring (read-string (concat "Column mark expression " (pp-to-string otdb-recipe-column-mark) ": ") nil otdb-recipe-read-column-mark-history otdb-recipe-column-mark)))
    (if (cic:is-not-empty-string-nil thestring)
        (setq otdb-recipe-column-mark thestring)
      nil)))

(defun otdb-recipe-menu-tags ()
  "Set menu item to reflect current value of otdb-recipe-item-tags."
  (cons (cond (otdb-recipe-item-tags
               (concat "Disable (otdb-recipe-item-tags): " (pp-to-string otdb-recipe-item-tags)))
              (otdb-recipe-item-last-tags
               (concat "Re-enable (otdb-recipe-item-tags): " (pp-to-string otdb-recipe-item-last-tags)))
              (t
               "Empty (otdb-recipe-item-tags): "))
        (lambda ()
          (interactive)
          (if otdb-recipe-item-tags
              (progn
                (setq otdb-recipe-item-last-tags otdb-recipe-item-tags)
                (setq otdb-recipe-item-tags nil))
            (progn
              (setq otdb-recipe-item-tags otdb-recipe-item-last-tags))))))

(defun otdb-recipe-menu-item-pattern ()
  (cons (cond (otdb-recipe-item-pattern
               (concat "Disable (otdb-recipe-item-pattern): " (pp-to-string otdb-recipe-item-pattern)))
              ((and otdb-recipe-item-pattern otdb-recipe-item-last-pattern)
               (concat "Re-enable (otdb-recipe-item-pattern): " (pp-to-string otdb-recipe-item-pattern)))
              (t
               "Empty (otdb-recipe-item-pattern): "))
        (lambda ()
          (interactive)
          (if otdb-recipe-item-pattern
              (progn
                (setq otdb-recipe-item-last-pattern otdb-recipe-item-pattern)
                (setq otdb-recipe-item-pattern nil))
            (progn
              (setq otdb-recipe-item-pattern otdb-recipe-item-last-pattern))))))

(defun otdb-recipe-menu-column-mark ()
  (cons (concat "Change column mark: " (pp-to-string otdb-recipe-column-mark)) 'otdb-recipe-read-column-mark))

(defun otdb-recipe-reset-filters ()
  (interactive)
  ;; XXXX: resetting clears all
  (setq otdb-recipe-item-last-pattern nil)
  (setq otdb-recipe-item-pattern nil)
  (setq otdb-recipe-item-last-tags nil)
  (setq otdb-recipe-item-tags nil)
  ;; TODO: last column mark maybe?
  (setq otdb-recipe-column-mark nil))

(defun otdb-recipe-menu-files (map otdb-recipe-menu &optional force)
  (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-collections)              (cons "Recipe files" (make-sparse-keymap "recipe files")))
  ;; TODO: does not update dynamically at the moment and may cause issues, will cause issues switching between different kinds of recipes (normal/backpacking)
  (when (and (boundp 'otdb-recipe-backpacking-alist) (boundp 'otdb-recipe-normal-alist))
    (dolist (collection (cic:ensure-list (otdb-recipe-get-variable 'otdb-recipe-files force)))
      (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-collections collection) (cons collection (cic:make-file-finder collection)))))
  ;; https://stackoverflow.com/questions/9966279/how-to-dynamically-define-a-menu-item-what-is-the-thing-in-square-braces
  ;; TODO: hope this always works out properly, might have issue if databases change
  ;;       does not update dynamically at the moment
  (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-databases)                (cons "Recipe databases" (make-sparse-keymap "recipe databases")))
  (when (and (boundp 'otdb-recipe-backpacking-alist) (boundp 'otdb-recipe-normal-alist))
    (dolist (database (cic:ensure-list (otdb-recipe-get-variable 'otdb-recipe-database force)))
      (define-key map (vector 'menu-bar otdb-recipe-menu 'recipe-databases 'database)    (cons database (cic:make-file-finder database))))))

(defun otdb-recipe-mode-map (&optional force)
  (let ((map (make-sparse-keymap))
        (otdb-recipe-menu (if (eq force 'backpacking)
                              'otdb-recipe-backpacking-menu
                            'otdb-recipe-normal-menu)))
    (otdb-table-skeleton-map map)
    (define-key map (vector 'menu-bar otdb-recipe-menu)                           (cons "otdb-recipe" (make-sparse-keymap "otdb-recipe")))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'reset-filters)            (cons "Reset recipe filters" 'otdb-recipe-reset-filters))
    ;; TODO: generate these from alist
    (otdb-recipe-menu-files map otdb-recipe-menu force)
    (define-key map (vector 'menu-bar otdb-recipe-menu 'separator4) '("--"))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'export-recipe-multiple)   (cons "Export recipe multiple" 'otdb-recipe-export-multiple))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'export-recipe)            (cons "Export recipe"          'otdb-recipe-export))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'separator3) '("--"))
    ;; TODO: change for tags more like
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-patterns)            (cons "Recipe ingredient patterns" (make-sparse-keymap "recipe ingredient patterns")))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-patterns 'spice)     (cons "Spice" (lambda () (interactive)
                                                                                                  nil)))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-patterns 'packaging) (cons "Packaging" (lambda () (interactive)
                                                                                                      nil)))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-pattern)             (otdb-recipe-menu-item-pattern))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-tags)                (cons "Ingredient tags" (make-sparse-keymap "ingredient tags patterns")))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-tags 'spice)         (cons "Spice" (lambda () (interactive)
                                                                                                  (setq otdb-recipe-item-tags "spice"))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-tags 'packaging)     (cons "Packaging" (lambda () (interactive)
                                                                                                      (setq otdb-recipe-item-tags "packaging"))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'item-tag)                 (otdb-recipe-menu-tags))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'separator2) '("--"))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'column-mark-cost)         '(menu-item "Toggle column mark cost (C)" (lambda () (interactive) (setq otdb-recipe-column-mark "C"))
                                                                                              :button (:toggle . (equal otdb-recipe-column-mark "C"))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'column-mark-not-check)    '(menu-item "Toggle column mark check (not X)" (lambda () (interactive) (setq otdb-recipe-column-mark "(not X)"))
                                                                                              :button (:toggle . (equal otdb-gear-column-mark "(not X)"))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'column-mark-check)        '(menu-item "Toggle column mark check (X)" (lambda () (interactive) (setq otdb-recipe-column-mark "X"))
                                                                                              :button (:toggle . (equal otdb-recipe-column-mark "X"))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'column-mark-nil)          '(menu-item "Toggle column mark empty" (lambda () (interactive) (setq otdb-recipe-column-mark nil))
                                                                                              :button (:toggle . (equal otdb-recipe-column-mark nil))))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'column-mark)              (otdb-recipe-menu-column-mark))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'toggle-check-invalid)     '("Toggle (X) invalid" . otdb-table-invalid-toggle-check-line))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'toggle-check)             '("Toggle (X)" . otdb-table-set-toggle-check-line))
    (define-key map (vector 'menu-bar otdb-recipe-menu 'calc-special-command)     '(menu-item "Use special buffer for calculation" (lambda () (interactive) (otdb-recipe-calc-special-command))
                                                                                              :keys "s-d s"))
    (otdb-table-skeleton-menu-map map otdb-recipe-menu)
    map))
;; doubled up for now
(setq otdb-recipe-mode-map (otdb-recipe-mode-map))
(setq otdb-recipe-backpacking-mode-map (otdb-recipe-mode-map 'backpacking))

(defun otdb-recipe-update-menu (&optional force)
  ;; (setq otdb-recipe-mode-map (otdb-recipe-mode-map))
  ;; (setq otdb-recipe-backpacking-mode-map (otdb-recipe-mode-map 'backpacking))
  ;; TODO: check mode first
  (let ((the-mode-map (if (eq force 'backpacking)
                          otdb-recipe-backpacking-mode-map
                        otdb-recipe-mode-map))
        (otdb-recipe-menu (if (eq force 'backpacking)
                              'otdb-recipe-backpacking-menu
                            'otdb-recipe-normal-menu)))
    (define-key the-mode-map (vector 'menu-bar otdb-recipe-menu 'column-mark)  (otdb-recipe-menu-column-mark))
    (define-key the-mode-map (vector 'menu-bar otdb-recipe-menu 'item-pattern) (otdb-recipe-menu-item-pattern))
    (define-key the-mode-map (vector 'menu-bar otdb-recipe-menu 'item-tag)     (otdb-recipe-menu-tags))))

(defun otdb-recipe-update-backpacking-menu ()
  (otdb-recipe-update-menu 'backpacking))

;; (run-hooks otdb-recipe-update-menu')
(add-hook 'menu-bar-update-hook 'otdb-recipe-update-menu)
(add-hook 'menu-bar-update-hook 'otdb-recipe-update-backpacking-menu)

(define-minor-mode otdb-recipe-mode
  :global nil
  :lighter " otdb-recipe-normal"
  :keymap otdb-recipe-mode-map
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))

(define-minor-mode otdb-recipe-backpacking-mode
  :global nil
  :lighter " otdb-recipe-backpacking"
  :keymap otdb-recipe-backpacking-mode-map
  (make-local-variable 'otdb-table-tablet-mode)
  (make-local-variable 'otdb-old-modeline-color)
  (make-local-variable 'otdb-old-modeline-color-inactive)
  (setq-local otdb-table-tablet-mode nil))

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
    (if (not (full-string-p default-item))
        (setq prompt "Grocery item to add: ")
      (setq prompt (concat "Grocery item to add (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (when (not (full-string-p (strip-full selected-item)))
      (setq selected-item default-item))
    (cond ((member selected-item shopping-list-checked)
           (mpp-echo (format "Grocery item %s already checked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
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
                                 (when (ignore-errors (re-search-forward (concat cic:emacs-stdlib-checkbox-regexp " " selected-item)))
                                   (org-toggle-checkbox))
                                 (widen))))
           (mpp-echo (format "Grocery item %s successfully checked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          (t
           (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                             (when (string-match "^Grocery.*" headline-name)
                               (setq shopping-headlines (cons headline-name shopping-headlines))))
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
                  (with-current-file-headline (otdb-recipe-get-variable 'otdb-recipe-agenda) (nth (- unmatched-item 48) shopping-headlines)
                                              (hide-subtree)
                                              (org-show-subtree)
                                              (org-end-of-subtree)
                                              (cic:org-insert-indent-list-item)
                                              (insert (concat "[X] " selected-item))))
                 ((equal unmatched-item (string-to-char "q"))
                  ;; just finish
                  ))))))

(add-hook 'otdb-recipe-mode-hook 'otdb-recipe-mode-init)
(defun otdb-recipe-mode-init ()
  (when (and (or otdb-recipe-mode otdb-recipe-backpacking-mode) (functionp 'hl-line-mode))
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
    (if (not (full-string-p default-item))
        (setq prompt "Shopping item to remove: ")
      (setq prompt (concat "Shopping item to remove (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (when (not (full-string-p (strip-full selected-item)))
      (setq selected-item default-item))
    (cond ((not (member selected-item full-list))
           (mpp-echo (format "Shopping item %s not found.  Nothing to remove!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((member selected-item shopping-list-checked)
           (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                             (when (string-match "^Grocery.*" headline-name)
                               ;; find in subtree
                               (save-excursion
                                 (org-narrow-to-subtree)
                                 (goto-char (point-min))
                                 (when (ignore-errors (re-search-forward (concat cic:emacs-stdlib-checkbox-regexp " " selected-item)))
                                   (org-toggle-checkbox))
                                 (widen))))
           (mpp-echo (format "Shopping item %s successfully unchecked!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          ((member selected-supply supply-list-unchecked)
           (mpp-echo (format "Shopping item %s already unchecked!" selected-supply) (otdb-recipe-get-variable 'otdb-recipe-message-buffer))))))

(defun otdb-table-agenda-jump ()
  "Jump to the item near point in agenda."
  (interactive)
  (let* ((default-item (ignore-errors (otdb-table-get-key-at-point)))
         (shopping-list-checked (otdb-recipe-get-shopping 'checked))
         (shopping-list-unchecked (otdb-recipe-get-shopping 'unchecked))
         (full-list (append shopping-list-checked shopping-list-unchecked))
         prompt
         selected-item)
    (if (not (full-string-p default-item))
        (setq prompt "Supply to jump to: ")
      (setq prompt (concat "Supply to jump to (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (when (not (full-string-p (strip-full selected-item)))
      (setq selected-item default-item))
    (cond ((not (member selected-item full-list))
           (mpp-echo (format "Supply %s not found.  Nothing to remove!" selected-item) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
          (t
           (find-file (otdb-recipe-get-variable 'otdb-recipe-agenda))
           (goto-char (point-min))
           (re-search-forward (concat cic:emacs-stdlib-checkbox-regexp " " selected-item))
           (org-back-to-heading)
           (beginning-of-line)
           (hide-subtree)
           (org-show-subtree)
           (re-search-forward (concat cic:emacs-stdlib-checkbox-regexp " " selected-item))
           (beginning-of-line)))))

(defun otdb-recipe-agenda-price-check ()
  "Add an item near point to be price checked."
  (interactive)
  (let* ((default-item (ignore-errors (otdb-table-get-key-at-point)))
         (shopping-list-checked (otdb-recipe-get-shopping 'checked))
         (shopping-list-unchecked (otdb-recipe-get-shopping 'unchecked))
         (full-list (append shopping-list-checked shopping-list-unchecked))
         prompt
         selected-item)
    (if (not (full-string-p default-item))
        (setq prompt "Item to price check: ")
      (setq prompt (concat "Item to price check (" default-item "): ")))
    (setq selected-item (completing-read-default prompt full-list nil nil))
    (unless (full-string-p selected-item)
      (setq selected-item default-item))
    (find-file (otdb-recipe-get-variable 'otdb-recipe-agenda))
    (goto-char (point-min))
    (goto-char (org-find-exact-headline-in-buffer (otdb-recipe-get-variable 'otdb-recipe-price-check-headline)))
    (beginning-of-line)
    (hide-subtree)
    (org-show-subtree)
    (org-end-of-subtree)
    (cic:org-insert-indent-list-item)
    (insert selected-item)))

(defun otdb-recipe-get-shopping (&optional checktype)
  "Get the list of items to shop for, optionally with CHECKTYPE."
  (let (line
        matched-text
        shopping-list
        (shopping-file (otdb-recipe-get-variable 'otdb-recipe-agenda)))
    (cond ((eq checktype 'checked)
           (setq checkbox-regexp cic:emacs-stdlib-checkbox-checked-regexp))
          ((eq checktype 'unchecked)
           (setq checkbox-regexp cic:emacs-stdlib-checkbox-unchecked-regexp))
          (t
           (setq checkbox-regexp cic:emacs-stdlib-checkbox-regexp)))
    (do-org-headlines shopping-file headline-name headline-subtree
                      (when (string-match "Grocery.*" headline-name)
                        (do-org-list-items shopping-file headline-name item-line
                                           (when (string-match checkbox-regexp item-line)
                                             (setq matched-text (match-string 3 item-line))
                                             (setq shopping-list (cons (strip-full-no-properties matched-text) shopping-list))))))
    (setq dups (cic:get-list-duplicates shopping-list))
    (when (> (length dups) 0)
      (mpp-echo (concat "Duplicate groceries: " (pp-to-string dups)) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
    (nreverse shopping-list)))

(defun otdb-recipe-get-calories-protein-fat-weight-volume-cost (recipe)
  "Get the totals from a particular RECIPE. Generally to use in a
another recipe."
  (let ((recipe-location (otdb-recipe-find recipe)))
    (with-current-file (car recipe-location)
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
      (with-current-file-min recipe-file
        (let ((found (progn
                       (when  (re-search-forward (concat "^\* " recipe " :recipe:") nil t)
                         (beginning-of-line)
                         (point)))))
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
                         (setq recipe-list (cons (match-string 1 table-name) recipe-list)))))
      (setq otdb-table-collections-cache recipe-list)
      recipe-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recipes
(defun otdb-recipe-calories-protein-fat-weight (ingredient-row quantity column to-unit)
  "Return the nutritional values of QUANTITY from database row
INGREDIENT-ROW from COLUMN converting to units TO-UNIT.
TODO combine with very similar cost function"
  (let* ((unit-type (otdb-table-unit-type (otdb-table-unit quantity)))
         ;; used to convert within servings
         (to-quantity (cond ((eq unit-type 'weight)
                             (elt ingredient-row 4))
                            ((eq unit-type 'volume)
                             (elt ingredient-row 5))
                            ;; TODO want to know what to do when quantity is dimensionless
                            (t
                             (elt ingredient-row 5)))))
    (cond
     ((and (not unit-type)
           (full-string-p (elt ingredient-row 5))
           (not (otdb-table-unit-type (elt ingredient-row 5))))
      ;; dimensionless units are volume
      (*
       (/
        (otdb-table-number quantity)
        (otdb-table-number (elt ingredient-row 5)))
       (string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (full-string-p (elt ingredient-row 2))
           (full-string-p (elt ingredient-row 5))
           (otdb-table-unit-type (elt ingredient-row 5)))
      (* (/ (* (otdb-table-number quantity)
               (otdb-table-number (elt ingredient-row 2)))
            (otdb-table-number (elt ingredient-row 5)))
         (otdb-table-unit-conversion 'volume (otdb-table-unit (elt ingredient-row 2)) (otdb-table-unit (elt ingredient-row 5)))
         (string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (full-string-p (elt ingredient-row 1))
           (full-string-p (elt ingredient-row 4))
           (otdb-table-unit-type (elt ingredient-row 4))
           (full-string-p (elt ingredient-row 2))
           (not (otdb-table-unit-type (elt ingredient-row 2))))
      (* (otdb-table-number quantity)
         (/ (/ (otdb-table-number (elt ingredient-row 1))
               (otdb-table-number (elt ingredient-row 2)))
            (otdb-table-number (elt ingredient-row 4)))
         (otdb-table-unit-conversion 'weight (otdb-table-unit (elt ingredient-row 1)) (otdb-table-unit (elt ingredient-row 4)))
         (string-to-float (elt ingredient-row column))))
     ((and (not unit-type)
           (full-string-p (elt ingredient-row 1))
           (full-string-p (elt ingredient-row 4))
           (otdb-table-unit-type (elt ingredient-row 4)))
      (* (/ (* (otdb-table-number quantity)
               (otdb-table-number (elt ingredient-row 1)))
            (otdb-table-number (elt ingredient-row 4)))
         (otdb-table-unit-conversion 'weight (otdb-table-unit (elt ingredient-row 1)) (otdb-table-unit (elt ingredient-row 4)))
         (string-to-float (elt ingredient-row column))))
     (t
      (*
       (/ (otdb-table-number quantity)
          (otdb-table-number to-quantity))
       (otdb-table-unit-conversion unit-type (otdb-table-unit quantity) (otdb-table-unit to-quantity))
       (string-to-float (elt ingredient-row column)))))))

(defun otdb-recipe-cost-row (ingredient-row quantity)
  "Return the cost based on quantity."
  (let ((unit-type (otdb-table-unit-type (otdb-table-unit quantity))))
    ;; figure out if we have both weight units for cost or not
    (cond ((eq unit-type 'weight)
           (cond ((not (full-string-p (elt ingredient-row 1)))
                  (*
                   ;; quantity
                   (otdb-table-number quantity)
                   ;; cost-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit (elt ingredient-row 1)))
                   ;; cost-quantity / cost)
                   (/ (otdb-table-number (elt ingredient-row 3))
                      (otdb-table-number (elt ingredient-row 1)))))
                 ;; all weight
                 ((and (full-string-p (elt ingredient-row 1))
                       (full-string-p (elt ingredient-row 4)))
                  (*
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number (elt ingredient-row 1)))
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit quantity)
                    (otdb-table-unit (elt ingredient-row 1)))
                   (otdb-table-number (elt ingredient-row 3))))
                 (t
                  ;; quantity / serving-quantity
                  (*
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number (elt ingredient-row 4)))
                   ;;  serving-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit (elt ingredient-row 4)))
                   ;; serving-quantity(alt)
                   (otdb-table-number (elt ingredient-row 5))
                   ;;  serving-units(alt)/cost-units
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit (elt ingredient-row 5))
                    (otdb-table-unit (elt ingredient-row 2)))
                   ;;  cost-quantity/cost
                   (/
                    (string-to-float (elt ingredient-row 3))
                    (otdb-table-number (elt ingredient-row 2)))))))
          ((eq unit-type 'volume)
           (cond ((full-string-p (elt ingredient-row 2))
                  (*
                   ;; quantity
                   (otdb-table-number quantity)
                   ;; cost-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit (elt ingredient-row 2)))
                   ;; cost-quantity / cost)
                   (/ (otdb-table-number (elt ingredient-row 3))
                      (otdb-table-number (elt ingredient-row 2)))))
                 (t
                  (*
                   ;; quantity / serving-quantity
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number (elt ingredient-row 5)))
                   ;;  serving-units / quantity-units
                   (otdb-table-unit-conversion
                    'volume
                    (otdb-table-unit quantity)
                    (otdb-table-unit (elt ingredient-row 5)))
                   ;; serving-quantity(alt)/serving-quantity
                   (otdb-table-number (elt ingredient-row 4))
                   ;;  serving-units(alt)/cost-units
                   (otdb-table-unit-conversion
                    'weight
                    (otdb-table-unit (elt ingredient-row 4))
                    (otdb-table-unit (elt ingredient-row 1)))
                   ;;  cost-quantity/cost
                   (/
                    (string-to-float (elt ingredient-row 3))
                    (otdb-table-number (elt ingredient-row 1)))))))
          (t
           (cond ((not (full-string-p (otdb-table-unit (elt ingredient-row 2))))
                  (*
                   (/
                    (otdb-table-number quantity)
                    (otdb-table-number (elt ingredient-row 2)))
                   (otdb-table-number (elt ingredient-row 3))))
                 (t
                  (*
                   (otdb-table-number quantity)
                   (otdb-table-number (elt ingredient-row 3)))))))))

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
      (if (member (strip-full-no-properties (elt row 1)) recipe-list)
          (progn
            (setq recipe-calories-protein-fat-weight-volume-cost-list
                  (let ((ccl (otdb-recipe-get-calories-protein-fat-weight-volume-cost (elt row 1))))
                    (cons (list (strip-full-no-properties (elt row 1))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 0)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 1)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 2)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 3)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 4)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 5)))
                                (ignore-errors (* (otdb-table-number (elt row 0)) (elt ccl 6)))
                                "")
                          recipe-calories-protein-fat-weight-volume-cost-list))))
        (progn
          (setq quantity-alist (cons (list (strip-full-no-properties (elt row 1))
                                           (strip-full-no-properties (elt row 0)))
                                     quantity-alist))
          (setq key-list (cons (strip-full-no-properties (elt row 1)) key-list)))))
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
               (when (full-string-p (elt row 6))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 6 nil))))
              (protein
               (when (full-string-p (elt row 7))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 7 nil))))
              (fat
               (when (full-string-p (elt row 8))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 8 nil))))
              (weight
               (if (full-string-p (elt row 9))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity 9 nil))
                 (when (full-string-p (elt row 4))
                   (ignore-errors
                     (otdb-recipe-calories-protein-fat-weight row quantity 4 nil)))))
              (volume
               (when (full-string-p (elt row 10))
                 (ignore-errors
                   (otdb-recipe-calories-protein-fat-weight row quantity 10 nil))))
              (tags
               (when (full-string-p (elt row 13))
                 (elt row 13)))
              (cost (ignore-errors
                      (otdb-recipe-cost-row row quantity))))
          (setq calories-protein-fat-weight-volume-cost-list (cons (list (car row-alist)
                                                                         calories
                                                                         protein
                                                                         fat
                                                                         weight
                                                                         volume
                                                                         cost
                                                                         tags)
                                                                   calories-protein-fat-weight-volume-cost-list)))))
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
                       (setq new-ingredient (strip-full-no-properties (elt row 1)))
                       (when (not (equal count 1))
                         (setq new-calories (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 1))
                         (setq new-protein (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 2))
                         (setq new-fat (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 3))
                         (setq new-weight (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 4))
                         (setq new-volume (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 5))
                         (setq new-cost (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 6))
                         (setq new-tags (elt (assoc new-ingredient calories-protein-fat-weight-volume-cost-list) 7))
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
      (setq ingredients (append ingredients (cic:org-table-get-keys database (otdb-recipe-get-variable 'otdb-recipe-database-headline))))
      )
    (setq dups (cic:get-list-duplicates ingredients))
    (when (> (length dups) 0)
      (mpp-echo (concat "Duplicate ingredients: " (pp-to-string dups)) (otdb-recipe-get-variable 'otdb-recipe-message-buffer)))
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

(defun otdb-recipe-get-invalid-text (text)
  "XXXX: unused for now, want to mark things that are bad with a color."
  (propertize text 'font-lock-face '(:foreground "red")))

(defun otdb-recipe-agenda-push-groceries ()
  "Push the currently checked groceries to the special file for export."
  (interactive)
  (with-current-file (cdr (assoc 'otdb-recipe-shopping otdb-recipe-normal-alist))
    (erase-buffer))
  ;; loop over the headings with "Grocery"
  (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                    (when (string-match "^Grocery.*" headline-name)
                      (with-current-file (otdb-recipe-get-variable 'otdb-recipe-shopping)
                        (insert headline-subtree)
                        (insert "\n"))))
  ;; kill unchecked lines
  (with-current-file-min (otdb-recipe-get-variable 'otdb-recipe-shopping)
    (while (= (forward-line 1) 0)
      (let ((current-line (cic:get-current-line)))
        (when (string-match "\\[ \\]" current-line)
          (beginning-of-line)
          (let ((kill-whole-line t))
            (kill-line)
            (forward-line -1))))))
  ;; add price checks
  (do-org-headlines (otdb-recipe-get-variable 'otdb-recipe-agenda) headline-name headline-subtree
                    (when (string-match (otdb-recipe-get-variable 'otdb-recipe-price-check-headline)  headline-name)
                      (with-current-file (otdb-recipe-get-variable 'otdb-recipe-shopping)
                        (insert headline-subtree)
                        (insert "\n")))))

(defun otdb-recipe-database-calorie-costs (package-weight package-volume package-cost serving-weight serving-volume serving-calories)
  "Helper function for TBLFM to calculate cost per 1000 calories
in the database."
  ;; check for nils and empty strings
  (cond ((and (full-string-p package-weight) (full-string-p serving-weight))
         (let ((factor (otdb-table-unit-conversion 'weight (otdb-table-unit package-weight) (otdb-table-unit serving-weight))))
           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-weight) (otdb-table-number serving-weight)) (otdb-table-number serving-calories))) 1000)))
        ((and (full-string-p package-volume) (full-string-p serving-volume))
         (let ((factor (otdb-table-unit-conversion 'volume (otdb-table-unit package-volume) (otdb-table-unit serving-volume))))
           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-volume) (otdb-table-number serving-volume)) (otdb-table-number serving-calories))) 1000)))
        (t
         error)))

(defun otdb-recipe-database-protein-costs (package-weight package-volume package-cost serving-weight serving-volume serving-protein)
  "Helper function for TBLFM to calculate cost per 100g protein
in the database."
  ;; check for nils and empty strings
  (cond ((and (full-string-p package-weight) (full-string-p serving-weight))
         (let ((factor (otdb-table-unit-conversion 'weight (otdb-table-unit package-weight) (otdb-table-unit serving-weight))))
           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-weight) (otdb-table-number serving-weight)) (otdb-table-number serving-protein))) 100)))
        ((and (full-string-p package-volume) (full-string-p serving-volume))
         (let ((factor (otdb-table-unit-conversion 'volume (otdb-table-unit package-volume) (otdb-table-unit serving-volume))))
           (* (/ (otdb-table-number package-cost) (* factor (/ (otdb-table-number package-volume) (otdb-table-number serving-volume)) (otdb-table-number serving-protein))) 100)))
        (t
         error)))

(defun otdb-recipe-export-multiple ()
  "Export each component recipe in a table containing recipes to a pdf file in ~/tmp."
  (interactive)
  (require 'ox-publish)
  ;; loop over rows in current table
  (save-excursion
    (org-back-to-heading)
    (let ((otdb-recipe-temp-directory "~/tmp/")
          (heading-name-collection (save-excursion
                                     (org-back-to-heading)
                                     ;; strip off after first colon
                                     (strip-full (car (split-string (cic:get-headline-text (cic:get-current-line)) ":")))))
          (current-file (buffer-file-name))
          (first-row t))
      (with-current-file (concat otdb-recipe-temp-directory heading-name-collection ".org")
        (erase-buffer)
        (let (current-recipe-subtree
              (tmp-buffer-string ""))
          (do-org-table-rows current-file heading-name-collection current-row
                             (if first-row
                                 (setq first-row nil)
                               ;; ingredient should be empty if on invalid non-first row
                               (when (cic:is-not-empty-string-nil (elt current-row 1))
                                 ;; lookup recipe and get subtree from there
                                 ;; do process to add one recipe to buffer, get this into a function
                                 (let ((recipe-location (otdb-recipe-find (elt current-row 1))))
                                   (with-current-file (car recipe-location)
                                     (goto-char (cadr recipe-location))
                                     (org-mark-subtree)
                                     (setq current-recipe-subtree (buffer-substring (region-beginning) (region-end)))
                                     (setq tmp-buffer-string (concat tmp-buffer-string (otdb-recipe-add-tmp-buffer current-recipe-subtree))))))))
          (insert tmp-buffer-string)
          (goto-char (point-min))
          (otdb-recipe-add-latex-header heading-name-collection)
          (save-buffer)
          (org-latex-export-to-pdf))))))

(defun otdb-recipe-export ()
  "Export the single recipe in a table containing recipes to a
pdf file in ~/tmp."
  (interactive)
  (require 'ox-publish)
  (save-excursion
    ;; get current table
    (org-mark-subtree)
    (let ((heading-name (save-excursion
                          (org-back-to-heading)
                          ;; strip off after first colon
                          (strip-full (car (split-string (cic:get-headline-text (cic:get-current-line)) ":")))))
          (current-recipe-subtree (buffer-substring (region-beginning) (region-end)))
          (otdb-recipe-temp-directory "~/tmp/"))
      ;; put table in temporary file named after headline
      (with-current-file (concat otdb-recipe-temp-directory heading-name ".org")
        (erase-buffer)
        (insert (otdb-recipe-add-tmp-buffer current-recipe-subtree))
        (goto-char (point-min))
        ;; put in the header
        (otdb-recipe-add-latex-header)
        (save-buffer)
        (org-latex-export-to-pdf)))))

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
    (when (search-forward "#+BEGIN_COMMENT")
      (beginning-of-line)
      ;; delete to end of buffer
      (kill-region (point) (point-max)))
    (goto-char (point-min))
    ;; get rid of tags?
    (when (string-match ":" (cic:get-current-line))
      (search-forward ":")
      (backward-char)
      (kill-line))
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

;; add to post-command-hook
;; (add-hook 'post-command-hook (lambda ()
;;                                (setq otdb-recipe-need-warning-partial t)))

(defun otdb-recipe-calc-recipe (lisp-table)
  "Calculated an updated lisp table from the LISP-TABLE
corresponding to a recipe."
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
        (char-columns (otdb-table-parse-char-columns lisp-table))
        (ask-continue t ;; (otdb-recipe-ask-continue)
                      )
        (new-lisp-table (list (car lisp-table))))
    (when ask-continue
      ;; add up the directly summable columns
      (dolist (lisp-row (butlast (cdr lisp-table)))
        ;; only skip non-intermediate calculations
        (unless (otdb-table-check-invalid-current-row-lisp lisp-row otdb-recipe-column-mark char-columns)
          (setq calories (+ calories (otdb-table-lisp-row-float lisp-row 3)))
          (setq protein (+ protein (otdb-table-lisp-row-float lisp-row 4)))
          (setq fat (+ fat (otdb-table-lisp-row-float lisp-row 5)))
          (setq cost (+ cost (otdb-table-lisp-row-float lisp-row 6)))
          ;; do the appropriate sums from the added up columns
          ;; control for case when calories are zero
          (setq weight (+ weight (otdb-table-lisp-row-float lisp-row 12)))
          (setq volume (+ volume (otdb-table-lisp-row-float lisp-row 13))))
        (if (otdb-table-lisp-row-check lisp-row 6)
            (progn
              (if (otdb-table-lisp-row-check lisp-row 3)
                  (setq cost-calories-column (nconc cost-calories-column (list (/ (otdb-table-lisp-row-float lisp-row 6) (/ (otdb-table-lisp-row-float lisp-row 3) 1000.0)))))
                (setq cost-calories-column (nconc cost-calories-column (list nil))))
              (if (otdb-table-lisp-row-check lisp-row 4)
                  (setq cost-protein-column (nconc cost-protein-column (list (/ (otdb-table-lisp-row-float lisp-row 6) (/ (otdb-table-lisp-row-float lisp-row 4) 100.0)))))
                (setq cost-protein-column (nconc cost-protein-column (list nil)))))
          (progn
            (setq cost-calories-column (nconc cost-calories-column (list nil)))
            (setq cost-protein-column (nconc cost-protein-column (list nil)))))
        (if (otdb-table-lisp-row-check lisp-row 3)
            (progn
              (let ((row-calories (otdb-table-lisp-row-float lisp-row 3))
                    (row-protein (otdb-table-lisp-row-float lisp-row 4))
                    (row-fat (otdb-table-lisp-row-float lisp-row 5)))
                (setq percent-carb-column (nconc percent-carb-column (list (* 100.0 (/ (- row-calories (+ (* 4.0 row-protein) (* 9.0 row-fat))) row-calories)))))
                (setq percent-protein-column (nconc percent-protein-column (list (* 100.0 (/ (* 4.0 row-protein) row-calories)))))
                (setq percent-fat-column (nconc percent-fat-column (list (* 100.0 (/ (* 9.0 row-fat) row-calories)))))))
          (progn
            (setq percent-carb-column (nconc percent-carb-column (list nil)))
            (setq percent-protein-column (nconc percent-protein-column (list nil)))
            (setq percent-fat-column (nconc percent-fat-column (list nil))))))
      (when (/= calories 0.0)
        (setq cost-calories (/ cost (/ calories 1000.0)))
        (setq percent-carb (* 100.0 (/ (- calories (+ (* 4.0 protein) (* 9.0 fat))) calories)))
        (setq percent-protein (* 100.0 (/ (* 4.0 protein) calories)))
        (setq percent-fat (* 100.0 (/ (* 9.0 fat) calories))))
      (when (/= protein 0.0)
        (setq cost-protein (/ cost (/ protein 100.0))))
      ;; insert into last row
      (dolist (current-lisp-row (cdr (butlast lisp-table)))
        (setq new-lisp-table
              (nconc
               new-lisp-table
               (list (nconc
                      (subseq current-lisp-row 0 7)
                      (list
                       (otdb-table-format-number-nil (pop cost-calories-column) 3)
                       (otdb-table-format-number-nil (pop cost-protein-column) 3)
                       (otdb-table-format-number-nil (pop percent-carb-column) 3)
                       (otdb-table-format-number-nil (pop percent-protein-column) 3)
                       (otdb-table-format-number-nil (pop percent-fat-column) 3))
                      (nthcdr 12 current-lisp-row))))))
      (setq new-lisp-table (nconc
                            new-lisp-table
                            (list
                             (list
                              (caar (last lisp-table))
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
                              (otdb-table-format-number-zero volume 2)))
                            (nthcdr 13 (last lisp-table))))
      new-lisp-table)))

(defun otdb-recipe-create-temporary-buffer ()
  ;; TODO: create type of buffer too
  (let (the-new-buffer)
    (when (eq (otdb-table-detect) 'recipe)
      (cond (otdb-recipe-item-pattern
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-recipe-pattern--" otdb-recipe-item-pattern "--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*"))))
            (otdb-recipe-item-tags
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-recipe-tags--" otdb-recipe-item-tags "--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*"))))
            (t
             (setq the-new-buffer (generate-new-buffer (concat "*otdb-recipe--" (format-time-string "%Y%m%dT%H%M%S" (current-time)) "*")))))
      (with-current-buffer the-new-buffer
        (org-mode)
        (otdb-recipe-mode)
        (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
        (insert "  | Quantity | Ingredient | Note | Cal | Pro | Fat | $ | $/kCal | $/100g pro | % carb | % pro | % fat | Weight | Volume | Tags | X |\n")
        (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n"))
      the-new-buffer)))

(defun otdb-recipe-calc-special-command ()
  (interactive)
  ;; (setq otdb-recipe-need-warning-partial nil)
  (let ((the-new-buffer (otdb-recipe-create-temporary-buffer)))
    (otdb-recipe-calc-special (cic:org-table-to-lisp-no-separators) the-new-buffer)
    (with-current-buffer the-new-buffer
      (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
      (insert "  |          |            |      |     |     |     |   |        |            |        |       |       |        |        |      |   |\n")
      (insert "  |----------+------------+------+-----+-----+-----+---+--------+------------+--------+-------+-------+--------+--------+------|---|\n")
      (insert "  #+TBLEL: otdb-recipe-calc-recipe\n")
      (forward-line -2)
      (org-table-align)
      (beginning-of-line)
      (tblel-eval)
      (goto-char (point-min)))
    (switch-to-buffer the-new-buffer)))

(defun otdb-recipe-calc-special (lisp-table current-temporary-buffer &optional quantity)
  (let ((current-collection-name)
        (char-columns (otdb-table-parse-char-columns lisp-table))
        lisp-row-quantity
        count)
    (unless quantity
      (setq quantity 1))
    ;; find all the current rows
    (dolist (lisp-row (butlast (cdr lisp-table)))
      (let ((recipe-location (otdb-recipe-find (elt lisp-row 1))))
        (unless (or
                 (otdb-table-check-invalid-current-row-lisp lisp-row otdb-recipe-column-mark char-columns)
                 ;; TODO: this not is confusing
                 (and otdb-recipe-column-mark (not (otdb-table-check-current-row-lisp lisp-row otdb-recipe-column-mark char-columns)))
                 ;; XXXX: allow continuing if thing is a collection of recipe that does not match
                 ;;       items only
                 (and otdb-recipe-item-pattern
                      (not (otdb-recipe-find (elt lisp-row 1)))
                      (not (string-match otdb-recipe-item-pattern (elt lisp-row 1))))
                 (and otdb-recipe-item-tags
                      (not (otdb-recipe-find (elt lisp-row 1)))
                      (not (otdb-table-tag-pattern-match otdb-recipe-item-tags (elt lisp-row 14)))))
          (unless recipe-location
            (with-current-buffer current-temporary-buffer
              (if (/= quantity 1)
                  (progn
                    (setq lisp-row-quantity nil)
                    (setq count 0)
                    (dolist (e lisp-row)
                      (if (member count '(0 3 4 5 6 12 13))
                          (setq lisp-row-quantity (append lisp-row-quantity (list (otdb-recipe-multiply-preserve e quantity))))
                        (setq lisp-row-quantity  (append lisp-row-quantity (list e))))
                      (setq count (+ count 1)))
                    (insert (concat "  | " (mapconcat 'identity lisp-row-quantity " | ") "\n")))
                (insert (concat "  | " (mapconcat 'identity lisp-row " | ") "\n")))))
          (when (and recipe-location (not (otdb-table-check-invalid-current-row-lisp lisp-row otdb-recipe-column-mark char-columns)))
            (save-excursion
              ;; find the recipe
              (with-current-file-min (car recipe-location)
                ;; TODO: open everything up?
                (goto-char (cadr recipe-location))
                (cic:org-find-table)
                ;; advance to table
                (otdb-recipe-calc-special (cic:org-table-to-lisp-no-separators) current-temporary-buffer (* quantity (otdb-table-number (elt lisp-row 0))))))))))))

(defun otdb-recipe-multiply-preserve (thestring quantity)
  (if (otdb-table-number thestring)
      (let* ((thequantity (otdb-table-number thestring))
             (theunit (otdb-table-unit thestring)))
        (concat (number-to-string (* quantity thequantity)) theunit))
    thestring))

(provide 'otdb-recipe)
