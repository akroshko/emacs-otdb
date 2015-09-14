;;; otdb-sample-config.el --- A sample config file I used for
;;; testing this library in isolation from my main Emacs installation.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Apr 10, 2015
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

(defvar otdb-root
  (when load-file-name
    (file-name-directory load-file-name))
  "The filename to use.")

(defvar otdb-gear-database
  (cic:join-paths otdb-root "gear/gear-database.org")
  "The main location of the gear database.")

(defvar otdb-gear-database-headline
  "Gear")

(defvar otdb-gear-collection-files
  (list (cic:join-paths otdb-root "gear/gear-collections.org")))

(defvar otdb-gear-message-buffer
  "*Backpacking messages*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normal recipes

(defvar otdb-recipe-normal-database
  (cic:join-paths otdb-root "recipes/food-database.org")
  "The main location of the database, should be re-re-factored
out of code.and use a list.")

(defvar otdb-recipe-normal-database-headline
  "Ingredients")

(defvar otdb-recipe-normal-agenda
  (cic:join-paths otdb-root "recipes/sample-agenda.org")
  "The main location of the agenda, should be re-re-factored out
  of code.")

(defvar otdb-recipe-normal-shopping
  (cic:join-paths otdb-root "recipes/groceries.org")
  "The grocery list used for mobile use.")

(defvar otdb-recipe-normal-price-check-headline
  "Price checks"
  "The headline for price checks to make.")

(defvar otdb-recipe-normal-files
  (mapcar (lambda (f)
            (cic:join-paths otdb-root f))
          '("recipes/recipes.org"
            "recipes/meals.org"
            "recipes/slowcooker-recipes.org")))

(defvar otdb-recipe-normal-message-buffer
  "*Recipe messages*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backpacking recipes

;; TODO need to decide how to have more databases
(defvar otdb-recipe-backpacking-database
  (cic:join-paths otdb-root "recipes/food-database.org")
  "The main location of the database, should be re-re-factored
out of code.and use a list.")

(defvar otdb-recipe-backpacking-database-headline
  "Ingredients")

(defvar otdb-recipe-backpacking-agenda
  (cic:join-paths otdb-root "backpacking-recipes/backpacking-agenda.org")
  "The main location of the agenda, should be re-re-factored out
  of code.")

(defvar otdb-recipe-backpacking-shopping
  (cic:join-paths otdb-root "backpacking-recipes/backpacking-groceries.org")
  "The grocery list used for mobile use.")

(defvar otdb-recipe-backpacking-price-check-headline
  "Price checks"
  "The headline for price checks to make.")

(defvar otdb-recipe-backpacking-files
  (mapcar (lambda (f)
            (cic:join-paths otdb-root f))
          '("backpacking-recipes/backpacking-meals.org"
            "backpacking-recipes/backpacking-recipes.org")))

(defvar otdb-recipe-backpacking-message-buffer
  "*Backpacking recipe messages*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables

(defvar otdb-recipe-database
  otdb-recipe-normal-database)

(defvar otdb-recipe-database-headline
  otdb-recipe-normal-database-headline)

(defvar otdb-recipe-agenda
  otdb-recipe-normal-agenda)

(defvar otdb-recipe-shopping
  otdb-recipe-normal-shopping)

(defvar otdb-recipe-price-check-headline
  otdb-recipe-normal-price-check-headline)

(defvar otdb-recipe-files
  otdb-recipe-normal-files)

(defvar otdb-recipe-message-buffer
  otdb-recipe-normal-message-buffer)

(defun otdb-table-detect ()
  "Users should modify this file to meet their file structure.
May eventually be generalized a little better."
  (let ((current-directory (file-name-base (directory-file-name default-directory)))
        (current-filename (buffer-file-name)))
    (cond ((or
            (equal current-directory "recipes")
            (equal current-filename "food-database.org")
            (equal current-directory "backpacking-recipes"))
           'recipe)
          ((or
            (equal current-directory "gear"))
           'backpacking)
          (t
           nil))))

(provide 'otdb-sample-config)
