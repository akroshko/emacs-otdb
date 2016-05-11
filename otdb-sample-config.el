;;; otdb-sample-config.el --- A sample config file I used for
;;; testing this library in isolation from my main Emacs installation.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Apr 10, 2015
;; Version: 20160511
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

;; TODO use alists here

;; TODO: make sure I can use lists
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

(defvar otdb-recipe-normal-alist
  (list
   ;; The main location of the database
   (cons 'otdb-recipe-database (cic:join-paths otdb-root "recipes/food-database.org"))
   (cons 'otdb-recipe-database-headline "Ingredients")
   ;; The main location of the agenda, should be re-re-factored out of code.
   (cons 'otdb-recipe-agenda (cic:join-paths otdb-root "recipes/sample-agenda.org"))
   ;; TODO: issue with this in otdb-recipe-agenda-push-groceries
   ;; The grocery list used for mobile use.
   (cons 'otdb-recipe-shopping (cic:join-paths otdb-root "recipes/groceries.org"))
   ;; TODO: issue with this in otdb-recipe-agenda-push-groceries
   ;; The headline for price checks to make.
   (cons 'otdb-recipe-price-check-headline "Price checks")
   (cons 'otdb-recipe-files (mapcar (lambda (f)
                                      (cic:join-paths otdb-root f))
                                    '("recipes/recipes.org"
                                      "recipes/meals.org"
                                      "recipes/slowcooker-recipes.org")))
   (cons 'otdb-recipe-message-buffer "*Recipe messages*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backpacking recipes

(defvar otdb-recipe-backpacking-alist
  (list
   ;; The main location of the database
   (cons 'otdb-recipe-database (cic:join-paths otdb-root "recipes/food-database.org"))
   (cons 'otdb-recipe-database-headline "Ingredients")
   ;; The main location of the agenda, should be re-re-factored out of code.
   (cons 'otdb-recipe-agenda (cic:join-paths otdb-root "backpacking-recipes/backpacking-agenda.org"))
   ;; TODO: issue with this in otdb-recipe-agenda-push-groceries
   ;; The grocery list used for mobile use.
   (cons 'otdb-recipe-shopping (cic:join-paths otdb-root "backpacking-recipes/backpacking-groceries.org"))
   ;; TODO: issue with this in otdb-recipe-agenda-push-groceries
   ;; The headline for price checks to make.
   (cons 'otdb-recipe-price-check-headline "Price checks")
   (cons 'otdb-recipe-files (mapcar (lambda (f)
                                      (cic:join-paths otdb-root f))
                                    '("backpacking-recipes/backpacking-meals.org"
                                      "backpacking-recipes/backpacking-recipes.org")))
   (cons 'otdb-recipe-message-buffer "*Backpacking recipe messages*")))

(provide 'otdb-sample-config)
