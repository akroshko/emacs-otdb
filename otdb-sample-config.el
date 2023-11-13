;;; otdb-test-config.el --- Create a database using an org-mode table and
;;; calculate similar to a spreadsheet.
;;
;; Copyright (C) 2015-2023, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <boreal6502@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20231111
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
;; A configuration used for testing that can be freely moved anywhere.

(put 'org-image-actual-width 'safe-local-variable #'numberp)

(add-to-list 'load-path (expand-file-name "."))
(require 'otdb-utility-functions)
(require 'otdb-table)
(require 'otdb-recipe)
(require 'otdb-gear)
(setq otdb-recipe-normal-alist `((otdb-recipe-database ,(expand-file-name "./sample-recipes/food-database-test.org"))
                                 (otdb-recipe-database-headline . "Ingredients")
                                 (otdb-recipe-files ,(expand-file-name "./sample-recipes/recipe-test.org"))
                                 (otdb-recipe-message-buffer . "*Recipe messages*")))
(setq otdb-gear-normal-alist `((otdb-gear-database ,(expand-file-name "./sample-gear/gear-database-sample.org"))
                               (otdb-gear-database-headline . "Gear")
                               (otdb-gear-files ,(expand-file-name "./sample-gear/gear-collections-sample.org"))
                               (otdb-gear-message-buffer . "*Gear messages*")))
