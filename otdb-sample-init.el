;;; otdb-sample-init.el --- A sample init file I used for testing
;;; this library in isolation from my main Emacs installation.
;;
;; Copyright (C) 2015-2023, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <boreal6502@gmail.com>
;; Created: Fri Apr 10, 2015
;; Version: 20230801
;; URL:
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
;; Standard Emacs features, to be documented specificly later.  Also
;; requires features from https://github.com/akroshko/cic-emacs-common,
;; TODO: no,fix this
;; using (require 'cic-emacs-common) is sufficient.
;;
;;; Code:

;; XXXX: these should produce errors when unable to load, or else why use
;; this file?
(require 'cl)
(require 'org-table)

(add-to-list 'load-path ".")
;; TODO: replace this
(requiring-package (cic-emacs-common-aliases))
(requiring-package (cic-emacs-patterns))
(requiring-package (cic-emacs-macros))
(requiring-package (cic-emacs-passwords))
(requiring-package (cic-emacs-strings))
(requiring-package (tblel))
;; XXXX: uncomment to use my other keys
;; (cic-emacs-keys-non-term-mode t)
;; (cic-emacs-keys-org-mode t)
;; TODO put these in an accessible place
(requiring-package (otdb-sample-config))
(requiring-package (otdb-table))
(requiring-package (otdb-recipe))
(requiring-package (otdb-gear))
