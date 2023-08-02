;;; otdb-utility-functions.el --- Some utility functions imported from elsewhere.
;;
;; Copyright (C) 2015-2023, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <boreal6502@gmail.com>
;; Created: Tue Aug 1, 2023
;; Version: 20230801
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
;; These are utility functions imported from unreleased code of mine.

(defmacro with-current-buffer-min (buffer-or-name &rest body)
  "Like with-current-buffer but always go to point min."
  `(save-excursion
     (set-buffer ,buffer-or-name)
     (goto-char (point-min))
     ,@body))

(defun s-trim-full (str)
  "Trim all leading and trailing whitespace from STR.  Does this
for every line."
  (save-match-data
    (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                         str)
      (setq str (replace-match "" t t str)))
    str))

(defun s-trim-full-no-properties (str)
  "Like s-trim-full but remove text properties."
  (substring-no-properties (s-trim-full str)))

(defun cic:org-table-to-lisp-no-separators ()
  "Convert the org-table at point to Emacs Lisp representation
and eliminate seperators."
  (cl-remove-if-not (lambda (x) (if (eq x 'hline) nil x)) (org-table-to-lisp)))

(defun cic:get-headline-text (headline-line)
  "Get only the clean headline text from HEADLINE-LINE
representing the text of a line the headline is on."
  (let (headline-text)
    (save-match-data
      (when (string-match cic:headline-regexp headline-line)
        (setq headline-text (match-string 1 headline-line))))))

(defun cic:get-current-line ()
  "Get the current line as a string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defconst cic:headline-regexp
  "^\\*+ \\(.*\\)")

;; XXXX: set filename filter if not already set, used for filenames in
;; special formats that may be declared late
(unless (fboundp 'with-filename-filter)
  (fset 'with-filename-filter 'identity))

;; TODO want to allow files with table-less headlines
(defmacro do-org-tables (filename table-name table &rest body)
  "Iterate over tables in FILENAME. TABLE-NAME holds the table
name itself with TABLE containing a lisp representation of the
table.  While iterating might position cursor in first column and
first row of table.  Use save-excursion while moving in the body
of this macro, just in case."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ;; TODO figure out best layout for file and best way to do this
     (let ((keep-going t))
       (while keep-going
         ;; next headline same level
         (save-match-data
           (when (string-match cic:headline-regexp (cic:get-current-line))
             (setq ,table-name (match-string 1 (cic:get-current-line)))))
         (save-excursion
           (forward-line 1)
           (setq ,table nil)
           (while (not (or (and (cic:org-headline-p (cic:get-current-line)) (= (org-outline-level) 1)) (org-at-table-p) (eobp)))
             (forward-line 1))
           (when (org-at-table-p)
             (setq ,table (cic:org-table-to-lisp-no-separators))))
         (when (and ,table-name ,table)
           ,@body)
         ;; see if we can keep going
         (setq keep-going (cic:org-check-last-heading-level-1))
         ;; if not end
         (when keep-going
           (org-forward-heading-same-level 1 nil))))))

(defun cic:org-headline-p (line-substring)
  "Test if LINE-SUBSTRING is an org-mode headline."
  (string-match-p cic:headline-regexp line-substring))

(defun cic:org-check-last-heading-level-1 ()
  "Check if we are at the last heading of level 1 in the curren buffer.
This is generally used to see if a loop over level 1 headings can
keep going."
  (let ((line-no (line-number-at-pos)))
    (save-excursion
      (org-forward-heading-same-level 1 nil)
      (if (equal line-no (line-number-at-pos))
          nil
        t))))

(defmacro with-current-file-transient (filename &rest body)
  "Execute BODY with FILENAME as buffer.  Close the file if it
does not already exist Uses with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let  ((current-buffer-transient t))
       (let* ((filtered-filename (with-filename-filter ,filename))
              (already-existing-buffer (get-file-buffer filtered-filename))
              current-file-buffer)
         (if already-existing-buffer
             (set-buffer already-existing-buffer)
           (progn
             (setq current-file-buffer (find-file-noselect filtered-filename))
             (set-buffer current-file-buffer)))
         (let ((the-return (progn
                             ,@body)))
           (unless already-existing-buffer
             (kill-buffer current-file-buffer))
           the-return)))))

(defmacro with-current-file-transient-min (filename &rest body)
  "Like with-current-file, but always go to point-min."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((current-buffer-transient t))
       (let* ((filtered-filename (with-filename-filter ,filename))
              (already-existing-buffer (get-file-buffer filtered-filename))
              current-file-buffer)
         (if already-existing-buffer
             (set-buffer already-existing-buffer)
           (progn
             (setq current-file-buffer (find-file-noselect filtered-filename))
             (set-buffer current-file-buffer)))
         (goto-char (point-min))
         (let ((the-return (progn
                             ,@body)))
           (unless already-existing-buffer
             (kill-buffer current-file-buffer))
           the-return)))))

(defun cic:org-find-table (&optional count)
  "Find either the first or COUNT table in BUFFER.  Go to the
last row of the table."
  (unless count
    (setq count 1))
  (dotimes (i count)
    (while (not (or (org-at-table-p) (eobp)))
      (forward-line 1))
    (when (< i (- count 1))
      (cic:org-table-last-row)
      (forward-line 2))))

(defun cic:ensure-list (object)
  "Put OBJECT into a list if it is not already a list."
  (unless (listp object)
    (setq object (list object)))
  object)

(defmacro with-current-file-transient-org-table (filename table-name &rest body)
  "Like with-current-file, but find TABLE-NAME."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((current-buffer-transient t))
       (let ((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
             (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))
         (set-buffer current-file-buffer)
         (goto-char (point-min))
         (when (re-search-forward (concat "^\* " ,table-name "$") nil t)
           (cic:org-find-table)
           (let ((the-return (progn
                               ,@body)))
             (unless already-existing-buffer
               (kill-buffer current-file-buffer))
             the-return))))))

(defun cic:get-list-duplicates (lst)
  "Get the duplicate items in list LST."
  (cl-set-difference lst (cl-remove-duplicates lst :test 'equal)))

(defun cic:org-table-assoc (lisp-table key-value &optional column equal-test)
  "Get row associated with KEY-VALUE in either the first column
or COLUMN column from LISP-TABLE (in Emacs Lisp with no
seperators format)."
  (unless column
    (setq column 1))
  (cic:assoc-nth column key-value lisp-table equal-test))

(defun cic:assoc-nth (nth-value key-value assoc-list &optional equal-test)
  "Get the NTH-VALUE item from the KEY-VALUE item in ASSOC-LIST
with an optional EQUAL-TEST that defaults to the Emacs Lisp
function equal."
  (unless equal-test
    (setq equal-test 'equal))
  (let (selected (zeron (- nth-value 1)))
    (dolist (item assoc-list)
      (when (funcall equal-test key-value (nth zeron item))
        (setq selected item)))
    selected))

(defun cic:full-string-p (thing-or-string)
  "Determine if something is nil or an empty string."
  (if (or (not thing-or-string) (equal (s-trim-full thing-or-string) ""))
      nil
    t))

;; TODO: make sure we can deal with seperators
(defmacro do-org-table-rows (filename table-name row &rest body)
  "Iterate over the rows of the table TABLE-NAME in FILENAME.
ROW contains the current row converted into elisp."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ;; TODO replace with org-headline-goto???
     ;; XXXX: complicated regexp deals with tags
     (when (re-search-forward (concat "^\* " ,table-name "\\([[:space:]]:.*\\)?$") nil t)
       (cic:org-find-table)
       (let ((keep-going t)
             (lisp-table (cic:org-table-to-lisp-no-separators))
             (row-count 0))
         (while keep-going
           (unless (string-match-p "|-+\+.*|" (cic:get-current-line))
             (setq ,row (nth row-count lisp-table))
             ,@body
             (setq row-count (1+ row-count)))
           (save-excursion
             ;; TODO need to catch error or whatever from this
             (forward-line 1)
             (unless (org-at-table-p)
               (setq keep-going nil)))
           (when keep-going
             (forward-line 1)))))))

(defun tblel-eval (&rest args)
  "Get the lisp table and run the appropriate function on it (several functions?)."
  ;; TODO: unwind protect to avoid nuking table
  ;; TODO: avoid back-to-heading/find-table and use a better methodology for tables
  (interactive)
  (when (tblel-p)
    (let (lisp-table
          original-lisp-table
          lisp-function
          new-lisp-table)
      ;; get elisp function to run
      (save-excursion
        (unless (tblel-line-p)
            (goto-char (org-table-end)))
        (beginning-of-line)
        ;; TODO: eventually get forms
        (let ((split-tblel-line (split-string (cic:get-current-line))))
          (setq lisp-function (substring-no-properties (elt split-tblel-line 1)))
          (setq lisp-function-args (cl-subseq split-tblel-line 2))))
      (save-excursion
        (when (tblel-line-p)
          (forward-line -1)
          (back-to-indentation))
        ;; TODO: just evaluating a single lisp function, want more and
        ;; want to check error before nuking current table
        (setq lisp-table          (copy-tree (org-table-to-lisp))
              original-lisp-table (copy-tree (cic:org-table-to-lisp-no-separators)))
        ;; XXXX: found it essential to send copy-tree of lisp-table to
        ;; function, stops many subtle bugs
        (if lisp-function-args
            (setq new-lisp-table (funcall (intern lisp-function) (copy-tree lisp-table) (copy-tree original-lisp-table) lisp-function-args))
          (setq new-lisp-table (funcall (intern lisp-function) (copy-tree lisp-table) (copy-tree original-lisp-table))))
        ;; XXXX: make sure nil does not erase table
        (when new-lisp-table
          ;; finally put it back if all is well
          (cic:org-table-elisp-replace original-lisp-table new-lisp-table)
          ;; TODO: option to avoid this?
          (org-table-align))))
    t))

(defun tblel-p ()
  "Find if we are at a location associated with an org-table
along with a #+TBLEL line."
  (or
   (tblel-line-p)
   (save-excursion
     (and
      (org-table-p)
      (progn
        (goto-char (org-table-end))
        (beginning-of-line)
        (looking-at " *#\\+TBLEL:"))))))

(defun tblel-line-p ()
  "Check if at a #+TBLEL line."
  (save-excursion
    (beginning-of-line)
    (looking-at " *#\\+TBLEL:")))

(defun cic:org-table-elisp-replace (elisp-table-original elisp-table-replacement)
  "Replace the original table at point ELISP-TABLE-ORIGINAL with
the ELISP-TABLE-REPLACEMENT.  Only uses expensive org-table-put
when values has changed.

Meant to be used programatically and behaviour is undefined if
there is not mutual correspondance between table at point,
ELISP-TABLE-ORIGINAL, and ELISP-TABLE-REPLACEMENT."
  ;; TODO: add some quick sanity checks here
  (when (org-at-table-p)
    (save-excursion
      (let ((nrows (length elisp-table-original))
            (ncols (length (car elisp-table-original))))
        ;; TODO: do better than straight imperative programming?
        (dotimes (i nrows)
          (dotimes (j ncols)
            (unless (string= (cic:elisp-array-string elisp-table-original i j)
                             (cic:elisp-array-string elisp-table-replacement i j))
              (org-table-put (1+ i) (1+ j) (cic:elisp-array-string elisp-table-replacement i j)))))))))

(defun cic:elisp-array-string (elisp-array i j)
  (let ((thestr (elt (elt elisp-array i) j)))
    (if (stringp thestr)
        (s-trim-full-no-properties thestr)
      "")))

(defun cic:string-to-float (str)
  "Convert STR to floating point number.  If STR is a
non-floating point number convert it to a floating point number.
If STR is already a floating point number then just return STR."
  (let (new-var)
    (when (stringp str)
      (setq str (string-to-number str)))
    (unless (floatp str)
      (setq str (float str)))
    str))

(defun cic:string-to-float-empty-zero (str)
  "Convert STR to float or return zero for a nil or empty
string."
  (if (cic:is-not-empty-string-nil str)
      (cic:string-to-float str)
    0))

(defun cic:is-not-empty-string-nil (str)
  "Check if STR is an empty string (no characters or all
whitespace) or a nil."
  (and str (not (string= (s-trim-full str) ""))))

(provide 'otdb-utility-functions)
