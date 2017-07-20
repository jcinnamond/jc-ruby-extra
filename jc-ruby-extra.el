;;; jc-ruby-extra.el --- helper functions for ruby

;; Copyright 2016 John Cinnamond

;; Author: John Cinnamond
;; Version: 1.0.0

;;; Commentary:
;;
;; This mode adds two functions:
;;
;; jc-align-hash - aligns elements in a ruby hash (using both => and : styles)
;; jc-ruby-instance-variables - adds instance variables and attribute readers from a param list

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
;; Alignment for hashes
(defun jc-align-hash-old-style-p (string)
  (if (string-match "=>" string)
      't
    nil))

(defun jc-align-hash-old-style ()
  (message "aligning old")
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>")
  (message "aligned old"))

(defun jc-align-hash-new-style ()
  (align-regexp (region-beginning) (region-end) ":\\(\\s-*\\)" 1 1 nil)
  (message "aligned new"))

(defun jc-align-hash ()
  (interactive)
  (if (region-active-p)
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
	(if (jc-align-hash-old-style-p selection)
	    (jc-align-hash-old-style)
	  (jc-align-hash-new-style)))
    (message "jc-align-hash requires an active mark")))

(defun jc-align-requests ()
  (interactive)
  (save-excursion
    (ruby-beginning-of-block)
    (let ((beg (point)))
      (ruby-forward-sexp)
      (align-regexp beg (point) "\\(\\s-*\\)>>"))))

;; Create instance variables from method params
(defun jc-ruby-instance-variables ()
  "Creates an assignment to an instance variables for each method param"
  (interactive)
  (save-excursion
    (let ((params (jc-ruby-extract-params)))
      (dolist (p params)
	(insert (concat "@" p " = " p))
	(newline-and-indent))
      (delete-blank-lines)
      (jc--insert-attr-reader params))))

(defun jc--insert-attr-reader (params)
  (ruby-beginning-of-block)
  (newline-and-indent)
  (forward-line -1)
  (insert "attr_reader :")
  (insert (s-join ", :" params))
  (newline-and-indent))

(defun jc-ruby-extract-params ()
  (jc-ruby-param-seq (jc-ruby-param-string (jc-ruby-method-header))))

(defun jc-ruby-param-seq (str)
  (let (params)
    (dolist (s (split-string str ","))
      (push (jc-ruby-cleanup-param s) params))
    (reverse params)))

(defun jc-ruby-cleanup-param (str)
  (let ((param (s-trim str)))
    (if (string-suffix-p ":" param)
	(substring param 0 (- (length param) 1))
      param)))

(defun jc-ruby-param-string (str)
  (string-match "(\\([^)]*\\)" str)
  (if (match-beginning 1)
      (substring str (match-beginning 1) (match-end 1))
    ""))

(defun jc-ruby-method-header ()
  (save-excursion
    (ruby-beginning-of-defun)
    (let ((p (point)))
      (end-of-line)
      (buffer-substring-no-properties p (point)))))

(define-minor-mode jc-ruby-extra
  "Provide helper functions for ruby"
  nil ; initially disabled
  " jc-ruby-extra")

(provide 'jc-ruby-extra)
;;; jc-ruby-extra.el ends here
