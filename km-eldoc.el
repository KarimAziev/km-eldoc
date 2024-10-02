;;; km-eldoc.el --- Miscellaneous utils for eldoc -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-eldoc
;; Version: 0.1.0
;; Keywords: help
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Miscellaneous utils for eldoc

;;; Code:

(defcustom km-eldoc-filter-args-functions '(km-eldoc--remove-html-entities
                                            km-eldoc--propertize-asterixes)
  "List of functions to filter and transform documentation strings.

A list of functions to filter and transform argument strings for
`km-eldoc'. Each function will be called in the temp buffer without arguments."
  :group 'km-eldoc
  :type 'hook)


(defun km-eldoc--remove-html-entities ()
  "Replace all occurrences of \"&nbsp;\" with an empty string."
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward "&nbsp;" nil t 1)
      (replace-match ""))))


(defun km-eldoc--propertize-asterixes ()
  "Propertize text between double asterisks with a bold face."
  (save-excursion
    (goto-char (point-max))
    (let ((regex "\\([*][*]\\)\\([a-z0-9_-][^*]+\\)\\([*][*]\\)"))
      (while (re-search-backward regex nil t 1)
        (let ((str (match-string 2))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (delete-region beg end)
          (add-face-text-property 0 (length str) 'bold nil str)
          (insert str))))))

(defun km-eldoc--map-nested-list-of-strings (fn list-of-strings)
  "Apply FN to each string in LIST-OF-STRINGS, handling nested lists and vectors.

Argument FN is a function to apply to each string in the nested list.

Argument LIST-OF-STRINGS is a nested list or vector of strings to process."
  (pcase list-of-strings
    ((pred stringp)
     (funcall fn list-of-strings))
    ((pred (proper-list-p))
     (mapcar (apply-partially #'km-eldoc--map-nested-list-of-strings fn)
             list-of-strings))
    ((pred (vectorp))
     (apply #'vector
            (mapcar (apply-partially #'km-eldoc--map-nested-list-of-strings fn)
                    list-of-strings)))
    (_ list-of-strings)))

(defun km-eldoc--transform-doc-buffer-arg (str)
  "Transform documentation string STR using filter functions if available.

Argument STR is the documentation string to be transformed."
  (if km-eldoc-filter-args-functions
      (with-temp-buffer (insert str)
                        (run-hooks 'km-eldoc-filter-args-functions)
                        (buffer-string))
    str))

(defun km-eldoc--transform-doc-buffer-args (args)
  "Transform each string in the nested list ARGS using a specific function.

Argument ARGS is a nested list or vector of strings to process."
  (km-eldoc--map-nested-list-of-strings
   #'km-eldoc--transform-doc-buffer-arg
   args))

(defun km-eldoc-prettify-doc-buffer-args (args)
  "Return a list of prettified documentation strings from ARGS.

Argument ARGS is the argument list passed to `eldoc--format-doc-buffer'.

This function processes the documentation strings by replacing
\"&nbsp;\" with spaces.

Argument ARGS is a list containing documentation strings to be prettified.

The result is a list that can be applied as:
\\=(apply #\\='eldoc--format-doc-buffer result)

Usage:

\\=(advice-add \\='eldoc--format-doc-buffer :filter-args
            #\\='km-eldoc-prettify-doc-buffer-args)"
  (let ((docs (car args)))
    (list (km-eldoc--transform-doc-buffer-args docs))))


(provide 'km-eldoc)
;;; km-eldoc.el ends here