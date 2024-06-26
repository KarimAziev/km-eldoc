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


(defun km-eldoc--replace-nbsp (args)
  "Convert nested lists of strings by replacing \"&nbsp;\" with a space.

Argument ARGS is a list or string to be processed and converted."
  (pcase args
    ((pred stringp)
     (replace-regexp-in-string "&nbsp;" "  " args))
    ((pred (proper-list-p))
     (mapcar #'km-eldoc--replace-nbsp args))
    (_ args)))

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
    (list (mapcar #'km-eldoc--replace-nbsp
                  docs))))


(provide 'km-eldoc)
;;; km-eldoc.el ends here