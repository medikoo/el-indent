;; el-indent/lisp.el --- indent rules for any lisp mode

;; Author:	Mariusz Nowak <mariusz+el-indent@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+el-indent@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.	 See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary
;;
;; Usage:
;;
;; (require 'el-indent/lisp)
;; (add-hook 'lisp-mode-hook 'el-indent-set-lisp)
;;
;; and for other lisp modes:
;;
;; (add-hook 'emacs-lisp-mode-hook 'el-indent-set-lisp)
;; (add-hook 'scheme-mode-hook 'el-indent-set-lisp)
;; (add-hook 'clojure-lisp-mode-hook 'el-indent-set-lisp)

(require 'el-indent/el-indent nil t)
(require 'el-kit/list nil t)

(setq el-indent-exp-lisp
	(el-indent-build-exps (let* ((main (list t))
				(str (list
						(list "\\\\." "\"")
						(list 0       0)
						(list t       main))))

			(el-kit-list-set main (list
					(list "\\\\." "\("  "\)"  "\""  ";")
					(list 0       1     -1    0     0)
					(list t       t     t     str   nil))))))

(defun el-indent-lisp ()
	"Indent current line as lisp source text."
	(el-indent-line (lambda () el-indent-exp-lisp)))

;;;###autoload
(defun el-indent-set-lisp ()
	"Set `indent-line-function' to `el-indent-lisp'."
	(setq indent-line-function 'el-indent-lisp))

(provide 'el-indent/lisp)
