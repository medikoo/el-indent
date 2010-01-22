;; my-indent/lisp.el --- indent rules for any lisp mode

;; Author:	Mariusz Nowak <mariusz+emacs.my-indent@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs.my-indent@medikoo.com>

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
;; (require 'my-indent/lisp)
;; (add-hook 'lisp-mode-hook 'my-indent-set-lisp)
;;
;; and for other lisp modes:
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-indent-set-lisp)
;; (add-hook 'scheme-mode-hook 'my-indent-set-lisp)
;; (add-hook 'clojure-lisp-mode-hook 'my-indent-set-lisp)

(require 'my-indent/my-indent)
(require 'my/list)

(setq my-indent-exp-lisp
	(my-indent-build-exps (let* ((main (list t))
				(str (list
						(list "\\\\." "\"")
						(list 0       0)
						(list t       main))))

			(my-list-set main (list
					(list "\\\\." "\("  "\)"  "\""  ";")
					(list 0       1     -1    0     0)
					(list t       t     t     str   nil))))))

(defun my-indent-lisp()
	(my-indent-line (lambda () my-indent-exp-lisp)))

(defun my-indent-set-lisp ()
	(setq indent-line-function 'my-indent-lisp))

(provide 'my-indent/lisp)