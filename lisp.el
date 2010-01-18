;; my-indent/lisp.el --- indent rules for lisp mode

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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'my-indent/my-indent)
(require 'my/list)

(setq my-indent-exp-lisp-start
	(my-indent-build-exp
		(list
			(list "\)")
			(list -1)
			(list t))))

(let ((exp-str
			(my-indent-build-exp
				(list
					(list "\\\\." "\"")
					(list 0       0)
					(list t       (setq my-indent-exp-lisp (list t)))))))

	(my-list-set my-indent-exp-lisp
		(my-indent-build-exp
			(list
				(list "\\\\." "\("  "\)"  "\""    ";")
				(list 0       1     -1    0       0)
				(list t       t     t     exp-str nil)))))

(defun my-indent-lisp()
	(my-indent-line (lambda () (list my-indent-exp-lisp-start my-indent-exp-lisp))))

(defun my-indent-set-lisp ()
	(setq indent-line-function 'my-indent-lisp))

(provide 'my-indent/lisp)
