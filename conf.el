;; my-indent/conf.el --- indent rules for conf-mode

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

; Standard mode
; Start
(let ((elc
			(my-indent-build-exp
				(list
					(list ">")
					(list 0)
					(list (setq my-indent-exp-conf-start (list t)))))))

	(my-list-set my-indent-exp-conf-start
		(my-indent-build-exp
			(list
				(list "</")
				(list -1)
				(list elc)))))

; In
(let ((elo
			(my-indent-build-exp
				(list
					(list ">")
					(list 0)
					(list (setq my-indent-exp-conf (list t)))))))

	(my-list-set my-indent-exp-conf
		(my-indent-build-exp
			(list
				(list "<[^/!]"  ">" "</"  "\\\\$")
				(list 1         0   -1    1)
				(list elo       t   elo   nil)))))

; Value mode
; Start
(setq my-indent-exp-conf-value-start nil)

; In
(setq my-indent-exp-conf-value
	(my-indent-build-exp
		(list
			(list "[^\\\\]$")
			(list -1)
			(list nil))))

(defun my-indent-conf ()
	(my-indent-line (lambda ()
			(if (looking-back "\\\\\n[ \t]*")
				(list my-indent-exp-conf-value-start my-indent-exp-conf-value)
				(list my-indent-exp-conf-start my-indent-exp-conf)))))

(defun my-indent-set-conf ()
	(setq indent-line-function 'my-indent-conf))

(provide 'my-indent/conf)