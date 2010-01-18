;; my-indent.el --- indent functions configurator

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

(require 'my/regexp)
(require 'my/list)

(defun my-indent-region (start end)
	; (message "START REG IND %S %S" start end)
	(save-excursion
		(goto-char start)
		(while (< (point) (min end (point-max)))
			; (message "POINT %S" (point))
			(if (not (and (bolp) (eolp)))
				(progn (end-of-line)
					(funcall indent-line-function)))
			(forward-line 1))))

(setq indent-region-function 'my-indent-region)

(defun my-indent-start-calculate (exps bound)
	; (message "START CALC, BOUND: %S" bound)
	(if (and exps (< (point) bound))
		(if (looking-at (car exps))
			(progn
				(goto-char (match-end 0))
				; (message "MOVED TO: %d" (point))
				(let ((group 1))
					(while (not (match-beginning group))
						(setq group (+ 1 group)))
					(+
						(* tab-width (nth (- group 1) (second exps)))
						(my-indent-start-calculate (nth (- group 1) (third exps)) bound))))
			0)
		0))

(defun my-indent-calculate (exps bound)
	; (message "PROCESS INDENT: point: %d, bound: %d, exps: %S" (point) bound exps)
	(if (and exps (re-search-forward (car exps) bound t))
		(let ((group 1))
			(while (not (match-beginning group))
				(setq group (+ 1 group)))
			; (message "GOT EXP: point: %d, exp: %S" (point) group)
			(+ (* tab-width (nth (- group 1) (second exps))) (my-indent-calculate (nth (- group 1) (third exps)) bound)))
		0))

(defun my-indent-set-and-calculate (exp-getter)
	(save-excursion
		(let ((bound (point)) exps current-column)
			(back-to-indentation)
			; (message "START THIS LINE, POINT: %d" (point))
			(max 0 (+ (save-excursion (my-indent-start-calculate (car (funcall exp-getter)) bound))
					(if (re-search-backward "^[ \t]*+[^ \t\n]" nil t)
						(progn
							(back-to-indentation)
							(setq bound (save-excursion
									(or
										(and (search-forward "\n") (match-beginning 0))
										(point-max))))
							(setq exps (funcall exp-getter))
							(setq current-column (current-column))
							; (message "IGNORE START PRE LINE: %d" (point))
							(my-indent-start-calculate (car exps) bound)
							; (message "ADD IN PRE LINE: %d %d" (point) current-column)
							(+ current-column
								(my-indent-calculate (second exps) bound)))
						0))))))

(defun my-indent-line (exp-getter)
	; (message "%S" (current-column))
	(let ((offset (- (current-column) (current-indentation))))
		; (message "--- START INDENT ---")
		(indent-line-to (my-indent-set-and-calculate exp-getter))
		(if (> offset 0) (forward-char offset))))

(defun my-indent-build-exp (list)
	(setcar list (my-regexp-group (car list)))
	(my-list-replace (third list) t list)
	list)

(provide 'my-indent/my-indent)