;; my-indent.el --- `indent-line-function' configurator

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

;;; Commentary:
;;
;; See README.

(require 'my/regexp)
(require 'my/list)

(defun my-indent-region (start end)
	"`indent-region-function' to be used with this tool.
	Default Emacs `indent-region-function' runs indent function with `point' at
	beginning of line. This one makes sure that it is run when `point' is at end
	of line. START and END are bound of region to be indented."
	(save-excursion
		(goto-char start)
		(while (< (point) (min end (point-max)))
			(if (not (and (bolp) (eolp)))
				(progn (end-of-line)
					(funcall indent-line-function)))
			(forward-line 1))))

(setq indent-region-function 'my-indent-region)

(defun my-indent-start-calculate (exps bound)
	"Scans beginning of line to calculate indent shift for a line.
	Used to calculate current line indent. In that case `point' is
	`back-to-indentation' result and bound is end of line.
	EXPS are briefly explained under `my-indent-calculate'."
	(if (and exps (< (point) bound))
		(if (looking-at (car exps))
			(progn
				(goto-char (match-end 0))
				(let ((group 1))
					(while (not (match-beginning group))
						(setq group (+ 1 group)))
					(+
						(* tab-width (nth (- group 1) (second exps)))
						(my-indent-start-calculate (nth (- group 1) (third exps)) bound))))
			0)
		0))

(defun my-indent-calculate (exps bound)
	"Scans (from `point' to BOUND) to calculate indent shift that should follow.
	Used to calculate current line indent by reading data of previous
	line. In that case `point' is placed after rules for beginning of line has
	finished their work and BOUND is set for end of line.
	EXPS is rule that tells what characters invoke indent shift.
	First element of EXPS is regexp that groups all strings that invoke indent
	shift.
	Second element of EXPS is list of indent shift magnitudes, usually 1, -1 or 0
	for according groups. Whichever string/group matches that group number is
	used. Third element is list of EXPS for content that follows found rule."
	(if (and exps (re-search-forward (car exps) bound t))
		(let ((group 1))
			(while (not (match-beginning group))
				(setq group (+ 1 group)))
			(+ (* tab-width (nth (- group 1) (second exps))) (my-indent-calculate
					(nth (- group 1) (third exps)) bound)))
		0))

(defun my-indent-set-and-calculate (exp-getter)
	"Indents given line accorting to current position and EXP-GETTER rules.
	EXP-GETTER is function that returns two indent rules. First one is for
	beginning of line and second is for rest of line."
	(save-excursion
		(let ((bound (point)) exps current-column)
			(back-to-indentation)
			(max 0 (+ (save-excursion
						(my-indent-start-calculate (car (funcall exp-getter)) bound))
					(if (re-search-backward "^[ \t]*+[^ \t\n]" nil t)
						(progn
							(back-to-indentation)
							(setq bound (save-excursion
									(or
										(and (search-forward "\n") (match-beginning 0))
										(point-max))))
							(setq exps (funcall exp-getter))
							(setq current-column (current-column))
							(my-indent-start-calculate (car exps) bound)
							(+ current-column
								(my-indent-calculate (second exps) bound)))
						0))))))

(defun my-indent-line (exp-getter)
	"Indent line function template.
	See EXP-GETTER description in `my-indent-set-and-calculate'."
	(let ((offset (- (current-column) (current-indentation))))
		(indent-line-to (my-indent-set-and-calculate exp-getter))
		(if (> offset 0) (forward-char offset))))

(defun my-indent-convert-rules (rules)
	"Convert indent RULES into expressions that can be processed by
	`my-indent-start-calculate' and `my-indent-calculate' functions.
	RULES should be in a form of list of three lists.
	1. First list are strings that invoke indents (e.g. { } [ ])
	or change rules (e.g. when we enter into string).
	2. Second list are indent magnitues e.g. 1 -1 0 for strings in first list.
	3. Third list are rules that should be used after presence of string found
	in first list. If same rules should apply then `t' may be used."
	(setcar rules (my-regexp-group (car rules)))
	(dolist (x (third rules))
		(if (and (listp x) (not (eq nil (car x))) (listp (car x)))
			(my-indent-convert-rules x)))
	(my-list-replace (third rules) t rules)
	rules)

(defun my-indent-calculate-start-rules (rules)
	"Calculates indent rules for line beginnings.
	RULES syntax is described at `my-indent-build-exps'."
	(let* ((index 0) indent exp
			(start-strings (list t))
			(start-indents (list t))
			(start-exps (list t))
			(start-rules (list start-strings start-indents start-exps))
			(strings (car rules)) (indents (second rules)) (exps (third rules)))
		(while (setq indent (nth index indents))
			(if (< indent 0)
				(progn (nconc start-strings (list (nth index strings)))
					(nconc start-indents (list indent))
					(setq exp (nth index exps))
					(if (and (not (eq t exp)) (not (eq nil exp)))
						(if (eq rules exp)
							(nconc start-exps (list t))
							(nconc start-exps (list (list
										(copy-list (car exp))
										(copy-list (second exp))
										(copy-list (third exp)))))
							(my-list-replace (third (car (last start-exps))) rules start-rules))
						(nconc start-exps (list exp)))))
			(setq index (+ index 1)))
		(if (<= (length start-strings) 1)
			nil
			(pop (car start-rules))
			(pop (second start-rules))
			(pop (third start-rules))
			start-rules)))

(defun my-indent-build-exps (rules)
	"Build indent expressions from given RULES.
	RULES should be in a form of list of three lists:
	1. First list is list of  strings that invoke indents (e.g. { } [ ]) or
	change rules (e.g. when we enter into comment).
	2. Second list are indent magnitudes e.g. 1 -1 0 each for each string in first list.
	3. Third list are rules that should be used after presence of string found
	in first list. If same rules should apply then `t' can be used."
	(setq rules (list (my-indent-calculate-start-rules rules) rules))
	(my-indent-convert-rules (car rules))
	(my-indent-convert-rules (second rules))
	rules)

(provide 'my-indent/my-indent)