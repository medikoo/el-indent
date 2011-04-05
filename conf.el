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
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary
;;
;; Usage:
;;
;; (require 'my-indent/conf)
;; (add-hook 'conf-mode-hook 'my-indent-set-conf)

(require 'my-indent/my-indent nil t)
(require 'el-kit/list nil t)

;; Standard mode
(setq my-indent-exp-conf
	(my-indent-build-exps (let* ((main (list t))
				(elo (list
						(list ">")
						(list 0)
						(list main))))

			(el-kit-list-set main
				(list
					(list "<[^/!]"  ">" "</"  "\\\\$")
					(list 1         0   -1    1)
					(list elo       t   elo   nil))))))

;; Value mode
(setq my-indent-exp-conf-value
	(my-indent-build-exps
		(list
			(list "[^\\\\]$")
			(list -1)
			(list nil))))

(defun my-indent-conf ()
	"Indent current line as configuration source text."
	(my-indent-line (lambda ()
			(if (looking-back "\\\\\n[ \t]*")
					my-indent-exp-conf-value my-indent-exp-conf))))

;;;###autoload
(defun my-indent-set-conf ()
	"Set `indent-line-function' to `my-indent-conf'."
	(setq indent-line-function 'my-indent-conf))

(provide 'my-indent/conf)
