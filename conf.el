;; el-indent/conf.el --- indent rules for conf-mode

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
;; (require 'el-indent/conf)
;; (add-hook 'conf-mode-hook 'el-indent-set-conf)

(require 'el-indent/el-indent nil t)
(require 'el-kit/list nil t)

;; Standard mode
(setq el-indent-exp-conf
	(el-indent-build-exps (let* ((main (list t))
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
(setq el-indent-exp-conf-value
	(el-indent-build-exps
		(list
			(list "[^\\\\]$")
			(list -1)
			(list nil))))

(defun el-indent-conf ()
	"Indent current line as configuration source text."
	(el-indent-line (lambda ()
			(if (looking-back "\\\\\n[ \t]*")
					el-indent-exp-conf-value el-indent-exp-conf))))

;;;###autoload
(defun el-indent-set-conf ()
	"Set `indent-line-function' to `el-indent-conf'."
	(setq indent-line-function 'el-indent-conf))

(provide 'el-indent/conf)
