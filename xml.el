;; el-indent/xml.el --- indent rules for xml-mode

;; Author:	Mariusz Nowak <medikoo+el-indent@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <medikoo+el-indent@medikoo.com>

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
;; (require 'el-indent/xml)
;; (add-hook 'nxml-mode-hook 'el-indent-set-xml)

(require 'el-indent/el-indent nil t)
(require 'el-kit/list nil t)

(setq el-indent-exp-xml
	(el-indent-build-exps (let* ((main (list t))
				(elo (list
						(list "/>"  ">")
						(list -1    0)
						(list main  main)))
				(elc (list
						(list ">")
						(list 0)
						(list main))))

			(el-kit-list-set main (list
					(list "<[^/!]"  "/>"  ">" "</"  "<!--"  "-->" "<!\\[CDATA\\[" "\]\\]>")
					(list 1         -1    0   -1    1       -1    1               -1)
					(list elo       t     t   elc   t       t     t               t))))))

(defun el-indent-xml ()
	"Indent current line as xml source text."
	(interactive)
	(el-indent-line (lambda () el-indent-exp-xml)))

;;;###autoload
(defun el-indent-set-xml ()
	"Set `indent-line-function' to `el-indent-xml'."
	(setq indent-line-function 'el-indent-xml))


(provide 'el-indent/xml)
