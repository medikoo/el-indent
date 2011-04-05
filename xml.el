;; my-indent/xml.el --- indent rules for xml-mode

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
;; (require 'my-indent/xml)
;; (add-hook 'nxml-mode-hook 'my-indent-set-xml)

(require 'my-indent/my-indent nil t)
(require 'el-kit/list nil t)

(setq my-indent-exp-xml
	(my-indent-build-exps (let* ((main (list t))
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

(defun my-indent-xml ()
	"Indent current line as xml source text."
	(interactive)
	(my-indent-line (lambda () my-indent-exp-xml)))

;;;###autoload
(defun my-indent-set-xml ()
	"Set `indent-line-function' to `my-indent-xml'."
	(setq indent-line-function 'my-indent-xml))


(provide 'my-indent/xml)
