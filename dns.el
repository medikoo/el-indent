;; el-indent/dns.el --- indent rules for dns-mode

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
;; (require 'el-indent/dns)
;; (add-hook 'dns-mode-hook 'el-indent-set-dns)

(require 'el-indent/el-indent nil t)

(setq el-indent-exp-dns
	(el-indent-build-exps
		(list
			(list "\("  "\)")
			(list 1     -1)
			(list t     t))))

(defun el-indent-dns ()
	"Indent current line as dns zone source text."
	(el-indent-line (lambda () el-indent-exp-dns)))

;;;###autoload
(defun el-indent-set-dns ()
	"Set `indent-line-function' to `el-indent-dns'."
	(setq indent-line-function 'el-indent-dns))

(provide 'el-indent/dns)
