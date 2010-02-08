;; my-indent/dns.el --- indent rules for dns-mode

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
;; (require 'my-indent/dns)
;; (add-hook 'dns-mode-hook 'my-indent-set-dns)

(require 'my-indent/my-indent nil t)

(setq my-indent-exp-dns
	(my-indent-build-exps
		(list
			(list "\("  "\)")
			(list 1     -1)
			(list t     t))))

(defun my-indent-dns ()
	"Indent current line as dns zone source text."
	(my-indent-line (lambda () my-indent-exp-dns)))

;;;###autoload
(defun my-indent-set-dns ()
	"Set `indent-line-function' to `my-indent-dns'."
	(setq indent-line-function 'my-indent-dns))

(provide 'my-indent/dns)