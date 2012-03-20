;;; duplicate-thing.el --- 

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Duplicate current line.
;; Duplicate a selection when selection is active.
;;
;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)
;; 

;;; Code:

(defun duplicate-thing ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-line)))

(defun duplicate-line ()
  (save-excursion
    (let (start)
    (beginning-of-line)
    (setq start (point))
    (next-line)
    (kill-ring-save start (point))
    (yank)
    )))

(defun duplicate-region ()
  (save-excursion
    (kill-ring-save (region-beginning) (region-end))
    (yank)))

(provide 'duplicate-thing)
;;; duplicate-thing.el ends here
