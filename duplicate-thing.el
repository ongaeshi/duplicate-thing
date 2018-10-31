;;; duplicate-thing.el --- Duplicate current line & selection

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: convenience command duplicate line selection
;; URL: https://github.com/ongaeshi/duplicate-thing
;; Version: 0.2

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
;; 1. Duplicate current line.
;; 2. Duplicate a selection when selection is active.
;; 3. Only C-u, replicate, comment out the range.
;; 4. Numerical prefix is specified as 'C-u 5': do multiple times repeatedly.

;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)

;;; Code:

(defun duplicate-thing-line-start-after-forward-line-p ()
  "Return 't if the position is beginning of line after foward-line."
  (forward-line)
  (= 0 (current-column)))

(defun duplicate-thing-select-current-line ()
  "Select current line."
  (let (start end)
    (beginning-of-line)
    (setq start (point))
    (unless (duplicate-thing-line-start-after-forward-line-p) (newline))
    (setq end (point))
    (setq deactivate-mark nil)
    (set-mark start)))

(defun duplicate-thing-expand-selection ()
  "Expand selection to contain whole lines."
  (let ((start (region-beginning))
        (end   (region-end)))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (goto-char end)
    (unless (= 0 (current-column))
      (unless (duplicate-thing-line-start-after-forward-line-p)
        (newline)))
    (setq end (point))
    (setq deactivate-mark nil)
    (set-mark start)))

(defun duplicate-thing-at (p text n)
  "Duplicate TEXT N times at P."
  (dotimes (i (or n 1)) (insert text))
  (set-mark p)
  (setq deactivate-mark nil))

;;;###autoload
(defun duplicate-thing (n)
  "Duplicate line or region N times.
If it has active mark, it will expand the selection and duplicate it.
If it doesn't have active mark, it will select current line and duplicate it."
  (interactive "P")
  (if mark-active
      (duplicate-thing-expand-selection)
    (duplicate-thing-select-current-line))
  (let (p1 p2 len text with-comment-out)
    (setq p1   (region-beginning)
          p2   (region-end)
          len  (- p2 p1)
          text (buffer-substring p1 p2)
          with-comment-out (consp n))
    (if with-comment-out
        (progn
          (comment-region p1 p2)
          (duplicate-thing-at (point) text 1))
      (duplicate-thing-at p2 text n)))
  (setq transient-mark-mode (cons 'only t)))

(provide 'duplicate-thing)
;;; duplicate-thing.el ends here
