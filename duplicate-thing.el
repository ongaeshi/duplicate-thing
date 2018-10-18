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
;;
;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)

;;; Code:

(defun duplicate-thing-line-start-after-forward-line-p ()
  "Return 't if the position is beginning of line after foward-line."
  (forward-line)
  (= 0 (current-column)))

(defun duplicate-thing-expand-selection (p1 p2)
  "Expand selection to contain while lines.
Expand P1 to beginning of line and P2 to end of line (or more precisely)
the beginning of next line."
  (let (start end)
    (cond (mark-active
           (goto-char p1)
           (beginning-of-line)
           (setq start (point))
           (goto-char p2)
           (unless (= 0 (current-column))
             (unless (duplicate-thing-line-start-after-forward-line-p)
               (newline)))
           (setq end (point)))
          (t
           (beginning-of-line)
           (setq start (point))
           (unless (duplicate-thing-line-start-after-forward-line-p) (newline))
           (setq end (point))))
    (setq deactivate-mark nil)
    (goto-char end)
    (set-mark start)))

;;;###autoload
(defun duplicate-thing (p1 p2 n)
  "Duplicate line or region N times.
If it has active mark (P1, P2), it will expand the selection and duplicate it.
If it doesn't have active mark, it will select current line and duplicate it."
  (interactive "r\np")
  (let (start end len text)
    (duplicate-thing-expand-selection p1 p2)
    (setq start (region-beginning)
          end   (region-end)
          len   (- end start)
          text  (buffer-substring start end))
    (dotimes (i (or n 1)) (insert text))
    (set-mark (- (point) len))
    (setq deactivate-mark nil)
    (setq transient-mark-mode (cons 'only t))))

(provide 'duplicate-thing)
;;; duplicate-thing.el ends here
