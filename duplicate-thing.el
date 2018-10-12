;;; duplicate-thing.el --- Duplicate current line & selection

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: command duplicate line selection
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
;;

;;; Code:

(defun line-start-p (p)
  "Return t if point P is beginning of line."
  (goto-char p)
  (= 0 (current-column)))

(defun expand-selection (p1 p2)
  "Expand region in a way that P1 to beginning of line and P2 to beginning of next line."
  (let (start end)
    (cond (mark-active
           (goto-char p1)
           (beginning-of-line)
           (setq start (point))
           (goto-char p2)
           (unless (= 0 (current-column)) (unless (= 0 (forward-line)) (newline)))
           (setq end (point)))
          (t
           (beginning-of-line)
           (setq start (point))
           (unless (= 0 (forward-line)) (newline))
           (setq end (point))))
    (setq deactivate-mark nil)
    (goto-char end)
    (set-mark start)))

(defun move-to-yank-pos (p)
  "Move point P to yank position."
  (goto-char p)
  (unless (line-start-p p)
    (unless (= 0 (forward-line))
      (newline))))

;;;###autoload
(defun duplicate-thing (p1 p2)
  "Duplicate active region from P1 to P2. If there is no active region, duplicate current line."
  (interactive "r")
  (let (start end len text)
    (expand-selection p1 p2)
    (setq start (region-beginning)
          end   (region-end)
          len   (- end start)
          text  (buffer-substring start end))
    (move-to-yank-pos end)
    (insert text)
    (set-mark (- (point) len))
    (setq deactivate-mark nil)
    (setq transient-mark-mode (cons 'only t))))

(provide 'duplicate-thing)
;;; duplicate-thing.el ends here
