;;; drag-stuff.el --- Drag stuff (lines, words, region, etc...) around

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/drag-stuff

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Code:

(defvar drag-stuff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<M-up>") 'drag-stuff-up)
    (define-key map (kbd "<M-down>") 'drag-stuff-down)
    (define-key map (kbd "<M-right>") 'drag-stuff-right)
    (define-key map (kbd "<M-left>") 'drag-stuff-left)
    map)
  "Keymap for `drag-stuff-mode'.")

(defun drag-stuff-up (arg)
  "Drag stuff ARG lines up."
  (interactive "p")
  (if mark-active
      (drag-stuff-lines-up arg)
    (drag-stuff-line-up arg)))

(defun drag-stuff-down (arg)
  "Drag stuff ARG lines down."
  (interactive "p")
  (if mark-active
      (drag-stuff-lines-down arg)
    (drag-stuff-line-down arg)))

(defun drag-stuff-right (arg)
  "Drag stuff ARG lines to the right."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-right arg)
    (drag-stuff-word-right arg)))

(defun drag-stuff-left (arg)
  "Drag stuff ARG lines to the left."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-left arg)
    (drag-stuff-word-left arg)))

(defun drag-stuff-line-up (arg)
  "Drag current line ARG lines up."
  (if (> (line-number-at-pos) arg)
      (drag-stuff-line-vertically
       (lambda (beg end column line)
         (delete-region beg end)
         (backward-delete-char 1)
         (forward-line (+ (- arg) 1))
         (goto-char (line-beginning-position))
         (insert line)
         (newline)
         (forward-line -1)
         (move-to-column column)))))

(defun drag-stuff-line-down (arg)
  "Drag current line ARG lines down."
  (if (<= (+ (line-number-at-pos) arg) (count-lines (point-min) (point-max)))
      (drag-stuff-line-vertically
       (lambda (beg end column line)
         (delete-region beg end)
         (delete-char 1)
         (forward-line (- arg 1))
         (goto-char (line-end-position))
         (newline)
         (insert line)
         (move-to-column column)))))

(defun drag-stuff-line-vertically (fn)
  "Yields variables used to drag line up and down."
  (let* ((column (current-column))
         (beg (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties beg end)))
    (funcall fn beg end column line)))

(defun drag-stuff-lines-up (arg)
  ""

  )

(defun drag-stuff-lines-down (arg)
  ""

  )

(defun drag-stuff-region-left (arg)
  "Drags region left ARG times."
  (if (> (min (point) (mark)) (point-min))
      (drag-stuff-region-horizontally (- arg))))

(defun drag-stuff-region-right (arg)
  "Drags region right ARG times."
  (if (< (max (point) (mark)) (point-max))
      (drag-stuff-region-horizontally arg)))

(defun drag-stuff-region-horizontally (arg)
  "Drags region horizontally ARG times."
  (let* ((beg (mark))
         (end (point))
         (region (buffer-substring-no-properties beg end))
         (deactivate-mark nil))
    (delete-region beg end)
    (forward-char arg)
    (insert region)
    (set-mark (+ beg arg))
    (goto-char (+ end arg))))

(defun drag-stuff-word-left (arg)
  "Drags word left ARG times."
  (drag-stuff-word-horizontally (- arg)))

(defun drag-stuff-word-right (arg)
  "Drags word right ARG times."
  (drag-stuff-word-horizontally arg))

(defun drag-stuff-word-horizontally (arg)
  "Drags word horizontally ARG times."
  (let ((old-point (point))
        (offset (- (save-excursion (forward-word) (point)) (point))))
    (condition-case err
        (progn
          (transpose-words arg)
          (backward-char offset))
      (error
       (goto-char old-point)))))

(define-minor-mode drag-stuff-mode
  "Drag stuff around."
  :init-value nil
  :lighter " ds"
  :keymap drag-stuff-mode-map)

(provide 'drag-stuff)

;;; drag-stuff.el ends here
