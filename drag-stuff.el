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
  "")

(defun drag-stuff-up (arg)
  ""
  (interactive "p")
  )

(defun drag-stuff-down (arg)
  ""
  (interactive "p")
  )

(defun drag-stuff-right (arg)
  ""
  (interactive "p")
  )

(defun drag-stuff-left (arg)
  ""
  (interactive "p")
  )

(defun drag-stuff-line-up (arg)
  ""

  )

(defun drag-stuff-line-down (arg)
  ""

  )

(defun drag-stuff-lines-up (arg)
  ""

  )

(defun drag-stuff-lines-right (arg)
  ""

  )

(defun drag-stuff-region-left (arg)
  ""

  )

(defun drag-stuff-region-right (arg)
  ""

  )

(defun drag-stuff-word-left (arg)
  ""

  )

(defun drag-stuff-word-right (arg)
  ""

  )

(define-minor-mode drag-stuff-mode
  ""
  :init-value nil
  :lighter " ds"
  :keymap drag-stuff-mode-map
  (if drag-stuff-mode

      ))

(provide 'drag-stuff)

;;; drag-stuff.el ends here
