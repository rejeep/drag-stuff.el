(let ((current-directory (file-name-directory load-file-name)))
  (setq drag-stuff-root-path (expand-file-name ".." current-directory))
  (setq drag-stuff-util-path (expand-file-name "util" drag-stuff-root-path)))

(add-to-list 'load-path drag-stuff-root-path)
(add-to-list 'load-path (expand-file-name "ecukes" drag-stuff-util-path))
(add-to-list 'load-path (expand-file-name "espuds" drag-stuff-util-path))

(require 'drag-stuff)
(require 'espuds)

(Before
 (setq transient-mark-mode t))
