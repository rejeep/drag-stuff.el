(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq drag-stuff-root-path project-directory)
  (setq drag-stuff-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path drag-stuff-root-path)
(add-to-list 'load-path (expand-file-name "espuds" drag-stuff-util-path))

(require 'drag-stuff)
(require 'espuds)


(Before
 (switch-to-buffer
  (get-buffer-create "*drag-stuff*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After
 ;; Reset the modifier
 (setq drag-stuff-modifier 'meta)

 ;; Disable drag-stuff
 (drag-stuff-global-mode -1)
 (drag-stuff-mode -1)

 (electric-indent-mode -1)

 ;; Remove all bindings
 (dolist (direction '(up down left right))
   (define-key drag-stuff-mode-map (drag-stuff--kbd direction) nil)))
