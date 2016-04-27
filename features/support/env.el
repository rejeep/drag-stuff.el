(require 'f)

(let* ((this-directory (f-dirname load-file-name))
       (drag-stuff-root-path (f-parent (f-parent this-directory)))
       (drag-stuff-vendor-path (f-expand "vendor" drag-stuff-root-path)))
  (add-to-list 'load-path drag-stuff-root-path)
  (unless (require 'ert nil 'noerror)
    (require 'ert (f-expand "ert" drag-stuff-vendor-path))))

(require 'ert)
(require 'drag-stuff)
(require 'espuds)
(require 'evil)

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

 ;; Reset hooks
 (setq drag-stuff-before-drag-hook nil)
 (setq drag-stuff-after-drag-hook nil)

 (auto-fill-mode -1)
 (if (fboundp 'electric-indent-mode)
     (electric-indent-mode -1))
 (if (fboundp 'longlines-mode)
     (longlines-mode -1))

 (if (fboundp 'evil-mode)
     (evil-mode -1))

 ;; Remove all bindings
 (dolist (direction '(up down left right))
   (define-key drag-stuff-mode-map (drag-stuff--kbd direction) nil)))
