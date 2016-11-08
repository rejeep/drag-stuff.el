(Given "^I turn on drag-stuff globaly$"
       (lambda ()
         (drag-stuff-global-mode 1)))

(Given "^I turn on drag-stuff$"
       (lambda ()
         (drag-stuff-mode 1)))

(Given "^I activate the suggested drag-stuff key-bindings$"
       (lambda ()
         (drag-stuff-define-keys)))

(Given "^I turn off drag-stuff$"
       (lambda ()
         (drag-stuff-mode -1)))

(When "^I add \"\\(.+\\)\" as an except mode$"
      (lambda (mode)
        (add-to-list 'drag-stuff-except-modes (intern mode))))

(When "I place cursor on word \"\\(.+\\)\""
      (lambda (word)
        (When "I go to word \"%s\"" word)
        (When "I press \"%s\"" "C-f")))

(When "^I drag line \"\\(.+\\)\" \\(up\\|down\\)$"
      (lambda (line direction)
        (When "I go to line \"%s\"" line)
        (And  "I drag line %s" direction)))

(When "^I drag lines \"\\(.+\\)\" to \"\\(.+\\)\" \\(up\\|down\\)$"
      (lambda (min-line max-line direction)
        (When "I go to line \"%s\"" min-line)
        (And  "I set the mark")
        (And  "I go to line \"%s\"" max-line)
        (And  "I drag lines %s" direction)))

(When "^I drag word \"\\(.+\\)\" \\(right\\|left\\)$"
      (lambda (word direction)
        (When "I place cursor on word \"%s\"" word)
        (And  "I drag word %s" direction)))

(When "^I drag region \"\\(.+\\)\" \\(left\\|right\\)$"
      (lambda (region direction)
        (When "I select \"%s\"" region)
        (And  "I drag region %s" direction)))

(When "^I drag \\(?:lines?\\|word\\|region\\) \\(up\\|down\\|left\\|right\\)$"
      (lambda (direction)
        (When "I press \"%s\"" (format "<M-%s>" direction))))

(Then "^\\(.+\\) should not be active$"
       (lambda (mode)
         (let* ((symbol (intern mode))
                (active (and (boundp symbol) (symbol-value symbol))))
           (should-not active))))

(Given "^I turn on evil$"
       (lambda ()
         (turn-on-evil-mode)))

(When "^I evil select region \\([0-9]+\\):\\([0-9]+\\)$"
      (lambda (start end)
        (setq start (string-to-number start))
        (setq end (string-to-number end))
        (evil-visual-select start end)
        (when (< end start)
          (evil-visual-exchange-corners))))

(When "^I evil select lines \\([0-9]+\\):\\([0-9]+\\) at column \\([0-9]+\\):\\([0-9]+\\)$"
      (lambda (begline endline begcol endcol)
        (When "I go to line \"%s\"" begline)
        (forward-char (- (string-to-number begcol) 1))
        (And "I call \"evil-visual-line\"")
        (And "I go to line \"%s\"" endline)
        (forward-char (- (string-to-number endcol) 1))))

(Then "^the evil point should be at point \\([0-9]+\\)$"
  (lambda (point)
    (let ((message "Expected point to be at point '%s', but was at '%s'"))
      (cl-assert (= (string-to-number point) evil-visual-point) nil message point evil-visual-point))))

(Then "^the evil mark should be at point \\([0-9]+\\)$"
  (lambda (point)
    (let ((message "Expected mark to be at point '%s', but was at '%s'"))
      (cl-assert (= (string-to-number point) evil-visual-mark) nil message point evil-visual-mark))))
