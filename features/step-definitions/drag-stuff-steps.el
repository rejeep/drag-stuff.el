(Given "^I turn on drag-stuff globaly$"
       (lambda ()
         (drag-stuff-global-mode 1)))

(Given "^I turn on drag-stuff$"
       (lambda ()
         (drag-stuff-mode 1)))

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
