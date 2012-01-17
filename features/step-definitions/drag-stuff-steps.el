(Given "^I \\(enable\\|disable\\) drag-stuff$"
       (lambda (status)
         (if (string= status "enable")
             (drag-stuff-mode 1)
           (drag-stuff-mode -1))))

(Given "^I enable drag-stuff$"
       (lambda ()
         (drag-stuff-mode 1)))

(Given "^I disable drag-stuff$"
       (lambda ()
         (drag-stuff-mode -1)))

(Given "^major mode is text-mode$"
       (lambda ()
         (text-mode)))

(When "I place cursor on word \"\\(.+\\)\""
      (lambda (word)
        (When "I go to word \"%s\"" word)
        (forward-char (/ (length word) 2))))
