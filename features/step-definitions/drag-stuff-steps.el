(Given "^I \\(enable\\|disable\\) drag-stuff$"
       (lambda (status)
         (if (string= status "enable")
             (turn-on-drag-stuff-mode)
           (turn-off-drag-stuff-mode))))
