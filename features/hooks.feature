Feature: Hooks

  Background:
    Given I insert:
      """
      line 1
      line 2
      line 3
      line 4
      """
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings

  Scenario: Before and after wrap hooks
    Given I load the following:
      """
        (defvar drag-stuff-hax nil)

        (add-hook 'drag-stuff-before-drag-hook
                  (lambda ()
                    (when (zerop (current-column))
                      (backward-char 1)
                      (setq drag-stuff-hax t))))

        (add-hook 'drag-stuff-after-drag-hook
                  (lambda ()
                    (when drag-stuff-hax
                      (forward-char 1)
                      (setq drag-stuff-hax nil))))
      """
    When I press "M-<"
    And I press "C-f"
    And I press "C-f"
    And I press "C-SPC"
    And I press "C-n"
    And I press "C-n"
    And I press "C-a"
    And I press "<M-down>"
    Then I should see:
      """
      line 3
      line 1
      line 2
      line 4
      """
    And the region should be:
      """
      ne 1
      line 2

      """
