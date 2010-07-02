Feature: Drag Stuff
  In order to move stuff in Emacs
  As an Emacs user
  I want to drag them

  Background:
    Given I am in buffer "*drag-stuff*"
    And the buffer is empty
    And there is no region selected

  Scenario: Global mode
    When I open temp file "global"
    And I insert:
      """
      line 1
      line 2
      """
    And I load the following:
      """
      (drag-stuff-global-mode t)
      """
    And I go to line "1"
    And I press "<M-down>"
    Then I should see:
      """
      line 2
      line 1
      """
      
  Scenario: Global mode except
    When I load the following:
      """
      (setq drag-stuff-except-modes '(text-mode))
      (drag-stuff-global-mode t)
      """
    And major mode is text-mode
    When I insert:
      """
      line 1
      line 2
      """
    And I go to line "1"
    And I press "<M-down>"
    Then I should see:
      """
      line 1
      line 2
      """
    And I should not see:
      """
      line 2
      line 1
      """
