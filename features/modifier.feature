Feature: Modifier
  In order to change prefix key
  As a drag stuff user
  I want to change the modifier key
  
  Background:
    Given I am in buffer "*drag-stuff*"
    And the buffer is empty
    And I insert:
      """
      line 1
      line 2
      """
    And there is no region selected
    And I go to line "1"
  
  Scenario: Change modifier key
    When I load the following:
      """
      (setq drag-stuff-modifier 'control)
      """
    And I enable drag-stuff
    And I press "<M-down>"
    Then I should see:
      """
      line 1
      line 2
      """
    And I should see message "<M-down> is undefined"
      
  Scenario: Single modifier key
    When I load the following:
      """
      (setq drag-stuff-modifier 'control)
      """
    And I enable drag-stuff
    When I press "<C-down>"
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Multiple modifier keys
    When I load the following:
      """
      (setq drag-stuff-modifier '(meta shift))
      """
    And I enable drag-stuff
    When I press "<M-S-down>"
    Then I should see:
      """
      line 2
      line 1
      """
