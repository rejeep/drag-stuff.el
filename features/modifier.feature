Feature: Modifier
  In order to change prefix key
  As a drag stuff user
  I want to change the modifier key

  Background: 
    Given I insert:
      """
      line 1
      line 2
      """
    And I go to line "1"

  Scenario: Single modifier key
    When I load the following:
      """
      (setq drag-stuff-modifier 'control)
      """
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings
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
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings
    When I press "<M-S-down>"
    Then I should see:
      """
      line 2
      line 1
      """

