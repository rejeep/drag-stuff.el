Feature: Drag line
  In order to move a line up and down
  As an emacs user
  I want to drag it

  Background:
    Given I am in buffer "*drag-stuff*"
    And the buffer is empty
    And I insert:
      """
      line 1
      line 2
      """
    And there is no region selected
    And I enable drag-stuff

  Scenario: Drag line up
    When I go to line "2"
    And I press "<M-up>"
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Drag line down
    When I go to line "1"
    And I press "<M-down>"
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Drag line down out of scope
    When I go to line "2"
    And I press "<M-down>"
    Then I should see:
      """
      line 1
      line 2
      """
    And I should see message "Can not move line further down"

  Scenario: Drag line up out of scope
    When I go to line "1"
    And I press "<M-up>"
    Then I should see:
      """
      line 1
      line 2
      """
    And I should see message "Can not move line further up"
