Feature: Drag lines
  In order to move lines up and down
  As an emacs user
  I want to drag them

  Background:
    Given I am in buffer "*drag-stuff*"
    And the buffer is empty
    And I insert:
      """
      line 1
      line 2
      line 3
      """
    And there is no region selected
    And I enable drag-stuff

  Scenario: Drag lines up
    When I go to point "10"
    And I set the mark
    And I go to point "17"
    When I press "<M-up>"
    Then I should see:
      """
      line 2
      line 3
      line 1
      """
    And the region should be:
      """
      ne 2
      li
      """

  Scenario: Drag lines down
    When I go to point "3"
    And I set the mark
    And I go to point "10"
    When I press "<M-down>"
    Then I should see:
      """
      line 3
      line 1
      line 2
      """
    And the region should be:
      """
      ne 1
      li
      """

  Scenario: Drag lines up out of scope
    When I go to point "3"
    And I set the mark
    And I go to point "10"
    When I press "<M-up>"
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further up"
    And the region should be:
      """
      ne 1
      li
      """

  Scenario: Drag lines down out of scope
    When I go to point "10"
    And I set the mark
    And I go to point "17"
    When I press "<M-down>"
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further down"
    And the region should be:
      """
      ne 2
      li
      """
