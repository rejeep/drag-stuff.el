Feature: Drag line
  In order to move a line up and down
  As an emacs user
  I want to drag it

  Background: 
    Given I insert:
      """
      line 1
      line 2
      """
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings

  Scenario: Drag line up
    When I drag line "2" up
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Drag line down
    When I drag line "1" down
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Drag line down out of scope
    When I drag line "2" down
    Then I should see:
      """
      line 1
      line 2
      """
    And I should see message "Can not move line further down"

  Scenario: Drag line up out of scope
    When I drag line "1" up
    Then I should see:
      """
      line 1
      line 2
      """
    And I should see message "Can not move line further up"

