Feature: Drag lines
  In order to move lines up and down
  As an emacs user
  I want to drag them

  Background: 
    Given I insert:
      """
      line 1
      line 2
      line 3
      """
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings

  Scenario: Drag lines up
    When I drag lines "2" to "3" up
    Then I should see:
      """
      line 2
      line 3
      line 1
      """
    And the region should be:
      """
      line 2

      """

  Scenario: Drag lines down
    When I drag lines "1" to "2" down
    Then I should see:
      """
      line 3
      line 1
      line 2
      """
    And the region should be:
      """
      line 1

      """

  Scenario: Drag lines up out of scope
    When I drag lines "1" to "2" up
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further up"
    And the region should be:
      """
      line 1

      """

  Scenario: Drag lines down out of scope
    When I drag lines "2" to "3" down
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further down"
    And the region should be:
      """
      line 2

      """

