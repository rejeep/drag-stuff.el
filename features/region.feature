Feature: Drag region
  In order to move a region left and right
  As an emacs user
  I want to drag it

  Background:
    And I am in buffer "*drag-stuff*"
    And the buffer is empty
    And I insert "beforeREGIONafter"
    And there is no region selected
    And I enable drag-stuff

  Scenario: Drag region left
    When I select "REGION"
    And I press "<M-left>"
    Then I should see "beforREGIONeafter"
    And the region should be "REGION"

  Scenario: Drag region right
    When I select "REGION"
    And I press "<M-right>"
    Then I should see "beforeaREGIONfter"
    And the region should be "REGION"

  Scenario: Drag word left out of scope
    When I select "before"
    And I press "<M-left>"
    Then I should see "beforeREGIONafter"
    And I should see message "Can not move region further to the left"
    And the region should be "before"

  Scenario: Drag word right out of scope
    When I select "after"
    And I press "<M-right>"
    Then I should see "beforeREGIONafter"
    And I should see message "Can not move region further to the right"
    And the region should be "after"
