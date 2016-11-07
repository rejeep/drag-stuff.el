Feature: Drag region
  In order to move a region left and right
  As an emacs user
  I want to drag it

  Background: 
    Given I insert "beforeREGIONafter"
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings

  Scenario: Drag region left
    When I drag region "REGION" left
    Then I should see "beforREGIONeafter"
    And the region should be "REGION"

  Scenario: Drag region right
    When I drag region "REGION" right
    Then I should see "beforeaREGIONfter"
    And the region should be "REGION"

  Scenario: Drag word left out of scope
    When I drag region "before" left
    Then I should see "beforeREGIONafter"
    And I should see message "Can not move region further to the left"
    And the region should be "before"

  Scenario: Drag word right out of scope
    When I drag region "after" right
    Then I should see "beforeREGIONafter"
    And I should see message "Can not move region further to the right"
    And the region should be "after"

