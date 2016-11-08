Feature: Drag word
  In order to move a word left and right
  As an emacs user
  I want to drag it

  Background: 
    Given I insert "word1 word2 word3"
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings

  Scenario: Drag word left
    When I drag word "word3" left
    Then I should see "word1 word3 word2"

  Scenario: Drag word right
    When I drag word "word1" right
    Then I should see "word2 word1 word3"

  Scenario: Drag word left out of scope
    When I drag word "word1" left
    Then I should see "word1 word2 word3"
    And I should see message "Can not move word further to the left"

  Scenario: Drag word right out of scope
    When I drag word "word3" right
    Then I should see "word1 word2 word3"
    And I should see message "Can not move word further to the right"

