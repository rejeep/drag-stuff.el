Feature: Drag Stuff
  In order to move stuff in Emacs
  As an Emacs user
  I want to drag them

  Scenario: Global mode
    When I turn on drag-stuff globaly
    And I activate the suggested drag-stuff key-bindings
    And I open temp file "global"
    And I insert:
      """
      line 1
      line 2
      """
    When I drag line "1" down
    Then I should see:
      """
      line 2
      line 1
      """

  Scenario: Global mode except
    When I turn on drag-stuff globaly
    And I activate the suggested drag-stuff key-bindings
    And I add "text-mode" as an except mode
    And I turn on text-mode
    Then drag-stuff-mode should not be active

