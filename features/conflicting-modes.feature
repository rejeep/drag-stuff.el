Feature: Drag Stuff
  In order to work with various conflicting modes
  Drag stuff needs to handle them manually

  Scenario: Auto fill mode
    Given I insert:
      """
      (defun lorem ()
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nunc nibh"
        (if (<= (+ (line-number-at-pos (max (point) (mark))) arg) (count-lines (point-min) (point-max)))
          ;; ...
          ))
      """
    And I turn on drag-stuff
    And I turn on auto-fill-mode
    And I drag line "3" up
    Then I should see:
      """
      (defun lorem ()
        (if (<= (+ (line-number-at-pos (max (point) (mark))) arg) (count-lines (point-min) (point-max)))
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nunc nibh"
          ;; ...
          ))
      """

  Scenario: Electric indent mode
    Given I insert:
      """
      class Foo
        end
        def bar
      end
      """
    And I turn on drag-stuff
    And I turn on electric-indent-mode
    And I drag line "2" down
    Then I should see:
      """
      class Foo
        def bar
        end
      end
      """
