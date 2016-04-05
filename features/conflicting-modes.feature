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

  @not-in-emacs-23
  Scenario Outline: Electric indent mode
    Given I insert:
      """
      class Foo
      end
      def bar
      end
      """
    And I turn on ruby-mode
    And I turn on drag-stuff
    And I turn on electric-indent-mode
    And I drag line "<line>" <direction>
    Then I should see:
      """
      class Foo
      def bar
      end
      end
      """

    Examples:
      | direction | line |
      | down      |    2 |
      | up        |    3 |

  @not-in-emacs-24.5
  Scenario: Longlines mode down
    Given I insert:
      """
      Move me down please
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed tristique sollicitudin massa, ut porta diam pellentesque et. Sed porttitor tempor egestas. Morbi accumsan quam sed elit auctor nec interdum mi tincidunt.
      """
    And I turn on drag-stuff
    And I turn on longlines-mode
    And I drag line "1" down
    Then I should see:
      """
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed tristique
      sollicitudin massa, ut porta diam pellentesque et. Sed porttitor
      tempor egestas. Morbi accumsan quam sed elit auctor nec interdum mi
      tincidunt.
      Move me down please
      """

  @not-in-emacs-24.5
  Scenario: Longlines mode up
    Given I insert:
      """
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed tristique sollicitudin massa, ut porta diam pellentesque et. Sed porttitor tempor egestas. Morbi accumsan quam sed elit auctor nec interdum mi tincidunt.
      Move me down please
      """
    And I turn on drag-stuff
    And I turn on longlines-mode
    And I drag line "5" up
    Then I should see:
      """
      Move me down please
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed tristique
      sollicitudin massa, ut porta diam pellentesque et. Sed porttitor
      tempor egestas. Morbi accumsan quam sed elit auctor nec interdum mi
      tincidunt.
      """

  Scenario: Do not activate longlines if not previously active
    Given I insert:
      """
      Lorem
      Ipsum
      """
    And I turn on drag-stuff
    And I drag line "1" down
    Then longlines-mode should not be active

  Scenario Outline: Evil region left/right
    Given I insert:
      """
      abcdef
      """
    And I turn on evil-mode
    And I turn on drag-stuff
    And I evil select region 3:4
    And I evil drag <direction>
    Then I should see:
      """
      <abcdef>
      """

    Examples:
      | direction | abcdef |
      | left      | acdbef |
      | right     | abecdf |

  Scenario Outline: Evil lines up/down
    Given I insert:
      """
      line 1
      line 2
      line 3
      line 4
      line 5
      """
    And I turn on evil-mode
    And I turn on drag-stuff
    And I evil select lines <start>:<end>
    And I evil drag <direction>
    And I evil drag <direction>
    Then I should see:
      """
      line 3
      line 4
      line 1
      line 2
      line 5
      """

    Examples:
      | start | end | direction |
      | 1     | 2   | down      |
      | 3     | 4   | up        |

