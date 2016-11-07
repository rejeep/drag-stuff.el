Feature: Drag Stuff for Evil Mode
  These are specific tests for evil-mode's visual mode support.
  
  Scenario Outline: Evil drag region left/right
    Given I insert:
      """
      abcdefg
      """
    And I turn on evil-mode
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings
    And I evil select region <beg>:<end>
    And I drag region <direction>
    Then I should see:
      """
      <text>
      """
    And the evil point should be at point <point>
    And the evil mark should be at point <mark>

    Examples:
      | direction | text    | beg | end | point | mark |
      | left      | acdebfg | 3   | 5   | 4     | 2    |
      | left      | bcdaefg | 4   | 2   | 1     | 3    |
      | right     | abfcdeg | 3   | 5   | 6     | 4    |
      | right     | dabcefg | 3   | 1   | 2     | 4    |

  Scenario Outline: Evil drag region up/down
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
    And I activate the suggested drag-stuff key-bindings
    And I evil select region <beg>:<end>
    And I drag region <direction>
    And I drag region <direction>
    Then I should see:
      """
      line 3
      line 4
      line 1
      line 2
      line 5
      """
    And the evil point should be at point <point>
    And the evil mark should be at point <mark>

    Examples:
      | beg | end | direction | mark | point |
      | 2   | 9   | down      | 16   | 23    |
      | 10  | 4   | down      | 24   | 18    |
      | 17  | 25  | up        | 3    | 11    |
      | 23  | 16  | up        | 9    | 2     |

  Scenario Outline: Evil drag lines up/down
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
    And I activate the suggested drag-stuff key-bindings
    And I evil select lines <begline>:<endline> at column <begcol>:<endcol>
    And I drag lines <direction>
    And I drag lines <direction>
    Then I should see:
      """
      line 3
      line 4
      line 1
      line 2
      line 5
      """
    And the evil point should be at point <point>
    And the evil mark should be at point <mark>
    
    Examples:
      | begline | endline | begcol | endcol | direction | mark | point |
      | 1       | 2       | 3      | 5      | down      | 17   | 26    |
      | 2       | 1       | 2      | 4      | down      | 23   | 18    |
      | 3       | 4       | 4      | 3      | up        | 4    | 10    |
      | 4       | 3       | 3      | 3      | up        | 10   | 3     |
      
  Scenario Outline: Evil drag region up/down out of scope
    Given I insert:
      """
      line 1
      line 2
      line 3
      """
    And I turn on evil-mode
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings
    And I evil select region <beg>:<end>
    And I drag region <direction>
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further <direction>"
    And the evil point should be at point <end>
    And the evil mark should be at point <beg>

    Examples:
      | beg | end | direction |
      | 2   | 9   | up        |
      | 10  | 4   | up        |
      | 10  | 17  | down      |
      | 16  | 9   | down      |

  Scenario Outline: Evil drag lines up/down out of scope
    Given I insert:
      """
      line 1
      line 2
      line 3
      """
    And I turn on evil-mode
    And I turn on drag-stuff
    And I activate the suggested drag-stuff key-bindings
    And I evil select lines <begline>:<endline> at column <begcol>:<endcol>
    And I drag lines <direction>
    Then I should see:
      """
      line 1
      line 2
      line 3
      """
    And I should see message "Can not move lines further <direction>"
    And the evil point should be at point <point>
    And the evil mark should be at point <mark>

    Examples:
      | begline | endline | begcol | endcol | direction | mark | point |
      | 1       | 2       | 3      | 5      | up        | 3    | 12    |
      | 2       | 1       | 2      | 4      | up        | 9    | 4     |
      | 3       | 3       | 4      | 3      | down      | 18   | 17    |
      | 3       | 2       | 3      | 3      | down      | 17   | 10    |
