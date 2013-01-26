# Drag Stuff
Drag Stuff is a minor mode for Emacs that makes it possible to drag
stuff (words, region, lines) around in Emacs.

[<img src="http://img.youtube.com/vi/KCy1sjUPwwg/0.jpg">](https://www.youtube.com/watch?v=KCy1sjUPwwg)

## Installation
I recommend installing via ELPA, but manual installation is simple as well:

    (add-to-list 'load-path "/path/to/drag-stuff")
    (require 'drag-stuff)

## Usage
Start `drag-stuff-mode` using.

    (drag-stuff-mode t)
    
or

    M-x drag-stuff-mode

### Drag line
To drag a line up and down. Put the cursor on that line and press
`<M-up>` and `<M-down>`.

### Drag lines
To drag several lines up and down. Select the lines you want to drag
and press `<M-up>` and `<M-down>`.

### Drag region
A region can be dragged to the left and right. Select the region you
want to drag and press `<M-left>` and `<M-right>`.

### Drag word
To drag a word. Place the cursor on the word and press `<M-left>` and
`<M-right>`.

For more information, see comments in `drag-stuff.el`.

## Contribution
Contribution is much welcome! Drag stuff is tested using [Ecukes](http://ecukes.info). When
adding new features, please write tests for them!

Install [carton](https://github.com/rejeep/carton) if you haven't
already, then:

    $ cd /path/to/drag-stuff
    $ carton

Run all tests with:

    $ make
