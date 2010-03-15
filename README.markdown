# Drag Stuff
Drag Stuff is a minor mode for Emacs that makes it possible to drag
stuff, such as words, region and lines, around in Emacs.

## Installation
First download the **drag-stuff.el** file. If you use git you can
fetch it from Github
    $ git clone git://github.com/rejeep/drag-stuff.git ~/.emacs.d/packages/drag-stuff
    
Make sure it's in Emacs load-path
    (add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
    
Then require it
    (require 'drag-stuff)

## Usage
To start drag-stuff
    (drag-stuff-mode t) or M-x drag-stuff-mode

Or if you want it to be done automatically
    (drag-stuff-global-mode t)

### Drag line
To drag a line up and down. Put the cursor on that line and press
**<M-up>** and **<M-down>**.

### Drag lines
To drag several lines up and down. Select the lines you want to drag
and press **<M-up>** and **<M-down>**.

### Drag region
A region can be dragged to the left and right. Select the region you
want to drag and press **<M-left>** and **<M-right>**.

### Drag word
To drag a word. Just place the cursor on the word and press
**<M-left>** and **<M-right>**.
