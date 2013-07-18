TAGS ?= '--tags ~@only-in-emacs-23'
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: test

test: ecukes

ecukes:
	carton exec ${ECUKES} features ${TAGS}

.PHONY: ecukes test all
