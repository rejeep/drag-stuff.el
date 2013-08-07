TAGS ?= '--tags ~@only-in-emacs-23'

all: test

test: ecukes

ecukes:
	cask exec ecukes features ${TAGS}

.PHONY: ecukes test all
