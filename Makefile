TAGS ?= '--tags ~@only-in-emacs-23,~@not-in-emacs-24.5'

all: test

test: ecukes

ecukes:
	cask exec ecukes features ${TAGS}

.PHONY: ecukes test all
