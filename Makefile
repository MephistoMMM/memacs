# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l init.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)
ROOT=`pwd`

## Byte compilation
# compile
compile: init.el clean
	@$(EMACS) -f memacs/recompile

clean:
	@for path in $(shell ls "$(ROOT)/" | grep ".elc"); do         \
		rm $${path};                                                \
		done
	@for path in $(shell find "$(ROOT)/core" -name "*.elc"); do   \
		rm $${path};                                                \
		done
	@for path in $(shell find "$(ROOT)/layers" -name "*.elc"); do \
		rm $${path};                                                \
		done

## Internal tasks
init.el:
	@ls init.el

.PHONY: compile
