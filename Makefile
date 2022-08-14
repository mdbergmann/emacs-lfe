### Makefile for lfe-mode
### 
# Variables
#
EMACS?=emacs
SELECTOR=t
ERROR_ON_WARN=nil

LOAD_PATH=-L .

ELFILES := inferior-lfe.el lfe-completion.el lfe-indent.el lfe-mode.el lfe-start.el
ELCFILES := $(ELFILES:.el=.elc)

ELPADEPS ?=--eval '(package-initialize)'                        \
           --eval '(package-refresh-contents)'                  \
           --eval '(defun install-latest (p)                    \
                     (package-install                           \
                       (cadr (assoc p                           \
                              package-archive-contents          \
                              (quote equal)))))'                \
           --eval '(install-latest (quote dash))'			\
           --eval '(install-latest (quote company))'

BYTECOMP_ERROR_ON_WARN := \
	--eval '(setq byte-compile-error-on-warn $(ERROR_ON_WARN))'

all: compile

# Compilation.  Note BYTECOMP_ERROR_ON_WARN after ELPADEPS 
# so deps can still warn on compilation.
#
%.elc: %.el
	$(EMACS) -Q $(ELPADEPS) $(BYTECOMP_ERROR_ON_WARN) $(LOAD_PATH) \
		--batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
