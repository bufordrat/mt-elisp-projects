# -*- makefile-gmake -*-
# new minimal elisp makefile

# list all your .el files here
EL = $(wildcard *.el)
# list all your test-suite .el files here
TEST =
# files to install in a local lisp dir
INSTALLED = $(EL) $(EL:.el=.elc)
# directory in which to install $(INSTALLED)
INSTALLDIR = /dev/null

# path to proper emacs
EMACS = emacs
# batch compilation emacs invocation
BATCH = $(EMACS) -batch -L .

compile: $(EL:.el=.elc) $(TEST:.el=.elc)   ## byte-compile all elisp files
.PHONY: compile

install: $(INSTALLED)
	install -m 444 $^ $(INSTALLDIR)
.PHONY: install

clean: $(SUBCLEANS)			## clean up build artifacts
	$(RM) $(EL:.el=.elc)
.PHONY: clean

.SUFFIXES: .el .elc .org .html .pdf .el .elc .texi .info

.el.elc:
	$(BATCH) -f package-initialize $(foreach fn,$(LOADFILES),-l $(fn)) \
	  -f batch-byte-compile $<
	$(RM) $(EL:.el=.elc)
