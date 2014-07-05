GITDESC = $(shell \
  git describe --tags --match='v[0-9]*\.[0-9]*\.[0-9]*' --abbrev=0)
VERSION = $(patsubst v%,%,$(GITDESC))

all: test package

clean:
	$(RM) loaddefs.el snakehump-pkg.el *.elc *~

test:
	emacs -Q -batch                                         \
	      -l ert -l snakehump.el -l snakehump-tests.el      \
	      -f ert-run-tests-batch-and-exit

snakehump-pkg.el:
	cask package
	sed 's%ckage "snakehump" "[^"]*"%ckage "snakehump" "'$(VERSION)'"%' \
	    -i $@

package: snakehump.el snakehump-pkg.el
	ln -s . snakehump-$(VERSION) || true
	tar cvf snakehump-$(VERSION).tar                        \
	  snakehump-$(VERSION)/Cask                             \
	  snakehump-$(VERSION)/Makefile                         \
	  snakehump-$(VERSION)/README.md                        \
	  snakehump-$(VERSION)/snakehump.el                     \
	  snakehump-$(VERSION)/snakehump-pkg.el                 \
	  snakehump-$(VERSION)/snakehump-tests.el
	rm snakehump-$(VERSION)
