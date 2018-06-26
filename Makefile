default : compile

# override this to add custom load paths to the required libraries, or perhaps to load the init file with `emacs --batch -l ~/.emacs`
EMACS_BATCH := emacs --batch

-include local.mk

all : autoloads compile

compile :
	$(EMACS_BATCH) -L . -f batch-byte-compile $(filter-out %-pkg.el, $(wildcard *.el))

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "notdeft.el" t (expand-file-name "notdeft-autoloads.el"))'

exe :
	$(MAKE) -C xapian

clean :
	-rm *.elc

PKGNAMEVER = `cat PKGNAMEVER`
package :
	emacs --batch -L . -q -l notdeft-info.el -f notdeft-pkg-basename > PKGNAMEVER 2>/dev/null
	mkdir -p download
	-rm -r /tmp/$(PKGNAMEVER)
	mkdir -p /tmp/$(PKGNAMEVER)
	cp -ai ./ /tmp/$(PKGNAMEVER)/
	( cd /tmp/$(PKGNAMEVER) && git clean -dxffq && rm -rf .git && rm notdeft-autoloads.el )
	( tar --create --file download/$(PKGNAMEVER).tar -C /tmp $(PKGNAMEVER) )
