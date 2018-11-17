default : compile

# override this to add custom load paths to the required libraries, or perhaps to load the init file with `emacs --batch -l ~/.emacs`
EMACS_BATCH := emacs --batch

-include local.mk

all : autoloads compile

compile :
	$(EMACS_BATCH) -L . -f batch-byte-compile $(wildcard *.el)

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "notdeft.el" t (expand-file-name "notdeft-autoloads.el"))'

exe :
	$(MAKE) -C xapian

clean :
	-rm *.elc

PKGVER := 0.6.$(shell date +%Y%m%d)
PKGNAMEVER := notdeft-$(PKGVER)
PKGTMPDIR := /tmp/$(PKGNAMEVER)
PKGMANIFEST := $(PKGTMPDIR)/notdeft-pkg.el

package :
	mkdir -p download
	-rm -r $(PKGTMPDIR)
	mkdir -p $(PKGTMPDIR)
	cp -ai ./ $(PKGTMPDIR)/
	( cd $(PKGTMPDIR) && git clean -dxffq && rm -rf .git && rm notdeft-autoloads.el )
	echo '(define-package "notdeft" "'$(PKGVER)'"' > $(PKGMANIFEST)
	echo '  "Edit, organize, and quickly find note files")' >> $(PKGMANIFEST)
	( tar --create --file download/$(PKGNAMEVER).tar -C /tmp $(PKGNAMEVER) )
