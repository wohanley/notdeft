default : compile

# Override this to add custom load paths to the required libraries, or perhaps to load the init file with "emacs --batch -l ~/.emacs".
EMACS_BATCH := emacs --batch

# Override this to compile more files. These ones typically would not require additional library load paths. However, it may be easiest to `byte-recompile-directory' from a running Emacs, one that has all the usual run-time libraries available.
COMPILED_EL := notdeft.el notdeft-global.el notdeft-path.el notdeft-xapian.el notdeft-xapian-make.el

-include local.mk

all : autoloads compile

compile :
	$(EMACS_BATCH) -L . -f batch-byte-compile $(COMPILED_EL)

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
