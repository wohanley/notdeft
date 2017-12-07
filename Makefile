default : compile

-include local.mk

all : autoloads compile

compile :
	emacs --batch -L . -f batch-byte-compile $(filter-out %-pkg.el, $(wildcard *.el))

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "notdeft.el" t (expand-file-name "notdeft-autoloads.el"))'

exe :
	$(MAKE) -C xapian

clean :
	-rm *.elc

PKGNAMEVER = `cat PKGNAMEVER`
package :
	emacs --batch -L . -q -l notdeft-info.el -f notdeft-pkg-basename > PKGNAMEVER 2>/dev/null
	mkdir -p web
	-rm -r /tmp/$(PKGNAMEVER)
	mkdir -p /tmp/$(PKGNAMEVER)
	cp -ai ./ /tmp/$(PKGNAMEVER)/
	( cd /tmp/$(PKGNAMEVER) && git clean -dxffq && rm -rf .git && rm notdeft-autoloads.el )
	( tar --create --file web/$(PKGNAMEVER).tar -C /tmp $(PKGNAMEVER) )

website : web/index.html

web/index.html : README.org notdeft-website.el template.html
	mkdir -p web
	emacsclient --eval '(progn (load "notdeft-website.el") (notdeft-web-make "README.org" "template.html" "web/index.html"))'
