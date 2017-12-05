default : compile

-include local.mk

all : autoloads compile

compile :
	emacs --batch -L . -f batch-byte-compile *.el

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "notdeft.el" t (expand-file-name "notdeft-autoloads.el"))'

exe :
	$(MAKE) -C xapian

clean :
	-rm *.elc

website : web/index.html

web/index.html : README.org notdeft-website.el template.html
	mkdir -p web
	emacsclient --eval '(progn (load "notdeft-website.el") (notdeft-web-make "README.org" "template.html" "web/index.html"))'
