default : compile

-include local.mk

all : autoloads compile

compile :
	emacs --batch -L . -f batch-byte-compile *.el

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "deft.el" t (expand-file-name "deft-autoloads.el"))'

clean :
	-rm *.elc

website : web/index.html

web/index.html : README.org deft-website.el template.html
	mkdir -p web
	emacsclient --eval '(progn (load "deft-website.el") (deft-web-make "README.org" "template.html" "web/index.html"))'
