all : autoloads compile

-include local.mk

compile :
	emacs --batch -L . -f batch-byte-compile *.el

autoloads :
	emacs --batch -L . --eval '(update-file-autoloads "deft.el" t (expand-file-name "deft-autoloads.el"))'

clean :
	-rm *.elc
