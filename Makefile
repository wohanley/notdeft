compile :
	emacs --batch -L . -f batch-byte-compile *.el

clean :
	-rm *.elc
