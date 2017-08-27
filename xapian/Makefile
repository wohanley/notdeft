default : deft-xapian

-include local.mk

deft-xapian : deft-xapian.cc
	g++ -o $@ -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs` $<

install-deps :
	sudo aptitude install libtclap-dev libxapian-dev

