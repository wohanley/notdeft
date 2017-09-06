PROGRAM := deft-xapian

default : build

-include local.mk

build : $(PROGRAM)

$(PROGRAM) : deft-xapian.cc
	g++ -o $@ -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs` $<

clean :
	-rm $(PROGRAM)

install-deps :
	sudo aptitude install libtclap-dev libxapian-dev

