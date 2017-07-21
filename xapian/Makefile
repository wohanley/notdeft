default : deft-xapian

deft-xapian : deft-xapian.cc
	g++ -o $@ -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs` $<

