FC=mpifort
FFLAGS=-g -O0 -Wall -Warray-temporaries -pedantic -std=f2008 -ffpe-trap=invalid,zero,overflow
CC=mpicc
CFlags=-g -Wall -Wextra -pedantic -std=c11
CXX=mpicxx
CXFLAGS=-g -Wall -Wextra -pedantic -std=c++11

all: ex1.exe ex2.exe ex3.exe

ex1.exe: ex1.cxx
	$(CXX) $(CXFLAGS) $< -o $@

ex2.exe: ex2.cxx
	$(CXX) $(CXFLAGS) $< -o $@

ex3.exe: ex3.cxx
	$(CXX) $(CXFLAGS) $< -o $@

clean:
	rm -rf *.exe

