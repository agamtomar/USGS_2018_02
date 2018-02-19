FC=mpifort
FFLAGS=-g -O0 -Wall -Warray-temporaries -pedantic -std=f2008 -ffpe-trap=invalid,zero,overflow
CC=mpicc
CFlags=-g -Wall -pedantic -std=c11
CXX=mpicxx
CXFLAGS=-g -Wall -pedantic -std=c++11

all: grid_decomposition.fexe mpi_collatz.fexe mpi_collatz.cexe mpi_trapezoid.fexe mpi_trapezoid.cexe

grid_decomposition.fexe: grid_decomposition.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_collatz.fexe: mpi_collatz.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_collatz.cexe: mpi_collatz.cpp
	$(CXX) $(CXLAGS) $< -o $@

mpi_trapezoid.fexe: mpi_trapezoid.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_trapezoid.cexe: mpi_trapezoid.cpp
	$(CXX) $(CXLAGS) $< -o $@


clean:
	rm -rf *.fexe *.cexe *.mod *.dSYM
