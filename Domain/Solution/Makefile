FC=mpifort
FFLAGS=-g -O0 -Wall -Warray-temporaries -pedantic -std=f2008 -ffpe-trap=invalid,zero,overflow
CC=mpicc
CFlags=-g -Wall -pedantic -std=c11
CXX=mpicxx
CXFLAGS=-g -Wall -pedantic -std=c++11

all: grid_overlap.fexe grid_type.fexe mpi_collatz_sol.fexe mpi_collatz_sol.cexe mpi_trapezoid_sol.fexe mpi_trapezoid_sol.cexe

grid_decomposition.fexe: grid_decomposition.f90
	$(FC) $(FFLAGS) $< -o $@

grid_overlap.fexe: grid_overlap.f90
	$(FC) $(FFLAGS) $< -o $@

grid_type.fexe: grid_type.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_collatz_sol.fexe: mpi_collatz_sol.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_collatz_sol.cexe: mpi_collatz_sol.cpp
	$(CXX) $(CXLAGS) $< -o $@

mpi_trapezoid_sol.fexe: mpi_trapezoid_sol.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_trapezoid_sol.cexe: mpi_trapezoid_sol.cpp
	$(CXX) $(CXLAGS) $< -o $@


clean:
	rm -rf *.fexe *.cexe *.mod
