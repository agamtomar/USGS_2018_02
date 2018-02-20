FC=mpif90
FFLAGS=-O3
CC=mpicc
CCFLAGS=-O3
CXX=mpicxx
CXFLAGS=-O3

all: exercise-01.fexe exercise-01.cexe mpi_barrier.fexe mpi_barrier.cexe

mpi_barrier.fexe: mpi_barrier.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_barrier.cexe: mpi_barrier.cpp
	$(CXX) $(CXFLAGS) $< -o $@

exercise-01.fexe: exercise-01.f90
	$(FC) $(FFLAGS) $< -o $@

exercise-01.cexe: exercise-01.cpp
	$(CXX) $(CXFLAGS) $< -o $@



clean:
	rm -rf mpi_barrier.cexe mpi_barrier.fexe exercise-01.fexe
