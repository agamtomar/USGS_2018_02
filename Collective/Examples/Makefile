FC=mpif90
FFLAGS=-O3
CC=mpicc
CCFLAGS=-O3
CXX=mpicxx
CXFLAGS=-O3

all: mpi_barrier.cexe mpi_barrier.fexe mpi_hello.cexe mpi_hello.fexe mpi_pi.cexe mpi_pi.fexe

mpi_barrier.fexe: mpi_barrier.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_barrier.cexe: mpi_barrier.cpp
	$(CXX) $(CXLAGS) $< -o $@

mpi_hello.fexe: mpi_hello.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_hello.cexe: mpi_hello.cpp
	$(CXX) $(CXLAGS) $< -o $@

mpi_pi.fexe: mpi_pi.f90
	$(FC) $(FFLAGS) $< -o $@

mpi_pi.cexe: mpi_pi.c
	$(CC) $(CCLAGS) $< -o $@



clean:
	rm -rf mpi_barrier.cexe mpi_barrier.fexe mpi_hello.cexe mpi_hello.fexe mpi_pi.cexe mpi_pi.fexe
