# Advanced Parallel Programming Draft Agenda
USGS Advanced HPC workshop - February 20-21

Prerequisite: MUST Complete hpc 101 webinar prior to class

## Day 1
|Time |Topic| Link to Material|
|---	|---	|---	|
| 08:30 - 09:00 | Welcome, introductions, logistics| |
| 09:00 - 09:30 | Speedup and scaling | Slides |
| 09:30 - 11:00 | Assessment of application scalability | hands on |
| 11:00 - 11:30 | Message passing - collective communication | Slides |
| 01:30 - 02:00 | Collective communication - master worker parallel programming | hands on |
| 02:00 - 02:30 | Introduction to point to point communication | Slides |
| 02:30 - 04:00 | Debugging message passing programs & consultation |

<div></div>

## Day 2
|Time |Topic| Link to Material|
|---	|---	|---	|
|08:30 - 09:00| Non-blocking point to point communication| Slides |
|09:00 - 10:00| Non-blocking exercises | hands on |
|10:00 - 10:30| Domain decomposition & MPI communicator | Slides |
|10:30 - 11:30| Scaling with different domain decomposition approaches | hands on |
|12:30-  01:00| Profiling MPI programs | Slides |
|01:00 - 02:00| Overlapping communication and computation | hands on|
|02:00 - 02:30| Parallel I/O | Slides |
|02:30 - 04:00| Using parallel I/O & consultation | hands on |

<div></div>

# Software necessary to complete the exercise on your laptop

## Fortran 2003 and C++ compiler

- gcc, gfortran
- Intel compilers

## MPI

Install or use an MPI implementation like:
- OpenMPI
- MPICH2
- IntelMPI

## Parallel HDF5

Install or use HDF5 with parallel I/O enabled

## Python

Install `mpi4py`

## R

### MPI for R

Install `pbdMPI`: https://cran.r-project.org/web/packages/pbdMPI/pbdMPI.pdf

Download the package source and then run the command below. You need
to substitute the include and lib paths with your installation. It
assumes you have a working MPI installation

``` bash
R CMD INSTALL pbdMPI --configure-args="--with-mpi-type=OPENMPI \
--with-mpi-include=/opt/local/include/openmpi-devel-mp \
--with-mpi-libpath=/opt/local/lib/openmpi-devel-mp" --no-test-load
```

### HDF5 for R

Install `hdf5r`: https://cran.r-project.org/package=hdf5r

I installed it using the `.R/Makevars` file since I have a parallel
HDF5 installation and the standard installation did not work. The
content of my Makevars file is 

```  bash
CFLAGS +=             -O3 -Wall -pipe -pedantic -std=gnu99
CXXFLAGS +=           -O3 -Wall -pipe -Wno-unused -pedantic 

CXX=h5c++
CC=h5pcc
LD=h5c++
```

I then used 

``` R
install.packages("hdf5r")
```
