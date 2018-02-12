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
|08:30 - 09:00| Domain decomposition & MPI communicator | Slides |
|09:00 - 10:00| Scaling with different domain decomposition approaches | hands on |
|10:00 - 10:30| Advanced point to point communication| Slides |
|10:30 - 11:30| Point to point | hands on |
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

Install `pbdMPI`

https://cran.r-project.org/web/packages/pbdMPI/pbdMPI.pdf
