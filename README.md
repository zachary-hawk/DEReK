DEReK
=====

Welcome to the Durham Electronic RElaxation (K)Code, DEReK for short! A planewave Ab Initio DFT code.
This is a simple all electron, plane-wave Density Functional Theory code for calculating electronic structures. This a toy project started during my PhD at Durham University and as such there are no guarantees that this produces accurate results. In addition, it is likely not very useful to many as it does not contain any crystal information, there are no atoms as such, although they could be included through a user provided external potential.

I must also appologise for the tortured acronym. This is named for my grandfather (Derek for those of you not paying attention...) so some linguistic acrobatics had to be performed.

Prerequisites
-------------

This program relies on some external libraries that neeed to be installed before you begin:
1) FFTW3 - https://www.fftw.org
2) OpenBLAS - https://www.openblas.net

These libraries are required for fast fourier transforms and speedy maths operations. OpenBlas is the only maths library currently supported, but I hope to expand to include others at a later date.

There is also the option to compile DEReK with MPI (Message Passing Interface) such that the code can be run in parallel, ideal for larger calculations with many electrons. You will need a working MPI library linked to your fortran compiler.

Not to state the obvious, but you will also need working Fortran and C compilers to use DEReK. Theoretically this will work with any compiler, however testing has been done with the GCC compilers.

Installation
------------

With all of the prerequisites dealt with, we turn to installation, which is straight forward. 

To build with the default settings, simply type: 
```./configure && gmake```
GNU make is recommended. On first compile, you will be asked to specify the location of the FFTW and OpenBLAS libraries. Note that the default Makefile compiles the MPI version. 

There are a three main flags that can be used for customising the build: 
1)  ```gmake COMMS_ARCH=serial/mpi``` - Specifies wheteher to build parallel executable 
2)  ```gmake BUILD=fast/debug``` - The compiler optimisations, ```fast``` is the default and should be used for all production calculations 
3)  ```gmake CODATA_YEAR=2014/2018``` - Specifies which internaitonal standard of physical constants to be used in the calculation. 







