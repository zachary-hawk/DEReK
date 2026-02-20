FRaNKS
======

Welcome to **FRaNKS** — *First-principles Relaxation and Non-collinear Kohn-Sham*!  
FRaNKS is a plane-wave **ab initio Density Functional Theory (DFT)** code.

This is a simple, all-electron, plane-wave DFT code for calculating electronic structure.  
It is primarily a **toy / educational project**, originally started during my PhD, and as such there are **no guarantees of quantitative accuracy**. The code is not intended to be a full-featured production DFT package.

At present, FRaNKS does not contain explicit crystal or atomic information. There are no atoms as such, although atomic effects can be included via a **user-provided external potential**. The focus is instead on illustrating the core ideas behind Kohn–Sham DFT in a plane-wave basis, including relaxation and non-collinear effects.

I must also appologise for the tortured acronym. This is named for my grandfather (Frank for those of you not paying attention...) so some linguistic acrobatics had to be performed.

Prerequisites
-------------

This program relies on several external libraries that must be installed before building:

1. **FFTW3** — https://www.fftw.org  
2. **OpenBLAS** — https://www.openblas.net  

These libraries are required for fast Fourier transforms and efficient linear algebra operations.  
OpenBLAS is currently the only supported BLAS/LAPACK implementation, though support for additional libraries may be added in the future.

FRaNKS can optionally be compiled with **MPI (Message Passing Interface)**, allowing the code to run in parallel. This is particularly useful for larger calculations involving many electrons. A working MPI library compatible with your Fortran compiler is required for this.

You will also need working **Fortran and C compilers**. In principle, any standards-compliant compiler should work, but testing has primarily been carried out using the **GCC** toolchain.

FRaNKS relies on **LibXC** for exchange–correlation functionals. LibXC is bundled with the FRaNKS source and does not need to be compiled separately.

Installation
------------

Once all prerequisites are installed, building FRaNKS is straightforward.

To build with the default settings, simply run:


GNU Make is recommended. The `configure` script will automatically detect the locations of FFTW, OpenBLAS, and other required dependencies. By default, the Makefile builds the **MPI-enabled** version of the code if MPI is available.

There are three main build flags available for customisation:

1. `gmake COMMS_ARCH=serial/mpi`  
   Specifies whether to build a serial or MPI-parallel executable.

2. `gmake BUILD=fast/debug`  
   Controls compiler optimisation levels.  
   `fast` (the default) should be used for production calculations, while `debug` enables additional runtime checks.

3. `gmake CODATA_YEAR=2014/2018`  
   Selects which international standard of physical constants (CODATA) is used in calculations.

