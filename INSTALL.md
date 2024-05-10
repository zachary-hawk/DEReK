# Installation Guide for DEReK

## Prerequisites

Before installing DEReK, make sure you have the following prerequisites installed:

1. **FFTW3** - Fast Fourier Transform library. You can download it from [here](https://www.fftw.org).

2. **OpenBLAS** - Optimized Basic Linear Algebra Subprograms library. Download it from [here](https://www.openblas.net).

3. **LIBXC** - Library of exchange-correlation functionals for density-functional theory. Get it from [here](https://libxc.gitlab.io).

Make sure these libraries are installed and their paths are accessible.

Additionally, if you want to compile DEReK with MPI support for parallel execution, ensure you have a working MPI library linked to your Fortran compiler.

## Installation Steps

1. Clone the DEReK repository to your local machine:

2. Navigate to the root directory of the cloned repository:

3. Compile DEReK using GNU make:

The first time you compile, you will be prompted to specify the locations of FFTW and OpenBLAS libraries. Follow the prompts to provide the correct paths.

4. Optionally, you can customize the build using the following flags:

- `COMMS_ARCH`: Specifies whether to build the parallel executable (`serial` or `mpi`).
- `BUILD`: Compiler optimizations (`fast` or `debug`).
- `CODATA_YEAR`: Specifies the international standard of physical constants to be used in the calculation (2014 or 2018).

Example usage:

```make COMMS_ARCH=mpi BUILD=fast CODATA_YEAR=2018```

This command will build the MPI version with fast compiler optimizations using CODATA 2018 physical constants.

5. Once compiled successfully, you will find the executable in the `Bin/` directory.

## Running DEReK

To run DEReK, simply execute the generated executable. You will need to provide additional input files depending on your specific calculation requirements.
