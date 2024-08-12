AC_INIT([DEReK], [1.0], [zachary.hawkhead@gmail.com])
AC_PREREQ([2.69])


# Check for the Fortran compiler
AC_PROG_FC([gfortran])

# Set library and include paths based on provided prefixes
FFTW3_LIBS="-lfftw3"
FFTW3_CFLAGS=""

OPENBLAS_LIBS="-lopenblas"
OPENBLAS_CFLAGS=""

# Check for FFTW3 library
AC_CHECK_LIB([fftw3], [fftw_execute], [], [AC_MSG_ERROR([FFTW3 library not found])])

# Check for OpenBLAS library
AC_CHECK_LIB([openblas], [cblas_dgemm], [], [AC_MSG_ERROR([OpenBLAS library not found])])

# Check for optional MPI library
AC_ARG_ENABLE([mpi],
    [AS_HELP_STRING([--disable-mpi], [Disable MPI support])],
    [enable_mpi=$enableval], [enable_mpi=yes])

if test "x$enable_mpi" = "xyes"; then
    AC_CHECK_PROG([MPIFC], [mpif90], [mpif90])
    if test -z "$MPIFC"; then
        AC_MSG_ERROR([mpif90 not found])
    fi
    MPI_FLAGS="-DMPI_ENABLED"
    MPI_COMPILER="$MPIFC"
else
    MPI_FLAGS=""
    MPI_COMPILER=""
fi


output_file="Build/buildinfo.mk"

AC_OUTPUT

# Generate the .mk file content
cat > $output_file <<EOF
FC=${FC}
CC=${CC}
FFTW_LIBS=${FFTW3_LIBS}
BLAS_LIBS=${OPENBLAS_LIBS}
MPIC=${MPI_COMPILER}
EOF



