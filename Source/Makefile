FCFLAGS= -O1  -funroll-loops -p -g 
BUILD_DIR ?= ./../build
current_dir = '"'$(shell pwd)'"'

ifeq ($(BUILD),fast)
OPT= '"FAST"'
ifeq ($(F90),gfortran)
FCFLAGS= -fconvert=big-endian -fno-realloc-lhs -fopenmp -fPIC -O3 -funroll-loops -fno-signed-zeros -g -fbacktrace -march=native -mno-avx -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch  -I/opt/local/include/
endif
ifeq ($(F90),ifort)
FCFLAGS=-convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -O3 -debug minimal -traceback -xHost -fbackslash -lfftw3 -lopenblas -I/opt/local/include/
endif

endif

ifeq ($(BUILD),debug)
OPT= '"DEBUG"'
ifeq ($(F90),gfortran)
FCFLAGS= -static-libgfortran -Ddebug -O0 -g -fbounds-check -fbacktrace -Wall -Waliasing -Wsurprising -Wline-truncation -Wno-tabs -Wno-uninitialized -Wno-unused-dummy-argument -Wno-unused -Wno-character-truncation -Wl,-no_pie -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch
endif
ifeq ($(F90),ifort)
FCFLAGS= -convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -Ddebug -O0 -g -debug extended -traceback -C -stand f03 -warn all -diag-disable 7025 -diag-disable 7712 -diag-disable 5268 -diag-disable 7847 -diag-disable 7893 -diag-disable 6439 -fbackslash -lfftw3 -lopenblas
 endif
endif

#FCFLAGS= -O1  -funroll-loops -p -g

ifeq ($(COMMS_ARCH),mpi)

derek.mpi: derek.o io.o trace.o comms.mpi.o basis.o memory.o wave.o pot.o state.o density.o fft.o
	$(MPI_F90) -o derek.mpi  derek.o io.o trace.o comms.mpi.o  basis.o memory.o wave.o pot.o state.o density.o fft.o $(FCFLAGS)
# Here are the compile steps
derek.o:derek.f90 io.o trace.o comms.mpi.o basis.o memory.o wave.o pot.o state.o density.o
	$(MPI_F90) -c  derek.f90 $(FCFLAGS)
state.o:./state.f90 io.o trace.o comms.mpi.o basis.o memory.o wave.o pot.o density.o
	$(MPI_F90) -c state.f90 $(FCFLAGS)
density.o: ./density.f90 io.o comms.mpi.o basis.o memory.o wave.o pot.o
	$(MPI_F90) -c density.f90 $(FCFLAGS)
pot.o:./pot.f90 io.o trace.o comms.mpi.o memory.o wave.o
	$(MPI_F90) -c pot.f90 $(FCFLAGS)
wave.o:./wave.f90 io.o comms.mpi.o 
	$(MPI_F90) -c ./wave.f90 $(FCFLAGS)
basis.o:./basis.f90 trace.o io.o comms.mpi.o
	$(MPI_F90) -c ./basis.f90 $(FCFLAGS)

memory.o:./memory.f90  trace.o io.o comms.mpi.o
	$(MPI_F90) -c ./memory.f90 $(FCFLAGS)

io.o:./io.f90 trace.o comms.mpi.o memory.o
	$(MPI_F90) -c  ./io.f90 -cpp  -D opt_strat=$(OPT) $(FCFLAGS)

trace.o:./trace.f90
	$(MPI_F90) -c  ./trace.f90 $(FCFLAGS)

comms.mpi.o:./comms.mpi.f90 trace.o
	$(MPI_F90) -c  ./comms.mpi.f90 $(FCFLAGS)
fft.o:./fft.f90 io.o
	$(MPI_F90) -c  ./fft.f90 $(FCFLAGS)
endif

ifeq ($(COMMS_ARCH),serial)

derek.serial: derek.o io.o trace.o comms.serial.o basis.o memory.o wave.o pot.o state.o density.o
	$(SERIAL_F90) -o derek.serial  derek.o io.o trace.o comms.serial.o  basis.o memory.o wave.o pot.o state.o density.o $(FCFLAGS)
# Here are the coserialle steps
derek.o:derek.f90 io.o trace.o comms.serial.o basis.o memory.o wave.o pot.o state.o density.o
	$(SERIAL_F90) -c  derek.f90 $(FCFLAGS)
state.o:./state.f90 io.o trace.o comms.serial.o basis.o memory.o wave.o pot.o density.o
	$(SERIAL_F90) -c state.f90 $(FCFLAGS)
density.o: ./density.f90 io.o comms.serial.o basis.o memory.o wave.o pot.o
	$(SERIAL_F90) -c density.f90 $(FCFLAGS)
pot.o:./pot.f90 io.o trace.o comms.serial.o memory.o wave.o
	$(SERIAL_F90) -c pot.f90 $(FCFLAGS)
wave.o:./wave.f90 io.o comms.serial.o 
	$(SERIAL_F90) -c ./wave.f90 $(FCFLAGS)
basis.o:./basis.f90 trace.o io.o comms.serial.o
	$(SERIAL_F90) -c ./basis.f90 $(FCFLAGS)

memory.o:./memory.f90  trace.o io.o comms.serial.o
	$(SERIAL_F90) -c ./memory.f90 $(FCFLAGS)

io.o:./io.f90 trace.o comms.serial.o memory.o
	$(SERIAL_F90) -c  ./io.f90 -cpp  -D opt_strat=$(OPT) $(FCFLAGS)

trace.o:./trace.f90
	$(SERIAL_F90) -c  ./trace.f90 $(FCFLAGS)

comms.serial.o:./comms.serial.f90 trace.o
	$(SERIAL_F90) -c  ./comms.serial.f90 $(FCFLAGS)

endif
