SOURCE=$(shell cat include_dependencies.mk)

FFTWDIR=$(shell cat $(BUILD_DIR)/fftw.dir)
OPENBLASDIR=$(shell cat $(BUILD_DIR)/openblas.dir)

#OBJ = $(SOURCE:.f90=.o)
OBJ=$(patsubst %.f90,$(BUILD_DIR)/%.o,$(notdir $(SOURCE)))



ifeq ($(BUILD),fast)
OPT= '"FAST"'
ifeq ($(F90),gfortran)
FCFLAGS= -fconvert=big-endian -fno-realloc-lhs -fopenmp -fPIC -O3 -funroll-loops -fno-signed-zeros -g -fbacktrace -march=native -mno-avx -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch  -L$(FFTWDIR) -L$(OPENBLASDIR)    -D opt_strat=$(OPT)
endif
ifeq ($(F90),ifort)
FCFLAGS=-convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -O3 -debug minimal -traceback -xHost -fbackslash -lfftw3 -lopenblas -L$(FFTWDIR) -L$(OPENBLASDIR)    -D opt_strat=$(OPT) 
endif

endif

ifeq ($(BUILD),debug)
OPT= '"DEBUG"'
ifeq ($(F90),gfortran)
FCFLAGS= -static-libgfortran -Ddebug -O0 -g -fbounds-check -fbacktrace -Wall -Waliasing -Wsurprising -Wline-truncation -Wno-tabs -Wno-uninitialized -Wno-unused-dummy-argument -Wno-unused -Wno-character-truncation -Wl,-no_pie -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch    -D opt_strat=$(OPT) -L$(FFTWDIR) -L$(OPENBLASDIR)
endif
ifeq ($(F90),ifort)
FCFLAGS= -convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -Ddebug -O0 -g -debug extended -traceback -C -stand f03 -warn all -diag-disable 7025 -diag-disable 7712 -diag-disable 5268 -diag-disable 7847 -diag-disable 7893 -diag-disable 6439 -fbackslash -lfftw3 -lopenblas    -D opt_strat=$(OPT) -L$(FFTWDIR) -L$(OPENBLASDIR) 
 endif
endif

$(BUILD_DIR)/io.o: FCFLAGS += -cpp

$(BUILD_DIR)/%.o: %.f90
	@echo "Compiling object files"

	mpif90 $(FCFLAGS) -o $@ -c $<

$(BUILD_DIR)/%.mod: $(OBJ)

$(BUILD_DIR)/derek.mpi: $(OBJ)
	@echo "Linking executable"
	mpif90 $(FCFLAGS) -o $@ $(OBJ)


