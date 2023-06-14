# Start by calculating the dependencies
$(shell ../Bin/dependencies.py $(COMMS_ARCH) > include_dependencies.mk)
SOURCE=$(shell cat include_dependencies.mk)

# Set the files with the directories for the libraries
FFTWDIR=$(shell cat $(BUILD_DIR)/fftw.dir)
OPENBLASDIR=$(shell cat $(BUILD_DIR)/openblas.dir)

#OBJ = $(SOURCE:.f90=.o)
#OBJ=$(patsubst %.f90,$(BUILD_DIR)/%.o,$(notdir $(SOURCE)))
#OBJ = $(patsubst %.f90,%.o,$(patsubst %.mpi.f90,%.o,$(wildcard *.f90 *.mpi.f90)))

# Calculate the object files
OBJ = $(patsubst %.f90,$(BUILD_DIR)/%.o,$(patsubst %.mpi.f90,$(BUILD_DIR)/%.o,$(SOURCE)))


# Set up the compiler options, for compiler type and build speed
ifeq ($(BUILD),fast)
OPT= '"FAST"'
ifeq ($(F90),gfortran)
FCFLAGS= -fconvert=big-endian -fno-realloc-lhs -fopenmp -fPIC -O3 -funroll-loops -fno-signed-zeros -g -fbacktrace -march=native -mno-avx -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch  -I$(FFTWDIR) -L$(OPENBLASDIR)    -D opt_strat=$(OPT)
endif

ifeq ($(F90),ifort)
FCFLAGS=-convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -O3 -debug minimal -traceback -xHost -fbackslash -lfftw3 -lopenblas -I$(FFTWDIR) -L$(OPENBLASDIR)    -D opt_strat=$(OPT) 
endif
endif

ifeq ($(BUILD),debug)
OPT= '"DEBUG"'
ifeq ($(F90),gfortran)
FCFLAGS= -static-libgfortran -Ddebug -O0 -g -fbounds-check -fbacktrace -Wall -Waliasing -Wsurprising -Wline-truncation -Wno-tabs -Wno-uninitialized -Wno-unused-dummy-argument -Wno-unused -Wno-character-truncation -Wl,-no_pie -fbackslash -lfftw3 -lopenblas -w -fallow-argument-mismatch    -D opt_strat=$(OPT) -I$(FFTWDIR) -L$(OPENBLASDIR)
endif

ifeq ($(F90),ifort)
FCFLAGS= -convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -Ddebug -O0 -g -debug extended -traceback -C -stand f03 -warn all -diag-disable 7025 -diag-disable 7712 -diag-disable 5268 -diag-disable 7847 -diag-disable 7893 -diag-disable 6439 -fbackslash -lfftw3 -lopenblas    -D opt_strat=$(OPT) -I$(FFTWDIR) -L$(OPENBLASDIR) 
 endif
endif


# Constants data
ifeq ($(CODATA_YEAR),2018)
DATFLAGS = -DCODATA2018
endif
ifeq ($(CODATA_YEAR),2014)
DATFLAGS = -DCODATA2014
endif

# .mod files
FCFLAGS += -I$(BUILD_DIR) -J$(BUILD_DIR)





$(BUILD_DIR)/io.o: FCFLAGS += -cpp
$(BUILD_DIR)/constants.o: FCFLAGS += -cpp $(DATFLAGS)



$(BUILD_DIR)/%.o: %.f90
	@mkdir -p $(BUILD_DIR)
	@echo "Compiling object file" $@ "from"  $< 
	$(MPI_F90)  $(FCFLAGS)  -o $@ -c $<

$(BUILD_DIR)/%.o: %.$(COMMS_ARCH).f90
	@mkdir -p $(BUILD_DIR)
	@echo "Compiling object file" $@ "from"  $< 
	$(MPI_F90)  $(FCFLAGS)   -o $@ -c $<

$(BUILD_DIR)/derek.$(COMMS_ARCH): $(OBJ)
	@mkdir -p $(BUILD_DIR)
	@echo "Linking executable and installing into " $(BUILD_DIR)/derek.$(COMMS_ARCH)
	$(MPI_F90) $(FCFLAGS) -o $@ $(OBJ) 

.PHONY: print_obj_files
print_obj_files:
	@echo $(OBJ)
