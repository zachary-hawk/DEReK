# Set the files with the directories for the libraries

# Find the system
VAR := $(shell . ./../Bin/arch_finder)
OS := $(shell uname -s)

MPI_VERSION := $(shell  ./../Bin/mpi_version)

ifeq ($(strip $(OS)),Darwin)
CPU := $(shell . ./../Bin/CPU_DARWIN)
endif

ifeq ($(strip $(OS)),Linux)
CPU := $(shell . ./../Bin/CPU_LINUX)
endif

ifeq ($(strip $(VAR)),)
VAR='Unknown'
endif

PHYSICAL_CORES=$(shell if [ "$$(uname)" = "Linux" ]; then grep -c '^processor' /proc/cpuinfo; elif [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.physicalcpu; fi)

# Determine the number of logical cores
LOGICAL_CORES=$(shell if [ "$$(uname)" = "Linux" ]; then grep -c '^processor' /proc/cpuinfo; elif [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.logicalcpu; fi)

# Determine the total amount of system memory in GB
TOTAL_MEMORY=$(shell if [ "$$(uname)" = "Linux" ]; then awk '/MemTotal/ {print $$2}' /proc/meminfo; elif [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.memsize; fi)
TOTAL_MEMORY_GB=$(shell echo "scale=2; $$(echo $$(if [ "$$(uname)" = "Linux" ]; then awk '/MemTotal/ {print $$2}' /proc/meminfo; elif [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.memsize; fi)) / 1024 / 1024 / 1024" | bc)




# Constants data
ifeq ($(CODATA_YEAR),2018)
DATFLAGS = -DCODATA2018
endif
ifeq ($(CODATA_YEAR),2014)
DATFLAGS = -DCODATA2014
endif


FCFLAGS += -fPIC -fbackslash -cpp $(DATFLAGS) 

LDFLAGS =  $(FFTW_LIBS) -lxc  -lxcf03   -L$(LIBXC_LIB) -I$(LIBXC_INC) $(BLAS_LIBS)

SYS_INFO:=$(shell ../Bin/gen_sys.py "$(GIT_VERSION)" "$(VAR)" "$(CPU)" "$(OPT)" "$(CODATA_YEAR)" "$(SAFETY)" '$(MPI_VERSION)' '$(PHYSICAL_CORES)' '$(LOGICAL_CORES)' '$(TOTAL_MEMORY_GB)')

FCFLAGS += -I$(BUILD_DIR) -J$(BUILD_DIR) 

# Set up the C compile steps for the fftw version numbers
CCFLAGS= -w 


$(BUILD_DIR)/constants.o: FCFLAGS += -cpp $(DATFLAGS) 

$(BUILD_DIR)/sys.o: FCFLAGS += -cpp 

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(BUILD_DIR)
	@echo "Compiling fftw3 version"
	@$(CC) $(CCFLAGS) -o $@ -c $< 

# Explicitly include fftw3_version.o

FOBJ = fftw3_version.o


ifeq ($(COMMS_ARCH),serial) 
include ../Bin/dependencies_serial.mk
else
include ../Bin/dependencies_mpi.mk
endif


#$(BUILD_DIR)/%.o: %.f90
#	@mkdir -p $(BUILD_DIR)
#	@echo "Compiling object file" $@ "from"  $< 
#	@$(MPIC)  $(FCFLAGS)  $(LDFLAGS) -MD  -o $@ -c $<
#
#
#$(BUILD_DIR)/%.o: %.$(COMMS_ARCH).f90
#	@mkdir -p $(BUILD_DIR)
#	@echo "Compiling object file" $@ "from"  $< 
#	@$(MPIC)  $(FCFLAGS)  $(LDFLAGS) -MD  -o $@ -c $<

#-include $(DEP_FILES)

#$(BUILD_DIR)/derek.$(COMMS_ARCH): $(OBJ)
#	@mkdir -p $(BUILD_DIR)
#	@echo "Linking executable and installing into " $(BUILD_DIR)/derek.$(COMMS_ARCH)
#	@$(MPIC) $(FCFLAGS)  $(LDFLAGS)  -o $@ $(OBJ)  
