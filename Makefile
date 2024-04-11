#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################

#########################################
# User editable options

# Fortran compiler, allowed: ifort, gfortran
F90:=gfortran

# Speed of the build, allowed: fast, debug
BUILD= debug

# Parallelisation architechture: mpi, serial
COMMS_ARCH= mpi

# Year of physical constants: 2018, 2014
CODATA_YEAR= 2018

########################################
#       NO CHANGES BEYOND HERE         #
########################################

MPI_F90:=mpif90


# Fix choices

$(if $(filter-out serial mpi,$(COMMS_ARCH)), $(error COMMS_ARCH should be one of "mpi","serial" (not $(COMMS_ARCH))) )
$(if $(filter-out debug fast,$(BUILD)), $(error BUILD should be one of "fast","debug" (not $(BUILD))) )
$(if $(filter-out gfortran ifort ,$(F90)), $(error F90 should be one of "gfortran","ifort" (not $(F90))) )
# Version info from io.f90
version=$(shell Bin/version.sh)
GIT_VERSION=$(shell Bin/git_log.sh)
ARCH = $(shell Bin/arch)
#####################################
#      DIRECTORY LOCATIONS          #
#####################################

SOURCE_DIR = ./Source

# Create the path 
BUILD_PATH=DEReK$(version)_$(ARCH)_$(BUILD)_$(COMMS_ARCH)

BUILD_DIR = ../Build/$(BUILD_PATH)
BUILD_LOC = ./Build/$(BUILD_PATH)


FFTW_FILE := $(BUILD_LOC)/fftw.dir
OPENBLAS_FILE := $(BUILD_LOC)/openblas.dir

ifeq ($(COMMS_ARCH),serial)
    MPI_F90 := $(F90)
endif


export F90
export MPI_F90
export BUILD
export BUILD_DIR
export SOURCE_DIR
export CODATA_YEAR
export COMMS_ARCH
export GIT_VERSION

.phony: all check_file

all: fftw_file openblas_file subsystem



fftw_file:
	@if [ ! -f $(FFTW_FILE) ]; then \
		mkdir -p $(BUILD_LOC);\
		echo "########################################################"; \
		echo "#  Please provide location of FFTW3 library files:     #"; \
		echo "########################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(FFTW_FILE); \
	fi

openblas_file:
	@if [ ! -f $(OPENBLAS_FILE) ]; then \
		mkdir -p $(BUILD_LOC);\
		echo "########################################################"; \
		echo "#  Please provide location of OPENBLAS library files:  #"; \
		echo "########################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(OPENBLAS_FILE); \
	fi


subsystem:
	$(MAKE) -C $(SOURCE_DIR)


clean:
	rm -f $(BUILD_LOC)/*.o  $(BUILD_LOC)/*.mod  $(BUILD_LOC)/derek.mpi
clean_all :
	rm -rf  $(BUILD_LOC)
clean_everything:
	rm -rf Build/*

dist:
	tar  --exclude="./.git" --exclude="./Test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./Source/*.o" --exclude="./Source/*.mod" -cvf DEREK.tar .

