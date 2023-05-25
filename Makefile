#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################

SOURCE_DIR = ./Source
BUILD_DIR = ../Build
BUILD_LOC = ./Build
#########################################
# User editable options

# Fortran compiler, allowed: ifort, gfortran
F90:=gfortran

# Speed of the build, allowed: fast, debug
BUILD= debug

########################################
#       NO CHANGES BEYOND HERE         #
########################################

MPI_F90:=mpif90

FFTW_FILE := $(BUILD_LOC)/fftw.dir
OPENBLAS_FILE := $(BUILD_LOC)/openblas.dir


export F90
export MPI_F90
export BUILD
export BUILD_DIR
export SOURCE_DIR



.phony: all check_file

all: fftw_file openblas_file subsystem



fftw_file:
	@if [ ! -f $(FFTW_FILE) ]; then \
		echo "#####################################################"; \
		echo "#  Please provide location of FFTW3 library files:  #"; \
		echo "#####################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(FFTW_FILE); \
	fi

openblas_file:
	@if [ ! -f $(OPENBLAS_FILE) ]; then \
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
	rm -f  $(BUILD_LOC)/*

dist:
	tar  --exclude="./.git" --exclude="./Test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./Source/*.o" --exclude="./Source/*.mod" -cvf DEREK.tar .

