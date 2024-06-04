#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################
include Build/buildinfo.mk
#########################################
# User editable options

# Speed of the build, allowed: fast, debug
BUILD= debug

# Parallelisation architechture: mpi, serial
COMMS_ARCH= mpi

# Year of physical constants: 2018, 2014
CODATA_YEAR= 2018

# Spell check safety: safe, normal, risky
SAFETY= normal
########################################
#       NO CHANGES BEYOND HERE         #
########################################

# Fix choices

$(if $(filter-out serial mpi,$(COMMS_ARCH)), $(error COMMS_ARCH should be one of "mpi","serial" (not $(COMMS_ARCH))) )
$(if $(filter-out debug fast,$(BUILD)), $(error BUILD should be one of "fast","debug" (not $(BUILD))) )
$(if $(filter-out safe normal risky ,$(SAFETY)), $(error SAFETY should be one of "safe","normal","risky" (not $(SATEFY))) )	

# Version info from io.f90
version=$(shell Bin/version.sh)

GIT_VERSION=$(shell Bin/git_log.sh)
ARCH = $(shell Bin/arch $(FC))

#####################################
#      DIRECTORY LOCATIONS          #
#####################################
ROOTDIR    := $(PWD)

SOURCE_DIR = $(ROOTDIR)/Source
BIN_DIR = $(ROOTDIR)/Bin
# Create the path 
BUILD_PATH=DEReK$(version)_$(ARCH)_$(BUILD)_$(COMMS_ARCH)

#BUILD_DIR = ../Build/$(BUILD_PATH)
BUILD_DIR = $(ROOTDIR)/Build/$(BUILD_PATH)

# LibXC details
LIBXC_INC = $(BUILD_DIR)/LibXC/include
LIBXC_LIB = $(BUILD_DIR)/LibXC/lib
LIBXC_BUILD = $(BUILD_DIR)/LibXC



FFTW_FILE := $(BUILD_DIR)/fftw.dir
OPENBLAS_FILE := $(BUILD_DIR)/openblas.dir
LIBXC_FILE := $(BUILD_DIR)/libxc.dir

ifeq ($(COMMS_ARCH),serial)
    MPIC := $(FC)
endif


export FC
export MPIC
export BUILD_DIR
export FFTW_LIBS
export BLAS_LIBS
export LIBXC_INC
export LIBXC_LIB
export SOURCE_DIR
export CODATA_YEAR
export COMMS_ARCH
export GIT_VERSION
export SAFETY
export CC
export BUILD

.phony: all check_file

all: $(LIBXC_BUILD)/Makefile $(LIBXC_LIB)/libxc.a  subsystem 



fftw_file:
	@if [ ! -f $(FFTW_FILE) ]; then \
		mkdir -p $(BUILD_DIR);\
		echo "########################################################"; \
		echo "#  Please provide location of FFTW3 library files:     #"; \
		echo "########################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(FFTW_FILE); \
	fi

openblas_file:
	@if [ ! -f $(OPENBLAS_FILE) ]; then \
		mkdir -p $(BUILD_DIR);\
		echo "########################################################"; \
		echo "#  Please provide location of OPENBLAS library files:  #"; \
		echo "########################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(OPENBLAS_FILE); \
	fi
libxc_file:
	@if [ ! -f $(LIBXC_FILE) ]; then \
		mkdir -p $(BUILD_DIR);\
		echo "########################################################"; \
		echo "#  Please provide location of LIBXC library files:     #"; \
		echo "########################################################"; \
		read -p "" input_text; \
		echo "$$input_text" > $(LIBXC_FILE); \
	fi

$(LIBXC_LIB)/libxc.a: $(LIBXC_BUILD)/Makefile
	@echo 'Building LibXC'
	@$(MAKE)  -C $(LIBXC_BUILD) all install-exec install-data -s 

$(LIBXC_BUILD)/Makefile:
	@echo 'Configuring LibXC'	
	@mkdir -p $(LIBXC_BUILD) && cd $(LIBXC_BUILD) && ../../../ExLib/libxc/configure  FC=$(FC) CC=$(CC) CFLAGS=-fPIC --enable-fortran=yes --enable-shared=no --disable-fhc  --prefix=$(LIBXC_BUILD) --libdir=$(LIBXC_LIB)

subsystem:
	$(MAKE) -C $(BIN_DIR)
	$(MAKE) -C $(SOURCE_DIR)
	@rm -f $(BUILD_DIR)/sys.mod
	@rm -f $(BIN_DIR)/*.o $(BIN_DIR)/mpi_version


clean:
	rm -f $(BUILD_DIR)/*.o  $(BUILD_DIR)/*.mod  $(BUILD_DIR)/derek.mpi
clean_all :
	rm -rf  $(BUILD_DIR)
clean_everything:
	rm -rf Build/*

pack:
	./Bin/tar_maker.sh

