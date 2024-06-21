#!/bin/sh

# Set the source directory
SRC_DIR="../Source"

# Set the output files
MPI_DEP_FILE="dependencies_mpi.mk"
SERIAL_DEP_FILE="dependencies_serial.mk"

# List of all Fortran source files in the directory
#SRC_FILES=$(find "$SRC_DIR" -name '*.f90' ! -name 'comms.mpi.f90' ! -name 'comms.serial.f90')
SRC_FILES=$( find $SRC_DIR -type d -name DevTools -prune -o -type f -name "*.f90" ! -name "*comms*" -print)
# Function to generate the list of existing Fortran source files excluding specific files



# MPI compilation command
MPI_CMD='@$(MPIC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<'

# Serial compilation command
SERIAL_CMD='@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<'

# Generate the MPI dependency file
echo "Generating MPI dependencies..."
~/makedepf90/makedepf90 -o '$(BUILD_DIR)/derek.mpi'  -l '@echo "Linking executable and installing into " $(BUILD_DIR)/derek.\$(COMMS_ARCH)' -l '@$(MPIC) $(FCFLAGS)  $(LDFLAGS)  -o $@ $(FOBJ)' -r "@echo \"Compiling object file\" \$@ \"from\"  \$< " -r "$MPI_CMD" $SRC_DIR/comms.mpi.f90 $SRC_FILES -b '$(BUILD_DIR)'> $MPI_DEP_FILE

# Generate the Serial dependency file
echo "Generating Serial dependencies..."
~/makedepf90/makedepf90 -o '$(BUILD_DIR)/derek.serial'  -l '@echo "Linking executable and installing into " $(BUILD_DIR)/derek.\$(COMMS_ARCH)' -l '@$(MPIC) $(FCFLAGS)  $(LDFLAGS)  -o $@ $(FOBJ) '  -r "@echo \"Compiling object file\" \$@ \"from\"  \$< " -r "$SERIAL_CMD" $SRC_DIR/comms.serial.f90 $SRC_FILES -b '$(BUILD_DIR)' > $SERIAL_DEP_FILE

sed -i '' 's/=/+=/g' $MPI_DEP_FILE
sed -i '' 's/=/+=/g' $SERIAL_DEP_FILE


echo "Dependency files generated:"
echo "  MPI:    $MPI_DEP_FILE"
echo "  Serial: $SERIAL_DEP_FILE"
