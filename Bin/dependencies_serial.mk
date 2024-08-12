FOBJ+=$(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/constants.o $(BUILD_DIR)/state.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/grids.o $(BUILD_DIR)/pot.o $(BUILD_DIR)/fft.o $(BUILD_DIR)/wave.o $(BUILD_DIR)/license.o $(BUILD_DIR)/derek.o $(BUILD_DIR)/spectral.o $(BUILD_DIR)/electronic.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/xsf.o $(BUILD_DIR)/utils.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/units.o $(BUILD_DIR)/sys.o $(BUILD_DIR)/xc.o $(BUILD_DIR)/density.o $(BUILD_DIR)/io.o 

$(BUILD_DIR)/derek.serial: $(FOBJ)
	@$(MPIC) $(FCFLAGS)  $(LDFLAGS)  -o $@ $(FOBJ) 

$(BUILD_DIR)/comms.serial.o : ../Source/comms.serial.f90 $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/constants.o : ../Source/constants.f90 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/state.o : ../Source/state.f90 $(BUILD_DIR)/xsf.o $(BUILD_DIR)/density.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/pot.o $(BUILD_DIR)/wave.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/io.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/trace.o : ../Source/trace.f90 $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/grids.o : ../Source/grids.f90 $(BUILD_DIR)/wave.o $(BUILD_DIR)/density.o $(BUILD_DIR)/pot.o $(BUILD_DIR)/fft.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/io.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/pot.o : ../Source/pot.f90 $(BUILD_DIR)/wave.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/io.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/fft.o : ../Source/fft.f90 $(BUILD_DIR)/io.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/wave.o : ../Source/wave.f90 $(BUILD_DIR)/memory.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/io.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/license.o : ../Source/license.f90 $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/io.o $(BUILD_DIR)/sys.o $(BUILD_DIR)/trace.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/derek.o : ../Source/derek.f90 $(BUILD_DIR)/xc.o $(BUILD_DIR)/grids.o $(BUILD_DIR)/electronic.o $(BUILD_DIR)/utils.o $(BUILD_DIR)/fft.o $(BUILD_DIR)/pot.o $(BUILD_DIR)/state.o $(BUILD_DIR)/wave.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/io.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/license.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/spectral.o : ../Source/spectral.f90 $(BUILD_DIR)/memory.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/electronic.o : ../Source/electronic.f90 $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/state.o $(BUILD_DIR)/io.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/basis.o : ../Source/basis.f90 $(BUILD_DIR)/fft.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/io.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/xsf.o : ../Source/xsf.f90 $(BUILD_DIR)/memory.o $(BUILD_DIR)/units.o $(BUILD_DIR)/basis.o $(BUILD_DIR)/io.o $(BUILD_DIR)/constants.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/utils.o : ../Source/utils.f90 $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/io.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/memory.o : ../Source/memory.f90 $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/units.o : ../Source/units.f90 $(BUILD_DIR)/constants.o $(BUILD_DIR)/trace.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/sys.o : ../Source/sys.f90 $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/xc.o : ../Source/xc.f90 $(BUILD_DIR)/io.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/density.o : ../Source/density.f90 $(BUILD_DIR)/basis.o $(BUILD_DIR)/memory.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/wave.o $(BUILD_DIR)/io.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o $(BUILD_DIR)/constants.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
$(BUILD_DIR)/io.o : ../Source/io.f90 $(BUILD_DIR)/memory.o $(BUILD_DIR)/comms.serial.o $(BUILD_DIR)/trace.o $(BUILD_DIR)/units.o 
	@echo "Compiling object file" $@ "from"  $< 
	@$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ -c $<
