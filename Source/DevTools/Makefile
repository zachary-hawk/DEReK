# Makefile for compiling and installing AuthorManager and AddParams

# Targets
all: $(DEVTOOLS)/AuthorManager $(DEVTOOLS)/AddParams

$(DEVTOOLS)/AuthorManager: AuthorManager.f90
	mkdir -p $(DEVTOOLS)
	$(FC) $(FCFLAGS) -o $@ $<

$(DEVTOOLS)/AddParams: add_params.f90
	mkdir -p $(DEVTOOLS)
	$(FC) $(FCFLAGS) -o $@ $<	


.PHONY: all 
