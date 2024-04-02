!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module spec
  !Impose strong typing
  use constants
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise
  use comms,only : rank, nprocs,comms_arch,on_root_node,max_version_length,COMMS_FINALISE&
       &,comms_library_version,comms_version
  use memory, only : memory_init,memory_allocate,memory_deallocate
  use iso_fortran_env, only : compiler_version
  implicit none


contains

  subroutine spec_bands
    call trace_entry('spec_bands')
    
    call trace_exit('spec_bands')
  end subroutine spec_bands


  subroutine spec_dos
    call trace_entry('spec_dos')
    
    call trace_exit('spec_dos')
  end subroutine spec_dos

end module spec
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
