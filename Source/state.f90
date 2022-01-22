!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module state
  use comms, only : on_root_node,rank,nprocs,dist_kpt,dist_gvec
  use io,    only : current_structure,current_params,stdout,parameters,structure
  use basis, only : current_basis,basis_dat
  use wave,  only : wavefunction,wave_allocate
  use pot,   only : potential, pot_allocate,pot_external_pot
  use trace, only : trace_entry,trace_exit,dp
  use memory,only : memory_allocate,memory_deallocate
  use density,only : density_allocate,elec_den
  type,public :: state_data
     type(wavefunction)   :: wfn     ! The current wavefunction
     type(basis_dat)      :: basis   ! The current basis
     type(structure)      :: struct  ! The current structure
     type(parameters)     :: params  ! The current parameters
     type(potential)      :: tot_pot ! Total potential
     type(potential)      :: ext_pot ! external potential
     type(elec_den)       :: den     ! The electron density
     real(dp),allocatable :: occ(:,:)! Occupancy
  end type state_data

  type(state_data), public,save ::  current_state

  public state_init

contains


  subroutine state_init()
    !==============================================================================!
    !                             S T A T E _ I N I T                              !
    !==============================================================================!
    ! Subroutine for initialising the state variable and filling it with all of    !
    ! the big data types that we need                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    call trace_entry('state_init')
    ! This is just going to allocate all of the things in state

    ! First off lets initialise the wavefunciton
    call wave_allocate(current_state%wfn,current_params%nbands)
    ! potential
    call pot_allocate(current_state%tot_pot)
    call pot_allocate(current_state%ext_pot)


    call memory_allocate(current_state%occ,1,current_params%nbands,1,current_structure%num_kpoints,'G')

    current_state%basis=current_basis
    current_state%struct=current_structure
    current_state%params=current_params
    call density_allocate(current_state%den)
    ! read the external potential
    call pot_external_pot(current_state%ext_pot)



    call trace_exit('state_init')
  end subroutine state_init

end module state
