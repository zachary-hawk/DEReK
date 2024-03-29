!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module state
  use constants
  use comms, only : on_root_node,rank,nprocs,dist_kpt,dist_gvec,comms_wall_time
  use io,    only : current_structure,current_params,stdout,parameters,structure,seed,io_errors,seed,io_flush
  use basis, only : current_basis,basis_dat
  use wave,  only : wavefunction,wave_allocate,wave_initialise
  use pot,   only : potential, pot_allocate,pot_external_pot,pot_writef
  use trace, only : trace_entry,trace_exit
  use memory,only : memory_allocate,memory_deallocate
  use density,only : density_allocate,elec_den,density_writef
  type,public :: state_data
     type(wavefunction)   :: wfn     ! The current wavefunction
     type(basis_dat)      :: basis   ! The current basis
     type(structure)      :: struct  ! The current structure
     type(parameters)     :: params  ! The current parameters
     type(potential)      :: tot_pot ! Total potential
     type(potential)      :: ext_pot ! external potential
     type(elec_den)       :: den     ! The electron density
     real(dp),allocatable :: occ(:,:)! Occupancy
     logical              :: converged ! Convergence flag
     real(dp)             :: total_energy ! total energy
     real(dp)             :: efermi ! Fermi energy of system
     real(dp),dimension(3):: total_spin
     real(dp),dimension(3):: total_modspin
     real(dp)             :: tot_moment ! Fermi energy of system
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
    call wave_initialise(current_state%wfn)

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



  subroutine state_finalise()
    !==============================================================================!
    !                         S T A T E _ F I N A L I S E                          !
    !==============================================================================!
    ! 4Subroutine for cleaning up and writing out state information.               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!

    integer       :: pot_file,den_file,wfn_file,state_file
    character(40) :: file_name,loc_pot_type
    character(100):: write_str
    integer:: stat
    integer:: line_len = 50
    integer:: loc_line
    logical:: no_files = .true.
    real(dp):: wall_time
    ! Bit of a cheat, but this should be categorized as an io routine, but that isnt possible 
    call trace_entry("io_data_dump")
    if (on_root_node)then 
       write(stdout,*)
       write(stdout,*)"+-------------------------------------------------------+----------+"
       write(stdout,*)"|          A U X .  F I L E  W R I T I N G              | Time (s) |"
       write(stdout,*)"+-------------------------------------------------------+----------+"

    end if
    ! Write out the externalu potential file

    if (current_params%write_potex)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       if (current_state%ext_pot%preset)then
          select case(current_params%external_pot)
          case('finite_barrier')
             loc_pot_type = 'FB'
          case('periodic_potential')
             loc_pot_type = 'PP'
          case('jelly')
             loc_pot_type = 'jelly'
          end select

          write(file_name,*)trim(seed)//'-'//trim(loc_pot_type)//'.potex'
          open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED')
       else
          write(file_name,*)trim(seed)//'.potex'
          open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED')
       end if
       write_str = "Ext. potential to "
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )


       wall_time = comms_wall_time()
       if (on_root_node)then
          write(pot_file)current_state%ext_pot

          if (current_params%iprint.ge.1)write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
          close(pot_file)
       end if

    end if
    ! formatted external potential
    if (current_params%write_formatted_potex)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       if (current_state%ext_pot%preset)then         
          select case(current_params%external_pot)
          case('finite_barrier')
             loc_pot_type = 'FB'
          case('periodic_potential')
             loc_pot_type = 'PP'
          case('jelly')
             loc_pot_type = 'jelly'
          end select

          write(file_name,*)trim(seed)//'-'//trim(loc_pot_type)//'.fpotex'        
          open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='FORMATTED')
       else
          write(file_name,*)trim(seed)//'.fpotex'
          open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='FORMATTED',RECL=8192)
       end if
       write_str = "Fmt. ext. potential to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then
          call pot_writef(current_state%ext_pot,pot_file)

          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
       end if
       close(pot_file)
    end if



    ! TOTAL POTENTIAL

    if (current_params%write_potential)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       write(file_name,*)trim(seed)//'.pot'
       open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED')

       write_str = "Total potential to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then

          write(pot_file)current_state%tot_pot

          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
       end if
       close(pot_file)
    end if

    ! formatted total potential
    if (current_params%write_formatted_potential)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       write(file_name,*)trim(seed)//'.fpot'
       open(newunit=pot_file,file=adjustl(file_name),status="unknown",form='FORMATTED',RECL=8192)

       write_str = "Fmt. potential to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then
          call pot_writef(current_state%tot_pot,pot_file)
          close(pot_file)
          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
       end if
       close(pot_file)
    end if

    ! TOTAL density
    if (current_params%write_density)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       write(file_name,*)trim(seed)//'.den'
       open(newunit=den_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED')

       write_str = "Density to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then
          write(den_file)current_state%den

          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
       end if
       close(den_file)
    end if

    ! formatted total density
    if (current_params%write_formatted_density)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       write(file_name,*)trim(seed)//'.fden'
       open(newunit=den_file,file=adjustl(file_name),status="unknown",form='FORMATTED',RECL=8192)
       write_str = "Fmt. density to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then
          call density_writef(current_state%den,den_file)

          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)          
       end if
       close(den_file)
    end if
    if (current_params%write_wvfn)then
       no_files=.false.
       ! Now we have calculated it we can write it if needed
       write(file_name,*)trim(seed)//'.wvfn'
       open(newunit=wfn_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED')


       write_str = "Wavefunction to"
       loc_len = line_len - ( len(trim(write_str)) +  len(trim(file_name)) )         
       wall_time = comms_wall_time()
       if (on_root_node)then
          write(wfn_file)current_state%wfn


          if (current_params%iprint.ge.1)&
               & write(stdout,10)trim(write_str),repeat('-',loc_len),trim(file_name),wall_time
          call io_flush(stdout)
       end if
       close(wfn_file)
    end if
    if (no_files)write(stdout,11)"No Auxiliary files to be written",wall_time

    write(stdout,*)"+-------------------------------------------------------+----------+"
    write(stdout,*)



10  format(1x,'|',1x a,1x,a,'>',1x,a,T58,'|',1x,f8.2,T69,'|')
11  format(1x,'|',1x a,T58,'|',1x,f8.2,T69,'|')
    !!
!!$    ! write the state
!!$    if (current_params%write_state)then
!!$       ! Now we have calculated it we can write it if needed
!!$       write(file_name,*)trim(seed)//'.state'
!!$       open(newunit=state_file,file=adjustl(file_name),status="unknown",form='UNFORMATTED',access='direct',RECL=465465465)
!!$       if (current_params%iprint.ge.1)write(stdout,*)"Writing final state to "//trim(file_name)
!!$       call state_write(current_state,state_file,stat)
!!$       close(state_file)
!!$    end if
!!$



    call trace_exit("io_data_dump")

  end subroutine state_finalise


  subroutine state_write(model,unit,iostat)
    !==============================================================================!
    !                            S T A T E _ W R I T E                             !
    !==============================================================================!
    ! Subroutine for writing out the state file, potentially used for restarts     !
    ! and also forthcoming python postprocessing.                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           model,             intent :: in                                    !
    !           unit,              intent :: in                                    !
    !           iostat,            intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!

    class(state_data), intent(in)    :: model
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat

    integer :: stat,state_file


    call trace_entry('state_write')
    if (on_root_node)then
       ! First write the current params because its easiest
       current_params%restart = .false. ! just make sure..
       write(unit,rec=1)current_params

       ! Now do the strucure
       write(unit,rec=2)    current_structure%num_kpoints
       write(unit,rec=3)    current_structure%cell 
       write(unit,rec=4)    current_structure%inv_cell
       write(unit,rec=5)    current_structure%kpt_scf_list 
       write(unit,rec=6)    current_structure%lattice_a
       write(unit,rec=7)    current_structure%lattice_b
       write(unit,rec=8)    current_structure%lattice_c
       write(unit,rec=9)    current_structure%alpha
       write(unit,rec=10)    current_structure%beta
       write(unit,rec=11)    current_structure%gamma
       write(unit,rec=12)    current_structure%volume

       ! Basis
       write(unit,rec=13)current_basis%ngx,ngy,ngz ! Standard grid dimensions
       write(unit,rec=14)current_basis%num_grid_points
       write(unit,rec=15)current_basis%num_node
       write(unit,rec=16)current_basis%max_node
       write(unit,rec=17)current_basis%grid_points
       write(unit,rec=18)current_basis%real_grid_points
       write(unit,rec=19)current_basis%frac_points

       write(unit,rec=20)current_basis%fine_ngx,fine_ngy,fine_ngz ! Fine grid dimensions
       write(unit,rec=21)current_basis%num_fine_grid_points
       write(unit,rec=22)current_basis%num_fine_node
       write(unit,rec=23)current_basis%max_fine_node
       write(unit,rec=24)current_basis%fine_grid_points
       write(unit,rec=25)current_basis%real_fine_grid_points
       write(unit,rec=26)current_basis%fine_frac_points


       ! potentials
       write(unit,rec=27)model%tot_pot
       write(unit,rec=28)model%ext_pot

       ! density
       write(unit,rec=29)model%den

       ! occupancy
       write(unit,rec=30)model%occ

       ! wavefunction
       write(unit,rec=31)model%wfn
    end  if

    call trace_exit('state_write')


    return

  end subroutine state_write


  subroutine state_restart()
    !==============================================================================!
    !                          S T A T E _ R E S T A R T                           !
    !==============================================================================!
    ! Subroutine for initiating a restart job from a .state file.                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    type(parameters) :: temp_param
    type(structure)  :: temp_structure
    integer  :: state_file, stat
    call trace_entry('state_restart')


    ! Open the file
    open(newunit=state_file,file=trim(seed)//'.state',status="old",form='UNFORMATTED',access='direct',RECL=8192)
    !if (stat.ne.0)call io_errors("Error in state_restart: No file "//trim(seed)//'.state')
    write(stdout,*)"Restarting from state file "//trim(seed)//'.state' 


    ! Read in the params
    read(state_file,rec=1)temp_param


    ! Now do the strucure
    read(state_file,rec=2)    temp_structure%num_kpoints

    read(state_file)    temp_structure%cell 
    read(state_file)    temp_structure%inv_cell
    call memory_allocate(temp_structure%kpt_scf_list,1,temp_structure%num_kpoints,1,3,"I")
    read(state_file)    temp_structure%kpt_scf_list 
    read(state_file)    temp_structure%lattice_a
    read(state_file)    temp_structure%lattice_b
    read(state_file)    temp_structure%lattice_c
    read(state_file)    temp_structure%alpha
    read(state_file)    temp_structure%beta
    read(state_file)    temp_structure%gamma
    read(state_file)    temp_structure%volume



    
    call trace_exit('state_restart')
    return 
  end subroutine state_restart


end module state
