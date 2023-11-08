!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
program derek
  use constants
  use comms!, only: comms_init, comms_finalise,rank,comms_distribute
  use trace, only: trace_entry,trace_exit,trace_init,trace_finalise
  use io   , only: io_initialise, io_errors,current_params,seed,stdout,&
       & io_dryrun,current_structure,io_dist_kpt,io_finalise, io_write_params,io_warnings
  use memory,only: memory_report,memory_deallocate
  use basis, only: basis_init,current_basis, basis_recip2real, basis_real2recip
  use wave,  only: wave_allocate,wavefunction_slice,wavefunction,operator (+),operator(-),operator(*)
  use state, only: state_data, current_state, state_init,state_finalise,state_restart
  use comms, only: rank,on_root_node
  use pot
  use utils
  implicit none

  real(dp)                               :: efficiency
  real(dp)                               :: start_time,end_time,current_time,time
  integer :: ig,ik


  ! TESTING
  ! Matrix and variables declaration
  real(kind=dp), allocatable,dimension(:,:) :: matrix, inverted_matrix,result
  integer :: i, j,n=3




  call trace_init()           ! Set up trace 
  call trace_entry("derek")   
  call comms_init()           ! Start the MPI        
  call io_initialise()        ! Open up the files and read

  if (current_params%restart) then
     call io_warnings("Restart currently unavailable, continuing without.")
     !call state_restart()
  end if
  call utils_init_random()    ! Set the random seed
  call io_write_params()      ! Write out all the parameters
  call basis_init()           ! Set up basis data
  call io_dist_kpt()          ! Distribute the k-points

  ! Workout the distribution
  call comms_distribute(current_structure%kpts_on_node,&
       & current_basis%local_grid_points,&
       & current_basis%local_fine_grid_points,&
       & current_structure%num_kpoints,&
       & current_basis%num_grid_points,&
       & current_basis%num_fine_grid_points,&
       & current_basis%num_node,&
       & current_basis%num_fine_node)


  ! allocate all of the things held in the state
  call state_init()

  current_state%ext_pot=1.0_dp*current_state%ext_pot


  ! report the memory usage, probably won't need much more memory stuff after this 
  call memory_report(stdout,current_params%calc_memory)

  ! Now check if its a dry run before going on
  if (current_params%dryrun)then
     call io_dryrun()
  end if


  !call basis_real2recip(current_state%ext_pot%nc_pot,'FINE')


  

  
  ! We can start tidying up now

  call state_finalise()
  
  if (on_root_node)call io_finalise()
  call trace_exit('derek')
  call trace_finalise(current_params%debugging,rank,seed=seed)


  call comms_reduce(global_time,1,"max")
  time=global_time

  if (on_root_node)then
     efficiency=(1.0_dp-comms_time/time)*100.0_dp
     write(stdout,'(1x,"|",5x,"Total time: ",f10.2,1x,"s",37x,"|")')time
     if (nprocs.gt.1) write(stdout,'(1x,"|",5x,"Efficiency: ",f10.2,1x,"%",37x,"|")')efficiency
     write(stdout,*)"+"//repeat("-",66)//"+"
  end if
  call comms_finalise()



end program derek
