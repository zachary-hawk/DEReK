!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
program derek
  use constants
  use comms
  use trace, only: trace_entry,trace_exit,trace_init,trace_finalise
  use io   , only: io_initialise, io_errors,current_params,seed,stdout,&
       & io_check,current_structure,io_dist_kpt,io_finalise, io_write_params,io_warnings,&
       & io_from_atomic,io_to_atomic,io_conversion
  use memory,only: memory_deallocate
  use basis, only: basis_init,current_basis, basis_recip2real, basis_real2recip
  use wave,  only: wave_allocate,wavefunction_slice,wavefunction,operator (+),operator(-),operator(*)
  use state, only: state_data, current_state, state_init,state_finalise,state_restart
  use comms, only: rank,on_root_node
  use pot
  use fft, only : fft_1d
  use utils
  use electronic, only : electronic_minimise
  implicit none

  real(dp)                               :: efficiency
  real(dp)                               :: start_time,end_time,current_time,time
  integer :: ig,ik


  ! TESTING
  ! Matrix and variables declaration
  real(kind=dp), allocatable,dimension(:,:) :: matrix, inverted_matrix,result
  integer :: i, j,n=3
  complex(kind=dp) , allocatable,dimension(:) :: test_fft
  real(kind=dp) , allocatable,dimension(:) :: test_fft_x
  real(dp) :: testvar


  call trace_init()           ! Set up trace
  call trace_entry("derek")
  call constants_initialise()
  call comms_init()           ! Start the MPI
  call io_initialise()        ! Open up the files and read


  if (current_params%restart) then
     call io_warnings("Restart currently unavailable, continuing without.")
     !call state_restart()
  end if
  call utils_init_random()    ! Set the random seed 
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

  !current_state%ext_pot=1.0_dp*current_state%ext_pot
  call io_write_params(current_basis%num_grid_points&
       &, current_basis%num_fine_grid_points&
       &, current_basis%ngx&
       &, current_basis%ngy&
       &, current_basis%ngz&
       &, current_basis%fine_ngx&
       &, current_basis%fine_ngy&
       &, current_basis%fine_ngz)      ! Write out all the parameters, need to pass it the basis info 

  ! report the memory usage, probably won't need much more memory stuff after this 
  !call memory_report(stdout,current_params%iprint,current_params%calc_memory)

  ! Now check if its a dry run before going on
  if (current_params%check)then
     call io_check()
  end if


  ! After the initialisation is finished, we move onto the electronic minimisation..
  ! Calculation starts here, all before this point was initialisation, we report it in the output
  call electronic_minimise()


  !call basis_real2recip(current_state%ext_pot%nc_pot,'FINE')

  !print*,"RANK:",rank,"BASIS:",current_basis%local_grid_points



  ! doing some big testing here
!!$
!!$  call memory_allocate(test_fft,1,current_basis%ngx,'B')
!!$  call memory_allocate(test_fft_x,1,current_basis%ngx,'B')
!!$
!!$
!!$  ! define a function
!!$  test_fft_x =  [(1.0*(i-1)/(current_basis%ngx-1), i=1,current_basis%ngx)]
!!$
!!$  test_fft = exp(-(test_fft_x-0.5_dp)**2/(0.03_dp)**2)!sin(test_fft_x*pi*2) + sin(4*test_fft_x*pi*2)
!!$  test_fft = [(i*0.0_dp,i=1,current_basis%ngx)]
!!$  test_fft(43:63) = 1.0_dp
!!$  do i = 1, current_basis%ngx
!!$     write(120,*) test_fft_x(i),real(test_fft(i))
!!$  end do
!!$
!!$
!!$  i=1
!!$  call fft_1d(test_fft,'STD',i)
!!$  do i = 1, current_basis%ngx
!!$     write(121,*) test_fft_x(i),real(test_fft(i))
!!$  end do



  ! We can start tidying up now

  call state_finalise()

  if (on_root_node)call io_finalise()
  call trace_exit('derek')
  call trace_finalise(current_params%debugging,rank,seed=seed)


  !call comms_reduce(global_time,1,"max")

  time=comms_wall_time()


  if (on_root_node)then
     efficiency=(1.0_dp-comms_time/time)*100.0_dp
     write(stdout,'(1x,"|",5x,"Total time: ",f10.2,1x,"s",37x,"|")')time
     if (nprocs.gt.1) write(stdout,'(1x,"|",5x,"Efficiency: ",f10.2,1x,"%",37x,"|")')efficiency
     write(stdout,*)"+"//repeat("-",66)//"+"
  end if
  call comms_finalise()

end program derek
