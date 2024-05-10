!*******************************************************************************
! Copyright 2024 Z. Hawkhead
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!*******************************************************************************
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
       & io_from_atomic,io_to_atomic,io_conversion,io_mem_report
  use memory,only: memory_deallocate
  use basis, only: basis_init,current_basis
  use wave,  only: wave_allocate,wavefunction_slice,wavefunction,operator (+),operator(-),operator(*)
  use state, only: state_data, current_state, state_init,state_finalise,state_restart
  use comms, only: rank,on_root_node
  use pot
  use fft, only : fft_1d
  use utils
  use electronic, only : electronic_minimise
  use grids
  use xc
  implicit none

  real(dp)                               :: efficiency
  real(dp)                               :: start_time,end_time,current_time,time
  integer :: ig,ik


  ! TESTING
  ! Matrix and variables declaration
  real(kind=dp), allocatable,dimension(:,:)   :: matrix, inverted_matrix,result
  integer :: i, j,n=3
  complex(kind=dp) , allocatable,dimension(:) :: test_fft
  real(kind=dp) , allocatable,dimension(:)    :: test_fft_x
  complex(kind=dp) , allocatable,dimension(:) :: test_fft_fine
  real(kind=dp) , allocatable,dimension(:)    :: test_fft_x_fine

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




  !current_state%ext_pot=1.0_dp*current_state%ext_pot
  call io_write_params(current_basis%num_grid_points&
       &, current_basis%num_fine_grid_points&
       &, current_basis%ngx&
       &, current_basis%ngy&
       &, current_basis%ngz&
       &, current_basis%fine_ngx&
       &, current_basis%fine_ngy&
       &, current_basis%fine_ngz)      ! Write out all the parameters, need to pass it the basis info 


  ! Now check if its a dry run before going on
  if (current_params%check)then
     call io_check()
  end if

  ! allocate all of the things held in the state
  call state_init()

  ! write out the memory report
  call io_mem_report()

  ! report the memory usage, probably won't need much more memory stuff after this 
  !call memory_report(stdout,current_params%iprint,current_params%calc_memory)



  ! We have done all of the initialisation, this is the only print statements we will be using in the main derek file!
  time=comms_wall_time()
  if (on_root_node.and..not.current_params%check)then
     write(stdout,*)
     write(stdout,*)
     write(stdout,*)"+==================================================================+"
     write(stdout,*)"|         I N I T I A L I S A T I O N   C O M P L E T E            |"
     write(stdout,*)"|         ---------------------------------------------            |"
     write(stdout,*)'|     DEReK has successfully initialised the parameters and        |'
     write(stdout,*)"|      will now begin the main phase of the calculation...         |"
     write(stdout,*)"+==================================================================+"
     write(stdout,'(1x,"|",5x,"Initialisation time: ",f10.2,1x,"s",T69,"|")')time
     write(stdout,*)"+"//repeat("=",66)//"+"

     write(stdout,*)
     write(stdout,*)
     write(stdout,*)
  end if



  ! After the initialisation is finished, we move onto the electronic minimisation..
  ! Calculation starts here
  call electronic_minimise()




  ! doing some big testing here
  call grids_real2recip(current_state%den,'FINE')
  call grids_recip2real(current_state%den,'FINE')




  call memory_allocate(test_fft,1,current_basis%ngx,'B')
  call memory_allocate(test_fft_x,1,current_basis%ngx,'B')
  call memory_allocate(test_fft_fine,1,current_basis%fine_ngx,'B')
  call memory_allocate(test_fft_x_fine,1,current_basis%fine_ngx,'B')


  ! define a function
  test_fft_x =  [(twopi*1.0*(i-1)/(current_basis%ngx-1), i=1,current_basis%ngx)]
  test_fft_x_fine =  [(twopi*1.0*(i-1)/(current_basis%fine_ngx-1), i=1,current_basis%fine_ngx)]

  test_fft = exp(-0.5*((test_fft_x - pi*0.4)/0.5)**2) / (0.5 * sqrt(2.0 * pi)) + &
       & exp(-0.5*((test_fft_x - pi*1.1)/0.35)**2) / (0.35 * sqrt(2.0 * pi)) - &
       & exp(-0.5*((test_fft_x - pi*1.4)/0.2)**2) / (0.2 * sqrt(2.0 * pi))


  !test_fft = [(i*0.0_dp,i=1,current_basis%ngx)]
  !test_fft(43:63) = 1.0_dp
  open(unit=120,file='real.txt',access='stream',form='FORMATTED')
  do i = 1, current_basis%ngx
     call utils_random_number(testvar)
     test_fft(i) = testvar
     write(120,*) test_fft_x(i),real(test_fft(i))
  end do

  i = 1
  open(unit=121,file='fft.txt',access='stream',form='FORMATTED')
  call fft_1d(test_fft,'STD',fft_forward)
  do i = 1, current_basis%ngx
     write(121,*) test_fft_x(i),real(test_fft(i))
  end do



  ! Zero the fine array
  test_fft_fine(:) = cmplx_0

  !print*,current_basis%ngx/2
  test_fft_fine(1:current_basis%ngx/2) = test_fft(1:current_basis%ngx/2)
  !print*,current_basis%fine_ngx-current_basis%ngx/2,current_basis%fine_ngx
  test_fft_fine(current_basis%fine_ngx-current_basis%ngx/2:current_basis%fine_ngx) = &
       & test_fft(current_basis%ngx/2:current_basis%ngx)

  i=1
  open(unit=122,file='fft_padded.txt',access='stream',form='FORMATTED')
  do i = 1, current_basis%fine_ngx
     write(122,*) test_fft_x_fine(i),real(test_fft_fine(i))
  end do

  call fft_1d(test_fft_fine,'FINE',fft_backward)
  i=1
  open(unit=123,file='real_padded.txt',access='stream',form='FORMATTED')
  do i = 1, current_basis%fine_ngx
     write(123,*) test_fft_x_fine(i),real(test_fft_fine(i)) 
  end do





  !  ____                   
  ! |  _ \  ___  _ __   ___ 
  ! | | | |/ _ \| '_ \ / _ \
  ! | |_| | (_) | | | |  __/
  ! |____/ \___/|_| |_|\___|





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
