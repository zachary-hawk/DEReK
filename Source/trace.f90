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
!=============================================================================!
!                                 T R A C E                                   !
!=============================================================================!
!                 Module for profiling the DEReK  code                        !
!-----------------------------------------------------------------------------!
!                           author: Z. Hawkhead                               !
!=============================================================================!
module trace
  use constants
  implicit none

  public


  type log_item
     character(40) :: name=''
     real(real64)  :: time=0.0
     integer       :: id = -1
     integer       :: parents =0 
     logical       :: closed = .false.
     character(15) :: cat=''
  end type log_item




  !real(dp),dimension(:),allocatable         :: entry_time_array
  !real(dp),dimension(:),allocatable         :: exit_time_array
  !character(40),dimension(:),allocatable    :: entry_array
  !character(40),dimension(:),allocatable    :: exit_array
  !real(dp),dimension(:),allocatable         :: temp_real_array
  !character(50),dimension(:),allocatable    :: temp_char_array

  character(50),dimension(:),allocatable    :: unique_array
  !integer,dimension(:),allocatable          :: parent_array
  !integer,dimension(:),allocatable          :: temp_int_array
  real(dp)                                  :: comms_start_time=0.0_dp
  real(dp)                                  :: io_start_time=0.0_dp
  real(dp)                                  :: comms_end_time=0.0_dp
  real(dp)                                  :: io_end_time=0.0_dp
  real(dp)                                  :: comms_time=0.0_dp
  real(dp)                                  :: io_time=0.0_dp
  integer                                   :: no_subs
  integer                                   :: parent_counter=0





  ! Global logging
  integer,parameter                         :: log_size = 10000
  type(log_item),dimension(:),allocatable   :: in_log
  type(log_item),dimension(:),allocatable   :: out_log
  integer                                   :: global_id = 0
  integer                                   :: close_count = 0

  real(dp)                                  :: global_end=0.0
  real(dp)                                  :: global_start=0.0
  real(dp)                                  :: global_time=0.0
  character(15),dimension(1:log_size)       :: modules
  real(dp)     ,dimension(1:log_size)       :: mod_times
  integer                                   :: global_mods

  integer                                   :: warning_counter=0
contains

  subroutine trace_init()
    !==============================================================================!
    !                             T R A C E _ I N I T                              !
    !==============================================================================!
    ! Initialisation for the profiling tools in the trace module. Allocates the    !
    ! arrays for the trace log such that they are ready for appending.             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    integer   :: stat
    !call CPU_TIME(global_start)
    global_start = TRACE_WALLCLOCK()
    ! Only one allocation for each of the entry and exit logs
    allocate(in_log(1:log_size),stat=stat)
    if (stat.ne.0) stop
    allocate(out_log(1:log_size),stat=stat)
    if (stat.ne.0) stop



  end subroutine trace_init


  subroutine trace_entry(sub_name)
    !==============================================================================!
    !                            T R A C E _ E N T R Y                             !
    !==============================================================================!
    ! Subroutine to be called at the start of every other subroutine. Adds the     !
    ! name of the subroutine to the log and initialises a timer for the            !
    ! subroutine of interest.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           sub_name,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    character(*), intent(in) :: sub_name
    character(30)            :: new_sub_name
    real(dp)                 :: time
    integer                  :: i

    new_sub_name=trace_string_to_lower(sub_name)

    ! Have a new entry, so increase global ID
    global_id = global_id + 1



    time  = TRACE_WALLCLOCK()

    in_log(global_id)%name=trim(new_sub_name)
    in_log(global_id)%time=time
    in_log(global_id)%id = global_id
    in_log(global_id)%parents = parent_counter 

    if (index(new_sub_name,"_").gt.0)then 
       in_log(global_id)%cat = trim(new_sub_name(1:index(trim(new_sub_name),"_")-1))
    else
       in_log(global_id)%cat=trim(new_sub_name)
    end if

    !increase the parent counter
    parent_counter=parent_counter+1
    !print*,"entry end"

  end subroutine trace_entry


  subroutine trace_exit(sub_name)
    !==============================================================================!
    !                             T R A C E _ E X I T                              !
    !==============================================================================!
    ! Subroutine to be called at the exit of every other subroutine. Adds the      !
    ! name of the subroutine to the log and initialises a timer for the exit       !
    ! time.                                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           sub_name,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    character(*), intent(in)      :: sub_name
    character(30)                    :: new_sub_name
    real(dp)                          :: time
    !print*,"exit start"
    new_sub_name=trace_string_to_lower(sub_name)
    !call CPU_TIME(time)
    time=TRACE_WALLCLOCK()
    close_count=close_count+1

    ! check for comms
    if (index(sub_name,"comms").gt.0)then
       comms_end_time=comms_end_time+time
    end if
    if (index(sub_name,"io_").gt.0) io_end_time=io_end_time+time
    !set the things to the last array elemen

    out_log(close_count)%name=trim(new_sub_name)
    out_log(close_count)%time=time
    out_log(close_count)%parents = parent_counter 

    if (index(new_sub_name,"_").gt.0)then 
       out_log(close_count)%cat = trim(new_sub_name(1:index(trim(new_sub_name),"_")-1))
    else
       out_log(close_count)%cat=trim(new_sub_name)
    end if

    !decrease the parent counter
    parent_counter=parent_counter-1

    !print*,"exit end"
  end subroutine trace_exit

  subroutine trace_finalise(debug,rank,check_stack,seed)
    !==============================================================================!
    !                         T R A C E _ F I N A L I S E                          !
    !==============================================================================!
    ! Subroutine that finalises the trace module, no further profiling can         !
    ! happen once this module is called.                                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !           debug,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!

    integer                          :: i,k,stat
    logical,intent(in)               :: debug
    real(dp),dimension(:),allocatable    :: start_time_sum
    real(dp),dimension(:),allocatable    :: end_time_sum
    real(dp),dimension(:),allocatable    :: sub_times
    integer,dimension(:),allocatable :: call_count
    character(len=50),dimension(:),allocatable :: unique_subs_trimmed
    integer,intent(inout)            :: rank

    logical, optional                :: check_stack
    character(*),intent(in) :: seed


    ! First thing is to close the logs
    call trace_close_logs()


    ! HANDLE ALL OF THE UNIQUE CHECKING AND SUMMING UP THE SHIT


    ! set up the array to fill with the unique subroutines, null packs the end 
    allocate(Unique_array(1:log_size))


    ! HERE IS THE CALL THAT DOES ALL OF THE FUN STUFF, SORTS SUBROUTINES AND RETURNS AN ARRAY OF UNIQUE ARRAYS
    call trace_unique(in_log,unique_array,no_subs)


    ! ALLOCATE THE TIMING ARRAYS

    allocate(start_time_sum(1:no_subs),end_time_sum(1:no_subs))
    allocate(sub_times(1:no_subs))
    allocate(call_count(1:no_subs))
    sub_times=0.0_dp
    call_count=0
    start_time_sum=0.0_dp
    end_time_sum=0.0_dp

!!$    do i=1,size(entry_time_array)
!!$       print*,entry_time_array(i),exit_time_array(i)
!!$    end do
    ! ALLOCATE THE UNIQUE SUBS ARRAY SO THERE ARE NO EMPTY CRAP AT THE END

    allocate(unique_subs_trimmed(1:no_subs))
    unique_subs_trimmed=unique_array(1:no_subs)
!!$    do i =1,size(entry_array)
!!$       print*,i,trim(entry_array(i))
!!$    end do
!!$    print*,""
!!$    do i =1,size(exit_array)
!!$       print*,i,trim(exit_array(i))
!!$    end do

    if (.not.present(check_stack))then
       if (close_count.ne.global_id) then
          write(*,*) "Error: trace array mismatch"
          stop
       end if
    end if

    do i=1,size(unique_subs_trimmed)
       do k=1,global_id

          if (in_log(k)%name.eq.unique_subs_trimmed(i))then             
             start_time_sum(i)=start_time_sum(i)+in_log(k)%time
             !if (entry_array(k).eq.'comms_reduce_real')print*,'ENTRY',entry_time_array(k)
             call_count(i)=call_count(i)+1
          end if
          if (.not.present(check_stack))then 
             if (out_log(k)%name.eq.unique_subs_trimmed(i))then
                end_time_sum(i)=end_time_sum(i)+out_log(k)%time
                !if (exit_array(k).eq.'comms_reduce_real')print*,"END",exit_time_array(k)
             end if
          end if
       end do
    end do




    sub_times=abs(end_time_sum-start_time_sum)
!!$    do i=1,size(unique_subs_trimmed)
!!$       print*,unique_subs_trimmed(i),start_time_sum(i),end_time_sum(i),sub_times(i)
!!$    end do

    ! LOOP THROUGH TO GET SPECIFIC TIMES
    comms_time=0.0_dp
    io_time=0.0_dp
    do i=1,size(sub_times)
       if (index(unique_subs_trimmed(i),'comms_').ne.0)then
          comms_time=comms_time+sub_times(i)
       else if (index(unique_subs_trimmed(i),'io_').ne.0)then          
          io_time=io_time+sub_times(i)
       end if
    end do

    call trace_sort(sub_times,unique_subs_trimmed,call_count)
    !print*,"test"


    ! do the modules now
    call trace_modules(modules,mod_times)


    if(debug)call trace_io(rank,unique_subs_trimmed,sub_times,call_count,seed)

    !deallocate(unique_subs_trimmed)


    !deallocate(call_count)
    !deallocate(sub_times)
    !deallocate(start_time_sum,end_time_sum)    
    !print*,"finalise end"

    !call CPU_TIME(global_end)
    global_end = TRACE_WALLCLOCK()
    global_time=global_end!-global_start
    return 
  end subroutine trace_finalise



  subroutine trace_unique(unsorted_log,out_array,k)
    !==============================================================================!
    !                           T R A C E _ U N I Q U E                            !
    !==============================================================================!
    ! Subroutine to extract unique entries in the trace log and reduce them to a   !
    ! new array.                                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unsorted_array,    intent :: in                                    !
    !           out_array,         intent :: inout                                 !
    !           k,                 intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    type(log_item),dimension(:),intent(in) :: unsorted_log
    !character(len=50),intent(in),dimension(:)    :: unsorted_array         ! The input
    character(len=50),dimension(:),allocatable   :: res  ! The output
    character(len=50),dimension(:),intent(inout) :: out_array
    integer,intent(out)                          :: k                   ! The number of unique elements
    integer :: i, j, stat

    !print*,"unique start"

    allocate(res(log_size))



    k=1
    all_logs: do i=1,global_id
       unique: do j=1,k
          if (unsorted_log(i)%name .eq. res(j) ) then 
             cycle all_logs
          end if

       end do unique
       ! if we have completed the loop it is a new one so add it
       res(k)=unsorted_log(i)%name
       k=k+1

    end do all_logs



!!$    k = 1
!!$    res(1) = unsorted_log(1)%name
!!$    outer: do i=2,size(unsorted_log%name)-1
!!$       do j=1,k
!!$          print*,unsorted_log(i)%name,res(j)
!!$          if (res(j) == unsorted_log(i)%name) then
!!$             ! Found a match so start looking again
!!$             cycle outer
!!$          end if
!!$       end do
!!$       ! No match found so add it to the output
!!$       k = k + 1
!!$       res(k) = unsorted_log(i)%name
!!$
!!$    end do outer
    out_array(1:k)=res(1:k)
    !print*,"unique end"
  end subroutine trace_unique


  subroutine trace_sort(array_to_sort,subs_array,count_array)
    !==============================================================================!
    !                             T R A C E _ S O R T                              !
    !==============================================================================!
    ! Subroutine that implements the Bubble Sort algorithm for sorting the total   !
    ! time spent in each subroutine into descending order. Also acts on the list   !
    ! of subroutine names and the call log so as to keep the orders consistent.    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array_to_sort,     intent :: inout                                 !
    !           subs_array,        intent :: inout                                 !
    !           count_array,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    real(dp),dimension(:),intent(inout)              :: array_to_sort
    character(len=*),dimension(:),intent(inout) :: subs_array
    character(len=50),dimension(:),allocatable   :: temp_array_char
    real(dp),dimension(:),allocatable                :: temp_array
    integer,dimension(:),intent(inout),optional  :: count_array
    integer,dimension(:),allocatable             :: temp_count
    integer                         :: i,j,k
    !print*, "sort start"
    allocate(temp_array(1:size(array_to_sort)))
    allocate(temp_array_char(1:size(array_to_sort)))
    allocate(temp_count(1:size(count_array)))


    do j=1,size(array_to_sort)-1
       do i=1,size(array_to_sort)-j
          temp_array=array_to_sort
          temp_array_char=subs_array
          if (present(count_array))temp_count=count_array
          if (array_to_sort(i).lt.array_to_sort(i+1))then
             temp_array(i)=temp_array(i+1)
             temp_array(i+1)=array_to_sort(i)
             temp_array_char(i)=temp_array_char(i+1)
             temp_array_char(i+1)=subs_array(i)
             if (present(count_array))then 
                temp_count(i)=temp_count(i+1)
                temp_count(i+1)=count_array(i)
             end if
             array_to_sort=temp_array
             subs_array=temp_array_char
             if (present(count_array))count_array=temp_count

          else
             cycle
          end if
       end do
    end do
    !print*,"sort end"
  end subroutine trace_sort


  function trace_string_to_lower( string ) result (new)
    !==============================================================================!
    !                    T R A C E _  S T R I N G _ T O L O W E R                  !
    !------------------------------------------------------------------------------! 
    ! Functional subroutine used to lower the case of inputted strings to          !
    ! prevent ambiguities in reading parameters.                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           strin                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=*)           :: string 

    character(len=len(string)) :: new 

    integer                    :: i 
    integer                    :: k 
    integer::length
    !print*,"string start"
    !    call trace_entry("IO_STRING_TO_LOWER")
    length = len(string)
    new    = string
    do i = 1,len(string)
       k = iachar(string(i:i))
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then
          k = k + iachar('a') - iachar('A')
          new(i:i) = achar(k)
       endif
    enddo
    !    call trace_exit("IO_STRING_TO_LOWER")
    !print*,"string end"
  end function trace_string_to_lower


  subroutine trace_io(rank,subs_list,time_list,call_list,seed)
    !==============================================================================!
    !                               T R A C E _ I O                                !
    !==============================================================================!
    ! Subroutine for opening and writing the profiling files, one per MPI          !
    ! instance.                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !           subs_list,         intent :: in                                    !
    !           time_list,         intent :: in                                    !
    !           call_list,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    integer,intent(inout)                 :: rank
    character(50),dimension(:),intent(in) :: subs_list
    real(dp),dimension(:),intent(in)          :: time_list
    integer,dimension(:),intent(in)       :: call_list
    character(*),intent(in)               :: seed
    integer                               :: file_id
    character(4)                          :: rank_char
    integer                               :: i
    !print*,"io start"
    file_id=rank*294+89

    write(rank_char,'(i4)')rank

    do i=1,3
       if (rank_char(i:i).eq." ") rank_char(i:i)="0"
    end do

    open(unit=file_id,file=trim(seed)//"."//rank_char//".profile",RECL=8192,form="FORMATTED",status="UNKNOWN")
    write(file_id,*) "+==============================================================================================+"
    write(file_id,*)    "|                                                                                              |"
    write(file_id,9)  rank_char

9   FORMAT(1x,"|",36x,"P R O F I L E :",2x,A,37x,"|")

    write(file_id,*)    "|                                                                                              |"
    write(file_id,*)    "+==============================================================================================+"
    write(file_id,*)    "|       Subroutine:                                                Call Count:       Time:     |"
    write(file_id,*)    "+==============================================================================================+"


19  format(1x,"|",3x,A,A,T75,i5,T83,f10.5,1x,"s",T97,"|")
    do i=1,size(subs_list)-1
       write(file_id,19) adjustl(trim(subs_list(i))),repeat('.',70-len(trim(subs_list(i)))),call_list(i),time_list(i)
    end do

    write(file_id,*)    "|                                                                        --------------------- |"
    write(file_id,'(1x,"|",T75,i5,T83,f10.5,1x,"s",T97,"|")') sum(call_list),in_log(global_id)%time-global_start
    write(file_id,*) "+==============================================================================================+"

!!$    write(file_id,'(1x,A,3x,A,3x,i3,59x,a)') "|","No. Subroutines Profiled :",size(subs_list),"|"
!!$    write(file_id,'(1x,A,3x,A,2x,f10.5,1x,a,51x,a)') "|","Time Spent in COMMS      :",comms_time,"s","|"
!!$    write(file_id,'(1x,A,3x,A,2x,f10.5,1x,a,51x,a)') "|","Time Spent in IO         :",io_time,"s","|"    

    write(file_id,'(" |",T8,A,T87,A,T97,"|")')"Modules Profiled:","Time:"
    write(file_id,*) "+==============================================================================================+"

20  format(1x,"|",3x,A,A,T83,f10.5,1x,"s",T97,"|")
    do i = 1 , global_mods
       write(file_id,20) adjustl(trim(modules(i))),repeat('.',77-len(trim(modules(i)))),mod_times(i)
    end do


    call trace_parents(file_id)
    close(file_id)
    !print*,"io end"
  end subroutine trace_io


  subroutine trace_parents(file_id)
    !==============================================================================!
    !                          T R A C E _ P A R E N T S                           !
    !==============================================================================!
    ! Subroutine used for sorting the subroutine calls by parents and children.    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           file_id,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  21/01/2020                                            !
    !==============================================================================!
    implicit none
    integer :: file_id,temp_int,i,width,width2,k
    character(len=100) :: fmt_str,fmt_str2,fmt_for,fmt_bk,fmt_po
    character(len=200)  :: forward, back, point
    !print*,"parents start"

    !39  format(1x,"|",4x,<temp_int>x,A,1x,A,<width>x"|")
    !38  format(1x,"|",3x,<temp_int>x,A,<width2>x"|")

    write(file_id,*) "+==============================================================================================+"
    write(file_id,'(" |",T8,A,T87,A,T97,"|")')"Call Log:","Time:"
    write(file_id,*) "+==============================================================================================+"




    do i=1,global_id-1

       temp_int=2*in_log(i)%parents
       width=82-4-temp_int-3-1-len(in_log(i)%name)-10
       width2=82-4-temp_int 

       ! Set up the characters that start the line
       if (temp_int.eq.0)then
          !skip
          write(point,*)"o->"
          write(back,*)" |\ "
          write(forward,*)" |/"
       else
          write(back,*) (" |",k=1,temp_int/2),"\ "
          write(point,*) ("| ", k=1,temp_int/2),"o->"
          write(forward,*) (" |",k=1,temp_int/2),"/ "

       end if


       if (temp_int.eq.0)then
          write(fmt_str,*) '(1x,"|",3x,A,1x,A,A,T83,f10.5,x,"s",T97,"|")'
          write(fmt_str2,*) '(1x,"|",2x,A,T97,"|")'
       else

          write(fmt_str,*) '(1x,"|",3x,A,1x,A,A,T83,f10.5,x,"s",T97,"|")'
          write(fmt_str2,*) '(1x,"|",2x,A,T97,"|")'
       end if

       !!print*,fmt_str
       !!print*,fmt_str2

       if (i.gt.1)then
          if (in_log(i)%parents.gt.in_log(i-1)%parents) then
             write(file_id,trim(fmt_str2)) trim(back)!trim('\ ')
          end if
       end if
       write(file_id,trim(fmt_str)) &
            & trim(point),trim(in_log(i)%name),repeat(".",(78-len(trim(in_log(i)%name))-len(trim(point)))),in_log(i)%time
       if (in_log(i)%parents.eq.in_log(i+1)%parents+1.and.i.lt.global_id-1)&
            then
          write(file_id,trim(fmt_str2)) trim(forward)!"/"
       end if

    end do




    write(file_id,*) "+==============================================================================================+"
    !print*, "parents end"
  end subroutine trace_parents


  subroutine trace_close_logs()
    !==============================================================================!
    !                       T R A C E _ C L O S E _ L O G S                        !
    !==============================================================================!
    ! Subroutine for adding a flag to the trace logs indicating if it is closed.   !
    ! It should not matter in most cases, but is useful for error handling and     !
    ! finding the stack                                                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!

    integer :: i,j

    ! Loop over entries
    entry:do i=1,global_id
       ! loop over all closed
       exit:do j = 0, close_count-1

          if (trim(in_log(i)%name) .eq. trim(out_log(close_count-j)%name) &
               & .and. .not. out_log(close_count-j)%closed)then
             ! we have a match and its not one that we have previously closed

             ! set the exit id
             out_log(close_count-j)%id = in_log(i)%id
             !set the exit closed status
             out_log(close_count-j)%closed = .true.
             ! set the entry log closed status
             in_log(i)%closed = .true.


             cycle entry

          end if
       end do exit
    end do entry



  end subroutine trace_close_logs

  subroutine trace_stack(err_file,rank,seed)
    !==============================================================================!
    !                            T R A C E _ S T A C K                             !
    !==============================================================================!
    ! Subroutine to handle the stack for the error handling in  DEREK. Gives the   !
    ! current trace of the subroutine calls.                                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           err_file,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  21/01/2020                                            !
    !==============================================================================!
    implicit none
    integer, intent(in)    :: err_file
    character(len=30)      :: current_sub
    character(len=30),allocatable,dimension(:) :: stack
    character(*),intent(in):: seed
    integer                :: i,iostat,current_parents,rank,unclosed=0
    integer                :: j=1
    !print*,"stack start"
    !Do some setting up, 
    !current_sub=entry_array(size(entry_array)-1)

    !call trace_exit(current_sub)


    ! Call the finialisation without finishing
    call trace_finalise(.FALSE.,rank,.true.,seed=seed)

    ! count the unclosed routines
    do i=1,global_id
       if (.not.in_log(i)%closed) unclosed=unclosed+1
    end do

    allocate(stack(1:unclosed))
    ! Go through the entry log backwards and add to the stack
    do i=0,global_id-1
       if (.not.in_log(global_id-i)%closed) then
          stack(j)=in_log(global_id-i)%name
          j=j+1
       end if
    end do

    write(err_file,*) "Stack trace:"
    do i=1,size(stack)
       write(err_file,'(4x,A)')stack(i)
    end do



    return
  end subroutine trace_stack



  function trace_wallclock()

    use constants, only: dp
    implicit none
    real(kind=dp)        :: trace_wallclock   
    real                 :: tmp_cpu_time 
    call cpu_time(tmp_cpu_time)
    trace_wallclock = real(tmp_cpu_time,dp) - global_start
    return

  end function trace_wallclock


  subroutine trace_modules(module_list, module_times)
    !==============================================================================!
    !                          T R A C E _ M O D U L E S                           !
    !==============================================================================!
    ! Subroutine for determining the amount of time spend inside each module.      !
    ! This is useful for analysing problematic modules.                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           module_lis,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!
    character(*),intent(inout),dimension(:) :: module_list
    real(dp)    ,intent(inout),dimension(:) :: module_times

    integer :: i,j,N
    real(dp):: in_time,out_time

    ! Loop through the in logs to find the unique module calls
    N=1
    all_logs: do i=1,global_id
       unique: do j=1,N
          if (in_log(i)%cat .eq. module_list(j) ) then 
             cycle all_logs
          end if

       end do unique
       ! if we have completed the loop it is a new one so add it
       module_list(N)=in_log(i)%cat
       N=N+1

    end do all_logs

    N=N-1
    global_mods=N

    ! Now have all the unique logs

    ! loop through entries for the timings

    module_times(:) = 0.0_dp

    do i = 1,N
       ! do the in_log
       do j=1, global_id
          if (in_log(j)%cat .eq. module_list(i))then 
             !We have a match
             module_times(i) = module_times(i) - in_log(j)%time
          end if
       end do

       ! do the out_log
       do j=1, close_count
          if (out_log(j)%cat .eq. module_list(i))then 
             !We have a match
             module_times(i) = module_times(i) + out_log(j)%time
          end if
       end do
    end do

    ! Sort the modules by time

    call trace_sort(module_times(1:N),module_list(1:N))

  end subroutine trace_modules



  subroutine trace_current(current_sub)
    !==============================================================================!
    !                          T R A C E _ C U R R E N T                           !
    !==============================================================================!
    ! A low level routine for determining which is the current subroutine.         !
    ! Mostly to be used for error handling.                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           current_sub,       intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    ! This routine is only to be used when handling errors, it will be a nightmare otherwise
    character(*), intent(out) :: current_sub
    integer :: i , last 
    ! Close the logs
    call trace_close_logs()

    do i=1, global_id
       if (.not.in_log(i)%closed) last = i
    end do

    current_sub = in_log(last)%name


  end subroutine trace_current
end module trace
