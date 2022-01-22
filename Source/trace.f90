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
  use iso_fortran_env,only: real64
  implicit none
  
  public

  integer,parameter,public                 :: dp=real64  

  real(dp),dimension(:),allocatable         :: entry_time_array
  real(dp),dimension(:),allocatable         :: exit_time_array
  character(50),dimension(:),allocatable    :: entry_array
  character(50),dimension(:),allocatable    :: exit_array
  real(dp),dimension(:),allocatable         :: temp_real_array
  character(50),dimension(:),allocatable    :: temp_char_array
  character(50),dimension(:),allocatable    :: unique_array
  integer,dimension(:),allocatable          :: parent_array
  integer,dimension(:),allocatable          :: temp_int_array
  real(dp)                                  :: comms_start_time=0.0_dp
  real(dp)                                  :: io_start_time=0.0_dp
  real(dp)                                  :: comms_end_time=0.0_dp
  real(dp)                                  :: io_end_time=0.0_dp
  real(dp)                                  :: comms_time=0.0_dp
  real(dp)                                  :: io_time=0.0_dp
  integer                                   :: no_subs
  integer                                   :: parent_counter=0

  ! Global timekeeping parameters
  real(dp),public                           :: global_start,global_end,global_time

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
    integer   :: iostat
    call CPU_TIME(global_start)

    !print*,"init start"
    allocate(entry_array(1),stat=iostat)
    if (iostat.ne.0) stop
    allocate(entry_time_array(1),stat=iostat)
    if (iostat.ne.0) stop
    allocate(exit_array(1),stat=iostat)
    if (iostat.ne.0) stop
    allocate(exit_time_array(1),stat=iostat)
    if (iostat.ne.0) stop
    allocate(parent_array(1),stat=iostat)
    if (iostat.ne.0) stop
    !print*,"init end"
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
    character(*), intent(in)  :: sub_name
    character(30)                    :: new_sub_name
    real(dp)                     :: time

    new_sub_name=trace_string_to_lower(sub_name)

    !print*,"entry start"
    call CPU_TIME(time)
    !print*,"ENTRY:",sub_name,time
    !print*,"IN: ",trim(sub_name)
    ! check for comms
    if (index(new_sub_name,"comms").gt.0)then
       comms_start_time=comms_start_time+time
    end if
    if (index(new_sub_name,"io_").gt.0) io_start_time=io_start_time+time
    !set the things to the last array element



    entry_array(size(entry_array))=trim(new_sub_name)
    parent_array(size(parent_array))=parent_counter
    entry_time_array(size(entry_time_array))=time

    !increase the size of the array

    allocate(temp_real_array(1:size(entry_time_array)+1))
    temp_real_array(1:size(entry_time_array))=entry_time_array
    call  move_alloc(temp_real_array,entry_time_array)

    allocate(temp_char_array(1:size(entry_array)+1))
    temp_char_array(1:size(entry_array))=entry_array
    call  move_alloc(temp_char_array,entry_array)

    allocate(temp_int_array(1:size(parent_array)+1))
    temp_int_array(1:size(parent_array))=parent_array
    call  move_alloc(temp_int_array,parent_array)


    if (allocated(temp_real_array)) deallocate(temp_real_array)
    if (allocated(temp_char_array)) deallocate(temp_char_array)

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
    call CPU_TIME(time)

    ! check for comms
    if (index(sub_name,"comms").gt.0)then
       comms_end_time=comms_end_time+time

    end if
    if (index(sub_name,"io_").gt.0) io_end_time=io_end_time+time
    !set the things to the last array elemen


    exit_array(size(exit_array))=trim(new_sub_name)
    exit_time_array(size(exit_time_array))=time

    !increase the size of the array

    allocate(temp_real_array(1:size(exit_time_array)+1))
    temp_real_array(1:size(exit_time_array))=exit_time_array
    call  move_alloc(temp_real_array,exit_time_array)

    allocate(temp_char_array(1:size(exit_array)+1))
    temp_char_array(1:size(exit_array))=exit_array
    call  move_alloc(temp_char_array,exit_array)


    if (allocated(temp_real_array)) deallocate(temp_real_array)
    if (allocated(temp_char_array)) deallocate(temp_char_array)


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
    implicit none
    integer                          :: i,k,stat
    logical,intent(in)               :: debug
    real,dimension(:),allocatable    :: start_time_sum
    real,dimension(:),allocatable    :: end_time_sum
    real,dimension(:),allocatable    :: sub_times
    integer,dimension(:),allocatable :: call_count
    character(len=50),dimension(:),allocatable :: unique_subs_trimmed
    integer,intent(inout)            :: rank

    logical, optional                :: check_stack
    character(*),intent(in) :: seed
    parent_array(size(parent_array))=0
    !!print*,parent_array

!!$    ! DEFINE THE COMM TIME- ACCESSIBLE GLOBALLY
!!$    comms_time=abs(comms_end_time-comms_start_time)
!!$    io_time=abs(io_end_time-io_start_time)
!!$    print*,comms_end_time,comms_start_time


    !if (allocated(temp_real_array)) deallocate(temp_real_array)
    !if (allocated(temp_char_array)) deallocate(temp_char_array)


    ! HANDLE ALL OF THE UNIQUE CHECKING AND SUMMING UP THE SHIT


    ! set up the array to fill with the unique subroutines, null packs the end 
    allocate(Unique_array(1:size(entry_array)))


    ! HERE IS THE CALL THAT DOES ALL OF THE FUN STUFF, SORTS SUBROUTINES AND RETURNS AN ARRAY OF UNIQUE ARRAYS
    call trace_unique(entry_array,unique_array,no_subs)


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
       if (size(entry_array).ne.size(exit_array)) then
          write(*,*) "Error: trace array mismatch"
          stop
       end if
    end if

    do i=1,size(unique_subs_trimmed)
       do k=1,size(entry_array)

          if (entry_array(k).eq.unique_subs_trimmed(i))then             
             start_time_sum(i)=start_time_sum(i)+entry_time_array(k)
             !if (entry_array(k).eq.'comms_reduce_real')print*,'ENTRY',entry_time_array(k)
             call_count(i)=call_count(i)+1
          end if
          if (.not.present(check_stack))then 
             if (exit_array(k).eq.unique_subs_trimmed(i))then
                end_time_sum(i)=end_time_sum(i)+exit_time_array(k)
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


    if(debug)call trace_IO(rank,unique_subs_trimmed,sub_times,call_count,seed)

    !deallocate(unique_subs_trimmed)


    !deallocate(call_count)
    !deallocate(sub_times)
    !deallocate(start_time_sum,end_time_sum)    
    !print*,"finalise end"
    call CPU_TIME(global_end)
    global_time=global_end-global_start
    return 
  end subroutine trace_finalise



  subroutine trace_unique(unsorted_array,out_array,k)
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
    character(len=50),intent(in),dimension(:)    :: unsorted_array         ! The input
    character(len=50),dimension(:),allocatable   :: res  ! The output
    character(len=50),dimension(:),intent(inout) :: out_array
    integer,intent(out)                          :: k                   ! The number of unique elements
    integer :: i, j, stat

    !print*,"unique start"

    allocate(res(size(unsorted_array)-1))


    k = 1
    res(1) = unsorted_array(1)
    outer: do i=2,size(unsorted_array)-1
       do j=1,k
          if (res(j) == unsorted_array(i)) then
             ! Found a match so start looking again
             cycle outer
          end if
       end do
       ! No match found so add it to the output
       k = k + 1
       res(k) = unsorted_array(i)
    end do outer
    out_array(1:size(res))=res
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
    real,dimension(:),intent(inout)              :: array_to_sort
    character(len=50),dimension(:),intent(inout) :: subs_array
    character(len=50),dimension(:),allocatable   :: temp_array_char
    real,dimension(:),allocatable                :: temp_array
    integer,dimension(:),intent(inout)           :: count_array
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
          temp_count=count_array
          if (array_to_sort(i).lt.array_to_sort(i+1))then
             temp_array(i)=temp_array(i+1)
             temp_array(i+1)=array_to_sort(i)
             temp_array_char(i)=temp_array_char(i+1)
             temp_array_char(i+1)=subs_array(i)
             temp_count(i)=temp_count(i+1)
             temp_count(i+1)=count_array(i)

             array_to_sort=temp_array
             subs_array=temp_array_char
             count_array=temp_count

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


  subroutine trace_IO(rank,subs_list,time_list,call_list,seed)
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
    real,dimension(:),intent(in)          :: time_list
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
    do i=1,size(subs_list)
       write(file_id,19) adjustl(trim(subs_list(i))),repeat('.',70-len(trim(subs_list(i)))),call_list(i),time_list(i)

    end do

    write(file_id,*) "+==============================================================================================+"

    write(file_id,'(1x,A,3x,A,3x,i3,59x,a)') "|","No. Subroutines Profiled :",size(subs_list),"|"
    write(file_id,'(1x,A,3x,A,2x,f10.5,1x,a,51x,a)') "|","Time Spent in COMMS      :",comms_time,"s","|"
    write(file_id,'(1x,A,3x,A,2x,f10.5,1x,a,51x,a)') "|","Time Spent in IO         :",io_time,"s","|"    

    write(file_id,*) "+==============================================================================================+"
    call trace_parents(file_id)
    close(file_id)
    !print*,"io end"
  end subroutine trace_IO


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
    character(len=30),allocatable,dimension(:) :: temp_stack
    character(*),intent(in):: seed
    integer                :: i,iostat,current_parents,rank
    !print*,"stack start"
    !Do some setting up, 
    current_sub=entry_array(size(entry_array)-1)

    call trace_exit(current_sub)

!!$    do i=1,size(entry_array)
!!$       print*,trim(entry_array(i))
!!$    end do
!!$    do i=1,size(exit_array)
!!$       print*,trim(exit_array(i))
!!$    end do


    call trace_finalise(.FALSE.,rank,.true.,seed=seed)
    current_parents=parent_array(size(parent_array)-1)
    allocate(stack(1),stat=iostat)
    if (iostat.ne.0) stop
    !allocate(temp_stack(1),stat=iostat)
    !if (iostat.ne.0) stop

    stack(1)=trim(current_sub)



    do i=1,size(parent_array)-1
       if (parent_array(size(parent_array)-i).lt.current_parents)then
          current_parents=parent_array(size(parent_array)-i)
          allocate(temp_stack(1:size(stack)+1))
          temp_stack(1:size(stack))=stack
          call  move_alloc(temp_stack,stack)

          stack(size(stack))=entry_array(size(parent_array)-i)

       end if
    end do
    write(err_file,*) "Stack trace:"
    do i=1,size(stack)
       write(err_file,'(4x,A)')stack(i)

    end do

    !if (allocated(temp_stack))deallocate(temp_stack)
    !print*,"stack end"
  end subroutine trace_stack


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


    write(file_id,'(" |",T8,A,T87,A,T97,"|")')"Call Log:","Time:"
    write(file_id,*) "+==============================================================================================+"




    do i=1,size(parent_array)-1

       temp_int=2*parent_array(i)
       width=82-4-temp_int-3-1-len(entry_array(i))-10
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
          if (parent_array(i).gt.parent_array(i-1)) then
             write(file_id,trim(fmt_str2)) trim(back)!trim('\ ')
          end if
       end if
       write(file_id,trim(fmt_str)) &
            & trim(point),trim(entry_array(i)),repeat(".",(78-len(trim(entry_array(i)))-len(trim(point)))),entry_time_array(i)
       if (parent_array(i).eq.parent_array(i+1)+1.and.i.lt.size(parent_array)-1)&
            then
          write(file_id,trim(fmt_str2)) trim(forward)!"/"
       end if

    end do



    
    write(file_id,*) "+==============================================================================================+"
    !print*, "parents end"
  end subroutine trace_parents



end module trace
