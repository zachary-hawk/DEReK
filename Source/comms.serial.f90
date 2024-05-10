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
!=============================================================================!                                                                                                                         
!                                  COMMS                                      !                                                                                                                         
!=============================================================================!                                                                                                                         
!              Module handeling comminications: MPI                           !                                                                                                                         
!-----------------------------------------------------------------------------!                                                                                                                         
!                        author: Z. Hawkhead                                  !                                                                                                                         
!=============================================================================!                                                                                                                         
module COMMS
  !use mpi                                                                                                                                                                                              
  use constants
  use trace
  implicit none
  !include 'mpif.h'
  integer                              :: ierr
  integer, parameter                   :: max_version_length = 0
  !integer,parameter                    :: max_version_length=MPI_MAX_LIBRARY_VERSION_STRING
  !integer, dimension(MPI_STATUS_SIZE)  :: status1
  !integer,parameter,private :: dp=real64                                                                                                                                                               
  ! Some of the stuff i'll need, gloabal                                                                                                                                                                
  integer,public,save                  :: rank
  integer,public,save                  :: nprocs
  logical,public,save                  :: on_root_node
  integer,public,save                  :: dist_kpt
  integer,public,save                  :: dist_gvec

  character(6)                         :: comms_arch="SERIAL"


  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------! 

  !  public comms_recv
  !  public comms_send
  !  public comms_reduce
  !  public comms_bcast

  interface comms_reduce
     module procedure comms_reduce_real
     module procedure comms_reduce_int
     module procedure comms_reduce_complex
  end interface comms_reduce

  interface comms_send
     module procedure comms_send_int
     module procedure comms_send_double
     module procedure comms_send_int_array
     module procedure comms_send_double_array
     module procedure comms_send_real_array2d
  end interface comms_send

  interface comms_recv
     module procedure comms_recv_int
     module procedure comms_recv_double
     module procedure comms_recv_int_array
     module procedure comms_recv_double_array
     module procedure comms_recv_real_array2d
  end interface comms_recv

  interface comms_bcast
     module procedure COMMS_BCAST_INT
     module procedure COMMS_BCAST_REAL
     module procedure COMMS_BCAST_DOUBLE
     module procedure COMMS_BCAST_INT_ARRAY
     module procedure COMMS_BCAST_REAL_ARRAY
     module procedure COMMS_BCAST_DOUBLE_ARRAY
     module procedure COMMS_BCAST_LOGICAL
  end interface comms_bcast

  public comms_wtime
  public comms_init
  public comms_finalise
  public comms_abort
  public comms_barrier
  public comms_version
  public comms_library_version
  public comms_reduce
  public comms_bcast

contains

  subroutine comms_scheme(nkpts,ngv,stdout,report)
    !==============================================================================!
    !                           C O M M S _ S C H E M E                            !
    !==============================================================================!
    ! Subroutine to decide how the kpoint and gvectors are split amongst the       !
    ! processes                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           nkpts,             intent :: in                                    !
    !           ngv,               intent :: in                                    !
    !           stdout,            intent :: in                                    !
    !           report,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    integer   :: nkpts
    integer   :: ngv
    integer   :: stdout
    logical,optional :: report


    integer   :: buff_nodes,iN
    logical   :: scheme_found=.false.
    call trace_entry('comms_scheme')

    ! only the 1 process... 
    nprocs=1
    rank=0
    dist_kpt=1
    dist_gvec=1    

    write(stdout,*)"+"//repeat("=",66)//"+"
    call trace_exit('comms_scheme')
    return
  end subroutine comms_scheme



  subroutine COMMS_BARRIER()
    !==============================================================================!
    !                          C O M M S _ B A R R I E R                           !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_BARRIER command, holds each process until     !
    ! each process reaches the same place.                                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    call trace_entry("COMMS_BARRIER")


    call trace_exit("COMMS_BARRIER")
    return
  end subroutine COMMS_BARRIER


  subroutine COMMS_ABORT(error_code)
    !==============================================================================!
    !                            C O M M S _ A B O R T                             !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_ABORT command. Calls an abort to all          !
    ! processes to kill trailing operations when an exception is met.              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           error_code,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: error_code
    call trace_entry("COMMS_ABORT")

    call trace_exit("COMMS_ABORT")

  end subroutine COMMS_ABORT




  subroutine COMMS_VERSION(maj_mpi,min_mpi)
    !==============================================================================!
    !                          C O M M S _ V E R S I O N                           !
    !==============================================================================!
    ! Subroutine wrapper for MPI_GET_VERSION command,  s the version          !
    ! information of the installed MPI libraries.                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           maj_mpi,           intent :: inout                                 !
    !           min_mpi,           intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer, intent(inout):: maj_mpi,min_mpi
    call trace_entry("COMMS_VERSION")

    call trace_exit("COMMS_VERSION")

  end subroutine COMMS_VERSION



  subroutine COMMS_LIBRARY_VERSION(MPI_version)
    !==============================================================================!
    !                  C O M M S _ L I B R A R Y _ V E R S I O N                   !
    !==============================================================================!
    ! Subroutine wrapper for MPI_LIBRARIES_VERSION command.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           MPI_version,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=50),intent(inout) :: MPI_version
    integer ::length


    CALL trace_entry("COMMS_LIBRARY_VERSION")
    MPI_version="Serial"
    call trace_exit("COMMS_LIBRARY_VERSION")

  end subroutine COMMS_LIBRARY_VERSION


  subroutine COMMS_PROC_NAME()
    !==============================================================================!
    !                        C O M M S _ P R O C _ N A M E                         !
    !==============================================================================!
    ! Subroutine wrapper for MPI_GET_PROC_NAME command, gets the id of the         !
    ! processor being queried.                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    call trace_entry("COMMS_PROC_NAME")

    call trace_EXIT("COMMS_PROC_NAME")

  end subroutine COMMS_PROC_NAME


  subroutine COMMS_INIT()
    !==============================================================================!
    !                             C O M M S _ I N I T                              !
    !==============================================================================!
    ! Subroutine wrapper for MPI_INIT, initialises the MPI instances at the        !
    ! start of a program.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: ierr
    call trace_entry("COMMS_INIT")


    rank=0
    nprocs=1

    if (rank.eq.0)then
       on_root_node=.true.
    else
       on_root_node=.false.
    end if
    call trace_exit("COMMS_INIT")

  end subroutine COMMS_INIT
  subroutine COMMS_FINALISE()
    !==============================================================================!
    !                         C O M M S _ F I N A L I S E                          !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_FINALIZE command, finalises the MPI           !
    ! envrionment and closes. No MPI commands should be called after               !
    ! COMMS_FINALISE.                                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!

    return


  end subroutine COMMS_FINALISE


  !Rank and size

  subroutine COMMS_RANK(rank)
    !==============================================================================!
    !                             C O M M S _ R A N K                              !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_RANK command, allocates the rank to each      !
    ! process.                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: rank
    call trace_entry("COMMS_RANK")

    call trace_exit("COMMS_RANK")

  end subroutine COMMS_RANK

  subroutine COMMS_SIZE(nprocs)
    !==============================================================================!
    !                             C O M M S _ S I Z E                              !
    !==============================================================================!
    ! Subrouitine wrapper for the MPI_SIZE command, initialises the number of      !
    ! processes to spawn.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           nprocs,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: nprocs
    call trace_entry("COMMS_SIZE")

    call trace_exit("COMMS_SIZE")
  end subroutine COMMS_SIZE


  function COMMS_WTIME() result(time)
    !==============================================================================!
    !                            C O M M S _ W T I M E                             !
    !==============================================================================!
    ! Function wrapper to time MPI processes.                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           time                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    real(dp):: time
    !    call trace_entry("COMMS_WTIME")
    call cpu_time(time)
    time=real(time,dp)
    !    call trace_exit("COMMS_WTIME")

  end function COMMS_WTIME




  subroutine comms_reduce_int(send_buff,count,op)
    !==============================================================================!
    !                       C O M M S _ R E D U C E _ I N T                        !
    !==============================================================================!
    ! Subroutine for mpi reduce function for integers                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    integer :: recv_buff
    integer,intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_int")


    call trace_exit("comms_reduce_int")


  end subroutine comms_reduce_int

  subroutine comms_reduce_real(send_buff,count,op)
    !==============================================================================!
    !                      C O M M S _ R E D U C E _ R E A L                       !
    !==============================================================================!
    ! Subroutine for mpi reduce function for reals                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    real(dp) :: recv_buff
    real(dp),intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_real")

    call trace_exit("comms_reduce_real")


  end subroutine comms_reduce_real


  subroutine comms_reduce_complex(send_buff,count,op)
    !==============================================================================!
    !                   C O M M S _ R E D U C E _ C O M P L E X                    !
    !==============================================================================!
    ! Subroutine for mpi reduce function for complex                               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    complex(dp) :: recv_buff
    complex(dp),intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_complex")
    call trace_exit("comms_reduce_complex")


  end subroutine comms_reduce_complex



  subroutine comms_distribute(loc_kpt_array,loc_grid_array,loc_fine_grid_array,&
       & num_kpts,num_g_points,num_fine_g_points,num_g_node,num_fine_g_node)
    !==============================================================================!
    !                       C O M M S _ D I S T R I B U T E                        !
    !==============================================================================!
    ! Subroutine for calculating the distribution on the processes                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           loc_kpt_array,     intent :: in                                    !
    !           loc_grid_array,    intent :: in                                    !
    !           loc_fine_grid_array,intent :: in                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    integer,dimension(:),allocatable,intent(inout) ::loc_kpt_array
    integer,dimension(:),allocatable,intent(inout) ::loc_grid_array
    integer,dimension(:),allocatable,intent(inout) ::loc_fine_grid_array

    integer,intent(in)  :: num_kpts
    integer,intent(in)  :: num_g_points
    integer,intent(in)  :: num_fine_g_points

    integer,intent(out) :: num_g_node
    integer,intent(out) :: num_fine_g_node    
    integer :: in,ik,ig,gc
    call trace_entry("comms_distribute")

    ! Distribute the kpoints

    if (dist_kpt.eq.1)then
       do ik = 1,size(loc_kpt_array)
          loc_kpt_array(ik)=ik
       end do
       gc=0

       do ig=rank+1,num_g_points,dist_gvec
          gc=gc+1
          loc_grid_array(gc)=ig
       end do
       num_g_node=gc
       gc=0
       do ig=rank+1,num_fine_g_points,dist_gvec
          gc=gc+1
          loc_fine_grid_array(gc)=ig
       end do
       num_fine_g_node=gc
       ! Case where kpts are distirbuted more than nodes
    elseif (num_kpts.gt.nprocs)then
       do ik = 1,size(loc_kpt_array)
          loc_kpt_array(ik)=rank+1+(ik-1)*dist_kpt
       end do

       do ig=1,num_g_points
          loc_grid_array(ig)=ig
       end do
       do ig=1,num_fine_g_points
          loc_fine_grid_array(ig)=ig
       end do
       num_g_node=num_g_points
       num_fine_g_node=num_fine_g_points

    elseif (num_kpts.lt.nprocs)then
       ! there is always only 1 kpt per node
       loc_kpt_array(1) = rank+1-dist_kpt*int(real(rank,dp)/real(dist_kpt,dp))
       gc=0
       do ig=loc_kpt_array(1),num_g_points,dist_gvec
          gc=gc+1
          loc_grid_array(gc)=ig
       end do
       num_g_node=gc
       gc=0
       do ig=loc_kpt_array(1),num_fine_g_points,dist_gvec
          gc=gc+1
          loc_fine_grid_array(gc)=ig
       end do
       num_fine_g_node=gc


    end if



    !print*,rank,loc_kpt_array
    call trace_exit("comms_distribute")
    return
  end subroutine comms_distribute
  subroutine comms_send_int(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                         c o m m s _ s e n d _ i n t                          !
    !==============================================================================!
    ! subroutine wrapper for sending 1d data of type int.                          !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    integer :: send_buff
    !    print*,send_buff
    call trace_entry("comms_send_int")
    call trace_exit("comms_send_int")

  end subroutine comms_send_int

  subroutine comms_send_real(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                        c o m m s _ s e n d _ r e a l                         !
    !==============================================================================!
    ! subroutine wrapper for sending 1d data of type real                          !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp):: send_buff
    call trace_entry("comms_send_real")

    call trace_exit("comms_send_real")

  end subroutine comms_send_real

  subroutine comms_send_double(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                      c o m m s _ s e n d _ d o u b l e                       !
    !==============================================================================!
    ! subroutine wrapper for sending 1d data of type double                        !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp):: send_buff
    call trace_entry("comms_send_double")

    call trace_exit("comms_send_double")

  end subroutine comms_send_double



  subroutine comms_send_int_array(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                   c o m m s _ s e n d _ i n t _ a r r a y                    !
    !==============================================================================!
    ! subroutine wrapper for sending array data of type int.                       !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    integer,dimension(1:count) :: send_buff
    call trace_entry("comms_send_int_array")

    call trace_exit("comms_send_int_array")

  end subroutine comms_send_int_array

  subroutine comms_send_real_array(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                  c o m m s _ s e n d _ r e a l _ a r r a y                   !
    !==============================================================================!
    ! subroutine wrapper for sending array data of type real.                      !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp),dimension(1:count) :: send_buff
    call trace_entry("comms_send_real_array")

    call trace_exit("comms_send_real_array")

  end subroutine comms_send_real_array

  subroutine comms_send_double_array(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                c o m m s _ s e n d _ d o u b l e _ a r r a y                 !
    !==============================================================================!
    ! subroutine wrapper for sending array data of type double.                    !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp),dimension(1:count) :: send_buff
    call trace_entry("comms_send_double_array")

    call trace_exit("comms_send_double_array")

  end subroutine comms_send_double_array


  !2d array
  subroutine comms_send_real_array2d(send_buff,count1,count2,dest_rank,tag)
    !==============================================================================!
    !                c o m m s _ s e n d _ r e a l _ a r r a y 2 d                 !
    !==============================================================================!
    ! subroutine wrapper for sending 2d array data of type real.                   !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count1,count2,dest_rank,tag
    real(dp),dimension(count1,count2) :: send_buff
    call trace_entry("comms_send_real_array2d")
    call trace_exit("comms_send_real_array2d")

  end subroutine comms_send_real_array2d

  subroutine comms_send_recv_real_array2d(send_buff,recv_buff,count1,count2,dest_rank,tag,send_rank)
    !==============================================================================!
    !           c o m m s _ s e n d _ r e c v _ r e a l _ a r r a y 2 d            !
    !==============================================================================!
    ! subroutine wrapper for recieving 2d arrays of real data.                     !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: in                                    !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !           send_rank,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count1,count2,dest_rank,tag,send_rank
    real(dp),dimension(count1,count2) :: send_buff,recv_buff
    call trace_entry("comms_send_recv_real_array2d")

    call trace_exit("comms_send_recv_real_array2d")


  end subroutine comms_send_recv_real_array2d



  !recv routines

  subroutine comms_recv_int(recv_buff,count,source,tag)
    !==============================================================================!
    !                         c o m m s _ r e c v _ i n t                          !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type int.                           !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    integer, intent(inout) :: recv_buff
    !   print*, "message in routine"
    call trace_entry("comms_recv_int")

    !    print*, "after",recv_buff
    call trace_exit("comms_recv_int")

  end subroutine comms_recv_int

  subroutine comms_recv_real(recv_buff,count,source,tag)
    !==============================================================================!
    !                        c o m m s _ r e c v _ r e a l                         !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type real.                          !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    real(dp), intent(inout) :: recv_buff
    call trace_entry("comms_recv_real")
    !   print*, "recv sent to routine"

    !  print*, "recv success from rank",source 
    call trace_exit("comms_recv_real")

  end subroutine comms_recv_real

  subroutine comms_recv_double(recv_buff,count,source,tag)
    !==============================================================================!
    !                      c o m m s _ r e c v _ d o u b l e                       !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type double.                        !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    real(dp),intent(inout) :: recv_buff
    call trace_entry("comms_recv_double")

    call trace_exit("comms_recv_double")

  end subroutine comms_recv_double




  subroutine comms_recv_int_array(recv_buff,count,source,tag)
    !==============================================================================!
    !                   c o m m s _ r e c v _ i n t _ a r r a y                    !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type int.                           !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    integer,dimension(1:count),intent(inout) :: recv_buff


  end subroutine comms_recv_int_array

  subroutine comms_recv_real_array(recv_buff,count,source,tag)
    !==============================================================================!
    !                  c o m m s _ r e c v _ r e a l _ a r r a y                   !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type real                           !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count,source,tag
    real(dp),dimension(1:count),intent(inout) :: recv_buff
    call trace_entry("comms_recv_real_array")

    call trace_exit("comms_recv_real_array")

  end subroutine comms_recv_real_array

  subroutine comms_recv_double_array(recv_buff,count,source,tag)
    !==============================================================================!
    !                c o m m s _ r e c v _ d o u b l e _ a r r a y                 !
    !==============================================================================!
    ! subroutine wrapper for recieving data of type double.                        !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count,source,tag
    real(dp),dimension(1:count),intent(inout) :: recv_buff
    call trace_entry("comms_recv_double_array")

    call trace_exit("comms_recv_double_array")

  end subroutine comms_recv_double_array

  !2d array
  subroutine comms_recv_real_array2d(recv_buff,count1,count2,source,tag)
    !==============================================================================!
    !                c o m m s _ r e c v _ r e a l _ a r r a y 2 d                 !
    !==============================================================================!
    ! subroutine wrapper for reciving 2d arrays of real data.                      !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count1,count2,source,tag
    real(dp),dimension(count1,count2),intent(inout) :: recv_buff
    call trace_entry("comms_recv_real_array2d")

    call trace_exit("comms_recv_real_array2d")

  end subroutine comms_recv_real_array2d


  !BCAST routines
  subroutine COMMS_BCAST_INT(start_buff,count)
    !==============================================================================!
    !                        C O M M S _ B C A S T _ I N T                         !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting integer data from the root to all        !
    ! children processes.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    integer :: start_buff
    call trace_entry("COMMS_BCAST_INT")

    call trace_exit("COMMS_BCAST_INT")
  end subroutine COMMS_BCAST_INT

  subroutine COMMS_BCAST_REAL(start_buff,count)
    !==============================================================================!
    !                       C O M M S _ B C A S T _ R E A L                        !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting real data from the root to all           !
    ! children processes.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real :: start_buff
    call trace_entry("COMMS_BCAST_REAL")

    call trace_exit("COMMS_BCAST_REAL")
  end subroutine COMMS_BCAST_REAL

  subroutine COMMS_BCAST_DOUBLE(start_buff,count)
    !==============================================================================!
    !                     C O M M S _ B C A S T _ D O U B L E                      !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting double precision data from the root to   !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    double precision :: start_buff
    call trace_entry("COMMS_BCAST_DOUBLE")

    call trace_exit("COMMS_BCAST_DOUBLE")
  end subroutine COMMS_BCAST_DOUBLE
  !ARRAY
  subroutine COMMS_BCAST_INT_ARRAY(start_buff,count)
    !==============================================================================!
    !                  C O M M S _ B C A S T _ I N T _ A R R A Y                   !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of integer data from the root to   !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    integer,dimension(count) :: start_buff
    call trace_entry("COMMS_BCAST_INT_ARRAY")

    call trace_exit("COMMS_BCAST_INT_ARRAY")
  end subroutine COMMS_BCAST_INT_ARRAY

  subroutine COMMS_BCAST_REAL_ARRAY(start_buff,count)
    !==============================================================================!
    !                 C O M M S _ B C A S T _ R E A L _ A R R A Y                  !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of real data from thr root to      !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real,dimension(count) :: start_buff

  end subroutine COMMS_BCAST_REAL_ARRAY

  subroutine COMMS_BCAST_DOUBLE_ARRAY(start_buff,count)
    !==============================================================================!
    !               C O M M S _ B C A S T _ D O U B L E _ A R R A Y                !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of double precision data from      !
    ! the root to all children processes.                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    double precision,dimension(count) :: start_buff
    call trace_entry("COMMS_BCAST_DOUBLE_ARRAY")

    call trace_exit("COMMS_BCAST_DOUBLE_ARRAY")
  end subroutine COMMS_BCAST_DOUBLE_ARRAY
  subroutine comms_bcast_logical(start_buff)
    !==============================================================================!
    !                    C O M M S _ B C A S T _ L O G I C A L                     !
    !==============================================================================!
    ! Mpi wrapper for broacasting logical variable from rood node to all           !
    ! childeren                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    logical :: start_buff
    call trace_entry('comms_bcast_logical')

    call trace_exit('comms_bcast_logical')
  end subroutine comms_bcast_logical

  function  comms_wall_time() result(time)
    use trace, only : global_time,global_start,trace_entry,trace_exit
    real(dp) :: time
    call trace_entry('comms_wall_time')
    ! this is called on all nodes, it starts by setting the global time variable to 0
    call cpu_time(time)
    time = time - global_start

    call comms_reduce(global_time,1,"max")

    call trace_exit('comms_wall_time')
  end function comms_wall_time

end module COMMS
