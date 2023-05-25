!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by ForOAAtran Documenter, Z.Hawkhead                                                                                                                                              
!=============================================================================!                                                                                                                         
!                                  COMMS                                      !                                                                                                                         
!=============================================================================!                                                                                                                         
!              Module handining comminications: MPI                           !                                                                                                                         
!-----------------------------------------------------------------------------!                                                                                                                         
!                        author: Z. Hawkhead                                  !                                                                                                                         
!=============================================================================!                                                                                                                         
module COMMS
  !use mpi                                                                                                                                                                                              
  use iso_fortran_env , only : real64
  use trace
  implicit none
  include 'mpif.h'
  integer                              :: ierr
  integer,parameter                    :: max_version_length=MPI_MAX_LIBRARY_VERSION_STRING
  integer, dimension(MPI_STATUS_SIZE)  :: status1
  !integer,parameter,private :: dp=real64                                                                                                                                                               
  ! Some of the stuff i'll need, gloabal                                                                                                                                                                
  integer,public,save                  :: rank
  integer,public,save                  :: nprocs
  logical,public,save                  :: on_root_node
  integer,public,save                  :: dist_kpt
  integer,public,save                  :: dist_gvec

  character(3)                         :: comms_arch="MPI"


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


  public COMMS_WTIME
  public COMMS_INIT
  public COMMS_FINALISE
  public COMMS_ABORT
  public comms_barrier
  public COMMS_VERSION
  public COMMS_LIBRARY_VERSION
  public comms_reduce
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


    ! For the comms scheme we need it such that kpt*gvec ~ nodes

    ! Set buff_nodes to nprocs, might need reducing if we can't find a scheme
    buff_nodes=nprocs

    ! Check if nprocs is divisible by nkpt

    do iN=0,nprocs/10
       buff_nodes=buff_nodes-iN       
       if (modulo(buff_nodes,nkpts).eq.0)then
          ! This is the easy option, if the number of cores is divisible by nkpts we have a nice situ
          dist_kpt=nkpts
          dist_gvec=buff_nodes/nkpts
          scheme_found=.true.
          exit
          ! The situ if buff_nodes lt nkpts
       elseif (modulo(nkpts,buff_nodes).eq.0)then
          dist_kpt=buff_nodes
          dist_gvec=1
          scheme_found=.true.
          exit       
       end if
    end do


    if (.not.scheme_found)then
       ! we failed to find a good scheme so now we do all G vector parallel
       dist_kpt=1
       dist_gvec=nprocs
    end if


    if (present(report))then
       if (report)then
          if (on_root_node)then
             write(stdout,*)
             write(stdout,*)"                           Comms Parameters"
             write(stdout,*)"                           ----------------"
             write(stdout,16)"Number of processes",nprocs
             write(stdout,16)"Distributed by kpoint",dist_kpt
             write(stdout,16)"Distributed by G vector",dist_gvec
             write(stdout,*)"+"//repeat("-",66)//"+"
             write(stdout,*)
          end if
       end if
    end if
15  format(T2,a,T36,":",T58,a12)   ! Character                                                                                                                                                          
16  format(T2,a,T36,":",T58,i12)   ! Integer                                                                                                                                                            
17  format(T2,a,T36,":",T58,f12.3)   ! Real                                                                                                                                                             
18  format(T2,a,T36,":",T58,ES12.2)   ! Science                                                                                                                                                         
19  format(T2,a,T36,":",T58,L12)   ! Logical  



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

    call MPI_BARRIER(MPI_COMM_WORLD,1)
    call trace_exit("COMMS_BARRIER")

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
    call MPI_ABORT(MPI_COMM_WORLD,error_code,ierr)
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
    call MPI_GET_VERSION(maj_MPI,min_MPI,ierr)
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
    character(len=max_version_length),intent(inout) :: MPI_version
    integer ::length


    CALL trace_entry("COMMS_LIBRARY_VERSION")
    call MPI_GET_LIBRARY_VERSION(MPI_version,length,ierr)
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
    character*(MPI_MAX_PROCESSOR_NAME) ::proc_name
    integer :: proc_name_len
    call trace_entry("COMMS_PROC_NAME")
    call MPI_GET_PROCESSOR_NAME(proc_name,proc_name_len,ierr)
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

    call MPI_INIT(ierr)

    call COMMS_RANK(rank)
    call COMMS_SIZE(nprocs)
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
    integer :: ierr

    call MPI_FINALIZE(ierr)


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
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
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
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
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
    time = MPI_WTIME()
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
    select case(trim(op))
    case('max')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,MPI_MAX,0,mpi_comm_world,status1,ierr)
    case('min')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,MPI_MIN,0,mpi_comm_world,status1,ierr)
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,MPI_SUM,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

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
    select case(trim(op))
    case('max')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,MPI_MAX,0,mpi_comm_world,status1,ierr)
    case('min')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,MPI_MIN,0,mpi_comm_world,status1,ierr)
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,MPI_SUM,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

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
    select case(trim(op))
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_complex,MPI_SUM,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

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


end module COMMS
