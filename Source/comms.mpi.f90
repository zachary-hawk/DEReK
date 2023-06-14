!---- file documented by fortran documenter, z.hawkhead
!---- file documented by fortran documenter, z.hawkhead
!---- file documented by fortran documenter, z.hawkhead
!=============================================================================!                                                                                                                         
!                                  comms                                      !                                                                                                                         
!=============================================================================!                                                                                                                         
!              module handeling comminications: mpi                           !                                                                                                                         
!-----------------------------------------------------------------------------!                                                                                                                         
!                        author: z. hawkhead                                  !                                                                                                                         
!=============================================================================!                                                                                                                         
module comms
  !use mpi                                                                                                                                                                                              
  use constants
  use trace
  implicit none
  include 'mpif.h'
  integer                              :: ierr
  integer,parameter                    :: max_version_length=mpi_max_library_version_string
  integer, dimension(mpi_status_size)  :: status1
  !integer,parameter,private :: dp=real64                                                                                                                                                               
  ! some of the stuff i'll need, gloabal                                                                                                                                                                
  integer,public,save                  :: rank
  integer,public,save                  :: nprocs
  logical,public,save                  :: on_root_node
  integer,public,save                  :: dist_kpt
  integer,public,save                  :: dist_gvec

  character(3)                         :: comms_arch="mpi"


  !-------------------------------------------------------!
  !              p u b l i c  r o u t i n e s             !
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
    !                           c o m m s _ s c h e m e                            !
    !==============================================================================!
    ! subroutine to decide how the kpoint and gvectors are split amongst the       !
    ! processes                                                                    !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           nkpts,             intent :: in                                    !
    !           ngv,               intent :: in                                    !
    !           stdout,            intent :: in                                    !
    !           report,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  11/01/2022                                            !
    !==============================================================================!
    integer   :: nkpts
    integer   :: ngv
    integer   :: stdout
    logical,optional :: report


    integer   :: buff_nodes,in
    logical   :: scheme_found=.false.
    call trace_entry('comms_scheme')


    ! for the comms scheme we need it such that kpt*gvec ~ nodes

    ! set buff_nodes to nprocs, might need reducing if we can't find a scheme
    buff_nodes=nprocs

    ! check if nprocs is divisible by nkpt

    do in=0,nprocs/10
       buff_nodes=buff_nodes-in       
       if (modulo(buff_nodes,nkpts).eq.0)then
          ! this is the easy option, if the number of cores is divisible by nkpts we have a nice situ
          dist_kpt=nkpts
          dist_gvec=buff_nodes/nkpts
          scheme_found=.true.
          exit
          ! the situ if buff_nodes lt nkpts
       elseif (modulo(nkpts,buff_nodes).eq.0)then
          dist_kpt=buff_nodes
          dist_gvec=1
          scheme_found=.true.
          exit       
       end if
    end do


    if (.not.scheme_found)then
       ! we failed to find a good scheme so now we do all g vector parallel
       dist_kpt=1
       dist_gvec=nprocs
    end if


    if (present(report))then
       if (report)then
          if (on_root_node)then
             write(stdout,*)
             write(stdout,*)"                           Comms parameters"
             write(stdout,*)"                           ----------------"
             write(stdout,16)"Number of processes",nprocs
             write(stdout,16)"Distributed by kpoint",dist_kpt
             write(stdout,16)"Distributed by g vector",dist_gvec
             write(stdout,*)"+"//repeat("-",66)//"+"
             write(stdout,*)
          end if
       end if
    end if
15  format(t2,a,t36,":",t58,a12)   ! character                                                                                                                                                          
16  format(t2,a,t36,":",t58,i12)   ! integer                                                                                                                                                            
17  format(t2,a,t36,":",t58,f12.3)   ! real                                                                                                                                                             
18  format(t2,a,t36,":",t58,es12.2)   ! science                                                                                                                                                         
19  format(t2,a,t36,":",t58,l12)   ! logical  



    call trace_exit('comms_scheme')
    return
  end subroutine comms_scheme



  subroutine comms_barrier()
    !==============================================================================!
    !                          c o m m s _ b a r r i e r                           !
    !==============================================================================!
    ! subroutine wrapper for the mpi_barrier command, holds each process until     !
    ! each process reaches the same place.                                         !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    call trace_entry("comms_barrier")

    call mpi_barrier(mpi_comm_world,1)
    call trace_exit("comms_barrier")

  end subroutine comms_barrier


  subroutine comms_abort(error_code)
    !==============================================================================!
    !                            c o m m s _ a b o r t                             !
    !==============================================================================!
    ! subroutine wrapper for the mpi_abort command. calls an abort to all          !
    ! processes to kill trailing operations when an exception is met.              !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           error_code,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: error_code
    call trace_entry("comms_abort")
    call mpi_abort(mpi_comm_world,error_code,ierr)
    call trace_exit("comms_abort")

  end subroutine comms_abort




  subroutine comms_version(maj_mpi,min_mpi)
    !==============================================================================!
    !                          c o m m s _ v e r s i o n                           !
    !==============================================================================!
    ! subroutine wrapper for mpi_get_version command,  s the version          !
    ! information of the installed mpi libraries.                                  !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           maj_mpi,           intent :: inout                                 !
    !           min_mpi,           intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer, intent(inout):: maj_mpi,min_mpi
    call trace_entry("comms_version")
    call mpi_get_version(maj_mpi,min_mpi,ierr)
    call trace_exit("comms_version")

  end subroutine comms_version



  subroutine comms_library_version(mpi_version)
    !==============================================================================!
    !                  c o m m s _ l i b r a r y _ v e r s i o n                   !
    !==============================================================================!
    ! subroutine wrapper for mpi_libraries_version command.                        !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           mpi_version,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=max_version_length),intent(inout) :: mpi_version
    integer ::length


    call trace_entry("comms_library_version")
    call mpi_get_library_version(mpi_version,length,ierr)
    call trace_exit("comms_library_version")

  end subroutine comms_library_version


  subroutine comms_proc_name()
    !==============================================================================!
    !                        c o m m s _ p r o c _ n a m e                         !
    !==============================================================================!
    ! subroutine wrapper for mpi_get_proc_name command, gets the id of the         !
    ! processor being queried.                                                     !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    character*(mpi_max_processor_name) ::proc_name
    integer :: proc_name_len
    call trace_entry("comms_proc_name")
    call mpi_get_processor_name(proc_name,proc_name_len,ierr)
    call trace_exit("comms_proc_name")

  end subroutine comms_proc_name


  subroutine comms_init()
    !==============================================================================!
    !                             c o m m s _ i n i t                              !
    !==============================================================================!
    ! subroutine wrapper for mpi_init, initialises the mpi instances at the        !
    ! start of a program.                                                          !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: ierr
    call trace_entry("comms_init")

    call mpi_init(ierr)

    call comms_rank(rank)
    call comms_size(nprocs)
    if (rank.eq.0)then
       on_root_node=.true.
    else
       on_root_node=.false.
    end if
    call trace_exit("comms_init")

  end subroutine comms_init
  subroutine comms_finalise()
    !==============================================================================!
    !                         c o m m s _ f i n a l i s e                          !
    !==============================================================================!
    ! subroutine wrapper for the mpi_finalize command, finalises the mpi           !
    ! envrionment and closes. no mpi commands should be called after               !
    ! comms_finalise.                                                              !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: ierr

    call mpi_finalize(ierr)


  end subroutine comms_finalise


  !rank and size

  subroutine comms_rank(rank)
    !==============================================================================!
    !                             c o m m s _ r a n k                              !
    !==============================================================================!
    ! subroutine wrapper for the mpi_rank command, allocates the rank to each      !
    ! process.                                                                     !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: rank
    call trace_entry("comms_rank")
    call mpi_comm_rank(mpi_comm_world,rank,ierr)
    call trace_exit("comms_rank")

  end subroutine comms_rank

  subroutine comms_size(nprocs)
    !==============================================================================!
    !                             c o m m s _ s i z e                              !
    !==============================================================================!
    ! subrouitine wrapper for the mpi_size command, initialises the number of      !
    ! processes to spawn.                                                          !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           nprocs,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: nprocs
    call trace_entry("comms_size")
    call mpi_comm_size(mpi_comm_world,nprocs,ierr)
    call trace_exit("comms_size")
  end subroutine comms_size


  function comms_wtime() result(time)
    !==============================================================================!
    !                            c o m m s _ w t i m e                             !
    !==============================================================================!
    ! function wrapper to time mpi processes.                                      !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! result:                                                                      !
    !           time                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  16/08/2019                                            !
    !==============================================================================!
    real(dp):: time
    !    call trace_entry("comms_wtime")
    time = mpi_wtime()
    !    call trace_exit("comms_wtime")

  end function comms_wtime




  subroutine comms_reduce_int(send_buff,count,op)
    !==============================================================================!
    !                       c o m m s _ r e d u c e _ i n t                        !
    !==============================================================================!
    ! subroutine for mpi reduce function for integers                              !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    integer :: recv_buff
    integer,intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_int")
    select case(trim(op))
    case('max')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,mpi_max,0,mpi_comm_world,status1,ierr)
    case('min')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,mpi_min,0,mpi_comm_world,status1,ierr)
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_int,mpi_sum,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

    call trace_exit("comms_reduce_int")


  end subroutine comms_reduce_int

  subroutine comms_reduce_real(send_buff,count,op)
    !==============================================================================!
    !                      c o m m s _ r e d u c e _ r e a l                       !
    !==============================================================================!
    ! subroutine for mpi reduce function for reals                                 !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    real(dp) :: recv_buff
    real(dp),intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_real")
    select case(trim(op))
    case('max')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,mpi_max,0,mpi_comm_world,status1,ierr)
    case('min')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,mpi_min,0,mpi_comm_world,status1,ierr)
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_double,mpi_sum,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

    call trace_exit("comms_reduce_real")


  end subroutine comms_reduce_real


  subroutine comms_reduce_complex(send_buff,count,op)
    !==============================================================================!
    !                   c o m m s _ r e d u c e _ c o m p l e x                    !
    !==============================================================================!
    ! subroutine for mpi reduce function for complex                               !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           send_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           op,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  11/01/2022                                            !
    !==============================================================================!

    integer:: count
    complex(dp) :: recv_buff
    complex(dp),intent(inout) :: send_buff
    character(*) :: op
    call trace_entry("comms_reduce_complex")
    select case(trim(op))
    case('sum')
       call mpi_reduce(send_buff,recv_buff,count,mpi_complex,mpi_sum,0,mpi_comm_world,status1,ierr)
    end select
    ! put it back in
    send_buff=recv_buff

    call trace_exit("comms_reduce_complex")


  end subroutine comms_reduce_complex



  subroutine comms_distribute(loc_kpt_array,loc_grid_array,loc_fine_grid_array,&
       & num_kpts,num_g_points,num_fine_g_points,num_g_node,num_fine_g_node)
    !==============================================================================!
    !                       c o m m s _ d i s t r i b u t e                        !
    !==============================================================================!
    ! subroutine for calculating the distribution on the processes                 !
    !------------------------------------------------------------------------------!
    ! arguments:                                                                   !
    !           loc_kpt_array,     intent :: in                                    !
    !           loc_grid_array,    intent :: in                                    !
    !           loc_fine_grid_array,intent :: in                                   !
    !           none                                                               !
    !------------------------------------------------------------------------------!
    ! author:   z. hawkhead  11/01/2022                                            !
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

    ! distribute the kpoints

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
       ! case where kpts are distirbuted more than nodes
    elseif (num_kpts.ge.nprocs)then
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
  !send routines
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
    call mpi_send(send_buff,count,mpi_int,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count,mpi_float,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count,mpi_double,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count,mpi_int,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count,mpi_float,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count,mpi_double,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_send(send_buff,count1*count2,mpi_float,dest_rank,tag,mpi_comm_world,ierr)
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
    call mpi_sendrecv(send_buff,count1*count2,mpi_float,dest_rank,tag,recv_buff,count1*count2,&
         mpi_float,send_rank,tag,mpi_comm_world,ierr)
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
    call mpi_recv(recv_buff,count,mpi_int,source,tag,mpi_comm_world,status1,ierr)
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
    call mpi_recv(recv_buff,count,mpi_real,source,tag,mpi_comm_world,status1,ierr)
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
    call mpi_recv(recv_buff,count,mpi_double,source,tag,mpi_comm_world,status1,ierr)
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
    call mpi_recv(recv_buff,count,mpi_int,source,tag,mpi_comm_world,status1,ierr)

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
    call mpi_recv(recv_buff,count,mpi_real,source,tag,mpi_comm_world,status1,ierr)
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
    call mpi_recv(recv_buff,count,mpi_double,source,tag,mpi_comm_world,status1,ierr)
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
    call mpi_recv(recv_buff,count1*count2,mpi_real,source,tag,mpi_comm_world,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,status1,ierr)
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
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_DOUBLE_ARRAY")
  end subroutine COMMS_BCAST_DOUBLE_ARRAY







end module comms
