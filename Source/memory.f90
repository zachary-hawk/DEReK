!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module memory
  use constants 
  use trace, only : trace_entry, trace_exit
  use comms, only : rank,nprocs

  implicit none
  real(dp), public,save :: tot_memory
  real(dp), public,save :: io_memory
  real(dp), public,save :: wave_memory
  real(dp), public,save :: basis_memory
  real(dp), public,save :: pot_memory
  real(dp), public,save :: gen_memory
  real(dp), public,save :: den_memory

  character(20),private :: local_seed
  logical,private       :: local_write_mem
  private

  interface memory_allocate
     module procedure memory_allocate_1d_real
     module procedure memory_allocate_2d_real
     module procedure memory_allocate_3d_real
     module procedure memory_allocate_4d_real
     module procedure memory_allocate_5d_real
     module procedure memory_allocate_1d_complex
     module procedure memory_allocate_2d_complex
     module procedure memory_allocate_3d_complex
     module procedure memory_allocate_4d_complex
     module procedure memory_allocate_5d_complex
     module procedure memory_allocate_1d_integer
     module procedure memory_allocate_2d_integer
     module procedure memory_allocate_3d_integer
     module procedure memory_allocate_4d_integer
     module procedure memory_allocate_5d_integer
  end interface memory_allocate


  interface memory_deallocate
     module procedure memory_deallocate_1d_real
     module procedure memory_deallocate_2d_real
     module procedure memory_deallocate_3d_real
     module procedure memory_deallocate_4d_real
     module procedure memory_deallocate_5d_real
     module procedure memory_deallocate_1d_complex
     module procedure memory_deallocate_2d_complex
     module procedure memory_deallocate_3d_complex
     module procedure memory_deallocate_4d_complex
     module procedure memory_deallocate_5d_complex
     module procedure memory_deallocate_1d_integer
     module procedure memory_deallocate_2d_integer
     module procedure memory_deallocate_3d_integer
     module procedure memory_deallocate_4d_integer
     module procedure memory_deallocate_5d_integer
  end interface memory_deallocate




  public memory_allocate
  public memory_deallocate
  public memory_init

contains

  subroutine memory_init(seed,write_mem)
    !==============================================================================!
    !                            M E M O R Y _ I N I T                             !
    !==============================================================================!
    ! Subroutine for initalising the memory tracking                               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           seed,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    character(*) :: seed
    logical      :: write_mem
    call trace_entry('memory_init')

    io_memory=0.0_dp
    wave_memory=0.0_dp
    basis_memory=0.0_dp
    pot_memory=0.0_dp
    gen_memory=0.0_dp

    tot_memory=0.0_dp


    local_seed=seed
    local_write_mem = write_mem
    call trace_exit('memory_init')
  end subroutine memory_init

  subroutine memory_allocate_1d_real(array,l1,l2,mem_type)
    !==============================================================================!
    !                M E M O R Y _ A L L O C A T E _ 1 D _ R E A L                 !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 1D real array         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2 
    integer  :: stat
    character(1) :: mem_type

    call trace_entry('memory_allocate_1d_real')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_1d_real: More memory required")


    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select




    tot_memory=tot_memory+sizeof(array)


    call memory_trace()
    call trace_exit('memory_allocate_1d_real')
  end subroutine memory_allocate_1d_real

  subroutine memory_allocate_2d_real(array,l1,l2,l3,l4,mem_type)
    !==============================================================================!
    !                M E M O R Y _ A L L O C A T E _ 2 D _ R E A L                 !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating 2D real array           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_2d_real')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_2d_real: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)
    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_2d_real')
  end subroutine memory_allocate_2d_real

  subroutine memory_allocate_3d_real(array,l1,l2,l3,l4,l5,l6,mem_type)
    !==============================================================================!
    !                M E M O R Y _ A L L O C A T E _ 3 D _ R E A L                 !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 3D real array         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_3d_real')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_3d_real: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)
    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_3d_real')
  end subroutine memory_allocate_3d_real

  subroutine memory_allocate_4d_real(array,l1,l2,l3,l4,l5,l6,l7,l8,mem_type)
    !==============================================================================!
    !                M E M O R Y _ A L L O C A T E _ 4 D _ R E A L                 !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating 4D real array           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_4d_real')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_4d_real: More memory required")

    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_4d_real')
  end subroutine memory_allocate_4d_real

  subroutine memory_allocate_5d_real(array,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,mem_type)
    !==============================================================================!
    !                M E M O R Y _ A L L O C A T E _ 5 D _ R E A L                 !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 5D real array         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           l9,                intent :: in                                    !
    !           l10,               intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8,l9,l10
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_5d_real')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8,l9:l10),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_5d_real: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)
    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_5d_real')
  end subroutine memory_allocate_5d_real








  subroutine memory_allocate_1d_complex(array,l1,l2,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 1 D _ C O M P L E X              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 1D complex array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_1d_complex')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_1d_complex: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_1d_complex')
  end subroutine memory_allocate_1d_complex

  subroutine memory_allocate_2d_complex(array,l1,l2,l3,l4,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 2 D _ C O M P L E X              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 2D complex array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_2d_complex')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_2d_complex: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_2d_complex')
  end subroutine memory_allocate_2d_complex

  subroutine memory_allocate_3d_complex(array,l1,l2,l3,l4,l5,l6,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 3 D _ C O M P L E X              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 3D complex array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_3d_complex')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_3d_complex: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_3d_complex')
  end subroutine memory_allocate_3d_complex

  subroutine memory_allocate_4d_complex(array,l1,l2,l3,l4,l5,l6,l7,l8,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 4 D _ C O M P L E X              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 4D complex array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_4d_complex')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_4d_complex: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_4d_complex')
  end subroutine memory_allocate_4d_complex

  subroutine memory_allocate_5d_complex(array,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 5 D _ C O M P L E X              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 5D complex array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           l9,                intent :: in                                    !
    !           l10,               intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8,l9,l10
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_5d_complex')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8,l9:l10),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_5d_complex: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_5d_complex')
  end subroutine memory_allocate_5d_complex










  subroutine memory_allocate_1d_integer(array,l1,l2,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 1 D _ I N T E G E R              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 1D integer array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_1d_integer')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_1d_integer: More memory required")

    tot_memory=tot_memory+sizeof(array)

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case('G')
       gen_memory=gen_memory+sizeof(array)
    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    call memory_trace()
    call trace_exit('memory_allocate_1d_integer')
  end subroutine memory_allocate_1d_integer

  subroutine memory_allocate_2d_integer(array,l1,l2,l3,l4,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 2 D _ I N T E G E R              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 2D integer array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_2d_integer')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_2d_integer: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_2d_integer')
  end subroutine memory_allocate_2d_integer

  subroutine memory_allocate_3d_integer(array,l1,l2,l3,l4,l5,l6,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 3 D _ I N T E G E R              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 3D integer array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_3d_integer')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_3d_integer: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_3d_integer')
  end subroutine memory_allocate_3d_integer

  subroutine memory_allocate_4d_integer(array,l1,l2,l3,l4,l5,l6,l7,l8,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 4 D _ I N T E G E R              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 4D integer array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8 
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_4d_integer')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_4d_integer: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_4d_integer')
  end subroutine memory_allocate_4d_integer

  subroutine memory_allocate_5d_integer(array,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,mem_type)
    !==============================================================================!
    !             M E M O R Y _ A L L O C A T E _ 5 D _ I N T E G E R              !
    !==============================================================================!
    ! Subroutine for calculating the memory and allocating a 5D integer array      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           l1,                intent :: in                                    !
    !           l2,                intent :: in                                    !
    !           l3,                intent :: in                                    !
    !           l4,                intent :: in                                    !
    !           l5,                intent :: in                                    !
    !           l6,                intent :: in                                    !
    !           l7,                intent :: in                                    !
    !           l8,                intent :: in                                    !
    !           l9,                intent :: in                                    !
    !           l10,               intent :: in                                    !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:,:,:), allocatable,intent(inout)  :: array
    integer,intent(in)  :: l1,l2,l3,l4,l5,l6,l7,l8,l9,l10
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_allocate_5d_integer')
    if (allocated(array)) call memory_deallocate(array,mem_type)
    allocate(array(l1:l2,l3:l4,l5:l6,l7:l8,l9:l10),stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_allocate_5d_integer: More memory required")

    select case(mem_type)
    case('I')
       io_memory=io_memory+sizeof(array)
    case('P')
       pot_memory=pot_memory+sizeof(array)       
    case('W')
       wave_memory=wave_memory+sizeof(array)
    case('B')
       basis_memory=basis_memory+sizeof(array)
    case('G')
       gen_memory=gen_memory+sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_allocate: Unknown array type")
    end select
    tot_memory=tot_memory+sizeof(array)

    call memory_trace()
    call trace_exit('memory_allocate_5d_integer')
  end subroutine memory_allocate_5d_integer






!!!!!!!!!!!!!!!!! DEALLOCATE ARRAYS !!!!!!!!!!!!!!!!!!!!!! 


  subroutine memory_deallocate_1d_real(array,mem_type)
    !==============================================================================!
    !              M E M O R Y _ D E A L L O C A T E _ 1 D _ R E A L               !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 1D real array       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type

    call trace_entry('memory_deallocate_1d_real')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_1d_real')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select

    tot_memory=tot_memory-sizeof(array)

    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_1d_real: More memory required")


    call memory_trace()
    call trace_exit('memory_deallocate_1d_real')
  end subroutine memory_deallocate_1d_real

  subroutine memory_deallocate_2d_real(array,mem_type)
    !==============================================================================!
    !              M E M O R Y _ D E A L L O C A T E _ 2 D _ R E A L               !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 2D real array       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_2d_real')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_2d_real')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)

    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_2d_real: More memory required")


    call memory_trace()
    call trace_exit('memory_deallocate_2d_real')
  end subroutine memory_deallocate_2d_real

  subroutine memory_deallocate_3d_real(array,mem_type)
    !==============================================================================!
    !              M E M O R Y _ D E A L L O C A T E _ 3 D _ R E A L               !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 3D real array       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_3d_real')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_3d_real')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case('G')
       gen_memory=gen_memory-sizeof(array)
    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)


    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_3d_real: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_3d_real')
  end subroutine memory_deallocate_3d_real

  subroutine memory_deallocate_4d_real(array,mem_type)
    !==============================================================================!
    !              M E M O R Y _ D E A L L O C A T E _ 4 D _ R E A L               !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 4D real array       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:,:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_4d_real')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_4d_real')
       return
    end if

    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)

    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_4d_real: More memory required")


    call memory_trace()
    call trace_exit('memory_deallocate_4d_real')
  end subroutine memory_deallocate_4d_real

  subroutine memory_deallocate_5d_real(array,mem_type)
    !==============================================================================!
    !              M E M O R Y _ D E A L L O C A T E _ 5 D _ R E A L               !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 5D real array       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    real(dp),dimension(:,:,:,:,:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_5d_real')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_5d_real')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_5d_real: More memory required")



    call memory_trace()
    call trace_exit('memory_deallocate_5d_real')
  end subroutine memory_deallocate_5d_real






  subroutine memory_deallocate_1d_complex(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 1 D _ C O M P L E X            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 1D complex array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_1d_complex')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_1d_complex')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_1d_complex: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_1d_complex')
  end subroutine memory_deallocate_1d_complex

  subroutine memory_deallocate_2d_complex(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 2 D _ C O M P L E X            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 2D complex array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:), allocatable,intent(inout)  :: array
    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_2d_complex')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_2d_complex')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_2d_complex: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_2d')
  end subroutine memory_deallocate_2d_complex

  subroutine memory_deallocate_3d_complex(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 3 D _ C O M P L E X            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 3D complex array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_3d_complex')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_3d_complex')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_3d_complex: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_3d_complex')
  end subroutine memory_deallocate_3d_complex

  subroutine memory_deallocate_4d_complex(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 4 D _ C O M P L E X            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 4D complex array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_4d_complex')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_4d_complex')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_4d_complex: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_4d_complex')
  end subroutine memory_deallocate_4d_complex

  subroutine memory_deallocate_5d_complex(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 5 D _ C O M P L E X            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 5D complex array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    complex(dp),dimension(:,:,:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_5d_complex')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_5d_complex')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_5d_complex: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_5d_complex')
  end subroutine memory_deallocate_5d_complex










  subroutine memory_deallocate_1d_integer(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 1 D _ I N T E G E R            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 1D integer array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_1d_integer')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_1d_integer')
       return
    end if




    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select

    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_1d_integer: More memory required")
    tot_memory=tot_memory-sizeof(array)
    call memory_trace()
    call trace_exit('memory_deallocate_1d_integer')
  end subroutine memory_deallocate_1d_integer

  subroutine memory_deallocate_2d_integer(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 2 D _ I N T E G E R            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 2D integer array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_2d_integer')

    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_2d_integer')
       return
    end if

    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_2d_integer: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_2d')
  end subroutine memory_deallocate_2d_integer

  subroutine memory_deallocate_3d_integer(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 3 D _ I N T E G E R            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 3D integer array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_3d_integer')
    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_3d_integer')
       return
    end if

    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_3d_integer: More memory required")


    call memory_trace()
    call trace_exit('memory_deallocate_3d_integer')
  end subroutine memory_deallocate_3d_integer

  subroutine memory_deallocate_4d_integer(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 4 D _ I N T E G E R            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 4D integer array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_4d_integer')

    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_4d_integer')
       return
    end if

    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_4d_integer: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_4d_integer')
  end subroutine memory_deallocate_4d_integer

  subroutine memory_deallocate_5d_integer(array,mem_type)
    !==============================================================================!
    !           M E M O R Y _ D E A L L O C A T E _ 5 D _ I N T E G E R            !
    !==============================================================================!
    ! Subroutine for calculating the memory and deallocating a 5D integer array    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           mem_type,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    integer,dimension(:,:,:,:,:), allocatable,intent(inout)  :: array

    integer  :: stat
    character(1) :: mem_type
    call trace_entry('memory_deallocate_5d_integer')

    if (.not.allocated(array))then
       call memory_trace()
       call trace_exit('memory_deallocate_5d_integer')
       return
    end if


    select case(mem_type)
    case('I')
       io_memory=io_memory-sizeof(array)
    case('P')
       pot_memory=pot_memory-sizeof(array)       
    case('W')
       wave_memory=wave_memory-sizeof(array)
    case('B')
       basis_memory=basis_memory-sizeof(array)
    case('G')
       gen_memory=gen_memory-sizeof(array)
    case('D')
       den_memory=den_memory+sizeof(array)

    case default
       call  memory_errors("Error in memory_deallocate: Unknown array type")
    end select
    tot_memory=tot_memory-sizeof(array)
    deallocate(array,stat=stat)
    if (stat.ne.0) call memory_errors("Error in memory_deallocate_5d_integer: More memory required")

    call memory_trace()
    call trace_exit('memory_deallocate_5d_integer')
  end subroutine memory_deallocate_5d_integer













  subroutine memory_errors(message)
    !==============================================================================!
    !                              I O _ E R R O R S                               !                                                                                                                   
    !==============================================================================!                                                                                                                   
    ! Subroutine handling all errors writing to the errors file                    !                                                                                                                   
    !------------------------------------------------------------------------------!                                                                                                                   
    ! Arguments:                                                                   !                                                                                                                   
    !           message,           intent :: in                                    !                                                                                                                   
    !------------------------------------------------------------------------------!                                                                                                                  
    ! Author:   Z. Hawkhead  19/01/2020                                            !                                                                                                                   
    !==============================================================================!                                                                                                                   
    use trace, only : trace_entry,trace_exit,trace_stack
    implicit none
    character(*)       :: message

    ! internal variable for rank processing                                                                                                                                                            
    character(len=40)  :: file_name

    write(file_name,'(A,".",I0.4,".err")') trim(local_seed),rank

    open(2,file=trim(file_name),RECL=8192,status="UNKNOWN")
    write(*,*)"Error: called memory_abort"
    write(2,*) message

    call trace_stack(2,rank,seed=local_seed)
    stop
    return
  end subroutine memory_errors




  subroutine memory_trace()
    !==============================================================================!
    !                           M E M O R Y _ T R A C E                            !
    !==============================================================================!
    ! Subroutine for writing out the .mem files if requested by the user           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!

    integer :: mem_unit

    character(30) :: mem_name
    real(dp) :: cur_time
    integer :: stat
    logical :: opened
    mem_unit=345*rank+9103

    if (local_write_mem)then
       write(mem_name,'(a,".",i0.4,a)')trim(local_seed),rank,".mem"
       inquire(mem_unit,opened=opened)
       if (.not.opened)then

          open(unit=mem_unit,file=mem_name,access="STREAM",form="FORMATTED")
          write(mem_unit,*) "#  MEMORY TIME REPORT "
          write(mem_unit,*) "#  Time (s)    IO (B) BASIS (B)   Wave (B)  Pot (B)   Den (B)   Gen (B)  Total (B) "

       end if
       call cpu_time(cur_time)

       write(mem_unit,*) cur_time , io_memory,basis_memory,wave_memory,pot_memory,den_memory,gen_memory,tot_memory

    end if


  end subroutine memory_trace

end module memory
