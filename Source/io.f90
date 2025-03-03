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
module io
  !Impose strong typing
  use units
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise,warning_counter &
       &, trace_wallclock
  use comms,only : rank, nprocs,comms_arch,on_root_node,max_version_length,COMMS_FINALISE&
       &,comms_library_version,comms_version,dist_kpt,dist_gvec,comms_stop
  use memory

  use iso_fortran_env, only : compiler_version
  use iso_c_binding

  implicit none

  logical,           public                :: file_exists
  character(20),     public                :: seed=''
  integer,           public                :: stdout
  integer,           public                :: info_unit
  integer,           public,parameter      :: glob_line_len =67
  integer,parameter                        :: n_cats = 10
  character(3)                             :: grp ! group for printing the parameters

  character(100),dimension(:),allocatable  :: present_array
  character(100),dimension(:),allocatable  :: keys_array
  character(100),dimension(:),allocatable  :: keys_description
  character(1000),dimension(:),allocatable  :: keys_example ! An optional example, for complicated keys
  character(100),dimension(:),allocatable  :: keys_default
  character(100),dimension(:),allocatable  :: keys_allowed
  character(100),dimension(:),allocatable  :: keys_type
  integer       ,dimension(:),allocatable  :: keys_cat
  character(30) ,dimension(1:n_cats)            :: cats
  character(100) :: version = "1.0.0"   ! Master version for all instances
  character(100) :: info = "Durham Electronic RElaxaction (K)code, DEReK (c) 2024 - Z. Hawkhead"
  logical        :: read_params
  character(30),dimension(:),allocatable   :: authors_title
  character(30),dimension(:),allocatable   :: authors_forename
  character(30),dimension(:),allocatable   :: authors_surname
  character(30),dimension(:),allocatable   :: authors_email
  integer                                  :: max_params=1
  logical, private :: cell_declared=.false.
  logical, public :: version_only  = .false.






  type  parameters
     ! %Begin: parameters

     !Calculation parameters
     logical :: check = .false.
     logical :: debugging = .false.
     integer :: n_electrons =            1
     real(dp) :: energy_tol =    1.0E-7_dp!*ev_to_hartree
     character(len=30) :: xc_functional = 'LDA'
     real(dp) :: cut_off_energy =    200.0_dp!*ev_to_hartree
     real(dp) :: g_fine_scale =    2.0_dp
     logical :: write_wvfn = .false.
     character(len=30) :: task = 'singlepoint'
     logical :: soc = .false.
     integer :: max_scf =           40
     character(len=30) :: scf_method = 'EDFT'
     real(dp) :: electronic_temp =    0.000_dp
     integer :: conduction_bands =           20
     integer :: nbands
     logical :: write_density = .false.
     logical :: Write_potential = .false.
     logical :: write_state = .true.
     character(len=30) :: external_pot = 'jelly'
     integer,dimension(1:3) :: kpt_mp_grid = (/1,1,1/)
     logical :: write_spec = .true.
     logical :: write_memory = .false.
     integer :: iprint =            1
     real(dp) :: finite_barrier_height =    10.00_dp
     real(dp),dimension(1:3) :: finite_barrier_width =   (/0.5_dp,0.5_dp,0.5_dp/)
     integer,dimension(1:3)  :: periodic_pot_grid = (/1,1,1/)
     real(dp) :: periodic_pot_amp =    10.00_dp
     integer :: random_seed =            0
     logical :: write_formatted_potential = .false.
     logical :: write_potex = .false.
     logical :: write_formatted_density = .false.
     logical :: restart = .false.
     logical :: write_formatted_potex = .false.
     character(len=30) :: output_level = 'minimal'
     real(dp),dimension(1:3) :: ext_efield =    (/0.0_dp,0.0_dp,0.0_dp/)
     real(dp),dimension(1:3) :: ext_bfield =    (/0.0_dp,0.0_dp,0.0_dp/)
     character(len=30) :: unit_energy = 'eV'
     character(len=30) :: unit_length = 'A'
     character(len=30) :: unit_efield = 'eV/A/e'
     character(len=30) :: unit_bfield = 'T'
     character(len=30) :: out_energy_unit = 'eV'
     character(len=30) :: out_len_unit = 'A'
     character(len=30) :: out_efield_unit = 'eV/A/e'
     character(len=30) :: out_bfield_unit = 'G'
     integer,dimension(1:3) :: dos_kpt_mp_grid =  (/11,11,11/)
     logical :: write_xsf = .false.
     ! %End: parameters
  end type parameters

  ! %Begin: keys

  character(len=30),parameter,public ::key_check   = 'check'
  character(len=30),parameter,public ::key_debugging   = 'profilling'
  character(len=30),parameter,public ::key_n_electrons   = 'n_electrons'
  character(len=30),parameter,public ::key_energy_tol   = 'energy_tol'
  character(len=30),parameter,public ::key_xc_functional   = 'xc_functional'
  character(len=30),parameter,public ::key_cut_off_energy   = 'cut_off_energy'
  character(len=30),parameter,public ::key_g_fine_scale   = 'g_fine_scale'
  character(len=30),parameter,public ::key_write_wvfn   = 'write_wvfn'
  character(len=30),parameter,public ::key_task   = 'task'
  character(len=30),parameter,public ::key_soc   = 'soc'
  character(len=30),parameter,public ::key_max_scf   = 'max_scf'
  character(len=30),parameter,public ::key_scf_method   = 'scf_method'
  character(len=30),parameter,public ::key_electronic_temp   = 'electronic_temp'
  character(len=30),parameter,public ::key_conduction_bands   = 'conduction_bands'
  character(len=30),parameter,public ::key_write_density   = 'write_density'
  character(len=30),parameter,public ::key_Write_potential   = 'write_potential'
  character(len=30),parameter,public ::key_write_state   = 'write_state'
  character(len=30),parameter,public ::key_external_pot   = 'external_pot'
  character(len=30),parameter,public ::key_kpt_mp_grid   = 'kpt_mp_grid'
  character(len=30),parameter,public ::key_write_spec   = 'write_spec'
  character(len=30),parameter,public ::key_write_memory   = 'write_memory'
  character(len=30),parameter,public ::key_iprint   = 'print_level'
  character(len=30),parameter,public ::key_finite_barrier_height   = 'finite_barrier_height'
  character(len=30),parameter,public ::key_finite_barrier_width   = 'finite_barrier_width'
  character(len=30),parameter,public ::key_periodic_pot_grid   = 'periodic_pot_grid'
  character(len=30),parameter,public ::key_periodic_pot_amp   = 'periodic_pot_amp'
  character(len=30),parameter,public ::key_random_seed   = 'random_seed'
  character(len=30),parameter,public ::key_write_formatted_potential   = 'write_formatted_potential'
  character(len=30),parameter,public ::key_write_potex   = 'write_potex'
  character(len=30),parameter,public ::key_write_formatted_density   = 'write_formatted_density'
  character(len=30),parameter,public ::key_write_formatted_potex   = 'write_formatted_potex'
  character(len=30),parameter,public ::key_output_level   = 'output_level'
  character(len=30),parameter,public ::key_ext_efield   = 'ext_efield'
  character(len=30),parameter,public ::key_ext_bfield   = 'ext_bfield'
  character(len=30),parameter,public ::key_unit_energy   = 'unit_energy'
  character(len=30),parameter,public ::key_unit_length   = 'unit_length'
  character(len=30),parameter,public ::key_unit_efield   = 'unit_efield'
  character(len=30),parameter,public ::key_unit_bfield   = 'unit_bfield'
  character(len=30),parameter,public ::key_out_energy_unit   = 'unit_energy_out'
  character(len=30),parameter,public ::key_out_len_unit   = 'unit_length_out'
  character(len=30),parameter,public ::key_out_efield_unit   = 'unit_efield_out'
  character(len=30),parameter,public ::key_out_bfield_unit   = 'unit_bfield_out'
  character(len=30),parameter,public ::key_dos_kpt_mp_grid   = 'dos_kpt_mp_grid'
  character(len=30),parameter,public ::key_write_xsf   = 'write_xsf'
  ! %End: keys




  integer,parameter::max_keys=          45
  ! %End: max_param


  type structure
     real(dp),dimension(1:3,1:3)         :: cell ! cell matrix
     real(dp),dimension(1:3,1:3)         :: inv_cell ! inverse cell matrix
     real(dp),dimension(:,:),allocatable :: kpt_scf_list ! SCF list of kpts, master
     integer ,dimension(:),  allocatable :: kpts_on_node ! SCF list of kpts, locally on node
     integer                             :: num_kpoints
     integer                             :: max_kpoints_on_node
     integer                             :: num_kpoints_on_node
     real(dp)                            :: lattice_a
     real(dp)                            :: lattice_b
     real(dp)                            :: lattice_c
     real(dp)                            :: alpha
     real(dp)                            :: beta
     real(dp)                            :: gamma
     real(dp)                            :: volume
     ! 'Atoms' - i.e. local potentials
     logical                             :: local_pots
     integer                             :: n_pots
     real(dp),dimension(:,:),allocatable :: pot_centres
     real(dp),dimension(:)  ,allocatable :: pot_heights
     real(dp),dimension(:)  ,allocatable :: pot_widths

     
  end type structure



  type(parameters),public,save :: current_params
  type(structure) ,public,save :: current_structure



  !external DGETRF


  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------!
  public :: io_initialise
  public :: io_finalise
  public :: io_errors

contains

  subroutine io_open_std()
    !==============================================================================!
    !                            I O _ O P E N _ S T D                             !
    !==============================================================================!
    ! I/O routine for opening up the main DEReK files. Can be called earlier       !
    ! than previous routines meaning more modules have access to the I/O           !
    ! routines.                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  20/06/2024                                            !
    !==============================================================================!

    call trace_entry('io_open_std')

    call io_cl_parser() ! Read the commandline arguments

    !
    ! We only do all of this if the user hasnt asked for version info
    ! set up the error files, in case they need to be used elsewhere
    if (trim(seed).eq.'')then
       seed = 'derek'
    end if

    write(error_file,'(A,".",I0.4,".err")') trim(seed),rank
    if (.not.version_only)then
       ! Open up the main file for the output
       !open(stdout,file=trim(seed)//".derek",RECL=8192,form="FORMATTED",access="APPEND")
       call io_open_fmt(stdout,trim(seed)//".derek",'APPEND')
    end if

    call trace_exit('io_open_std')
  end subroutine io_open_std

  subroutine io_initialise(safety)
    !==============================================================================!
    !                          I O _ I N I T I A L I S E                           !
    !==============================================================================!
    ! Subroutine for initialising all input/output for the parallel code N_BODY    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    ! Should be called in top level file
    implicit none
    character(*),intent(in) :: safety
    integer :: i ! counters
    character(10) :: line
    integer :: stat

    ! Some junk variables


    call trace_entry("io_initialise")

    ! Get the length of the parameters file
    inquire(file=trim(seed)//'.info',exist=file_exists)

    if (file_exists)then
       !openg(unit=1,file=trim(seed)//'.info',iostat=stat,status="OLD",access="stream",form="formatted")
       call io_open_fmt(info_unit,trim(seed)//'.info',action='read',status='old',access='stream')
       do while (stat.eq.0)
          read(info_unit,'(A60)',iostat=stat) line
          max_params=max_params+1
       end do
       close(1)
    else
       call io_errors(" file '"//trim(seed)//".info' does not exist.")
    end if

    max_params=max_params+1
    ! Allocate space for the params array
    allocate(present_array(1:max_params))
    do i=1,max_params
       write(present_array(i),*)i
    end do
    ! Fist things first, try to read paramteters
    call io_list_params(.false.)


    if (read_params) call io_read_param(current_params,safety)
    if (.not.cell_declared) call io_errors(" No lattice provided")
    call memory_init(seed,current_params%write_memory)
    call io_kpoint_grid()


    ! We have the lattice we can define params
    call io_lattice_to_abc()

    call io_convert_all()

!!$    ! Check the units and convert, we do this here because it handles all of the defaults too.
!!$    current_params%cut_off_energy=current_params%cut_off_energy*ev_to_hartree
!!$    current_params%energy_tol = current_params%energy_tol * ev_to_hartree
!!$    current_params%finite_barrier_height=current_params%finite_barrier_height * ev_to_hartree
!!$    current_params%periodic_pot_amp=current_params%periodic_pot_amp * ev_to_hartree

    ! Work out the new values of some on the fly parameters
    current_params%nbands=current_params%n_electrons+current_params%conduction_bands



    call trace_exit("io_initialise")
    return
  end subroutine io_initialise

  subroutine io_convert_all()
    !==============================================================================!
    !                         I O _ C O N V E R T _ A L L                          !
    !==============================================================================!
    ! Helper routine for performing unit conversion on all of the parameters       !
    ! that need to be in atomic units.                                             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    call trace_entry('io_convert_all')

    current_params%energy_tol = units_to_atomic(current_params%energy_tol,current_params%unit_energy)
    current_params%cut_off_energy = units_to_atomic(current_params%cut_off_energy,current_params%unit_energy)
    current_params%electronic_temp =    units_to_atomic(current_params%electronic_temp,'K')
    current_params%finite_barrier_height =    units_to_atomic(current_params%finite_barrier_height,current_params%unit_energy)
    current_params%finite_barrier_width(1) =  units_to_atomic(current_params%finite_barrier_width(1),current_params%unit_length)
    current_params%finite_barrier_width(2) =  units_to_atomic(current_params%finite_barrier_width(2),current_params%unit_length)
    current_params%finite_barrier_width(3) =  units_to_atomic(current_params%finite_barrier_width(3),current_params%unit_length)

    current_params%periodic_pot_amp =units_to_atomic(current_params%periodic_pot_amp,current_params%unit_energy)
    current_params%ext_efield(1) =units_to_atomic(current_params%ext_efield(1),current_params%unit_efield)
    current_params%ext_efield(2) =units_to_atomic(current_params%ext_efield(2),current_params%unit_efield)
    current_params%ext_efield(3) =units_to_atomic(current_params%ext_efield(3),current_params%unit_efield)

    current_params%ext_bfield(1) =units_to_atomic(current_params%ext_bfield(1),current_params%unit_bfield)
    current_params%ext_bfield(2) =units_to_atomic(current_params%ext_bfield(2),current_params%unit_bfield)
    current_params%ext_bfield(3) =units_to_atomic(current_params%ext_bfield(3),current_params%unit_bfield)



    call trace_exit('io_convert_all')
  end subroutine io_convert_all

  subroutine io_read_param(dummy_params,safety)
    !==============================================================================!
    !                          I O _ R E A D _ P A R A M                           !
    !==============================================================================!
    ! Subroutine for reading parameters from the file "param.n_body" to the        !
    ! parameter type current_params                                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           dummy_params,      intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  18/11/2020                                            !
    !==============================================================================!
    implicit none
    !The inout stuff
    type(parameters),intent(inout)  :: dummy_params
    character(*), intent(in) :: safety

    !The boring stuff to make the whole shebang work
    integer           :: stat
    integer           :: read_stat=0
    integer           :: i,j,k           !counter

    character(len=60) :: line        ! charcter string into which each line is read, overwritten in loop
    character(len=30) :: key         ! the keyword used
    character(len=30) :: param       ! the value of the param
    character(len=30) :: match       ! spell check match
    character(len=30) :: buff        ! buffer
    logical           :: comment     ! Boolean for comment line, will skip
    logical           :: spelling = .false.
    logical           :: file_exists
    real(dp)          :: real_dump   ! a dump for handling scientific


    integer :: lev_dist
    integer :: width=69
    integer :: max_lev=3
    call trace_entry("io_read_param")

    ! Do parameter safety
    select case(safety)
    case('Safe')
       max_lev = 0
    case('Normal')
       max_lev = 3
    case('Risky')
       max_lev = 5
    case default
       call io_errors('Unknown maximum spell check safety')
    end select

    !Open the parameter file
    if (file_exists) then
       !open(unit=1,file=trim(seed)//".info",iostat=stat,status="OLD",access="stream",form="formatted")
       !call io_open_fmt(info_unit,trim(seed)//'.info')
       call io_open_fmt(info_unit,trim(seed)//'.info',action='read',status='old',access='stream')
       !if (stat.ne.0) call io_errors(" Open file '"//trim(seed)//".info'")
       ! now we can do the reading
       k=0
       do i=1,max_params
          !first thing, read new line into 'line' variable
          read(info_unit,'(A)',iostat=read_stat) line
          

          !print*,trim(present_array(i))
          ! Check for blank line
          if (read_stat.ne.0)exit
          if (trim(line).eq."") cycle
          if (index(trim(line),'!').gt.0)cycle

          ! read the block data
          if (index(io_case(line),"$data").gt.0)then
             ! Hold initial external_pot value for comparison
             buff = dummy_params%external_pot
             call io_block_parse(line,dummy_params,stat)
             if (trim(dummy_params%external_pot).ne.trim(buff)) present_array(i) = 'external_pot'
             ! Check to see if the 
             cycle
          end if

          !Read everying into a thing
          call io_freeform_read(line,key,param,comment)

          if (comment) cycle ! skip if comment
          !Do some trimming
          key=adjustl(trim(io_case(key)))

          ! We need to preserve the capitalisation for the units, so make an exception

          if (index(key,'unit').eq.0)then
             param=adjustl(trim(io_case(param)))
          end if

          ! Check for spelling

          call io_spell_check(key,lev_dist,match)


          if (lev_dist.gt.0 .and. lev_dist.lt.max_lev)then
             call io_warnings()
             if (.not.spelling)then
                write(stdout,*)"********************************************************************"
                write(stdout,*)"*                         *** WARNING ***                          *"
                write(stdout,*)"*   POSSIBLE TYPOS DETECTED IN INFO FILE, ATTEMPTING AUTOCORRECT   *"
                write(stdout,*)"********************************************************************"
                write(stdout,*)"*  UNKNOWN PARAMETER        AUTOCORRECT       LEVENSHTEIN DISTANCE *"
                write(stdout,*)"********************************************************************"
                spelling=.true.
             end if
             write(stdout,9)trim(adjustl(key)),trim(adjustl(match)),lev_dist


             key = match
          elseif (lev_dist.ge.max_lev)then
             write(stdout,*)"********************************************************************"
             write(stdout,*)"*           **** UNABLE TO SAFELY MATCH PARAMETER ****             *"
             write(stdout,*)"*           ---------------- ABORTING ----------------             *"
             write(stdout,*)"********************************************************************"
             call io_errors("Unknown parameter - "//key)

          end if
9         format(' *',T5,A,T30,A,T60,i3,T69,'*')

          ! %Begin: case_read
          select case(key)

          case(key_check)
             read(param,*,iostat=stat) dummy_params%check
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_debugging)
             read(param,*,iostat=stat) dummy_params%debugging
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_n_electrons)
             read(param,*,iostat=stat) dummy_params%n_electrons
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_energy_tol)
             read(param,*,iostat=stat) dummy_params%energy_tol
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             dummy_params%energy_tol=dummy_params%energy_tol
             present_array(i)=key
          case(key_xc_functional)
             read(param,*,iostat=stat) dummy_params%xc_functional
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_cut_off_energy)
             read(param,*,iostat=stat) dummy_params%cut_off_energy
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
             dummy_params%cut_off_energy=dummy_params%cut_off_energy
          case(key_g_fine_scale)
             read(param,*,iostat=stat) dummy_params%g_fine_scale
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_wvfn)
             read(param,*,iostat=stat) dummy_params%write_wvfn
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_task)
             read(param,*,iostat=stat) dummy_params%task
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             select case(dummy_params%task)
             case ('singlepoint','bandstructure','dos')
                continue
             case default
                call io_errors("Unknown task: "//param)
             end select
             present_array(i)=key
          case(key_soc)
             read(param,*,iostat=stat) dummy_params%soc
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_max_scf)
             read(param,*,iostat=stat) dummy_params%max_scf
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_scf_method)
             read(param,*,iostat=stat) dummy_params%scf_method
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_electronic_temp)
             read(param,*,iostat=stat) dummy_params%electronic_temp
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_conduction_bands)
             read(param,*,iostat=stat) dummy_params%conduction_bands
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_density)
             read(param,*,iostat=stat) dummy_params%write_density
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_Write_potential)
             read(param,*,iostat=stat) dummy_params%Write_potential
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_state)
             read(param,*,iostat=stat) dummy_params%write_state
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_external_pot)
             read(param,*,iostat=stat) dummy_params%external_pot
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%external_pot)
             case('jelly','finite_barrier','periodic_pot')
             case('gaussian_pot','lorentzian_pot')
                ! These should not be set at this point, they need to be set as a block
                call io_errors('External pot type '//trim(dummy_params%external_pot)//' should only be set in a data block.')
             case default
                ! Check if file exists, else throw error
                inquire(file=dummy_params%external_pot,exist=file_exists)

                if (file_exists)then
                   if (index(dummy_params%external_pot,'potex').eq.0)then
                      call io_errors('Unsupported external potential file: '//trim(dummy_params%external_pot))
                   end if
                else
                   call io_errors('Potential file does not exist: '//trim(dummy_params%external_pot))
                end if
             end select
          case(key_kpt_mp_grid)
             read(param,*,iostat=stat) dummy_params%kpt_mp_grid(1),dummy_params%kpt_mp_grid(2),dummy_params%kpt_mp_grid(3)
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_spec)
             read(param,*,iostat=stat) dummy_params%write_spec
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_memory)
             read(param,*,iostat=stat) dummy_params%write_memory
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_iprint)
             read(param,*,iostat=stat) dummy_params%iprint
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
             if (dummy_params%iprint.lt.1 .or. dummy_params%iprint.gt.3)call io_errors('print_level  must be between 1 and 3')
          case(key_finite_barrier_height)
             read(param,*,iostat=stat) dummy_params%finite_barrier_height
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_finite_barrier_width)
             read(param,*,iostat=stat) dummy_params%finite_barrier_width(1),&
                  & dummy_params%finite_barrier_width(2),&
                  & dummy_params%finite_barrier_width(3)
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_periodic_pot_grid)
             read(param,*,iostat=stat) dummy_params%periodic_pot_grid
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_periodic_pot_amp)
             read(param,*,iostat=stat) dummy_params%periodic_pot_amp
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_random_seed)
             read(param,*,iostat=stat) dummy_params%random_seed
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_formatted_potential)
             read(param,*,iostat=stat) dummy_params%write_formatted_potential
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_potex)
             read(param,*,iostat=stat) dummy_params%write_potex
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_formatted_density)
             read(param,*,iostat=stat) dummy_params%write_formatted_density
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_write_formatted_potex)
             read(param,*,iostat=stat) dummy_params%write_formatted_potex
             if (stat.ne.0) call io_errors(" Error parsing value: "//param)
             present_array(i)=key
          case(key_output_level)
             read(param,*,iostat=stat) dummy_params%output_level
             if (stat.ne.0) call io_errors("Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%output_level)
             case ('none','minimal','all')
                continue
             case default
                call io_errors("Invalid output: "//param//" for key: "//key)
             end select
          case(key_ext_efield)
             read(param,*,iostat=stat) dummy_params%ext_efield(1),dummy_params%ext_efield(2),dummy_params%ext_efield(3)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_ext_bfield)
             read(param,*,iostat=stat) dummy_params%ext_bfield(1),dummy_params%ext_bfield(2),dummy_params%ext_bfield(3)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_unit_energy)
             read(param,*,iostat=stat) dummy_params%unit_energy
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! Error checking for energy unit
             select case(dummy_params%unit_energy)
             case('Ha','mHa','eV','meV','Ry','mRy','J')
                continue
             case default
                call io_errors('Invalid energy unit')
             end select
          case(key_unit_length)
             read(param,*,iostat=stat) dummy_params%unit_length
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! Error checking for length unit
             select case(dummy_params%unit_length)
             case('m','mm','cm','nm','mum','pm','A','bohr')
                continue
             case default
                call io_errors('Invalid length unit')
             end select
          case(key_unit_efield)
             read(param,*,iostat=stat) dummy_params%unit_efield
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! Error checking for unit
             select case(dummy_params%unit_efield)
             case('Ha/Bohr/e','eV/A/e','N/C')
                continue
             case default
                call io_errors('Invalid electric field unit')
             end select
          case(key_unit_bfield)
             read(param,*,iostat=stat) dummy_params%unit_bfield
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! Error checking for unit
             select case(dummy_params%unit_bfield)
             case('T','G','agr')
                continue
             case default
                call io_errors('Invalid magnetic field unit')
             end select
          case(key_out_energy_unit)
             read(param,*,iostat=stat) dummy_params%out_energy_unit
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%out_energy_unit)
             case('Ha','mHa','eV','meV','Ry','mRy','J')
                continue
             case default
                call io_errors('Invalid output energy unit')
             end select
          case(key_out_len_unit)
             read(param,*,iostat=stat) dummy_params%out_len_unit
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%out_len_unit)
             case('m','mm','cm','nm','mum','pm','A','bohr')
                continue
             case default
                call io_errors('Invalid output length unit')
             end select
          case(key_out_efield_unit)
             read(param,*,iostat=stat) dummy_params%out_efield_unit
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%out_efield_unit)
             case('Ha/Bohr/e','eV/A/e','N/C')
                continue
             case default
                call io_errors('Invalid output electric field unit')
             end select
          case(key_out_bfield_unit)
             read(param,*,iostat=stat) dummy_params%out_bfield_unit
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             select case(dummy_params%out_bfield_unit)
             case('T','G','agr')
                continue
             case default
                call io_errors('Invalid out magnetic field unit')
             end select
          case(key_dos_kpt_mp_grid)
             read(param,*,iostat=stat) dummy_params%dos_kpt_mp_grid
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_xsf)
             read(param,*,iostat=stat) dummy_params%write_xsf
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! %End: case_read
          case default
             call io_errors(" Error parsing keyword: "//key)
          end select



       end do
       if (spelling)then
          write(stdout,*)"********************************************************************"
          write(stdout,*)
       end if


    else
       call trace_exit("io_read_param")
       return
    end if
    ! Check for duplicates

    do i=1,max_params
       !print*,trim(present_array(i))
       do j=1,max_params
          if (i.eq.j)cycle
          !print*,present_array
          if (io_case(present_array(i)).eq.io_case(present_array(j)))then
             call io_errors(" Duplicate parameter found: "//present_array(i))
          end if
       end do
    end do
    close(1)
    !call io_errors("Test")
    ! Handle the kpoint list


    ! Set the file writing
    do i = 1,max_params
       if (trim(adjustl(present_array(i))).eq.'output_level')then
          select case(dummy_params%output_level)
          case('minimal')
             dummy_params%write_wvfn               = .false.
             dummy_params%write_spec               = .false.
             dummy_params%write_state              = .true.
             dummy_params%write_potex              = .true.
             dummy_params%write_formatted_potex    = .false.
             dummy_params%write_density            = .true.
             dummy_params%write_formatted_density  = .false.
             dummy_params%write_potential          = .false.
             dummy_params%write_formatted_potential= .false.
          case('none')
             dummy_params%write_wvfn               = .false.
             dummy_params%write_spec               = .false.
             dummy_params%write_state              = .false.
             dummy_params%write_potex              = .false.
             dummy_params%write_formatted_potex    = .false.
             dummy_params%write_density            = .false.
             dummy_params%write_formatted_density  = .false.
             dummy_params%write_potential          = .false.
             dummy_params%write_formatted_potential= .false.
          case('all')
             dummy_params%write_wvfn               = .true.
             dummy_params%write_spec               = .true.
             dummy_params%write_state              = .true.
             dummy_params%write_potex              = .true.
             dummy_params%write_formatted_potex    = .true.
             dummy_params%write_density            = .true.
             dummy_params%write_formatted_density  = .true.
             dummy_params%write_potential          = .true.
             dummy_params%write_formatted_potential= .true.
          end select
          exit
       end if
    end do

    ! If a spectral calculation, must write the spec file
    if (dummy_params%task.eq.'bandstructure'.or.&
         & dummy_params%task.eq.'dos')then
       dummy_params%write_spec               = .true.
    end if



    call trace_exit("io_read_param")
    return
  end subroutine io_read_param




  subroutine io_freeform_read(line_unparsed,key,val,com)
    !==============================================================================!
    !                       I O _ F R E E F O R M _ R E A D                        !
    !==============================================================================!
    ! Subroutine for parsing keys and params from general line with delimiter      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           line_unparsed,     intent :: in                                    !
    !           key,               intent :: out                                   !
    !           val,               intent :: out                                   !
    !           com,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    !subroutine that parses the lines from param.pop into the key and the the value
    implicit none
    character(*),intent(in)           :: line_unparsed
    character(*),intent(out)          :: key
    character(*),intent(out)          :: val
    logical,     intent(inout)        :: com

    integer                           :: j

    call trace_entry("io_freeform_read")
    if (line_unparsed(1:1).eq."!" .or. line_unparsed(1:1).eq."#") then
       com=.true.
       return
    else
       com=.false.
    end if

    do j=1,len_trim(line_unparsed)

       if (line_unparsed(j:j).eq.':' .or. line_unparsed(j:j).eq."=")then
          key=line_unparsed(1:j-1)
          val=line_unparsed(j+1:len_trim(line_unparsed))
          exit

       else if (j.eq.len_trim(line_unparsed))then
          call io_errors(" Error parsing line:  "//trim(line_unparsed))
       end if
    end do

    !print*,line_unparsed,key,val,com
    call trace_exit("io_freeform_read")
    return
  end subroutine io_freeform_read


  subroutine io_errors(message,major_error)
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
    use trace, only : trace_current

    implicit none
    character(*)       :: message
    logical,optional   :: major_error
    character(100)     :: current_sub

    call io_open_fmt(err_unit,trim(error_file),status='unknown',action='write')
    call trace_current(current_sub)



    !open(newunit=err_unit,file=trim(error_file),RECL=8192,status="UNKNOWN")
    write(*,*)"Error: called io_errors"
    write(err_unit,*) "Error in ",trim(current_sub),": ",message
    write(*,*) "Error in ",trim(current_sub),": ",message

    if (present(major_error))then
       if (major_error)then
          ! In the event of a major error, we repeat the message on the terminal
          write(*,*) 'D E R e K   M A J O R   E R R O R    D E T E C T E D '
          write(*,*) 'D E R e K   M A J O R   E R R O R    D E T E C T E D '
          write(*,*)
          write(*,*) "   ",io_case(message,.true.)
          write(*,*)
          write(*,*) 'D E R e K   M A J O R   E R R O R    D E T E C T E D '
          write(*,*) 'D E R e K   M A J O R   E R R O R    D E T E C T E D '

       end if
    end if
    
    call trace_stack(err_unit,rank,seed=seed)
    call comms_stop()
    return
  end subroutine io_errors

  function io_case( string , upper) result (new)
    !==============================================================================!
    !                                I O _ C A S E                                 !
    !==============================================================================!
    ! Low level subroutine for modifying the case of user given arguments to       !
    ! lowercase                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           strin                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  19/01/2020                                            !
    !==============================================================================!
    character(len=*)           :: string

    character(len=len(string)) :: new

    integer                    :: i
    integer                    :: k
    integer                    :: length
    logical, optional          :: upper
    !call trace_entry('io_case')
    length = len(string)
    new    = string
    do i = 1,len(string)
       k = iachar(string(i:i))
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then
          k = k + iachar('a') - iachar('A')
          new(i:i) = achar(k)
       end if
    end do

    if (present(upper))then
       if (upper) then
          do i = 1, len(string)
             k = iachar(string(i:i))
             if (k >= iachar('a') .and. k <= iachar('z')) then
                k = k + iachar('A') - iachar('a')  ! Convert lowercase to uppercase
                new(i:i) = achar(k)
             elseif (k >= iachar('A') .and. k <= iachar('Z')) then
                new(i:i) = achar(k)  ! Preserve uppercase letters
             else
                new(i:i) = string(i:i)  ! Preserve non-alphabetic characters
             end if
          end do


!!$          do i = 1,len(string)
!!$             k = iachar(string(i:i))
!!$             if ( k >= iachar('a') .and. k <= iachar('z') ) then
!!$                k = k + iachar('A') - iachar('a')
!!$                new(i:i) = achar(k)
!!$             end if
!!$          end do
       end if
    end if
    !call trace_exit('io_case')
  end function io_case


  subroutine io_cl_parser()
    !==============================================================================!
    !                           I O _ C L _ P A R S E R                            !
    !==============================================================================!
    ! Subroutine for the handling of commandline arguments.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    integer          ::   nargs     !Number of args
    integer          ::   arg_index !The index
    character(50)    ::   name      !The name of the argument

    logical          ::   search = .false.
    logical          ::   help   = .false.
    logical          ::   list   = .false.
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num
    character(len=100):: compile_version,cpuinfo
    character(len=5) :: opt

    nargs=command_argument_count()


    if (nargs.eq.0)then
       seed='derek'
       write(*,*) trim(info)
       write(*,*) trim(version)

       call io_help()
       !call io_errors("no seed provided.")
    end if


    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(min_mpi,maj_mpi)

       write(mpi_version_num,97)min_mpi,maj_mpi
97     format(i1,"."i1)
       min_char=scan(mpi_c_version,".")
       !print*, mpi_c_version,mpi_version_num
    end if


    if (nargs.gt.0)then
       do arg_index=1,nargs

          call get_command_argument(arg_index,name)
          select case(adjustl(trim(name)))
          case("-h","--help")
             write(*,*) trim(info)
             write(*,*) trim(version)
             read_params=.false.
             call io_list_params(.false.)
             help=.true.
             if (arg_index.eq.nargs)then
                call io_help()
                call comms_stop()
             end if
          case("-s","--search")
             write(*,*) trim(info)
             write(*,*) trim(version)
             write(*,*)
             read_params=.false.
             call io_list_params(.false.)
             search=.true.
             if (arg_index.eq.nargs)then
                call io_help()
                call comms_stop()
             end if
          case("-v")
             ! This is now a bit of a mess. This should be called in the main derek file before opening anything. All to avoid cascade.
             version_only = .true.
             read_params=.false.
          case("-c","--check")
             current_params%check=.true.
             if (nargs.lt.2)then
                write(*,*) trim(info)
                write(*,*) trim(version)
                call io_help()
             end if
          case ('-r','--restart')
             current_params%restart=.true.
             if (nargs.lt.2)then
                write(*,*) trim(info)
                write(*,*) trim(version)
                call io_help()
             end if

          case("-l","--list")
             read_params=.false.
             list=.true.
             if (arg_index.eq.nargs)then
                write(*,*) trim(info)
                write(*,*) trim(version)
                call io_list_params(.true.)
                call comms_stop()
             end if
          case('--master_debug') ! Don't worry about this
          case default
             if (help)then
                call io_help(name)
                help=.false.
                call comms_stop()
             elseif(search)then
                call io_search(io_case(name))
                search=.false.
                call comms_stop()
             else if(list)then
                write(*,*) trim(info)
                write(*,*) trim(version)
                write(*,*)
                call io_list_params(.true.,io_case(name))
                list=.false.
                call comms_stop()

             else
                seed=name
                if (seed(1:1).eq.'-')then
                   write(*,*) trim(info)
                   write(*,*) trim(version)
                   write(*,*)"Unknown argument: ",trim(seed)
                   call io_help()
                end if
                read_params=.true.
             end if
          end select
       end do
    else
       write(*,*) trim(info)
       write(*,*) trim(version)
       call io_help()
    end if




    return
  end subroutine io_cl_parser

  subroutine io_help(string)
    !==============================================================================!
    !                                I O _ H E L P                                 !
    !==============================================================================!
    ! Subroutine for printing the help information to the terminal.                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/02/2020                                            !
    !==============================================================================!
    implicit none
    character(*),optional   :: string
    integer                 :: i !counter
    logical                 :: found=.false.

    if (present(string))then
       do i=1,max_keys
          if (trim(keys_array(i)).eq.io_case(trim(string))) then
             found=.true.
             write(*,*)
             write(*,12) repeat("*",70-len(keys_array(i))/2),&
                  & trim(io_case(keys_array(i),upper=.true.)),repeat("*",70-len(keys_array(i))/2)
             write(*,*) trim(keys_description(i))
             write(*,*)
             write(*,*) "Allowed Values: ",trim(keys_allowed(i))
             write(*,*) "Default:        ",trim(keys_default(i))
             if (keys_example(i).ne.'')then
                write(*,*)                
                write(*,12) repeat("-",23),&
                     & 'Example',repeat("-",23)

                write(*,*)trim(keys_example(i))
             end if
             write(*,*)
12           format(1x,a,1x,a,1x,a)

             exit
          end if
       end do
       if (.not.found)then
          write(*,*)
          write(*,*) "************** NO MATCHING PARAMETERS **************"
          write(*,*)
       end if
    else
       write(*,*)  "Usage:"
       write(*,30) "derek.mpi","<seed>","Run a calculation from <seed>.info"
       write(*,30) '    "    ', "-v","Print version information."
       write(*,30) '    "    ', "-h,--help    <keyword>","Get help and commandline options. Optional keyword."
       write(*,30) '    "    ', "-s,--search  <keyword>", "Search list of available parameters."
       write(*,30) '    "    ', "-l,--list    <keyword>","Print list of parameters. Optional keyword for specific category."
       write(*,30) '    "    ', "-c,--check   <seed>","Run calculation to check input files."
       write(*,30) '    "    ', "-r,--restart <seed>","Run calculation continuing from a previous <seed>.state file"
    end if
30  format(2x,A,4x,A,T40,":",3x,A)
    call comms_stop()
    return
  end subroutine io_help


  subroutine io_search(string)
    !==============================================================================!
    !                              I O _ S E A R C H                               !
    !==============================================================================!
    ! Subroutine for using the command to search for availible variables that      !
    ! the user can change in a calculation                                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           string,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    character(*)     :: string
    logical          :: found
    integer          :: i,scan_res



    do i=1,max_keys
       scan_res=index(trim(keys_array(i)),trim(string))
       if (scan_res.eq.0) then
          scan_res=index(trim(keys_description(i)),trim(string))
       end if
       if (scan_res.gt.0)then
          found=.true.
100       format(1x,A,T35,A)
          write(*,100)io_case(keys_array(i),.true.),keys_description(i)
       end if
    end do
    if (.not.found)then
       write(*,*)
       write(*,*) "************** NO MATCHING PARAMETERS **************"
       write(*,*)
    end if

    return
  end subroutine io_search

  subroutine io_list_params(print_flag,string)
    !==============================================================================!
    !                         I O _ L I S T _ P A R A M S                          !
    !==============================================================================!
    ! Subroutine used to print the total list of variables to the terminal and     !
    ! also to allocate all of the global arrays which contain the variable data.   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           print_flag,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    logical  :: print_flag
    character(*),optional  :: string
    character(100) :: junk
    integer       :: i,j,nl ! loops


    integer :: cat_index = -1
    integer,      dimension(1:max_keys) :: mapping

    ! Allocate all the arrays for the parameters
    allocate(keys_array(1:max_keys))
    allocate(keys_default(1:max_keys))
    allocate(keys_description(1:max_keys))
    allocate(keys_example(1:max_keys))
    allocate(keys_allowed(1:max_keys))
    allocate(keys_cat(1:max_keys))



    ! assign the keys
    ! %Begin: assign_keys

    keys_array(1)=trim(key_check)
    keys_array(2)=trim(key_debugging)
    keys_array(3)=trim(key_n_electrons)
    keys_array(4)=trim(key_energy_tol)
    keys_array(5)=trim(key_xc_functional)
    keys_array(6)=trim(key_cut_off_energy)
    keys_array(7)=trim(key_g_fine_scale)
    keys_array(8)=trim(key_write_wvfn)
    keys_array(9)=trim(key_task)
    keys_array(10)=trim(key_soc)
    keys_array(11)=trim(key_max_scf)
    keys_array(12)=trim(key_scf_method)
    keys_array(13)=trim(key_electronic_temp)
    keys_array(14)=trim(key_conduction_bands)
    keys_array(15)=trim(key_write_density)
    keys_array(16)=trim(key_Write_potential)
    keys_array(17)=trim(key_write_state)
    keys_array(18)=trim(key_external_pot)
    keys_array(19)=trim(key_kpt_mp_grid)
    keys_array(20)=trim(key_write_spec)
    keys_array(21)=trim(key_write_memory)
    keys_array(22)=trim(key_iprint)
    keys_array(23)=trim(key_finite_barrier_height)
    keys_array(24)=trim(key_finite_barrier_width)
    keys_array(25)=trim(key_periodic_pot_grid)
    keys_array(26)=trim(key_periodic_pot_amp)
    keys_array(27)=trim(key_random_seed)
    keys_array(28)=trim(key_write_formatted_potential)
    keys_array(29)=trim(key_write_potex)
    keys_array(30)=trim(key_write_formatted_density)
    keys_array(31)=trim(key_write_formatted_potex)
    keys_array(32)=trim(key_output_level)
    keys_array(33)=trim(key_ext_efield)
    keys_array(34)=trim(key_ext_bfield)
    keys_array(35)=trim(key_unit_energy)
    keys_array(36)=trim(key_unit_length)
    keys_array(37)=trim(key_unit_efield)
    keys_array(38)=trim(key_unit_bfield)
    keys_array(39)=trim(key_out_energy_unit)
    keys_array(40)=trim(key_out_len_unit)
    keys_array(41)=trim(key_out_efield_unit)
    keys_array(42)=trim(key_out_bfield_unit)
    keys_array(43)='lattice'      ! Special case for block type, needs a help section, but doesnt need to be a key
    keys_array(44)=trim(key_dos_kpt_mp_grid)
    keys_array(45)=trim(key_write_xsf)
    ! %End: assign_keys

    ! %Begin: assign_default

    write(junk,*)current_params%check
    keys_default(1)=trim(adjustl(junk))
    write(junk,*)current_params%debugging
    keys_default(2)=trim(adjustl(junk))
    write(junk,*)current_params%n_electrons
    keys_default(3)=trim(adjustl(junk))
    write(junk,*)current_params%energy_tol
    keys_default(4)=trim(adjustl(junk))
    write(junk,*)current_params%xc_functional
    keys_default(5)=trim(adjustl(junk))
    write(junk,*)current_params%cut_off_energy
    keys_default(6)=trim(adjustl(junk))
    write(junk,*)current_params%g_fine_scale
    keys_default(7)=trim(adjustl(junk))
    write(junk,*)current_params%write_wvfn
    keys_default(8)=trim(adjustl(junk))
    write(junk,*)current_params%task
    keys_default(9)=trim(adjustl(junk))
    write(junk,*)current_params%soc
    keys_default(10)=trim(adjustl(junk))
    write(junk,*)current_params%max_scf
    keys_default(11)=trim(adjustl(junk))
    write(junk,*)current_params%scf_method
    keys_default(12)=trim(adjustl(junk))
    write(junk,*)current_params%electronic_temp
    keys_default(13)=trim(adjustl(junk))
    write(junk,*)current_params%conduction_bands
    keys_default(14)=trim(adjustl(junk))
    write(junk,*)current_params%write_density
    keys_default(15)=trim(adjustl(junk))
    write(junk,*)current_params%Write_potential
    keys_default(16)=trim(adjustl(junk))
    write(junk,*)current_params%write_state
    keys_default(17)=trim(adjustl(junk))
    write(junk,*)current_params%external_pot
    keys_default(18)=trim(adjustl(junk))
    write(junk,*)current_params%kpt_mp_grid
    keys_default(19)=trim(adjustl(junk))
    write(junk,*)current_params%write_spec
    keys_default(20)=trim(adjustl(junk))
    write(junk,*)current_params%write_memory
    keys_default(21)=trim(adjustl(junk))
    write(junk,*)current_params%iprint
    keys_default(22)=trim(adjustl(junk))
    write(junk,*)current_params%finite_barrier_height
    keys_default(23)=trim(adjustl(junk))
    write(junk,*)current_params%finite_barrier_width(1)
    keys_default(24)=trim(adjustl(junk))
    write(junk,*)current_params%periodic_pot_grid
    keys_default(25)=trim(adjustl(junk))
    write(junk,*)current_params%periodic_pot_amp
    keys_default(26)=trim(adjustl(junk))
    write(junk,*)current_params%random_seed
    keys_default(27)=trim(adjustl(junk))
    write(junk,*)current_params%write_formatted_potential
    keys_default(28)=trim(adjustl(junk))
    write(junk,*)current_params%write_potex
    keys_default(29)=trim(adjustl(junk))
    write(junk,*)current_params%write_formatted_density
    keys_default(30)=trim(adjustl(junk))
    write(junk,*)current_params%write_formatted_potex
    keys_default(31)=trim(adjustl(junk))
    write(junk,*)current_params%output_level
    keys_default(32)=trim(adjustl(junk))
    write(junk,*)current_params%ext_efield
    keys_default(33)=trim(adjustl(junk))
    write(junk,*)current_params%ext_bfield
    keys_default(34)=trim(adjustl(junk))
    write(junk,*)current_params%unit_energy
    keys_default(35)=trim(adjustl(junk))
    write(junk,*)current_params%unit_length
    keys_default(36)=trim(adjustl(junk))
    write(junk,*)current_params%unit_efield
    keys_default(37)=trim(adjustl(junk))
    write(junk,*)current_params%unit_bfield
    keys_default(38)=trim(adjustl(junk))
    write(junk,*)current_params%out_energy_unit
    keys_default(39)=trim(adjustl(junk))
    write(junk,*)current_params%out_len_unit
    keys_default(40)=trim(adjustl(junk))
    write(junk,*)current_params%out_efield_unit
    keys_default(41)=trim(adjustl(junk))
    write(junk,*)current_params%out_bfield_unit
    keys_default(42)=trim(adjustl(junk))
    keys_default(43)=trim(adjustl('None'))  !! Special case for block data, no default, must be specified

    write(junk,*)current_params%dos_kpt_mp_grid
    keys_default(44)=trim(adjustl(junk))
    write(junk,*)current_params%write_xsf
    keys_default(45)=trim(adjustl(junk))
    ! %End: assign_default

    ! %Begin: assign_description
    keys_description(1)='Check the input files without starting any intensive calculations'
    keys_description(2)='Turn on profilling of the code'
    keys_description(3)='Number of electrons in the calculation'
    keys_description(4)='Energy convergence tolerance of the SCF cycle'
    keys_description(5)='Exchange and correlation functional'
    keys_description(6)='Maximum plane wave energy used to determine G vector basis'
    keys_description(7)='G vector grid scale for fine grid basis'
    keys_description(8)='Write wavefunction to file'
    keys_description(9)='Type of calculation to perform'
    keys_description(10)='Include spin orbit coupling in the calculation'
    keys_description(11)='Maximum number of SCF steps'
    keys_description(12)='The type of method used for SCF minimisation, either EDFT or DM'
    keys_description(13)='Electronic temperature used to smear the Fermi level, for better convergence'
    keys_description(14)='Number of conduction bands to be included'
    keys_description(15)='Write the final density to file'
    keys_description(16)='Write total potential to file'
    keys_description(17)='Write state information to file.'
    keys_description(18)='Provide an external potential, jelly, finite_barrier, periodic_pot or from file'
    keys_description(19)='Monkhurst pack grid for the SCF cycle'
    keys_description(20)='Write orbital information to spectral file'
    keys_description(21)='Write out the memory usage on each node throughout the calculation'
    keys_description(22)='Determine output verbosity in the output file'
    keys_description(23)='Height of the external potential barrier for a finite potential barrier in eV'
    keys_description(24)='Fractional width of barrier in each principal direction'
    keys_description(25)='Number of cycles of periodic potential in each crystal direction'
    keys_description(26)='Amplitude of the periodic potential in eV'
    keys_description(27)='A random seed to initialise the random number generator'
    keys_description(28)='Write total potential to file in a human readable format.'
    keys_description(29)='Write the external potential to a file'
    keys_description(30)='Write the density to a human readable file'
    keys_description(31)='Write the external potential to a human readable file'
    keys_description(32)='Level to set amount of output files written.'
    keys_description(33)='Real vector of externally applied electric field'
    keys_description(34)='Vector of externally applied magnetic field in Tesla'
    keys_description(35)='Unit of energy to be used to parse all energy terms in the .info file'
    keys_description(36)='Unit of length to be used to parse all length terms in the .info file'
    keys_description(37)='Unit of electric field to be used to parse all electric field terms in the .info file'
    keys_description(38)='Unit of magnetic field to be used to parse all magnetic field terms in the .info file'
    keys_description(39)='Unit of energy to be used to output all energy terms in the .derek file'
    keys_description(40)='Unit of length to be used to output all length terms in the .derek file'
    keys_description(41)='Unit of electric field to be output to parse all electric field terms in the .derek file'
    keys_description(42)='Unit of magnetic field to be output to parse all magnetic field terms in the .derek file'
    keys_description(43)='Block type data to define lattice vectors of unit cell '
    keys_description(44)='Monkorst-Pack grid used for calculating the Density of States'
    keys_description(45)='Write output files using the XcrySDen file format rather than the standard DEReK file formats.'
    ! %End: assign_description

    ! %Begin: assign_allowed
    keys_allowed(1)  = 'Boolean'                          ! key_check
    keys_allowed(2)  = 'Boolean'                          ! key_debugging
    keys_allowed(3)  = 'Integer > 0'                      ! key_n_electrons
    keys_allowed(4)  = 'Real > 0 eV'                      ! key_energy_tol
    keys_allowed(5)  = 'LDA'                              ! key_xc_functional
    keys_allowed(6)  = 'Real > 150 eV'                    ! key_cut_off_energy
    keys_allowed(7)  = 'Real > 1.0'                       ! key_g_fine_scale
    keys_allowed(8)  = 'Boolean'                          ! key_write_wvfn
    keys_allowed(9)  = 'singlepoint, bandstructure'       ! key_task
    keys_allowed(10) = 'Boolean'                          ! key_soc
    keys_allowed(11) = 'Integer > 5'                      ! key_max_scf
    keys_allowed(12) = 'DM, EDFT'                         ! key_scf_method
    keys_allowed(13) = 'Real > 0'                         ! key_electronic_temp
    keys_allowed(14) = 'Integer > 0'                      ! key_conduction_bands
    keys_allowed(15) = 'Boolean'                          ! key_write_density
    keys_allowed(16) = 'Boolean'                          ! key_Write_potential
    keys_allowed(17) = 'Boolean'                          ! key_write_state
    keys_allowed(18) = 'Jelly, periodic_pot, finite_barrier, potex, fpotex' ! key_external_pot
    keys_allowed(19) = 'Integer > 1'                      ! key_kpt_mp_grid
    keys_allowed(20) = 'Boolean'                          ! key_write_spec
    keys_allowed(21) = 'Boolean'                          ! key_write_memory
    keys_allowed(22) = '1, 2, 3'                          ! key_iprint
    keys_allowed(23) = 'Any real number'                  ! key_finite_barrier_height
    keys_allowed(24) = 'Fractional triplet (e.g., 0.2, 0.3, 0.5)' ! key_finite_barrier_width
    keys_allowed(25) = 'Integer > 0'                      ! key_periodic_pot_grid
    keys_allowed(26) = 'Real > 0'                         ! key_periodic_pot_amp
    keys_allowed(27) = 'Any integer'                      ! key_random_seed
    keys_allowed(28) = 'Boolean'                          ! key_write_formatted_potential
    keys_allowed(29) = 'Boolean'                          ! key_write_potex
    keys_allowed(30) = 'Boolean'                          ! key_write_formatted_density
    keys_allowed(31) = 'Boolean'                          ! key_write_formatted_potex
    keys_allowed(32) = 'none, minimal, all'               ! key_output_level
    keys_allowed(33) = 'Any real vector'                  ! key_ext_efield
    keys_allowed(34) = 'Any real vector'                  ! key_ext_bfield
    keys_allowed(35) = 'Ha, mHa, eV, meV, Ry, mRy, J'     ! key_unit_energy
    keys_allowed(36) = 'm, mm, cm, nm, μm, pm, Å, bohr'   ! key_unit_length
    keys_allowed(37) = 'eV/Å/e, Ha/Bohr/e, N/C'           ! key_unit_efield
    keys_allowed(38) = 'T, G, A/m'                        ! key_unit_bfield
    keys_allowed(39) = 'Ha, mHa, eV, meV, Ry, mRy, J'     ! key_out_energy_unit
    keys_allowed(40) = 'm, mm, cm, nm, μm, pm, Å, bohr'   ! key_out_len_unit
    keys_allowed(41) = 'eV/Å/e, Ha/Bohr/e, N/C'           ! key_out_efield_unit
    keys_allowed(42) = 'T, G, A/m'                        ! key_out_bfield_unit
    keys_allowed(43) = '3x3 matrix of lattice vectors'    ! key_dos_kpt_mp_grid
    keys_allowed(44) = 'Any vector'                       ! key_dos_kpt_mp_grid
    keys_allowed(45) = 'Boolean'                          ! key_write_xsf
    ! %End: assign_allowed


    ! %Begin: assign_example

    keys_example(1) =''
    keys_example(2) =''
    keys_example(3) =''
    keys_example(4) =''
    keys_example(5) =''
    keys_example(6) =''
    keys_example(7) =''
    keys_example(8) =''
    keys_example(9) =''
    keys_example(10)=''
    keys_example(11)=''
    keys_example(12)=''
    keys_example(13)=''
    keys_example(14)=''
    keys_example(15)=''
    keys_example(16)=''
    keys_example(17)=''
    keys_example(18)='For a default potential --> e.g. external_pot : Jelly\n\n'//&
         & ' To read potential from a file --> e.g. external_potential : some_file.potex\n' //&
         & '                               --> e.g. external_potential : some_file.fpotex\n\n'//&
         & ' To build potentials from local spherical distributions (gaussian or lorentzian) requires a DATA block:\n'//&
         & '  $DATA gaussian_pot\n'//&
         & '   # Centre (frac)   Height (eV)   Width (A)\n'// &
         & '     0.2  0.3 0.6       0.1          0.05 \n'//&
         & '     0.15 0.7 0.25      0.1          0.05 \n'//&
         & '  $ENDDATA gaussian_pot\n\n'//&
         & ' N.B. Positions are in fractional coordinates'
    keys_example(19)=''
    keys_example(20)=''
    keys_example(21)=''
    keys_example(22)=''
    keys_example(23)=''
    keys_example(24)=''
    keys_example(25)=''
    keys_example(26)=''
    keys_example(27)=''
    keys_example(28)=''
    keys_example(29)=''
    keys_example(30)=''
    keys_example(31)=''
    keys_example(32)=''
    keys_example(33)='EXT_EFIELD : 1.0 0.0 0.0'
    keys_example(34)='EXT_BFIELD : 0.0 0.0 0.1'
    keys_example(35)=''
    keys_example(36)=''
    keys_example(37)=''
    keys_example(38)=''
    keys_example(39)=''
    keys_example(40)=''
    keys_example(41)=''
    keys_example(42)=''
    keys_example(43)='$DATA lattice\n '//&
         & ' 5.0 0.0 0.0\n '//&
         & ' 0.0 5.0 0.0\n '//&
         & ' 0.0 0.0 5.0\n '//&
         & '$ENDDATA lattice'

    keys_example(44)='dos_kpt_mp_grid : 25 25 25'
    keys_example(45)=''
    ! %End: assign_example



    cats = (/'FUNDAMENTAL  '&
         &  ,'PLANEWAVES   '&
         &  ,'MINIMISATION '&
         &  ,'POTENTIAL    '&
         &  ,'WAVEFUNCTION '&
         &  ,'I/O          '&
         &  ,'MISCELLANEOUS'&
         &  ,'ADVANCED     '&
         &  ,'DATA BLOCKS  '&
         &  ,'SPECTRAL     '/)

    !  %Begin: assign_cats
    keys_cat(1)  = 7    ! key_check
    keys_cat(2)  = 7    ! key_debugging
    keys_cat(3)  = 1    ! key_n_electrons
    keys_cat(4)  = 3    ! key_energy_tol
    keys_cat(5)  = 1    ! key_xc_functional
    keys_cat(6)  = 2    ! key_cut_off_energy
    keys_cat(7)  = 2    ! key_g_fine_scale
    keys_cat(8)  = 6    ! key_write_wvfn
    keys_cat(9)  = 1    ! key_task
    keys_cat(10) = 8    ! key_soc
    keys_cat(11) = 3    ! key_max_scf
    keys_cat(12) = 3    ! key_scf_method
    keys_cat(13) = 3    ! key_electronic_temp
    keys_cat(14) = 5    ! key_conduction_bands
    keys_cat(15) = 6    ! key_write_density
    keys_cat(16) = 6    ! key_Write_potential
    keys_cat(17) = 6    ! key_write_state
    keys_cat(18) = 4    ! key_external_pot
    keys_cat(19) = 1    ! key_kpt_mp_grid
    keys_cat(20) = 6    ! key_write_spec
    keys_cat(21) = 7    ! key_write_memory
    keys_cat(22) = 6    ! key_iprint
    keys_cat(23) = 4    ! key_finite_barrier_height
    keys_cat(24) = 4    ! key_finite_barrier_width
    keys_cat(25) = 4    ! key_periodic_pot_grid
    keys_cat(26) = 4    ! key_periodic_pot_amp
    keys_cat(27) = 6    ! key_random_seed
    keys_cat(28) = 6    ! key_write_formatted_potential
    keys_cat(29) = 6    ! key_write_potex
    keys_cat(30) = 6    ! key_write_formatted_density
    keys_cat(31) = 6    ! key_write_formatted_potex
    keys_cat(32) = 6    ! key_output_level
    keys_cat(33) = 8    ! key_ext_efield
    keys_cat(34) = 8    ! key_ext_bfield
    keys_cat(35) = 6    ! key_unit_energy
    keys_cat(36) = 6    ! key_unit_length
    keys_cat(37) = 6    ! key_unit_efield
    keys_cat(38) = 6    ! key_unit_bfield
    keys_cat(39) = 6    ! key_out_energy_unit
    keys_cat(40) = 6    ! key_out_len_unit
    keys_cat(41) = 6    ! key_out_efield_unit
    keys_cat(42) = 6    ! key_out_bfield_unit
    keys_cat(43) = 9    ! key_dos_kpt_mp_grid
    keys_cat(44) = 10   ! key_dos_kpt_mp_grid
    keys_cat(45) = 6    ! key_write_xsf
    ! %End: assign_cats
    call io_alphabetise(keys_array,max_keys,mapping)

    ! do the loop for printing stuff

    if (present(string))then
       do i=1,n_cats
          if (trim(string).eq.trim(io_case(cats(i))))then
             cat_index = i
          end if
       end do
       if (cat_index.eq.-1)then
          write(*,*) "Unknown category: ",trim(string)
          call comms_stop()
       end if
    end if



    if (print_flag)then
100    format(1x,A,T35,A)
       write(*,*)
       do j=1,n_cats
          if (cat_index.eq.-1 .or. j.eq.cat_index)then
             write(*,*)adjustl(trim(cats(j)))
             write(*,*)repeat('-',len(adjustl(trim(cats(j)))))
             do i=1,max_keys
                if (keys_cat(mapping(i)).eq.j )then
                   write(*,100) io_case(trim(keys_array(mapping(i))),.true.),trim(keys_description(mapping(i)))
                end if
             end do
             write(*,*)
          end if
       end do
    end if


    return
  end subroutine io_list_params





  subroutine io_check()
    !==============================================================================!
    !                              I O _ D R Y R U N                               !
    !==============================================================================!
    ! Subroutine for handlind the check command which allows for parameter        !
    ! checking                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    call trace_entry("io_check")
    if (on_root_node)then
       write(stdout,*)
       write(stdout,'(16x,A)') "+======================================+"
       write(stdout,'(16x,A)') "|                                      |"
       write(stdout,'(16x,A)') "|         Check complete....           |"
       write(stdout,'(16x,A)') "|          No errors found             |"
       write(stdout,'(16x,A)') "|                                      |"
       write(stdout,'(16x,A)') "+======================================+"
       write(stdout,*)
    end if
    call io_finalise()
    call trace_exit("io_check")
    call trace_exit("derek")
    !call COMMS_FINALISE()
    call trace_finalise(current_params%debugging,rank,seed=seed)
    call comms_stop()

  end subroutine io_check



  function io_present(key) result(is_present)
    !==============================================================================!
    !                             I O _ P R E S E N T                              !
    !==============================================================================!
    ! Function used to determine if there is a keyword present in the input file   !
    !                                                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           key,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           is_present                                                         !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    logical      :: is_present
    character(*) :: key
    call trace_entry("io_present")
    if (any(present_array.eq.key))then
       is_present=.TRUE.
    else
       is_present=.FALSE.
    end if
    call trace_exit("io_present")
  end function io_present

  subroutine io_flush(unit)
    !==============================================================================!
    !                               I O _ F L U S H                                !
    !==============================================================================!
    ! Subroutine wrapper for the intrinsic function that forces the system to      !
    ! clear the cache so that I/O can be written to file. Mainly used with the     !
    ! GNU fortran compiler, programs compiled with that compiler tend to hold on   !
    ! to the cache much longer.                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  29/02/2020                                            !
    !==============================================================================!
    implicit none
    integer  :: unit
    call trace_entry("io_flush")
    call flush(unit)
    call trace_exit("io_flush")
    return
  end subroutine io_flush



  subroutine io_block_parse(line,dummy_params,stat)
    !==============================================================================!
    !                         I O _ B L O C K _ P A R S E                          !
    !==============================================================================!
    ! Subroutine for parsing block type data in the structure file                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           line,              intent :: in                                    !
    !           stat,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  03/12/2020                                            !
    !==============================================================================!
    implicit none
    character(*), intent(in)    :: line
    type(parameters),intent(inout)  :: dummy_params
    integer     , intent(inout) :: stat

    character(len=6)   :: block_str                    ! Place to store $DATA
    character(len=20)  :: block_type_in,block_type_out ! The Type of block data
    character(len=300) :: buff                         ! Empty Buffer

    logical,dimension(:),allocatable :: not_comments       ! Logical array to show if the line is a comment or not

    integer :: in_stat, i, j, k,counter

    integer :: n_lines ! Total number of lines inside the block
    logical :: block_closed

    call trace_entry("io_block_parse")

    ! Make sure the line is lower case and write it to a buffer
    buff = trim(adjustl(io_case(line)))
    read(buff,*,iostat=in_stat) block_str,block_type_in

    if (in_stat.ne.0) call io_errors(" problem parsing block")
    n_lines = 0

    ! loop over all the lines to see the length
    do

       read(info_unit,'(a)') buff

       buff=trim(adjustl(io_case(buff)))
       ! If we can successfully read the line, increment the counter


       ! See if it is the end of the block i.e. contains $ENDDATA          
       if (index(buff,"$enddata").ne.0)then

          ! Check to see if the blocks match
          read(buff,*,iostat=in_stat) block_str,block_type_out      
          if (in_stat.ne.0) call io_errors("Problem parsing end block")

          if (trim(block_type_in).ne.trim(block_type_out))&
               & call io_errors("Block "//trim(block_type_in)//" not closed" )
          exit
       else
          n_lines = n_lines + 1
       end if
       if (stat.ne.0)call io_errors("Error parsing "//trim(seed)//".info")
    end do

    if (trim(block_type_in).ne.trim(block_type_out))then
       call io_errors("Block type mismatch")
    end if

    ! take us back to the top
    do i=1,n_lines+1
       backspace(info_unit)
    end do

    ! Now we read it again and allocate the comment information
    call memory_allocate(not_comments,1,n_lines,'I')

    do i=1,n_lines
       read(info_unit,'(a)',iostat=stat)buff

       ! See if its a comment
       if (buff(1:1).eq.'#' .or. buff(1:1).eq.'!'.or.buff(1:1).eq."")then
          not_comments(i) = .false.
       else
          not_comments(i) = .true.
       end if
    end do

    ! Rewind again 
    do i=1,n_lines
       backspace(info_unit)
    end do


    if (count(not_comments).lt.1)call io_errors('No data in block')
    ! Select type of block

    select case(io_case(block_type_in))
    case("lattice")

       if (count(not_comments).ne.3)then
          call io_errors("LATTICE must have 3 lines")
       end if

       counter=0
       do i=1,n_lines
          ! Skip if its a comment
          if (not_comments(i))then
             counter = counter+1
          else
             read(info_unit,'(a)')buff
             cycle
          end if

          read(info_unit,*,iostat=stat)current_structure%cell(counter,1),current_structure%cell(counter,2),current_structure%cell(counter,3)        
          if (stat.ne.0)call io_errors('Error parsing lattice matrix')
       end do
       cell_declared=.true.
       ! convert to Bohr
       do i =1,3
          do j =1,3
             current_structure%cell(i,j)=units_to_atomic(current_structure%cell(i,j),current_params%unit_length)
          end do
       end do


    case('gaussian_pot','lorentzian_pot')
       dummy_params%external_pot = io_case(block_type_in)
       ! Allocate the arrays
       current_structure%local_pots = .true.
       current_structure%n_pots =count(not_comments)


       
       call memory_allocate(current_structure%pot_centres,1,3,1,current_structure%n_pots,'I')
       call memory_allocate(current_structure%pot_heights,1,current_structure%n_pots,'I')
       call memory_allocate(current_structure%pot_widths,1,current_structure%n_pots,'I')
       counter = 0
       do i = 1, n_lines
          if (not_comments(i))then
             counter = counter+1
          else
             read(info_unit,'(a)')buff
             cycle
          end if

          read(info_unit,*) current_structure%pot_centres(1,counter),&  !! Read in the centres first
               & current_structure%pot_centres(2,counter),&
               & current_structure%pot_centres(3,counter),&
               & current_structure%pot_heights(counter), &              !! Potential heights
               & current_structure%pot_widths(counter)                  !! Potential widths


          current_structure%pot_heights(counter) = &
               & units_to_atomic(current_structure%pot_heights(counter),&
               & current_params%unit_energy)
          
          current_structure%pot_widths(counter) = &
               & units_to_atomic(current_structure%pot_widths(counter),&
               & current_params%unit_length)
       end do
       

    case default
       call io_errors(' Unknown block type '//trim(block_type_in))
    end select

    ! read the close line again
    read(info_unit,*)buff
    call memory_deallocate(not_comments,'I')

    call trace_exit("io_block_parse")
    return
  end subroutine io_block_parse


  subroutine io_lattice_to_abc()
    !==============================================================================!
    !                      I O _ L A T T I C E _ T O _ A B C                       !
    !==============================================================================!
    ! Subroutine for converting from a cartesian lattice matrix to lattice         !
    ! parameters                                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    implicit none
    call trace_entry('io_lattice_to_abc')

    ! Calculate the inverse lattice while were here
    current_structure%inv_cell=transpose(twopi*io_inv(current_structure%cell))

    current_structure%lattice_a=sqrt(current_structure%cell(1,1)**2+&
         & current_structure%cell(2,1)**2+&
         & current_structure%cell(3,1)**2)

    current_structure%lattice_b=sqrt(current_structure%cell(1,2)**2+&
         & current_structure%cell(2,2)**2+&
         & current_structure%cell(3,2)**2)

    current_structure%lattice_c=sqrt(current_structure%cell(1,3)**2+&
         & current_structure%cell(2,3)**2+&
         & current_structure%cell(3,3)**2)


    current_structure%alpha=rad_to_deg*acos(dot_product(current_structure%cell(:,2),current_structure%cell(:,3))&
         &/(current_structure%lattice_b*current_structure%lattice_c))

    current_structure%beta=rad_to_deg*acos(dot_product(current_structure%cell(:,1),current_structure%cell(:,3))&
         &/(current_structure%lattice_a*current_structure%lattice_c))

    current_structure%gamma=rad_to_deg*acos(dot_product(current_structure%cell(:,2),current_structure%cell(:,1))&
         &/(current_structure%lattice_b*current_structure%lattice_a))

    current_structure%volume = io_volume(current_structure%cell(:,1),current_structure%cell(:,2),current_structure%cell(:,3))
    call trace_exit('io_lattice_to_abc')
  end subroutine io_lattice_to_abc


  function io_volume(a,b,c)
    real(dp),intent(in),dimension(1:3) :: a,b,c
    real(dp) :: io_volume
    call trace_entry("io_volume")
    io_volume=c(1)*(a(2)*b(3)-a(3)*b(2))+c(2)*(b(1)*a(3)-a(3)*b(1))+c(3)*(a(1)*b(2)-a(2)*b(1))
    call trace_exit("io_volume")
  end function io_volume


  subroutine io_kpoint_grid()
    !==============================================================================!
    !                         I O _ K P O I N T _ G R I D                          !
    !==============================================================================!
    ! Subroutine for calculating the Monkhurst Pack grid for the SCF cycle         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!

    implicit none
    integer    :: num_kpoints
    integer    :: nkx,nky,nkz,ik=0
    real(dp)   :: dkx,dky,dkz
    call trace_entry('io_kpoint_grid')

    num_kpoints = current_params%kpt_mp_grid(1)*current_params%kpt_mp_grid(2)*current_params%kpt_mp_grid(3)
    current_structure%num_kpoints = num_kpoints

    if (num_kpoints.le.0)call io_errors(" invalid number of kpoints")

    ! Allocate the kpoint arrays
    call memory_allocate(current_structure%kpt_scf_list,1,num_kpoints,1,3,"I")

    dkx=1.0_dp/current_params%kpt_mp_grid(1)
    dky=1.0_dp/current_params%kpt_mp_grid(2)
    dkz=1.0_dp/current_params%kpt_mp_grid(3)

    do nkx=1,current_params%kpt_mp_grid(1)
       do nky=1,current_params%kpt_mp_grid(2)
          do nkz=1,current_params%kpt_mp_grid(3)
             ik=ik+1
             current_structure%kpt_scf_list(ik,1)=nkx*dkx - (1.0_dp+dkx)/2
             current_structure%kpt_scf_list(ik,2)=nky*dky - (1.0_dp+dky)/2
             current_structure%kpt_scf_list(ik,3)=nkz*dkz - (1.0_dp+dkz)/2
          end do
       end do
    end do

    call trace_exit('io_kpoint_grid')
    return
  end subroutine io_kpoint_grid


  subroutine io_write_params(num_grid_points,num_fine_grid_points,ngx,ngy,ngz,fine_ngx,fine_ngy,fine_ngz)
    !==============================================================================!
    !                        I O _ W R I T E _ P A R A M S                         !
    !==============================================================================!
    ! Subroutine that writes the input parameters to the stdout seed.derek         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    ! Inputs from basis and comms
    integer,intent(in) :: num_grid_points
    integer,intent(in) :: num_fine_grid_points
    integer,intent(in) :: ngx
    integer,intent(in) :: ngy
    integer,intent(in) :: ngz
    integer,intent(in) :: fine_ngx
    integer,intent(in) :: fine_ngy
    integer,intent(in) :: fine_ngz

    character(50)   :: sec_title
    integer         :: width=69,length,i
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)
    character*10                    :: b(3)

    character(len=:), allocatable :: string
    character(10) :: out_invlen_unit
    character(10) :: out_vol_unit

    call trace_entry("io_write_params")
    out_invlen_unit = trim(current_params%out_len_unit)//'-1'
    out_vol_unit = trim(current_params%out_len_unit)//'**3'

    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    write(stdout,*)"+"//repeat("-",(width-15)/2)//" RUN STARTED "//repeat("-",(width-16)/2)//"+"
    write(stdout,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(stdout,*)"+"//repeat("-",width-3)//"+"
    write(stdout,*) " "
1000 FORMAT(1x,"|",22x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,23x,"|")


    !write(stdout,*)"+------------------------------------------------------------------+"
    !write(stdout,*)"|                 P A R A M E T E R   R E P O R T                  |"
    !write(stdout,*)"+------------------------------------------------------------------+"
    call io_section(stdout,'parameter report')

    grp = 'CEL'

    !----------------------------------------! UNIT CELL !------------------------------------------!
    if (current_params%iprint.ge.1) then ! cell info is a must
       !write(stdout,*)"+"//repeat("=",(width-15)/2)//"  UNIT CELL  "//repeat("=",(width-16)/2)//"+"
       write(stdout,24)
       call io_subsection(stdout,'unit cell')
       write(stdout,23) grp
       write(stdout,'(T2,"|",T14,"Lattice (",a,")",T44,"Inverse Lattice (1/",a,")",T69,"| <-- ",a)')&
            & trim(current_params%out_len_unit),trim(current_params%out_len_unit),grp
       write(stdout,10) units_from_atomic(current_structure%cell(1,1),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(1,2),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(1,3),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%inv_cell(1,1),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(1,2),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(1,3),trim(out_invlen_unit)),&
            & grp

       write(stdout,10) units_from_atomic(current_structure%cell(2,1),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(2,2),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(2,3),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%inv_cell(2,1),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(2,2),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(2,3),trim(out_invlen_unit)),&
            & grp

       write(stdout,10) units_from_atomic(current_structure%cell(3,1),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(3,2),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%cell(3,3),trim(current_params%out_len_unit)),&
            & units_from_atomic(current_structure%inv_cell(3,1),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(3,2),trim(out_invlen_unit)),&
            & units_from_atomic(current_structure%inv_cell(3,3),trim(out_invlen_unit)),&
            & grp


       write(stdout,23) grp
       write(stdout,12) "Cell Volume =", units_from_atomic(current_structure%volume,trim(out_vol_unit)),trim(out_vol_unit),grp

       write(stdout,23) grp
       !write(stdout,*) "|                       Lattice Parameters (A)                     |"
       !write(stdout,*) "|                       ----------------------                     |"
       call io_heading(stdout,'Lattice Parameters ('//trim(current_params%out_len_unit)//')')
       write(stdout,11) 'a =',units_from_atomic(current_structure%lattice_a,trim(current_params%out_len_unit)),&
            & 'b =',units_from_atomic(current_structure%lattice_b,trim(current_params%out_len_unit)),&
            & 'c =', units_from_atomic(current_structure%lattice_c,trim(current_params%out_len_unit)),grp
       write(stdout,23) grp
       !write(stdout,*) "|                           Cell Angles (o)                        |"
       !write(stdout,*) "|                           ---------------                        |"
       call io_heading(stdout,'Cell Angles ('//'o'//')')
       write(stdout,11) 'alpha =',current_structure%alpha,'beta =',current_structure%beta,'gamma =', current_structure%gamma,grp
       write(stdout,23) grp
       write(stdout,23) grp
    end if


    !------------------------------------- GENERAL -------------------------------------------!
    !call io_section(stdout,"testing")
    !call io_subsection(stdout,"testing")
    grp = 'GEN'
    if (current_params%iprint.ge.1) then
       !write(stdout,*)"+"//repeat("=",(width-30)/2)//"   CALCULATION PARAMETERS   "//repeat("=",(width-30)/2)//"+"
       call io_subsection(stdout,'calculation parameters')
       write(stdout,23) grp
       !write(stdout,*)"|                              General                             |"
       !write(stdout,*)"|                              -------                             |"

       call io_heading(stdout,'General')
       write(stdout,15)"Parameters file",adjustr(trim(seed)//'.info'),GRP
       write(stdout,15)"Calculation type",adjustr(trim(current_params%task)),GRP
       write(stdout,15)"XC Functional",adjustr(trim(current_params%xc_functional)),GRP
       write(stdout,16)"Verbosity",current_params%iprint,GRP
       write(stdout,16)"Random seed", current_params%random_seed,GRP
       write(stdout,23) grp


       !write(stdout,*)"|                       Electronic Parameters                      |"
       !write(stdout,*)"|                       ---------------------                      |"
       grp = 'ELT'
       call io_heading(stdout,'Electronic Parameters')
       write(stdout,16)"Number of electrons",current_params%n_electrons,grp
       write(stdout,16)"Number of conduction bands",current_params%conduction_bands,grp
       write(stdout,17)"Electronic temperature (K)",current_params%electronic_temp,grp
       write(stdout,23) grp
       !write(stdout,*)"|                        Potential Parameters                      |"
       !write(stdout,*)"|                        --------------------                      |"
       grp = 'POT'
       call io_heading(stdout,'Potential Parameters')
       select case(trim(current_params%external_pot))
       case('jelly')
          write(stdout,15)"External potential",adjustr('Jellium'),grp
       case('finite_barrier')
          write(stdout,15)"External potential",adjustr('Finite Barrier'),grp
          write(stdout,22)"Frac. barrier widths",current_params%finite_barrier_width,grp
          write(stdout,17)"Barrier height ("//trim(current_params%out_energy_unit)//")",&
               & units_from_atomic(current_params%finite_barrier_height,trim(current_params%out_energy_unit)),grp
       case('periodic_pot')
          write(stdout,15)"External potential",adjustr('Periodic Potential'),grp
          write(stdout,21)"Periodic grid",current_params%periodic_pot_grid,grp
          write(stdout,17)"Potential amplitude  ("//trim(current_params%out_energy_unit)//")",&
               & units_from_atomic(current_params%periodic_pot_amp,trim(current_params%out_energy_unit)),grp
       case('lorentzian_pot')
          write(stdout,15)"External potential",adjustr('Local Lorentzians'),grp
          if (current_params%iprint.gt.1)then
             write(stdout,23)grp
             write(stdout,126)repeat('=',56),grp
             write(stdout,124)trim(current_params%out_energy_unit),trim(current_params%out_len_unit),grp
             write(stdout,126)repeat('=',56),grp

             do i=1,current_structure%n_pots
                write(stdout,125)i , current_structure%pot_centres(1,i),&
                     & current_structure%pot_centres(2,i),&
                     & current_structure%pot_centres(3,i),&
                     & units_from_atomic(current_structure%pot_heights(i),trim(current_params%out_energy_unit)),&
                     & units_from_atomic(current_structure%pot_widths(i),trim(current_params%out_len_unit)),grp
             end do
             write(stdout,126)repeat('=',56),grp
          end if

       case('gaussian_pot')
          write(stdout,15)"External potential",adjustr('Local Gaussians'),grp
          if (current_params%iprint.gt.1)then
             write(stdout,23)grp
             write(stdout,126)repeat('=',56),grp
             write(stdout,124)trim(current_params%out_energy_unit),trim(current_params%out_len_unit),grp
             write(stdout,126)repeat('=',56),grp

             do i=1,current_structure%n_pots
                write(stdout,125)i , current_structure%pot_centres(1,i),&
                     & current_structure%pot_centres(2,i),&
                     & current_structure%pot_centres(3,i),&
                     & units_from_atomic(current_structure%pot_heights(i),trim(current_params%out_energy_unit)),&
                     & units_from_atomic(current_structure%pot_widths(i),trim(current_params%out_len_unit)),grp
             end do
             write(stdout,126)repeat('=',56),grp
          end if

       case default
          write(stdout,15)"External potential",adjustr('Custom Potential'),grp
          write(stdout,15)"Potential file",adjustr(trim(current_params%external_pot)),grp
       end select

       write(stdout,23) grp
       !write(stdout,*)"|                           SCF Parameters                         |"
       !write(stdout,*)"|                           --------------                         |"
       grp='MIN'
       call io_heading(stdout,'Minimisation Parameters')
       write(stdout,16)"Maximum SCF steps",current_params%max_scf,grp
       write(stdout,15)"SCF method",adjustr(trim(current_params%scf_method)),grp
       write(stdout,18)"SCF convergence tolerance ("//trim(current_params%out_energy_unit)//")",&
            &units_from_atomic(current_params%energy_tol,trim(current_params%out_energy_unit)),grp
       write(stdout,23) grp
       !write(stdout,*)"|                           I/O Parameters                         |"
       !write(stdout,*)"|                           --------------                         |"
       grp='I/O'
       call io_heading(stdout,'I/O Parameters')
       write(stdout,19)"Write wavefunction",io_print_logical(current_params%write_wvfn),grp
       write(stdout,19)"Write continuation file",io_print_logical(current_params%write_state),grp
       write(stdout,19)"Write density",io_print_logical(current_params%write_density),grp
       write(stdout,19)"Write formatted density",io_print_logical(current_params%write_formatted_density),grp
       write(stdout,19)"Write potential",io_print_logical(current_params%write_potential),grp
       write(stdout,19)"Write formatted potential",io_print_logical(current_params%write_formatted_potential),grp
       write(stdout,19)"Write external potential",io_print_logical(current_params%write_potex),grp
       write(stdout,19)"Write formatted ext. potential",io_print_logical(current_params%write_formatted_potex),grp
       write(stdout,19)"Write electronic spectrum",io_print_logical(current_params%write_spec),grp
       write(stdout,19)"Calculate memory",io_print_logical(current_params%write_memory),grp

       write(stdout,23) grp
       !write(stdout,*)"|                                UNITS                             |"
       !write(stdout,*)"|                                -----                             |"
       if (current_params%iprint.gt.2)then
          grp='UNT'
          call io_heading(stdout,'Units')
          write(stdout,15)"Energy unit (in|out)",trim(current_params%unit_energy)//' | '//trim(current_params%out_energy_unit),grp
          write(stdout,15)"Length unit (in|out)",trim(current_params%unit_length)//' | '//trim(current_params%out_len_unit),grp
          write(stdout,15)"E-field unit (in|out)",trim(current_params%unit_efield)//' | '//trim(current_params%out_efield_unit),grp
          write(stdout,15)"B-field unit (in|out)",trim(current_params%unit_bfield)//' | '//trim(current_params%out_bfield_unit),grp
       end if
       !write(stdout,*)"|                         Advanced Parameters                      |"
       !write(stdout,*)"|                         -------------------                      |"
       grp='ADV'
       call io_heading(stdout,'Advanced Parameters')
       write(stdout,19)"Spin orbit coupling",io_print_logical(current_params%soc),grp
       write(stdout,22)"External E-field ("//trim(current_params%out_efield_unit)//')',&
            & units_from_atomic(current_params%ext_Efield(1),current_params%unit_efield),&
            & units_from_atomic(current_params%ext_Efield(2),current_params%unit_efield),&
            & units_from_atomic(current_params%ext_Efield(3),current_params%unit_efield),&
            & grp

       write(stdout,22)"External B-field ("//trim(current_params%out_bfield_unit)//')',&
            & units_from_atomic(current_params%ext_Bfield(1),current_params%unit_bfield),&
            & units_from_atomic(current_params%ext_Bfield(2),current_params%unit_bfield),&
            & units_from_atomic(current_params%ext_Bfield(3),current_params%unit_bfield),&
            & grp
       write(stdout,23) grp

       if (current_params%iprint.ge.2)then ! this is verbose

          ! write(stdout,*)"|                         Basis Set Parameters                     |"
          ! write(stdout,*)"|                         --------------------                     |"
          grp = 'BAS'
          call io_heading(stdout,'Basis Set Parameters')
          write(stdout,17)"Plane wave cut off ("//trim(current_params%out_energy_unit)//")",&
               units_from_atomic(current_params%cut_off_energy,trim(current_params%out_energy_unit)),grp
          write(stdout,17)"G vector fine scale",current_params%g_fine_scale,grp
          write(stdout,16)"Number of standard grid points",num_grid_points,grp
          write(stdout,16)"Number of fine grid points",num_fine_grid_points,grp
          write(stdout,21)"Standard FFT grid",ngx,ngy,ngz,grp
          write(stdout,21)"Fine FFT grid",fine_ngx,fine_ngy,fine_ngz,grp
          write(stdout,23) grp
       end if



       !write(stdout,*)"|                          Comms Parameters                        |"
       !write(stdout,*)"|                          ----------------                        |"
       if (comms_arch.eq."MPI")then
          grp='COM'
          call io_heading(stdout,'Communication Parameters')
          write(stdout,16)"Number of processes",nprocs,grp
          write(stdout,16)"Distributed by kpoint",dist_kpt,grp
          write(stdout,16)"Distributed by g vector",dist_gvec,grp

          write(stdout,23) grp
       end if
       !write(stdout,*)"|                       Brillouin Zone Sampling                    |"
       !write(stdout,*)"|                       -----------------------                    |"
       grp='KPT'
       call io_subsection(stdout,'brillouin zone sampling')
       write(stdout,23) grp
       write(stdout,21)"SCF MP kpoint grid",&
            & current_params%kpt_mp_grid(1),current_params%kpt_mp_grid(2),current_params%kpt_mp_grid(3),grp
       write(stdout,16)"Number kpoints",&
            & current_params%kpt_mp_grid(1)*current_params%kpt_mp_grid(2)*current_params%kpt_mp_grid(3),grp




       if (current_params%iprint.ge.3)then

          !write(stdout,*)"+"//repeat("=",(width-30)/2)//"       K-POINT REPORT       "//repeat("=",(width-30)/2)//"+"
          call io_heading(stdout,'K-Point Report')
          write(stdout,23)grp
          write(stdout,226)repeat('=',56),grp ! line
          write(stdout,224)grp ! heading 
          write(stdout,226)repeat('=',56),grp ! line

          !write(stdout,*)"|                                                                  |"
          !write(stdout,*)"|         Number                      Fractional Coordinate        | <-- ",grp
          !write(stdout,*)"+------------------------------------------------------------------+ <-- ",grp
          do i=1,current_structure%num_kpoints
             write(stdout,225) i,current_structure%kpt_scf_list(i,1),&
                  & current_structure%kpt_scf_list(i,2),&
                  & current_structure%kpt_scf_list(i,3),grp
          end do


          write(stdout,226)repeat('=',56),grp

       end if
       write(stdout,*)"+"//repeat("=",66)//"+ <-- ",grp
       write(stdout,*)



    end if
    !call io_flush(stdout)


23  format(T2,'|',T69,'| <-- ',a)
24  format(T2,'|',T69,'|')
15  format(T2,'|',1x,a,T36,":",T38,a30,1x,'| <-- ',a)   ! Character
16  format(T2,'|',1x,a,T36,":",T56,i12,1x,'| <-- ',a)   ! Integer
17  format(T2,'|',1x,a,T36,":",T56,f12.3,1x,'| <-- ',a)   ! Real
18  format(T2,'|',1x,a,T36,":",T56,ES12.2,1x,'| <-- ',a)   ! Science
19  format(T2,'|',1x,a,T36,":",T56,A12,1x,'| <-- ',a)   ! Logical
21  format(T2,'|',1x,a,T36,":",T54,3(i4,1x),'| <-- ',a)   ! Integer vec
22  format(T2,'|',1x,a,T36,":",T51,3(f5.3,1x),'| <-- ',a)   ! Real vec

    ! The potentials
125 format(T2,'|',T7,'|',T11,i3,5x,3(f5.3,1x),T42,2(f5.3,8x),T64,'|',T69,'| <-- ',a) ! The numbers
124 format(T2,'|',T7,'|',T9,'Pot ID',4x,'Positions (frac)',T40,'Height (',a,')',2x, 'Width (',a,')',T64,'|',T69,'| <-- ',a)  ! Headers
126 format(T2,"|",T7,"+",a,T64,"+",T69,"| <-- ",a)  ! The borders

    ! The kpoints
225 format(T2,'|',T7,'|',T11,i3,T35,3(f7.3,1x),T64,'|',T69,'| <-- ',a) ! The numbers
224 format(T2,'|',T7,'|',T9,'Kpt ID',24x,'Positions (frac)',T64,'|',T69,'| <-- ',a)  ! Headers
226 format(T2,"|",T7,"+",a,T64,"+",T69,"| <-- ",a)  ! The borders

12  format(T2,'|',18x,a14,2x,g14.6,2x,a,T69,'| <-- ',a)
11  format(T2'|',7x,3(a7,1x,g9.3,1x),T69,'| <-- ',a)
10  format(1x,'|',4x,3(g9.3,1x), 2x, 3(g9.3,1x),T69,'| <-- ',a)
31  format(T2,"|",T12,i4,T35,3(f9.6,1x),T69,"| <-- ",a)

    call trace_exit("io_write_params")
    return
  end subroutine io_write_params


!!$  subroutine io_print_kpt()
  !==============================================================================!
  !                           I O _ P R I N T _ K P T                            !
  !==============================================================================!
  ! Defunct                                                                      !
  !------------------------------------------------------------------------------!
  ! Arguments:                                                                   !
  !           None                                                               !
  !------------------------------------------------------------------------------!
  ! Author:   Z. Hawkhead  31/03/2024                                            !
  !==============================================================================!
!!$    implicit none
!!$    character(50)   :: sec_title
!!$    integer         :: width=69,length,i
!!$    ! Stuff for getting run time
!!$    character(len=3),dimension(12)  :: months
!!$    integer                         :: d_t(8)
!!$    character*10                    :: b(3)
!!$
!!$    character(len=:), allocatable :: string
!!$
!!$    call trace_entry('io_print_kpt')
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! BZ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    if (on_root_node)then
!!$       if (current_params%iprint.ge.1) then ! BZ is a little more verbose, so we will only do iprint 2
!!$          !write(stdout,*)"+"//repeat("-",(width-15)/2)//" BZ SAMPLING "//repeat("-",(width-16)/2)//"+"
!!$          !write(stdout,*)
!!$          write(stdout,*)
!!$          write(stdout,*) "        +-----------------------------------------------+ <-- KPT "
!!$          write(stdout,*) "        |  B R I L L O U I N  Z O N E  S A M P L I N G  | <-- KPT "
!!$          write(stdout,*) "        |                 K-point report                | <-- KPT "
!!$          write(stdout,*) "        +-----------------------------------------------+ <-- KPT "
!!$          write(stdout,13)"SCF MP kpoint grid :",&
!!$               & current_params%kpt_mp_grid(1),current_params%kpt_mp_grid(2),current_params%kpt_mp_grid(3)
!!$          write(stdout,131)"    Number kpoints :",&
!!$               & current_params%kpt_mp_grid(1)*current_params%kpt_mp_grid(2)*current_params%kpt_mp_grid(3)
!!$          if (current_params%iprint.ge.2)then
!!$             write(stdout,*) "        +-----------------------------------------------+ <-- KPT "
!!$             write(stdout,*) "        |    Number          Fractional co-ordinate     | <-- KPT "
!!$             write(stdout,*) "        +-----------------------------------------------+ <-- KPT "
!!$             do i=1,current_structure%num_kpoints
!!$                write(stdout,14) i,current_structure%kpt_scf_list(i,1),&
!!$                     & current_structure%kpt_scf_list(i,2),&
!!$                     & current_structure%kpt_scf_list(i,3)
!!$             end do
!!$
!!$          end if
!!$          write(stdout,*) "        +-----------------------------------------------+ <-- KPT"
!!$
!!$          write(stdout,*)
!!$       end if
!!$
!!$
!!$14     format(T10,"|",T16,i4,T31,3(f7.4,1x),T58,"| <-- KPT")
!!$13     format(T10,"|",T19,a,3x,3(i3,1x),T58,"| <-- KPT")
!!$131    format(T10,"|",T19,a,3x,i4,T58,"| <-- KPT")
!!$    end if
!!$    call trace_exit('io_print_kpt')
!!$
!!$  end subroutine io_print_kpt

  subroutine io_dist_kpt()
    !==============================================================================!
    !                            I O _ D I S T _ K P T                             !
    !==============================================================================!
    ! Subroutine for calculating which kpoints exist on each node                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    use comms, only : dist_kpt
    call trace_entry("io_dist_kpt")

    current_structure%max_kpoints_on_node =ceiling(real(current_structure%num_kpoints,dp)/real(dist_kpt,dp))
    call memory_deallocate(current_structure%kpts_on_node,'I')
    call memory_allocate(current_structure%kpts_on_node,1,current_structure%max_kpoints_on_node,'I')

    call trace_exit('io_dist_kpt')
    return
  end subroutine io_dist_kpt


  function io_inv(A) result(Ainv)
    real(dp), dimension(:,:), intent(in) :: A
    real(dp), dimension(size(A,1),size(A,2)) :: Ainv

    real(dp), dimension(size(A,1)) :: work  ! work array for LAPACK
    integer, dimension(size(A,1)) :: ipiv   ! pivot indices
    integer :: n, info
    call trace_entry('io_inv')
    ! External procedures defined in LAPACK

    ! Store A in Ainv to prevent it from being overwritten by LAPACK
    Ainv = A
    n = size(A,1)

    ! DGETRF computes an LU factorization of a general M-by-N matrix A
    ! using partial pivoting with row interchanges.
    call DGETRF(n, n, Ainv, n, ipiv, info)

    if (info /= 0) then
       call io_errors("Matrix invert: 'Matrix is numerically singular!'")
    end if

    call DGETRI(n, Ainv, n, ipiv, work, n, info)


    call trace_exit('io_inv')
  end function io_inv



  subroutine io_finalise()
    !==============================================================================!
    !                            I O _ F I N A L I S E                             !
    !==============================================================================!
    ! Subroutine for finishing up the io and printing timing information to the    !
    ! .derek file                                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    integer         :: width=69,length,i
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)
    character*10                    :: b(3)

    character(len=:), allocatable :: string
    call trace_entry('io_finalise')

    ! check the warning status
    if (warning_counter.gt.1)then
       write(stdout,'(a,i2,a)')"*** There were ",warning_counter,' warnings in this run, check carefully ***'
    elseif(warning_counter.eq.1)then
       write(stdout,'(a,i2,a)')"*** There was ",warning_counter,' warning in this run, check carefully ***'
    end if

    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']


    write(stdout,*)"+"//repeat("-",(width-15)/2)//" RUN FINISHED "//repeat("-",(width-18)/2)//"+"
    write(stdout,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(stdout,*)"+"//repeat("-",width-3)//"+"
1000 FORMAT(1x,"|",23x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,22x,"|")


    call trace_exit('io_finalise')
  end subroutine io_finalise



  function io_levenshtein(a,b) result(D)
    character(len=*)  :: a
    character(len=*)  :: b
    real(dp)          :: D

    integer,dimension(:,:),allocatable :: LD

    integer            :: len_a,len_b,i,j

    !call trace_entry('io_levenshtein')

    a=trim(adjustl(a))
    b=trim(adjustl(b))

    len_a=len_trim(trim(adjustl(a)))
    len_b=len_trim(trim(adjustl(b)))

    allocate(LD(0:len_a,0:len_b))
    LD(:,:)=0.0_dp
    ! Fill up the first column and row
    do i=0,len_a
       LD(i,0) = i
    end do
    do j=0,len_b
       LD(0,j) = j
    end do


    do i=1,len_a
       do j=1,len_b

          if (a(i:i).eq.b(j:j)) then
             LD(i,j)=min(LD(i-1,j)+1,LD(i,j-1)+1,LD(i-1,j-1))

          else
             LD(i,j)=min(LD(i-1,j)+1,LD(i,j-1)+1,LD(i-1,j-1)+1)

          end if
       end do
    end do

    D=LD(len_a,len_b)

    !print*,D
    !call trace_exit('io_levenshtein')
  end function io_levenshtein


  subroutine io_spell_check(test_str,min_ld,match)
    !==============================================================================!
    !                         I O _ S P E L L _ C H E C K                          !
    !==============================================================================!
    ! Subroutine for checking the spelling of an input                             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           test_str,          intent :: in                                    !
    !           min_ld,            intent :: out                                   !
    !           match,             intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!
    character(*),intent(in) :: test_str
    integer,intent(out)     :: min_ld
    character(*), intent(out) :: match

    real(dp),dimension(1:max_keys) :: dist
    real(dp)  :: min_real
    integer :: i,j,loc_arr(1),loc, num_mins

    call trace_entry('io_spell_check')

    call trace_entry('io_levenshtein')
    do i=1,max_keys
       dist(i)=io_levenshtein(trim(adjustl(test_str)),trim(adjustl(keys_array(i))))
    end do
    call trace_exit('io_levenshtein')


    min_ld=minval(dist)
    loc_arr=minloc(dist)
    loc=loc_arr(1)

    match=keys_array(loc)

    call trace_exit('io_spell_check')
  end  subroutine io_spell_check


  subroutine io_out_file_header(unit,type)
    !==============================================================================!
    !                     I O _ O U T _ F I L E _ H E A D E R                      !
    !==============================================================================!
    ! Subroutine for writing a smaller version of the DEREK header such that it    !
    ! can be used in output files.                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !           type,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!

    integer,intent(in)      :: unit
    character(*),intent(in) :: type

    character(52)           :: fmt
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8),width=50
    character*10                    :: b(3)

    character(len=:), allocatable :: string
    call trace_entry('io_out_file_header')

    ! 46 wide
    select case(type)
    case('P','p') ! potential file
       fmt="# |                 POTENTIAL FILE                 |"
    case('D','d')
       fmt="# |                  DENSITY FILE                  |"
    case default
       call io_errors(' Unknown file type '//trim(type))
    end select
    write(unit,*)"# +================================================+"
    write(unit,*)"# | ooooooo.  oooooooo oooooo.            ooo  ooo |"
    write(unit,*)"# | `88'  Y8b `88'  `8 `88   Y8   .0000.  `88 dP'  |"
    write(unit,*)"# |  88    88  88o8     88oo8P'  d8oooo8b  888[    |"
    write(unit,*)"# |  88   d88  88       88 `8b.  88    .o  88`8b.  |"
    write(unit,*)"# | o88bod8P  o88oood8 o88o `88o `YboodP' o88o o8o |"
    write(unit,*)"# |================================================|"
    write(unit,'(T2,"# |",T20,A, 1x, A,T53 ,"|")') 'Version', version
    write(unit,*)"# |------------------------------------------------|"
    write(unit,*)fmt
    write(unit,*)"# +------------------------------------------------+"
    write(unit,*)"# |        Author: Dr Z. Hawkhead (c) 2023         |"
    write(unit,*)"# +================================================+"

    !call io_sys_info(unit,.true.)


    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    write(unit,*)"# +------------------ WRITE DATE ------------------+"
    write(unit,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(unit,*)"# +------------------------------------------------+"
    write(unit,*)
1000 format(1x,"# |",14x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,13x,"|")


    write(unit,*)" LATTICE ("//trim(current_params%out_len_unit)//')'


    write(unit,10) units_from_atomic(current_structure%cell(1,1),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(1,2),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(1,3),trim(current_params%out_len_unit)),&
         'a =',units_from_atomic(current_structure%lattice_a,trim(current_params%out_len_unit))

    write(unit,10) units_from_atomic(current_structure%cell(2,1),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(2,2),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(2,3),trim(current_params%out_len_unit)),&
         'b =',units_from_atomic(current_structure%lattice_b,trim(current_params%out_len_unit))

    write(unit,10) units_from_atomic(current_structure%cell(3,1),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(3,2),trim(current_params%out_len_unit)),&
         & units_from_atomic(current_structure%cell(3,3),trim(current_params%out_len_unit)),&
         'c =',units_from_atomic(current_structure%lattice_c,trim(current_params%out_len_unit))

!!$    write(unit,10) current_structure%cell(1,:)*bohr_to_angstrom, 'a =',units_from_atomic(current_structure%lattice_a,trim(current_params%out_len_unit))
!!$    write(unit,10) current_structure%cell(2,:)*bohr_to_angstrom, 'b =',units_from_atomic(current_structure%lattice_b,trim(current_params%out_len_unit))
!!$    write(unit,10) current_structure%cell(3,:)*bohr_to_angstrom, 'c =',units_from_atomic(current_structure%lattice_c,trim(current_params%out_len_unit))
10  format(1x, 3(f10.7,1x), 3x, a,f10.7,1x)
    write(unit,*)

    call trace_exit('io_out_file_header')

  end subroutine io_out_file_header





  function io_print_logical(Bool_in) result(outstring)
    logical, intent(in) :: Bool_in
    character(3) :: outstring
    call trace_entry("io_print_logical")
    if (Bool_in) then
       outstring='on'
    else
       outstring='off'
    end if
    outstring=adjustr(outstring)
    call trace_exit("io_print_logical")
  end function io_print_logical

  subroutine io_warnings(message)
    !==============================================================================!
    !                            I O _ W A R N I N G S                             !
    !==============================================================================!
    ! Io routine for printing out a warning (and tracking warnings) if there is    !
    ! an issue with the calculation that doesnt result in a fatal crash.           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           message,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    implicit none
    character(*),optional   :: message
    call trace_entry('io_warnings')

    if (present(message))then
       write(stdout,'("**** Warning: ",a)') message
    end if
    warning_counter = warning_counter+1

    call trace_exit('io_warnings')
    return
  end subroutine io_warnings



  subroutine io_alphabetise(arr, size,mapping)
    !==============================================================================!
    !                         I O _ A L P H A B E T I S E                          !
    !==============================================================================!
    ! Routine for applying a sorting routine for alphabetising the list of         !
    ! available parameters                                                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           ar,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    character(len=*), dimension(:) :: arr
    integer, intent(in) :: size
    integer :: i, j
    character(len=len(arr(1))) :: temp
    character(len=len(arr(1))),dimension(1:size) :: arr_copy
    integer, dimension(:), intent(out) :: mapping
    mapping = [(i, i=1, size)]

    arr_copy = arr


    do i = 1, size - 1
       do j = i + 1, size
          if (arr_copy(i) > arr_copy(j)) then
             ! Swap elements
             temp = arr_copy(i)
             arr_copy(i) = arr_copy(j)
             arr_copy(j) = temp
          end if
       end do
    end do

    outer:do i = 1, size
       do j= 1, size

          if (arr(i) .eq. arr_copy(j))then
             mapping(j) = i

             cycle outer
          end if
       end do
    end do outer


    !do i = 1, size
    !   print*,i,arr(i),arr_copy(i)
    !end do

  end subroutine io_alphabetise

  subroutine io_alphabet_mapping(arr, size, mapping)
    !==============================================================================!
    !                    I O _ A L P H A B E T _ M A P P I N G                     !
    !==============================================================================!
    ! Routine for producing a map that corresponds to the alphabetical order of    !
    ! a list of parameters.                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           ar,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    character(len=*), dimension(:), intent(in) :: arr
    integer, intent(in) :: size
    integer, dimension(:), intent(out) :: mapping
    integer :: i, j

    ! Initialize the mapping array
    mapping = [(i, i=1, size)]

    do i = 1, size - 1
       do j = i + 1, size
          if (arr(mapping(i)) > arr(mapping(j))) then
             ! Swap indices in the mapping array
             mapping(i:j) = [mapping(j), mapping(i)]
          end if
       end do
    end do
  end subroutine io_alphabet_mapping



  subroutine io_section(unit,title,cat)
    !==============================================================================!
    !                             I O _ S E C T I O N                              !
    !==============================================================================!
    ! Small routine for generating a section header to be outputted to the         !
    ! specified file. Makes for uniform formatting in the output file              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !           title,             intent :: in                                    !
    !           cat,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    character(*),intent(in) :: title
    integer     ,intent(in) :: unit
    character(3),intent(in),optional :: cat
    character(len=100) :: new_title = ''
    character(len=100) :: line=''
    integer :: i,new_len
    integer :: left_pad,right_pad
    ! write the new title
    call trace_entry('io_section')
    if (on_root_node)then
       new_title = ''
       do i=1,len(trim(title))
          new_title(2*i-1:2*i-1) = io_case(title(i:i),.true.)
       end do
       new_len = len(trim(new_title))
       left_pad = (glob_line_len-2 - new_len)/2
       right_pad = (glob_line_len-2 - new_len)/2
       if (left_pad+right_pad+new_len .lt. glob_line_len - 2)then
          left_pad = left_pad+1
       end if

       write(line,'("|",a,a,a,T68,"|")')repeat(" ",left_pad),trim(new_title),repeat(" ",right_pad)

       if (present(cat))then

          write(unit,*)'+',repeat('=',glob_line_len-1),'+ <-- ',cat
          write(unit,*)trim(line),' <-- ',cat
          write(unit,*)'+',repeat('=',glob_line_len-1),'+ <-- ',cat
       else
          write(unit,*)'+',repeat('=',glob_line_len-1),'+'
          write(unit,*)trim(line)
          write(unit,*)'+',repeat('=',glob_line_len-1),'+'

       end if
    end if
    !call io_flush(unit)
    call trace_exit('io_section')
  end subroutine io_section

  subroutine io_subsection(unit,title)
    !==============================================================================!
    !                          I O _ S U B S E C T I O N                           !
    !==============================================================================!
    ! Small routine for generating a section header to be outputted to the         !
    ! specified file. Makes for uniform formatting in the output file              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !           title,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    character(*),intent(in) :: title
    integer     ,intent(in) :: unit

    character(len=100) :: new_title = ''
    character(len=100) :: line=''
    integer :: i,new_len
    integer :: left_pad,right_pad,pad = 3
    integer :: loc_line_len  = glob_line_len -1
    call trace_entry('io_subsection')
    if (on_root_node)then
       new_title = io_case(title,.true.)
       new_len = len(trim(new_title)) + 2*pad

       left_pad = (loc_line_len - new_len)/2
       right_pad = (loc_line_len - new_len)/2

       if (left_pad+right_pad+new_len .lt. loc_line_len)then
          left_pad = left_pad+1
       end if

       write(unit,*)'+',repeat('=',left_pad),repeat(' ',pad),trim(new_title),repeat(' ',pad),repeat('=',right_pad) , '+ <-- ',grp
       !call io_flush(unit)
    end if
    call trace_exit('io_subsection')
  end subroutine io_subsection

  subroutine io_heading(unit,title,override)
    character(*),intent(in) :: title
    integer     ,intent(in) :: unit

    character(len=100) :: new_title = ''
    character(len=100) :: line=''
    integer :: i,new_len
    integer :: left_pad,right_pad,pad = 0
    integer :: loc_line_len  = glob_line_len -1
    integer,optional :: override
    call trace_entry('io_heading')
    if (on_root_node)then
       new_title = title
       new_len = len(trim(new_title)) + 2*pad

       left_pad = (loc_line_len - new_len)/2
       right_pad = (loc_line_len - new_len)/2

       if (left_pad+right_pad+new_len .lt. loc_line_len)then
          left_pad = left_pad+1
       end if

       if (present(override))then
          right_pad=right_pad+override
       end if

       write(unit,*)'|',repeat(' ',left_pad+pad),trim(new_title),repeat(' ',right_pad+pad) , '| <-- ',grp
       write(unit,*)'|',repeat(' ',left_pad+pad),repeat('-',new_len),repeat(' ',right_pad+pad) , '| <-- ',grp

       !call io_flush(unit)
    end if
    call trace_exit('io_heading')

  end subroutine io_heading
  subroutine io_mem_report()
    !==============================================================================!
    !                          M E M O R Y _ R E P O R T                           !
    !==============================================================================!
    ! Subroutine for reporting the memory requirements to the main .derek file     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           stdout,            intent :: in                                    !
    !           check_mem,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!
    use comms, only : on_root_node,comms_reduce,rank

    real(dp) :: byte_to_giga=1.0_dp/(1024.0_dp)**3
    real(dp) :: byte_to_mega=1.0_dp/(1024.0_dp)**2
    real(dp) :: byte_to_kilo=1.0_dp/(1024.0_dp)
    real(dp) :: tot_max
    real(dp) :: io_mem_real,wave_mem_real,basis_mem_real,gen_mem_real,pot_mem_real,den_mem_real,tot_mem_real
    call trace_entry("io_mem_report")

    io_mem_real = real(io_memory,dp)
    pot_mem_real= real(pot_memory,dp)
    den_mem_real= real(den_memory,dp)
    basis_mem_real= real(basis_memory,dp)
    gen_mem_real= real(gen_memory,dp)
    tot_mem_real= real(tot_memory,dp)
    wave_mem_real= real(wave_memory,dp)



    tot_max=tot_mem_real
    ! We will need a comms gather in here but I've not written it yet...

    !print*,rank,'before comms io_mem'
    call comms_reduce(io_mem_real,1,"sum")

    call comms_reduce(basis_mem_real,1,"sum")

    call comms_reduce(pot_mem_real,1,"sum")

    call comms_reduce(wave_mem_real,1,"sum")

    call comms_reduce(den_mem_real,1,"sum")

    call comms_reduce(gen_mem_real,1,"sum")

    call comms_reduce(tot_mem_real,1,"sum")

    call comms_reduce(tot_max,1,'max')


    if (on_root_node)then

       !write(stdout,*)"        +-----------------------------------------------+ <-- MEM"
       !write(stdout,*)"        |        M E M O R Y   E S T I M A T E S        | <-- MEM"
       !write(stdout,*)"        +-----------------------------------------------+ <-- MEM"
       write(stdout,*)
       call io_section(stdout,'memory estimates','MEM')
       if (current_params%iprint.ge.2)then
          if (io_mem_real.le.1.0e3_dp)then
             write(stdout,17)"IO requirements",io_mem_real,'B'
          elseif(io_mem_real.le.1.0e6_dp.and.io_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"IO requirements",io_mem_real*byte_to_kilo,'KB'
          elseif(io_mem_real.le.1.0e9_dp.and.io_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"IO requirements",io_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"IO requirements",io_mem_real*byte_to_giga,'GB'
          end if


          if (basis_mem_real.le.1.0e3_dp)then
             write(stdout,17)"Basis requirements",basis_mem_real,'B'
          elseif(basis_mem_real.le.1.0e6_dp.and.basis_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"Basis requirements",basis_mem_real*byte_to_kilo,'KB'
          elseif(basis_mem_real.le.1.0e9_dp.and.basis_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"Basis requirements",basis_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"Basis requirements",basis_mem_real*byte_to_giga,'GB'
          end if


          if (pot_mem_real.le.1.0e3_dp)then
             write(stdout,17)"Potential requirements",pot_mem_real,'B'
          elseif(pot_mem_real.le.1.0e6_dp.and.pot_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"Potential requirements",pot_mem_real*byte_to_kilo,'KB'
          elseif(pot_mem_real.le.1.0e9_dp.and.pot_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"Potential requirements",pot_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"Potential requirements",pot_mem_real*byte_to_giga,'GB'
          end if


          if (wave_mem_real.le.1.0e3_dp)then
             write(stdout,17)"Wavefunction requirements",wave_mem_real,'B'
          elseif(wave_mem_real.le.1.0e6_dp.and.wave_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"Wavefunction requirements",wave_mem_real*byte_to_kilo,'KB'
          elseif(wave_mem_real.le.1.0e9_dp.and.wave_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"Wavefunction requirements",wave_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"Wavefunction requirements",wave_mem_real*byte_to_giga,'GB'
          end if

          if (den_mem_real.le.1.0e3_dp)then
             write(stdout,17)"Density requirements",den_mem_real,'B'
          elseif(den_mem_real.le.1.0e6_dp.and.den_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"Density requirements",den_mem_real*byte_to_kilo,'KB'
          elseif(den_mem_real.le.1.0e9_dp.and.den_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"Density requirements",den_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"Density requirements",den_mem_real*byte_to_giga,'GB'
          end if

          if (gen_mem_real.le.1.0e3_dp)then
             write(stdout,17)"General requirements",gen_mem_real,'B'
          elseif(gen_mem_real.le.1.0e6_dp.and.gen_mem_real.gt.1.0e3_dp)then
             write(stdout,17)"General requirements",gen_mem_real*byte_to_kilo,'KB'
          elseif(gen_mem_real.le.1.0e9_dp.and.gen_mem_real.gt.1.0e6_dp)then
             write(stdout,17)"General requirements",gen_mem_real*byte_to_mega,'MB'
          else
             write(stdout,17)"General requirements",gen_mem_real*byte_to_giga,'GB'
          end if

          write(stdout,*)"|                                                  =============== | <-- MEM"
       end if
       if (tot_mem_real.le.1.0e3_dp)then
          write(stdout,17)"Total requirements",tot_mem_real,'B'
       elseif(tot_mem_real.le.1.0e6_dp.and.tot_mem_real.gt.1.0e3_dp)then
          write(stdout,17)"Total requirements",tot_mem_real*byte_to_kilo,'KB'
       elseif(tot_mem_real.le.1.0e9_dp.and.tot_mem_real.gt.1.0e6_dp)then
          write(stdout,17)"Total requirements",tot_mem_real*byte_to_mega,'MB'
       else
          write(stdout,17)"Total requirements",tot_mem_real*byte_to_giga,'GB'
       end if

       write(stdout,*)"+------------------------------------------------------------------+ <-- MEM"
       if (nprocs.gt.1)then
          if (tot_max.le.1.0e3_dp)then
             write(stdout,17)"Maximum memory per process",tot_max,'B'
          elseif(tot_max.le.1.0e6_dp.and.tot_max.gt.1.0e3_dp)then
             write(stdout,17)"Maximum memory per process",tot_max*byte_to_kilo,'KB'
          elseif(tot_max.le.1.0e9_dp.and.tot_max.gt.1.0e6_dp)then
             write(stdout,17)"Maximum memory per process",tot_max*byte_to_mega,'MB'
          else
             write(stdout,17)"Maximum memory per process",tot_max*byte_to_giga,'GB'
          end if


          write(stdout,*)"+------------------------------------------------------------------+ <-- MEM"


       end if

!!$       write(stdout,1)"Basis requirements" ,basis_memory*byte_to_mega,'MB'
!!$       write(stdout,1)"Potential requirements" ,pot_memory*byte_to_mega,'MB'
!!$       write(stdout,1)"Wavefunction requirements" ,wave_memory*byte_to_mega,'MB'
!!$       write(stdout,1)"General requirements" ,gen_memory*byte_to_mega,'MB'
!!$       write(stdout,*)"    +                                      ================== +"
!!$       write(stdout,1)"Total memory requirements",tot_memory*byte_to_mega
!!$       write(stdout,*)"    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    end if

1   format(T10,"|",1x,a,T38,":",3x,f10.3,2x,a2,T58,"| <-- MEM")

23  format(T2,'|',T69,'| <-- ',a)
15  format(T2,'|',1x,a,T36,":",T38,a30,1x,'| <-- ',a)   ! Character
16  format(T2,'|',1x,a,T36,":",T56,i12,1x,'| <-- ',a)   ! Integer
17  format(T2,'|',1x,a,T36,":",T52,f12.3,2x,a2,T69,'| <-- MEM')   ! Real
18  format(T2,'|',1x,a,T36,":",T56,ES12.2,1x,'| <-- ',a)   ! Science
19  format(T2,'|',1x,a,T36,":",T56,A12,1x,'| <-- ',a)   ! Logical
21  format(T2,'|',1x,a,T36,":",T54,3(i4,1x),'| <-- ',a)   ! Integer vec
22  format(T2,'|',1x,a,T36,":",T51,3(f5.3,1x),'| <-- ',a)   ! Real vec

12  format(T2,'|',20x,a14,2x,f12.6,2x,"A**3",T69,'| <-- ',a)
11  format(T2'|',10x,3(a7,1x,f7.4,1x),T69,'| <-- ',a)
10  format(1x,'|', 3(f9.6,1x), 6x, 3(f9.6,1x),T69,'| <-- ',a)
31  format(T2,"|",T12,i4,T35,3(f9.6,1x),T69,"| <-- ",a)
    !call io_flush(stdout)
    call trace_exit("io_mem_report")

  end subroutine io_mem_report

  subroutine io_open_fmt(unit,file,access,action,status)
    integer, intent(inout)              :: unit
    character(*),intent(in)             :: file
    character(*),intent(in),   optional :: access
    character(*),intent(in),   optional :: action
    character(*),intent(in),   optional :: status


    ! Internal
    integer       :: stat
    character(25) :: loc_access,loc_action,loc_status
    logical       :: is_open
    call trace_entry('io_open_fmt')


    ! Close if opened
    inquire(unit,opened=is_open)
    if (is_open)then
       call io_close(unit)
    end if

    ! Check for defaults
    if (present(access))then
       loc_access = access
    else
       loc_access = 'stream'
    end if

    if (present(status))then
       loc_status = status
    else
       loc_status = 'unknown'
    end if


    if (present(action))then
       loc_action = action
    else
       loc_action = 'readwrite'
    end if
    
    
    ! check the flags are alright

    select case(trim(io_case(loc_access)))
    case('append')
    case('stream')
    case default
       call io_errors('Unknown file access type: '//access)
    end select
    

    select case(trim(io_case(loc_status)))
    case('old')
    case('new')
    case('unknown')
    case('replace')
    case('scratch')
    case default
       call io_errors('Unknown file status type: '//status)
    end select
    
    select case(trim(io_case(loc_action)))
    case('read')
    case('write')
    case('readwrite')
    case default
       call io_errors('Unknown file action type: '//action)
    end select

    open(newunit=unit,file=file,form="FORMATTED",access=loc_access,status=loc_status,action=loc_action)
    !if (stat.ne.0)call io_errors('Unable to open file: '//trim(file))


    call trace_exit('io_open_fmt')
  end subroutine io_open_fmt



  subroutine io_close(unit)
    implicit none
    integer, intent(in) :: unit

    logical :: is_open
    integer :: iostat
    character(len=100) :: errmsg
    call trace_entry('io_close')
    ! Check if the unit is open
    inquire(unit=unit, opened=is_open)
    if (is_open) then       
       close(unit, iostat=iostat)
    end if
    call trace_exit('io_close')
  end subroutine io_close

end module io










