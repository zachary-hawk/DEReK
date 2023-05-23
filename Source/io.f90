module io
  !Impose strong typing
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise,dp
  use comms,only : rank, nprocs,comms_arch,on_root_node,max_version_length,COMMS_FINALISE&
       &,comms_library_version,comms_version
  use memory, only : memory_init,memory_allocate,memory_deallocate
  use iso_fortran_env, only :real64,compiler_version
  implicit none

  real(dp),          public,parameter      :: pi=3.1415926535
  real(dp),          public,parameter      :: twopi=2.0_dp*pi
  real(dp),          public,parameter      :: fourpi=4.0_dp*pi
  real(dp),          public,parameter      :: small_number= 1e-15
  logical,           public                :: file_exists
  character(20),     public                :: seed
  integer,           public                :: stdout


  character(100),dimension(:),allocatable  :: present_array

  character(100),dimension(:),allocatable  :: keys_array
  character(100),dimension(:),allocatable  :: keys_description
  character(100),dimension(:),allocatable  :: keys_default
  character(100),dimension(:),allocatable  :: keys_allowed
  character(100),dimension(:),allocatable  :: keys_type

  character(100) :: version = "1.0.0"
  character(100) :: info = "Durham Electronic RElaxaction (K)code, DEReK"
  logical        :: read_params

  integer                                  :: max_params=1
  logical, private :: cell_declared=.false.

  ! Conversion factors

  ! Energies
  real(dp), parameter,public :: ev_to_hartree = 0.036749308136649_dp
  real(dp), parameter,public :: hartree_to_ev = 1.0_dp/ev_to_hartree


  !Lengths
  real(dp),parameter,public  :: angstrom_to_bohr = 1.8897259886_dp
  real(dp),parameter,public  :: bohr_to_angstrom = 1.0_dp/angstrom_to_bohr

  !Angles
  real(dp),parameter,public  :: rad_to_deg=180.0_dp/pi
  real(dp),parameter,public  :: deg_to_rad=pi/180.0_dp

  !Physical constants
  real(dp),parameter,public  :: hbar_si = 1.054571800E-34_dp
  real(dp),parameter,public  :: mu_b_si = 927.4009994E-26_dp
  real(dp),parameter,public  :: m_e_si  = 9.1093837015E-31_dp
  real(dp),parameter,public  :: kb_si   = 1.38
  real(dp),parameter,public  :: kB      = 8.617333262145E-5_dp*ev_to_hartree
  real(dp),parameter,public  :: mu_0_si = fourpi*1.0E-7_dp



  !Complex numbers
  complex(dp),parameter,public :: cmplx_0 = (0.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_1 = (1.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_i = (0.0_dp,1.0_dp)


  type  parameters
     ! %Begin: parameters

     !Calculation parameters
     logical :: dryrun = .false.
     logical :: debugging = .false.
     integer :: n_electrons =            1
     real(dp) :: energy_tol =    9.9999999999999995E-008_dp!*ev_to_hartree
     character(len=30) :: xc_functional = 'LDA'
     real(dp) :: cut_off_energy =    200.00000000000000!*ev_to_hartree
     real(dp) :: g_fine_scale =    2.0000000000000000
     logical :: write_wvfn = .true.
     character(len=30) :: task = 'singlepoint'
     logical :: soc = .false.
     integer :: max_scf =           40
     character(len=30) :: scf_method = 'EDFT'
     real(dp) :: electronic_temp =    0.000_dp
     integer :: conduction_bands =           20
     integer :: nbands
     logical :: write_density = .false.
     logical :: Write_potential = .false.
     logical :: write_cont = .true.
     character(len=30) :: external_pot = 'Jelly'
     integer,dimension(1:3) :: kpt_mp_grid = (/1,1,1/)
     logical :: write_spec = .true.
     logical :: calc_memory = .true.
     integer :: iprint =            1
     real(dp) :: finite_barrier_height =    10.000000000000000
     real(dp),dimension(1:3) :: finite_barrier_width =   (/0.5,0.5,0.5/)
     integer,dimension(1:3)  :: periodic_pot_grid = (/1,1,1/)
     real(dp) :: periodic_pot_amp =    10.000000000000000     
     ! %End: parameters
  end type parameters

  ! %Begin: keys

  character(len=30),parameter,public ::key_dryrun   = 'dryrun'
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
  character(len=30),parameter,public ::key_conduction_bands   = 'condunction_bands'
  character(len=30),parameter,public ::key_write_density   = 'write_density'
  character(len=30),parameter,public ::key_Write_potential   = 'write_potential'
  character(len=30),parameter,public ::key_write_cont   = 'write_cont'
  character(len=30),parameter,public ::key_external_pot   = 'external_pot'
  character(len=30),parameter,public ::key_kpt_mp_grid   = 'kpt_mp_grid'
  character(len=30),parameter,public ::key_write_spec   = 'write_spec'
  character(len=30),parameter,public ::key_calc_memory   = 'calc_memory'
  character(len=30),parameter,public ::key_iprint   = 'print_level'
  character(len=30),parameter,public ::key_finite_barrier_height   = 'finite_barrier_height'
  character(len=30),parameter,public ::key_finite_barrier_width   = 'finite_barrier_width'
  character(len=30),parameter,public ::key_periodic_pot_grid   = 'periodic_pot_grid'
  character(len=30),parameter,public ::key_periodic_pot_amp   = 'periodic_pot_amp'
  ! %End: keys




  integer,parameter::max_keys=          26
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
  end type structure



  type(parameters),public,save :: current_params
  type(structure) ,public,save :: current_structure


  external DGETRF


  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------!
  public :: io_initialise
  public :: io_finalise
  public :: io_errors

contains



  subroutine io_initialise()
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

    integer :: i ! counters
    character(10) :: line
    integer :: stat

    ! Some junk variables


    call trace_entry("io_initialise")
    call io_cl_parser() ! Read the commandline arguments

    call memory_init(seed)

    ! Get the length of the parameters file
    inquire(file=trim(seed)//'.info',exist=file_exists)

    if (file_exists)then
       open(unit=1,file=trim(seed)//'.info',iostat=stat,status="OLD",access="stream",form="formatted")
       do while (stat.eq.0)
          read(1,'(A60)',iostat=stat) line
          max_params=max_params+1
       end do
       close(1)
    end if
    max_params=max_params+1
    ! Allocate space for the params array
    allocate(present_array(1:max_params))
    do i=1,max_params
       write(present_array(i),*)i
    end do

    ! Open up the main file for the output
    open(stdout,file=trim(seed)//".derek",RECL=8192,form="FORMATTED",access="APPEND")

    call io_flush(stdout)
    call io_header()



    
    ! Fist things first, try to read paramteters
    call io_list_params(.false.)
    if (read_params) call io_read_param(current_params)
    if (.not.cell_declared) call io_errors("Error in I/O: No lattice provided")



    ! We have the lattice we can define params
    call io_lattice_to_abc()



    ! Check the units and convert
    current_params%cut_off_energy=current_params%cut_off_energy*ev_to_hartree
    current_params%energy_tol = current_params%energy_tol * ev_to_hartree
    current_params%finite_barrier_height=current_params%finite_barrier_height * ev_to_hartree
    current_params%periodic_pot_amp=current_params%periodic_pot_amp * ev_to_hartree
    ! Work out the new values of some on the fly parameters
    current_params%nbands=current_params%n_electrons+current_params%conduction_bands


    ! Write out the preliminary stuff to the .derek
    call io_write_params()


    call trace_exit("io_initialise")
    return
  end subroutine io_initialise


  subroutine io_read_param(dummy_params)
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


    !The boring stuff to make the whole shebang work
    integer           :: stat
    integer           :: read_stat=0
    integer           :: i,j,k           !counter

    character(len=60) :: line        ! charcter string into which each line is read, overwritten in loop
    character(len=30) :: key         ! the keyword used
    character(len=30) :: param       ! the value of the param
    character(len=30) :: match       ! spell check match
    logical           :: comment     ! boolean for comment line, will skip
    logical           :: spelling = .false. 
    real(dp)          :: real_dump   ! a dump for handling scientific

    integer :: lev_dist
    integer ::width=69

    call trace_entry("io_read_param")



    !Open the parameter file
    if (file_exists) then
       open(unit=1,file=trim(seed)//".info",iostat=stat,status="OLD",access="stream",form="formatted")


       if (stat.ne.0) call io_errors("Error in I/O: Open file '"//trim(seed)//".info'")
       ! now we can do the reading
       k=0
       do i=1,max_params
          !first thing, read new line into 'line' variable
          read(1,'(A)',iostat=read_stat) line


          !print*,trim(present_array(i))
          ! Check for blank line
          if (read_stat.ne.0)exit
          if (trim(line).eq."") cycle
          if (index(trim(line),'!').gt.0)cycle

          ! read the block data
          if (index(io_case(line),"%begin").gt.0)then
             call io_block_parse(line,stat)
             cycle
          end if

          !Read everying into a thing
          call io_freeform_read(line,key,param,comment)

          if (comment) cycle ! skip if comment
          !Do some trimming
          key=adjustl(trim(io_case(key)))
          param=adjustl(trim(io_case(param)))


          ! Check for spelling

          call io_spell_check(key,lev_dist,match)

          
          if (lev_dist.gt.0)then

             if (.not.spelling)then
                write(stdout,*)"********************************************************************"
                write(stdout,*)"*                         *** WARNING ***                          *"
                write(stdout,*)"*   TYPOS DETECTED IN INFO FILE, CONTINUING WITH AUTO SUGGESTION   *"
                write(stdout,*)"********************************************************************"
                write(stdout,*)"*  UNKNOWN PARAMETER        AUTOCORRECT       LEVENSHTEIN DISTANCE *"
                write(stdout,*)"********************************************************************"
                spelling=.true.
             end if
             write(stdout,9)trim(adjustl(key)),trim(adjustl(match)),lev_dist

             key = match
          end if
9         format(' *',T5,A,T30,A,T60,i3,T69,'*')

          ! %Begin: case_read
          select case(key)

          case(key_dryrun)
             read(param,*,iostat=stat) dummy_params%dryrun
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_debugging)
             read(param,*,iostat=stat) dummy_params%debugging
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_n_electrons)
             read(param,*,iostat=stat) dummy_params%n_electrons
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_energy_tol)
             read(param,*,iostat=stat) dummy_params%energy_tol
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             dummy_params%energy_tol=dummy_params%energy_tol
             present_array(i)=key
          case(key_xc_functional)
             read(param,*,iostat=stat) dummy_params%xc_functional
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_cut_off_energy)
             read(param,*,iostat=stat) dummy_params%cut_off_energy
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             dummy_params%cut_off_energy=dummy_params%cut_off_energy
          case(key_g_fine_scale)
             read(param,*,iostat=stat) dummy_params%g_fine_scale
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_wvfn)
             read(param,*,iostat=stat) dummy_params%write_wvfn
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_task)
             read(param,*,iostat=stat) dummy_params%task
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_soc)
             read(param,*,iostat=stat) dummy_params%soc
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_max_scf)
             read(param,*,iostat=stat) dummy_params%max_scf
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_scf_method)
             read(param,*,iostat=stat) dummy_params%scf_method
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_electronic_temp)
             read(param,*,iostat=stat) dummy_params%electronic_temp
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_conduction_bands)
             read(param,*,iostat=stat) dummy_params%conduction_bands
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_density)
             read(param,*,iostat=stat) dummy_params%write_density
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_Write_potential)
             read(param,*,iostat=stat) dummy_params%Write_potential
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_write_cont)
             read(param,*,iostat=stat) dummy_params%write_cont
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_external_pot)
             read(param,*,iostat=stat) dummy_params%external_pot
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_kpt_mp_grid)
             read(param,*,iostat=stat) dummy_params%kpt_mp_grid(1),dummy_params%kpt_mp_grid(2),dummy_params%kpt_mp_grid(3)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key

          case(key_write_spec)
             read(param,*,iostat=stat) dummy_params%write_spec
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_calc_memory)
             read(param,*,iostat=stat) dummy_params%calc_memory
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_iprint)
             read(param,*,iostat=stat) dummy_params%iprint
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_finite_barrier_height)
             read(param,*,iostat=stat) dummy_params%finite_barrier_height
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_finite_barrier_width)
             read(param,*,iostat=stat) dummy_params%finite_barrier_width(1),&
                  & dummy_params%finite_barrier_width(2),&
                  & dummy_params%finite_barrier_width(3)
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_periodic_pot_grid)
             read(param,*,iostat=stat) dummy_params%periodic_pot_grid
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
          case(key_periodic_pot_amp)
             read(param,*,iostat=stat) dummy_params%periodic_pot_amp
             if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)
             present_array(i)=key
             ! %End: case_read
          case default
             call io_errors("Error in I/O: Error parsing keyword: "//key)
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
             call io_errors("Error in I/O: Duplicate parameter found: "//present_array(i))
          end if
       end do
    end do
    close(1)

    ! Handle the kpoint list
    call io_kpoint_grid()

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
          call io_errors("Error in I/O: Error parsing line:  "//trim(line_unparsed))
       end if
    end do

    !print*,line_unparsed,key,val,com
    call trace_exit("io_freeform_read")
    return
  end subroutine io_freeform_read


  subroutine io_errors(message)
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
    implicit none
    character(*)       :: message

    ! internal variable for rank processing
    character(len=40)  :: file_name
    
    write(file_name,'(A,".",I0.4,".err")') trim(seed),rank

       
    
    open(2,file=trim(file_name),RECL=8192,status="UNKNOWN")
    write(*,*)"Error: called io_abort"
    write(2,*) message

    call trace_stack(2,rank,seed=seed)
    stop
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
          do i = 1,len(string)
             k = iachar(string(i:i))
             if ( k >= iachar('a') .and. k <= iachar('z') ) then
                k = k + iachar('A') - iachar('a')
                new(i:i) = achar(k)
             end if
          end do
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
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num
    character(len=100):: compile_version,cpuinfo
    character(len=5) :: opt

    nargs=command_argument_count()

    
    if (nargs.eq.0)then
       seed='derek'
       call io_errors("Error in command line parser: no seed provided.")
    end if


    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(min_mpi,maj_mpi)

       write(mpi_version_num,97)min_mpi,maj_mpi
97     format(i1,"."i1)
       min_char=scan(mpi_c_version,".")
       !print*, mpi_c_version,mpi_version_num
    end if

#ifdef __INTEL_COMPILER
#define compiler "Intel Compiler"

#endif
#ifdef __GFORTRAN__
#define compiler "GNU Fortran"
    !#define compile_version __VERSION__
#endif

#define opt opt_strat
    compile_version=compiler_version()
    if (compiler.eq."Intel Compiler")then
       compile_version=compiler_version()

       compile_version=trim(compile_version(87:97))
    end if


    if (nargs.gt.0)then
       do arg_index=1,nargs

          call get_command_argument(arg_index,name)
          select case(adjustl(trim(name)))
          case("-h","--help")
             write(*,*) trim(version)
             write(*,*) trim(info)
             read_params=.false.
             call io_list_params(.false.)
             help=.true.
             if (arg_index.eq.nargs) call io_help
          case("-s","--search")
             write(*,*) trim(version)
             write(*,*) trim(info)
             write(*,*)
             read_params=.false.
             call io_list_params(.false.)
             search=.true.
             if (arg_index.eq.nargs) call io_help

          case("-v")
             write(*,*) trim(version)
             write(*,*) trim(info)
             write(*,*) "Compiled with ",compiler," ",Trim(compile_version), " on ", __DATE__, " at ",__TIME__
             write(*,*) "Communications architechture: ",comms_arch
             if (comms_arch.eq."MPI")then
                write(*,*) "MPI Version: ",mpi_c_version(1:min_char+1)
             end if
             write(*,*) "Optimisation Strategy: ",opt
             write(*,*)
             read_params=.false.
             stop
          case("-d","--dryrun")
             current_params%dryrun=.true.
          case("-l","--list")
             write(*,*) trim(version)
             write(*,*) trim(info)
             read_params=.false.
             call io_list_params(.true.)
             stop
          case default
             if (help)then
                call io_help(name)
                help=.false.
                stop
             elseif(search)then
                call io_search(io_case(name))
                search=.false.
                stop
             else
                seed=name
                read_params=.true.
             end if

          end select
       end do
    else
       write(*,*) trim(version)
       write(*,*) trim(info)
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
30     format(4x,A,T40,A)
       write(*,30) "-v","Print version information."
       write(*,30) "-h,--help   <keyword>","Get help and commandline options."
       write(*,30) "-s,--search <keyword>", "Search list of available parameters"
       write(*,30) "-l,--list","Get list of parameters avilable for the user."
       write(*,30) "-d,--dryrun","Run calculation to check input files"
    end if
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

  subroutine io_list_params(print_flag)
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

    character(60) :: junk
    integer       :: i ! loops
    ! Allocate all the arrays for the parameters
    allocate(keys_array(1:max_keys))
    allocate(keys_default(1:max_keys))
    allocate(keys_description(1:max_keys))
    allocate(keys_allowed(1:max_keys))




    ! assign the keys
    ! %Begin: assign_keys

    keys_array(1)=trim(key_dryrun)
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
    keys_array(17)=trim(key_write_cont)
    keys_array(18)=trim(key_external_pot)
    keys_array(19)=trim(key_kpt_mp_grid)
    keys_array(20)=trim(key_write_spec)
    keys_array(21)=trim(key_calc_memory)
    keys_array(22)=trim(key_iprint)
    keys_array(23)=trim(key_finite_barrier_height)
    keys_array(24)=trim(key_finite_barrier_width)
    keys_array(25)=trim(key_periodic_pot_grid)
    keys_array(26)=trim(key_periodic_pot_amp)
    ! %End: assign_keys

    ! %Begin: assign_default

    write(junk,*)current_params%dryrun
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
    write(junk,*)current_params%write_cont
    keys_default(17)=trim(adjustl(junk))
    write(junk,*)current_params%external_pot
    keys_default(18)=trim(adjustl(junk))
    write(junk,*)current_params%kpt_mp_grid
    keys_default(19)=trim(adjustl(junk))
    write(junk,*)current_params%write_spec
    keys_default(20)=trim(adjustl(junk))
    write(junk,*)current_params%calc_memory
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
    ! %End: assign_default

    ! %Begin: assign_description

    keys_description(1)='A dryrun allows the calculation to be &
         & initialised to check the input files without starting any intensive calculations'
    keys_description(2)='Turn on profilling of the code'
    keys_description(3)='Number of electrons in the calculation'
    keys_description(4)='Energy convergence tolerance of the SCF cycle'
    keys_description(5)='LDA'
    keys_description(6)='Maximum plane wave energy used to determine G vector basis'
    keys_description(7)='G vector grid scale for fine grid basis'
    keys_description(8)='Write wavefunction to file'
    keys_description(9)='Type of calculation to perform'
    keys_description(10)='Incllude spin orbit coupling in the calculation'
    keys_description(11)='Maximum number of SCF steps'
    keys_description(12)='The type of method used for SCF minimisation, either EDFT or DM'
    keys_description(13)='Electronic temperature used to smear the Fermi level, for better convergence'
    keys_description(14)='Number of conduction bands to be included'
    keys_description(15)='Write the final density to file'
    keys_description(16)='Write total potential to file'
    keys_description(17)='Write continuation information to file'
    keys_description(18)='Provide an external potential, jelly, finite_barrier, periodic_pot or from file'
    keys_description(19)='Monkhurst pack grid for the SCF cycle'
    keys_description(20)='Write orbital information to spectral file'
    keys_description(21)='Calculate memory usage throughout the calculation'
    keys_description(22)='Determine output verbosity in the output file'
    keys_description(23)='Height of the external potential barrier for a finite potential barrier in eV'
    keys_description(24)='Fractional width of barrier in each principal direction'
    keys_description(25)='Number of cycles of periodic potential in each crystal direction'
    keys_description(26)='Amplitude of the periodic potential in eV'
    ! %End: assign_description

    ! %Begin: assign_allowed

    keys_allowed(1)='Boolean'
    keys_allowed(2)='Boolean'
    keys_allowed(3)='any int > 0'
    keys_allowed(4)='any real > 0 eV'
    keys_allowed(5)='LDA'
    keys_allowed(6)='any real > 150 eV'
    keys_allowed(7)='any real > 1.0 '
    keys_allowed(8)='Boolean'
    keys_allowed(9)='singlepoint, bandstructure'
    keys_allowed(10)='Boolean'
    keys_allowed(11)='any int > 5'
    keys_allowed(12)='DM, EDFT'
    keys_allowed(13)='any real > 0'
    keys_allowed(14)='any int > 0'
    keys_allowed(15)='Boolean'
    keys_allowed(16)='Boolean'
    keys_allowed(17)='Boolean'
    keys_allowed(18)='any'
    keys_allowed(19)='any int > 1'
    keys_allowed(20)='Boolean'
    keys_allowed(21)='Boolean'
    keys_allowed(22)='1,2,3'
    keys_allowed(23)='any real'
    keys_allowed(24)='Fractional triplet'
    keys_allowed(25)='any int > 0'
    keys_allowed(26)='any real >0'
    ! %End: assign_allowed

    ! do the loop for printing stuff

    if (print_flag)then
100    format(1x,A,T35,A)
       write(*,*)
       do i=1,max_keys
          write(*,100) io_case(trim(keys_array(i)),.true.),trim(keys_description(i))

       end do
    end if


    return
  end subroutine io_list_params


  subroutine io_header()
    !==============================================================================!
    !                          I O _  H E A D E R                                  !
    !==============================================================================!
    ! Subroutine used to write the io_header of the main Mandelbrot output file    !
    ! "out.mand".                                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num
    character(len=100):: compile_version,cpuinfo


    call trace_entry("io_header")



    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(min_mpi,maj_mpi)

       write(mpi_version_num,97)min_mpi,maj_mpi
97     format(i1,"."i1)
       min_char=scan(mpi_c_version,".")
       !print*, mpi_c_version,mpi_version_num
    end if

#ifdef __INTEL_COMPILER
#define compiler "Intel Compiler"

#endif
#ifdef __GFORTRAN__
#define compiler "GNU Fortran"
    !#define compile_version __VERSION__
#endif



#define opt opt_strat



    compile_version=compiler_version()
    if (compiler.eq."Intel Compiler")then
       compile_version=compiler_version()

       compile_version=trim(compile_version(87:97))
    end if

    write(stdout,*) "+==================================================================+"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|  oooooooooo.   oooooooooooo ooooooooo.             oooo    oooo  |"
    write(stdout,*) "|  `888'   `Y8b  `888'     `8 `888   `Y88.           `888   .8P'   |"
    write(stdout,*) "|   888      888  888          888   .d88'  .ooooo.   888  d8'     |"
    write(stdout,*) "|   888      888  888oooo8     888ooo88P'  d88' `88b  88888[       |"
    write(stdout,*) "|   888      888  888          888`88b.    888ooo888  888`88b.     |"
    write(stdout,*) "|   888     d88'  888       o  888  `88b.  888    .o  888  `88b.   |"
    write(stdout,*) "|  o888bood8P'   o888ooooood8 o888o  o888o `Y8bod8P' o888o  o888o  |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|                         Version 1.0.0                            |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,*) "|     Salutations! Welcome to the Durham Electronic RElaxation     |"
    write(stdout,*) "|     (K)Code, DEReK for short! A planewave Ab Initio DFT code     |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|      This is simply a toy project and no guarantees are made     |"
    write(stdout,*) "|        about the results obtained, for reliable electronic       |"
    write(stdout,*) "|              properties the author recommends CASTEP:            |"
    write(stdout,*) "|                      http://www.castep.org                       |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,*) "|                Author: Dr Z. Hawkhead (c) 2022                   |"
    write(stdout,*) "+==================================================================+"


    write(stdout,*) "Compiled with ",compiler," ",Trim(compile_version), " on ", __DATE__, " at ",__TIME__
    write(stdout,*) "Communications architechture: ",comms_arch
    if (comms_arch.eq."MPI")then
       write(stdout,*) "MPI Version: ",mpi_c_version(1:min_char+1)
    end if
    write(stdout,*) "Optimisation Strategy: ",opt
    write(stdout,*)
    call trace_exit("io_header")
  end subroutine io_header


  subroutine io_dryrun()
    !==============================================================================!
    !                              I O _ D R Y R U N                               !
    !==============================================================================!
    ! Subroutine for handlind the dryrun command which allows for parameter        !
    ! checking                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    call trace_entry("io_dryrun")
    if (on_root_node)then
       write(stdout,*) " "
       write(stdout,'(16x,A)') "****************************************"
       write(stdout,'(16x,A)') "*                                      *"
       write(stdout,'(16x,A)') "*         Dryrun complete....          *"
       write(stdout,'(16x,A)') "*          No errors found             *"
       write(stdout,'(16x,A)') "*                                      *"
       write(stdout,'(16x,A)') "****************************************"
    end if
    call io_finalise()
    call trace_exit("io_dryrun")
    call trace_exit("derek")
    call COMMS_FINALISE()
    call trace_finalise(current_params%debugging,rank,seed=seed)
    stop

  end subroutine io_dryrun



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



  subroutine io_block_parse(line,stat)
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
    character(*), intent(in) :: line
    integer ,intent(inout) ::stat
    character(len=6) :: block_str
    character(len=20):: block_type1,block_type2
    character(len=300):: buff
    integer :: in_stat, i, j, k ,n=0,n_com=0, counter,n_lines
    logical :: block_closed
    real(dp):: mass,sx,sy,sz
    real(dp),dimension(1:3) :: vel


    call trace_entry("io_block_parse")
    read(line,*,iostat=in_stat) block_str,block_type1

    if (in_stat.ne.0) call io_errors("Error in I/O: problem parsing block")


    do
       read(1,'(a)',iostat=stat)buff
       buff=trim(adjustl(io_case(buff)))
       if (index(buff,"%end").ne.0)then
          ! check to see if the blocks match
          read(buff,*,iostat=in_stat) block_str,block_type2
          if (in_stat.ne.0) call io_errors("Error in I/O: problem parsing end block")
          if (trim(block_type1).ne.trim(block_type2))call io_errors("Error in I/O: block "//trim(block_type1)//" not closed" )
          exit
       else if (buff(1:1).eq.'#' .or. buff(1:1).eq.'!'.or.buff(1:1).eq."")then
          n_com=n_com+1
          n=n+1
       else
          n=n+1
       end if
       if (stat.ne.0)call io_errors("Error in I/O: error parsing "//trim(seed)//".info")
    end do

    n_lines=n-n_com

    if (trim(block_type1).ne.trim(block_type2))then
       call io_errors("Error in I/O: block type mismatch")
    end if


    select case(io_case(block_type1))
    case("lattice")
       if (n_lines.ne.3)then
          call io_errors("Error in I/O: error parsing LATTICE block")
       end if
       ! take us back to the top
       do i=1,n+1
          backspace(1)
       end do


       counter=1
       do i=1,n
          read(1,*,iostat=stat)current_structure%cell(counter,1),current_structure%cell(counter,2),current_structure%cell(counter,3)
          if (stat.eq.0)counter=counter+1
       end do
       cell_declared=.true.
       ! convert to Bohr
       current_structure%cell(:,:)=current_structure%cell(:,:)*angstrom_to_bohr


    case default
       call io_errors('Error in I/O: Unknown block type '//trim(block_type1))
    end select

    ! read the close line again
    read(1,*)buff

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

    if (num_kpoints.le.0)call io_errors("Error in I/O: invalid number of kpoints")

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


  subroutine io_write_params()
    !==============================================================================!
    !                        I O _ W R I T E _ P A R A M S                         !
    !==============================================================================!
    ! Subroutine that writes the input parameters to the stdout out.pop            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  23/02/2020                                            !
    !==============================================================================!
    implicit none
    character(50)   :: sec_title
    integer         :: width=69,length,i
    ! Stuff for getting run time
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)
    character*10                    :: b(3)

    character(len=:), allocatable :: string

    call trace_entry("io_write_params")

    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    write(stdout,*)"+"//repeat("-",(width-15)/2)//" RUN STARTED "//repeat("-",(width-16)/2)//"+"
    write(stdout,1000) d_t(5),d_t(6),d_t(7),trim(months(d_t(2))),d_t(3),d_t(1)
    write(stdout,*)"+"//repeat("-",width-3)//"+"
    write(stdout,*) " "
1000 FORMAT(1x,"|",22x,i2.2,":",i2.2,":",i2.2,",",1x,A,1x,i2.2,1x,i4,23x,"|")




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! UNIT CELL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(stdout,*)"+"//repeat("-",(width-15)/2)//"  UNIT CELL  "//repeat("-",(width-16)/2)//"+"

    write(stdout,*)
    write(stdout,*) "            Lattice (A)                   Inverse Lattice (1/A)         "
    write(stdout,10) current_structure%cell(1,:)*bohr_to_angstrom,current_structure%inv_cell(1,:)/bohr_to_angstrom
    write(stdout,10) current_structure%cell(2,:)*bohr_to_angstrom,current_structure%inv_cell(2,:)/bohr_to_angstrom
    write(stdout,10) current_structure%cell(3,:)*bohr_to_angstrom,current_structure%inv_cell(3,:)/bohr_to_angstrom
    write(stdout,*)
    write(stdout,12) "Cell Volume =", current_structure%volume*bohr_to_angstrom**3

    write(stdout,*)
    write(stdout,*) "                      Lattice Parameters (A)"
    write(stdout,*) "                      ----------------------"
    write(stdout,11) 'a =',current_structure%lattice_a*bohr_to_angstrom,&
         & 'b =',current_structure%lattice_b*bohr_to_angstrom,&
         & 'c =', current_structure%lattice_c*bohr_to_angstrom
    write(stdout,*)
    write(stdout,*) "                          Cell Angles (o)"
    write(stdout,*) "                          ---------------"
    write(stdout,11) 'alpha =',current_structure%alpha,'beta =',current_structure%beta,'gamma =', current_structure%gamma


    write(stdout,*)
    write(stdout,*)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! BZ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    write(stdout,*)"+"//repeat("-",(width-15)/2)//" BZ SAMPLING "//repeat("-",(width-16)/2)//"+"
    write(stdout,*)


    write(stdout,*) "        +-------------------------------------------------+ <-- KPNT"
    write(stdout,13)"SCF MP kpoint grid :",&
         & current_params%kpt_mp_grid(1),current_params%kpt_mp_grid(2),current_params%kpt_mp_grid(3)
    write(stdout,131)"    Number kpoints :",&
         & current_params%kpt_mp_grid(1)*current_params%kpt_mp_grid(2)*current_params%kpt_mp_grid(3)
    if (current_params%iprint.ge.2)then
       write(stdout,*) "        +-------------------------------------------------+ <-- KPNT"
       write(stdout,*) "        |     Number          Fractional co-ordinate      | <-- KPNT"
       write(stdout,*) "        +-------------------------------------------------+ <-- KPNT"
       do i=1,current_structure%num_kpoints
          write(stdout,14) i,current_structure%kpt_scf_list(i,1),&
               & current_structure%kpt_scf_list(i,2),&
               & current_structure%kpt_scf_list(i,3)
       end do

    end if
    write(stdout,*) "        +-------------------------------------------------+ <-- KPNT"

    write(stdout,*)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! GENERAL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(stdout,*)"+"//repeat("-",(width-15)/2)//"   GENERAL   "//repeat("-",(width-16)/2)//"+"
    write(stdout,*)



    write(stdout,15)"Calculation type",adjustr(trim(current_params%task))
    write(stdout,15)"XC Functional",adjustr(trim(current_params%xc_functional))
    write(stdout,19)"Spin orbit coupling",current_params%soc
    write(stdout,16)"Verbosity",current_params%iprint
    write(stdout,*)
    write(stdout,*)"                        Electronic Parameters"
    write(stdout,*)"                        ---------------------"
    write(stdout,16)"Number of electrons",current_params%n_electrons
    write(stdout,16)"Number of conduction bands",current_params%conduction_bands
    write(stdout,17)"Electronic temperature (K)",current_params%electronic_temp
    write(stdout,*)
    write(stdout,*)"                         Potential Parameters"
    write(stdout,*)"                         --------------------"
    select case(trim(current_params%external_pot))
    case('jelly')
       write(stdout,15)"External potential",adjustr('Jellium')
    case('finite_barrier')
       write(stdout,15)"External potential",adjustr('Finite Barrier')
       write(stdout,22)"Fractional barrier widths",current_params%finite_barrier_width
       write(stdout,17)"Barrier height (eV)",current_params%finite_barrier_height*hartree_to_ev
    case('periodic_pot')
       write(stdout,15)"External potential",adjustr('Periodic Potential')
       write(stdout,21)"Periodic grid",current_params%periodic_pot_grid
       write(stdout,17)"Potential amplitude (eV)",current_params%periodic_pot_amp*hartree_to_ev
    case default
       write(stdout,15)"External potential",adjustr('Custom Potential')
       write(stdout,15)"Potential file",adjustr(trim(current_params%external_pot))
    end select

    write(stdout,*)
    write(stdout,*)"                            SCF Parameters"
    write(stdout,*)"                            --------------"
    write(stdout,16)"Maximum SCF steps",current_params%max_scf
    write(stdout,15)"SCF method",adjustr(trim(current_params%scf_method))
    write(stdout,18)"SCF convergence tolerance (eV)",current_params%energy_tol*hartree_to_ev
    write(stdout,*)
    write(stdout,*)"                            I/O Parameters"
    write(stdout,*)"                            --------------"
    write(stdout,19)"Write wavefunction",current_params%write_wvfn
    write(stdout,19)"Write continuation file",current_params%write_cont
    write(stdout,19)"Write density",current_params%write_density
    write(stdout,19)"Write potential",current_params%WRITE_POTENTIAL
    write(stdout,19)"Write electronic spectrum",current_params%write_spec
    write(stdout,19)"Calculate memory",current_params%calc_memory





15  format(T2,a,T36,":",T40,a30)   ! Character
16  format(T2,a,T36,":",T58,i12)   ! Integer
17  format(T2,a,T36,":",T58,f12.3)   ! Real
18  format(T2,a,T36,":",T58,ES12.2)   ! Science
19  format(T2,a,T36,":",T58,L12)   ! Logical
21  format(T2,a,T36,":",T56,3(i4,1x))   ! Integer vec
22  format(T2,a,T36,":",T53,3(f5.3,1x))   ! Real vec

14  format(T10,"|",T16,i4,T31,3(f7.4,1x),T60,"| <-- KPNT")
13  format(T10,"|",T19,a,3x,3(i3,1x),T60,"| <-- KPNT")
131 format(T10,"|",T19,a,3x,i4,T60,"| <-- KPNT")
12  format(20x,a14,2x,f12.6,2x,"A**3")
11  format(10x,3(a7,1x,f7.4,1x))
10  format(1x, 3(f10.7,1x), 3x, 3(f10.7,1x))

    call trace_exit("io_write_params")
    return
  end subroutine io_write_params

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



end module io
 
 
 
 
 
 
 
 
