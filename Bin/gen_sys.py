#!/usr/bin/env python3
import sys

def parse_author_line(line):
    # Split the line into components
    components = line.split()

    # Extract title, initials, surname, and email
    title = ''.join([components[0],"."])
    initials = ''.join([char + '.' for char in components[1]])  
    surname = components[2]
    try:
        email = components[3]
    except:
        email='Null'
    print(initials)
    print(title)
    return title, initials, surname, email

def read_authors_file(filename):
    # Initialize arrays
    titles = []
    initials = []
    surnames = []
    emails = []

    # Read the file line by line
    with open(filename, 'r') as file:
        for line in file:
            # Parse each line and append to arrays
            title, initial, surname, email = parse_author_line(line.strip())
            titles.append(title)
            initials.append(initial)
            surnames.append(surname)
            emails.append(email)

    return titles, initials, surnames, emails


def format_as_f90_arrays(titles, initials, surnames, emails):
    # Initialize the output string
    output = ''

    # Add data assignment statements
    for i in range(len(titles)):
        output += "current_sys%%title(%d) = '%s'\n" % (i+1, titles[i])
    output += "\n"
    for i in range(len(initials)):
        output += "current_sys%%initials(%d) = '%s'\n" % (i+1, initials[i])
    output += "\n"
    for i in range(len(surnames)):
        output += "current_sys%%surname(%d) = '%s'\n" % (i+1, surnames[i])
    output += "\n"
    for i in range(len(emails)):
        output += "current_sys%%email(%d) = '%s'\n" % (i+1, emails[i])

    for i in range(len(titles)):
        output += "write(current_sys%%names(%d),'(a,1x,a,1x,a)') trim(current_sys%%title(%d)),trim(current_sys%%initials(%d)),trim(current_sys%%surname(%d)) \n"%(i+1,i+1,i+1,i+1)
    return output

# Template Fortran code with placeholders
fortran_code_template = """
module sys
  use trace
  use comms
  use iso_fortran_env
  use iso_c_binding
  use xc_f03_lib_m, only: xc_f03_version,xc_f03_version_string
  type sys_info
     ! Compile time parameters
     character(100) :: git     
     character(100) :: arch
     character(100) :: cpu
     character(100) :: opt
     character(100) :: consts
     character(100) :: max_lev
     character(100) :: phys_cores
     character(100) :: logi_cores
     character(100) :: tot_mem
     ! On the fly parameters
     character(100) :: date
     character(100) :: compiler
     character(100) :: comms
     character(100) :: comms_version
     character(100) :: ffts
     character(100) :: openblas
     character(100) :: libxc
     ! Author details, not useful now but future proofing
     character(50),dimension(:),allocatable :: title
     character(50),dimension(:),allocatable :: initials
     character(50),dimension(:),allocatable :: surname
     character(50),dimension(:),allocatable :: names
     integer                           :: nauth
  end type sys_info
  interface
     function fftw_version_ptr_c()bind (c,name='padded_fftw_version')
       use iso_c_binding
       type(c_ptr) :: fftw_version_ptr_c
     end function fftw_version_ptr_c
  end interface

  type(sys_info),public     :: current_sys
  integer,parameter         :: fft_v_len = 64
contains


  subroutine sys_init()

    character(100)                 :: compile_buff
    character(100)                 :: compile_version
    character(100)                 :: mpi_version_num
    integer                       :: math_maj,math_min,math_patch
    integer                       :: maj_mpi,min_mpi,min_char
    character(max_version_length) :: mpi_c_version
    character(10) :: junk1, junk2
    integer :: i, vmajor, vminor, vmicro


    call trace_entry('sys_init')

    ! Get the version of openblas
    call ilaver(math_maj,math_min,math_patch)

    !***
    current_sys%git   = trim(adjustl('$1'))
    current_sys%arch  = trim(adjustl('$2'))
    current_sys%cpu   = trim(adjustl('$3'))
    current_sys%opt   = trim(adjustl('$4'))
    current_sys%consts= trim(adjustl('$5'))
    current_sys%max_lev=trim(adjustl('$6'))
    current_sys%comms_version = trim(adjustl('$7'))
    current_sys%phys_cores = trim(adjustl('$8'))
    current_sys%logi_cores = trim(adjustl('$9'))
    current_sys%tot_mem = trim(adjustl('$0'))
    !***


#ifdef __INTEL_COMPILER
#define compile_buff "Intel Compiler"

#endif
#ifdef __GFORTRAN__
#define compile_buff "GNU Fortran"
    !#define compile_version __VERSION__
#endif


    compile_version = compiler_version()
    if (compile_buff.eq."Intel Compiler")then
       compile_version=trim(compile_version(87:97))
    end if

    ! Write the compiler
    write(current_sys%compiler,*) trim(compile_buff)," ", trim(compile_version)
    current_sys%compiler = adjustl(current_sys%compiler)
    ! FFTS
    write(current_sys%ffts,*) sys_fftw_version()
    current_sys%ffts = adjustl(current_sys%ffts) 
    !open blas
    write(current_sys%openblas,'(i0,".",I0,".",i0)')math_maj,math_min,math_patch


    current_sys%comms = adjustl(comms_arch)
    ! Date and time
    write(current_sys%date,*)trim(adjustl(__DATE__)), ", ",trim(adjustl(__TIME__))
    current_sys%date = adjustl(current_sys%date) 

    ! LibXC version information
    call xc_f03_version(vmajor, vminor, vmicro)
    call xc_f03_version_string(current_sys%libxc)
    !write(current_sys%libxc,'(I1,".",I1,".",I1)') vmajor, vminor, vmicro

    !! AUTHORS
call sys_read_authors('../Bin/AUTHORS',current_sys%title,current_sys%initials,current_sys%surname,current_sys%names,current_sys%nauth)

    call trace_exit('sys_init')    
  end subroutine sys_init

  character(len=fft_v_len) function sys_fftw_version()

    implicit none
    integer :: i
    character(kind=c_char), dimension(:), pointer :: fftw_version_ptr
    call trace_entry('sys_fftw_version')
    call c_f_pointer(fftw_version_ptr_c(), fftw_version_ptr, [fft_v_len])

    sys_fftw_version = ' '
    do i=1, fft_v_len
       if( fftw_version_ptr(i) == C_NULL_CHAR ) exit

       sys_fftw_version(i:i) = fftw_version_ptr(i)
    end do
    call trace_exit('sys_fftw_version')
    return
  end function sys_fftw_version

subroutine sys_read_authors(filename, title, initials, surname, names, nauth)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=50), dimension(:), allocatable, intent(out) :: title
    character(len=50), dimension(:), allocatable, intent(out) :: initials
    character(len=50), dimension(:), allocatable, intent(out) :: surname
    character(len=50), dimension(:), allocatable, intent(out) :: names
    integer, intent(out) :: nauth

    integer :: unit, ios, i, count
    character(len=200) :: line
    character(len=50) :: t, ini, sur
    character(len=200) :: n
    character(len=50), dimension(:), allocatable :: t_arr, ini_arr, sur_arr, n_arr

    ! Open the file for reading
    open(unit=10, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Error: Could not open AUTHORS file for reading.'
        nauth = 0
        return
    end if

    ! Initialize author count
    count = 0

    ! First, count the number of lines (authors) in the file
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        count = count + 1
    end do

    ! Allocate arrays based on the number of authors
    allocate(t_arr(count))
    allocate(ini_arr(count))
    allocate(sur_arr(count))
    allocate(n_arr(count))

    ! Rewind the file to read the contents again
    rewind(10)

    ! Read and parse each line
    i = 0
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        i = i + 1
        read(line, *) t, ini, sur
        t = trim(adjustl(t)) // '.'
        ini = sys_adjust_initials(trim(adjustl(ini)))
        

        sur = trim(adjustl(sur))
        n = trim(t) // ' ' // trim(ini) // ' ' // trim(sur)
print*,n
        t_arr(i) = t
        ini_arr(i) = ini
        sur_arr(i) = sur
        n_arr(i) = n
    end do

    ! Close the file
    close(10)

    ! Set output variables
    title = t_arr
    initials = ini_arr
    surname = sur_arr
    names = n_arr
    nauth = count

end subroutine sys_read_authors
    function sys_adjust_initials(initials) result(adjusted)
        character(len=*), intent(in) :: initials
        character(len=len(initials)+10) :: adjusted
        integer :: j, len_initials

        adjusted = ''
        len_initials = len(trim(initials))
        print*,initials,len_initials
        do j = 1, 2*len_initials,2

            adjusted(j:j) = initials(j:j)
            adjusted(j+1:j+1) = '.'
        end do
        
    end function sys_adjust_initials



end module sys
"""

def main():
    # Check if command-line arguments are provided
    if len(sys.argv) < 6:
        print("Usage: python script.py arg1 arg2 arg3 arg4 arg5")
        sys.exit(1)


    filename='../Bin/AUTHORS'
    titles, initials, surnames, emails = read_authors_file(filename)
    nauth = len(titles)
    fortran_output = format_as_f90_arrays(titles, initials, surnames, emails)

    # Replace placeholders in the Fortran code template with command-line arguments
    modified_code = fortran_code_template.replace('$1', sys.argv[1]) \
                                          .replace('$2', sys.argv[2]) \
                                          .replace('$3', sys.argv[3]) \
                                          .replace('$4', sys.argv[4]) \
                                          .replace('$5', sys.argv[5]) \
                                          .replace('$6', sys.argv[6]) \
                                          .replace('$7', sys.argv[7]) \
                                          .replace('$8', sys.argv[8]) \
                                          .replace('$9', sys.argv[9]) \
                                          .replace('$0', sys.argv[10]) \
                                          .replace('$nauth',str(nauth))\
                                          .replace('$auth',fortran_output)


    
    
    #print(sys.argv[1],sys.argv[2])
    # Write the modified code to a file
    with open("sys.f90", "w") as file:
        file.write(modified_code)

if __name__ == "__main__":
    main()
