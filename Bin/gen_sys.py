#!/usr/bin/env python3
import sys

# Template Fortran code with placeholders
fortran_code_template = """
module sys
  use trace
  use comms
  use iso_fortran_env
  use iso_c_binding
  type sys_info
     ! Compile time parameters
     character(100) :: git     
     character(100) :: arch
     character(100) :: cpu
     character(100) :: opt
     character(100) :: consts
     ! On the fly parameters
     character(100) :: date
     character(100) :: compiler
     character(100) :: comms
     character(100) :: comms_version
     character(100) :: ffts
     character(100) :: openblas
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


  subroutine sys_init
    character(100)                :: compile_buff
    character(100)                :: compile_version
    integer                       :: math_maj,math_min,math_patch
    integer                       :: maj_mpi,min_mpi,min_char
    character(max_version_length) :: mpi_c_version
    call trace_entry('sys_init')

    ! Get the version of openblas
    call ilaver(math_maj,math_min,math_patch)

    !***
    current_sys%git   = trim(adjustl('$1'))
    current_sys%arch  = trim(adjustl('$2'))
    current_sys%cpu   = trim(adjustl('$3'))
    current_sys%opt   = trim(adjustl('$4'))
    current_sys%consts= trim(adjustl('$5'))
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
    write(current_sys%ffts,*) f_fftw_version()
    current_sys%ffts = adjustl(current_sys%ffts) 
    !open blas
    write(current_sys%openblas,'(i0,".",I0,".",i0)')math_maj,math_min,math_patch
    ! comms
    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(maj_mpi,min_mpi)

!write(mpi_version_num,1)maj_mpi,min_mpi
1      format(i1,".",i1)
       min_char=scan(mpi_c_version,",")
    end if

    current_sys%comms = adjustl(comms_arch)
    current_sys%comms_version = adjustl(mpi_c_version(1:min_char))
    ! Date and time
    write(current_sys%date,*)trim(adjustl(__DATE__)), ", ",trim(adjustl(__TIME__))
     current_sys%date = adjustl(current_sys%date) 

    call trace_exit('sys_init')    
  end subroutine sys_init
  character(len=fft_v_len) function f_fftw_version()

    implicit none
    integer :: i
    character(kind=c_char), dimension(:), pointer :: fftw_version_ptr
    call trace_entry('f_fftw_version')
    call c_f_pointer(fftw_version_ptr_c(), fftw_version_ptr, [fft_v_len])

    f_fftw_version = ' '
    do i=1, fft_v_len
       if( fftw_version_ptr(i) == C_NULL_CHAR ) exit

       f_fftw_version(i:i) = fftw_version_ptr(i)
    end do
    call trace_exit('f_fftw_version')
    return
  end function f_fftw_version


end module sys
"""

def main():
    # Check if command-line arguments are provided
    if len(sys.argv) < 6:
        print("Usage: python script.py arg1 arg2 arg3 arg4 arg5")
        sys.exit(1)

    # Replace placeholders in the Fortran code template with command-line arguments
    modified_code = fortran_code_template.replace('$1', sys.argv[1]) \
                                          .replace('$2', sys.argv[2]) \
                                          .replace('$3', sys.argv[3]) \
                                          .replace('$4', sys.argv[4]) \
                                          .replace('$5', sys.argv[5])
    print(sys.argv[1],sys.argv[2])
    # Write the modified code to a file
    with open("sys.f90", "w") as file:
        file.write(modified_code)

if __name__ == "__main__":
    main()
