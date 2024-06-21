!---- File documented by Fortran Documenter, Z.Hawkhead
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
module utils
  use constants
  use units
  use trace, only : trace_entry, trace_exit
  use memory, only : memory_allocate, memory_deallocate
  use io, only     : io_errors, io_present,current_params
  use comms
  implicit none

  !private

  interface utils_mat_inv
     module procedure utils_mat_inv_real
     module procedure utils_mat_inv_complex
     module procedure utils_mat_inv_int
  end interface utils_mat_inv

  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------!
  public :: utils_mat_inv

contains


  subroutine utils_mat_inv_real(input_matrix, inverted_matrix)
    !==============================================================================!
    !                     U T I L S _ M A T _ I N V _ R E A L                      !
    !==============================================================================!
    ! A utility subroutine that uses BLAS and LAPACK routines for calculaing the   !
    ! inverse of a real matrix.                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           input_matri,       intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!
    implicit none

    ! Matrix and variables declaration
    real(kind=dp), dimension(:, :), intent(in) :: input_matrix
    real(kind=dp), dimension(:, :),allocatable, intent(out) :: inverted_matrix
    real(kind=dp), dimension(:, :), allocatable :: temp_matrix
    integer :: n, info
    integer, dimension(:), allocatable :: ipiv
    real(kind=dp), dimension(:,:), allocatable :: work
    integer :: lwork
    character(200) :: Error

    call trace_entry('utils_mat_inv_real')

    ! Get dimensions of the matrix
    n = size(input_matrix, 1)

    ! Check if the matrix is square
    if (size(input_matrix, 1) /= size(input_matrix, 2)) then
       call io_errors("Matrix is not square")
    end if

    ! Allocate temporary matrices
    call memory_allocate(temp_matrix,1,n,1,n,'G')
    call memory_allocate(inverted_matrix,1,n,1,n,'G')
    allocate(ipiv(n))
    lwork = n
    call memory_allocate(work,1,n, 1,lwork,'G')

    ! Copy input matrix to temporary matrix
    temp_matrix = input_matrix

    ! Invert the matrix using LAPACK routines from OpenBLAS
    call dgetrf(n, n, temp_matrix, n, ipiv, info)


    if (info == 0) then
       call dgetri(n, temp_matrix, n, ipiv, work, lwork, info)
       inverted_matrix = temp_matrix
    else
       write(error,*)"OPENBLAS routine DGETRI returned non-zero exit code: ",info
       call io_errors(trim(error))
    end if

    ! Deallocate temporary matrices deallocate(temp_matrix)
    call memory_deallocate(temp_matrix,'G')
    call memory_deallocate(work,'G')
    call trace_exit('utils_mat_inv_real')
  end subroutine utils_mat_inv_real


  subroutine utils_mat_inv_complex(input_matrix, inverted_matrix)
    !==============================================================================!
    !                  U T I L S _ M A T _ I N V _ C O M P L E X                   !
    !==============================================================================!
    ! A utility subroutine that uses BLAS and LAPACK routines for calculaing the   !
    ! inverse of a complex matrix.                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           input_matri,       intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!
    implicit none

    ! Matrix and variables declaration
    complex(kind=dp), dimension(:, :), intent(in) :: input_matrix
    complex(kind=dp), dimension(:, :),allocatable, intent(out) :: inverted_matrix
    complex(kind=dp), dimension(:, :), allocatable :: temp_matrix
    integer :: n, info
    integer, dimension(:), allocatable :: ipiv
    complex(kind=dp), dimension(:,:), allocatable :: work
    integer :: lwork
    character(200) :: Error

    call trace_entry('utils_mat_inv_complex')

    ! Get dimensions of the matrix
    n = size(input_matrix, 1)

    ! Check if the matrix is square
    if (size(input_matrix, 1) /= size(input_matrix, 2)) then
       call io_errors("Matrix is not square")
    end if

    ! Allocate temporary matrices
    call memory_allocate(temp_matrix,1,n,1,n,'G')
    call memory_allocate(inverted_matrix,1,n,1,n,'G')
    allocate(ipiv(n))
    lwork = n
    call memory_allocate(work,1,n, 1,lwork,'G')

    ! Copy input matrix to temporary matrix
    temp_matrix = input_matrix

    ! Invert the matrix using LAPACK routines from OpenBLAS
    call dgetrf(n, n, temp_matrix, n, ipiv, info)


    if (info == 0) then
       call dgetri(n, temp_matrix, n, ipiv, work, lwork, info)
       inverted_matrix = temp_matrix
    else
       write(error,*)"OPENBLAS routine DGETRI returned non-zero exit code: ",info
       call io_errors(trim(error))
    end if

    ! Deallocate temporary matrices deallocate(temp_matrix)
    call memory_deallocate(ipiv,'G')
    call memory_deallocate(work,'G')
    call trace_exit('utils_mat_inv_complex')
  end subroutine utils_mat_inv_complex


  subroutine utils_mat_inv_int(input_matrix, inverted_matrix)
    !==============================================================================!
    !                      U T I L S _ M A T _ I N V _ I N T                       !
    !==============================================================================!
    ! A utility subroutine that uses BLAS and LAPACK routines for calculaing the   !
    ! inverse of a int matrix                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           input_matri,       intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  07/06/2023                                            !
    !==============================================================================!
    implicit none

    ! Matrix and variables declaration
    integer, dimension(:, :), intent(in) :: input_matrix
    integer, dimension(:, :),allocatable, intent(out) :: inverted_matrix
    integer, dimension(:, :), allocatable :: temp_matrix
    integer :: n, info
    integer, dimension(:), allocatable :: ipiv
    integer, dimension(:,:), allocatable :: work
    integer :: lwork
    character(200) :: Error

    call trace_entry('utils_mat_inv_int')

    ! Get dimensions of the matrix
    n = size(input_matrix, 1)

    ! Check if the matrix is square
    if (size(input_matrix, 1) /= size(input_matrix, 2)) then
       call io_errors("Matrix is not square")
    end if

    ! Allocate temporary matrices
    call memory_allocate(temp_matrix,1,n,1,n,'G')
    call memory_allocate(inverted_matrix,1,n,1,n,'G')
    allocate(ipiv(n))
    lwork = n
    call memory_allocate(work,1,n, 1,lwork,'G')

    ! Copy input matrix to temporary matrix
    temp_matrix = input_matrix

    ! Invert the matrix using LAPACK routines from OpenBLAS
    call dgetrf(n, n, temp_matrix, n, ipiv, info)


    if (info == 0) then
       call dgetri(n, temp_matrix, n, ipiv, work, lwork, info)
       inverted_matrix = temp_matrix
    else
       write(error,*)"OPENBLAS routine DGETRI returned non-zero exit code: ",info
       call io_errors(trim(error))
    end if

    ! Deallocate temporary matrices deallocate(temp_matrix)
    call memory_deallocate(ipiv,'G')
    call memory_deallocate(work,'G')
    call trace_exit('utils_mat_inv_int')
  end subroutine utils_mat_inv_int




  subroutine utils_init_random() 
!==============================================================================!
!                      U T I L S _ I N I T _ R A N D O M                       !
!==============================================================================!
! Subtrouting for setting up the random numbers.                               !
!------------------------------------------------------------------------------!
! Arguments:                                                                   !
!           None                                                               !
!------------------------------------------------------------------------------!
! Author:   Z. Hawkhead  08/11/2023                                            !
!==============================================================================!
    implicit none
    integer, allocatable :: seed(:)   
    integer ::  n, un, istat,the_seed
    call trace_entry("utils_init_random")


    if (on_root_node)then
       if (.not.io_present("random_seed"))then
          call random_seed(size = n)
          allocate(seed(n))
          open(newunit=un, file="/dev/urandom", access="stream", &
               form="unformatted", action="read", status="old", iostat=istat)
          read(un) seed
          close(un)
          the_seed=abs(seed(1))
          seed=the_seed
       else
          call random_seed(size = n)
          allocate(seed(n))
          the_seed=current_params%random_seed
          seed=the_seed
       end if
    end if

    call comms_bcast(the_seed,1)
    call comms_bcast(n,1)
    if (rank.gt.0) allocate(seed(n))
    seed=the_seed+37*rank
    current_params%random_seed=seed(1)



    call random_seed(put=seed)

    call trace_exit("utils_init_random")
    return
  end subroutine utils_init_random

  subroutine utils_random_number(rand)
    !==============================================================================!
    !                    U T I L S _ R A N D O M _ N U M B E R                     !
    !==============================================================================!
    ! Subroutine for generating a random number.                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rand,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    implicit none
    real(dp),intent(inout)  ::rand 
    call trace_entry("utils_random_number")                                                                                                                                                                                                                                  
    call random_number(rand)
    call trace_exit("utils_random_number")                                                                                                                                                                                                                                   
    return
  end subroutine utils_random_number
end module utils
