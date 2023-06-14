!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module density
  use constants
  use trace, only: trace_entry,trace_exit
  use io,    only: stdout,io_errors
  use wave,  only: wavefunction, wavefunction_slice
  use comms, only: rank,on_root_node
  use memory,only: memory_allocate,memory_deallocate
  use basis, only: current_basis
  private


  type,public :: elec_den
     complex(dp),allocatable,dimension(:)   :: charge
     complex(dp),allocatable,dimension(:,:) :: spin
     logical                                :: allocated=.false.
  end type elec_den

  public :: density_allocate

contains

  subroutine density_allocate(den)
    !==============================================================================!
    !                       D E N S I T Y _ A L L O C A T E                        !
    !==============================================================================!
    ! Subroutine for allocating a density type                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           den,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    type(elec_den),intent(inout) :: den

    call trace_entry('density_allocate')

    call memory_allocate(den%charge,1,current_basis%num_fine_grid_points,'D')
    call memory_allocate(den%spin,1,current_basis%num_fine_grid_points,1,3,'D')

    ! allocated
    den%allocated  = .true.

    call density_zero(den)


    call trace_exit('density_allocate')
  end subroutine density_allocate


  subroutine density_zero(den)
    !==============================================================================!
    !                           D E N S I T Y _ Z E R O                            !
    !==============================================================================!
    ! Subroutine for setting a density to zero                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           den,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    type(elec_den),intent(inout) :: den

    call trace_entry("density_zero")
    if (.not.den%allocated)then
       call io_errors("Error in density_zero: density not allocated")
    end if

    den%charge(:) = cmplx_0    
    den%spin(:,:) = cmplx_0    
    call trace_exit('density_zero')
    return
  end subroutine density_zero


  subroutine density_calculate(wfn,den,occ)
    !==============================================================================!
    !                      D E N S I T Y _ C A L C U L A T E                       !
    !==============================================================================!
    ! Subroutine for calculating the density, takes a wavefunction and loops       !
    ! over all g vectors and kpoints                                               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: in                                    !
    !           den,               intent :: out                                   !
    !           occ,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    type(wavefunction) ,intent(in)   :: wfn
    type(elec_den) ,intent(out)      :: den
    real(dp),dimension(*),intent(in) :: occ

    integer :: nb,nk
    call trace_entry('density_calculate')  
    ! do some gathering of the distributed kpoints and gvecs


    call density_zero(den)





    call trace_exit('density_calculate')
  end subroutine density_calculate




end module density
