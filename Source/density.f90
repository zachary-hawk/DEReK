!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module density
  use constants
  use trace, only: trace_entry,trace_exit
  use io,    only: stdout,io_errors,seed,io_out_file_header
  use wave,  only: wavefunction, wavefunction_slice
  use comms, only: rank,on_root_node
  use memory,only: memory_allocate,memory_deallocate
  use basis, only: current_basis
  private


  type,public :: elec_den
     complex(dp),allocatable,dimension(:)   :: charge
     complex(dp),allocatable,dimension(:,:) :: spin
     logical                                :: allocated=.false.
   contains
     procedure density_write
     generic :: write(unformatted) => density_write
  end type elec_den

  public :: density_allocate
  public :: density_writef
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

  subroutine density_write(den,unit,iostat,iomsg)
    class(elec_den) , intent(in)    :: den
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat
    character(len=*), intent(inout) :: iomsg

    integer :: stat,density_file

    call trace_entry('density_write')

    write(unit,iostat=iostat)current_basis%ngx,current_basis%ngy,current_basis%ngz
    if (iostat.ne.0) call io_errors("Error in density_write: unable to write to "//trim(seed)//".den file")
    write(unit,iostat=iostat,iomsg=iomsg)den%charge,den%spin,den%allocated
    if (iostat.ne.0) call io_errors("Error in density_write: unable to write to "//trim(seed)//".den file")


    call trace_exit('density_write')
  end subroutine density_write

  subroutine density_writef(den,unit) 
    type(elec_den), intent(in)    :: den
    integer         , intent(in)    :: unit

    integer :: stat,density_file
    integer :: ix,iy,iz,n=0

    call trace_entry('density_writef')

    call io_out_file_header(unit,'D')

    write(unit,*)" FFT FINE BASIS GRID  "
    write(unit,10) 'nx:',current_basis%fine_ngx
    write(unit,10) 'ny:',current_basis%fine_ngy
    write(unit,10) 'nz:',current_basis%fine_ngz


    write(unit,*)
    write(unit,20) 'ix','iy','iz', 'Charge', 'Sx','Sy','Sz'
    write(unit,*) repeat('-',65)
    n=0
    do ix=1,current_basis%fine_ngx
       do iy=1,current_basis%fine_ngy
          do iz=1,current_basis%fine_ngz
             n=n+1
             write(unit,21)ix,iy,iz,real(den%charge(n),dp),real(den%spin(n,1),dp),real(den%spin(n,2),dp),real(den%spin(n,3),dp)
          end do
       end do
    end do

10  format(2x,a,1x,i6)
20  format(1x,3(a4,2x), 5x, 4(3x, A6, 1x))
21  format(1x,3(i4,2x), 5x, 4(f9.4,2x))

    call trace_exit('density_writef')
  end subroutine density_writef

end module density
