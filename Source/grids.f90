module grids
  use constants
  use trace
  use io,    only : io_errors
  use basis, only : current_basis
  use fft,   only : fft_init, fft_3d,fft_1d
  use pot,   only : potential
  use density,only: elec_den
  use wave,  only : wavefunction,wavefunction_slice


  interface grids_real2recip
     module procedure grids_real2recip_1d
     module procedure grids_real2recip_3d
     module procedure grids_real2recip_pot
     module procedure grids_real2recip_den
     !module procedure grids_real2recip_wave
     !module procedure grids_real2recip_wave_slice
  end interface grids_real2recip

  interface grids_recip2real
     module procedure grids_recip2real_1d
     module procedure grids_recip2real_3d
     module procedure grids_recip2real_pot
     module procedure grids_recip2real_den
     !module procedure grids_recip2real_wave
     !module procedure grids_recip2real_wave_slice
  end interface grids_recip2real


  public  grids_real2recip
  public  grids_recip2real
  public  grids_real2recip_pot

contains
 
  subroutine grids_real2recip_1d(array,gtype)
    !==============================================================================!
    !                    G R I D _ R E A L 2 R E C I P _ 1 D                     !
    !==============================================================================!
    ! Subroutine using the fftw3 fast fourier transforms for a 1d array.           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           gtype,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:),intent(inout) :: array
    character(4), intent(in)             :: gtype
    ! Backwards transform so dir = +1
    integer  :: dir = 1

    call trace_entry('grids_real2recip_1d')

    ! Here is the call to fft - should only be done from these routines 
    call fft_1d(array,gtype,dir)

    call trace_exit('grids_real2recip_1d')
  end subroutine grids_real2recip_1d

  
  subroutine grids_recip2real_1d(array,gtype)
    !==============================================================================!
    !                    G R I D _ R E C I P 2 R E A L _ 1 D                     !
    !==============================================================================!
    ! Subroutine using the fftw3 fast fourier transforms for a 1d array.           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           gtype,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:),intent(inout) :: array
    character(4), intent(in)             :: gtype
    ! Backwards transform so dir = -1
    integer  :: dir = -1

    call trace_entry('grids_recip2real_1d')

    ! Here is the call to fft - should only be done from these routines 
    call fft_1d(array,gtype,dir)

    call trace_exit('grids_recip2real_1d')
  end subroutine grids_recip2real_1d


  subroutine grids_real2recip_3d(array,gtype)
    !==============================================================================!
    !                    G R I D _ R E A L 2 R E C I P _ 3 D                     !
    !==============================================================================!
    ! Subroutine using the fftw3 fast fourier transforms for a 3d array.           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           gtype,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:,:),intent(inout) :: array
    character(4), intent(in)                  :: gtype
    ! Backwards transform so dir = +1
    integer  :: dir = 1

    call trace_entry('grids_real2recip_1d')

    ! Here is the call to fft - should only be done from these routines 
    call fft_3d(array(:,1),gtype,dir)
    call fft_3d(array(:,2),gtype,dir)
    call fft_3d(array(:,3),gtype,dir)

    call trace_exit('grids_real2recip_1d')
  end subroutine grids_real2recip_3d


  subroutine grids_recip2real_3d(array,gtype)
    !==============================================================================!
    !                    G R I D _ R E C I P 2 R E A L _ 3 D                     !
    !==============================================================================!
    ! Subroutine using the fftw3 fast fourier transforms for a 3d array            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           gtype,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:,:),intent(inout) :: array
    character(4), intent(in)                 :: gtype
    ! Backwards transform so dir = -1
    integer  :: dir = -1

    call trace_entry('grids_recip2real_3d')

    ! Here is the call to fft - should only be done from these routines 
    call fft_3d(array(:,1),gtype,dir)
    call fft_3d(array(:,2),gtype,dir)
    call fft_3d(array(:,3),gtype,dir)

    call trace_exit('grids_recip2real_3d')
  end subroutine grids_recip2real_3d


  subroutine grids_real2recip_pot(pot,gtype)
    !==============================================================================!
    !                   G R I D _ R E A L 2 R E C I P _ P O T                    !
    !==============================================================================!
    ! Subroutine using the fftw3 fast fourier transforms for a potential           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array,             intent :: inout                                 !
    !           gtype,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    type(potential)            :: pot 
    character(4), intent(in)   :: gtype
    ! Backwards transform so dir = +1
    integer  :: dir = 1

    call trace_entry('grids_real2recip_pot')
    if (pot%recip)then
       call io_errors('Potential is already in reciprocal space')
    end if
    
    ! Here is the call to fft - should only be done from these routines 
    call fft_3d(pot%nc_pot(:,1,1),gtype,dir)
    call fft_3d(pot%nc_pot(:,1,2),gtype,dir)
    call fft_3d(pot%nc_pot(:,2,1),gtype,dir)
    call fft_3d(pot%nc_pot(:,2,2),gtype,dir)

    pot%recip = .true.
    call trace_exit('grids_real2recip_pot')
  end subroutine grids_real2recip_pot


  subroutine grids_recip2real_pot(pot,gtype)
    type(potential)            :: pot 
    character(4), intent(in)   :: gtype
    ! Backwards transform so dir = -1
    integer  :: dir = -1
    call trace_entry('grids_recip2real_pot')

    ! Error checking
    if (.not.pot%recip)then
       call io_errors('Potential is already in direct space')
    end if
    
    ! Here is the call to fft - should only be done from these routines 
    call fft_3d(pot%nc_pot(:,1,1),gtype,dir)
    call fft_3d(pot%nc_pot(:,1,2),gtype,dir)
    call fft_3d(pot%nc_pot(:,2,1),gtype,dir)
    call fft_3d(pot%nc_pot(:,2,2),gtype,dir)

    pot%recip = .false.
    call trace_exit('grids_recip2real_pot')
  end subroutine grids_recip2real_pot



    subroutine grids_real2recip_den(den,gtype)
    type(elec_den)             :: den 
    character(4), intent(in)   :: gtype
    ! Backwards transform so dir = +1
    integer  :: dir = 1

    call trace_entry('grids_real2recip_den')
    if (den%recip)then
       call io_errors('Density is already in reciprocal space')
    end if
    
    ! Here is the call to fft - should only be done from these routines 

    ! FFT the charge
    call fft_3d(den%charge,gtype,dir)
    
    ! FFT the spins
    call fft_3d(den%spin(:,1),gtype,dir)
    call fft_3d(den%spin(:,2),gtype,dir)
    call fft_3d(den%spin(:,3),gtype,dir)


    den%recip = .true.
    call trace_exit('grids_real2recip_den')
  end subroutine grids_real2recip_den


  subroutine grids_recip2real_den(den,gtype)
    type(elec_den)            :: den 
    character(4), intent(in)   :: gtype
    ! Backwards transform so dir = -1
    integer  :: dir = -1
    call trace_entry('grids_recip2real_den')

    ! Error checking
    if (.not.den%recip)then
       call io_errors('Density is already in direct space')
    end if
    
    ! Here is the call to fft - should only be done from these routines 
    ! FFT the charge
    call fft_3d(den%charge,gtype,dir)
    
    ! FFT the spins
    call fft_3d(den%spin(:,1),gtype,dir)
    call fft_3d(den%spin(:,2),gtype,dir)
    call fft_3d(den%spin(:,3),gtype,dir)

    
    den%recip = .false.
    call trace_exit('grids_recip2real_den')
  end subroutine grids_recip2real_den


!!$
!!$
!!$  
!!$  subroutine grids_real2recip_wave(wave,gtype)
!!$    type(elec_wave)             :: wave 
!!$    character(4), intent(in)   :: gtype
!!$    ! Backwards transform so dir = +1
!!$    integer  :: dir = 1
!!$
!!$    call trace_entry('grids_real2recip_wave')
!!$    if (wave%recip)then
!!$       call io_errors('Wavefunction is already in reciprocal space')
!!$    end if
!!$    
!!$    ! Here is the call to fft - should only be done from these routines 
!!$
!!$    ! FFT the charge
!!$    call fft_3d(wave%charge,gtype,dir)
!!$    
!!$    ! FFT the spins
!!$    call fft_3d(wave%spin(:,1),gtype,dir)
!!$    call fft_3d(wave%spin(:,2),gtype,dir)
!!$    call fft_3d(wave%spin(:,3),gtype,dir)
!!$
!!$
!!$    wave%recip = .true.
!!$    call trace_exit('grids_real2recip_wave')
!!$  end subroutine grids_real2recip_wave
!!$
!!$
!!$  subroutine grids_recip2real_wave(wave,gtype)
!!$    type(elec_wave)            :: wave 
!!$    character(4), intent(in)   :: gtype
!!$    ! Backwards transform so dir = -1
!!$    integer  :: dir = -1
!!$    call trace_entry('grids_recip2real_wave')
!!$
!!$    ! Error checking
!!$    if (.not.wave%recip)then
!!$       call io_errors('Wavefunction is already in direct space')
!!$    end if
!!$    
!!$    ! Here is the call to fft - should only be done from these routines 
!!$    ! FFT the charge
!!$    call fft_3d(wave%charge,gtype,dir)
!!$    
!!$    ! FFT the spins
!!$    call fft_3d(wave%spin(:,1),gtype,dir)
!!$    call fft_3d(wave%spin(:,2),gtype,dir)
!!$    call fft_3d(wave%spin(:,3),gtype,dir)
!!$
!!$    
!!$    wave%recip = .false.
!!$    call trace_exit('grids_recip2real_wave')
!!$  end subroutine grids_recip2real_wave




end module grids