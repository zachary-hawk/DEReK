!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module fft
  use, intrinsic :: iso_c_binding
  use constants
  use trace, only : trace_entry, trace_exit
  use memory, only : memory_allocate, memory_deallocate
  use io, only : io_errors
  include 'fftw3.f03'

  ! Define the plans for the standard and fine grids, both forwards and backwards
  type (c_ptr) :: plan_std_fwd
  type (c_ptr) :: plan_std_bwd
  type (c_ptr) :: plan_fine_fwd
  type (c_ptr) :: plan_fine_bwd

  ! Testing plans
  type (c_ptr) :: plan_std_fwd_1d
  type (c_ptr) :: plan_std_bwd_1d
  type (c_ptr) :: plan_fine_fwd_1d
  type (c_ptr) :: plan_fine_bwd_1d

  ! The arrays that will be used for the input 

  complex(dp), dimension(:,:,:), allocatable :: indata
  complex(dp), dimension(:,:,:), allocatable :: outdata
  complex(dp), dimension(:,:,:), allocatable :: indata_fine
  complex(dp), dimension(:,:,:), allocatable :: outdata_fine

  ! Testing arrays
  complex(dp), dimension(:), allocatable :: indata_1d
  complex(dp), dimension(:), allocatable :: outdata_1d
  complex(dp), dimension(:), allocatable :: indata_fine_1d
  complex(dp), dimension(:), allocatable :: outdata_fine_1d


  ! Local dimensions
  integer :: loc_nx,loc_ny, loc_nz, loc_nx_fine,loc_ny_fine,loc_nz_fine

contains

  subroutine fft_init(nx,ny,nz,nx_fine,ny_fine,nz_fine)
    !==============================================================================!
    !                               F F T _ I N I T                                !
    !==============================================================================!
    ! Fast fourier transform initialisation                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           nx,                intent :: in                                    !
    !           ny,                intent :: in                                    !
    !           nz,                intent :: in                                    !
    !           nx_fine,           intent :: in                                    !
    !           ny_fine,           intent :: in                                    !
    !           nz_fine,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    integer , intent(in) :: nx
    integer , intent(in) :: ny
    integer , intent(in) :: nz
    integer , intent(in) :: nx_fine
    integer , intent(in) :: ny_fine
    integer , intent(in) :: nz_fine

    call trace_entry("fft_init")

    ! allocate the arrays
    if (allocated(indata)) call memory_deallocate(indata,'B')
    if (allocated(outdata)) call memory_deallocate(outdata,'B')
    if (allocated(indata_fine)) call memory_deallocate(indata_fine,'B')
    if (allocated(outdata_fine)) call memory_deallocate(outdata_fine,'B')


    call memory_allocate(indata, 1, nx, 1, ny, 1, nz,'B')
    call memory_allocate(outdata, 1, nx, 1, ny, 1, nz,'B')  
    call memory_allocate(indata_fine, 1, nx_fine, 1, ny_fine, 1, nz_fine,'B')
    call memory_allocate(outdata_fine, 1, nx_fine, 1, ny_fine, 1, nz_fine,'B')

    ! Allocate testing arrays
    call memory_allocate(indata_1d, 1, nx,'B')
    call memory_allocate(outdata_1d, 1, nx,'B')  
    call memory_allocate(indata_fine_1d, 1, nx_fine,'B')
    call memory_allocate(outdata_fine_1d, 1, nx_fine,'B')

    

    loc_nx = nx
    loc_ny = ny
    loc_nz = nz
    loc_nx_fine = nx_fine
    loc_ny_fine = ny_fine
    loc_nz_fine = nz_fine


    ! Zero them
    indata(:,:,:) = 0.0_dp
    outdata(:,:,:) = 0.0_dp
    indata_fine(:,:,:) = 0.0_dp
    outdata_fine(:,:,:) = 0.0_dp

    indata_1d(:) = 0.0_dp
    outdata_1d(:) = 0.0_dp
    indata_fine_1d(:) = 0.0_dp
    outdata_fine_1d(:) = 0.0_dp

    
    plan_std_fwd = fftw_plan_dft_3d(nz,ny,nx, indata, outdata,FFTW_FORWARD, FFTW_ESTIMATE)
    plan_std_bwd = fftw_plan_dft_3d(nz,ny,nx, indata, outdata,FFTW_BACKWARD, FFTW_ESTIMATE)
    plan_fine_fwd = fftw_plan_dft_3d(nz_fine,ny_fine,nx_fine, indata, outdata,FFTW_FORWARD, FFTW_ESTIMATE)
    plan_fine_bwd = fftw_plan_dft_3d(nz_fine,ny_fine,nx_fine, indata, outdata,FFTW_BACKWARD, FFTW_ESTIMATE)

    plan_std_fwd_1d = fftw_plan_dft_1d(nz, indata_1d, outdata_1d,FFTW_FORWARD, FFTW_ESTIMATE)
    plan_std_bwd_1d = fftw_plan_dft_1d(nz, indata_1d, outdata_1d,FFTW_BACKWARD, FFTW_ESTIMATE)
    plan_fine_fwd_1d = fftw_plan_dft_1d(nz_fine, indata_1d, outdata_1d,FFTW_FORWARD, FFTW_ESTIMATE)
    plan_fine_bwd_1d = fftw_plan_dft_1d(nz_fine, indata_1d, outdata_1d,FFTW_BACKWARD, FFTW_ESTIMATE)
    



    call trace_exit("fft_init")

  end subroutine fft_init


  subroutine fft_3d(in_grid, grid, dir)
    !==============================================================================!
    !                                 F F T _ 3 D                                  !
    !==============================================================================!
    ! Subroutine for the fftw3 FFT 3d                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           in_gri,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:), intent(inout)    :: in_grid
    character(4),optional, intent(in)           :: grid
    integer, optional, intent(inout)            :: dir


    call trace_entry("fft_3d")

    if (.not.present(dir))dir=1



    if (.not.allocated(indata) .or. .not.allocated(outdata) &
         .or..not.allocated(indata_fine) .or..not.allocated(outdata_fine)) then
       call io_errors('FFTs not correctly initialised')
    end if
    ! Zero them again just in case
    indata(:,:,:) = 0.0_dp
    outdata(:,:,:) = 0.0_dp
    indata_fine(:,:,:) = 0.0_dp
    outdata_fine(:,:,:) = 0.0_dp


    select case(trim(grid))
    case('STD')



       ! First we reshape the grid
       indata = reshape(in_grid,(/loc_nx,loc_ny,loc_nz/),order=(/3,2,1/))

       ! perform the fft
       if (dir.eq.1)then
          call fftw_execute_dft(plan_std_fwd,indata,outdata)
       else if (dir.eq.-1)then
          call fftw_execute_dft(plan_std_bwd,indata,outdata)          
       else
          call io_errors("direction must be +1 or -1")
       end if


       ! reshape it again (has to be done in two steps to account for the C ordering
       in_grid = reshape(reshape(outdata,(/loc_nz,loc_ny,loc_nx/),order=(/3,2,1/)),shape=(/loc_nz*loc_ny*loc_nx/))

    case('FINE')

       ! First we reshape the grid
       indata_fine = reshape(in_grid,(/loc_nx_fine,loc_ny_fine,loc_nz_fine/),order=(/3,2,1/))

       ! perform the fft
       if (dir.eq.1)then
          call fftw_execute_dft(plan_fine_fwd,indata_fine,outdata_fine)
       else if (dir.eq.-1)then
          call fftw_execute_dft(plan_fine_bwd,indata_fine,outdata_fine)          
       else
          call io_errors("direction must be +1 or -1")
       end if


       ! reshape it again (has to be done in two steps to account for the C ordering
       in_grid = reshape(reshape(outdata_fine,(/loc_nz_fine,loc_ny_fine,loc_nx_fine/),order=(/3,2,1/)),&
            & shape=(/loc_nz_fine*loc_ny_fine*loc_nx_fine/))
       !in_grid = reshape(reshape(outdata_fine, SHAPE=(/loc_nz_fine, loc_ny_fine, loc_nx_fine/), order=(/3, 2, 1/)), shape=[loc_nz_fine * loc_ny_fine * loc_nx_fine])


    case default
       call io_errors("unknown grid")
    end select



    call trace_exit("fft_3d")
  end subroutine fft_3d



  subroutine fft_1d(in_grid, grid, dir)
    !==============================================================================!
    !                                 F F T _ 3 D                                  !
    !==============================================================================!
    ! Subroutine for the fftw3 FFT 1d                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           in_gri,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    complex(dp), dimension(:), intent(inout)    :: in_grid
    character(*),optional, intent(in)          :: grid
    integer, optional, intent(inout)            :: dir


    call trace_entry("fft_1d")

    if (.not.present(dir))dir=1



    if (.not.allocated(indata_1d) .or. .not.allocated(outdata_1d) &
         .or..not.allocated(indata_fine_1d) .or..not.allocated(outdata_fine_1d)) then
       call io_errors('FFTs not correctly initialised')
    end if

    ! Zero them again just in case
    indata_1d(:) = 0.0_dp
    outdata_1d(:) = 0.0_dp
    indata_fine_1d(:) = 0.0_dp
    outdata_fine_1d(:) = 0.0_dp



    select case(trim(grid))
    case('STD')

       indata_1d = in_grid
       ! perform the fft
       if (dir.eq.1)then
          call fftw_execute_dft(plan_std_fwd_1d,indata_1d,outdata_1d)
       else if (dir.eq.-1)then
          call fftw_execute_dft(plan_std_bwd_1d,indata_1d,outdata_1d)          
       else
          call io_errors("direction must be +1 or -1")
       end if
       
       in_grid=outdata_1d
    case('FINE')

       indata_fine_1d = in_grid
       ! perform the fft
       if (dir.eq.1)then
          call fftw_execute_dft(plan_fine_fwd_1d,indata_fine_1d,outdata_fine_1d)
       else if (dir.eq.-1)then
          call fftw_execute_dft(plan_fine_bwd_1d,indata_fine_1d,outdata_fine_1d)          
       else
          call io_errors("direction must be +1 or -1")
       end if

       in_grid=outdata_fine_1d
    case default
       call io_errors("unknown grid")
    end select



    call trace_exit("fft_1d")
  end subroutine fft_1d

end module fft
