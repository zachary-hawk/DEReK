module xsf
  use trace, only : trace_entry,trace_exit
  use comms
  use constants
  use io, only : current_structure, current_params,io_errors
  use basis, only : current_basis
  use units
  use memory, only: memory_allocate, memory_deallocate
  implicit none

  interface xsf_write
     module procedure xsf_write_pot
  end interface xsf_write  

contains

  subroutine xsf_write_pot(unit,data,comp)
    
    integer,intent(in)                                  :: unit
    complex(dp),dimension(:,:,:),allocatable,intent(in) :: data
    character(1), intent(in)                            :: comp

    ! The potentials should always be on the fine grid! 
    integer :: loc_nx,loc_ny,loc_nz
    real(dp),dimension(:), allocatable :: data_local
    logical :: opened

    call trace_entry('xsf_write_pot')

    ! Let us do some checking 

    ! Check file is open
    inquire(unit=unit,opened=opened)
    if (.not.opened)call io_errors('XSF potential file is not open')

          
    ! allocate local memory, remember that this is fine grid
    call memory_allocate(data_local,1,current_basis%num_fine_grid_points,'P')


    ! Time to calculate the components
    
    select case(comp)
    case('v','V')
       data_local(:) =(data(:,1,1)  + data(:,2,2) )/2.0_dp
    case('x','X')
       data_local(:) = (data(:,1,2) + data(:,2,1))/(mu_b*2.0_dp)
    case('y','Y')
       data_local(:) = (data(:,1,2) - data(:,2,1))/(-mu_b*cmplx_2i)
    case('z','Z')
       data_local(:) = (data(:,1,1) - data(:,2,2)) /(2.0_dp*mu_b)
       
    case default
       call io_errors('Invalid component for xsf potential writing: '//comp)
    end select

    loc_nx = current_basis%fine_ngx
    loc_ny = current_basis%fine_ngy
    loc_nz = current_basis%fine_ngz

    

    ! Now actually make the call to the xsf_write_gen

    call  xsf_write_gen(unit,data_local,loc_nx,loc_ny,loc_nz)
    

    call memory_deallocate(data_local,'P')
    call trace_exit('xsf_write_pot')
    return
  end subroutine xsf_write_pot
  
  subroutine xsf_write_gen(file_unit,data,nx,ny,nz)

    integer, intent(in) :: file_unit
    real(dp), intent(in) :: data(:)
    integer,intent(in) :: nx, ny, nz
    ! Global variables (assumed to be declared elsewhere)
    
    
    

    integer :: i, j, k, idx

    ! Write header
    write(file_unit, '(A)') "CRYSTAL"
    write(file_unit, '(A)') "PRIMVEC"

    ! Write primitive cell vectors
    do i = 1, 3
       write(file_unit, '(3F20.10)') current_structure%cell(i, 1), current_structure%cell(i, 2), current_structure%cell(i, 3)
    end do

    write(file_unit, '(A)') "PRIMCOORD"
    write(file_unit, '(I5, I5)') 1, 1  ! Placeholder for number of atoms (1) and a flag (1)
    write(file_unit, '(3F20.10)') 0.0, 0.0, 0.0  ! Placeholder for a single atom (at origin)

    write(file_unit, '(A)') "BEGIN_BLOCK_DATAGRID_3D"
    write(file_unit, '(A)') "DATAGRID_3D_UNKNOWN"
    write(file_unit, '(A)') "BEGIN_DATAGRID_3D"
    write(file_unit, '(3I5)') nx, ny, nz
    write(file_unit, '(3F20.10)') 0.0, 0.0, 0.0  ! Placeholder for a single atom (at origin)
    ! Write primitive cell vectors again for the grid
    do i = 1, 3
       write(file_unit, '(3F20.10)') current_structure%cell(i, 1), current_structure%cell(i, 2), current_structure%cell(i, 3)
    end do

    ! Write origin of the grid
    !write(file_unit, '(3F20.10)') 0.0, 0.0, 0.0

    ! Write the data in column-major order

    idx = 1
    do k = 1, nz
       do j = 1, ny
          do i = 1, nx
             write(file_unit, '(E20.10)') data(idx)
             idx = idx + 1
             !if (mod(idx-1, 5) == 0) then
             !   write(file_unit, *)  ! Write a new line every 5 values
             !end if
          end do
          !write(file_unit,*)
       end do       
       !write(file_unit,*)
    end do

    write(file_unit, '(A)') "END_DATAGRID_3D"
    write(file_unit, '(A)') "END_BLOCK_DATAGRID_3D"
  end subroutine xsf_write_gen




end module xsf
