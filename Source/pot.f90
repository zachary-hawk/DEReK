!---- File documented by Fortran Documenter, Z.Hawkhead
module pot
  use trace, only : trace_entry, trace_exit,dp
  use comms, only : rank, on_root_node, nprocs
  use io,    only : stdout,io_errors,current_params,current_structure,twopi,seed
  use memory,only : memory_allocate, memory_deallocate
  use basis, only : current_basis
  use wave,  only : wavefunction,wavefunction_slice
  private

  type,public :: potential
     complex(dp),dimension(:,:,:),allocatable :: nc_pot ! pos,(11,12)
     !     (21,22)
     logical                                  :: allocated=.false.
   contains
     procedure pot_write
     procedure pot_read
     generic :: write(unformatted) => pot_write 
     generic :: read(unformatted) => pot_read
  end type potential


  interface operator (+)
     module procedure pot_add
  end interface operator (+)
  interface operator (-)
     module procedure pot_sub
  end interface operator (-)

  interface operator (*)
     module procedure pot_scale_real
     module procedure pot_scale_complex
  end interface operator (*)

  public :: pot_allocate
  public :: pot_external_pot
  public :: operator (+)
  public :: operator (-)
  public :: operator (*)

contains


  subroutine pot_allocate(pot)
    !==============================================================================!
    !                           P O T _ A L L O C A T E                            !
    !==============================================================================!
    ! Subroutine for allocating the potential type                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    type(potential),intent(inout) :: pot

    if (pot%allocated)then
       call memory_deallocate(pot%nc_pot,'P')
    end if

    call memory_allocate(pot%nc_pot,1,current_basis%num_fine_grid_points,1,2,1,2,'P')
    pot%allocated=.true.

  end subroutine pot_allocate

  subroutine pot_external_pot(ext_pot)
    !==============================================================================!
    !                       P O T _ E X T E R N A L _ P O T                        !
    !==============================================================================!
    ! subroutine for calculating the external potential, either from a             !
    ! predefined type or reading one from a file                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           ext_pot,           intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    type(potential), intent(inout):: ext_pot

    integer :: np
    real(dp),dimension(1:3) :: temp_vec
    logical :: exists
    integer :: pot_file

    integer :: loc_ig
    logical :: preset=.true.
    real(dp)  :: bounds(1:3)
    call trace_entry('pot_external_pot')


    ! We need to know what type of external potential we are reading for
    ! Most of these cases are going to be collinear potentials
    select case(trim(current_params%external_pot))
    case ('jelly')
       ! This is the default case and easiest to set up
       ext_pot%nc_pot(:,1,1)=0.5_dp * current_params%n_electrons / current_basis%num_fine_grid_points
       ext_pot%nc_pot(:,2,2)=0.5_dp * current_params%n_electrons / current_basis%num_fine_grid_points
       ext_pot%nc_pot(:,1,2)=0.0_dp
       ext_pot%nc_pot(:,2,1)=0.0_dp

    case ('finite_barrier')
       bounds(:)=current_params%finite_barrier_width(:)/2.0_dp
       ext_pot%nc_pot(:,:,:)=0.0_dp

       do np=1,current_basis%num_fine_node
          loc_ig=current_basis%local_fine_grid_points(np)
          temp_vec(:) = current_basis%fine_frac_points(loc_ig,:) - 0.5_dp
          if (all(abs(temp_vec(:)).lt.bounds(:)))then 
             ext_pot%nc_pot(loc_ig,1,1)=0.5_dp * current_params%finite_barrier_height
             ext_pot%nc_pot(loc_ig,2,2)=0.5_dp * current_params%finite_barrier_height
          end if
       end do

    case('periodic_pot')
       ext_pot%nc_pot(:,1,2)=0.0_dp
       ext_pot%nc_pot(:,2,1)=0.0_dp

       ext_pot%nc_pot(:,1,1) = 0.5_dp * current_params%periodic_pot_amp * &
            & sin(twopi*current_params%periodic_pot_grid(1) * current_basis%fine_frac_points(:,1)) * &
            & sin(twopi*current_params%periodic_pot_grid(2) * current_basis%fine_frac_points(:,1)) * &
            & sin(twopi*current_params%periodic_pot_grid(3) * current_basis%fine_frac_points(:,1)) 

       ext_pot%nc_pot(:,2,2) = 0.5_dp * current_params%periodic_pot_amp * &
            & sin(twopi*current_params%periodic_pot_grid(1) * current_basis%fine_frac_points(:,1)) * &
            & sin(twopi*current_params%periodic_pot_grid(2) * current_basis%fine_frac_points(:,1)) * &
            & sin(twopi*current_params%periodic_pot_grid(3) * current_basis%fine_frac_points(:,1)) 


    case default
       ! This is the case where we read it in from a file
       ! If we are here, the param is not one of the defaults
       if (index(trim(current_params%external_pot),'.pot').eq.0)then
          call io_errors("Error in pot_read: potential naming convension not correct")
       end if

       inquire(file=trim(current_params%external_pot),exist=exists)
       if (.not.exists)then
          call io_errors("Error in pot_read: file "//trim(current_params%external_pot)//' does not exist')
       end if

       open(newunit=pot_file,file=trim(current_params%external_pot),form="UNFORMATTED",status='old')
       read(pot_file)ext_pot
       preset=.false.
    end select


    if (current_params%write_potential)then
       ! Now we have calculated it we can write it if needed
       if (preset)then
          open(newunit=pot_file,file=trim(seed)//'.'//trim(current_params%external_pot)//'.pot',status="unknown",form='UNFORMATTED')
       else
          open(newunit=pot_file,file=trim(seed)//'.pot',status="unknown",form='UNFORMATTED')
       end if
       write(pot_file)ext_pot

    end if


    call trace_exit('pot_external_pot')
    return
  end subroutine pot_external_pot



  subroutine pot_write(pot,unit,iostat,iomsg)
    !==============================================================================!
    !                              P O T _ W R I T E                               !
    !==============================================================================!
    ! Subroutine for writing a potential to a bin file                             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot,               intent :: in                                    !
    !           unit,              intent :: in                                    !
    !           iostat,            intent :: out                                   !
    !           iomsg,             intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    class(potential) , intent(in)    :: pot
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat
    character(len=*), intent(inout) :: iomsg

    integer :: stat,pot_file

    call trace_entry('pot_write')
    !open(newunit=pot_file,file=trim(seed)//'.pot',status="unknown",form='UNFORMATTED')
    write(unit,iostat=iostat)current_basis%ngx,current_basis%ngy,current_basis%ngz
    if (iostat.ne.0) call io_errors("Error in pot_write: unable to write to "//trim(seed)//".pot file")
    write(unit,iostat=iostat,iomsg=iomsg)pot%nc_pot,pot%allocated
    if (iostat.ne.0) call io_errors("Error in pot_write: unable to write to "//trim(seed)//".pot file")


    call trace_exit('pot_write')
  end subroutine pot_write


  subroutine pot_read(pot,unit,iostat,iomsg)
    !==============================================================================!
    !                               P O T _ R E A D                                !
    !==============================================================================!
    ! Subroutine for reading a potential from a file                               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot,               intent :: inout                                 !
    !           unit,              intent :: in                                    !
    !           iostat,            intent :: out                                   !
    !           iomsg,             intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  11/01/2022                                            !
    !==============================================================================!
    class(potential) , intent(inout) :: pot
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat
    character(len=*), intent(inout) :: iomsg

    integer :: stat,pot_file,gx,gy,gz

    call trace_entry('pot_read')
    !open(newunit=pot_file,file=trim(seed)//'.pot',status="unknown",form='UNFORMATTED')
    !write(pot_file,iostat=stat)current_basis%ngx,current_basis%ngy,current_basis%ngz
    !if (stat.ne.0) call io_errors("Error in pot_write: unable to write to "//trim(seed)//".pot file")
    read(unit,iostat=iostat,iomsg=iomsg)gx,gy,gz
    if (iostat.ne.0) call io_errors("Error in pot_read: unable to read .pot file")

    if (gx.ne.current_basis%ngx.or.gy.ne.current_basis%ngy.or.gz.ne.current_basis%ngz)then
       call io_errors("Error in pot_read: fine grid mismatch")
    end if

    read(unit,iostat=iostat,iomsg=iomsg)pot%nc_pot,pot%allocated
    if (iostat.ne.0) call io_errors("Error in pot_read: unable to read .pot file")


    call trace_exit('pot_read')
  end subroutine pot_read



  subroutine pot_apply_wfn(pot,wfn,wfn_out)
    !==============================================================================!
    !                          P O T _ A P P L Y _ W F N                           !
    !==============================================================================!
    ! Subroutine for calculating V|psi>                                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot,               intent :: in                                    !
    !           wfn,               intent :: inout                                 !
    !           wfn_out,           intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    type(potential),intent(in)                    :: pot
    type(wavefunction),intent(inout)        :: wfn
    type(wavefunction),intent(out),optional :: wfn_out

    ! counters
    integer :: ik,ig,nb
    call trace_entry('pot_apply_wfn')

    ! Boring handling stuff
    if (.not.wfn%allocated)then
       call io_errors("Error in pot_apply_wfn: wfn is not allocated")
    end if


    if (present(wfn_out))then
       ! Make sure its allocated
       if (.not.wfn_out%allocated)then
          call io_errors("Error in pot_apply_wfn: wfn_out is not allocated")
       end if



    end if

    call trace_exit('pot_apply_wfn')
  end subroutine pot_apply_wfn



  function pot_add(pot1,pot2) result(pot_out)
    !==============================================================================!
    !                                P O T _ A D D                                 !
    !==============================================================================!
    ! Overloaded function for adding two potentials                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot1,              intent :: in                                    !
    !           pot2,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           pot_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    type(potential),intent(in) :: pot1
    type(potential),intent(in) :: pot2
    type(potential)            :: pot_out


    call trace_entry("pot_add")

    if (.not.pot1%allocated)call io_errors("Error in pot_add: pot1 not allocated")
    if (.not.pot2%allocated)call io_errors("Error in pot_add: pot2 not allocated")

    if (.not.pot_out%allocated)then
       call pot_allocate(pot_out)
    end if

    pot_out%nc_pot(:,:,:) = pot1%nc_pot(:,:,:)+pot2%nc_pot(:,:,:)


    call trace_exit('pot_add')
  end function pot_add

  function pot_sub(pot1,pot2) result(pot_out)
    !==============================================================================!
    !                                P O T _ S U B                                 !
    !==============================================================================!
    ! Overloaded subroutine for subtracting two potentials                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot1,              intent :: in                                    !
    !           pot2,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           pot_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    type(potential),intent(in) :: pot1
    type(potential),intent(in) :: pot2
    type(potential)            :: pot_out


    call trace_entry("pot_sub")

    if (.not.pot1%allocated)call io_errors("Error in pot_sub: pot1 not allocated")
    if (.not.pot2%allocated)call io_errors("Error in pot_sub: pot2 not allocated")

    if (.not.pot_out%allocated)then
       call pot_allocate(pot_out)
    end if

    pot_out%nc_pot(:,:,:) = pot1%nc_pot(:,:,:)-pot2%nc_pot(:,:,:)


    call trace_exit('pot_sub')
  end function pot_sub


  function pot_scale_real(s,pot) result(pot_out)
    !==============================================================================!
    !                         P O T _ S C A L E _ R E A L                          !
    !==============================================================================!
    ! Overloaded function for multiplying a potential by a real scalar             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           pot,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           pot_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    real(dp), intent(in)       :: s
    type(potential),intent(in) :: pot
    type(potential)            :: pot_out

    call trace_entry("pot_scale_real")
    if (.not.pot%allocated)call io_errors("Error in pot_scale_real: pot not allocated")
    if (.not.pot_out%allocated)then
       call pot_allocate(pot_out)
    end if

    pot_out%nc_pot(:,:,:) = s*pot%nc_pot(:,:,:)

    call trace_exit('pot_scale_real')
  end function pot_scale_real

  function pot_scale_complex(s,pot) result(pot_out)
    !==============================================================================!
    !                      P O T _ S C A L E _ C O M P L E X                       !
    !==============================================================================!
    ! Overloaded function for multiplying a potential by a complex scalar          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           pot,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           pot_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    complex(dp), intent(in)    :: s
    type(potential),intent(in) :: pot
    type(potential)            :: pot_out
    call trace_entry("pot_scale_complex")

    if (.not.pot%allocated)call io_errors("Error in pot_scale_complex: pot not allocated")
    if (.not.pot_out%allocated)then
       call pot_allocate(pot_out)
    end if

    pot_out%nc_pot(:,:,:) = s*pot%nc_pot(:,:,:)
    call trace_exit('pot_scale_complex')
  end function pot_scale_complex


  function pot_copy(pot) result(pot_out)
    !==============================================================================!
    !                               P O T _ C O P Y                                !
    !==============================================================================!
    ! Overloaded function for copying the allocation of a potential into another   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           pot,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           pot_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  15/01/2022                                            !
    !==============================================================================!
    type(potential), intent(in) :: pot
    type(potential)             :: pot_out
    call trace_entry('pot_copy')
    if (.not.pot%allocated)call io_errors("Error in pot_copy: pot not allocated")
    if (.not.pot_out%allocated)then
       call pot_allocate(pot_out)
    end if

    pot_out%nc_pot(:,:,:) = pot%nc_pot(:,:,:)
    call trace_entry('pot_copy')
  end function pot_copy

end module pot
