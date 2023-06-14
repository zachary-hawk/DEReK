!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module wave
  use constants
  use trace, only : trace_entry,trace_exit
  use comms, only : rank,on_root_node,nprocs
  use io,    only : current_params,current_structure,io_errors
  use basis, only : current_basis
  use memory,only : memory_allocate,memory_deallocate


  type wavefunction
     complex(dp),dimension(:,:,:,:),allocatable   :: coeff  ! Wavefunction coefficients gvec,kpt,band,spinor
     integer                                      :: nbands
     integer                                      :: kpts
     logical                                      :: allocated=.false.
  end type wavefunction

  type wavefunction_slice
     complex(dp),dimension(:,:,:) ,allocatable    :: coeff  ! A wavefunction at a kpoint only gvec,band,spinor
     integer                                      :: kpt    ! Which kpoint the slice is for
     integer                                      :: nbands
     logical                                      :: allocated=.false.
  end type wavefunction_slice


  private


  interface wave_allocate
     module procedure wave_allocate_wfn
     module procedure wave_allocate_slice
  end interface wave_allocate

  interface wave_copy
     module procedure wave_copy_wfn_wfn
     module procedure wave_copy_slice_wfn
     module procedure wave_copy_slice_slice
     module procedure wave_copy_wfn_slice
  end interface wave_copy

  interface operator (+)
     module procedure wave_add_slice_slice
     module procedure wave_add_slice_wfn
     module procedure wave_add_wfn_slice
     module procedure wave_add_wfn_wfn
  end interface operator (+)

  interface operator (-)
     module procedure wave_sub_slice_slice
     module procedure wave_sub_slice_wfn
     module procedure wave_sub_wfn_slice
     module procedure wave_sub_wfn_wfn
  end interface operator (-)

  interface operator (*)
     module procedure wave_scale_wfn_real
     module procedure wave_scale_wfn_complex
     module procedure wave_scale_slice_real
     module procedure wave_scale_slice_complex
  end interface operator (*)


  interface wave_initialise
     module procedure wave_initialise_wfn
  end interface wave_initialise

  public wave_allocate
  public wavefunction
  public wavefunction_slice
  public wave_copy
  public wave_initialise
  public operator (+)
  public operator (-)
  public operator (*)
contains

  subroutine wave_allocate_slice(wfn,nbands,kpt)
    !==============================================================================!
    !                    W A V E _ A L L O C A T E _ S L I C E                     !
    !==============================================================================!
    ! Subroutine for allocating a wavefunction a wavefunction slice                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: inout                                 !
    !           nbands,            intent :: in                                    !
    !           kpt,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice), intent(inout) :: wfn
    integer, intent(in) :: nbands
    integer,intent(in)  :: kpt
    call trace_entry("wave_allocate_slice")
    call memory_deallocate(wfn%coeff,'W')
    call memory_allocate(wfn%coeff,1,current_basis%num_grid_points,&
         & 1,nbands,1,2,'W')
    wfn%kpt=kpt
    wfn%nbands=nbands
    wfn%allocated=.true.

    ! zero coefficients
    wfn%coeff(:,:,:)=cmplx_0
    call trace_exit("wave_allocate_slice")
    return
  end subroutine wave_allocate_slice

  subroutine wave_allocate_wfn(wfn,nbands)
    !==============================================================================!
    !                      W A V E _ A L L O C A T E _ W F N                       !
    !==============================================================================!
    ! Subroutine for allocating a wavefunction a wavefunction                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: inout                                 !
    !           nbands,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction), intent(inout) :: wfn
    integer, intent(in) :: nbands

    call trace_entry("wave_allocate_wfn")
    call memory_deallocate(wfn%coeff,'W')
    call memory_allocate(wfn%coeff,1,current_basis%num_grid_points,1,current_structure%num_kpoints,1,nbands,1,2,'W')
    wfn%nbands=nbands
    wfn%allocated=.true.
    ! zero coefficients
    wfn%coeff(:,:,:,:)=cmplx_0
    wfn%kpts=current_structure%num_kpoints
    call trace_exit("wave_allocate_wfn")
    return
  end subroutine wave_allocate_wfn



  function wave_add_slice_slice(slice1,slice2) result(slice_out)
    !==============================================================================!
    !                   W A V E _ A D D _ S L I C E _ S L I C E                    !
    !==============================================================================!
    ! Overloaded function for adding a slice to a slice                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice1,            intent :: in                                    !
    !           slice2,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           slice_out                                                          !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice) :: slice_out
    type(wavefunction_slice),intent(in) :: slice1
    type(wavefunction_slice),intent(in) :: slice2
    call trace_entry('wave_add_slice_slice')

    !check if allocated
    if (.not.slice1%allocated .or. .not.slice2%allocated)then
       call io_errors('Error in wave_add_slice_slice: slices not allocated')
    end if


    if (slice1%nbands.ne.slice2%nbands)then
       call io_errors("Error in wave_add_slice_slice: incompatible bands")
    end if

    !check that theyre for the same kpoint
    if (slice1%kpt.ne.slice2%kpt)then
       call io_errors('Error in wave_add_slice_slice: incompatible kpoints')
    end if

    !allocate the result
    call wave_allocate(slice_out,slice1%nbands,slice1%kpt)

    !Now we have compatible shapes
    slice_out%coeff(:,:,:) = slice1%coeff(:,:,:) + slice2%coeff(:,:,:)


    call trace_exit('wave_add_slice_slice')



  end function wave_add_slice_slice


  function wave_add_slice_wfn(slice,wfn) result(wfn_out)
    !==============================================================================!
    !                     W A V E _ A D D _ S L I C E _ W F N                      !
    !==============================================================================!
    ! Overloaded function for adding a slice to a wavefunction                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice,             intent :: in                                    !
    !           wfn,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    type(Wavefunction),intent(in) :: wfn
    type(wavefunction):: wfn_out

    call trace_entry('wave_add_slice_wfn')

    !check if allocated
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_add_slice_wfn: slices not allocated')
    end if


    if (slice%nbands.ne.wfn%nbands)then
       call io_errors("Error in wave_add_slice_wfn: incompatible bands")
    end if

    if (slice%kpt.gt.wfn%kpts)then
       call io_errors('Error in wave_add_slice_wfn: slice kpt out of range')
    end if
    !allocate the result
    call wave_allocate(wfn_out,slice%nbands)

    wfn_out%coeff(:,slice%kpt,:,:) = slice%coeff(:,:,:) + wfn%coeff(:,slice%kpt,:,:)

    call trace_exit('wave_add_slice_wfn')


  end function wave_add_slice_wfn


  function wave_add_wfn_slice(wfn,slice) result(wfn_out)
    !==============================================================================!
    !                     W A V E _ A D D _ W F N _ S L I C E                      !
    !==============================================================================!
    ! Overloaded function for adding a wavefunction to a slice                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: in                                    !
    !           slice,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    type(Wavefunction),intent(in) :: wfn
    type(wavefunction):: wfn_out

    call trace_entry('wave_add_wfn_slice')

    !check if allocated
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_add_wfn_slice: slices not allocated')
    end if


    if (slice%nbands.ne.wfn%nbands)then
       call io_errors("Error in wave_add_wfn_slice: incompatible bands")
    end if

    if (slice%kpt.gt.wfn%kpts)then
       call io_errors('Error in wave_add_wfn_wfn: slice kpt out of range')
    end if
    !allocate the result
    call wave_allocate(wfn_out,slice%nbands)

    wfn_out%coeff(:,slice%kpt,:,:) = slice%coeff(:,:,:) + wfn%coeff(:,slice%kpt,:,:)

    call trace_exit('wave_add_wfn_slice')


  end function wave_add_wfn_slice




  function wave_add_wfn_wfn(wfn1,wfn2) result(wfn_out)
    !==============================================================================!
    !                       W A V E _ A D D _ W F N _ W F N                        !
    !==============================================================================!
    ! Overloaded function for adding a wavefunction to a wavefunction              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn1,              intent :: in                                    !
    !           wfn2,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction),intent(in) :: wfn1
    type(Wavefunction),intent(in) :: wfn2
    type(wavefunction):: wfn_out

    call trace_entry('wave_add_wfn_wfn')

    !check if allocated
    if (.not.wfn1%allocated .or. .not.wfn2%allocated)then
       call io_errors('Error in wave_add_wfn_wfn: slices not allocated')
    end if


    if (wfn1%nbands.ne.wfn2%nbands)then
       call io_errors("Error in wave_add_wfn_wfn: incompatible bands")
    end if

    !allocate the result
    call wave_allocate(wfn_out,wfn1%nbands)

    wfn_out%coeff(:,:,:,:) = wfn1%coeff(:,:,:,:) + wfn2%coeff(:,:,:,:)

    call trace_exit('wave_add_wfn_wfn')


  end function wave_add_wfn_wfn


  function wave_sub_slice_slice(slice1,slice2) result(slice_out)
    !==============================================================================!
    !                   W A V E _ S U B _ S L I C E _ S L I C E                    !
    !==============================================================================!
    ! Overloaded function for subtracting a slice from a slice                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice1,            intent :: in                                    !
    !           slice2,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           slice_out                                                          !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice) :: slice_out
    type(wavefunction_slice),intent(in) :: slice1
    type(wavefunction_slice),intent(in) :: slice2
    call trace_entry('wave_sub_slice_slice')

    !check if allocated
    if (.not.slice1%allocated .or. .not.slice2%allocated)then
       call io_errors('Error in wave_sub_slice_slice: slices not allocated')
    end if


    if (slice1%nbands.ne.slice2%nbands)then
       call io_errors("Error in wave_sub_slice_slice: incompatible bands")
    end if

    !check that theyre for the same kpoint
    if (slice1%kpt.ne.slice2%kpt)then
       call io_errors('Error in wave_sub_slice_slice: incompatible kpoints')
    end if

    !allocate the result
    call wave_allocate(slice_out,slice1%nbands,slice1%kpt)

    !Now we have compatible shapes
    slice_out%coeff(:,:,:) = slice1%coeff(:,:,:) - slice2%coeff(:,:,:)


    call trace_exit('wave_sub_slice_slice')



  end function wave_sub_slice_slice


  function wave_sub_slice_wfn(slice,wfn) result(wfn_out)
    !==============================================================================!
    !                     W A V E _ S U B _ S L I C E _ W F N                      !
    !==============================================================================!
    ! Overloaded function for subtracting a slice from a wavefunction              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice,             intent :: in                                    !
    !           wfn,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    type(Wavefunction),intent(in) :: wfn
    type(wavefunction):: wfn_out

    call trace_entry('wave_sub_slice_wfn')

    !check if allocated
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_sub_slice_wfn: slices not allocated')
    end if


    if (slice%nbands.ne.wfn%nbands)then
       call io_errors("Error in wave_sub_slice_wfn: incompatible bands")
    end if

    if (slice%kpt.gt.wfn%kpts)then
       call io_errors('Error in wave_sub_slice_wfn: slice kpt out of range')
    end if
    !allocate the result
    call wave_allocate(wfn_out,slice%nbands)

    wfn_out%coeff(:,slice%kpt,:,:) = slice%coeff(:,:,:) - wfn%coeff(:,slice%kpt,:,:)

    call trace_exit('wave_sub_slice_wfn')


  end function wave_sub_slice_wfn


  function wave_sub_wfn_slice(wfn,slice) result(wfn_out)
    !==============================================================================!
    !                     W A V E _ S U B _ W F N _ S L I C E                      !
    !==============================================================================!
    ! Overloaded function for subtracting a wavefuntion from a slice               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: in                                    !
    !           slice,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    type(Wavefunction),intent(in) :: wfn
    type(wavefunction):: wfn_out

    call trace_entry('wave_sub_wfn_slice')

    !check if allocated
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_sub_wfn_slice: slices not allocated')
    end if


    if (slice%nbands.ne.wfn%nbands)then
       call io_errors("Error in wave_sub_wfn_slice: incompatible bands")
    end if

    if (slice%kpt.gt.wfn%kpts)then
       call io_errors('Error in wave_sub_wfn_wfn: slice kpt out of range')
    end if
    !allocate the result
    call wave_allocate(wfn_out,slice%nbands)

    wfn_out%coeff(:,slice%kpt,:,:) = - slice%coeff(:,:,:) + wfn%coeff(:,slice%kpt,:,:)

    call trace_exit('wave_sub_wfn_slice')


  end function wave_sub_wfn_slice




  function wave_sub_wfn_wfn(wfn1,wfn2) result(wfn_out)
    !==============================================================================!
    !                       W A V E _ S U B _ W F N _ W F N                        !
    !==============================================================================!
    ! Overloaded function for subtracting a wavefunction from a wavefunction       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn1,              intent :: in                                    !
    !           wfn2,              intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction),intent(in) :: wfn1
    type(Wavefunction),intent(in) :: wfn2
    type(wavefunction):: wfn_out

    call trace_entry('wave_sub_wfn_wfn')

    !check if allocated
    if (.not.wfn1%allocated .or. .not.wfn2%allocated)then
       call io_errors('Error in wave_sub_wfn_wfn: slices not allocated')
    end if


    if (wfn1%nbands.ne.wfn2%nbands)then
       call io_errors("Error in wave_sub_wfn_wfn: incompatible bands")
    end if

    !allocate the result
    call wave_allocate(wfn_out,wfn1%nbands)

    wfn_out%coeff(:,:,:,:) = wfn1%coeff(:,:,:,:) - wfn2%coeff(:,:,:,:)

    call trace_exit('wave_sub_wfn_wfn')


  end function wave_sub_wfn_wfn

  function wave_scale_slice_real(s,slice) result(slice_out)
    !==============================================================================!
    !                  W A V E _ S C A L E _ S L I C E _ R E A L                   !
    !==============================================================================!
    ! Function for overloading intrinsic operation scaling a wavefunction by a     !
    ! real constant                                                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           slice,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           slice_out                                                          !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    real(dp),intent(in)              :: s
    type(wavefunction_slice):: slice_out

    call trace_entry('wave_scale_slice_real')

    !check if allocated
    if (.not.slice%allocated)then
       call io_errors('Error in wave_scale_slice_real: slices not allocated')
    end if

    !Allocate slice
    call wave_allocate(slice_out,slice%nbands,slice%kpt)

    slice_out%coeff(:,:,:)=s*slice%coeff(:,:,:)


    call trace_exit('wave_scale_slice_real')
  end function wave_scale_slice_real


  function wave_scale_slice_complex(s,slice) result(slice_out)
    !==============================================================================!
    !               W A V E _ S C A L E _ S L I C E _ C O M P L E X                !
    !==============================================================================!
    ! Function for overloading intrinsic operation scaling a slice by a complex    !
    ! constant                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           slice,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           slice_out                                                          !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in) :: slice
    complex(dp),intent(in)           :: s
    type(wavefunction_slice):: slice_out

    call trace_entry('wave_scale_slice_complex')

    !check if allocated
    if (.not.slice%allocated)then
       call io_errors('Error in wave_scale_slice_real: slices not allocated')
    end if

    !Allocate slice
    call wave_allocate(slice_out,slice%nbands,slice%kpt)

    slice_out%coeff(:,:,:)=s*slice%coeff(:,:,:)


    call trace_exit('wave_scale_slice_complex')
  end function wave_scale_slice_complex


  function wave_scale_wfn_real(s,wfn) result(wfn_out)
    !==============================================================================!
    !                    W A V E _ S C A L E _ W F N _ R E A L                     !
    !==============================================================================!
    ! Function for overloading intrinsic operation scaling a wavefunction by a     !
    ! real constant                                                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           wfn,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction),intent(in) :: wfn
    real(dp),intent(in)           :: s
    type(wavefunction):: wfn_out

    call trace_entry('wave_scale_wfn_real')

    !check if allocated
    if (.not.wfn%allocated)then
       call io_errors('Error in wave_scale_wfn_real: wfn not allocated')
    end if

    !Allocate sfn
    call wave_allocate(wfn_out,wfn%nbands)

    wfn_out%coeff(:,:,:,:)=s*wfn%coeff(:,:,:,:)


    call trace_exit('wave_scale_wfn_real')
  end function wave_scale_wfn_real


  function wave_scale_wfn_complex(s,wfn) result(wfn_out)
    !==============================================================================!
    !                 W A V E _ S C A L E _ W F N _ C O M P L E X                  !
    !==============================================================================!
    ! Function for overloading intrinsic operation scaling a wavefunction by a     !
    ! complex constant                                                             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           s,                 intent :: in                                    !
    !           wfn,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           wfn_out                                                            !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction),intent(in) :: wfn
    complex(dp),intent(in)           :: s
    type(wavefunction):: wfn_out

    call trace_entry('wave_scale_wfn_complex')

    !check if allocated
    if (.not.wfn%allocated)then
       call io_errors('Error in wave_scale_wfn_complex: wfns not allocated')
    end if

    !Allocate wfn
    call wave_allocate(wfn_out,wfn%nbands)

    wfn_out%coeff(:,:,:,:)=s*wfn%coeff(:,:,:,:)


    call trace_exit('wave_scale_wfn_complex')
  end function wave_scale_wfn_complex



  subroutine wave_copy_wfn_wfn(wfn1,wfn2)
    !==============================================================================!
    !                      W A V E _ C O P Y _ W F N _ W F N                       !
    !==============================================================================!
    ! Subroutine for copying a wavefunction to another wavefunction                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn1,              intent :: in                                    !
    !           wfn2,              intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction),intent(in)   :: wfn1   
    type(wavefunction),intent(inout):: wfn2

    call trace_entry('wave_copy_wfn_wfn')
    if (.not.wfn1%allocated .or. .not.wfn2%allocated)then
       call io_errors('Error in wave_copy_wfn_wfn: Wavefunction not allocated')
    end if

    wfn2%nbands=wfn1%nbands
    wfn2%kpts=wfn1%kpts
    wfn2%coeff(:,:,:,:)=wfn1%coeff(:,:,:,:)

    call trace_exit('wave_copy_wfn_wfn')
    return
  end subroutine wave_copy_wfn_wfn


  subroutine wave_copy_slice_wfn(slice,wfn)
    !==============================================================================!
    !                    W A V E _ C O P Y _ S L I C E _ W F N                     !
    !==============================================================================!
    ! Subroutine for copying a slice into a wavefunction                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice,             intent :: in                                    !
    !           wfn,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(in)   :: slice   
    type(wavefunction),intent(inout):: wfn

    call trace_entry('wave_copy_slice_wfn')
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_copy_slice_wfn: Wavefunction not allocated')
    end if

    wfn%nbands=slice%nbands
    wfn%coeff(:,slice%kpt,:,:)=slice%coeff(:,:,:)

    call trace_exit('wave_copy_slice_wfn')
    return

  end subroutine wave_copy_slice_wfn


  subroutine wave_copy_wfn_slice(wfn,slice,kpt)
    !==============================================================================!
    !                    W A V E _ C O P Y _ W F N _ S L I C E                     !
    !==============================================================================!
    ! Subroutine for copying a wavefunction at a kpt into a slice                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           wfn,               intent :: in                                    !
    !           slice,             intent :: inout                                 !
    !           kpt,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(inout)   :: slice   
    type(wavefunction),intent(in):: wfn
    integer,intent(in) :: kpt
    call trace_entry('wave_copy_wfn_slice')
    if (.not.slice%allocated .or. .not.wfn%allocated)then
       call io_errors('Error in wave_copy_wfn_slice: Wavefunction not allocated')
    end if

    slice%nbands=wfn%nbands

    slice%coeff(:,:,:)=wfn%coeff(:,kpt,:,:)

    call trace_exit('wave_copy_wfn_slice')
    return


  end subroutine wave_copy_wfn_slice

  subroutine wave_copy_slice_slice(slice1,slice2)
    !==============================================================================!
    !                  W A V E _ C O P Y _ S L I C E _ S L I C E                   !
    !==============================================================================!
    ! Subroutine for copying a slice into another slice                            !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           slice1,            intent :: in                                    !
    !           slice2,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  05/01/2022                                            !
    !==============================================================================!
    type(wavefunction_slice),intent(inout)   :: slice2   
    type(wavefunction_slice),intent(in)  ::  slice1

    call trace_entry('wave_copy_slice_slice')
    if (.not.slice1%allocated .or. .not.slice2%allocated)then
       call io_errors('Error in wave_copy_slice_slice: Wavefunction not allocated')
    end if

    slice2%nbands=slice1%nbands  
    slice2%coeff(:,:,:)=slice1%coeff(:,:,:)

    call trace_exit('wave_copy_slice_slice')
    return

  end subroutine wave_copy_slice_slice



  subroutine wave_initialise_wfn(wfn)
    type(wavefunction), intent(inout) :: wfn
    real(dp)         :: r1, r2

    integer          :: nk,nb,ng,k,g
    call trace_entry('wave_initialise_wfn')

    wfn%coeff(:,:,:,:) = 0.0_dp
    do ng=1,current_basis%num_node
       g=current_basis%local_grid_points(ng)
       do nk = 1,current_structure%max_kpoints_on_node
          k=current_structure%kpts_on_node(nk)
          do nb=1, wfn%nbands
             call random_number(r1)
             call random_number(r2)
             r1=r1-0.5_dp
             r2=r2-0.5_dp
             wfn%coeff(g,k,nb,:)=(/r1,r2/)
          end do
       end do
    end do



    call trace_exit('wave_initialise_wfn')
  end subroutine wave_initialise_wfn

end module wave
