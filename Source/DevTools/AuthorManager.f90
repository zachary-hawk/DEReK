program AuthorsManager
  implicit none
  character(len=100) :: command
  character(len=50) :: t, ini, sur
  integer :: iargc, i, ios, index_to_delete, nauth
  character(len=200) :: line
  character(len=50), dimension(:), allocatable :: title    !! These are all allocated in read_authors
  character(len=50), dimension(:), allocatable :: initials
  character(len=50), dimension(:), allocatable :: surname
  character(len=50), dimension(:), allocatable :: names

  ! Define constants for file name
  character(len=*), parameter :: filename = 'AUTHORS'

  ! Get command line arguments count
  iargc = command_argument_count()

  if (iargc == 0) then
     call print_help()
     stop
  end if

  ! Get the command line argument
  call get_command_argument(1, command)

  select case (trim(command))
  case ('-h', '--help')
     call print_help()
  case ('-I', '--inspect')
     call inspect_authors()
  case ('-a', '--add')
     if (iargc /= 4) then
        print *, 'Error: The --add option requires a <title> <initials> <surname>'
        call print_help()
        stop
     end if
     ! Get the author's details from the command line arguments
     call get_command_argument(2, t)
     call get_command_argument(3, ini)
     call get_command_argument(4, sur)
     call add_author(trim(t), trim(ini), trim(sur))
  case ('-d', '--delete')
     if (iargc /= 2) then
        print *, 'Error: The --delete option requires an index'
        call print_help()
        stop
     end if
     call get_command_argument(2, line)
     read(line, *, iostat=ios) index_to_delete
     if (ios /= 0) then
        print *, 'Error: Invalid index'
        stop
     end if
     call delete_author(index_to_delete)
  case default
     print *, 'Error: Unknown command ', trim(command)
     call print_help()
     stop
  end select

contains

  subroutine print_help()
    print *, 'Usage: AuthorsManager [options]'
    print *, 'Options:'
    print *, '  -h, --help      Print this help information'
    print *, '  -I, --inspect   Print the current contents of the AUTHORS file'
    print *, '  -a, --add       Add a new author to the AUTHORS file in the format <title> <initials> <surname>'
    print *, '  -d, --delete    Delete an author from the AUTHORS file by index'
  end subroutine print_help

  subroutine inspect_authors()
    integer :: nauth, i
    character(50), dimension(:), allocatable :: title
    character(50), dimension(:), allocatable :: initials
    character(50), dimension(:), allocatable :: surname
    character(50), dimension(:), allocatable :: names

    call read_authors(filename, title, initials, surname, names, nauth)
    if (nauth > 0) then
       print *, 'Contents of AUTHORS file:'
       do i = 1, nauth
          !print *, 'Title: ', title(i)
          !print *, 'Initials: ', initials(i)
          !print *, 'Surname: ', surname(i)
          write(*,'(T4,i0,") ",a)') i, names(i)
       end do
    else
       print *, 'No authors found or unable to read file.'
    end if
  end subroutine inspect_authors

  subroutine add_author(t, ini, sur)
    character(len=*), intent(in) :: t, ini, sur
    integer :: ios, nauth, i
    character(50), dimension(:), allocatable :: title_new
    character(50), dimension(:), allocatable :: initials_new
    character(50), dimension(:), allocatable :: surname_new
    character(50), dimension(:), allocatable :: names_new

    call read_authors(filename, title, initials, surname, names, nauth)

    ! Add the new author
    nauth = nauth + 1
    allocate(title_new(nauth), initials_new(nauth), surname_new(nauth), names_new(nauth))
    if (allocated(title))then ! Must be old titles, so fill the new array with the old       
       do i=1,nauth-1
          title_new(1:nauth-1) = title(1:nauth-1)
          initials_new(1:nauth-1) = initials(1:nauth-1)
          surname_new(1:nauth-1) = surname(1:nauth-1)
          names_new(1:nauth-1) = names(1:nauth-1)
       end do
    end if
    
    title_new(nauth) = trim(adjustl(t)) // '.'
    initials_new(nauth) = adjust_initials(trim(adjustl(ini)))
    surname_new(nauth) = trim(adjustl(sur))
    names_new(nauth) = trim(title_new(nauth)) // ' ' // trim(initials_new(nauth)) // ' ' // trim(surname_new(nauth))


    
    ! Write the updated list to the file
    OPEN(UNIT=10, FILE=filename, STATUS='REPLACE', ACCESS='SEQUENTIAL', FORM='UNFORMATTED', IOSTAT=ios,action='Write')

    !open(unit=10, file=filename, status='replace', access='direct', form='unformatted', action='write', iostat=ios,RECL=8192)
    if (ios /= 0) then
       print *, 'Error: Could not open AUTHORS file for writing.'
       return
    end if

    write(10) nauth
    if (ios /= 0) then
       print *, 'Error: Could not write the number of authors.'
       close(10)
       return
    end if

    do i = 1, nauth
       write(10, iostat=ios) title_new(i)
       write(10, iostat=ios) initials_new(i)
       write(10, iostat=ios) surname_new(i)
       if (ios /= 0) then
          print *, 'Error: Could not write author data.'
          close(10)
          return
       end if
    end do

    close(10)
    print *, 'Author added: ', trim(names_new(nauth))
  end subroutine add_author

  subroutine delete_author(index_to_delete)
    integer, intent(in) :: index_to_delete
    integer :: ios, nauth, i
    character(50), dimension(:), allocatable :: title
    character(50), dimension(:), allocatable :: initials
    character(50), dimension(:), allocatable :: surname
    character(50), dimension(:), allocatable :: names

    call read_authors(filename, title, initials, surname, names, nauth)

    ! Check if the index to delete is valid
    if (index_to_delete < 1 .or. index_to_delete > nauth) then
       print *, 'Error: Invalid index ', index_to_delete
       return
    end if

    ! Shift the remaining authors up in the array
    do i = index_to_delete, nauth-1
       title(i) = title(i+1)
       initials(i) = initials(i+1)
       surname(i) = surname(i+1)
       names(i) = names(i+1)
    end do

    ! Decrease the author count
    nauth = nauth - 1

    ! Write the updated list to the file
        ! Write the updated list to the file
    OPEN(UNIT=10, FILE=filename, STATUS='REPLACE', ACCESS='SEQUENTIAL', FORM='UNFORMATTED', IOSTAT=ios,action='WRITE')
    if (ios /= 0) then
       print *, 'Error: Could not open AUTHORS file for writing.'
       return
    end if

    write(10, iostat=ios) nauth
    if (ios /= 0) then
       print *, 'Error: Could not write the number of authors.'
       close(10)
       return
    end if

    do i = 1, nauth
       write(10, iostat=ios) title(i)
       write(10, iostat=ios) initials(i)
       write(10, iostat=ios) surname(i)
       if (ios /= 0) then
          print *, 'Error: Could not write author data.'
          close(10)
          return
       end if
    end do

    close(10)
    print *, 'Author at index ', index_to_delete, ' deleted.'
  end subroutine delete_author

  subroutine read_authors(filename, title, initials, surname, names, nauth)
    implicit none
    character(len=*), intent(in) :: filename
    character(50), dimension(:), allocatable :: title
    character(50), dimension(:), allocatable :: initials
    character(50), dimension(:), allocatable :: surname
    character(50), dimension(:), allocatable :: names
    integer, intent(out) :: nauth

    integer :: ios, i
    character(50) :: t, ini, sur

    ! Open the file for reading
    
    OPEN(UNIT=10, FILE=filename, STATUS='old', ACCESS='SEQUENTIAL', FORM='UNFORMATTED', IOSTAT=ios,action='read')
    if (ios /= 0) then
       print *, 'Error: Could not open AUTHORS file for reading.'
       nauth = 0
       return
    end if

    ! Read the number of authors
    read(10, iostat=ios) nauth
    if (ios /= 0) then
       print *, 'Error: Could not read the number of authors.'
       nauth = 0
       close(10)
       return
    end if

    ! Allocate arrays for title, initials, surname, and names
    allocate(title(nauth))
    allocate(initials(nauth))
    allocate(surname(nauth))
    allocate(names(nauth))

    ! Read and parse each author's data
    do i = 1, nauth
       read(10, iostat=ios) t
       read(10, iostat=ios) ini
       read(10, iostat=ios) sur
       if (ios /= 0) then
          print *, 'Error: Could not read author data.'
          nauth = 0
          close(10)
          return
       end if
       title(i) = trim(adjustl(t))
       initials(i) = trim(adjustl(ini))
       surname(i) = trim(adjustl(sur))
       names(i) = trim(title(i)) // ' ' // trim(initials(i)) // ' ' // trim(surname(i))
    end do

    close(10)
  end subroutine read_authors


  function adjust_initials(initials) result(adjusted)
    character(len=*), intent(in) :: initials
    character(len=len(initials)*2) :: adjusted
    character(1) :: init
    integer :: j, len_initials

    adjusted = ''
    len_initials = len(trim(initials))

    do j = 1, len_initials
       init = initials(j:j)
       adjusted(2*j-1:2*j-1)=init
       adjusted(2*j:2*j)='.'
       !adjusted = trim(adjusted) // initials(2*j-1:2*j-1) // '.'
       !print*,'j',j,trim(adjusted)
       !if (j < len_initials) adjusted = trim(adjusted) // '.'
    end do
    ! Remove the extra dot at the end if it exists
    !if (len(adjusted) > 0 .and. adjusted(len(adjusted):len(adjusted)) == '.') &
    !     adjusted = adjusted(1:len(adjusted)-1)

  end function adjust_initials



end program AuthorsManager
