program AuthorsManager
    implicit none
    character(len=100) :: command
    character(len=100) :: new_author
    integer :: iargc, i, ios, index_to_delete
    character(len=200) :: line

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
            call get_command_argument(2, new_author)
            do i = 3, iargc
                call get_command_argument(i, line)
                new_author = trim(new_author) // ' ' // trim(line)
            end do
            call add_author(new_author)
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
        character(len=200) :: line
        integer :: unit, ios
        open(unit=10, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error: Could not open AUTHORS file for reading.'
            return
        end if

        print *, 'Contents of AUTHORS file:'
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            print *, trim(line)
        end do

        close(10)
    end subroutine inspect_authors

    subroutine add_author(new_author)
        character(len=*), intent(in) :: new_author
        integer :: ios
        open(unit=10, file=filename, status='unknown', action='write', position='append', iostat=ios)
        if (ios /= 0) then
            print *, 'Error: Could not open AUTHORS file for writing.'
            return
        end if

        write(10, '(A)') trim(new_author)
        close(10)
        print *, 'Author added: ', trim(new_author)
    end subroutine add_author

    subroutine delete_author(index_to_delete)
        integer, intent(in) :: index_to_delete
        character(len=200) :: line
        integer :: unit, ios, i, count
        character(len=200), dimension(:), allocatable :: lines

        ! Open the file for reading
        open(unit=10, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error: Could not open AUTHORS file for reading.'
            return
        end if

        ! Count the number of lines (authors) in the file
        count = 0
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            count = count + 1
        end do

        ! Check if the index to delete is valid
        if (index_to_delete < 1 .or. index_to_delete > count) then
            print *, 'Error: Invalid index ', index_to_delete
            close(10)
            return
        end if

        ! Allocate array to hold the lines
        allocate(lines(count))

        ! Rewind and read the lines into the array
        rewind(10)
        do i = 1, count
            read(10, '(A)', iostat=ios) lines(i)
        end do

        close(10)

        ! Open the file for writing to overwrite the content
        open(unit=10, file=filename, status='unknown', action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'Error: Could not open AUTHORS file for writing.'
            return
        end if

        ! Write back all lines except the one to be deleted
        do i = 1, count
            if (i /= index_to_delete) then
                write(10, '(A)') trim(lines(i))
            end if
        end do

        close(10)
        print *, 'Author at index ', index_to_delete, ' deleted.'
    end subroutine delete_author

end program AuthorsManager
