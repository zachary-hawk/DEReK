module license
  use trace, only : trace_entry, trace_exit
  use sys, only : current_sys,sys_init
  use comms
contains


  subroutine license_header(stdout,version)
    !==============================================================================!
    !                              I O _ H E A D E R                               !
    !==============================================================================!
    ! Subroutine for writing out the DEREK license to whatever file is given        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    integer     , intent(in) :: stdout
    character(*), intent(in) :: version
    integer :: llen,aulen
       
    call trace_entry('license_header')
    ! call to sys init
    call sys_init()

    aulen = len_trim(current_sys%names(1)) + 8
    llen = (65-aulen) /2


    
    
    write(stdout,*) "+==================================================================+"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|  oooooooooo.   oooooooooooo ooooooooo.             oooo    oooo  |"
    write(stdout,*) "|  `888'   `Y8b  `888'     `8 `888   `Y88.           `888   .8P'   |"
    write(stdout,*) "|   888      888  888          888   .d88'  .ooooo.   888  d8'     |"
    write(stdout,*) "|   888      888  888oooo8     888ooo88P'  d88' `88b  88888[       |"
    write(stdout,*) '|   888      888  888    "     888`88b.    888ooo888  888`88b.     |'
    write(stdout,*) "|   888     d88'  888       o  888  `88b.  888    .o  888  `88b.   |"
    write(stdout,*) "|  o888bood8P'   o888ooooood8 o888o  o888o `Y8bod8P' o888o  o888o  |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,*) "|                                                                  |"
    write(stdout,'(T2,"|",T28,A, 1x, A,T69 ,"|")') 'Version', trim(version)
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,*) "|     Salutations! Welcome to the Durham Electronic RElaxation     |"
    write(stdout,*) "|     (K)Code, DEReK for short! A planewave Ab Initio DFT code     |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|      This is simply a toy project and no guarantees are made     |"
    write(stdout,*) "|        about the results obtained, for reliable electronic       |"
    write(stdout,*) "|              properties the author recommends CASTEP:            |"
    write(stdout,*) "|                      http://www.castep.org                       |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|    Acknowledgements go to my PhD supervisor Prof. S. J. Clark    |"
    write(stdout,*) "|    for teaching me the inner working of a planewave DFT code.    |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|            Thesis: http://etheses.dur.ac.uk/14737/               |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|  This program is licensed under the Apache License, Version 2.0. |"
    write(stdout,*) "|            You may obtain a copy of the License at:              |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|           http://www.apache.org/licenses/LICENSE-2.0             |"
    write(stdout,*) "|                                                                  |"
    write(stdout,*) "|                      Copyright (c) 2024                          |"
    write(stdout,*) "+------------------------------------------------------------------+"
    write(stdout,'(T2,"|",a,a,T69,"|")') repeat(' ',llen),'Author: '//trim(current_sys%names(1)) 
    write(stdout,*) "+==================================================================+"


    call license_authors(stdout)
    call license_sys_info(stdout)
    !call license_flush(stdout)
    call trace_exit('license_header')

  end subroutine license_header

  subroutine license_authors(unit)
    integer,intent(in) :: unit

    ! Local variables
    character(100) :: line
    integer :: i, current_len,start,j
    logical :: new_line
    integer :: line_len = 50
    integer,dimension(:),allocatable :: line_nums
    integer,dimension(:),allocatable :: lens
    integer :: nlines
    ! Initialize variables
    current_len = 0
    new_line = .true.

    if (current_sys%nauth.gt.1)then

       allocate(lens(1:current_sys%nauth))

       write(unit,*)"|    Contributors                                                  |"
       write(unit,*)"|    ------------                                                  |"

       do i = 1,current_sys%nauth
          lens(i) = len_trim(current_sys%names(i))
       end do

       current_len = 0
       start=2
       do i=2,current_sys%nauth
          if (current_len.eq.0)then

             if (i.gt.2) then
                ! not the first name
                ! we have a zero length so we write from start to i-1

                write(line,*) (trim(current_sys%names(j))//", " ,j=start,i-2),trim(current_sys%names(i-1))
                ! write the line to the license
                write(unit,67) line

             end if
             start = i

          end if
          if (current_len + lens(i)+2 .le. line_len-2)then
             ! We want to include this name
             current_len = current_len + lens(i)+2
          else
             ! We dont want to include this line
             current_len = 0
          end if

       end  do
       
       ! Write out the final line
       write(line,*) (trim(current_sys%names(j))//", " ,j=start,current_sys%nauth-1),trim(current_sys%names(current_sys%nauth))
       write(unit,67) line
67     format(1x,"|",2x,a,T69,"|")

       write(unit,*) "+------------------------------------------------------------------+"

    end if
  end  subroutine license_authors
  subroutine license_sys_info(unit,comment)
    !==============================================================================!
    !                            I O _ S Y S _ I N F O                             !
    !==============================================================================!
    ! Subroutine for printing the system details, including compiler               !
    ! information, system architechture and CPU. Also includes date and time of    !
    ! compilation.                                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unit,              intent :: in                                    !
    !           comment,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  08/11/2023                                            !
    !==============================================================================!
    integer,intent(in) :: unit
    integer::file
    logical,optional,intent(in) :: comment
    character(1) :: comment_char
    character(100)::fmt
    character(1) :: junk
    integer :: maj,min,mic  ! The LIBXC version checking, i only want to work with one version
    call trace_entry("license_sys_info")
    comment_char = ' '
    if (present(comment))then
       if (comment)comment_char='#'
       fmt='(1x,a,1x,a,T22,":",1x,a)'
    else
       fmt='(1x,"|",1x,a,a,T22,":",1x,a,T69,"|")'
    end if

    ! Do some handling of spell check
    select case(trim(current_sys%max_lev))
    case('normal')
       current_sys%max_lev='Normal'
    case('risky')
       current_sys%max_lev='Risky'
    case('safe')
       current_sys%max_lev='Safe'
    case default
       current_sys%max_lev = 'UNKNOWN'
    end select
    
       if (.not.present(comment))then

          write(unit,*)"|                       SYSTEM INFORMATION                         |"
          write(unit,*)"+==================================================================+"
       end if
       write(unit,fmt)trim(comment_char),"Operating System ",trim(current_sys%arch)
       write(unit,fmt)trim(comment_char),"System CPU       ",trim(current_sys%cpu)
       write(unit,fmt)trim(comment_char),"Physical Cores   ",trim(current_sys%phys_cores)
       write(unit,fmt)trim(comment_char),"Logical Cores    ",trim(current_sys%logi_cores)
       write(unit,fmt)trim(comment_char),"System Memory(GB)",trim(current_sys%tot_mem)



       write(unit,fmt)trim(comment_char),"Compiler         ",trim(current_sys%compiler)
       write(unit,fmt)trim(comment_char),"Compile Date     ",trim(current_sys%date)
       write(unit,fmt)trim(comment_char),"Code Version     ",trim(current_sys%git)
       write(unit,fmt)trim(comment_char),"Optimisation     ",trim(current_sys%opt)


       write(unit,fmt)trim(comment_char),"Parallelisation  ",trim(current_sys%comms)
       if (comms_arch.eq."MPI")then
          write(unit,fmt)trim(comment_char),"MPI Version    ",trim(current_sys%comms_version)
       end if
       write(unit,fmt)trim(comment_char),"FFTW3 Version    ",trim(current_sys%ffts)
       write(unit,fmt)trim(comment_char),"OpenBLAS Version ",trim(current_sys%openblas)
       write(unit,fmt)trim(comment_char),"LibXC Version    ",trim(current_sys%libxc)
       write(unit,fmt)trim(comment_char),"CODATA Year      ",trim(current_sys%consts)
       write(unit,fmt)trim(comment_char),'Spellcheck Safety',trim(current_sys%max_lev)

       if (.not.present(comment))then
          write(unit,*)"+==================================================================+"
       end if
       write(unit,*)
       !write(unit,fmt)trim(comment_char)

       ! doing a formatted read, not great but it is a fixed format
!!$    read(current_sys%libxc,'(i1,a,i1,a,i1)')maj,junk,min,junk,mic
!!$    if (maj.ne.6 .or. min.ne.2)then
!!$       call license_errors('Unsupported LIBXC version, must be 6.2.x',.true.)
!!$    end if



       call trace_exit("license_sys_info")
     end subroutine license_sys_info

end module license
