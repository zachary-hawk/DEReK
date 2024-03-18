program add_params
  use iso_fortran_env, only : real64
  implicit none
  integer,parameter :: dp=real64

  integer :: i, j, k, io_unit,io_buff,stat,i_line
  character(len=300) :: line, action,end

  integer           :: max_param

  character(len=60) :: junk,junk2,junk3,junk4,var_name,key_name
  character(len=120):: descr
  character(len=30) :: allowed
  ! types of data ::
  character(len=1) :: dat_type ! R, L, C or I

  character(len=30) :: char_var
  real(dp)          :: real_var
  integer           :: int_var
  logical           :: logi_var
  integer           :: data_cat



  ! write to the terminal to get the data
  write(*,*) "Type variable name:"
  read(*,*) var_name
  write(*,*) "Type variable key:"
  read(*,*) key_name
  write(*,*) "Type data type (allowed: I,C,R or L)"
  read(*,*) dat_type
  write(*,*) "Type parameter description:"
  read(*,'(a)') descr
  write(*,*) "Type the default value"


!!$  var_name="help"
!!$  key_name="help_key"
!!$  dat_type="R"
!!$  descr="A description"
!!$  real_var=3.6_dp
!!$  allowed="(any real) > 0"

  if (dat_type.eq."C")then
     read(*,*) char_var
  else if (dat_type.eq."I")then
     read(*,*) int_var
  else if (dat_type.eq."R")then
     read(*,*) real_var
  else if (dat_type.eq."L")then
     read(*,*)logi_var
  else
     write(*,*) "Unknown data type"
     stop
  end if
  write(*,*) "Type allowed values:"
  read(*,'(A)') allowed

  write(*,*) "Type data category (1=Fundamental,2=Planewaves,3=Minimisation,4=Potential,5=Wavefunction,6=I/O,7=Misc,8=Advanced)"
  read(*,*) data_cat


  ! open the io files
  open(newunit=io_unit,access="STREAM",form="FORMATTED",file="../Source/io.f90",status="OLD",action="READ")
  open(newunit=io_buff,access="STREAM",form="FORMATTED",file="../Source/io_add_params.f90",status="UNKNOWN")

  stat=0
  i_line=0
  do while (stat.eq.0)
     i_line=i_line+1
     read(io_unit,'(a)',iostat=stat) line
     if (index(line,"%End:").ne.0)then
        read(line,*) junk, end, action
        if (trim(action).eq."parameters")then

           select case(dat_type)
           case("L")
              if (logi_var)then             
                 write(io_buff,*) "logical :: ",trim(var_name)," = ",".true."
              else
                 write(io_buff,*) "logical :: ",trim(var_name)," = ",".false."
              end if
           case("R")
              write(io_buff,*) "real(dp) :: ",trim(var_name)," = ",real_var
           case("I")
              write(io_buff,*) "integer :: ",trim(var_name)," = ",int_var
           case("C")
              write(io_buff,*) "character(len=30) :: ",trim(var_name)," = '",trim(char_var),"'"
           end select

           write(io_buff,*)trim(line)


        else if( trim(action).eq."max_param")then
           backspace(io_unit)
           backspace(io_unit)
           read(io_unit,*,iostat=stat)junk,junk2,max_param
           read(io_unit,*)junk
           max_param=max_param+1

           backspace(io_buff)
           write(io_buff,*)"integer,parameter::max_keys=",max_param
           write(io_buff,*)" ! %End: max_param"

        else if (trim(action).eq."keys") then
           write(io_buff,*)"character(len=30),parameter,public ::","key_"//trim(var_name),"   = ","'",trim(key_name),"'"
           write(io_buff,*)trim(line)
        else if(trim(action).eq."assign_keys")then
           write(junk,*)max_param
           write(io_buff,*) "keys_array(",trim(adjustl(junk)),")=trim(","key_"//trim(var_name),")"
           write(io_buff,*)trim(line)
        else if (trim(action).eq."assign_default")then
           write(io_buff,*) "write(junk,*)current_params%"//trim(var_name)
           write(junk,*)max_param
           write(io_buff,*)"keys_default(",trim(adjustl(junk)),")=trim(adjustl(junk))"
           write(io_buff,*)trim(line)
        else if(trim(action).eq."assign_description") then
           write(junk,*)max_param
           write(io_buff,*) "keys_description(",trim(adjustl(junk)),")=","'",trim(descr),"'"
           write(io_buff,*)trim(line)
        else if(trim(action).eq."assign_allowed") then
           write(junk,*)max_param
           write(io_buff,*) "keys_allowed(",trim(adjustl(junk)),")=","'",trim(allowed),"'"
           write(io_buff,*)trim(line)
        else if (trim(action).eq."case_read") then
           write(io_buff,*)"case(key_"//trim(var_name)//")"
           write(io_buff,*)"read(param,*,iostat=stat) dummy_params%"//trim(var_name)
           write(io_buff,*)'if (stat.ne.0) call io_errors("Error in I/O: Error parsing value: "//param)'
           write(io_buff,*)"present_array(i)=key"
           write(io_buff,*)"! %End: case_read"

        else if (trim(action).eq."assign_cats")then
           write(junk,*)max_param
           write(io_buff,*) "keys_cat(",trim(adjustl(junk)),")=",data_cat
           write(io_buff,*)trim(line)
        end if
     else
        write(io_buff,*)trim(line)


     end if



  end do


  close(io_buff)
  close(io_buff)
end program add_params
