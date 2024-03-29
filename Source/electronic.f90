!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module electronic
  use constants
  use trace, only : trace_entry,trace_exit,global_start
  use io, only: stdout,current_params,glob_line_len,io_flush,io_warnings,io_section
  use state, only: current_state
  use comms


  public ::  electronic_minimise 

contains

  subroutine electronic_minimise()

    call trace_entry('electronic_minimise')


    call electronic_init()
    
    
    call electronic_scf()

    call electronic_scf_report()
    

    call trace_exit('electronic_minimise')
  end subroutine electronic_minimise

  subroutine electronic_init()
    call trace_entry('electronic_init')

    ! We start by writing out some stuff, we want to have indicate the start of the calculation
    
    if (on_root_node)then
       call io_section(stdout,'self-consistent field')
!!$       write(stdout,*)"+------------------------------------------------------------------+"
!!$       write(stdout,*)"|             S E L F - C O N S I S T E N T  F I E L D             |"
!!$       write(stdout,*)"+------------------------------------------------------------------+"
    end if
    
    call trace_exit('electronic_init')
  end subroutine electronic_init


  subroutine electronic_scf()

    !counters
    integer :: iscf=0

    ! Time (comms_wall_time must be called by each process)
    real(dp)   :: wall_time
    ! Energy stuff
    real(dp)   :: current_energy=-1456.256345234534, energy_hist= -1451.256345234534

    call trace_entry('electronic_scf')

    current_state%total_modspin = 0e-9
    current_state%total_spin = 0e-9

    ! First we report that we are starting the SCF Calculation
    ! Wall time for the initial step:
    wall_time = comms_wall_time()

    if (on_root_node)then
       call io_flush(stdout)

       write(stdout,*)"+--------+-------------------+-------------------+---------+ <-- SCF"
       write(stdout,*)"|  SCF   |      Energy       |   Energy Change   |  Time   | <-- SCF"
       write(stdout,*)"|  Step  |       (eV)        |        (eV)       |   (s)   | <-- SCF"
       write(stdout,*)"+--------+-------------------+-------------------+---------+ <-- SCF"
       call io_flush(stdout)

       ! To start we need to report the initial energy, there will be no change
       write(stdout,11)iscf,current_energy,wall_time
    end if

    !print*,rank,'before loop'
    current_state%converged = .false.
    do iscf = 1,current_params%max_scf
       !print*,rank,'inside the loop'
       ! at the end, write out the energy

       wall_time = comms_wall_time()


       if (on_root_node)&
            & write(stdout,12)iscf,current_energy,current_energy - energy_hist,wall_time       
       !print*,'rank',rank


!!!!! At This point, the energy tolerance and the energy history should be common to all nodes!!!!
       ! Check for convergence on the root_node
       !print*,rank,current_state%converged

       if (iscf .eq. 15)current_energy=energy_hist

       if (on_root_node)then
          if (abs(current_energy-energy_hist).lt.current_params%energy_tol)then
             current_state%total_energy = current_energy
             current_state%converged =.true.
          else
             current_state%converged=.false.
          end if
       end if
       call comms_bcast(current_state%converged)
       if (current_state%converged) exit

    end  do

    if (on_root_node)&
         & write(stdout,*)"+--------+-------------------+-------------------+---------+ <-- SCF"

11  format(1x,'|',T6,I05,T11,"|",T15,ES16.8E3,T31,"|",T51,'|',T53,f8.2,T61,'| <-- SCF')
12  format(1x,'|',T6,I05,T11,"|",T15,ES16.8E3,T31,"|",T35,ES16.8E3,T51,'|',T53,f8.2,T61,'| <-- SCF')
    call trace_exit('electronic_scf')
  end subroutine electronic_scf

  subroutine electronic_scf_report()
    
    call trace_entry('electronic_scf_report')
    ! Check for convergence once we have finished the scf
    if (on_root_node)then
       if (current_state%converged)then
          ! We have converged, we are all happy  - this is good

          write(stdout,*)"|                 CONVERGED SUCCESSFULLY                   | <-- SCF"
          write(stdout,*)"+----------------------------------------------------------+ <-- SCF"
          write(stdout,111)'Total energy',current_state%total_energy, 'eV'
          write(stdout,111)'Fermi energy',current_state%efermi,       'eV'
          write(stdout,111)'Total moment',current_state%tot_moment,   'hbar/2'
          write(stdout,112)'Total spin  ',current_state%total_spin(1),&
               & current_state%total_spin(2),&
               & current_state%total_spin(3),&
               & 'hbar/2'
          write(stdout,112)'Total |spin|',current_state%total_modspin(1),&
               & current_state%total_modspin(2),&
               & current_state%total_modspin(3),&
               & 'hbar/2'

          
111       format(1x,'|',T4,a,':',T35,g21.12,T55,a,T61,'| <-- SCF')         
112       format(1x,'|',T4,a,':',T17,3(g12.4),T55,a,T61,'| <-- SCF')         
       else

          write(stdout,*)"|                   CONVERGENCE FAILED                     | <-- SCF"
          write(stdout,*)"+----------------------------------------------------------+ <-- SCF"

          call io_warnings('SCF failed to meet convergence criteria within specified number of steps')

       end if
    end if
    call io_section(stdout,'end of minimisation')
    call trace_exit('electronic_scf_report')
  end subroutine electronic_scf_report

end module electronic
