!*******************************************************************************
! Copyright 2024 Z. Hawkhead
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!*******************************************************************************
!---- File documented by Fortran Documenter, Z.Hawkhead

module electronic
  use constants
  use units
  use trace, only : trace_entry,trace_exit,global_start,trace_wallclock
  use io, only: stdout,current_params,glob_line_len,io_flush,io_warnings,io_section,units_from_atomic
  use state, only: current_state
  use comms


  public ::  electronic_minimise 

contains

  subroutine electronic_minimise()
    !==============================================================================!
    !                    E L E C T R O N I C _ M I N I M I S E                     !
    !==============================================================================!
    ! Electronic minimisation routine. This should only be a routine for calling   !
    ! other routines. There should be no major computation carried out here.       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!

    call trace_entry('electronic_minimise')


    call electronic_init()


    call electronic_scf()

    call electronic_scf_report()


    call trace_exit('electronic_minimise')
  end subroutine electronic_minimise

  subroutine electronic_init()
    !==============================================================================!
    !                        E L E C T R O N I C _ I N I T                         !
    !==============================================================================!
    ! Inititation routine for the electronic properties module, in fairness its    !
    ! probably a little pointless                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
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
    !==============================================================================!
    !                         E L E C T R O N I C _ S C F                          !
    !==============================================================================!
    ! Electronic routine for perfoming the scf minimisation. The majority of the   !
    ! calculation will be carried out in this routine.                             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!

    !counters
    integer :: iscf=0

    ! Time (trace_wallclock must be called by each process)
    real(dp)   :: wall_time
    ! Energy stuff
    real(dp)   :: current_energy=-1456.256345234534, energy_hist= -1451.256345234534

    character(2) :: unit

    call trace_entry('electronic_scf')
    unit = trim(current_params%out_energy_unit)
    
    current_state%total_modspin = 0e-9
    current_state%total_spin = 0e-9

    ! First we report that we are starting the SCF Calculation
    ! Wall time for the initial step:
    wall_time = trace_wallclock()

    if (on_root_node)then
       call io_flush(stdout)

       write(stdout,*)"+--------+-------------------+-------------------+---------+ <-- SCF"
       write(stdout,*)"|  SCF   |      Energy       |   Energy Change   |  Time   | <-- SCF"
       !write(stdout,*)"|  Step  |       (eV)        |        (eV)       |   (s)   | <-- SCF"
       write(stdout,14) unit,unit
       write(stdout,*)"+--------+-------------------+-------------------+---------+ <-- SCF"
       call io_flush(stdout)

       ! To start we need to report the initial energy, there will be no change
       write(stdout,11)iscf,units_from_atomic(current_energy,unit),wall_time
    end if

    !print*,rank,'before loop'
    current_state%converged = .false.
    do iscf = 1,current_params%max_scf
       if (iscf .eq. 15)current_energy=energy_hist
       !print*,rank,'inside the loop'
       ! at the end, write out the energy

       wall_time = trace_wallclock()


       if (on_root_node)&
            & write(stdout,12)iscf,units_from_atomic(current_energy,unit),&
            & units_from_atomic(current_energy - energy_hist,unit),&
            & wall_time       
       !print*,'rank',rank


       !**** At This point, the energy tolerance and the energy history should be common to all nodes!!!!
       ! Check for convergence on the root_node
       !print*,rank,current_state%converged



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
14  format(T2,"|  Step  |       (",a2,")        |        (",a2,")       |   (s)   | <-- SCF")
11  format(1x,'|',T6,I05,T11,"|",T15,ES16.8E3,T31,"|",T51,'|',T53,f8.2,T61,'| <-- SCF')
12  format(1x,'|',T6,I05,T11,"|",T15,ES16.8E3,T31,"|",T35,ES16.8E3,T51,'|',T53,f8.2,T61,'| <-- SCF')
    call trace_exit('electronic_scf')
  end subroutine electronic_scf

  subroutine electronic_scf_report()
    !==============================================================================!
    !                  E L E C T R O N I C _ S C F _ R E P O R T                   !
    !==============================================================================!
    ! This is an i/o routine for printing out the results of the SCF calculation   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!

    call trace_entry('electronic_scf_report')
    ! Check for convergence once we have finished the scf
    if (on_root_node)then
       if (current_state%converged)then
          ! We have converged, we are all happy  - this is good

          write(stdout,*)"|                 CONVERGED SUCCESSFULLY                   | <-- SCF"
          write(stdout,*)"+----------------------------------------------------------+ <-- SCF"
          write(stdout,111)'Total energy',units_from_atomic(current_state%total_energy, &
               & trim(current_params%out_energy_unit)),trim(current_params%out_energy_unit)
          write(stdout,111)'Fermi energy',units_from_atomic(current_state%efermi,&
               & trim(current_params%out_energy_unit)),trim(current_params%out_energy_unit)
          write(stdout,111)'Total moment',current_state%tot_moment,   'hbar/2'
          write(stdout,112)'Total spin  ',current_state%total_spin(1),&
               & current_state%total_spin(2),&
               & current_state%total_spin(3),&
               & 'hbar/2'
          write(stdout,112)'Total |spin|',current_state%total_modspin(1),&
               & current_state%total_modspin(2),&
               & current_state%total_modspin(3),&
               & 'hbar/2'


111       format(1x,'|',T4,a,':',T30,g21.12e3,T55,a,T61,'| <-- SCF')         
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
