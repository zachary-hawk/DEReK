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
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module constants
  use iso_fortran_env , only: real64

  ! Basic parameters
  integer,parameter,public     :: dp=real64 

  ! Mathematical constants
  real(dp),public,parameter    :: pi=3.141592653589793238462643383279502884197_dp
  real(dp),public,parameter    :: twopi=2.0_dp*pi
  real(dp),public,parameter    :: fourpi=4.0_dp*pi
  real(dp),public,parameter    :: small_number= 1.0e-15_dp
  real(dp),public,parameter    :: sqrt_two_pi=2.506628274631000241612355239340104162693_dp

  !Complex numbers                                                                                                            
  complex(dp),parameter,public :: cmplx_0 = (0.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_1 = (1.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_i = (0.0_dp,1.0_dp)

  ! wont be needing these in while
  ! Conversion factors


  !Angles
  real(dp),parameter,public  :: rad_to_deg=180.0_dp/pi
  real(dp),parameter,public  :: deg_to_rad=pi/180.0_dp

  ! Usable FFT symbols
  integer, public :: fft_forward = -1
  integer, public :: fft_backward = 1
  
  
  !Symbols
  character(2), parameter :: deg_sym = char(int(Z'C2'))//char(int(Z'B0'))



#ifndef CODATA2018
#ifndef CODATA2014
#define CODATA2018
#endif
#endif



  !Fundamental Physical constants
#ifdef CODATA2018
  character(30),parameter,public :: const_version = "CODATA 2018"

  real(dp),parameter,public  :: speed_light_si = 299792458.0_dp                       ! Speed of light
  real(dp),parameter,public  :: planck_si = 6.62607015e-34_dp                         ! Planck constant (h)
  real(dp),parameter,public  :: elementary_charge_si = 1.602176634e-19_dp             ! Electron charge (e)
  real(dp),parameter,public  :: electron_mass_si = 9.1093837015e-31_dp                ! Electron mass (m_e)
  real(dp),parameter,public  :: proton_mass_si = 1.67262192369e-27_dp                 ! Proton mass (m_p)
  real(dp),parameter,public  :: electron_gyromagnetic_ratio_si = 1.76085963023e11_dp  ! Electron gyromagnetic ratio (gamma)
  real(dp),parameter,public  :: avogadro_si = 6.02214076e23_dp                        ! Avagadro's constant (N_A)
  real(dp),parameter,public  :: molar_gas_si = 8.314462618_dp                         ! Gas constant (R)
  real(dp),parameter,public  :: electron_spin_g = -2.00231930436256_dp                ! Electron spin G-factor (g)
#endif

#ifdef CODATA2014
  character(30),parameter,public :: const_version = "CODATA 2014"

  real(dp),parameter,public  :: speed_light_si = 299792458.0_dp                       ! Speed of light
  real(dp),parameter,public  :: planck_si = 6.626070040e-34_dp                        ! Planck constant (h)
  real(dp),parameter,public  :: elementary_charge_si = 1.6021766208e-19_dp            ! Electron charge (e)
  real(dp),parameter,public  :: electron_mass_si = 9.10938356e-31_dp                  ! Electron mass (m_e)
  real(dp),parameter,public  :: proton_mass_si = 1.672621898e-27_dp                   ! Proton mass (m_p)
  real(dp),parameter,public  :: electron_gyromagnetic_ratio_si = 1.760859644e11_dp    ! Electron gyromagnetic ratio (gamma)
  real(dp),parameter,public  :: avogadro_si = 6.022140857e23_dp                       ! Avagadro's constant (N_A)
  real(dp),parameter,public  :: molar_gas_si = 8.3144598_dp                           ! Gas constant (R)
  real(dp),parameter,public  :: electron_spin_g = -2.00231930436182_dp                ! Electron spin G-factor (g)
#endif


  ! Derived constants
  real(dp),parameter,public  :: hbar_si = planck_si/twopi                                                                   ! Reduced Planks constant (h_bar)
  real(dp),parameter,public  :: mu_b_si = elementary_charge_si * hbar_si /2.0_dp * electron_mass_si                         ! Bohr Magneton mu_B
  real(dp),parameter,public  :: mu_0_si = fourpi*1.0E-7_dp                                                                  ! Magnetic permiability (mu_0)
  real(dp),parameter,public  :: epsilon_0_si = 1.0_dp/(mu_0_si*speed_light_si**2)                                           ! Electric permitivity (epsilon_0)
  real(dp),parameter,public  :: fine_structure_si = elementary_charge_si**2/(4.0_dp*pi*epsilon_0_si*hbar_si*speed_light_si) ! Fine structure (alpha)
  real(dp),parameter,public  :: boltzmann_si = molar_gas_si/avogadro_si                                                     ! Boltzmann Constant (kB)

  !! -----------------------------------------------------------------------------------------------------------------------------------------------

  !!--------------------------------------!!
  !!        UNITS IN ATOMIC UNITS         !!
  !!--------------------------------------!!

  ! Number of units
  integer, parameter :: num_units = 75




  ! Lengths                                                                                                                
  real(dp), parameter :: bohr       = 1.0_dp
  real(dp), parameter :: metre      = electron_mass_si*speed_light_si*fine_structure_si/hbar_si
  real(dp), parameter :: centimetre = metre * 1e-2_dp
  real(dp), parameter :: millimetre = metre * 1e-3_dp
  real(dp), parameter :: mumetre    = metre * 1e-6_dp
  real(dp), parameter :: nanometre  = metre * 1e-9_dp
  real(dp), parameter :: angstrom   = metre * 1e-10_dp
  real(dp), parameter :: picometre  = metre * 1e-12_dp
  ! Masses                                                                                                                 
  real(dp), parameter :: electron_mass = 1.0_dp
  real(dp), parameter :: amu           = 1e-3_dp/avogadro_si/electron_mass_si
  real(dp), parameter :: kilogram      = 1.0_dp/electron_mass_si
  real(dp), parameter :: gram          = kilogram*1e-3_dp
  ! Times                                                                                                                  
  real(dp), parameter :: aut         = 1.0_dp
  real(dp), parameter :: second      = speed_light_si**2*fine_structure_si**2*electron_mass_si/hbar_si
  real(dp), parameter :: millisecond = 1e-3_dp*second
  real(dp), parameter :: microsecond = 1e-6_dp*second
  real(dp), parameter :: nanosecond  = 1e-9_dp*second
  real(dp), parameter :: picosecond  = 1e-12_dp*second
  real(dp), parameter :: femtosecond = 1e-15_dp*second
  ! Charges                                                                                                                
  real(dp), parameter :: elementary_charge = 1.0_dp
  real(dp), parameter :: coulomb           = 1.0_dp/elementary_charge_si

  ! Spins                                                                                                                  
  real(dp), parameter :: elementary_spin   = 1.0_dp           ! electrons                                               
  real(dp), parameter :: hbar              = 2.0_dp           ! electron spin = 0.5 hbar                                
  ! Magnetic Dipole moments
  real(dp), parameter :: magneton_si = elementary_charge_si*hbar_si/(2.0_dp*electron_mass_si) ! Defn is muB=e.hbar/(2*Me)
  real(dp), parameter :: magneton    = hbar / electron_spin_g                                 ! Defn is mag dip = g * muB *spin
  ! Energies
  real(dp), parameter :: hartree            = 1.0_dp
  real(dp), parameter :: millihartree       = 1e-3_dp
  real(dp), parameter :: electron_volt      = elementary_charge_si/(fine_structure_si**2*electron_mass_si*speed_light_si**2)
  real(dp), parameter :: millielectron_volt = electron_volt*1e-3_dp
  real(dp), parameter :: rydberg            = 0.5_dp
  real(dp), parameter :: millirydberg       = rydberg*1e-3_dp
  real(dp), parameter :: joule              = 1.0_dp/(fine_structure_si**2*electron_mass_si*speed_light_si**2)
  real(dp), parameter :: hertz              = planck_si*joule
  real(dp), parameter :: megahertz          = hertz*1e6_dp
  real(dp), parameter :: gigahertz          = hertz*1e9_dp
  real(dp), parameter :: terahertz          = hertz*1e12_dp
  real(dp), parameter :: wavenumber         = hertz*speed_light_si*1e2_dp
  ! Temperature
  real(dp), parameter :: kelvin             = boltzmann_si*joule
  ! Forces                                                                                                                 
  real(dp), parameter :: hartree_bohr = 1.0_dp
  real(dp), parameter :: eV_ang       = electron_volt/angstrom
  real(dp), parameter :: newton        = joule/metre
  ! Velocities                                                                                                             
  real(dp), parameter :: auv            = 1.0_dp
  real(dp), parameter :: angperps       = angstrom/picosecond
  real(dp), parameter :: angperfs       = angstrom/femtosecond
  real(dp), parameter :: bohrperps      = bohr/picosecond
  real(dp), parameter :: bohrperfs      = bohr/femtosecond
  real(dp), parameter :: metrepersecond = metre/second
  ! Pressures                                                                                                              
  real(dp), parameter :: hartree_bohr3 = 1.0_dp
  real(dp), parameter :: ev_ang3       = electron_volt/angstrom**3
  real(dp), parameter :: pascal         = newton/metre**3
  real(dp), parameter :: megapascal     = pascal*1e6_dp
  real(dp), parameter :: gigapascal     = pascal*1e9_dp
  real(dp), parameter :: terapascal     = pascal*1e12_dp
  real(dp), parameter :: petapascal     = pascal*1e15_dp
  real(dp), parameter :: atmosphere     = pascal*101325.027_dp ! Conversion to atmospheres                              
  real(dp), parameter :: bar            = pascal*1e5_dp
  real(dp), parameter :: megabar        = bar*1e6_dp
  ! Reciprocal length                                                                                                     
  real(dp), parameter :: invbohr      = 1.0_dp
  real(dp), parameter :: invmetre     = 1.0_dp/metre
  real(dp), parameter :: invnanometre = 1.0_dp/nanometre
  real(dp), parameter :: invangstrom  = 1.0_dp/angstrom
  real(dp), parameter :: invpicometre = 1.0_dp/picometre
  ! Volumes                                                                                                                
  real(dp), parameter :: bohr3       = 1.0_dp
  real(dp), parameter :: metre3      = metre**3
  real(dp), parameter :: centimetre3 = (metre * 1e-2_dp)**3
  real(dp), parameter :: millimetre3 = (millimetre)**3
  real(dp), parameter :: nanometre3  = (metre * 1e-9_dp)**3
  real(dp), parameter :: angstrom3   = (metre * 1e-10_dp)**3
  real(dp), parameter :: picometre3  = (metre * 1e-12_dp)**3
  ! Current + Magnetism
  real(dp), parameter :: ampere      = (fine_structure_si**2*electron_mass_si*speed_light_si**2*planck_si)/elementary_charge_si
  real(dp), parameter :: acd         = 1.0_dp
  real(dp), parameter :: tesla        = elementary_charge_si*invmetre**2*twopi/planck_si
  real(dp), parameter :: gauss        = 1.0e-4_dp*elementary_charge_si*invmetre**2*twopi/planck_si
  real(dp), parameter :: agr          = 1.0_dp
  !Efield
  real(dp), parameter :: hartree_bohr_e = 1.0_dp
  real(dp), parameter :: eV_ang_e       = electron_volt/angstrom
  real(dp), parameter :: newton_coulomb  = joule/(metre/elementary_charge_si)


  character(len=30),public :: conv_units(num_units)
  real(dp)         ,public :: conv_values(num_units)                           


contains
  subroutine constants_initialise()
    !==============================================================================!
    !                   C O N S T A N T S _ I N I T I A L I S E                    !
    !==============================================================================!
    ! Initialisation for the units data, a bit pointless other than all data       !
    ! having to be allocated in a subroutine                                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  31/03/2024                                            !
    !==============================================================================!
    ! these assignments have to be done in a subroutine

    ! Length
    conv_units(1)  = 'bohr'
    conv_values(1) = bohr

    conv_units(2)  = 'm'
    conv_values(2) = metre

    conv_units(3)  = 'cm'
    conv_values(3) = centimetre

    conv_units(67)  = 'mm'          ! late edition, hence numbering
    conv_values(67) = millimetre

    conv_units(68)  = 'mum'         ! late edition, hence numbering
    conv_values(68) = mumetre

    conv_units(4)  = 'nm'
    conv_values(4) = nanometre

    conv_units(5)  = 'A'
    conv_values(5) = angstrom

    conv_units(6)  = 'pm'
    conv_values(6) = picometre

    ! Masses
    conv_units(7)  = 'e_m'
    conv_values(7) = electron_mass

    conv_units(8)  = 'amu'
    conv_values(8) = amu

    conv_units(9)  = 'kg'
    conv_values(9) = kilogram

    conv_units(10)  = 'g'
    conv_values(10) = gram

    ! Times
    conv_units(11)  = 'aut'
    conv_values(11) = aut

    conv_units(12)  = 's'
    conv_values(12) = second

    conv_units(13)  = 'ms'
    conv_values(13) = millisecond

    conv_units(14)  = 'mus'
    conv_values(14) = microsecond

    conv_units(15)  = 'ns'
    conv_values(15) = nanosecond

    conv_units(16)  = 'fs'
    conv_values(16) = femtosecond

    !Charge
    conv_units(17)  = 'e'
    conv_values(17) = elemetary_charge

    conv_units(18)  = 'C'
    conv_values(18) = coulomb

    ! Spins
    conv_units(19)  = 'hbar/2'
    conv_values(19) = elementary_spin

    conv_units(20)  = 'hbar'
    conv_values(20) = hbar

    ! Magnetic dipole  ! only doing the non-SI version here
    conv_units(21)  = 'muB'
    conv_values(21) = magneton

    ! Energies
    conv_units(22)  = 'Ha'
    conv_values(22) = hartree

    conv_units(23)  = 'mHa'
    conv_values(23) = millihartree

    conv_units(24)  = 'eV'
    conv_values(24) = electron_volt

    conv_units(25)  = 'meV'
    conv_values(25) = millielectron_volt

    conv_units(26)  = 'Ry'
    conv_values(26) = rydberg

    conv_units(27)  = 'mRy'
    conv_values(27) = millirydberg

    conv_units(28)  = 'J'
    conv_values(28) = joule

    conv_units(29)  = 'Hz'
    conv_values(29) = hertz

    conv_units(30)  = 'MHz'
    conv_values(30) = megahertz

    conv_units(31)  = 'GHz'
    conv_values(31) = gigahertz

    conv_units(32)  = 'THz'
    conv_values(32) = terahertz

    conv_units(33)  = 'cm-1'
    conv_values(33) = wavenumber

    ! Temperature
    conv_units(34)  = 'K'
    conv_values(34) = kelvin

    ! Forces

    conv_units(35)  = 'Ha/Bohr'
    conv_values(35) = hartree_bohr

    conv_units(36)  = 'eV/A'
    conv_values(36) = ev_ang

    conv_units(37)  = 'N'
    conv_values(37) = newton

    ! Velocities
    conv_units(38)  = 'auv'
    conv_values(38) = auv

    conv_units(39)  = 'A/ps'
    conv_values(39) = angperps

    conv_units(40)  = 'A/fs'
    conv_values(40) = andperfs

    conv_units(41)  = 'Bohr/ps'
    conv_values(41) = bohrperps

    conv_units(42)  = 'Borh/fs'
    conv_values(42) = bohrperfs

    conv_units(43)  = 'm/s'
    conv_values(43) = metrepersecond

    ! Pressure
    conv_units(44)  = 'Ha/Bohr**3'
    conv_values(44) = hartree_bohr3

    conv_units(45)  = 'eV/A**3'
    conv_values(45) = ev_ang3

    conv_units(46)  = 'Pa'
    conv_values(46) = pascal

    conv_units(47)  = 'MPa'
    conv_values(47) = megapascal

    conv_units(48)  = 'GPa'
    conv_values(48) = gigapascal

    conv_units(49)  = 'TPa'
    conv_values(49) = terapascal

    conv_units(50)  = 'PPa'
    conv_values(50) = petapascal

    conv_units(51)  = 'atm'
    conv_values(51) = atmosphere

    conv_units(52)  = 'bar'
    conv_values(52) = bar

    conv_units(53)  = 'Mbar'
    conv_values(53) = megabar

    ! Recip lengths
    conv_units(54)  = 'Bohr-1'
    conv_values(54) = invbohr

    conv_units(55)  = 'm-1'
    conv_values(55) = invmetre

    conv_units(56)  = 'nm-1'
    conv_values(56) = invnanometre

    conv_units(57)  = 'A-1'
    conv_values(57) = invangstrom

    conv_units(58)  = 'pm-1'
    conv_values(58) = invpicometre

    ! Current + Mag
    conv_units(59)  = 'Ampere'
    conv_values(59) = ampere

    conv_units(60)  = 'acd'
    conv_values(60) = acd

    conv_units(61)  = 'T'
    conv_values(61) = tesla

    conv_units(62)  = 'G'
    conv_values(62) = gauss

    conv_units(63)  = 'agr'
    conv_values(63) = agr

    ! Efield
    conv_units(64)  = 'Ha/Bohr/e'
    conv_values(64) = hartree_bohr_e

    conv_units(65)  = 'eV/A/e'
    conv_values(65) = ev_ang_e

    conv_units(66)  = 'N/C'
    conv_values(66) = newton_coulomb


    ! Volumes
    conv_units(69)  = 'bohr**3'
    conv_values(69) = bohr3

    conv_units(70)  = 'm**3'
    conv_values(70) = metre3

    conv_units(71)  = 'cm**3'
    conv_values(71) = centimetre3

    conv_units(72)  = 'mm**3'
    conv_values(72) = millimetre3

    conv_units(73)  = 'nm**3'
    conv_values(73) = nanometre3

    conv_units(74)   = 'A**3'
    conv_values(74) = angstrom3

    conv_units(75)  = 'pm**3'
    conv_values(75) = picometre3


  end subroutine constants_initialise
end  module constants
