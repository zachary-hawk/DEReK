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
  real(dp),public,parameter    :: small_number= 1e-15
  real(dp),public,parameter    :: sqrt_two_pi=2.506628274631000241612355239340104162693_dp

  !Complex numbers                                                                                                                                                                                                                                                          
  complex(dp),parameter,public :: cmplx_0 = (0.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_1 = (1.0_dp,0.0_dp)
  complex(dp),parameter,public :: cmplx_i = (0.0_dp,1.0_dp)


  ! Conversion factors
  ! Energies
  real(dp), parameter,public :: ev_to_hartree = 0.036749308136649_dp
  real(dp), parameter,public :: hartree_to_ev = 1.0_dp/ev_to_hartree

  !Lengths
  real(dp),parameter,public  :: angstrom_to_bohr = 1.8897259886_dp
  real(dp),parameter,public  :: bohr_to_angstrom = 1.0_dp/angstrom_to_bohr

  !Angles
  real(dp),parameter,public  :: rad_to_deg=180.0_dp/pi
  real(dp),parameter,public  :: deg_to_rad=pi/180.0_dp


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



  

end  module constants
