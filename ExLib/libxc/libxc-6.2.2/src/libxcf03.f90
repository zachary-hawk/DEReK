# 1 "./libxc_master.F90"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 385 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "./libxc_master.F90" 2
!! Copyright (C) 2016 Micael Oliveira
!! 2020-2022 Susi Lehtola
!! All rights reserved.
!!
!! This Source Code Form is subject to the terms of the Mozilla Public
!! License, v. 2.0. If a copy of the MPL was not distributed with this
!! file, You can obtain one at mozilla.org/MPL/2.0/.

module xc_f03_lib_m
  use, intrinsic :: iso_c_binding
  implicit none

  private
  public :: &
    ! version
    xc_f03_version, &
    xc_f03_version_string, &
    ! literature reference
    xc_f03_reference, &
    xc_f03_reference_doi, &
    xc_f03_reference_key, &
    ! func_info
    xc_f03_func_info_t, &
    xc_f03_func_info_get_number, &
    xc_f03_func_info_get_kind, &
    xc_f03_func_info_get_name, &
    xc_f03_func_info_get_family, &
    xc_f03_func_info_get_references, &
    xc_f03_func_info_get_flags, &
    xc_f03_func_info_get_n_ext_params, &
    xc_f03_func_info_get_ext_params_name, &
    xc_f03_func_info_get_ext_params_description, &
    xc_f03_func_info_get_ext_params_default_value, &
    ! func_reference
    xc_f03_func_reference_t, &
    xc_f03_func_reference_get_ref, &
    xc_f03_func_reference_get_doi, &
    xc_f03_func_reference_get_key, &
    xc_f03_func_reference_get_bibtex, &
    ! func
    xc_f03_func_t, &
    xc_f03_func_init, &
    xc_f03_func_end, &
    xc_f03_func_get_info, &
    xc_f03_functional_get_name, &
    xc_f03_functional_get_number, &
    xc_f03_family_from_id, &
    xc_f03_number_of_functionals, &
    xc_f03_maximum_name_length, &
    xc_f03_available_functional_numbers, &
    xc_f03_available_functional_names, &
    xc_f03_func_set_dens_threshold, &
    xc_f03_func_set_zeta_threshold, &
    xc_f03_func_set_sigma_threshold, &
    xc_f03_func_set_tau_threshold, &
    xc_f03_func_set_ext_params, &
    xc_f03_func_set_ext_params_name, &
    ! mixed functional interfaces
    xc_f03_num_aux_funcs, &
    xc_f03_aux_func_ids, &
    xc_f03_aux_func_weights, &
    ! lda
    xc_f03_lda, &
    xc_f03_lda_exc, &
    xc_f03_lda_exc_vxc, &
    xc_f03_lda_exc_vxc_fxc, &
    xc_f03_lda_exc_vxc_fxc_kxc, &
    xc_f03_lda_vxc, &
    xc_f03_lda_vxc_fxc, &
    xc_f03_lda_vxc_fxc_kxc, &
    xc_f03_lda_fxc, &
    xc_f03_lda_kxc, &
    xc_f03_lda_lxc, &
    ! gga
    xc_f03_gga, &
    xc_f03_gga_exc, &
    xc_f03_gga_exc_vxc, &
    xc_f03_gga_exc_vxc_fxc, &
    xc_f03_gga_exc_vxc_fxc_kxc, &
    xc_f03_gga_vxc, &
    xc_f03_gga_vxc_fxc, &
    xc_f03_gga_vxc_fxc_kxc, &
    xc_f03_gga_fxc, &
    xc_f03_gga_kxc, &
    xc_f03_gga_lxc, &
    xc_f03_gga_ak13_get_asymptotic, &
    xc_f03_hyb_exx_coef, &
    xc_f03_hyb_cam_coef, &
    xc_f03_nlc_coef, &
    ! mgga
    xc_f03_mgga, &
    xc_f03_mgga_exc, &
    xc_f03_mgga_exc_vxc, &
    xc_f03_mgga_exc_vxc_fxc, &
    xc_f03_mgga_exc_vxc_fxc_kxc, &
    xc_f03_mgga_vxc, &
    xc_f03_mgga_vxc_fxc, &
    xc_f03_mgga_vxc_fxc_kxc, &
    xc_f03_mgga_fxc, &
    xc_f03_mgga_kxc, &
    xc_f03_mgga_lxc

  integer(c_int), parameter, public :: &
    XC_UNPOLARIZED = 1, & ! Spin unpolarized
    XC_POLARIZED = 2 ! Spin polarized

    integer(c_int), parameter, public :: &
    XC_NON_RELATIVISTIC = 0, & ! Functional includes or not relativistic
    XC_RELATIVISTIC = 1 ! corrections. Only available in some functionals.

  ! Kinds
  integer(c_int), parameter, public :: &
    XC_EXCHANGE = 0, &
    XC_CORRELATION = 1, &
    XC_EXCHANGE_CORRELATION = 2, &
    XC_KINETIC = 3

  ! Families of xc functionals
  integer(c_int), parameter, public :: &
    XC_FAMILY_UNKNOWN = -1, &
    XC_FAMILY_NONE = 0, &
    XC_FAMILY_LDA = 1, &
    XC_FAMILY_GGA = 2, &
    XC_FAMILY_MGGA = 4, &
    XC_FAMILY_LCA = 8, &
    XC_FAMILY_OEP = 16, &
    XC_FAMILY_HYB_GGA = 32, &
    XC_FAMILY_HYB_MGGA = 64, &
    XC_FAMILY_HYB_LDA = 128

  integer(c_int), parameter, public :: &
    XC_FLAGS_HAVE_EXC = 1, &
    XC_FLAGS_HAVE_VXC = 2, &
    XC_FLAGS_HAVE_FXC = 4, &
    XC_FLAGS_HAVE_KXC = 8, &
    XC_FLAGS_HAVE_LXC = 16, &
    XC_FLAGS_HAVE_ALL = 31, & ! The most common case
    XC_FLAGS_1D = 32, &
    XC_FLAGS_2D = 64, &
    XC_FLAGS_3D = 128, &
    XC_FLAGS_HYB_CAM = 256, &
    XC_FLAGS_HYB_CAMY = 512, &
    XC_FLAGS_VV10 = 1024, &
    XC_FLAGS_HYB_LC = 2048, &
    XC_FLAGS_HYB_LCY = 4096, &
    XC_FLAGS_STABLE = 8192, &
    XC_FLAGS_DEVELOPMENT = 16384, &
    XC_FLAGS_NEEDS_LAPLACIAN = 32768

  integer(c_int), parameter, public :: &
    XC_TAU_EXPLICIT = 0, &
    XC_TAU_EXPANSION = 1

  integer(c_int), parameter, public :: &
    XC_MAX_REFERENCES = 5

  ! List of functionals
# 1 "../src/libxc_inc.f90" 1
 ! Slater exchange
 integer(c_int), parameter, public :: XC_LDA_X = 1

 ! Wigner
 integer(c_int), parameter, public :: XC_LDA_C_WIGNER = 2

 ! Random Phase Approximation (RPA)
 integer(c_int), parameter, public :: XC_LDA_C_RPA = 3

 ! Hedin & Lundqvist
 integer(c_int), parameter, public :: XC_LDA_C_HL = 4

 ! Gunnarson & Lundqvist
 integer(c_int), parameter, public :: XC_LDA_C_GL = 5

 ! Slaters Xalpha
 integer(c_int), parameter, public :: XC_LDA_C_XALPHA = 6

 ! Vosko, Wilk & Nusair (VWN5)
 integer(c_int), parameter, public :: XC_LDA_C_VWN = 7

 ! Vosko, Wilk & Nusair (VWN5_RPA)
 integer(c_int), parameter, public :: XC_LDA_C_VWN_RPA = 8

 ! Perdew & Zunger
 integer(c_int), parameter, public :: XC_LDA_C_PZ = 9

 ! Perdew & Zunger (Modified)
 integer(c_int), parameter, public :: XC_LDA_C_PZ_MOD = 10

 ! Ortiz & Ballone (PZ parametrization)
 integer(c_int), parameter, public :: XC_LDA_C_OB_PZ = 11

 ! Perdew & Wang
 integer(c_int), parameter, public :: XC_LDA_C_PW = 12

 ! Perdew & Wang (modified)
 integer(c_int), parameter, public :: XC_LDA_C_PW_MOD = 13

 ! Ortiz & Ballone (PW parametrization)
 integer(c_int), parameter, public :: XC_LDA_C_OB_PW = 14

 ! AMGB (for 2D systems)
 integer(c_int), parameter, public :: XC_LDA_C_2D_AMGB = 15

 ! PRM (for 2D systems)
 integer(c_int), parameter, public :: XC_LDA_C_2D_PRM = 16

 ! von Barth & Hedin
 integer(c_int), parameter, public :: XC_LDA_C_VBH = 17

 ! Casula, Sorella & Senatore
 integer(c_int), parameter, public :: XC_LDA_C_1D_CSC = 18

 ! Slater exchange
 integer(c_int), parameter, public :: XC_LDA_X_2D = 19

 ! Teter 93
 integer(c_int), parameter, public :: XC_LDA_XC_TETER93 = 20

 ! Exchange in 1D for an soft-Coulomb interaction
 integer(c_int), parameter, public :: XC_LDA_X_1D_SOFT = 21

 ! Modified LSD (version 1) of Proynov and Salahub
 integer(c_int), parameter, public :: XC_LDA_C_ML1 = 22

 ! Modified LSD (version 2) of Proynov and Salahub
 integer(c_int), parameter, public :: XC_LDA_C_ML2 = 23

 ! Gombas
 integer(c_int), parameter, public :: XC_LDA_C_GOMBAS = 24

 ! Perdew & Wang (fit to the RPA energy)
 integer(c_int), parameter, public :: XC_LDA_C_PW_RPA = 25

 ! P-F Loos correlation LDA
 integer(c_int), parameter, public :: XC_LDA_C_1D_LOOS = 26

 ! Ragot-Cortona
 integer(c_int), parameter, public :: XC_LDA_C_RC04 = 27

 ! Vosko, Wilk & Nusair (VWN1)
 integer(c_int), parameter, public :: XC_LDA_C_VWN_1 = 28

 ! Vosko, Wilk & Nusair (VWN2)
 integer(c_int), parameter, public :: XC_LDA_C_VWN_2 = 29

 ! Vosko, Wilk & Nusair (VWN3)
 integer(c_int), parameter, public :: XC_LDA_C_VWN_3 = 30

 ! Vosko, Wilk & Nusair (VWN4)
 integer(c_int), parameter, public :: XC_LDA_C_VWN_4 = 31

 ! Minnesota GAM exhange functional
 integer(c_int), parameter, public :: XC_GGA_X_GAM = 32

 ! Minnesota GAM correlation functional
 integer(c_int), parameter, public :: XC_GGA_C_GAM = 33

 ! HCTH-A
 integer(c_int), parameter, public :: XC_GGA_X_HCTH_A = 34

 ! Engel and Vosko
 integer(c_int), parameter, public :: XC_GGA_X_EV93 = 35

 ! Dispersionless Density Functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_DLDF = 36

 ! Dispersionless Density Functional
 integer(c_int), parameter, public :: XC_MGGA_C_DLDF = 37

 ! Burke, Cancio, Gould, and Pittalis
 integer(c_int), parameter, public :: XC_GGA_X_BCGP = 38

 ! acGGA, asymptotically corrected GGA correlation
 integer(c_int), parameter, public :: XC_GGA_C_ACGGA = 39

 ! lambda_OC2(N) version of PBE
 integer(c_int), parameter, public :: XC_GGA_X_LAMBDA_OC2_N = 40

 ! Revised Becke 86 with modified gradient correction
 integer(c_int), parameter, public :: XC_GGA_X_B86_R = 41

 ! Zhao, Levy & Parr, Eq. (21)
 integer(c_int), parameter, public :: XC_MGGA_XC_ZLP = 42

 ! Zhao, Levy & Parr, Eq. (20)
 integer(c_int), parameter, public :: XC_LDA_XC_ZLP = 43

 ! lambda_CH(N) version of PBE
 integer(c_int), parameter, public :: XC_GGA_X_LAMBDA_CH_N = 44

 ! lambda_LO(N) version of PBE
 integer(c_int), parameter, public :: XC_GGA_X_LAMBDA_LO_N = 45

 ! HJS screened exchange B88 corrected version
 integer(c_int), parameter, public :: XC_GGA_X_HJS_B88_V2 = 46

 ! Chiodo et al
 integer(c_int), parameter, public :: XC_GGA_C_Q2D = 47

 ! Chiodo et al
 integer(c_int), parameter, public :: XC_GGA_X_Q2D = 48

 ! Reparametrized PBE by del Campo, Gazquez, Trickey & Vela
 integer(c_int), parameter, public :: XC_GGA_X_PBE_MOL = 49

 ! Thomas-Fermi kinetic energy
 integer(c_int), parameter, public :: XC_LDA_K_TF = 50

 ! Lee and Parr Gaussian ansatz for the kinetic energy
 integer(c_int), parameter, public :: XC_LDA_K_LP = 51

 ! Thomas-Fermi plus von Weiszaecker correction
 integer(c_int), parameter, public :: XC_GGA_K_TFVW = 52

 ! interpolated version of revAPBE
 integer(c_int), parameter, public :: XC_GGA_K_REVAPBEINT = 53

 ! interpolated version of APBE
 integer(c_int), parameter, public :: XC_GGA_K_APBEINT = 54

 ! revised APBE
 integer(c_int), parameter, public :: XC_GGA_K_REVAPBE = 55

 ! Armiento & Kuemmel 2013
 integer(c_int), parameter, public :: XC_GGA_X_AK13 = 56

 ! Meyer, Wang, and Young
 integer(c_int), parameter, public :: XC_GGA_K_MEYER = 57

 ! Berland and Hyldgaard
 integer(c_int), parameter, public :: XC_GGA_X_LV_RPW86 = 58

 ! PBE revised by Tognetti et al
 integer(c_int), parameter, public :: XC_GGA_X_PBE_TCA = 59

 ! PBE for hybrid interfaces
 integer(c_int), parameter, public :: XC_GGA_X_PBEINT = 60

 ! spin-dependent gradient correction to PBEint
 integer(c_int), parameter, public :: XC_GGA_C_ZPBEINT = 61

 ! PBE for hybrid interfaces
 integer(c_int), parameter, public :: XC_GGA_C_PBEINT = 62

 ! spin-dependent gradient correction to PBEsol
 integer(c_int), parameter, public :: XC_GGA_C_ZPBESOL = 63

 ! oTPSS-D functional of Goerigk and Grimme
 integer(c_int), parameter, public :: XC_MGGA_XC_OTPSS_D = 64

 ! oPBE-D functional of Goerigk and Grimme
 integer(c_int), parameter, public :: XC_GGA_XC_OPBE_D = 65

 ! oPWLYP-D functional of Goerigk and Grimme
 integer(c_int), parameter, public :: XC_GGA_XC_OPWLYP_D = 66

 ! oBLYP-D functional of Goerigk and Grimme
 integer(c_int), parameter, public :: XC_GGA_XC_OBLYP_D = 67

 ! VMT{8,4} with constraint satisfaction with mu = mu_GE
 integer(c_int), parameter, public :: XC_GGA_X_VMT84_GE = 68

 ! VMT{8,4} with constraint satisfaction with mu = mu_PBE
 integer(c_int), parameter, public :: XC_GGA_X_VMT84_PBE = 69

 ! Vela, Medel, and Trickey with mu = mu_GE
 integer(c_int), parameter, public :: XC_GGA_X_VMT_GE = 70

 ! Vela, Medel, and Trickey with mu = mu_PBE
 integer(c_int), parameter, public :: XC_GGA_X_VMT_PBE = 71

 ! Colle and Salvetti
 integer(c_int), parameter, public :: XC_MGGA_C_CS = 72

 ! Minnesota MN12-SX correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_MN12_SX = 73

 ! Minnesota MN12-L correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_MN12_L = 74

 ! Minnesota M11-L correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M11_L = 75

 ! Minnesota M11 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M11 = 76

 ! Minnesota M08-SO correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M08_SO = 77

 ! Minnesota M08 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M08_HX = 78

 ! Minnesota N12-SX correlation functional
 integer(c_int), parameter, public :: XC_GGA_C_N12_SX = 79

 ! Minnesota N12 correlation functional
 integer(c_int), parameter, public :: XC_GGA_C_N12 = 80

 ! Minnesota N12-SX exchange functional
 integer(c_int), parameter, public :: XC_HYB_GGA_X_N12_SX = 81

 ! Minnesota N12 exchange functional
 integer(c_int), parameter, public :: XC_GGA_X_N12 = 82

 ! regularized TPSS correlation
 integer(c_int), parameter, public :: XC_GGA_C_REGTPSS = 83

 ! one-parameter progressive functional (Xalpha version)
 integer(c_int), parameter, public :: XC_GGA_C_OP_XALPHA = 84

 ! one-parameter progressive functional (G96 version)
 integer(c_int), parameter, public :: XC_GGA_C_OP_G96 = 85

 ! one-parameter progressive functional (PBE version)
 integer(c_int), parameter, public :: XC_GGA_C_OP_PBE = 86

 ! one-parameter progressive functional (B88 version)
 integer(c_int), parameter, public :: XC_GGA_C_OP_B88 = 87

 ! Filatov & Thiel correlation
 integer(c_int), parameter, public :: XC_GGA_C_FT97 = 88

 ! PBE correlation to be used with the SSB exchange
 integer(c_int), parameter, public :: XC_GGA_C_SPBE = 89

 ! Swart, Sola and Bickelhaupt correction to PBE
 integer(c_int), parameter, public :: XC_GGA_X_SSB_SW = 90

 ! Swart, Sola and Bickelhaupt
 integer(c_int), parameter, public :: XC_GGA_X_SSB = 91

 ! Swart, Sola and Bickelhaupt dispersion
 integer(c_int), parameter, public :: XC_GGA_X_SSB_D = 92

 ! HCTH/407+
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_407P = 93

 ! HCTH p=7/6
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_P76 = 94

 ! HCTH p=1/4
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_P14 = 95

 ! Becke 97 GGA-1
 integer(c_int), parameter, public :: XC_GGA_XC_B97_GGA1 = 96

 ! HCTH-A
 integer(c_int), parameter, public :: XC_GGA_C_HCTH_A = 97

 ! BPCCAC (GRAC for the energy)
 integer(c_int), parameter, public :: XC_GGA_X_BPCCAC = 98

 ! Tognetti, Cortona, Adamo (revised)
 integer(c_int), parameter, public :: XC_GGA_C_REVTCA = 99

 ! Tognetti, Cortona, Adamo
 integer(c_int), parameter, public :: XC_GGA_C_TCA = 100

 ! Perdew, Burke & Ernzerhof
 integer(c_int), parameter, public :: XC_GGA_X_PBE = 101

 ! Revised PBE from Zhang & Yang
 integer(c_int), parameter, public :: XC_GGA_X_PBE_R = 102

 ! Becke 86
 integer(c_int), parameter, public :: XC_GGA_X_B86 = 103

 ! Becke 86 with modified gradient correction
 integer(c_int), parameter, public :: XC_GGA_X_B86_MGC = 105

 ! Becke 88
 integer(c_int), parameter, public :: XC_GGA_X_B88 = 106

 ! Gill 96
 integer(c_int), parameter, public :: XC_GGA_X_G96 = 107

 ! Perdew & Wang 86
 integer(c_int), parameter, public :: XC_GGA_X_PW86 = 108

 ! Perdew & Wang 91
 integer(c_int), parameter, public :: XC_GGA_X_PW91 = 109

 ! Handy & Cohen OPTX 01
 integer(c_int), parameter, public :: XC_GGA_X_OPTX = 110

 ! dePristo & Kress 87 version R1
 integer(c_int), parameter, public :: XC_GGA_X_DK87_R1 = 111

 ! dePristo & Kress 87 version R2
 integer(c_int), parameter, public :: XC_GGA_X_DK87_R2 = 112

 ! Lacks & Gordon 93
 integer(c_int), parameter, public :: XC_GGA_X_LG93 = 113

 ! Filatov & Thiel 97 (version A)
 integer(c_int), parameter, public :: XC_GGA_X_FT97_A = 114

 ! Filatov & Thiel 97 (version B)
 integer(c_int), parameter, public :: XC_GGA_X_FT97_B = 115

 ! Perdew, Burke & Ernzerhof SOL
 integer(c_int), parameter, public :: XC_GGA_X_PBE_SOL = 116

 ! Hammer, Hansen, and Norskov
 integer(c_int), parameter, public :: XC_GGA_X_RPBE = 117

 ! Wu & Cohen
 integer(c_int), parameter, public :: XC_GGA_X_WC = 118

 ! mPW91 of Adamo & Barone
 integer(c_int), parameter, public :: XC_GGA_X_MPW91 = 119

 ! Armiento & Mattsson 05
 integer(c_int), parameter, public :: XC_GGA_X_AM05 = 120

 ! Madsen 07
 integer(c_int), parameter, public :: XC_GGA_X_PBEA = 121

 ! Adamo & Barone modification to PBE
 integer(c_int), parameter, public :: XC_GGA_X_MPBE = 122

 ! Extended PBE by Xu & Goddard III
 integer(c_int), parameter, public :: XC_GGA_X_XPBE = 123

 ! Becke 86 with modified gradient correction for 2D
 integer(c_int), parameter, public :: XC_GGA_X_2D_B86_MGC = 124

 ! Bayesian best fit for the enhancement factor
 integer(c_int), parameter, public :: XC_GGA_X_BAYESIAN = 125

 ! Reparametrized PBE by Pedroza, Silva & Capelle
 integer(c_int), parameter, public :: XC_GGA_X_PBE_JSJR = 126

 ! Becke 88 in 2D
 integer(c_int), parameter, public :: XC_GGA_X_2D_B88 = 127

 ! Becke 86 in 2D
 integer(c_int), parameter, public :: XC_GGA_X_2D_B86 = 128

 ! Perdew, Burke & Ernzerhof in 2D
 integer(c_int), parameter, public :: XC_GGA_X_2D_PBE = 129

 ! Perdew, Burke & Ernzerhof
 integer(c_int), parameter, public :: XC_GGA_C_PBE = 130

 ! Lee, Yang & Parr
 integer(c_int), parameter, public :: XC_GGA_C_LYP = 131

 ! Perdew 86
 integer(c_int), parameter, public :: XC_GGA_C_P86 = 132

 ! Perdew, Burke & Ernzerhof SOL
 integer(c_int), parameter, public :: XC_GGA_C_PBE_SOL = 133

 ! Perdew & Wang 91
 integer(c_int), parameter, public :: XC_GGA_C_PW91 = 134

 ! Armiento & Mattsson 05
 integer(c_int), parameter, public :: XC_GGA_C_AM05 = 135

 ! Extended PBE by Xu & Goddard III
 integer(c_int), parameter, public :: XC_GGA_C_XPBE = 136

 ! Langreth & Mehl
 integer(c_int), parameter, public :: XC_GGA_C_LM = 137

 ! Reparametrized PBE by Pedroza, Silva & Capelle
 integer(c_int), parameter, public :: XC_GGA_C_PBE_JRGX = 138

 ! opt-Becke 88 for vdW
 integer(c_int), parameter, public :: XC_GGA_X_OPTB88_VDW = 139

 ! Reparametrized PBE for vdW
 integer(c_int), parameter, public :: XC_GGA_X_PBEK1_VDW = 140

 ! Reparametrized PBE for vdW
 integer(c_int), parameter, public :: XC_GGA_X_OPTPBE_VDW = 141

 ! Regularized PBE
 integer(c_int), parameter, public :: XC_GGA_X_RGE2 = 142

 ! Regularized PBE
 integer(c_int), parameter, public :: XC_GGA_C_RGE2 = 143

 ! Refitted Perdew & Wang 86
 integer(c_int), parameter, public :: XC_GGA_X_RPW86 = 144

 ! Exchange part of Keal and Tozer version 1
 integer(c_int), parameter, public :: XC_GGA_X_KT1 = 145

 ! Keal and Tozer, version 2
 integer(c_int), parameter, public :: XC_GGA_XC_KT2 = 146

 ! Wilson & Levy
 integer(c_int), parameter, public :: XC_GGA_C_WL = 147

 ! Wilson & Ivanov
 integer(c_int), parameter, public :: XC_GGA_C_WI = 148

 ! Modified Becke 88 for proton transfer
 integer(c_int), parameter, public :: XC_GGA_X_MB88 = 149

 ! Second-order generalized gradient approximation
 integer(c_int), parameter, public :: XC_GGA_X_SOGGA = 150

 ! Second-order generalized gradient approximation 2011
 integer(c_int), parameter, public :: XC_GGA_X_SOGGA11 = 151

 ! Second-order generalized gradient approximation 2011
 integer(c_int), parameter, public :: XC_GGA_C_SOGGA11 = 152

 ! Wilson & Ivanov initial version
 integer(c_int), parameter, public :: XC_GGA_C_WI0 = 153

 ! Tozer and Handy v. 1
 integer(c_int), parameter, public :: XC_GGA_XC_TH1 = 154

 ! Tozer and Handy v. 2
 integer(c_int), parameter, public :: XC_GGA_XC_TH2 = 155

 ! Tozer and Handy v. 3
 integer(c_int), parameter, public :: XC_GGA_XC_TH3 = 156

 ! Tozer and Handy v. 4
 integer(c_int), parameter, public :: XC_GGA_XC_TH4 = 157

 ! C09x to be used with the VdW of Rutgers-Chalmers
 integer(c_int), parameter, public :: XC_GGA_X_C09X = 158

 ! To be used with HYB_GGA_X_SOGGA11_X
 integer(c_int), parameter, public :: XC_GGA_C_SOGGA11_X = 159

 ! van Leeuwen & Baerends
 integer(c_int), parameter, public :: XC_GGA_X_LB = 160

 ! HCTH/93
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_93 = 161

 ! HCTH/120
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_120 = 162

 ! HCTH/147
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_147 = 163

 ! HCTH/407
 integer(c_int), parameter, public :: XC_GGA_XC_HCTH_407 = 164

 ! EDF1
 integer(c_int), parameter, public :: XC_GGA_XC_EDF1 = 165

 ! XLYP
 integer(c_int), parameter, public :: XC_GGA_XC_XLYP = 166

 ! Keal and Tozer, version 1
 integer(c_int), parameter, public :: XC_GGA_XC_KT1 = 167

 ! lsPBE, a PW91-like modification of PBE exchange
 integer(c_int), parameter, public :: XC_GGA_X_LSPBE = 168

 ! lsRPBE, a PW91-like modification of RPBE
 integer(c_int), parameter, public :: XC_GGA_X_LSRPBE = 169

 ! Becke 97-D
 integer(c_int), parameter, public :: XC_GGA_XC_B97_D = 170

 ! Becke 86 reoptimized for use with vdW functional of Dion et al
 integer(c_int), parameter, public :: XC_GGA_X_OPTB86B_VDW = 171

 ! Revised Minnesota M11 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_REVM11 = 172

 ! PBE1W
 integer(c_int), parameter, public :: XC_GGA_XC_PBE1W = 173

 ! mPWLYP1w
 integer(c_int), parameter, public :: XC_GGA_XC_MPWLYP1W = 174

 ! PBELYP1W
 integer(c_int), parameter, public :: XC_GGA_XC_PBELYP1W = 175

 ! acGGA+, asymptotically corrected GGA correlation+
 integer(c_int), parameter, public :: XC_GGA_C_ACGGAP = 176

 ! LDA hybrid exchange (LDA0)
 integer(c_int), parameter, public :: XC_HYB_LDA_XC_LDA0 = 177

 ! CAM version of LDA0
 integer(c_int), parameter, public :: XC_HYB_LDA_XC_CAM_LDA0 = 178

 ! Becke 88 reoptimized with the 6-311G** basis set
 integer(c_int), parameter, public :: XC_GGA_X_B88_6311G = 179

 ! Nearly correct asymptotic potential
 integer(c_int), parameter, public :: XC_GGA_X_NCAP = 180

 ! NCAP exchange + P86 correlation
 integer(c_int), parameter, public :: XC_GGA_XC_NCAP = 181

 ! van Leeuwen & Baerends modified
 integer(c_int), parameter, public :: XC_GGA_X_LBM = 182

 ! Exchange form based on Ou-Yang and Levy v.2
 integer(c_int), parameter, public :: XC_GGA_X_OL2 = 183

 ! mu fixed from the semiclassical neutral atom
 integer(c_int), parameter, public :: XC_GGA_X_APBE = 184

 ! mu fixed from the semiclassical neutral atom
 integer(c_int), parameter, public :: XC_GGA_K_APBE = 185

 ! mu fixed from the semiclassical neutral atom
 integer(c_int), parameter, public :: XC_GGA_C_APBE = 186

 ! Tran and Wesolowski set 1 (Table II)
 integer(c_int), parameter, public :: XC_GGA_K_TW1 = 187

 ! Tran and Wesolowski set 2 (Table II)
 integer(c_int), parameter, public :: XC_GGA_K_TW2 = 188

 ! Tran and Wesolowski set 3 (Table II)
 integer(c_int), parameter, public :: XC_GGA_K_TW3 = 189

 ! Tran and Wesolowski set 4 (Table II)
 integer(c_int), parameter, public :: XC_GGA_K_TW4 = 190

 ! Haas, Tran, Blaha, and Schwarz
 integer(c_int), parameter, public :: XC_GGA_X_HTBS = 191

 ! Constantin et al based on the Airy gas
 integer(c_int), parameter, public :: XC_GGA_X_AIRY = 192

 ! Local Airy Gas
 integer(c_int), parameter, public :: XC_GGA_X_LAG = 193

 ! Functional for organometallic chemistry
 integer(c_int), parameter, public :: XC_GGA_XC_MOHLYP = 194

 ! Functional for barrier heights
 integer(c_int), parameter, public :: XC_GGA_XC_MOHLYP2 = 195

 ! Tozer and Handy v. FL
 integer(c_int), parameter, public :: XC_GGA_XC_TH_FL = 196

 ! Tozer and Handy v. FC
 integer(c_int), parameter, public :: XC_GGA_XC_TH_FC = 197

 ! Tozer and Handy v. FCFO
 integer(c_int), parameter, public :: XC_GGA_XC_TH_FCFO = 198

 ! Tozer and Handy v. FCO
 integer(c_int), parameter, public :: XC_GGA_XC_TH_FCO = 199

 ! Optimized correlation functional of Cohen and Handy
 integer(c_int), parameter, public :: XC_GGA_C_OPTC = 200

 ! Local tau approximation
 integer(c_int), parameter, public :: XC_MGGA_X_LTA = 201

 ! Tao, Perdew, Staroverov & Scuseria
 integer(c_int), parameter, public :: XC_MGGA_X_TPSS = 202

 ! Minnesota M06-L exchange functional
 integer(c_int), parameter, public :: XC_MGGA_X_M06_L = 203

 ! GVT4 (X part of VSXC)
 integer(c_int), parameter, public :: XC_MGGA_X_GVT4 = 204

 ! tau-HCTH from Boese and Handy
 integer(c_int), parameter, public :: XC_MGGA_X_TAU_HCTH = 205

 ! Becke-Roussel 89, gamma = 0.8
 integer(c_int), parameter, public :: XC_MGGA_X_BR89 = 206

 ! Becke & Johnson 06
 integer(c_int), parameter, public :: XC_MGGA_X_BJ06 = 207

 ! Tran & Blaha 09
 integer(c_int), parameter, public :: XC_MGGA_X_TB09 = 208

 ! Rasanen, Pittalis & Proetto 09
 integer(c_int), parameter, public :: XC_MGGA_X_RPP09 = 209

 ! Pittalis-Rasanen-Helbig-Gross 2007
 integer(c_int), parameter, public :: XC_MGGA_X_2D_PRHG07 = 210

 ! PRHG07 with Pittalis-Rasanen-Proetto 2010 correction
 integer(c_int), parameter, public :: XC_MGGA_X_2D_PRHG07_PRP10 = 211

 ! revised Tao, Perdew, Staroverov & Scuseria
 integer(c_int), parameter, public :: XC_MGGA_X_REVTPSS = 212

 ! Perdew, Kurth, Zupan, and Blaha
 integer(c_int), parameter, public :: XC_MGGA_X_PKZB = 213

 ! Becke-Roussel 89, gamma = 1.0
 integer(c_int), parameter, public :: XC_MGGA_X_BR89_1 = 214

 ! Engel, Chevary, Macdonald and Vosko
 integer(c_int), parameter, public :: XC_GGA_X_ECMV92 = 215

 ! Perdew, Burke & Ernzerhof based on VWN correlation
 integer(c_int), parameter, public :: XC_GGA_C_PBE_VWN = 216

 ! Perdew 86 with more accurate value for ftilde
 integer(c_int), parameter, public :: XC_GGA_C_P86_FT = 217

 ! RATIONAL$^{p}$ by Lehtomaki and Lopez-Acevedo (by default $p=3/2$, $C_{2}=0.7687$)
 integer(c_int), parameter, public :: XC_GGA_K_RATIONAL_P = 218

 ! PG1 (Pauli-Gaussian) functional by Constantin, Fabiano, and Della Sala
 integer(c_int), parameter, public :: XC_GGA_K_PG1 = 219

 ! PGSL025 (Pauli-Gaussian) functional by Constantin, Fabiano, and Della Sala
 integer(c_int), parameter, public :: XC_MGGA_K_PGSL025 = 220

 ! MS exchange of Sun, Xiao, and Ruzsinszky
 integer(c_int), parameter, public :: XC_MGGA_X_MS0 = 221

 ! MS1 exchange of Sun, et al
 integer(c_int), parameter, public :: XC_MGGA_X_MS1 = 222

 ! MS2 exchange of Sun, et al
 integer(c_int), parameter, public :: XC_MGGA_X_MS2 = 223

 ! MS2 hybrid exchange of Sun, et al
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_MS2H = 224

 ! Tsuneda and Hirao
 integer(c_int), parameter, public :: XC_MGGA_X_TH = 225

 ! Minnesota M11-L exchange functional
 integer(c_int), parameter, public :: XC_MGGA_X_M11_L = 226

 ! Minnesota MN12-L exchange functional
 integer(c_int), parameter, public :: XC_MGGA_X_MN12_L = 227

 ! MS2 exchange of Sun, et al with revised value for c
 integer(c_int), parameter, public :: XC_MGGA_X_MS2_REV = 228

 ! Cancio and Chou 2006
 integer(c_int), parameter, public :: XC_MGGA_XC_CC06 = 229

 ! Exchange for accurate virtual orbital energies
 integer(c_int), parameter, public :: XC_MGGA_X_MK00 = 230

 ! Tao, Perdew, Staroverov & Scuseria
 integer(c_int), parameter, public :: XC_MGGA_C_TPSS = 231

 ! VSXC (correlation part)
 integer(c_int), parameter, public :: XC_MGGA_C_VSXC = 232

 ! Minnesota M06-L correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M06_L = 233

 ! Minnesota M06-HF correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M06_HF = 234

 ! Minnesota M06 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M06 = 235

 ! Minnesota M06-2X correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M06_2X = 236

 ! Minnesota M05 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M05 = 237

 ! Minnesota M05-2X correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M05_2X = 238

 ! Perdew, Kurth, Zupan, and Blaha
 integer(c_int), parameter, public :: XC_MGGA_C_PKZB = 239

 ! Becke correlation 95
 integer(c_int), parameter, public :: XC_MGGA_C_BC95 = 240

 ! revised TPSS correlation
 integer(c_int), parameter, public :: XC_MGGA_C_REVTPSS = 241

 ! TPSSLYP1W
 integer(c_int), parameter, public :: XC_MGGA_XC_TPSSLYP1W = 242

 ! Exchange for accurate virtual orbital energies (v. B)
 integer(c_int), parameter, public :: XC_MGGA_X_MK00B = 243

 ! functional with balanced localization
 integer(c_int), parameter, public :: XC_MGGA_X_BLOC = 244

 ! Modified Tao, Perdew, Staroverov & Scuseria
 integer(c_int), parameter, public :: XC_MGGA_X_MODTPSS = 245

 ! Semilocal dynamical correlation
 integer(c_int), parameter, public :: XC_GGA_C_PBELOC = 246

 ! Semilocal dynamical correlation
 integer(c_int), parameter, public :: XC_MGGA_C_TPSSLOC = 247

 ! Minnesota MN12-SX hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_MN12_SX = 248

 ! mBEEF exchange
 integer(c_int), parameter, public :: XC_MGGA_X_MBEEF = 249

 ! mBEEF-vdW exchange
 integer(c_int), parameter, public :: XC_MGGA_X_MBEEFVDW = 250

 ! Tao and Mo 2016 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_TM = 251

 ! Perdew 86 based on VWN5 correlation
 integer(c_int), parameter, public :: XC_GGA_C_P86VWN = 252

 ! Perdew 86 based on VWN5 correlation, with more accurate value for ftilde
 integer(c_int), parameter, public :: XC_GGA_C_P86VWN_FT = 253

 ! B97M-V exchange-correlation functional
 integer(c_int), parameter, public :: XC_MGGA_XC_B97M_V = 254

 ! Vydrov and Van Voorhis
 integer(c_int), parameter, public :: XC_GGA_XC_VV10 = 255

 ! Jemmer-Knowles meta-GGA exchange
 integer(c_int), parameter, public :: XC_MGGA_X_JK = 256

 ! MVS exchange of Sun, Perdew, and Ruzsinszky
 integer(c_int), parameter, public :: XC_MGGA_X_MVS = 257

 ! PBE for formation energies
 integer(c_int), parameter, public :: XC_GGA_C_PBEFE = 258

 ! Karasiev, Sjostrom, Dufty & Trickey
 integer(c_int), parameter, public :: XC_LDA_XC_KSDT = 259

 ! Minnesota MN15-L exchange functional
 integer(c_int), parameter, public :: XC_MGGA_X_MN15_L = 260

 ! Minnesota MN15-L correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_MN15_L = 261

 ! one-parameter progressive functional (PW91 version)
 integer(c_int), parameter, public :: XC_GGA_C_OP_PW91 = 262

 ! SCAN exchange of Sun, Ruzsinszky, and Perdew
 integer(c_int), parameter, public :: XC_MGGA_X_SCAN = 263

 ! SCAN hybrid exchange (SCAN0)
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_SCAN0 = 264

 ! PBE for formation energies
 integer(c_int), parameter, public :: XC_GGA_X_PBEFE = 265

 ! version of B97 by Cohen and Handy
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97_1P = 266

 ! SCAN correlation of Sun, Ruzsinszky, and Perdew
 integer(c_int), parameter, public :: XC_MGGA_C_SCAN = 267

 ! Minnesota MN15 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_MN15 = 268

 ! Minnesota MN15 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_MN15 = 269

 ! Correct Asymptotic Potential
 integer(c_int), parameter, public :: XC_GGA_X_CAP = 270

 ! Non-empirical (excogitated) B88 functional of Becke and Elliott
 integer(c_int), parameter, public :: XC_GGA_X_EB88 = 271

 ! Reparametrized PBE by del Campo, Gazquez, Trickey & Vela
 integer(c_int), parameter, public :: XC_GGA_C_PBE_MOL = 272

 ! PBEmol0
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE_MOL0 = 273

 ! PBEsol0
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE_SOL0 = 274

 ! PBEbeta0
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBEB0 = 275

 ! PBEmolbeta0
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE_MOLB0 = 276

 ! gamma-TFvW form by Acharya et al [$g = 1 - 1.513/N^{0.35}]$
 integer(c_int), parameter, public :: XC_GGA_K_ABSP3 = 277

 ! gamma-TFvW form by Acharya et al [$g = l = 1/(1 + 1.332/N^{1/3})$]
 integer(c_int), parameter, public :: XC_GGA_K_ABSP4 = 278

 ! Boese-Martin for kinetics
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_BMK = 279

 ! Boese-Martin correlation for kinetics
 integer(c_int), parameter, public :: XC_GGA_C_BMK = 280

 ! correlation part of tau-hcth
 integer(c_int), parameter, public :: XC_GGA_C_TAU_HCTH = 281

 ! Hybrid version of tau-HCTH
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_TAU_HCTH = 282

 ! correlation part of hyb-tau-hcth
 integer(c_int), parameter, public :: XC_GGA_C_HYB_TAU_HCTH = 283

 ! Becke 2000
 integer(c_int), parameter, public :: XC_MGGA_X_B00 = 284

 ! BEEF-vdW exchange
 integer(c_int), parameter, public :: XC_GGA_X_BEEFVDW = 285

 ! BEEF-vdW exchange-correlation
 integer(c_int), parameter, public :: XC_GGA_XC_BEEFVDW = 286

 ! Chachiyo simple 2 parameter correlation
 integer(c_int), parameter, public :: XC_LDA_C_CHACHIYO = 287

 ! high local exchange 2017
 integer(c_int), parameter, public :: XC_MGGA_XC_HLE17 = 288

 ! Liu-Parr correlation
 integer(c_int), parameter, public :: XC_LDA_C_LP96 = 289

 ! PBE50
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE50 = 290

 ! Gradient-regulated connection-based correction for the PBE exchange
 integer(c_int), parameter, public :: XC_GGA_X_PBETRANS = 291

 ! SCAN + rVV10 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_SCAN_RVV10 = 292

 ! Minnesota revM06-L exchange functional
 integer(c_int), parameter, public :: XC_MGGA_X_REVM06_L = 293

 ! Minnesota revM06-L correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_REVM06_L = 294

 ! Minnesota M08-HX hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M08_HX = 295

 ! Minnesota M08-SO hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M08_SO = 296

 ! Minnesota M11 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M11 = 297

 ! Chachiyo exchange
 integer(c_int), parameter, public :: XC_GGA_X_CHACHIYO = 298

 ! TPSS for surface adsorption
 integer(c_int), parameter, public :: XC_MGGA_X_RTPSS = 299

 ! MS2beta exchange of Furness and Sun
 integer(c_int), parameter, public :: XC_MGGA_X_MS2B = 300

 ! MS2beta* exchange of Furness and Sun
 integer(c_int), parameter, public :: XC_MGGA_X_MS2BS = 301

 ! MVSbeta exchange by Furness and Sun
 integer(c_int), parameter, public :: XC_MGGA_X_MVSB = 302

 ! MVSbeta* exchange by Furness and Sun
 integer(c_int), parameter, public :: XC_MGGA_X_MVSBS = 303

 ! Revised Minnesota M11 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_REVM11 = 304

 ! Revised Minnesota M06 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_REVM06 = 305

 ! Revised Minnesota M06 correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_REVM06 = 306

 ! Chachiyo simple 2 parameter correlation with modified spin scaling
 integer(c_int), parameter, public :: XC_LDA_C_CHACHIYO_MOD = 307

 ! Karasiev reparameterization of Chachiyo
 integer(c_int), parameter, public :: XC_LDA_C_KARASIEV_MOD = 308

 ! Chachiyo simple GGA correlation
 integer(c_int), parameter, public :: XC_GGA_C_CHACHIYO = 309

 ! Minnesota M06-SX short-range hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M06_SX = 310

 ! Minnesota M06-SX correlation functional
 integer(c_int), parameter, public :: XC_MGGA_C_M06_SX = 311

 ! Revised Swart, Sola and Bickelhaupt dispersion
 integer(c_int), parameter, public :: XC_GGA_X_REVSSB_D = 312

 ! ccDF: coupled-cluster motivated density functional
 integer(c_int), parameter, public :: XC_GGA_C_CCDF = 313

 ! HF + LYP correlation
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HFLYP = 314

 ! B3P86, NWChem version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3P86_NWCHEM = 315

 ! PW91, alternate version with more digits
 integer(c_int), parameter, public :: XC_GGA_X_PW91_MOD = 316

 ! Xie, Wu, and Zhao interpolation ansatz without fitting parameters
 integer(c_int), parameter, public :: XC_LDA_C_W20 = 317

 ! Corrected KSDT by Karasiev, Dufty and Trickey
 integer(c_int), parameter, public :: XC_LDA_XC_CORRKSDT = 318

 ! Filatov and Thiel 1998 meta-GGA exchange
 integer(c_int), parameter, public :: XC_MGGA_X_FT98 = 319

 ! Perdew, Burke & Ernzerhof with less precise value for beta
 integer(c_int), parameter, public :: XC_GGA_X_PBE_MOD = 320

 ! Perdew, Burke & Ernzerhof with parameter values used in Gaussian
 integer(c_int), parameter, public :: XC_GGA_X_PBE_GAUSSIAN = 321

 ! Perdew, Burke & Ernzerhof with parameters from Gaussian
 integer(c_int), parameter, public :: XC_GGA_C_PBE_GAUSSIAN = 322

 ! Tao, Perdew, Staroverov & Scuseria with parameters from Gaussian
 integer(c_int), parameter, public :: XC_MGGA_C_TPSS_GAUSSIAN = 323

 ! Nearly correct asymptotic potential revised
 integer(c_int), parameter, public :: XC_GGA_X_NCAPR = 324

 ! relPBE0 a.k.a. relPBE: PBE0 refitted for actinide compounds
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_RELPBE0 = 325

 ! Becke 97-3c by Grimme et. al.
 integer(c_int), parameter, public :: XC_GGA_XC_B97_3C = 327

 ! Self-interaction corrected correlation functional by Schmidt et al
 integer(c_int), parameter, public :: XC_MGGA_C_CC = 387

 ! Iso-orbital corrected LDA correlation by Lebeda et al
 integer(c_int), parameter, public :: XC_MGGA_C_CCALDA = 388

 ! BR3P86 hybrid meta-GGA from Neumann and Handy
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_BR3P86 = 389

 ! CASE21: Constrained And Smoothed semi-Empirical 2021 functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CASE21 = 390

 ! Revised regTM correlation by Jana et al
 integer(c_int), parameter, public :: XC_MGGA_C_RREGTM = 391

 ! PBE-2X: PBE0 with 56% exact exchange
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE_2X = 392

 ! PBE38: PBE0 with 3/8 = 37.5% exact exchange
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE38 = 393

 ! B3LYP with VWN functional 3 instead of RPA
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYP3 = 394

 ! CAM-O3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_O3LYP = 395

 ! TPSS0 with 25% exact exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_TPSS0 = 396

 ! Becke 1994 meta-GGA correlation
 integer(c_int), parameter, public :: XC_MGGA_C_B94 = 397

 ! Becke 1994 hybrid meta-GGA
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_B94_HYB = 398

 ! wB97X-D3 range-separated functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WB97X_D3 = 399

 ! LC version of BLYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_BLYP = 400

 ! The original (ACM, B3PW91) hybrid of Becke
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3PW91 = 401

 ! B3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYP = 402

 ! B3P86
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3P86 = 403

 ! O3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_O3LYP = 404

 ! mPW1K
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW1K = 405

 ! PBEH (PBE0)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBEH = 406

 ! Becke 97
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97 = 407

 ! Becke 97-1
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97_1 = 408

 ! APF hybrid functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_APF = 409

 ! Becke 97-2
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97_2 = 410

 ! X3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_X3LYP = 411

 ! B1WC
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B1WC = 412

 ! Boese-Martin for Kinetics
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97_K = 413

 ! Becke 97-3
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B97_3 = 414

 ! MPW3PW of Adamo & Barone
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW3PW = 415

 ! B1LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B1LYP = 416

 ! B1PW91
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B1PW91 = 417

 ! mPW1PW
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW1PW = 418

 ! MPW3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW3LYP = 419

 ! SB98 (1a)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_1A = 420

 ! SB98 (1b)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_1B = 421

 ! SB98 (1c)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_1C = 422

 ! SB98 (2a)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_2A = 423

 ! SB98 (2b)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_2B = 424

 ! SB98 (2c)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_SB98_2C = 425

 ! Hybrid based on SOGGA11 form
 integer(c_int), parameter, public :: XC_HYB_GGA_X_SOGGA11_X = 426

 ! HSE03
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HSE03 = 427

 ! HSE06
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HSE06 = 428

 ! HJS hybrid screened exchange PBE version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HJS_PBE = 429

 ! HJS hybrid screened exchange PBE_SOL version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HJS_PBE_SOL = 430

 ! HJS hybrid screened exchange B88 version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HJS_B88 = 431

 ! HJS hybrid screened exchange B97x version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HJS_B97X = 432

 ! CAM version of B3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_B3LYP = 433

 ! CAM version of B3LYP, tuned for excitations and properties
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_TUNED_CAM_B3LYP = 434

 ! BHandH i.e. BHLYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_BHANDH = 435

 ! BHandHLYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_BHANDHLYP = 436

 ! B3LYP with RC04 LDA
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MB3LYP_RC04 = 437

 ! Minnesota M05 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M05 = 438

 ! Minnesota M05-2X hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M05_2X = 439

 ! Mixture of B88 with BC95 (B1B95)
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_B88B95 = 440

 ! Mixture of B86 with BC95
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_B86B95 = 441

 ! Mixture of PW86 with BC95
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_PW86B95 = 442

 ! Mixture of B88 with BC95 from Zhao and Truhlar
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_BB1K = 443

 ! Minnesota M06-HF hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M06_HF = 444

 ! Mixture of mPW91 with BC95 from Zhao and Truhlar
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_MPW1B95 = 445

 ! Mixture of mPW91 with BC95 for kinetics
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_MPWB1K = 446

 ! Mixture of X with BC95
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_X1B95 = 447

 ! Mixture of X with BC95 for kinetics
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_XB1K = 448

 ! Minnesota M06 hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M06 = 449

 ! Minnesota M06-2X hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_M06_2X = 450

 ! Mixture of PW91 with BC95 from Zhao and Truhlar
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_PW6B95 = 451

 ! Mixture of PW91 with BC95 from Zhao and Truhlar for kinetics
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_PWB6K = 452

 ! MPW with 1 par. for metals/LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPWLYP1M = 453

 ! Revised B3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_REVB3LYP = 454

 ! CAMY version of BLYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAMY_BLYP = 455

 ! PBE0-1/3
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_PBE0_13 = 456

 ! TPSSh
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_TPSSH = 457

 ! revTPSSh
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_REVTPSSH = 458

 ! B3LYP*
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYPS = 459

 ! Global hybrid for vertical ionization potentials
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_QTP17 = 460

 ! B3LYP-MCM1
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYP_MCM1 = 461

 ! B3LYP-MCM2
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYP_MCM2 = 462

 ! wB97 range-separated functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WB97 = 463

 ! wB97X range-separated functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WB97X = 464

 ! Long-range corrected short-range hybrid PBE (LRC-wPBEh) by Rohrdanz, Martins and Herbert
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LRC_WPBEH = 465

 ! wB97X-V range-separated functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WB97X_V = 466

 ! LCY version of PBE
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LCY_PBE = 467

 ! LCY version of BLYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LCY_BLYP = 468

 ! Vydrov and Van Voorhis
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_VV10 = 469

 ! CAMY version of B3LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAMY_B3LYP = 470

 ! wB97X-D range-separated functional
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WB97X_D = 471

 ! hPBEint
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HPBEINT = 472

 ! Long-range corrected PBE (LRC-wPBE) by Rohrdanz, Martins and Herbert
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LRC_WPBE = 473

 ! MVSh hybrid exchange functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_MVSH = 474

 ! B3LYP with VWN functional 5 instead of RPA
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B3LYP5 = 475

 ! EDF2
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_EDF2 = 476

 ! Correct Asymptotic Potential hybrid
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAP0 = 477

 ! Long-range corrected PBE (LC-wPBE) by Vydrov and Scuseria
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_WPBE = 478

 ! HSE12
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HSE12 = 479

 ! HSE12 (short-range version)
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HSE12S = 480

 ! HSEsol
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HSE_SOL = 481

 ! CAM-B3LYP retuned using ionization potentials of water
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_QTP_01 = 482

 ! mPW1LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW1LYP = 483

 ! mPW1PBE
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MPW1PBE = 484

 ! Kang-Musgrave hybrid
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_KMLYP = 485

 ! Long-range corrected PBE (LC-wPBE) by Weintraub, Henderson and Scuseria
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_WPBE_WHS = 486

 ! Long-range corrected short-range hybrid PBE (LC-wPBE) by Weintraub, Henderson and Scuseria
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_WPBEH_WHS = 487

 ! Long-range corrected PBE (LC-wPBE) by Weintraub, Henderson and Scuseria
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_WPBE08_WHS = 488

 ! Long-range corrected PBE (LC-wPBE) by Weintraub, Henderson and Scuseria
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_WPBESOL_WHS = 489

 ! CAM-B3LYP retuned using ionization potentials of water
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_QTP_00 = 490

 ! CAM-B3LYP retuned using ionization potentials of water
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_QTP_02 = 491

 ! CAM-B3LYP retuned using ionization potentials of water
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_QTP = 492

 ! Regularized SCAN exchange by Bartok and Yates
 integer(c_int), parameter, public :: XC_MGGA_X_RSCAN = 493

 ! Regularized SCAN correlation by Bartok and Yates
 integer(c_int), parameter, public :: XC_MGGA_C_RSCAN = 494

 ! Swart 2012 GGA exchange
 integer(c_int), parameter, public :: XC_GGA_X_S12G = 495

 ! Swart 2012 hybrid exchange
 integer(c_int), parameter, public :: XC_HYB_GGA_X_S12H = 496

 ! Re-regularized SCAN exchange by Furness et al
 integer(c_int), parameter, public :: XC_MGGA_X_R2SCAN = 497

 ! Re-regularized SCAN correlation by Furness et al
 integer(c_int), parameter, public :: XC_MGGA_C_R2SCAN = 498

 ! BLYP35
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_BLYP35 = 499

 ! von Weiszaecker correction to Thomas-Fermi
 integer(c_int), parameter, public :: XC_GGA_K_VW = 500

 ! Second-order gradient expansion of the kinetic energy density
 integer(c_int), parameter, public :: XC_GGA_K_GE2 = 501

 ! TF-lambda-vW form by Golden (l = 13/45)
 integer(c_int), parameter, public :: XC_GGA_K_GOLDEN = 502

 ! TF-lambda-vW form by Yonei and Tomishima (l = 1/5)
 integer(c_int), parameter, public :: XC_GGA_K_YT65 = 503

 ! TF-lambda-vW form by Baltin (l = 5/9)
 integer(c_int), parameter, public :: XC_GGA_K_BALTIN = 504

 ! TF-lambda-vW form by Lieb (l = 0.185909191)
 integer(c_int), parameter, public :: XC_GGA_K_LIEB = 505

 ! gamma-TFvW form by Acharya et al [$g = 1 - 1.412/N^{1/3}$]
 integer(c_int), parameter, public :: XC_GGA_K_ABSP1 = 506

 ! gamma-TFvW form by Acharya et al [$g = 1 - 1.332/N^{1/3}$]
 integer(c_int), parameter, public :: XC_GGA_K_ABSP2 = 507

 ! gamma-TFvW form by Gazquez and Robles
 integer(c_int), parameter, public :: XC_GGA_K_GR = 508

 ! gamma-TFvW form by Ludena
 integer(c_int), parameter, public :: XC_GGA_K_LUDENA = 509

 ! gamma-TFvW form by Ghosh and Parr
 integer(c_int), parameter, public :: XC_GGA_K_GP85 = 510

 ! Pearson 1992
 integer(c_int), parameter, public :: XC_GGA_K_PEARSON = 511

 ! Ou-Yang and Levy v.1
 integer(c_int), parameter, public :: XC_GGA_K_OL1 = 512

 ! Ou-Yang and Levy v.2
 integer(c_int), parameter, public :: XC_GGA_K_OL2 = 513

 ! Fuentealba & Reyes (B88 version)
 integer(c_int), parameter, public :: XC_GGA_K_FR_B88 = 514

 ! Fuentealba & Reyes (PW86 version)
 integer(c_int), parameter, public :: XC_GGA_K_FR_PW86 = 515

 ! DePristo and Kress
 integer(c_int), parameter, public :: XC_GGA_K_DK = 516

 ! Perdew
 integer(c_int), parameter, public :: XC_GGA_K_PERDEW = 517

 ! Vitos, Skriver, and Kollar
 integer(c_int), parameter, public :: XC_GGA_K_VSK = 518

 ! Vitos, Johansson, Kollar, and Skriver
 integer(c_int), parameter, public :: XC_GGA_K_VJKS = 519

 ! Ernzerhof
 integer(c_int), parameter, public :: XC_GGA_K_ERNZERHOF = 520

 ! Lembarki & Chermette
 integer(c_int), parameter, public :: XC_GGA_K_LC94 = 521

 ! Lee, Lee & Parr
 integer(c_int), parameter, public :: XC_GGA_K_LLP = 522

 ! Thakkar 1992
 integer(c_int), parameter, public :: XC_GGA_K_THAKKAR = 523

 ! short-range part of the PBE (default w=0 gives PBEh)
 integer(c_int), parameter, public :: XC_GGA_X_WPBEH = 524

 ! HJS screened exchange PBE version
 integer(c_int), parameter, public :: XC_GGA_X_HJS_PBE = 525

 ! HJS screened exchange PBE_SOL version
 integer(c_int), parameter, public :: XC_GGA_X_HJS_PBE_SOL = 526

 ! HJS screened exchange B88 version
 integer(c_int), parameter, public :: XC_GGA_X_HJS_B88 = 527

 ! HJS screened exchange B97x version
 integer(c_int), parameter, public :: XC_GGA_X_HJS_B97X = 528

 ! Short-range recipe for B88 functional - erf
 integer(c_int), parameter, public :: XC_GGA_X_ITYH = 529

 ! Short-range recipe for B88 functional - Yukawa
 integer(c_int), parameter, public :: XC_GGA_X_SFAT = 530

 ! wB97M-V exchange-correlation functional
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_WB97M_V = 531

 ! Slater exchange with relativistic corrections
 integer(c_int), parameter, public :: XC_LDA_X_REL = 532

 ! Semiclassical GGA at fourth order
 integer(c_int), parameter, public :: XC_GGA_X_SG4 = 533

 ! Semiclassical GGA at fourth order
 integer(c_int), parameter, public :: XC_GGA_C_SG4 = 534

 ! Gilbert and Gill 1999
 integer(c_int), parameter, public :: XC_GGA_X_GG99 = 535

 ! LDA constructed from slab-like systems of 1 electron
 integer(c_int), parameter, public :: XC_LDA_XC_1D_EHWLRG_1 = 536

 ! LDA constructed from slab-like systems of 2 electrons
 integer(c_int), parameter, public :: XC_LDA_XC_1D_EHWLRG_2 = 537

 ! LDA constructed from slab-like systems of 3 electrons
 integer(c_int), parameter, public :: XC_LDA_XC_1D_EHWLRG_3 = 538

 ! PBE power
 integer(c_int), parameter, public :: XC_GGA_X_PBEPOW = 539

 ! Tao and Mo 2016 exchange
 integer(c_int), parameter, public :: XC_MGGA_X_TM = 540

 ! meta-GGA version of VT{8,4} GGA
 integer(c_int), parameter, public :: XC_MGGA_X_VT84 = 541

 ! TPSS with correct surface asymptotics
 integer(c_int), parameter, public :: XC_MGGA_X_SA_TPSS = 542

 ! Perdew and Constantin 2007
 integer(c_int), parameter, public :: XC_MGGA_K_PC07 = 543

 ! Gilbert and Gill 1999 (mixed)
 integer(c_int), parameter, public :: XC_GGA_X_KGG99 = 544

 ! high local exchange 2016
 integer(c_int), parameter, public :: XC_GGA_XC_HLE16 = 545

 ! Short-range LDA exchange with error function kernel (erfc)
 integer(c_int), parameter, public :: XC_LDA_X_ERF = 546

 ! Lee-Parr reparametrization A
 integer(c_int), parameter, public :: XC_LDA_XC_LP_A = 547

 ! Lee-Parr reparametrization B
 integer(c_int), parameter, public :: XC_LDA_XC_LP_B = 548

 ! Rae self-energy corrected exchange
 integer(c_int), parameter, public :: XC_LDA_X_RAE = 549

 ! Wigner including kinetic energy contribution
 integer(c_int), parameter, public :: XC_LDA_K_ZLP = 550

 ! McWeeny 76
 integer(c_int), parameter, public :: XC_LDA_C_MCWEENY = 551

 ! Brual & Rothstein 78
 integer(c_int), parameter, public :: XC_LDA_C_BR78 = 552

 ! GGA component of SCAN
 integer(c_int), parameter, public :: XC_GGA_C_SCAN_E0 = 553

 ! Proynov and Kong 2009
 integer(c_int), parameter, public :: XC_LDA_C_PK09 = 554

 ! GapC
 integer(c_int), parameter, public :: XC_GGA_C_GAPC = 555

 ! Gaploc
 integer(c_int), parameter, public :: XC_GGA_C_GAPLOC = 556

 ! another spin-dependent correction to PBEint
 integer(c_int), parameter, public :: XC_GGA_C_ZVPBEINT = 557

 ! another spin-dependent correction to PBEsol
 integer(c_int), parameter, public :: XC_GGA_C_ZVPBESOL = 558

 ! Takkar and McCarthy reparametrization
 integer(c_int), parameter, public :: XC_GGA_C_TM_LYP = 559

 ! Thakkar and McCarthy reparametrization
 integer(c_int), parameter, public :: XC_GGA_C_TM_PBE = 560

 ! Wilson 94 (Eq. 25)
 integer(c_int), parameter, public :: XC_GGA_C_W94 = 561

 ! Krieger, Chen, Iafrate, and Savin
 integer(c_int), parameter, public :: XC_MGGA_C_KCIS = 562

 ! Hybrid based on KCIS
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_B0KCIS = 563

 ! Lee & Parr, Eq. (56)
 integer(c_int), parameter, public :: XC_MGGA_XC_LP90 = 564

 ! A dynamical correlation functional
 integer(c_int), parameter, public :: XC_GGA_C_CS1 = 565

 ! MPW1KCIS for barrier heights
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_MPW1KCIS = 566

 ! MPWKCIS1K for barrier heights
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_MPWKCIS1K = 567

 ! PBE1KCIS for binding energies
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_PBE1KCIS = 568

 ! TPSS1KCIS for thermochemistry and kinetics
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_TPSS1KCIS = 569

 ! Becke 88 reoptimized to be used with tau1
 integer(c_int), parameter, public :: XC_GGA_X_B88M = 570

 ! Meta-GGA correlation by Becke
 integer(c_int), parameter, public :: XC_MGGA_C_B88 = 571

 ! B5050LYP
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_B5050LYP = 572

 ! Wigner with corresponding LYP parameters
 integer(c_int), parameter, public :: XC_LDA_C_OW_LYP = 573

 ! Optimized Wigner
 integer(c_int), parameter, public :: XC_LDA_C_OW = 574

 ! GX functional of Loos
 integer(c_int), parameter, public :: XC_MGGA_X_GX = 575

 ! PBE-GX functional of Loos
 integer(c_int), parameter, public :: XC_MGGA_X_PBE_GX = 576

 ! Groth, Dornheim, Sjostrom, Malone, Foulkes, Bonitz
 integer(c_int), parameter, public :: XC_LDA_XC_GDSMFB = 577

 ! Gordon and Kim 1972
 integer(c_int), parameter, public :: XC_LDA_C_GK72 = 578

 ! Karasiev reparameterization of Chachiyo
 integer(c_int), parameter, public :: XC_LDA_C_KARASIEV = 579

 ! Liu-Parr kinetic
 integer(c_int), parameter, public :: XC_LDA_K_LP96 = 580

 ! revised SCAN
 integer(c_int), parameter, public :: XC_MGGA_X_REVSCAN = 581

 ! revised SCAN
 integer(c_int), parameter, public :: XC_MGGA_C_REVSCAN = 582

 ! revised SCAN hybrid exchange (SCAN0)
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_REVSCAN0 = 583

 ! SCAN + VV10 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_SCAN_VV10 = 584

 ! REVSCAN + VV10 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_REVSCAN_VV10 = 585

 ! Becke-Roussel 89 with an explicit inversion of x(y), gamma = 0.8
 integer(c_int), parameter, public :: XC_MGGA_X_BR89_EXPLICIT = 586

 ! Keal and Tozer, version 3
 integer(c_int), parameter, public :: XC_GGA_XC_KT3 = 587

 ! Baer and Neuhauser, gamma=1
 integer(c_int), parameter, public :: XC_HYB_LDA_XC_BN05 = 588

 ! Livshits and Baer, empirical functional also used for IP tuning
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LB07 = 589

 ! Long-range LDA correlation functional
 integer(c_int), parameter, public :: XC_LDA_C_PMGB06 = 590

 ! Combined analytical theory with Monte Carlo sampling
 integer(c_int), parameter, public :: XC_GGA_K_GDS08 = 591

 ! As GDS08 but for an electron gas with spin
 integer(c_int), parameter, public :: XC_GGA_K_GHDS10 = 592

 ! Reparametrized GHDS10
 integer(c_int), parameter, public :: XC_GGA_K_GHDS10R = 593

 ! Trickey, Karasiev, and Vela
 integer(c_int), parameter, public :: XC_GGA_K_TKVLN = 594

 ! Three parameter PBE-like expansion
 integer(c_int), parameter, public :: XC_GGA_K_PBE3 = 595

 ! Four parameter PBE-like expansion
 integer(c_int), parameter, public :: XC_GGA_K_PBE4 = 596

 ! Intermediate form between PBE3 and PBE4
 integer(c_int), parameter, public :: XC_GGA_K_EXP4 = 597

 ! Becke 98
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_B98 = 598

 ! Neural network LDA from Tozer et al
 integer(c_int), parameter, public :: XC_LDA_XC_TIH = 599

 ! Exchange in 1D for an exponentially screened interaction
 integer(c_int), parameter, public :: XC_LDA_X_1D_EXPONENTIAL = 600

 ! Short-range recipe for PBE functional - Yukawa
 integer(c_int), parameter, public :: XC_GGA_X_SFAT_PBE = 601

 ! Becke-Roussel 89 with an explicit inversion of x(y), gamma = 1.0
 integer(c_int), parameter, public :: XC_MGGA_X_BR89_EXPLICIT_1 = 602

 ! Regularized TPSS
 integer(c_int), parameter, public :: XC_MGGA_X_REGTPSS = 603

 ! Functional derivative recovered from the stray LB94 potential
 integer(c_int), parameter, public :: XC_GGA_X_FD_LB94 = 604

 ! Revised FD_LB94
 integer(c_int), parameter, public :: XC_GGA_X_FD_REVLB94 = 605

 ! PBEloc variation with enhanced compatibility with exact exchange
 integer(c_int), parameter, public :: XC_GGA_C_ZVPBELOC = 606

 ! Hybrid based on APBE
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_APBE0 = 607

 ! Hybrid based in APBE and zvPBEloc
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_HAPBE = 608

 ! JS17 meta-GGA for 2D
 integer(c_int), parameter, public :: XC_MGGA_X_2D_JS17 = 609

 ! Similar to CAM-B3LYP, but trying to reduce the many-electron self-interaction
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_RCAM_B3LYP = 610

 ! hybrid fitted to carbon NMR shifts
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WC04 = 611

 ! hybrid fitted to proton NMR shifts
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WP04 = 612

 ! Luo-Karasiev-Trickey GGA kinetic
 integer(c_int), parameter, public :: XC_GGA_K_LKT = 613

 ! CAM version of B3LYP, tuned for TDDFT
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAMH_B3LYP = 614

 ! Long-range corrected short-range hybrid PBE (whPBE0) by Shao et al
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_WHPBE0 = 615

 ! Three parameter PBE-like expansion
 integer(c_int), parameter, public :: XC_GGA_K_PBE2 = 616

 ! L0.4 by Laricchia et al
 integer(c_int), parameter, public :: XC_MGGA_K_L04 = 617

 ! L0.6 by Laricchia et al
 integer(c_int), parameter, public :: XC_MGGA_K_L06 = 618

 ! VT84F by Karasiev et al
 integer(c_int), parameter, public :: XC_GGA_K_VT84F = 619

 ! LGAP by Constantin et al
 integer(c_int), parameter, public :: XC_GGA_K_LGAP = 620

 ! Reduced derivative approximation by Karasiev et al
 integer(c_int), parameter, public :: XC_MGGA_K_RDA = 621

 ! Short-range recipe for OPTX functional
 integer(c_int), parameter, public :: XC_GGA_X_ITYH_OPTX = 622

 ! Short-range recipe for PBE functional
 integer(c_int), parameter, public :: XC_GGA_X_ITYH_PBE = 623

 ! Short-range LYP by Ai, Fang, and Su
 integer(c_int), parameter, public :: XC_GGA_C_LYPR = 624

 ! LC version of BLYP for electron affinities
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_BLYP_EA = 625

 ! Regularized Tao and Mo exchange
 integer(c_int), parameter, public :: XC_MGGA_X_REGTM = 626

 ! Second-order gradient expansion
 integer(c_int), parameter, public :: XC_MGGA_K_GEA2 = 627

 ! Fourth-order gradient expansion
 integer(c_int), parameter, public :: XC_MGGA_K_GEA4 = 628

 ! mGGA-rev functional by Cancio, Stewart, and Kuna (a=1)
 integer(c_int), parameter, public :: XC_MGGA_K_CSK1 = 629

 ! mGGA-rev functional by Cancio, Stewart, and Kuna (a=4)
 integer(c_int), parameter, public :: XC_MGGA_K_CSK4 = 630

 ! mGGAloc-rev functional by Cancio, Stewart, and Kuna (a=1)
 integer(c_int), parameter, public :: XC_MGGA_K_CSK_LOC1 = 631

 ! mGGAloc-rev functional by Cancio, Stewart, and Kuna (a=4)
 integer(c_int), parameter, public :: XC_MGGA_K_CSK_LOC4 = 632

 ! LGAP-GE by Constantin et al
 integer(c_int), parameter, public :: XC_GGA_K_LGAP_GE = 633

 ! Reoptimized PC07 by Mejia-Rodriguez and Trickey
 integer(c_int), parameter, public :: XC_MGGA_K_PC07_OPT = 634

 ! empirically optimized gamma-TFvW form
 integer(c_int), parameter, public :: XC_GGA_K_TFVW_OPT = 635

 ! LC version of B88
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_BOP = 636

 ! LC version of PBE
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_PBEOP = 637

 ! Krieger, Chen, and Kurth
 integer(c_int), parameter, public :: XC_MGGA_C_KCISK = 638

 ! LC version of BLYP with correlation only in the short range
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_LC_BLYPR = 639

 ! Modified CAM-B3LYP by Day, Nguyen and Pachter
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_MCAM_B3LYP = 640

 ! Short-range LDA exchange with Yukawa attenuation
 integer(c_int), parameter, public :: XC_LDA_X_YUKAWA = 641

 ! Re-regularized SCAN correlation with larger value for eta
 integer(c_int), parameter, public :: XC_MGGA_C_R2SCAN01 = 642

 ! Revised correlation energy for MGGAC exchange functional
 integer(c_int), parameter, public :: XC_MGGA_C_RMGGAC = 643

 ! MCML exchange
 integer(c_int), parameter, public :: XC_MGGA_X_MCML = 644

 ! Re-regularized SCAN exchange by Furness et al with larger value for eta
 integer(c_int), parameter, public :: XC_MGGA_X_R2SCAN01 = 645

 ! Swart 2012 range-separated hybrid GGA exchange
 integer(c_int), parameter, public :: XC_HYB_GGA_X_CAM_S12G = 646

 ! Swart 2012 range-separated hybrid GGA exchange
 integer(c_int), parameter, public :: XC_HYB_GGA_X_CAM_S12H = 647

 ! r++SCAN: rSCAN with uniform density limit and coordinate scaling behavior
 integer(c_int), parameter, public :: XC_MGGA_X_RPPSCAN = 648

 ! r++SCAN: rSCAN with uniform density limit and coordinate scaling behavior
 integer(c_int), parameter, public :: XC_MGGA_C_RPPSCAN = 649

 ! r$^{4}$SCAN, a functional that satisfies the same exact constraints that SCAN does
 integer(c_int), parameter, public :: XC_MGGA_X_R4SCAN = 650

 ! Exchange part of VCML-rVV10 by Trepte and Voss
 integer(c_int), parameter, public :: XC_MGGA_X_VCML = 651

 ! VCML-rVV10 by Trepte and Voss
 integer(c_int), parameter, public :: XC_MGGA_XC_VCML_RVV10 = 652

 ! Google Accelerated Science 22
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_GAS22 = 658

 ! r2SCANh: r2SCAN hybrid like TPSSh with 10% exact exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_R2SCANH = 659

 ! r2SCAN0: r2SCAN hybrid like PBE0 with 25% exact exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_R2SCAN0 = 660

 ! r2SCAN50: r2SCAN hybrid like PBE50 with 50% exact exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_R2SCAN50 = 661

 ! CAM hybrid screened exchange PBE version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAM_PBEH = 681

 ! CAMY hybrid screened exchange PBE version
 integer(c_int), parameter, public :: XC_HYB_GGA_XC_CAMY_PBEH = 682

 ! Ruggeri, Rios, and Alavi unrestricted fit
 integer(c_int), parameter, public :: XC_LDA_C_UPW92 = 683

 ! Ruggeri, Rios, and Alavi restricted fit
 integer(c_int), parameter, public :: XC_LDA_C_RPW92 = 684

 ! LDA-type exchange with tau-dependent potential
 integer(c_int), parameter, public :: XC_MGGA_X_TLDA = 685

 ! Tao 2001
 integer(c_int), parameter, public :: XC_MGGA_X_EDMGGA = 686

 ! Generalized density-matrix with a=1/2
 integer(c_int), parameter, public :: XC_MGGA_X_GDME_NV = 687

 ! Reparametrized local-density approximation
 integer(c_int), parameter, public :: XC_MGGA_X_RLDA = 688

 ! Generalized density-matrix with a=0
 integer(c_int), parameter, public :: XC_MGGA_X_GDME_0 = 689

 ! Generalized density-matrix with a=0.00638
 integer(c_int), parameter, public :: XC_MGGA_X_GDME_KOS = 690

 ! Varied-terms (VT) mGGA of Koehl, Odom, and Scuseria
 integer(c_int), parameter, public :: XC_MGGA_X_GDME_VT = 691

 ! simple local model for Slater potential
 integer(c_int), parameter, public :: XC_LDA_X_SLOC = 692

 ! revised Tao and Mo 2016 exchange
 integer(c_int), parameter, public :: XC_MGGA_X_REVTM = 693

 ! revised Tao and Mo 2016 exchange
 integer(c_int), parameter, public :: XC_MGGA_C_REVTM = 694

 ! EDMGGA hybrid
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_EDMGGAH = 695

 ! Modified Becke-Roussel for band gaps - cuspless hole
 integer(c_int), parameter, public :: XC_MGGA_X_MBRXC_BG = 696

 ! Modified Becke-Roussel for band gaps - hydrogen hole
 integer(c_int), parameter, public :: XC_MGGA_X_MBRXH_BG = 697

 ! Half-and-half meta-LDAized LDA exchange by Lehtola and Marques
 integer(c_int), parameter, public :: XC_MGGA_X_HLTA = 698

 ! Half-and-half meta-LDAized PW correlation by Lehtola and Marques
 integer(c_int), parameter, public :: XC_MGGA_C_HLTAPW = 699

 ! Deorbitalized SCAN (SCAN-L) exchange
 integer(c_int), parameter, public :: XC_MGGA_X_SCANL = 700

 ! Deorbitalized revised SCAN (revSCAN-L) exchange
 integer(c_int), parameter, public :: XC_MGGA_X_REVSCANL = 701

 ! Deorbitalized SCAN (SCAN-L) correlation
 integer(c_int), parameter, public :: XC_MGGA_C_SCANL = 702

 ! SCAN-L + rVV10 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_SCANL_RVV10 = 703

 ! SCAN-L + VV10 correlation
 integer(c_int), parameter, public :: XC_MGGA_C_SCANL_VV10 = 704

 ! Jana and Samal 2018, screened range-separated TM exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_JS18 = 705

 ! Patra, Jana and Samal 2018, screened range-separated TM exchange
 integer(c_int), parameter, public :: XC_HYB_MGGA_X_PJS18 = 706

 ! TASK exchange of Aschebrock and Kuemmel
 integer(c_int), parameter, public :: XC_MGGA_X_TASK = 707

 ! MGGAC exchange of Patra et al
 integer(c_int), parameter, public :: XC_MGGA_X_MGGAC = 711

 ! beta fitted to LC20 to be used with MGGAC
 integer(c_int), parameter, public :: XC_GGA_C_MGGAC = 712

 ! modified Becke-Roussel by Patra et al
 integer(c_int), parameter, public :: XC_MGGA_X_MBR = 716

 ! Deorbitalized re-regularized SCAN (r2SCAN-L) exchange
 integer(c_int), parameter, public :: XC_MGGA_X_R2SCANL = 718

 ! Deorbitalized re-regularized SCAN (r2SCAN-L) correlation
 integer(c_int), parameter, public :: XC_MGGA_C_R2SCANL = 719

 ! Long-range corrected TM-LYP by Jana et al
 integer(c_int), parameter, public :: XC_HYB_MGGA_XC_LC_TMLYP = 720

 ! modified TASK exchange
 integer(c_int), parameter, public :: XC_MGGA_X_MTASK = 724

 ! Functional for quasi-1D systems
 integer(c_int), parameter, public :: XC_GGA_X_Q1D = 734

 ! KTBM learned exchange - 0
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_0 = 735

 ! KTBM learned exchange - 1
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_1 = 736

 ! KTBM learned exchange - 2
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_2 = 737

 ! KTBM learned exchange - 3
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_3 = 738

 ! KTBM learned exchange - 4
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_4 = 739

 ! KTBM learned exchange - 5
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_5 = 740

 ! KTBM learned exchange - 6
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_6 = 741

 ! KTBM learned exchange - 7
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_7 = 742

 ! KTBM learned exchange - 8
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_8 = 743

 ! KTBM learned exchange - 9
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_9 = 744

 ! KTBM learned exchange - 10
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_10 = 745

 ! KTBM learned exchange - 11
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_11 = 746

 ! KTBM learned exchange - 12
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_12 = 747

 ! KTBM learned exchange - 13
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_13 = 748

 ! KTBM learned exchange - 14
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_14 = 749

 ! KTBM learned exchange - 15
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_15 = 750

 ! KTBM learned exchange - 16
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_16 = 751

 ! KTBM learned exchange - 17
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_17 = 752

 ! KTBM learned exchange - 18
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_18 = 753

 ! KTBM learned exchange - 19
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_19 = 754

 ! KTBM learned exchange - 20
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_20 = 755

 ! KTBM learned exchange - 21
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_21 = 756

 ! KTBM learned exchange - 22
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_22 = 757

 ! KTBM learned exchange - 23
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_23 = 758

 ! KTBM learned exchange - 24
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_24 = 759

 ! KTBM learned exchange - GAP
 integer(c_int), parameter, public :: XC_MGGA_X_KTBM_GAP = 760
# 159 "./libxc_master.F90" 2

  ! These are old names kept for compatibility
  integer(c_int), parameter, public :: &
    XC_LDA_X_1D = 21, &
    XC_GGA_X_BGCP = 38, &
    XC_GGA_C_BGCP = 39, &
    XC_GGA_C_BCGP = 39, &
    XC_GGA_C_VPBE = 83, &
    XC_GGA_XC_LB = 160, &
    XC_MGGA_C_CC06 = 229, &
    XC_GGA_K_ABSR1 = 506, &
    XC_GGA_K_ABSR2 = 507, &
    XC_LDA_C_LP_A = 547, &
    XC_LDA_C_LP_B = 548, &
    XC_MGGA_C_LP90 = 564

  !----------------------------------------------------------------
  interface
    subroutine xc_version(major, minor, micro) bind(c)
      import
      integer(c_int), intent(out) :: major, minor, micro
    end subroutine xc_version

    type(c_ptr) function xc_version_string() bind(c)
      import
    end function xc_version_string

    type(c_ptr) function xc_reference() bind(c)
      import
    end function xc_reference

    type(c_ptr) function xc_reference_doi() bind(c)
      import
    end function xc_reference_doi

    type(c_ptr) function xc_reference_key() bind(c)
      import
    end function xc_reference_key
  end interface


  !----------------------------------------------------------------
  type :: xc_f03_func_info_t
    private
    type(c_ptr) :: ptr = C_NULL_PTR
  end type xc_f03_func_info_t

  interface
    integer(c_int) function xc_func_info_get_number(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_number

    integer(c_int) function xc_func_info_get_kind(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_kind

    type(c_ptr) function xc_func_info_get_name(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_name

    integer(c_int) function xc_func_info_get_family(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_family

    integer(c_int) function xc_func_info_get_flags(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_flags

    type(c_ptr) function xc_func_info_get_references(info, number) bind(c)
      import
      type(c_ptr), value :: info
      integer(c_int), value :: number
    end function xc_func_info_get_references

    integer(c_int) function xc_func_info_get_n_ext_params(info) bind(c)
      import
      type(c_ptr), value :: info
    end function xc_func_info_get_n_ext_params

    type(c_ptr) function xc_func_info_get_ext_params_name(info, number) bind(c)
      import
      type(c_ptr), value :: info
      integer(c_int), value :: number
    end function xc_func_info_get_ext_params_name

    type(c_ptr) function xc_func_info_get_ext_params_description(info, number) bind(c)
      import
      type(c_ptr), value :: info
      integer(c_int), value :: number
    end function xc_func_info_get_ext_params_description

    real(c_double) function xc_func_info_get_ext_params_default_value(info, number) bind(c)
      import
      type(c_ptr), value :: info
      integer(c_int), value :: number
    end function xc_func_info_get_ext_params_default_value

  end interface

  !----------------------------------------------------------------
  type :: xc_f03_func_reference_t
    private
    type(c_ptr) :: ptr = C_NULL_PTR
  end type xc_f03_func_reference_t

  interface
    type(c_ptr) function xc_func_reference_get_ref(reference) bind(c)
      import
      type(c_ptr), value :: reference
    end function xc_func_reference_get_ref

    type(c_ptr) function xc_func_reference_get_doi(reference) bind(c)
      import
      type(c_ptr), value :: reference
    end function xc_func_reference_get_doi

    type(c_ptr) function xc_func_reference_get_bibtex(reference) bind(c)
      import
      type(c_ptr), value :: reference
    end function xc_func_reference_get_bibtex

    type(c_ptr) function xc_func_reference_get_key(reference) bind(c)
      import
      type(c_ptr), value :: reference
    end function xc_func_reference_get_key
  end interface

  !----------------------------------------------------------------
  type :: xc_f03_func_t
    private
    type(c_ptr) :: ptr = C_NULL_PTR
  end type xc_f03_func_t

  interface
    type(c_ptr) function xc_func_alloc() bind(c)
      import
    end function xc_func_alloc

    integer(c_int) function xc_func_init(p, functional, nspin) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_int), value :: functional, nspin
    end function xc_func_init

    subroutine xc_func_end(p) bind(c)
      import
      type(c_ptr), value :: p
    end subroutine xc_func_end

    subroutine xc_func_free(p) bind(c)
      import
      type(c_ptr), value :: p
    end subroutine xc_func_free

    subroutine libxc_free(p) bind(c)
      import
      type(c_ptr), value :: p
    end subroutine libxc_free

    type(c_ptr) function xc_func_get_info(p) bind(c)
      import
      type(c_ptr), value :: p
    end function xc_func_get_info

    type(c_ptr) function xc_functional_get_name(number) bind(c)
      import
      integer(c_int), value :: number
    end function xc_functional_get_name

    integer(c_int) function xc_functional_get_number(func_string) bind(c)
      import
      character(kind=c_char), intent(in) :: func_string(*)
    end function xc_functional_get_number

    integer(c_int) function xc_family_from_id(id, family, number) bind(c)
      import
      integer(c_int), value :: id
      type(c_ptr), value :: family, number
    end function xc_family_from_id

    integer(c_int) function xc_f03_number_of_functionals() bind(c, name="xc_number_of_functionals")
      import
    end function xc_f03_number_of_functionals

    integer(c_int) function xc_f03_maximum_name_length() bind(c, name="xc_maximum_name_length")
      import
    end function xc_f03_maximum_name_length

    subroutine xc_f03_available_functional_numbers(list) bind(c, name="xc_available_functional_numbers")
      import
      integer(c_int), intent(out) :: list(*)
    end subroutine xc_f03_available_functional_numbers

    subroutine xc_available_functional_names(list) bind(c)
      import
      type(c_ptr) :: list(*)
    end subroutine xc_available_functional_names

    subroutine xc_func_set_dens_threshold(p, dens_threshold) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), value :: dens_threshold
    end subroutine xc_func_set_dens_threshold

    subroutine xc_func_set_zeta_threshold(p, zeta_threshold) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), value :: zeta_threshold
    end subroutine xc_func_set_zeta_threshold

    subroutine xc_func_set_sigma_threshold(p, sigma_threshold) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), value :: sigma_threshold
    end subroutine xc_func_set_sigma_threshold

    subroutine xc_func_set_tau_threshold(p, tau_threshold) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), value :: tau_threshold
    end subroutine xc_func_set_tau_threshold

    subroutine xc_func_set_ext_params(p, ext_params) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), intent(in) :: ext_params(*)
    end subroutine xc_func_set_ext_params

    subroutine xc_func_set_ext_params_name(p, name, par) bind(c)
      import
      type(c_ptr), value :: p
      character(kind=c_char), intent(in) :: name(*)
      real(c_double), value :: par
    end subroutine xc_func_set_ext_params_name
end interface

  ! LDAs
  !----------------------------------------------------------------
  interface
    subroutine xc_lda(p, np, rho, zk, vrho, v2rho2, v3rho3, v4rho4) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*), v3rho3(*), v4rho4(*)
    end subroutine xc_lda

    subroutine xc_lda_exc(p, np, rho, zk) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: zk(*)
    end subroutine xc_lda_exc

    subroutine xc_lda_exc_vxc(p, np, rho, zk, vrho) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: zk(*), vrho(*)
    end subroutine xc_lda_exc_vxc

    subroutine xc_lda_exc_vxc_fxc(p, np, rho, zk, vrho, v2rho2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*)
    end subroutine xc_lda_exc_vxc_fxc

    subroutine xc_lda_exc_vxc_fxc_kxc(p, np, rho, zk, vrho, v2rho2, v3rho3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*), v3rho3(*)
    end subroutine xc_lda_exc_vxc_fxc_kxc

    subroutine xc_lda_vxc(p, np, rho, vrho) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: vrho(*)
    end subroutine xc_lda_vxc

    subroutine xc_lda_vxc_fxc(p, np, rho, vrho, v2rho2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: vrho(*), v2rho2(*)
    end subroutine xc_lda_vxc_fxc

    subroutine xc_lda_vxc_fxc_kxc(p, np, rho, vrho, v2rho2, v3rho3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: vrho(*), v2rho2(*), v3rho3(*)
    end subroutine xc_lda_vxc_fxc_kxc

    subroutine xc_lda_fxc(p, np, rho, v2rho2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: v2rho2(*)
    end subroutine xc_lda_fxc

    subroutine xc_lda_kxc(p, np, rho, v3rho3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: v3rho3(*)
    end subroutine xc_lda_kxc

    subroutine xc_lda_lxc(p, np, rho, v4rho4) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*)
      real(c_double), intent(out) :: v4rho4(*)
    end subroutine xc_lda_lxc
  end interface


  ! GGAs
  !----------------------------------------------------------------
  interface
    subroutine xc_gga(p, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3, &
         v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4 &
         ) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)
      real(c_double), intent(out) :: v4rho4(*), v4rho3sigma(*), v4rho2sigma2(*), v4rhosigma3(*), v4sigma4(*)
    end subroutine xc_gga

    subroutine xc_gga_exc(p, np, rho, sigma, zk) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: zk(*)
    end subroutine xc_gga_exc

    subroutine xc_gga_exc_vxc(p, np, rho, sigma, zk, vrho, vsigma) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
    end subroutine xc_gga_exc_vxc

    subroutine xc_gga_exc_vxc_fxc(p, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    end subroutine xc_gga_exc_vxc_fxc

    subroutine xc_gga_exc_vxc_fxc_kxc(p, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)
    end subroutine xc_gga_exc_vxc_fxc_kxc

    subroutine xc_gga_vxc(p, np, rho, sigma, vrho, vsigma) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*)
    end subroutine xc_gga_vxc

    subroutine xc_gga_vxc_fxc(p, np, rho, sigma, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    end subroutine xc_gga_vxc_fxc

    subroutine xc_gga_vxc_fxc_kxc(p, np, rho, sigma, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)
    end subroutine xc_gga_vxc_fxc_kxc

    subroutine xc_gga_fxc(p, np, rho, sigma, v2rho2, v2rhosigma, v2sigma2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    end subroutine xc_gga_fxc

    subroutine xc_gga_kxc(p, np, rho, sigma, v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)
    end subroutine xc_gga_kxc

    subroutine xc_gga_lxc(p, np, rho, sigma, v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*)
      real(c_double), intent(out) :: v4rho4(*), v4rho3sigma(*), v4rho2sigma2(*), v4rhosigma3(*), v4sigma4(*)
    end subroutine xc_gga_lxc

  end interface


  interface
    real(c_double) function xc_gga_ak13_get_asymptotic(homo) bind(c)
      import
      real(c_double), value :: homo
    end function xc_gga_ak13_get_asymptotic
  end interface


  interface
    real(c_double) function xc_hyb_exx_coef(p) bind(c)
      import
      type(c_ptr), value :: p
    end function xc_hyb_exx_coef

    subroutine xc_hyb_cam_coef(p, omega, alpha, beta) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), intent(out) :: omega, alpha, beta
    end subroutine xc_hyb_cam_coef

    subroutine xc_nlc_coef(p, nlc_b, nlc_c) bind(c)
      import
      type(c_ptr), value :: p
      real(c_double), intent(out) :: nlc_b, nlc_c
    end subroutine xc_nlc_coef

    integer(c_int) function xc_num_aux_funcs(p) bind(c, name="xc_num_aux_funcs")
      import
      type(c_ptr), value :: p
    end function xc_num_aux_funcs

    subroutine xc_aux_func_ids(p, ids) bind(c, name="xc_aux_func_ids")
      import
      type(c_ptr), value :: p
      integer(c_int), intent(out) :: ids(*)
    end subroutine xc_aux_func_ids

    subroutine xc_aux_func_weights(p, weights) bind(c, name="xc_aux_func_weights")
      import
      type(c_ptr), value :: p
      real(c_double), intent(in) :: weights(*)
    end subroutine xc_aux_func_weights
  end interface


  ! the meta-GGAs
  !----------------------------------------------------------------
  interface
    subroutine xc_mgga(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3, &
         v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
         v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
         v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
         v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
         v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
         v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
         v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
         ) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
           v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
           v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
           v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
           v3lapltau2(*), v3tau3(*)
      real(c_double), intent(out) :: &
           v4rho4(*), v4rho3sigma(*), v4rho3lapl(*), v4rho3tau(*), v4rho2sigma2(*), v4rho2sigmalapl(*), &
           v4rho2sigmatau(*), v4rho2lapl2(*), v4rho2lapltau(*), v4rho2tau2(*), v4rhosigma3(*), &
           v4rhosigma2lapl(*), v4rhosigma2tau(*), v4rhosigmalapl2(*), v4rhosigmalapltau(*), &
           v4rhosigmatau2(*), v4rholapl3(*), v4rholapl2tau(*), v4rholapltau2(*), v4rhotau3(*), v4sigma4(*), &
           v4sigma3lapl(*), v4sigma3tau(*), v4sigma2lapl2(*), v4sigma2lapltau(*), v4sigma2tau2(*), &
           v4sigmalapl3(*), v4sigmalapl2tau(*), v4sigmalapltau2(*), v4sigmatau3(*), v4lapl4(*), &
           v4lapl3tau(*), v4lapl2tau2(*), v4lapltau3(*), v4tau4(*)
    end subroutine xc_mgga

    subroutine xc_mgga_exc(p, np, rho, sigma, lapl, tau, zk) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: zk(*)
    end subroutine xc_mgga_exc

    subroutine xc_mgga_exc_vxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
    end subroutine xc_mgga_exc_vxc

    subroutine xc_mgga_exc_vxc_fxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    end subroutine xc_mgga_exc_vxc_fxc

    subroutine xc_mgga_exc_vxc_fxc_kxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
           v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
           v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
           v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
           v3lapltau2(*), v3tau3(*)
    end subroutine xc_mgga_exc_vxc_fxc_kxc

    subroutine xc_mgga_vxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)
    end subroutine xc_mgga_vxc

    subroutine xc_mgga_vxc_fxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    end subroutine xc_mgga_vxc_fxc

    subroutine xc_mgga_vxc_fxc_kxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
           v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
           v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
           v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
           v3lapltau2(*), v3tau3(*)
    end subroutine xc_mgga_vxc_fxc_kxc

    subroutine xc_mgga_fxc(p, np, rho, sigma, lapl, tau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
           v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    end subroutine xc_mgga_fxc

    subroutine xc_mgga_kxc(p, np, rho, sigma, lapl, tau, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
           v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
           v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
           v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
           v3lapltau2(*), v3tau3(*)
    end subroutine xc_mgga_kxc

    subroutine xc_mgga_lxc(p, np, rho, sigma, lapl, tau, &
         v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
         v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
         v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
         v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
         v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
         v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
         v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
         ) bind(c)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: np
      real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
      real(c_double), intent(out) :: &
           v4rho4(*), v4rho3sigma(*), v4rho3lapl(*), v4rho3tau(*), v4rho2sigma2(*), v4rho2sigmalapl(*), &
           v4rho2sigmatau(*), v4rho2lapl2(*), v4rho2lapltau(*), v4rho2tau2(*), v4rhosigma3(*), &
           v4rhosigma2lapl(*), v4rhosigma2tau(*), v4rhosigmalapl2(*), v4rhosigmalapltau(*), &
           v4rhosigmatau2(*), v4rholapl3(*), v4rholapl2tau(*), v4rholapltau2(*), v4rhotau3(*), v4sigma4(*), &
           v4sigma3lapl(*), v4sigma3tau(*), v4sigma2lapl2(*), v4sigma2lapltau(*), v4sigma2tau2(*), &
           v4sigmalapl3(*), v4sigmalapl2tau(*), v4sigmalapltau2(*), v4sigmatau3(*), v4lapl4(*), &
           v4lapl3tau(*), v4lapl2tau2(*), v4lapltau3(*), v4tau4(*)
    end subroutine xc_mgga_lxc
  end interface

  contains

  !----------------------------------------------------------------
  subroutine xc_f03_version(major, minor, micro)
    integer(c_int), intent(out) :: major, minor, micro

    call xc_version(major, minor, micro)

  end subroutine xc_f03_version

  subroutine xc_f03_version_string(version)
    character(len=*), intent(out) :: version

    type(c_ptr) :: c_version

    c_version = xc_version_string()
    call c_to_f_string_ptr(c_version, version)

  end subroutine xc_f03_version_string

  subroutine xc_f03_reference(ref)
    character(len=*), intent(out) :: ref

    type(c_ptr) :: c_ref

    c_ref = xc_reference()
    call c_to_f_string_ptr(c_ref, ref)

  end subroutine xc_f03_reference

  subroutine xc_f03_reference_doi(doi)
    character(len=*), intent(out) :: doi

    type(c_ptr) :: c_doi

    c_doi = xc_reference_doi()
    call c_to_f_string_ptr(c_doi, doi)

  end subroutine xc_f03_reference_doi

  subroutine xc_f03_reference_key(key)
    character(len=*), intent(out) :: key

    type(c_ptr) :: c_key

    c_key = xc_reference_key()
    call c_to_f_string_ptr(c_key, key)

  end subroutine xc_f03_reference_key

  !----------------------------------------------------------------
  integer(c_int) function xc_f03_func_info_get_number(info) result(number)
    type(xc_f03_func_info_t), intent(in) :: info

    number = xc_func_info_get_number(info%ptr)

  end function xc_f03_func_info_get_number

  integer(c_int) function xc_f03_func_info_get_kind(info) result(kind)
    type(xc_f03_func_info_t), intent(in) :: info

    kind = xc_func_info_get_kind(info%ptr)

  end function xc_f03_func_info_get_kind

  character(len=128) function xc_f03_func_info_get_name(info) result(name)
    type(xc_f03_func_info_t), intent(in) :: info

    call c_to_f_string_ptr(xc_func_info_get_name(info%ptr), name)

  end function xc_f03_func_info_get_name

  integer(c_int) function xc_f03_func_info_get_family(info) result(family)
    type(xc_f03_func_info_t), intent(in) :: info

    family = xc_func_info_get_family(info%ptr)

  end function xc_f03_func_info_get_family

  integer(c_int) function xc_f03_func_info_get_flags(info) result(flags)
    type(xc_f03_func_info_t), intent(in) :: info

    flags = xc_func_info_get_flags(info%ptr)

  end function xc_f03_func_info_get_flags

  type(xc_f03_func_reference_t) function xc_f03_func_info_get_references(info, number) result(reference)
    type(xc_f03_func_info_t), intent(in) :: info
    integer(c_int), intent(inout) :: number ! number of the reference. Must be 0 in the first call

    type(c_ptr) :: next_ref

    reference%ptr = xc_func_info_get_references(info%ptr, number)
    if (.not. c_associated(reference%ptr)) then
       number = -1
    else
       next_ref = xc_func_info_get_references(info%ptr, INT(number + 1, c_int))
       if (c_associated(next_ref)) then
          number = number + 1
       else
          number = -1
       end if
    end if

  end function xc_f03_func_info_get_references

  integer(c_int) function xc_f03_func_info_get_n_ext_params(info) result(n_ext_params)
    type(xc_f03_func_info_t), intent(in) :: info

    n_ext_params = xc_func_info_get_n_ext_params(info%ptr)

  end function xc_f03_func_info_get_n_ext_params

  character(len=128) function xc_f03_func_info_get_ext_params_name(info, number) result(name)
    type(xc_f03_func_info_t), intent(in) :: info
    integer(c_int), intent(in) :: number

    call c_to_f_string_ptr(xc_func_info_get_ext_params_name(info%ptr, number), name)

  end function xc_f03_func_info_get_ext_params_name

  character(len=128) function xc_f03_func_info_get_ext_params_description(info, number) result(description)
    type(xc_f03_func_info_t), intent(in) :: info
    integer(c_int), intent(in) :: number

    call c_to_f_string_ptr(xc_func_info_get_ext_params_description(info%ptr, number), description)

  end function xc_f03_func_info_get_ext_params_description

  real(c_double) function xc_f03_func_info_get_ext_params_default_value(info, number) result(val)
    type(xc_f03_func_info_t), intent(in) :: info
    integer(c_int), intent(in) :: number

    val = xc_func_info_get_ext_params_default_value(info%ptr, number)

  end function xc_f03_func_info_get_ext_params_default_value

  !----------------------------------------------------------------
  character(len=1024) function xc_f03_func_reference_get_ref(reference) result(ref)
    type(xc_f03_func_reference_t), intent(in) :: reference

    call c_to_f_string_ptr(xc_func_reference_get_ref(reference%ptr), ref)

  end function xc_f03_func_reference_get_ref

  character(len=1024) function xc_f03_func_reference_get_doi(reference) result(doi)
    type(xc_f03_func_reference_t), intent(in) :: reference

    call c_to_f_string_ptr(xc_func_reference_get_doi(reference%ptr), doi)

  end function xc_f03_func_reference_get_doi

  character(len=1024) function xc_f03_func_reference_get_bibtex(reference) result(bibtex)
    type(xc_f03_func_reference_t), intent(in) :: reference

    call c_to_f_string_ptr(xc_func_reference_get_bibtex(reference%ptr), bibtex)

  end function xc_f03_func_reference_get_bibtex

  character(len=1024) function xc_f03_func_reference_get_key(reference) result(key)
    type(xc_f03_func_reference_t), intent(in) :: reference

    call c_to_f_string_ptr(xc_func_reference_get_key(reference%ptr), key)

  end function xc_f03_func_reference_get_key

  !----------------------------------------------------------------
  subroutine xc_f03_func_init(p, functional, nspin, err)
    type(xc_f03_func_t), intent(inout) :: p
    integer(c_int), intent(in) :: functional
    integer(c_int), intent(in) :: nspin
    integer(c_int), optional, intent(out) :: err

    integer(c_int) :: ierr

    p%ptr = xc_func_alloc()
    ierr = xc_func_init(p%ptr, functional, nspin)

    if(present(err)) err = ierr
  end subroutine xc_f03_func_init

  subroutine xc_f03_func_end(p)
    type(xc_f03_func_t), intent(inout) :: p

    call xc_func_end(p%ptr)
    call xc_func_free(p%ptr)

  end subroutine xc_f03_func_end

  type(xc_f03_func_info_t) function xc_f03_func_get_info(p) result(info)
    type(xc_f03_func_t), intent(in) :: p

    info%ptr = xc_func_get_info(p%ptr)

  end function xc_f03_func_get_info

  character(len=128) function xc_f03_functional_get_name(number) result(name)
    integer(c_int), intent(in) :: number
    type(c_ptr) :: cstr

    cstr = xc_functional_get_name(number)
    call c_to_f_string_ptr(cstr, name)
    call libxc_free(cstr)

  end function xc_f03_functional_get_name

  integer(c_int) function xc_f03_functional_get_number(func_string) result(number)
    character(len=*), intent(in) :: func_string

    number = xc_functional_get_number(f_to_c_string(func_string))

  end function xc_f03_functional_get_number

  integer(c_int) function xc_f03_family_from_id(id, family, number)
    integer(c_int), intent(in) :: id
    integer(c_int), intent(out), optional, target :: family, number

    type(c_ptr) c_family, c_number
    integer(c_int), pointer :: f_family, f_number

    if (present(family)) then
      f_family => family
      call c_f_pointer(c_family, f_family)
    else
      c_family = C_NULL_PTR
    end if
    if (present(number)) then
      f_number => number
      call c_f_pointer(c_number, f_number)
    else
      c_number = C_NULL_PTR
    end if

    xc_f03_family_from_id = xc_family_from_id(id, c_family, c_number)

  end function xc_f03_family_from_id

  subroutine xc_f03_available_functional_names(list)
    character(len=*), intent(out) :: list(*)

    integer(c_int) :: n, i, maxlen
    character(kind=c_char), allocatable, target :: names(:,:)
    type(c_ptr), allocatable :: c_list(:)

    n = xc_f03_number_of_functionals()
    maxlen = xc_f03_maximum_name_length()

    allocate(names(maxlen, n))
    allocate(c_list(n))
    do i = 1, n
      c_list(i) = c_loc(names(1,i))
    end do

    call xc_available_functional_names(c_list)

    do i = 1, n
      call c_to_f_string_ptr(c_list(i), list(i))
    end do

    deallocate(c_list)
    deallocate(names)

  end subroutine xc_f03_available_functional_names


  subroutine xc_f03_func_set_dens_threshold(p, dens_threshold)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(in) :: dens_threshold

    call xc_func_set_dens_threshold(p%ptr, dens_threshold)

  end subroutine xc_f03_func_set_dens_threshold

  subroutine xc_f03_func_set_zeta_threshold(p, zeta_threshold)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(in) :: zeta_threshold

    call xc_func_set_zeta_threshold(p%ptr, zeta_threshold)

  end subroutine xc_f03_func_set_zeta_threshold

  subroutine xc_f03_func_set_sigma_threshold(p, sigma_threshold)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(in) :: sigma_threshold

    call xc_func_set_sigma_threshold(p%ptr, sigma_threshold)

  end subroutine xc_f03_func_set_sigma_threshold

  subroutine xc_f03_func_set_tau_threshold(p, tau_threshold)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(in) :: tau_threshold

    call xc_func_set_tau_threshold(p%ptr, tau_threshold)

  end subroutine xc_f03_func_set_tau_threshold

  subroutine xc_f03_func_set_ext_params(p, ext_params)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(in) :: ext_params(*)

    call xc_func_set_ext_params(p%ptr, ext_params)

  end subroutine xc_f03_func_set_ext_params

  subroutine xc_f03_func_set_ext_params_name(p, name, par)
    type(xc_f03_func_t), intent(in) :: p
    character(len=*), intent(in) :: name
    real(c_double), intent(in) :: par

    call xc_func_set_ext_params_name(p%ptr, f_to_c_string(name), par)

  end subroutine xc_f03_func_set_ext_params_name

  ! LDAs
  !----------------------------------------------------------------
  subroutine xc_f03_lda(p, np, rho, zk, vrho, v2rho2, v3rho3, v4rho4)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*), v3rho3(*), v4rho4(*)

    call xc_lda(p%ptr, np, rho, zk, vrho, v2rho2, v3rho3, v4rho4)

  end subroutine xc_f03_lda

  subroutine xc_f03_lda_exc(p, np, rho, zk)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: zk(*)

    call xc_lda_exc(p%ptr, np, rho, zk)

  end subroutine xc_f03_lda_exc

  subroutine xc_f03_lda_exc_vxc(p, np, rho, zk, vrho)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: zk(*), vrho(*)

    call xc_lda_exc_vxc(p%ptr, np, rho, zk, vrho)

  end subroutine xc_f03_lda_exc_vxc

  subroutine xc_f03_lda_exc_vxc_fxc(p, np, rho, zk, vrho, v2rho2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*)

    call xc_lda_exc_vxc_fxc(p%ptr, np, rho, zk, vrho, v2rho2)

  end subroutine xc_f03_lda_exc_vxc_fxc

  subroutine xc_f03_lda_exc_vxc_fxc_kxc(p, np, rho, zk, vrho, v2rho2, v3rho3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: zk(*), vrho(*), v2rho2(*), v3rho3(*)

    call xc_lda_exc_vxc_fxc_kxc(p%ptr, np, rho, zk, vrho, v2rho2, v3rho3)

  end subroutine xc_f03_lda_exc_vxc_fxc_kxc

  subroutine xc_f03_lda_vxc(p, np, rho, vrho)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: vrho(*)

    call xc_lda_vxc(p%ptr, np, rho, vrho)

  end subroutine xc_f03_lda_vxc

  subroutine xc_f03_lda_vxc_fxc(p, np, rho, vrho, v2rho2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: vrho(*), v2rho2(*)

    call xc_lda_vxc_fxc(p%ptr, np, rho, vrho, v2rho2)

  end subroutine xc_f03_lda_vxc_fxc

  subroutine xc_f03_lda_vxc_fxc_kxc(p, np, rho, vrho, v2rho2, v3rho3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: vrho(*), v2rho2(*), v3rho3(*)

    call xc_lda_vxc_fxc_kxc(p%ptr, np, rho, vrho, v2rho2, v3rho3)

  end subroutine xc_f03_lda_vxc_fxc_kxc

  subroutine xc_f03_lda_fxc(p, np, rho, v2rho2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: v2rho2(*)

    call xc_lda_fxc(p%ptr, np, rho, v2rho2)

  end subroutine xc_f03_lda_fxc

  subroutine xc_f03_lda_kxc(p, np, rho, v3rho3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: v3rho3(*)

    call xc_lda_kxc(p%ptr, np, rho, v3rho3)

  end subroutine xc_f03_lda_kxc

  subroutine xc_f03_lda_lxc(p, np, rho, v4rho4)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*)
    real(c_double), intent(out) :: v4rho4(*)

    call xc_lda_lxc(p%ptr, np, rho, v4rho4)

  end subroutine xc_f03_lda_lxc

  ! GGAs
  !----------------------------------------------------------------
  subroutine xc_f03_gga(p, np, rho, sigma, zk, vrho, vsigma, &
       v2rho2, v2rhosigma, v2sigma2, &
       v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3, &
       v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4 &
    )
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)
    real(c_double), intent(out) :: v4rho4(*), v4rho3sigma(*), v4rho2sigma2(*), v4rhosigma3(*), v4sigma4(*)

    call xc_gga(p%ptr, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3, &
         v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4 &
         )

  end subroutine xc_f03_gga

  subroutine xc_f03_gga_exc(p, np, rho, sigma, zk)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: zk(*)

    call xc_gga_exc(p%ptr, np, rho, sigma, zk)

  end subroutine xc_f03_gga_exc

  subroutine xc_f03_gga_exc_vxc(p, np, rho, sigma, zk, vrho, vsigma)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)

    call xc_gga_exc_vxc(p%ptr, np, rho, sigma, zk, vrho, vsigma)

  end subroutine xc_f03_gga_exc_vxc

  subroutine xc_f03_gga_exc_vxc_fxc(p, np, rho, sigma, zk, vrho, vsigma, &
       v2rho2, v2rhosigma, v2sigma2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)

    call xc_gga_exc_vxc_fxc(p%ptr, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2)

  end subroutine xc_f03_gga_exc_vxc_fxc

  subroutine xc_f03_gga_exc_vxc_fxc_kxc(p, np, rho, sigma, zk, vrho, vsigma, &
       v2rho2, v2rhosigma, v2sigma2, &
       v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)

    call xc_gga_exc_vxc_fxc_kxc(p%ptr, np, rho, sigma, zk, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)

  end subroutine xc_f03_gga_exc_vxc_fxc_kxc

  subroutine xc_f03_gga_vxc(p, np, rho, sigma, vrho, vsigma)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*)

    call xc_gga_vxc(p%ptr, np, rho, sigma, vrho, vsigma)

  end subroutine xc_f03_gga_vxc

  subroutine xc_f03_gga_vxc_fxc(p, np, rho, sigma, vrho, vsigma, &
       v2rho2, v2rhosigma, v2sigma2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)

    call xc_gga_vxc_fxc(p%ptr, np, rho, sigma, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2)

  end subroutine xc_f03_gga_vxc_fxc

  subroutine xc_f03_gga_vxc_fxc_kxc(p, np, rho, sigma, vrho, vsigma, &
       v2rho2, v2rhosigma, v2sigma2, &
       v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)

    call xc_gga_vxc_fxc_kxc(p%ptr, np, rho, sigma, vrho, vsigma, &
         v2rho2, v2rhosigma, v2sigma2, &
         v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)

  end subroutine xc_f03_gga_vxc_fxc_kxc

  subroutine xc_f03_gga_fxc(p, np, rho, sigma, v2rho2, v2rhosigma, v2sigma2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2sigma2(*)

    call xc_gga_fxc(p%ptr, np, rho, sigma, v2rho2, v2rhosigma, v2sigma2)

  end subroutine xc_f03_gga_fxc

  subroutine xc_f03_gga_kxc(p, np, rho, sigma, v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rhosigma2(*), v3sigma3(*)

    call xc_gga_kxc(p%ptr, np, rho, sigma, v3rho3, v3rho2sigma, v3rhosigma2, v3sigma3)

  end subroutine xc_f03_gga_kxc

  subroutine xc_f03_gga_lxc(p, np, rho, sigma, v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*)
    real(c_double), intent(out) :: v4rho4(*), v4rho3sigma(*), v4rho2sigma2(*), v4rhosigma3(*), v4sigma4(*)

    call xc_gga_lxc(p%ptr, np, rho, sigma, v4rho4, v4rho3sigma, v4rho2sigma2, v4rhosigma3, v4sigma4)
  end subroutine xc_f03_gga_lxc

  real(c_double) function xc_f03_gga_ak13_get_asymptotic(homo) result(asymptotic)
    real(c_double), intent(in) :: homo

    asymptotic = xc_gga_ak13_get_asymptotic(homo)

  end function xc_f03_gga_ak13_get_asymptotic

  real(c_double) function xc_f03_hyb_exx_coef(p) result(coef)
    type(xc_f03_func_t), intent(in) :: p

    coef = xc_hyb_exx_coef(p%ptr)

  end function xc_f03_hyb_exx_coef

  subroutine xc_f03_hyb_cam_coef(p, omega, alpha, beta)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(out) :: omega, alpha, beta

    call xc_hyb_cam_coef(p%ptr, omega, alpha, beta)

  end subroutine xc_f03_hyb_cam_coef

  subroutine xc_f03_nlc_coef(p, nlc_b, nlc_c)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(out) :: nlc_b, nlc_c

    call xc_nlc_coef(p%ptr, nlc_b, nlc_c)

  end subroutine xc_f03_nlc_coef

  integer(c_int) function xc_f03_num_aux_funcs(p) result(naux)
    type(xc_f03_func_t), intent(in) :: p

    naux = xc_num_aux_funcs(p%ptr)
  end function xc_f03_num_aux_funcs

  subroutine xc_f03_aux_func_ids(p, ids)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_int), intent(out) :: ids(:)

    call xc_aux_func_ids(p%ptr, ids)
  end subroutine xc_f03_aux_func_ids

  subroutine xc_f03_aux_func_weights(p, weights)
    type(xc_f03_func_t), intent(in) :: p
    real(c_double), intent(out) :: weights(:)

    call xc_aux_func_weights(p%ptr, weights)
  end subroutine xc_f03_aux_func_weights


  ! the meta-GGAs
  !----------------------------------------------------------------
  subroutine xc_f03_mgga(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3, &
         v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
         v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
         v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
         v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
         v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
         v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
         v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
         )
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
         v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
         v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
         v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
         v3lapltau2(*), v3tau3(*)
    real(c_double), intent(out) :: &
           v4rho4(*), v4rho3sigma(*), v4rho3lapl(*), v4rho3tau(*), v4rho2sigma2(*), v4rho2sigmalapl(*), &
           v4rho2sigmatau(*), v4rho2lapl2(*), v4rho2lapltau(*), v4rho2tau2(*), v4rhosigma3(*), &
           v4rhosigma2lapl(*), v4rhosigma2tau(*), v4rhosigmalapl2(*), v4rhosigmalapltau(*), &
           v4rhosigmatau2(*), v4rholapl3(*), v4rholapl2tau(*), v4rholapltau2(*), v4rhotau3(*), v4sigma4(*), &
           v4sigma3lapl(*), v4sigma3tau(*), v4sigma2lapl2(*), v4sigma2lapltau(*), v4sigma2tau2(*), &
           v4sigmalapl3(*), v4sigmalapl2tau(*), v4sigmalapltau2(*), v4sigmatau3(*), v4lapl4(*), &
           v4lapl3tau(*), v4lapl2tau2(*), v4lapltau3(*), v4tau4(*)

    call xc_mgga(p%ptr, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3, &
         v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
         v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
         v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
         v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
         v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
         v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
         v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
         )
  end subroutine xc_f03_mgga

  subroutine xc_f03_mgga_exc(p, np, rho, sigma, lapl, tau, zk)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: zk(*)

    call xc_mgga_exc(p%ptr, np, rho, sigma, lapl, tau, zk)

  end subroutine xc_f03_mgga_exc

  subroutine xc_f03_mgga_exc_vxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)

    call xc_mgga_exc_vxc(p%ptr, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau)

  end subroutine xc_f03_mgga_exc_vxc

  subroutine xc_f03_mgga_exc_vxc_fxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)

    call xc_mgga_exc_vxc_fxc(p%ptr, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2)
  end subroutine xc_f03_mgga_exc_vxc_fxc

  subroutine xc_f03_mgga_exc_vxc_fxc_kxc(p, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: zk(*), vrho(*), vsigma(*), vlapl(*), vtau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
         v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
         v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
         v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
         v3lapltau2(*), v3tau3(*)

    call xc_mgga_exc_vxc_fxc_kxc(p%ptr, np, rho, sigma, lapl, tau, zk, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3)
  end subroutine xc_f03_mgga_exc_vxc_fxc_kxc

  subroutine xc_f03_mgga_vxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)

    call xc_mgga_vxc(p%ptr, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau)

  end subroutine xc_f03_mgga_vxc

  subroutine xc_f03_mgga_vxc_fxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)

    call xc_mgga_vxc_fxc(p%ptr, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2)
  end subroutine xc_f03_mgga_vxc_fxc

  subroutine xc_f03_mgga_vxc_fxc_kxc(p, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: vrho(*), vsigma(*), vlapl(*), vtau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
         v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
         v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
         v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
         v3lapltau2(*), v3tau3(*)

    call xc_mgga_vxc_fxc_kxc(p%ptr, np, rho, sigma, lapl, tau, vrho, vsigma, vlapl, vtau, &
         v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
         v2lapl2, v2lapltau, v2tau2, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3)
  end subroutine xc_f03_mgga_vxc_fxc_kxc

  subroutine xc_f03_mgga_fxc(p, np, rho, sigma, lapl, tau, &
       v2rho2, v2rhosigma, v2rholapl, v2rhotau, v2sigma2, v2sigmalapl, v2sigmatau, &
       v2lapl2, v2lapltau, v2tau2)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: v2rho2(*), v2rhosigma(*), v2rholapl(*), v2rhotau(*), &
         v2sigma2(*), v2sigmalapl(*), v2sigmatau(*), v2lapl2(*), v2lapltau(*), v2tau2(*)

    call xc_mgga_fxc(p%ptr, np, rho, sigma, lapl, tau, &
      v2rho2, v2rhosigma, v2rholapl, v2rhotau, &
      v2sigma2, v2sigmalapl, v2sigmatau, v2lapl2, v2lapltau, v2tau2)

  end subroutine xc_f03_mgga_fxc

  subroutine xc_f03_mgga_kxc(p, np, rho, sigma, lapl, tau, &
       v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
       v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
       v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
       v3lapltau2, v3tau3)
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: v3rho3(*), v3rho2sigma(*), v3rho2lapl(*), v3rho2tau(*), &
           v3rhosigma2(*), v3rhosigmalapl(*), v3rhosigmatau(*), v3rholapl2(*), &
           v3rholapltau(*), v3rhotau2(*), v3sigma3(*), v3sigma2lapl(*), v3sigma2tau(*), &
           v3sigmalapl2(*), v3sigmalapltau(*), v3sigmatau2(*), v3lapl3(*), v3lapl2tau(*), &
           v3lapltau2(*), v3tau3(*)

    call xc_mgga_kxc(p%ptr, np, rho, sigma, lapl, tau, &
         v3rho3, v3rho2sigma, v3rho2lapl, v3rho2tau, v3rhosigma2, v3rhosigmalapl, &
         v3rhosigmatau, v3rholapl2, v3rholapltau, v3rhotau2, v3sigma3, v3sigma2lapl, &
         v3sigma2tau, v3sigmalapl2, v3sigmalapltau, v3sigmatau2, v3lapl3, v3lapl2tau, &
         v3lapltau2, v3tau3)
  end subroutine xc_f03_mgga_kxc

  subroutine xc_f03_mgga_lxc(p, np, rho, sigma, lapl, tau, &
       v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
       v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
       v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
       v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
       v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
       v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
       v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
       )
    type(xc_f03_func_t), intent(in) :: p
    integer(c_size_t), intent(in) :: np
    real(c_double), intent(in) :: rho(*), sigma(*), lapl(*), tau(*)
    real(c_double), intent(out) :: &
         v4rho4(*), v4rho3sigma(*), v4rho3lapl(*), v4rho3tau(*), v4rho2sigma2(*), v4rho2sigmalapl(*), &
         v4rho2sigmatau(*), v4rho2lapl2(*), v4rho2lapltau(*), v4rho2tau2(*), v4rhosigma3(*), &
         v4rhosigma2lapl(*), v4rhosigma2tau(*), v4rhosigmalapl2(*), v4rhosigmalapltau(*), &
         v4rhosigmatau2(*), v4rholapl3(*), v4rholapl2tau(*), v4rholapltau2(*), v4rhotau3(*), v4sigma4(*), &
         v4sigma3lapl(*), v4sigma3tau(*), v4sigma2lapl2(*), v4sigma2lapltau(*), v4sigma2tau2(*), &
         v4sigmalapl3(*), v4sigmalapl2tau(*), v4sigmalapltau2(*), v4sigmatau3(*), v4lapl4(*), &
         v4lapl3tau(*), v4lapl2tau2(*), v4lapltau3(*), v4tau4(*)

    call xc_mgga_lxc(p%ptr, np, rho, sigma, lapl, tau, &
         v4rho4, v4rho3sigma, v4rho3lapl, v4rho3tau, v4rho2sigma2, v4rho2sigmalapl, &
         v4rho2sigmatau, v4rho2lapl2, v4rho2lapltau, v4rho2tau2, v4rhosigma3, &
         v4rhosigma2lapl, v4rhosigma2tau, v4rhosigmalapl2, v4rhosigmalapltau, &
         v4rhosigmatau2, v4rholapl3, v4rholapl2tau, v4rholapltau2, v4rhotau3, v4sigma4, &
         v4sigma3lapl, v4sigma3tau, v4sigma2lapl2, v4sigma2lapltau, v4sigma2tau2, &
         v4sigmalapl3, v4sigmalapl2tau, v4sigmalapltau2, v4sigmatau3, v4lapl4, &
         v4lapl3tau, v4lapl2tau2, v4lapltau3, v4tau4 &
         )
  end subroutine xc_f03_mgga_lxc


  ! Helper functions to convert between C and Fortran strings
  ! Based on the routines by Joseph M. Krahn
  function f_to_c_string(f_string) result(c_string)
    character(len=*), intent(in) :: f_string
    character(kind=c_char,len=1) :: c_string(len_trim(f_string)+1)

    integer :: i, strlen

    strlen = len_trim(f_string)

    forall (i=1:strlen)
      c_string(i) = f_string(i:i)
    end forall
    c_string(strlen+1) = C_NULL_CHAR

  end function f_to_c_string

  subroutine c_to_f_string(c_string, f_string)
    character(kind=c_char,len=1), intent(in) :: c_string(*)
    character(len=*), intent(out) :: f_string

    integer :: i

    i = 1
    do while(c_string(i) /= C_NULL_CHAR .and. i <= len(f_string))
      f_string(i:i) = c_string(i)
      i = i + 1
    end do
    if (i < len(f_string)) f_string(i:) = ' '

  end subroutine c_to_f_string

  subroutine c_to_f_string_ptr(c_string, f_string)
    type(c_ptr), intent(in) :: c_string
    character(len=*), intent(out) :: f_string

    character(len=1, kind=c_char), pointer :: p_chars(:)
    integer :: i

    if (.not. c_associated(c_string)) then
      f_string = ' '
    else
      call c_f_pointer(c_string, p_chars, [huge(0)])
      i = 1
      do while(p_chars(i) /= C_NULL_CHAR .and. i <= len(f_string))
        f_string(i:i) = p_chars(i)
        i = i + 1
      end do
      if (i < len(f_string)) f_string(i:) = ' '
    end if

  end subroutine c_to_f_string_ptr

end module xc_f03_lib_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
