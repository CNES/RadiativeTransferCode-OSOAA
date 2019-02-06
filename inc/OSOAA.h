C******************************************************************************
C* FILE: OSOAA.h
C* PROJECT: Ocean Successive Orders with Atmosphere - Advanced (OSOAA)
C* RULE: Defines the constant parameters of the OSOAA computing programs.
C*
C* AUTHOR: B.Lafrance ( CS )
C* DATE: 02/04/2015
C*
C* MOD:VERSION:1.0
C* MOD:VERSION:1.1: 30/05/2018:  
C*     - New constants are introduced for tabulated data of sea bottom 
C*       spectral reflectance : CTE_FIC_BOT_SPECTRAL_DATA and CTE_NB_WA_FIC_BOT 
C* MOD:VERSION:1.2: 11/01/2019: 
C*     - Change the CTE_OS_NS_MAX value
C*     - Change the CTE_NBANGLES_MAX value and recommendation
C******************************************************************************

C#######################################################################
C    1. Common constants                        
C#######################################################################

C Length of character chains
C-----------------------------------
C Length of directory name
#define CTE_LENDIR 350 

C Length of filename (without the directory tree)
#define CTE_LENFIC1 150

C Length of complete filename (with the complete directory tree)
#define CTE_LENFIC2 450

C Maximum size of the Keywords for the user's parameters definition
#define CTE_LENKEYWORD 30

C Length of command system chains
#define CTE_LENCOM 400



C ID for values not defined by the user
C--------------------------------------
C --> Integer type :
#define CTE_NOT_DEFINED_VALUE_INT -999
C --> Double type :
#define CTE_NOT_DEFINED_VALUE_DBLE -999.D+00





C#######################################################################
C    2. Specific constants related to the computation of particles
C       radiative properties                       
C#######################################################################


C Default filename for the aerosols radiative properties definition
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICGRANU_AER "PM_AER.txt"

C Default filename for the phytoplancton radiative properties 
C definition
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICGRANU_PHYTO "PM_PHYTO.txt"

C Default filename for the Mineral-Like Particles radiative 
C properties definition
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICGRANU_MLP "PM_MLP.txt"

C Size of arrays --> Specific constant to the file OSOAA_MIE.F
C----------------------------------------------------------------
#define CTE_MIE_DIM 10000


C Maximal size of user phaze function arrays
C    --> Specific constant to the file OSOAA_AEROSOLS.F
C----------------------------------------------------------------
#define CTE_MAXNB_ANG_EXT 200

C Default value for the maximum radius of aerosols particles
C for a Junge model of size distribution
C    --> Specific constant to the file OSOAA_MAIN.F
C----------------------------------------------------------------
#define CTE_DEFAULT_AER_JUNGE_RMAX 50.

C Default value for the minimum radius of hydrosols particles
C for a Junge model of size distribution
C    --> Specific constant to the file OSOAA_MAIN.F
C----------------------------------------------------------------
#define CTE_DEFAULT_HYD_JUNGE_RMIN 0.01

C Default value for the maximum radius of hydrosols particles
C for a Junge model of size distribution
C    --> Specific constant to the file OSOAA_MAIN.F
C----------------------------------------------------------------
#define CTE_DEFAULT_HYD_JUNGE_RMAX 200.

C Corrective value for the slope of Junge model in the case of 
C the user has defined the singular value slope = 3
C    --> Specific constant to the files OSOAA_AEROSOLS.F
C        and OSOAA_HYDROSOLS.F
C----------------------------------------------------------------
#define CTE_JUNGE_SLOPE_COR 0.05

C Limit value of the size parameter to calculate the MIE 
C files for WMO and Shettle & Fenn model particles
C    --> Specific constant to the file OSOAA_AEROSOLS.F
C----------------------------------------------------------------
#define CTE_ALPHAMAX_WMO_DL   4000.
#define CTE_ALPHAMAX_WMO_WS   50.
#define CTE_ALPHAMAX_WMO_OC   800.
#define CTE_ALPHAMAX_WMO_SO   10.

#define CTE_ALPHAMAX_SF_SR    70.
#define CTE_ALPHAMAX_SF_SU    90.

C Minimal value of the size distribution ratio n(r) / Nmax
C used to estimate the limit value of the size parameter
C required for Mie calculations.
C    --> Specific constant to the files OSOAA_AEROSOLS.F
C        and OSOAA_HYDROSOLS.F
C----------------------------------------------------------------
#define CTE_COEF_NRMAX 0.002

C File including information on WMO particle models:
C modal radius, log of standard deviation, volumic concentration 
C and refractive index values (real and imaginary parts) 
C as a fonction of the wavelength.
C    --> Specific constant to the file SOS_AEROSOLS.F
C---------------------------------------------------------------
#define CTE_AER_DATAWMO		"Data_WMO_cor_2015_12_16"

C File including information on Shettle & Fenn particle size
C distributions: log of standard deviation and modal radius values 
C as a fonction of the relative humidity.
C    --> Specific constant to the file SOS_AEROSOLS.F
C---------------------------------------------------------------
#define CTE_AER_DATASF		"Data_SF_cor_2015_12_16"

C Files including information on the refractive index 
C of Shettle & Fenn particles (real and imaginary parts)
C as a fonction of the wavelength and relative humidity.
C    --> Specific constant to the file SOS_AEROSOLS.F
C---------------------------------------------------------------
#define CTE_AER_SR_SF		"IRefrac_SR_cor_2015_12_16"
#define CTE_AER_LR_SF		"IRefrac_LR"
#define CTE_AER_SU_SF		"IRefrac_SU_cor_2015_12_16"
#define CTE_AER_LU_SF		"IRefrac_LU_cor_2015_12_16"
#define CTE_AER_OM_SF		"IRefrac_OM_cor_2015_12_16"

C Angles used to define the angular range for the phaze function 
C linearization while applying a troncation.
C    --> Specific constant to the files SOS_AEROSOLS.F
C        and OSOAA_HYDROSOLS.F
C---------------------------------------------------------------
#define CTE_AER_MU1_TRONCA 0.8
#define CTE_AER_MU2_TRONCA 0.94

#define CTE_HYD_MU1_TRONCA 0.85
#define CTE_HYD_MU2_TRONCA 0.92

C Troncation threshold
C    --> Specific constant to the file OSOAA_PARTICLES_RAD.F
C---------------------------------------------------------------
#define CTE_PH_SEUIL_TRONCA 0.1




C#######################################################################
C    3. Specific constants related to the computation of surface
C       interface matrixes
C#######################################################################
 
C Threshold on maximal value of wave probability GMAX
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_GMAX 1.D-40

C Dichotomy threshold on incidence angle calculation 
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_DICHO 1.D-10

C Threshold on minimal value for cosine of the zenith 
C angle of the waves normal vector
C estimated to an air -> sea coupled directions (incidence, transmission)  
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_COSTHETAN 0.001

C Threshold for the estimation of possible geometric configurations
C in case of transmission   
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_GEO_CONFIG 1.D-15

C Factor used to compare GMIN and GMAX
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_PH_TEST 10000

C Number of azimutale angles (2**CTE_PH_NQ)
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_PH_NU 1024

C Value of the exponent CTE_PH_NQ giving CTE_PH_NU = 2 exponent CTE_PH_NQ
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_PH_NQ 10


C Value of the threshold for maximal order estimate of the 
C Fourier expansion of the wave probability function
C    --> Specific constant to the file OSOAA_SURF_MATRICES.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_G_SMAX 0.0001



C#######################################################################
C    4. Specific constants related to the radiative transfer 
C       equation calculation.
C#######################################################################

C    Default filename for the binary result file of radiance  
C    in Fourier series expansion
C       --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICSOS_RES_BIN "LUM_SF.bin"

C    Default filename for the radiance result file  
C    as a fonction of the zenith angle
C       --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICSOS_RES_VS_VZA "LUM_vsVZA.txt"

C Default value of the maximal number of interaction (scattering, reflexion)
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_IGMAX 100

C Minimal wavelength for radiance calculation
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_WAMIN 0.299

C Factor of molecular depolarization in the air
C    --> Specific constant to the file OSOAA_SOS_CORE.F
C--------------------------------------------------------------
#define CTE_MDF_AIR 0.0279

C Factor of molecular depolarization in the sea water
C    --> Specific constant to the file OSOAA_SOS_CORE.F
C--------------------------------------------------------------
#define CTE_MDF_SEA 0.0906

C Threshold to test the convergence of geometric series
C    --> Specific constant to the file OSOAA_SOS_CORE.F
C------------------------------------------------------------
#define CTE_PH_SEUIL_CV_SG  0.005

C Threshold to test the stop of cumulative scatterings
C    --> Specific constant to the file OSOAA_SOS_CORE.F
C------------------------------------------------------------
#define CTE_PH_SEUIL_SUMDIF  0.001

C Threshold to test the stop of Fourier series expansion
C    --> Specific constant to the file OSOAA_SOS_CORE.F
C------------------------------------------------------------
#define CTE_PH_SEUIL_SF  0.0002


C Thresholds to calculate the rotation angles
C    --> Specific constant to the file OSOAA_TRPHI.F
C--------------------------------------------------------------
#define CTE_SEUIL_Z 0.0001
#define CTE_SEUIL_EPSILON 0.00001

C Threshold value under which Q or U is fixed to be null
C    --> Specific constant to the file OSOAA_TRPHI.F
C--------------------------------------------------------------
#define CTE_THRESHOLD_Q_U_NULL 1.D-99

C Value of the solar disc solid angle (sr) for the 
C mean Earth-Sun distance
C    --> Specific constant to the file OSOAA_TRPHI.F
C--------------------------------------------------------------
#define CTE_SOLAR_DISC_SOLID_ANGLE 6.8D-05

C    Filename defining the sea bottom spectral reflectance
C    for tabulated data and number of spectral data
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_FIC_BOT_SPECTRAL_DATA "OSOAA_SEA_BOTTOM_REFLECTANCES.txt"
#define CTE_NB_WA_FIC_BOT 91


C#######################################################################
C    5. Specific constants related to the definition of angles and orders
C       of Legendre series expansion
C#######################################################################

C =========================================================================
C 5.1 Constants related to the size of the tables of angles and limit order
C     of Fourier series expansion.
C =========================================================================

C    Maximum number of angles (positive value) used to define the size  
C    of phaze functions tables.
C    Advices : * Have to be at least the default value CTE_DEFAULT_NBMU_MIE
C                + the maximum number of user's angles CTE_NBMAX_USER_ANGLES 
C              * Have to be adjust if the common use requires more Gauss' angles
C                than the default value.
C              * But, do not define a too high value in order to avoid reducing 
C                the calculation velocity. 
C    --> Specific constant to the files OSOAA_AEROSOLS.F, OSOAA_ANGLES.F,
C        OSOAA_HYDROSOLS.F, OSOAA_MIE.F and OSOAA_PARTICLES_RAD.F
C---------------------------------------------------------------------------
#define CTE_MIE_NBMU_MAX 510		

C    Maximum number of angles (positive value) used to define the size  
C    of radiance and interface matrix tables.
C    Advices : * Have to be at least the default value CTE_DEFAULT_NBMU_LUM
C                + the maximum number of user's angles CTE_NBMAX_USER_ANGLES 
C              * Have to be adjust if the common use requires more Gauss' angles
C                than the default value.
C              * But, do not define a too high value in order to avoid reducing 
C                the calculation velocity. 
C     --> Specific constant to the files OSOAA_ANGLES.F, OSOAA_SOS.F,
C         OSOAA_SOS_CORE.F, OSOAA_SURFACE.F, OSOAA_SURF_MATRICES.F
C         and OSOAA_TRPHI.F
C---------------------------------------------------------------------------
#define CTE_OS_NBMU_MAX 510  

C    Maximum order of Fourier series expansion
C    and limit order for the Legendre polynomial expansion
C    to define the size of tables used by the code
C    Advice : let's define CTE_OS_NB_MAX >= 2*CTE_MIE_NBMU_MAX
C    --> Specific constant to the files OSOAA_AEROSOLS.F, OSOAA_ANGLES.F,
C        OSOAA_HYDROSOLS.F, OSOAA_PARTICLES_RAD.F, OSOAA_SOS.F, OSOAA_SOS_CORE.F,
C        and OSOAA_SURF_MATRICES.F
C---------------------------------------------------------------------------
#define CTE_OS_NB_MAX 1024

C    Maximum order of Legendre polynomial expansion
C    to define the size of tables used to compute the Fresnel matrix elements 
C    Advice : let's define CTE_OS_NS_MAX = 2*CTE_OS_NBMU_MAX
C    --> Specific constant to the files OSOAA_ANGLES.F and OSOAA_SURF_MATRICES.F    
C---------------------------------------------------------------------------
#define CTE_OS_NS_MAX 1020

C    Maximum order of Fourier series expansion to define the size of 
C    tables used to compute the G function (statistic of wave slopes)
C    Advice : let's define CTE_OS_NM_MAX = CTE_OS_NB_MAX + CTE_OS_NS_MAX
C    --> Specific constant to the files OSOAA_ANGLES.F and OSOAA_SURF_MATRICES.F    
C---------------------------------------------------------------------------
#define CTE_OS_NM_MAX 2048

C =========================================================================
C 5.2 Default number of angles  and maximum orders of series expansions
C     to be used in the case of none definition of angles by the user.
C =========================================================================

C    Default number of Gauss' angles (positive values) to be used for the 
C    mie phaze function calculations
C    --> specific constant to the file OSOAA_ANGLES.F
C--------------------------------------------------------------
#define CTE_DEFAULT_NBMU_MIE 40

C    Default number of Gauss' angles (positive values) to be used for the 
C    radiance calculations
C    --> specific constant to the file OSOAA_ANGLES.F
C--------------------------------------------------------------
#define CTE_DEFAULT_NBMU_LUM 48

C    Default value for the limit order of Legendre polynomial and 
C    Fourier series expansion to compute radiance :
C    Usually : CTE_DEFAULT_OS_NB = 2*CTE_DEFAULT_NBMU_MIE
C    --> Specific constant to the file OSOAA_ANGLES.F
C---------------------------------------------------------------------------
#define CTE_DEFAULT_OS_NB 80

C    Default value for the limit order of Legendre polynomial expansion 
C    to compute the Fresnel matrix elements 
C    Usually : CTE_DEFAULT_OS_NS = 2*CTE_DEFAULT_NBMU_LUM
C    --> Specific constant to the file OSOAA_ANGLES.F
C---------------------------------------------------------------------------
#define CTE_DEFAULT_OS_NS 96

C    Default value for the limit order of Fourier series expansion  
C    of the G function (statistic of wave slopes).
C    It is necessary than 
C       CTE_DEFAULT_OS_NM >= CTE_DEFAULT_OS_NB + CTE_DEFAULT_OS_NS
C    --> Specific constant to the file OSOAA_ANGLES.F
C---------------------------------------------------------------------------
#define CTE_DEFAULT_OS_NM 176


C ===================================================
C 5.3 Limitation of the number of angles in order to
C     define the angles set to be used for simulations
C ===================================================

C    Maximum number of user's angles to be added to the Gauss angles
C    --> Specific constant to the file OSOAA_ANGLES.F
C--------------------------------------------------------------
#define CTE_NBMAX_USER_ANGLES 100

C    Maximum number of positive angles for the calculation of angles 
C    to be used by the routines (Gauss angles + user's angles 
C    + 1 = solar zenith angle )
C    Advice : it should be the max between CTE_MIE_NBMU_MAX 
C             and (CTE_OS_NBMU_MAX+1)
C    --> Specific constant to the file OSOAA_ANGLES.F
C--------------------------------------------------------------
#define CTE_NBANGLES_MAX 511

C    Minimum absolute difference between cos(X) and cos(TETA)  
C    to assign TETA = X 
C    --> Specific constant to the file OSOAA_ANGLES.F
C--------------------------------------------------------------
#define CTE_SEUIL_ECART_MU 0.00001


C ===================================================
C 5.4 Default name of result angles files 
C ===================================================

C    Default filename for the definition of angles and orders
C    of Legendre series expansion applied to the radiance  
C    calculations
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICANGLES_RES_LUM "RAD_UsedAngles.txt"

C    Default filename for the definition of angles and orders
C    of Legendre series expansion applied to the Mie calculations
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICANGLES_RES_MIE "MIE_UsedAngles.txt"




C#######################################################################
C    6. Specific constants related to the calculation of atmospheric
C       and maritime profiles
C#######################################################################

C    Default filename for the definition of the atmospheric profile
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICPROFIL_ATM_RES "PROFILE_ATM.txt"

C    Default filename for the definition of the sea profile
C    --> Specific constant to the file OSOAA_MAIN.F
C--------------------------------------------------------------
#define CTE_DEFAULT_FICPROFIL_SEA_RES "PROFILE_SEA.txt"

C    Standard Pressure (mb) 
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_STD_PRESSURE 1013

C    Altitude of the Top of Atmosphere (km) 
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_ALT_TOA 300.

C    Number of atmospheric layers  
C    --> Specific constant to the files OSOAA_PROFILE.F,
C        OSOAA_SOS.F, OSOAA_SOS_CORE.F and OSOAA_TRPHI.F
C--------------------------------------------------------------
#define CTE_NT_ATM 26

C    Number of sea layers 
C    --> Specific constant to the files OSOAA_PROFILE.F,
C        OSOAA_SOS.F, OSOAA_SOS_CORE.F and OSOAA_TRPHI.F
C--------------------------------------------------------------
#define CTE_NT_SEA 80

C    Optical thickness of the transition layer atmosphere / sea   
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_TRANS_OPT_THICKNESS 0.0001

C    Maximum number of layers used to profile computations  
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_NZ_MAX 25000

C    Sampling step of the oceanic profile  (m) 
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_SEA_DEPTH_STEP 0.05

C    Filename of euphotic depth depending on chlorophylle cocentration
C    (Morel table)
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_FIC_EUPH_DEPTH "OSOAA_SEA_EUPH_DEPTH.txt"

C    Filename of the sea molecules absorption and scattering coefficients 
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_FIC_MOL_SPECTRAL_DATA "OSOAA_SEA_MOL_COEFFS_JUNE_2013.txt"

C    Filename of the AP and EP coefficients to be used for the 
C    calculation of phytoplankton absorption
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_FIC_PHYTO_SPECTRAL_DATA "OSOAA_SEA_PHYT_COEFFS.txt"

C    Maximum number of spectral data in files defined by
C    CTE_FIC_MOL_SPECTRAL_DATA and CTE_FIC_PHYTO_SPECTRAL_DATA
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_NBWA_MAX 2500

C    Default value of the coefficient for the spectral variation 
C    of yellow substance absorption (m-1)
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_DEFAULT_SPECTRAL_YS 0.014

C    Default value of the coefficient for the spectral variation 
C    of detritus absorption (m-1)
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_DEFAULT_SPECTRAL_DET 0.011

C    Maximal value of the sea column optical thickness
C    --> Specific constant to the file OSOAA_PROFILE.F
C--------------------------------------------------------------
#define CTE_SEA_T_LIMIT 30.
