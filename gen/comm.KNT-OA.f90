!     =======================================================================
!          THIS MODULE CONTAINS THE GLOBAL PARAMETERS FOR MOSAIC-2DVBS
!     =======================================================================
!     NOTE: FILE GENERATED AUTOMATICALLY. CHANGES WILL BE LOST.

      MODULE comm_KNT_OA
      
!     LINK EXTERNAL MODULES:
!     =======================================================================
!     NUMBER OF MOISTURE SPECIES AND 
!     CHEMICAL TRACER SPECIES:
      USE MODULE_STATE_DESCRIPTION, ONLY: nMOIST => num_moist
      USE MODULE_STATE_DESCRIPTION, ONLY: nCHEM => num_chem
      
!     POINTERS TO ALL CHEMICAL SPECIES:
      USE MODULE_CONFIGURE

!     AEROSOL CONFIGURATIONS:
      USE mod_AERO_CONFIG, ONLY: nBINSxx => nBINS
      USE mod_AERO_CONFIG, ONLY: bCENTER

!     DEFINE 2D-VBS GRID:
!     =======================================================================
!     NUMBER OF ORGANIC SPECIES:
      INTEGER,PARAMETER :: nCOMP = 87

!     NUMBER OF PARTICLE SIZE BINS:
      INTEGER,PARAMETER :: nBINS = nBINSxx
      
!     GRID POINTS IN SATURATION
!     VAPOR CONCENTRATION [ug m-3]:
      REAL(8),PARAMETER, DIMENSION(nCOMP) :: CSAT = (/ & 
      1.0e-03,   1.0e+00,   1.0e+03,   1.0e-02,   1.0e-02, &
      1.0e-02,   1.0e+00,   1.0e+00,   1.0e+00,   1.0e+01, &
      1.0e+01,   1.0e+01,   1.0e+03,   1.0e+03,   1.0e+03, &
      1.0e-02,   1.0e-02,   1.0e-02,   1.0e+00,   1.0e+00, &
      1.0e+00,   1.0e+01,   1.0e+01,   1.0e+01,   1.0e+03, &
      1.0e+03,   1.0e+03,   1.0e-02,   1.0e-02,   1.0e-02, &
      1.0e+00,   1.0e+00,   1.0e+00,   1.0e+01,   1.0e+01, &
      1.0e+01,   1.0e+03,   1.0e+03,   1.0e+03,   1.0e-02, &
      1.0e-02,   1.0e-02,   1.0e+00,   1.0e+00,   1.0e+00, &
      1.0e+01,   1.0e+01,   1.0e+01,   1.0e+03,   1.0e+03, &
      1.0e+03,   1.0e-02,   1.0e-02,   1.0e-02,   1.0e+00, &
      1.0e+00,   1.0e+00,   1.0e+01,   1.0e+01,   1.0e+01, &
      1.0e+03,   1.0e+03,   1.0e+03,   1.0e-02,   1.0e-02, &
      1.0e-02,   1.0e+00,   1.0e+00,   1.0e+00,   1.0e+01, &
      1.0e+01,   1.0e+01,   1.0e+03,   1.0e+03,   1.0e+03, &
      1.0e-02,   1.0e-02,   1.0e-02,   1.0e+00, &
      1.0e+00,   1.0e+00,   1.0e+01,   1.0e+01, &
      1.0e+01,   1.0e+03,   1.0e+03,   1.0e+03 /)

!     MOLCULAR WEIGHT OF SPECIES [g mol-1]:
      REAL(8),PARAMETER, DIMENSION(nCOMP) :: MW_ARRAY = (/ & 
      202.30,    163.14,    123.99,    231.66,    189.24, &
      135.74,    199.71,    163.14,    117.02,    183.73, &
      150.09,    107.66,    151.78,    123.99,     88.94, &
      231.66,    189.24,    135.74,    199.71,    163.14, &
      117.02,    183.73,    150.09,    107.66,    151.78, &
      123.99,     88.94,    231.66,    189.24,    135.74, &
      199.71,    163.14,    117.02,    183.73,    150.09, &
      107.66,    151.78,    123.99,     88.94,    231.66, &
      189.24,    135.74,    199.71,    163.14,    117.02, &
      183.73,    150.09,    107.66,    151.78,    123.99, &
       88.94,    231.66,    200.00,    135.74,    199.71, &
      172.41,    117.02,    183.73,    158.62,    107.66, &
      151.78,    131.03,     88.94,    231.66,    189.24, &
      135.74,    199.71,    163.14,    117.02,    183.73, &
      150.09,    107.66,    151.78,    123.99,     88.94, &
      348.00,    200.00,    162.40,    300.00, &
      172.41,    140.00,    276.00,    158.62, &
      128.80,    228.00,    131.03,    106.40 /)

!     SIZE BIN DIAMETERS [nm]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: DIAM = bCENTER
            
!     DECLARATIONS:
!     NOTE: FOR SOME PHYSICAL CONSTANTS.
!     =======================================================================      
!     PI VALUE:
      REAL(8),PARAMETER :: PI = 3.1415926

!     AVOGADRO'S NUMBER:
      REAL(8),PARAMETER :: nAVO = 6.022e23

!     DECLARATIONS:
!     NOTE: FOR POINTER ARRAYS.
!     =======================================================================
!     POINTERS:
!     NOTE: FOR GAS-PHASE ORGANIC SPECIES.
      INTEGER,SAVE :: PTRARRAY_GAS(nCOMP)

!     POINTERS:
!     NOTE: FOR AERO. AND OLIG. ORGANIC SPECIES.
      INTEGER,SAVE :: PTRARRAY_AERO(nCOMP,nBINS)
      INTEGER,SAVE :: PTRARRAY_OLIG(nCOMP,nBINS)

!     POINTERS:
!     NOTE: FOR PARTICLE NUM. SPECIES.
      INTEGER,SAVE :: PTRARRAY_NUM(nBINS)

      END MODULE comm_KNT_OA
