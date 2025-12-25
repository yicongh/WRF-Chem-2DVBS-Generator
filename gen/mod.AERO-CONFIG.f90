!     ==================================================================
!                       MODULE FOR AEROSOL CONFIGURATIONS
!     ==================================================================
!     NOTE: THIS SCRIPT IS AUTOMATICALLY GENERATED. CHANGES WILL BE LOST.

      MODULE mod_AERO_CONFIG

!     USE MODULES:
!     ==================================================================
      USE MODULE_CONFIGURE

!     USE: TYPE FOR FORTRAN DATAFRAME
      USE mod_FORTRANDF, ONLY: data_frame

!     DEFINE: DATAFRAMES
!     ==================================================================
!     DATAFRAME: AEROSOL SPECIES
      TYPE(data_frame) :: df_AERO

!     DATAFRAME: AEROSOL SIZE BINS
      TYPE(data_frame) :: df_BINS

!     DATAFRAME: KINETICALLY PARTITIONING ORGANICS
      TYPE(data_frame) :: df_KNTOA

!     DATAFRAME: EMISSION FROM ANTHROPOGENIC SOURCES
      TYPE(data_frame) :: df_EMISSaa

!     DATAFRAME: EMISSION FROM BIOMASS-BURNING SOURCES
      TYPE(data_frame) :: df_EMISSbb

!     DEFINE: DIMENSIONS
!     ==================================================================
!     NUMBER OF PARTICLE-PHASE SPECIES:
      INTEGER,PARAMETER :: nCOMP = 102

!     NUMBER OF SIZE BINS:
      INTEGER,PARAMETER :: nBINS = 4

!     NUMBER OF KINETIC ORGANIC SPECIES:
      INTEGER,PARAMETER :: nKNTOA = 87

!     NUMBER OF EMISSION TARGET-SOURCE PAIRS FOR 
!     BIOMASS BURNING AND ANTHROPOGENIC SOURCES:
      INTEGER,PARAMETER :: nEMISSbb = 6
      INTEGER,PARAMETER :: nEMISSaa = 8

!     NUMBER OF EMISSION MASS MODES:
      INTEGER,PARAMETER :: nEMISSMODE = 7

!     DEFINE: SIZE BINS
!     ==================================================================
!     LOWEST AND HIGHEST SIZE BOUND [nm]:
      REAL(8),PARAMETER :: LOWERx = 30.0
      REAL(8),PARAMETER :: UPPERx = 10000.0

!     BIN RATIOS FOR 1-ST AND 3-RD MOMENTS:
      REAL(8),PARAMETER :: &
      bRATIO1 = 10.0**(1.0*LOG10(UPPERx/LOWERx)/DBLE(nBINS))

      REAL(8),PARAMETER :: &
      bRATIO3 = 10.0**(3.0*LOG10(UPPERx/LOWERx)/DBLE(nBINS))
      
!     BIN BOUNDS [nm]:
      REAL(8),PARAMETER,DIMENSION(nBINS+1) :: &
      BOUNDS = (/(LOWERx*(bRATIO1)**(i-1), i=1,nBINS+1)/)

!     BIN UPPER/LOWER BOUNDS AND CENTER IN DIAMETER [nm]:
      REAL(8),PARAMETER,DIMENSION(nBINS) :: bUPPER = BOUNDS(2:nBINS+1)
      REAL(8),PARAMETER,DIMENSION(nBINS) :: bLOWER = BOUNDS(1:nBINS)
      REAL(8),PARAMETER,DIMENSION(nBINS) :: bCENTR = SQRT(bUPPER*bLOWER)

!     BIN UPPER/LOWER BOUNDS AND CENTER IN VOLUME [um3]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bCENTRv = (3.14159/6.d0)*((bCENTR*1e-3)**3.d0)

      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bLOWERv = (3.14159/6.d0)*((bLOWER*1e-3)**3.d0)

      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bUPPERv = (3.14159/6.d0)*((bUPPER*1e-3)**3.d0)


!     - MULTIPLICATION FACTOR:
!!      REAL(8),PARAMETER :: &
!!      ff = 10.0**(LOG10(UPPERx/LOWERx)/DBLE(nBINS))

!!!!!!!!!
!     BIN CENTER [nm]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: bCENTER = SQRT(bUPPER*bLOWER)
!!!!!!!!!

!!!!!!!!!
!     AVERAGE PARTICLE DENSITY [kg m-3]:
      REAL(8),PARAMETER :: RHO = 1800.0

!     MASS OF BIN CENTERS [kg]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bCENTERm = (3.14159/6.d0)*((bCENTER*1e-9)**3.d0)*RHO

!     MASS OF LOWER BIN BOUNDS [kg]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bLOWERm = (3.14159/6.d0)*((bLOWER*1e-9)**3.d0)*RHO

!     MASS OF UPPER BIN BOUNDS [kg]:
      REAL(8),DIMENSION(nBINS),PARAMETER :: &
      bUPPERm = (3.14159/6.d0)*((bUPPER*1e-9)**3.d0)*RHO
!!!!!!!!!

!!!!!!!!!
!     MASS RATIO OF BINS:
      REAL(8),PARAMETER :: bRATIO = bUPPERm(1)/bLOWERm(1)
!!!!!!!!!



!!!   YICONG HE:



!     DEFINE: ARRAYS
!     ==================================================================
!     POINTER ARRAY: AEROSOL SPECIES
      INTEGER,DIMENSION(nBINS,nCOMP) :: PTRARRAY_AERO
      INTEGER,DIMENSION(nBINS,nCOMP) :: PTRARRAYaw

!     POINTER ARRAY: NUMBERS.
      INTEGER,DIMENSION(nBINS) :: PTRARRAY_NUM
      INTEGER,DIMENSION(nBINS) :: PTRARRAYnw

!     ARRAY: WET AEROSOL COMPONENT BOOLEAN [0 OR 1]
      INTEGER,DIMENSION(nCOMP) :: ARRAY_WETBOOL

!     ARRAY: AEROSOL DENSITY [kg m-3]
      REAL(8),DIMENSION(nCOMP) :: ARRAY_AERODENS

!     DEFINE: EMISSION SIZE DISTRIBUTIONS 
!     ==================================================================
!     EMISSION MODE: SORGAM AITKEN MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M01 = (/ &
      2.50e-01, 6.49e-01, 8.38e-02, 2.00e-04/)

!     EMISSION MODE: SORGAM ACCUMULATION MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M02 = (/ &
      2.50e-01, 6.49e-01, 8.38e-02, 2.00e-04/)

!     EMISSION MODE: SORGAM COARSE MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M03 = (/ &
      0.00e+00, 4.18e-03, 1.70e-01, 7.93e-01/)

!     EMISSION MODE: MOSAIC FINE MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M04 = (/ &
      1.22e-01, 5.42e-01, 3.01e-01, 2.44e-02/)

!     EMISSION MODE: MOSAIC COARSE MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M05 = (/ &
      0.00e+00, 0.00e+00, 5.39e-02, 9.09e-01/)

!     EMISSION MODE: MOSAIC BB FINE MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M06 = (/ &
      7.24e-02, 6.52e-01, 2.71e-01, 4.29e-03/)

!     EMISSION MODE: MOSAIC BB COARSE MODE
      REAL(8),DIMENSION(nBINS),PARAMETER :: M07 = (/ &
      0.00e+00, 0.00e+00, 4.15e-04, 5.49e-04/)

!     ARRAY: STACKED ARRAY FOR EMISSION MODES:
      REAL(8),DIMENSION(nEMISSMODE,nBINS) :: ARRxEMISSMODE 

!     DEFINE: PSEUDO-DATAFRAMES FOR EMISSIONS 
!     ==================================================================
!     DATAFRAME: BIOMASS-BURNING EMISSIONS
      INTEGER,DIMENSION(nEMISSbb) :: df_EMbb_COL01 ! << TARGET 
      INTEGER,DIMENSION(nEMISSbb) :: df_EMbb_COL02 ! << SOURCE
      INTEGER,DIMENSION(nEMISSbb) :: df_EMbb_COL03 ! << EMISSION MASS MODE
      INTEGER,DIMENSION(nEMISSbb) :: df_EMbb_COL04 ! << EMISSION PHASE
      REAL(8),DIMENSION(nEMISSbb) :: df_EMbb_COL05 ! << VOLATILITY
      REAL(8),DIMENSION(nEMISSbb) :: df_EMbb_COL06 ! << MOLECULAR WEIGHT

!     DATAFRAME: ANTHROPOGENIC EMISSIONS
      INTEGER,DIMENSION(nEMISSaa) :: df_EMaa_COL01 ! << TARGET 
      INTEGER,DIMENSION(nEMISSaa) :: df_EMaa_COL02 ! << SOURCE
      INTEGER,DIMENSION(nEMISSaa) :: df_EMaa_COL03 ! << EMISSION MASS MODE
      INTEGER,DIMENSION(nEMISSaa) :: df_EMaa_COL04 ! << EMISSION PHASE
      REAL(8),DIMENSION(nEMISSaa) :: df_EMaa_COL05 ! << VOLATILITY
      REAL(8),DIMENSION(nEMISSaa) :: df_EMaa_COL06 ! << MOLECULAR WEIGHT

!     DEFINE: PSEUDO-DATAFRAME FOR AEROSOL SPECIES
!     ==================================================================
!     DEFINE: SPECIES NAME
      CHARACTER(len=30),DIMENSION(nCOMP) :: df_AERO_COL01

!     DEFINE: INDEX IN CHEM-ARRAY
      INTEGER,DIMENSION(nCOMP,nBINS) :: df_AERO_COL02

!     DEFINE: OTHER COLUMNS
      REAL(8),DIMENSION(nCOMP) :: df_AERO_COL03 ! << MOLECULAR WEIGHT [g mol-1]
      REAL(8),DIMENSION(nCOMP) :: df_AERO_COL04 ! << DENSITY [kg m-3]
      INTEGER,DIMENSION(nCOMP) :: df_AERO_COL05 ! << WET SPECIES [0 or 1]
      INTEGER,DIMENSION(nCOMP) :: df_AERO_COL06 ! << ORGANIC SPECIES [0 or 1]
      REAL(8),DIMENSION(nCOMP) :: df_AERO_COL07 ! << REFRACTIVE INDEX r
      REAL(8),DIMENSION(nCOMP) :: df_AERO_COL08 ! << REFRACTIVE INDEX i

!     DEFINE SUBROUTINE:
!     ==================================================================
      CONTAINS

      SUBROUTINE f_INIT_PTRARRAY

!        INITIALZE DATAFRAME: df_AERO
!        ===============================================================
!        INITIALIZE:
         CALL df_AERO%new()

!        SET COLUMNS:
         CALL df_AERO%APPEND_EMPTYh(nCOMP, 'NAMES')
         CALL df_AERO%APPEND_EMPTYr(nCOMP, 'MW')
         CALL df_AERO%APPEND_EMPTYr(nCOMP, 'DENSITY')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'if-WET')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'if-SORB')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'if-ELECTROLYTE')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'if-CLOUD-ACT')
         CALL df_AERO%APPEND_EMPTYr(nCOMP, 'HYGRO-KAPPA')
         CALL df_AERO%APPEND_EMPTYr(nCOMP, 'REFR-i')
         CALL df_AERO%APPEND_EMPTYr(nCOMP, 'REFR-j')

!        SET COLUMNS:
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRaa-01')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRaa-02')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRaa-03')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRaa-04')

!        SET COLUMNS:
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRcw-01')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRcw-02')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRcw-03')
         CALL df_AERO%APPEND_EMPTYi(nCOMP, 'PTRcw-04')

!        SET COLUMN: df_AERO; SPECIES NAMES
!        ===============================================================
         CALL df_AERO%SETh('NAMES', 0001, 'SO4')
         CALL df_AERO%SETh('NAMES', 0002, 'NO3')
         CALL df_AERO%SETh('NAMES', 0003, 'CL')
         CALL df_AERO%SETh('NAMES', 0004, 'NH4')
         CALL df_AERO%SETh('NAMES', 0005, 'NA')
         CALL df_AERO%SETh('NAMES', 0006, 'OIN')
         CALL df_AERO%SETh('NAMES', 0007, 'OC')
         CALL df_AERO%SETh('NAMES', 0008, 'BC')
         CALL df_AERO%SETh('NAMES', 0009, 'HYSW')
         CALL df_AERO%SETh('NAMES', 0010, 'WATER')
         CALL df_AERO%SETh('NAMES', 0011, 'CLOUDSOA')
         CALL df_AERO%SETh('NAMES', 0012, 'IEPOX')
         CALL df_AERO%SETh('NAMES', 0013, 'IEPOXOS')
         CALL df_AERO%SETh('NAMES', 0014, 'TETROL')
         CALL df_AERO%SETh('NAMES', 0015, 'GLY')
         CALL df_AERO%SETh('NAMES', 0016, 'P07U03Y06')
         CALL df_AERO%SETh('NAMES', 0017, 'P07X00Y06')
         CALL df_AERO%SETh('NAMES', 0018, 'P07X03Y06')
         CALL df_AERO%SETh('NAMES', 0019, 'S01U02Y03')
         CALL df_AERO%SETh('NAMES', 0020, 'S01U02Y06')
         CALL df_AERO%SETh('NAMES', 0021, 'S01U02Y20')
         CALL df_AERO%SETh('NAMES', 0022, 'S01X00Y03')
         CALL df_AERO%SETh('NAMES', 0023, 'S01X00Y06')
         CALL df_AERO%SETh('NAMES', 0024, 'S01X00Y20')
         CALL df_AERO%SETh('NAMES', 0025, 'S01X01Y03')
         CALL df_AERO%SETh('NAMES', 0026, 'S01X01Y06')
         CALL df_AERO%SETh('NAMES', 0027, 'S01X01Y20')
         CALL df_AERO%SETh('NAMES', 0028, 'S01X03Y03')
         CALL df_AERO%SETh('NAMES', 0029, 'S01X03Y06')
         CALL df_AERO%SETh('NAMES', 0030, 'S01X03Y20')
         CALL df_AERO%SETh('NAMES', 0031, 'S02U02Y03')
         CALL df_AERO%SETh('NAMES', 0032, 'S02U02Y06')
         CALL df_AERO%SETh('NAMES', 0033, 'S02U02Y20')
         CALL df_AERO%SETh('NAMES', 0034, 'S02X00Y03')
         CALL df_AERO%SETh('NAMES', 0035, 'S02X00Y06')
         CALL df_AERO%SETh('NAMES', 0036, 'S02X00Y20')
         CALL df_AERO%SETh('NAMES', 0037, 'S02X01Y03')
         CALL df_AERO%SETh('NAMES', 0038, 'S02X01Y06')
         CALL df_AERO%SETh('NAMES', 0039, 'S02X01Y20')
         CALL df_AERO%SETh('NAMES', 0040, 'S02X03Y03')
         CALL df_AERO%SETh('NAMES', 0041, 'S02X03Y06')
         CALL df_AERO%SETh('NAMES', 0042, 'S02X03Y20')
         CALL df_AERO%SETh('NAMES', 0043, 'S03U02Y03')
         CALL df_AERO%SETh('NAMES', 0044, 'S03U02Y06')
         CALL df_AERO%SETh('NAMES', 0045, 'S03U02Y20')
         CALL df_AERO%SETh('NAMES', 0046, 'S03X00Y03')
         CALL df_AERO%SETh('NAMES', 0047, 'S03X00Y06')
         CALL df_AERO%SETh('NAMES', 0048, 'S03X00Y20')
         CALL df_AERO%SETh('NAMES', 0049, 'S03X01Y03')
         CALL df_AERO%SETh('NAMES', 0050, 'S03X01Y06')
         CALL df_AERO%SETh('NAMES', 0051, 'S03X01Y20')
         CALL df_AERO%SETh('NAMES', 0052, 'S03X03Y03')
         CALL df_AERO%SETh('NAMES', 0053, 'S03X03Y06')
         CALL df_AERO%SETh('NAMES', 0054, 'S03X03Y20')
         CALL df_AERO%SETh('NAMES', 0055, 'S04U02Y03')
         CALL df_AERO%SETh('NAMES', 0056, 'S04U02Y06')
         CALL df_AERO%SETh('NAMES', 0057, 'S04U02Y20')
         CALL df_AERO%SETh('NAMES', 0058, 'S04X00Y03')
         CALL df_AERO%SETh('NAMES', 0059, 'S04X00Y06')
         CALL df_AERO%SETh('NAMES', 0060, 'S04X00Y20')
         CALL df_AERO%SETh('NAMES', 0061, 'S04X01Y03')
         CALL df_AERO%SETh('NAMES', 0062, 'S04X01Y06')
         CALL df_AERO%SETh('NAMES', 0063, 'S04X01Y20')
         CALL df_AERO%SETh('NAMES', 0064, 'S04X03Y03')
         CALL df_AERO%SETh('NAMES', 0065, 'S04X03Y06')
         CALL df_AERO%SETh('NAMES', 0066, 'S04X03Y20')
         CALL df_AERO%SETh('NAMES', 0067, 'S05U02Y03')
         CALL df_AERO%SETh('NAMES', 0068, 'S05U02Y05')
         CALL df_AERO%SETh('NAMES', 0069, 'S05U02Y20')
         CALL df_AERO%SETh('NAMES', 0070, 'S05X00Y03')
         CALL df_AERO%SETh('NAMES', 0071, 'S05X00Y05')
         CALL df_AERO%SETh('NAMES', 0072, 'S05X00Y20')
         CALL df_AERO%SETh('NAMES', 0073, 'S05X01Y03')
         CALL df_AERO%SETh('NAMES', 0074, 'S05X01Y05')
         CALL df_AERO%SETh('NAMES', 0075, 'S05X01Y20')
         CALL df_AERO%SETh('NAMES', 0076, 'S05X03Y03')
         CALL df_AERO%SETh('NAMES', 0077, 'S05X03Y05')
         CALL df_AERO%SETh('NAMES', 0078, 'S05X03Y20')
         CALL df_AERO%SETh('NAMES', 0079, 'S06U02Y03')
         CALL df_AERO%SETh('NAMES', 0080, 'S06U02Y06')
         CALL df_AERO%SETh('NAMES', 0081, 'S06U02Y20')
         CALL df_AERO%SETh('NAMES', 0082, 'S06X00Y03')
         CALL df_AERO%SETh('NAMES', 0083, 'S06X00Y06')
         CALL df_AERO%SETh('NAMES', 0084, 'S06X00Y20')
         CALL df_AERO%SETh('NAMES', 0085, 'S06X01Y03')
         CALL df_AERO%SETh('NAMES', 0086, 'S06X01Y06')
         CALL df_AERO%SETh('NAMES', 0087, 'S06X01Y20')
         CALL df_AERO%SETh('NAMES', 0088, 'S06X03Y03')
         CALL df_AERO%SETh('NAMES', 0089, 'S06X03Y06')
         CALL df_AERO%SETh('NAMES', 0090, 'S06X03Y20')
         CALL df_AERO%SETh('NAMES', 0091, 'S07U02Y00')
         CALL df_AERO%SETh('NAMES', 0092, 'S07U02Y05')
         CALL df_AERO%SETh('NAMES', 0093, 'S07U02Y10')
         CALL df_AERO%SETh('NAMES', 0094, 'S07X00Y00')
         CALL df_AERO%SETh('NAMES', 0095, 'S07X00Y05')
         CALL df_AERO%SETh('NAMES', 0096, 'S07X00Y10')
         CALL df_AERO%SETh('NAMES', 0097, 'S07X01Y00')
         CALL df_AERO%SETh('NAMES', 0098, 'S07X01Y05')
         CALL df_AERO%SETh('NAMES', 0099, 'S07X01Y10')
         CALL df_AERO%SETh('NAMES', 0100, 'S07X03Y00')
         CALL df_AERO%SETh('NAMES', 0101, 'S07X03Y05')
         CALL df_AERO%SETh('NAMES', 0102, 'S07X03Y10')

!        SET COLUMN: df_AERO; MOLECULAR WEIGHT [g mol-1]
!        ===============================================================
         CALL df_AERO%SETr('MW', 0001, 96.00d0)
         CALL df_AERO%SETr('MW', 0002, 62.00d0)
         CALL df_AERO%SETr('MW', 0003, 35.50d0)
         CALL df_AERO%SETr('MW', 0004, 18.00d0)
         CALL df_AERO%SETr('MW', 0005, 23.00d0)
         CALL df_AERO%SETr('MW', 0006, 1.00d0)
         CALL df_AERO%SETr('MW', 0007, 250.00d0)
         CALL df_AERO%SETr('MW', 0008, 1.00d0)
         CALL df_AERO%SETr('MW', 0009, 18.00d0)
         CALL df_AERO%SETr('MW', 0010, 18.00d0)
         CALL df_AERO%SETr('MW', 0011, 250.00d0)
         CALL df_AERO%SETr('MW', 0012, 118.00d0)
         CALL df_AERO%SETr('MW', 0013, 126.00d0)
         CALL df_AERO%SETr('MW', 0014, 136.00d0)
         CALL df_AERO%SETr('MW', 0015, 58.00d0)
         CALL df_AERO%SETr('MW', 0016, 202.30d0)
         CALL df_AERO%SETr('MW', 0017, 163.14d0)
         CALL df_AERO%SETr('MW', 0018, 123.99d0)
         CALL df_AERO%SETr('MW', 0019, 231.66d0)
         CALL df_AERO%SETr('MW', 0020, 189.24d0)
         CALL df_AERO%SETr('MW', 0021, 135.74d0)
         CALL df_AERO%SETr('MW', 0022, 199.71d0)
         CALL df_AERO%SETr('MW', 0023, 163.14d0)
         CALL df_AERO%SETr('MW', 0024, 117.02d0)
         CALL df_AERO%SETr('MW', 0025, 183.73d0)
         CALL df_AERO%SETr('MW', 0026, 150.09d0)
         CALL df_AERO%SETr('MW', 0027, 107.66d0)
         CALL df_AERO%SETr('MW', 0028, 151.78d0)
         CALL df_AERO%SETr('MW', 0029, 123.99d0)
         CALL df_AERO%SETr('MW', 0030, 88.94d0)
         CALL df_AERO%SETr('MW', 0031, 231.66d0)
         CALL df_AERO%SETr('MW', 0032, 189.24d0)
         CALL df_AERO%SETr('MW', 0033, 135.74d0)
         CALL df_AERO%SETr('MW', 0034, 199.71d0)
         CALL df_AERO%SETr('MW', 0035, 163.14d0)
         CALL df_AERO%SETr('MW', 0036, 117.02d0)
         CALL df_AERO%SETr('MW', 0037, 183.73d0)
         CALL df_AERO%SETr('MW', 0038, 150.09d0)
         CALL df_AERO%SETr('MW', 0039, 107.66d0)
         CALL df_AERO%SETr('MW', 0040, 151.78d0)
         CALL df_AERO%SETr('MW', 0041, 123.99d0)
         CALL df_AERO%SETr('MW', 0042, 88.94d0)
         CALL df_AERO%SETr('MW', 0043, 231.66d0)
         CALL df_AERO%SETr('MW', 0044, 189.24d0)
         CALL df_AERO%SETr('MW', 0045, 135.74d0)
         CALL df_AERO%SETr('MW', 0046, 199.71d0)
         CALL df_AERO%SETr('MW', 0047, 163.14d0)
         CALL df_AERO%SETr('MW', 0048, 117.02d0)
         CALL df_AERO%SETr('MW', 0049, 183.73d0)
         CALL df_AERO%SETr('MW', 0050, 150.09d0)
         CALL df_AERO%SETr('MW', 0051, 107.66d0)
         CALL df_AERO%SETr('MW', 0052, 151.78d0)
         CALL df_AERO%SETr('MW', 0053, 123.99d0)
         CALL df_AERO%SETr('MW', 0054, 88.94d0)
         CALL df_AERO%SETr('MW', 0055, 231.66d0)
         CALL df_AERO%SETr('MW', 0056, 189.24d0)
         CALL df_AERO%SETr('MW', 0057, 135.74d0)
         CALL df_AERO%SETr('MW', 0058, 199.71d0)
         CALL df_AERO%SETr('MW', 0059, 163.14d0)
         CALL df_AERO%SETr('MW', 0060, 117.02d0)
         CALL df_AERO%SETr('MW', 0061, 183.73d0)
         CALL df_AERO%SETr('MW', 0062, 150.09d0)
         CALL df_AERO%SETr('MW', 0063, 107.66d0)
         CALL df_AERO%SETr('MW', 0064, 151.78d0)
         CALL df_AERO%SETr('MW', 0065, 123.99d0)
         CALL df_AERO%SETr('MW', 0066, 88.94d0)
         CALL df_AERO%SETr('MW', 0067, 231.66d0)
         CALL df_AERO%SETr('MW', 0068, 200.00d0)
         CALL df_AERO%SETr('MW', 0069, 135.74d0)
         CALL df_AERO%SETr('MW', 0070, 199.71d0)
         CALL df_AERO%SETr('MW', 0071, 172.41d0)
         CALL df_AERO%SETr('MW', 0072, 117.02d0)
         CALL df_AERO%SETr('MW', 0073, 183.73d0)
         CALL df_AERO%SETr('MW', 0074, 158.62d0)
         CALL df_AERO%SETr('MW', 0075, 107.66d0)
         CALL df_AERO%SETr('MW', 0076, 151.78d0)
         CALL df_AERO%SETr('MW', 0077, 131.03d0)
         CALL df_AERO%SETr('MW', 0078, 88.94d0)
         CALL df_AERO%SETr('MW', 0079, 231.66d0)
         CALL df_AERO%SETr('MW', 0080, 189.24d0)
         CALL df_AERO%SETr('MW', 0081, 135.74d0)
         CALL df_AERO%SETr('MW', 0082, 199.71d0)
         CALL df_AERO%SETr('MW', 0083, 163.14d0)
         CALL df_AERO%SETr('MW', 0084, 117.02d0)
         CALL df_AERO%SETr('MW', 0085, 183.73d0)
         CALL df_AERO%SETr('MW', 0086, 150.09d0)
         CALL df_AERO%SETr('MW', 0087, 107.66d0)
         CALL df_AERO%SETr('MW', 0088, 151.78d0)
         CALL df_AERO%SETr('MW', 0089, 123.99d0)
         CALL df_AERO%SETr('MW', 0090, 88.94d0)
         CALL df_AERO%SETr('MW', 0091, 348.00d0)
         CALL df_AERO%SETr('MW', 0092, 200.00d0)
         CALL df_AERO%SETr('MW', 0093, 162.40d0)
         CALL df_AERO%SETr('MW', 0094, 300.00d0)
         CALL df_AERO%SETr('MW', 0095, 172.41d0)
         CALL df_AERO%SETr('MW', 0096, 140.00d0)
         CALL df_AERO%SETr('MW', 0097, 276.00d0)
         CALL df_AERO%SETr('MW', 0098, 158.62d0)
         CALL df_AERO%SETr('MW', 0099, 128.80d0)
         CALL df_AERO%SETr('MW', 0100, 228.00d0)
         CALL df_AERO%SETr('MW', 0101, 131.03d0)
         CALL df_AERO%SETr('MW', 0102, 106.40d0)

!        SET COLUMN: df_AERO; DENSITY [kg m-3]
!        ===============================================================
         CALL df_AERO%SETr('DENSITY', 0001, 1800.00d0)
         CALL df_AERO%SETr('DENSITY', 0002, 1800.00d0)
         CALL df_AERO%SETr('DENSITY', 0003, 2200.00d0)
         CALL df_AERO%SETr('DENSITY', 0004, 1800.00d0)
         CALL df_AERO%SETr('DENSITY', 0005, 2200.00d0)
         CALL df_AERO%SETr('DENSITY', 0006, 2600.00d0)
         CALL df_AERO%SETr('DENSITY', 0007, 1000.00d0)
         CALL df_AERO%SETr('DENSITY', 0008, 1700.00d0)
         CALL df_AERO%SETr('DENSITY', 0009, 1000.00d0)
         CALL df_AERO%SETr('DENSITY', 0010, 1000.00d0)
         CALL df_AERO%SETr('DENSITY', 0011, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0012, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0013, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0014, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0015, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0016, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0017, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0018, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0019, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0020, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0021, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0022, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0023, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0024, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0025, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0026, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0027, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0028, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0029, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0030, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0031, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0032, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0033, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0034, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0035, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0036, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0037, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0038, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0039, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0040, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0041, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0042, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0043, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0044, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0045, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0046, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0047, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0048, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0049, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0050, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0051, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0052, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0053, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0054, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0055, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0056, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0057, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0058, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0059, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0060, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0061, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0062, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0063, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0064, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0065, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0066, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0067, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0068, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0069, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0070, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0071, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0072, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0073, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0074, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0075, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0076, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0077, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0078, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0079, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0080, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0081, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0082, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0083, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0084, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0085, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0086, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0087, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0088, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0089, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0090, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0091, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0092, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0093, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0094, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0095, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0096, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0097, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0098, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0099, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0100, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0101, 1400.00d0)
         CALL df_AERO%SETr('DENSITY', 0102, 1400.00d0)

!        SET COLUMN: df_AERO; WET FLAG
!        ===============================================================
         CALL df_AERO%SETi('if-WET', 0001, 0)
         CALL df_AERO%SETi('if-WET', 0002, 0)
         CALL df_AERO%SETi('if-WET', 0003, 0)
         CALL df_AERO%SETi('if-WET', 0004, 0)
         CALL df_AERO%SETi('if-WET', 0005, 0)
         CALL df_AERO%SETi('if-WET', 0006, 0)
         CALL df_AERO%SETi('if-WET', 0007, 0)
         CALL df_AERO%SETi('if-WET', 0008, 0)
         CALL df_AERO%SETi('if-WET', 0009, 1)
         CALL df_AERO%SETi('if-WET', 0010, 1)
         CALL df_AERO%SETi('if-WET', 0011, 0)
         CALL df_AERO%SETi('if-WET', 0012, 0)
         CALL df_AERO%SETi('if-WET', 0013, 0)
         CALL df_AERO%SETi('if-WET', 0014, 0)
         CALL df_AERO%SETi('if-WET', 0015, 0)
         CALL df_AERO%SETi('if-WET', 0016, 0)
         CALL df_AERO%SETi('if-WET', 0017, 0)
         CALL df_AERO%SETi('if-WET', 0018, 0)
         CALL df_AERO%SETi('if-WET', 0019, 0)
         CALL df_AERO%SETi('if-WET', 0020, 0)
         CALL df_AERO%SETi('if-WET', 0021, 0)
         CALL df_AERO%SETi('if-WET', 0022, 0)
         CALL df_AERO%SETi('if-WET', 0023, 0)
         CALL df_AERO%SETi('if-WET', 0024, 0)
         CALL df_AERO%SETi('if-WET', 0025, 0)
         CALL df_AERO%SETi('if-WET', 0026, 0)
         CALL df_AERO%SETi('if-WET', 0027, 0)
         CALL df_AERO%SETi('if-WET', 0028, 0)
         CALL df_AERO%SETi('if-WET', 0029, 0)
         CALL df_AERO%SETi('if-WET', 0030, 0)
         CALL df_AERO%SETi('if-WET', 0031, 0)
         CALL df_AERO%SETi('if-WET', 0032, 0)
         CALL df_AERO%SETi('if-WET', 0033, 0)
         CALL df_AERO%SETi('if-WET', 0034, 0)
         CALL df_AERO%SETi('if-WET', 0035, 0)
         CALL df_AERO%SETi('if-WET', 0036, 0)
         CALL df_AERO%SETi('if-WET', 0037, 0)
         CALL df_AERO%SETi('if-WET', 0038, 0)
         CALL df_AERO%SETi('if-WET', 0039, 0)
         CALL df_AERO%SETi('if-WET', 0040, 0)
         CALL df_AERO%SETi('if-WET', 0041, 0)
         CALL df_AERO%SETi('if-WET', 0042, 0)
         CALL df_AERO%SETi('if-WET', 0043, 0)
         CALL df_AERO%SETi('if-WET', 0044, 0)
         CALL df_AERO%SETi('if-WET', 0045, 0)
         CALL df_AERO%SETi('if-WET', 0046, 0)
         CALL df_AERO%SETi('if-WET', 0047, 0)
         CALL df_AERO%SETi('if-WET', 0048, 0)
         CALL df_AERO%SETi('if-WET', 0049, 0)
         CALL df_AERO%SETi('if-WET', 0050, 0)
         CALL df_AERO%SETi('if-WET', 0051, 0)
         CALL df_AERO%SETi('if-WET', 0052, 0)
         CALL df_AERO%SETi('if-WET', 0053, 0)
         CALL df_AERO%SETi('if-WET', 0054, 0)
         CALL df_AERO%SETi('if-WET', 0055, 0)
         CALL df_AERO%SETi('if-WET', 0056, 0)
         CALL df_AERO%SETi('if-WET', 0057, 0)
         CALL df_AERO%SETi('if-WET', 0058, 0)
         CALL df_AERO%SETi('if-WET', 0059, 0)
         CALL df_AERO%SETi('if-WET', 0060, 0)
         CALL df_AERO%SETi('if-WET', 0061, 0)
         CALL df_AERO%SETi('if-WET', 0062, 0)
         CALL df_AERO%SETi('if-WET', 0063, 0)
         CALL df_AERO%SETi('if-WET', 0064, 0)
         CALL df_AERO%SETi('if-WET', 0065, 0)
         CALL df_AERO%SETi('if-WET', 0066, 0)
         CALL df_AERO%SETi('if-WET', 0067, 0)
         CALL df_AERO%SETi('if-WET', 0068, 0)
         CALL df_AERO%SETi('if-WET', 0069, 0)
         CALL df_AERO%SETi('if-WET', 0070, 0)
         CALL df_AERO%SETi('if-WET', 0071, 0)
         CALL df_AERO%SETi('if-WET', 0072, 0)
         CALL df_AERO%SETi('if-WET', 0073, 0)
         CALL df_AERO%SETi('if-WET', 0074, 0)
         CALL df_AERO%SETi('if-WET', 0075, 0)
         CALL df_AERO%SETi('if-WET', 0076, 0)
         CALL df_AERO%SETi('if-WET', 0077, 0)
         CALL df_AERO%SETi('if-WET', 0078, 0)
         CALL df_AERO%SETi('if-WET', 0079, 0)
         CALL df_AERO%SETi('if-WET', 0080, 0)
         CALL df_AERO%SETi('if-WET', 0081, 0)
         CALL df_AERO%SETi('if-WET', 0082, 0)
         CALL df_AERO%SETi('if-WET', 0083, 0)
         CALL df_AERO%SETi('if-WET', 0084, 0)
         CALL df_AERO%SETi('if-WET', 0085, 0)
         CALL df_AERO%SETi('if-WET', 0086, 0)
         CALL df_AERO%SETi('if-WET', 0087, 0)
         CALL df_AERO%SETi('if-WET', 0088, 0)
         CALL df_AERO%SETi('if-WET', 0089, 0)
         CALL df_AERO%SETi('if-WET', 0090, 0)
         CALL df_AERO%SETi('if-WET', 0091, 0)
         CALL df_AERO%SETi('if-WET', 0092, 0)
         CALL df_AERO%SETi('if-WET', 0093, 0)
         CALL df_AERO%SETi('if-WET', 0094, 0)
         CALL df_AERO%SETi('if-WET', 0095, 0)
         CALL df_AERO%SETi('if-WET', 0096, 0)
         CALL df_AERO%SETi('if-WET', 0097, 0)
         CALL df_AERO%SETi('if-WET', 0098, 0)
         CALL df_AERO%SETi('if-WET', 0099, 0)
         CALL df_AERO%SETi('if-WET', 0100, 0)
         CALL df_AERO%SETi('if-WET', 0101, 0)
         CALL df_AERO%SETi('if-WET', 0102, 0)

!        SET COLUMN: df_AERO; ABSORBING FLAG
!        ===============================================================
         CALL df_AERO%SETi('if-SORB', 0001, 0)
         CALL df_AERO%SETi('if-SORB', 0002, 0)
         CALL df_AERO%SETi('if-SORB', 0003, 0)
         CALL df_AERO%SETi('if-SORB', 0004, 0)
         CALL df_AERO%SETi('if-SORB', 0005, 0)
         CALL df_AERO%SETi('if-SORB', 0006, 0)
         CALL df_AERO%SETi('if-SORB', 0007, 0)
         CALL df_AERO%SETi('if-SORB', 0008, 0)
         CALL df_AERO%SETi('if-SORB', 0009, 0)
         CALL df_AERO%SETi('if-SORB', 0010, 0)
         CALL df_AERO%SETi('if-SORB', 0011, 1)
         CALL df_AERO%SETi('if-SORB', 0012, 0)
         CALL df_AERO%SETi('if-SORB', 0013, 0)
         CALL df_AERO%SETi('if-SORB', 0014, 0)
         CALL df_AERO%SETi('if-SORB', 0015, 0)
         CALL df_AERO%SETi('if-SORB', 0016, 1)
         CALL df_AERO%SETi('if-SORB', 0017, 1)
         CALL df_AERO%SETi('if-SORB', 0018, 1)
         CALL df_AERO%SETi('if-SORB', 0019, 1)
         CALL df_AERO%SETi('if-SORB', 0020, 1)
         CALL df_AERO%SETi('if-SORB', 0021, 1)
         CALL df_AERO%SETi('if-SORB', 0022, 1)
         CALL df_AERO%SETi('if-SORB', 0023, 1)
         CALL df_AERO%SETi('if-SORB', 0024, 1)
         CALL df_AERO%SETi('if-SORB', 0025, 1)
         CALL df_AERO%SETi('if-SORB', 0026, 1)
         CALL df_AERO%SETi('if-SORB', 0027, 1)
         CALL df_AERO%SETi('if-SORB', 0028, 1)
         CALL df_AERO%SETi('if-SORB', 0029, 1)
         CALL df_AERO%SETi('if-SORB', 0030, 1)
         CALL df_AERO%SETi('if-SORB', 0031, 1)
         CALL df_AERO%SETi('if-SORB', 0032, 1)
         CALL df_AERO%SETi('if-SORB', 0033, 1)
         CALL df_AERO%SETi('if-SORB', 0034, 1)
         CALL df_AERO%SETi('if-SORB', 0035, 1)
         CALL df_AERO%SETi('if-SORB', 0036, 1)
         CALL df_AERO%SETi('if-SORB', 0037, 1)
         CALL df_AERO%SETi('if-SORB', 0038, 1)
         CALL df_AERO%SETi('if-SORB', 0039, 1)
         CALL df_AERO%SETi('if-SORB', 0040, 1)
         CALL df_AERO%SETi('if-SORB', 0041, 1)
         CALL df_AERO%SETi('if-SORB', 0042, 1)
         CALL df_AERO%SETi('if-SORB', 0043, 1)
         CALL df_AERO%SETi('if-SORB', 0044, 1)
         CALL df_AERO%SETi('if-SORB', 0045, 1)
         CALL df_AERO%SETi('if-SORB', 0046, 1)
         CALL df_AERO%SETi('if-SORB', 0047, 1)
         CALL df_AERO%SETi('if-SORB', 0048, 1)
         CALL df_AERO%SETi('if-SORB', 0049, 1)
         CALL df_AERO%SETi('if-SORB', 0050, 1)
         CALL df_AERO%SETi('if-SORB', 0051, 1)
         CALL df_AERO%SETi('if-SORB', 0052, 1)
         CALL df_AERO%SETi('if-SORB', 0053, 1)
         CALL df_AERO%SETi('if-SORB', 0054, 1)
         CALL df_AERO%SETi('if-SORB', 0055, 1)
         CALL df_AERO%SETi('if-SORB', 0056, 1)
         CALL df_AERO%SETi('if-SORB', 0057, 1)
         CALL df_AERO%SETi('if-SORB', 0058, 1)
         CALL df_AERO%SETi('if-SORB', 0059, 1)
         CALL df_AERO%SETi('if-SORB', 0060, 1)
         CALL df_AERO%SETi('if-SORB', 0061, 1)
         CALL df_AERO%SETi('if-SORB', 0062, 1)
         CALL df_AERO%SETi('if-SORB', 0063, 1)
         CALL df_AERO%SETi('if-SORB', 0064, 1)
         CALL df_AERO%SETi('if-SORB', 0065, 1)
         CALL df_AERO%SETi('if-SORB', 0066, 1)
         CALL df_AERO%SETi('if-SORB', 0067, 1)
         CALL df_AERO%SETi('if-SORB', 0068, 1)
         CALL df_AERO%SETi('if-SORB', 0069, 1)
         CALL df_AERO%SETi('if-SORB', 0070, 1)
         CALL df_AERO%SETi('if-SORB', 0071, 1)
         CALL df_AERO%SETi('if-SORB', 0072, 1)
         CALL df_AERO%SETi('if-SORB', 0073, 1)
         CALL df_AERO%SETi('if-SORB', 0074, 1)
         CALL df_AERO%SETi('if-SORB', 0075, 1)
         CALL df_AERO%SETi('if-SORB', 0076, 1)
         CALL df_AERO%SETi('if-SORB', 0077, 1)
         CALL df_AERO%SETi('if-SORB', 0078, 1)
         CALL df_AERO%SETi('if-SORB', 0079, 1)
         CALL df_AERO%SETi('if-SORB', 0080, 1)
         CALL df_AERO%SETi('if-SORB', 0081, 1)
         CALL df_AERO%SETi('if-SORB', 0082, 1)
         CALL df_AERO%SETi('if-SORB', 0083, 1)
         CALL df_AERO%SETi('if-SORB', 0084, 1)
         CALL df_AERO%SETi('if-SORB', 0085, 1)
         CALL df_AERO%SETi('if-SORB', 0086, 1)
         CALL df_AERO%SETi('if-SORB', 0087, 1)
         CALL df_AERO%SETi('if-SORB', 0088, 1)
         CALL df_AERO%SETi('if-SORB', 0089, 1)
         CALL df_AERO%SETi('if-SORB', 0090, 1)
         CALL df_AERO%SETi('if-SORB', 0091, 1)
         CALL df_AERO%SETi('if-SORB', 0092, 1)
         CALL df_AERO%SETi('if-SORB', 0093, 1)
         CALL df_AERO%SETi('if-SORB', 0094, 1)
         CALL df_AERO%SETi('if-SORB', 0095, 1)
         CALL df_AERO%SETi('if-SORB', 0096, 1)
         CALL df_AERO%SETi('if-SORB', 0097, 1)
         CALL df_AERO%SETi('if-SORB', 0098, 1)
         CALL df_AERO%SETi('if-SORB', 0099, 1)
         CALL df_AERO%SETi('if-SORB', 0100, 1)
         CALL df_AERO%SETi('if-SORB', 0101, 1)
         CALL df_AERO%SETi('if-SORB', 0102, 1)

!        SET COLUMN: df_AERO COLUMN={if-CLOUD-ACT}
!        NOTE: FOR WHETHER THE SPECIES EXISTS IN ACTIVATED DROPLETS
!        ===============================================================
         CALL df_AERO%SETi('if-CLOUD-ACT', 0001, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0002, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0003, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0004, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0005, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0006, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0007, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0008, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0009, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0010, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0011, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0012, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0013, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0014, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0015, 1)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0016, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0017, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0018, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0019, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0020, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0021, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0022, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0023, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0024, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0025, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0026, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0027, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0028, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0029, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0030, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0031, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0032, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0033, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0034, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0035, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0036, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0037, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0038, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0039, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0040, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0041, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0042, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0043, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0044, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0045, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0046, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0047, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0048, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0049, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0050, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0051, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0052, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0053, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0054, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0055, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0056, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0057, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0058, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0059, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0060, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0061, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0062, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0063, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0064, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0065, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0066, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0067, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0068, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0069, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0070, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0071, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0072, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0073, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0074, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0075, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0076, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0077, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0078, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0079, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0080, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0081, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0082, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0083, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0084, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0085, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0086, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0087, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0088, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0089, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0090, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0091, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0092, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0093, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0094, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0095, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0096, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0097, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0098, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0099, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0100, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0101, 0)
         CALL df_AERO%SETi('if-CLOUD-ACT', 0102, 0)

!        SET COLUMN: df_AERO; AEROSOL-PHASE POINTERS
!        ===============================================================
         CALL df_AERO%SETi('PTRaa-01', 0001, p_SO4_a01)
         CALL df_AERO%SETi('PTRaa-01', 0002, p_NO3_a01)
         CALL df_AERO%SETi('PTRaa-01', 0003, p_CL_a01)
         CALL df_AERO%SETi('PTRaa-01', 0004, p_NH4_a01)
         CALL df_AERO%SETi('PTRaa-01', 0005, p_NA_a01)
         CALL df_AERO%SETi('PTRaa-01', 0006, p_OIN_a01)
         CALL df_AERO%SETi('PTRaa-01', 0007, p_OC_a01)
         CALL df_AERO%SETi('PTRaa-01', 0008, p_BC_a01)
         CALL df_AERO%SETi('PTRaa-01', 0009, p_HYSW_a01)
         CALL df_AERO%SETi('PTRaa-01', 0010, p_WATER_a01)
         CALL df_AERO%SETi('PTRaa-01', 0011, p_CLOUDSOA_a01)
         CALL df_AERO%SETi('PTRaa-01', 0012, p_IEPOX_a01)
         CALL df_AERO%SETi('PTRaa-01', 0013, p_IEPOXOS_a01)
         CALL df_AERO%SETi('PTRaa-01', 0014, p_TETROL_a01)
         CALL df_AERO%SETi('PTRaa-01', 0015, p_GLY_a01)
         CALL df_AERO%SETi('PTRaa-01', 0016, p_P07U03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0017, p_P07X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0018, p_P07X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0019, p_S01U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0020, p_S01U02Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0021, p_S01U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0022, p_S01X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0023, p_S01X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0024, p_S01X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0025, p_S01X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0026, p_S01X01Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0027, p_S01X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0028, p_S01X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0029, p_S01X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0030, p_S01X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0031, p_S02U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0032, p_S02U02Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0033, p_S02U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0034, p_S02X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0035, p_S02X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0036, p_S02X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0037, p_S02X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0038, p_S02X01Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0039, p_S02X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0040, p_S02X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0041, p_S02X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0042, p_S02X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0043, p_S03U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0044, p_S03U02Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0045, p_S03U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0046, p_S03X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0047, p_S03X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0048, p_S03X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0049, p_S03X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0050, p_S03X01Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0051, p_S03X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0052, p_S03X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0053, p_S03X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0054, p_S03X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0055, p_S04U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0056, p_S04U02Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0057, p_S04U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0058, p_S04X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0059, p_S04X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0060, p_S04X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0061, p_S04X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0062, p_S04X01Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0063, p_S04X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0064, p_S04X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0065, p_S04X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0066, p_S04X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0067, p_S05U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0068, p_S05U02Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0069, p_S05U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0070, p_S05X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0071, p_S05X00Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0072, p_S05X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0073, p_S05X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0074, p_S05X01Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0075, p_S05X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0076, p_S05X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0077, p_S05X03Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0078, p_S05X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0079, p_S06U02Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0080, p_S06U02Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0081, p_S06U02Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0082, p_S06X00Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0083, p_S06X00Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0084, p_S06X00Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0085, p_S06X01Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0086, p_S06X01Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0087, p_S06X01Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0088, p_S06X03Y03_a01)
         CALL df_AERO%SETi('PTRaa-01', 0089, p_S06X03Y06_a01)
         CALL df_AERO%SETi('PTRaa-01', 0090, p_S06X03Y20_a01)
         CALL df_AERO%SETi('PTRaa-01', 0091, p_S07U02Y00_a01)
         CALL df_AERO%SETi('PTRaa-01', 0092, p_S07U02Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0093, p_S07U02Y10_a01)
         CALL df_AERO%SETi('PTRaa-01', 0094, p_S07X00Y00_a01)
         CALL df_AERO%SETi('PTRaa-01', 0095, p_S07X00Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0096, p_S07X00Y10_a01)
         CALL df_AERO%SETi('PTRaa-01', 0097, p_S07X01Y00_a01)
         CALL df_AERO%SETi('PTRaa-01', 0098, p_S07X01Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0099, p_S07X01Y10_a01)
         CALL df_AERO%SETi('PTRaa-01', 0100, p_S07X03Y00_a01)
         CALL df_AERO%SETi('PTRaa-01', 0101, p_S07X03Y05_a01)
         CALL df_AERO%SETi('PTRaa-01', 0102, p_S07X03Y10_a01)

         CALL df_AERO%SETi('PTRaa-02', 0001, p_SO4_a02)
         CALL df_AERO%SETi('PTRaa-02', 0002, p_NO3_a02)
         CALL df_AERO%SETi('PTRaa-02', 0003, p_CL_a02)
         CALL df_AERO%SETi('PTRaa-02', 0004, p_NH4_a02)
         CALL df_AERO%SETi('PTRaa-02', 0005, p_NA_a02)
         CALL df_AERO%SETi('PTRaa-02', 0006, p_OIN_a02)
         CALL df_AERO%SETi('PTRaa-02', 0007, p_OC_a02)
         CALL df_AERO%SETi('PTRaa-02', 0008, p_BC_a02)
         CALL df_AERO%SETi('PTRaa-02', 0009, p_HYSW_a02)
         CALL df_AERO%SETi('PTRaa-02', 0010, p_WATER_a02)
         CALL df_AERO%SETi('PTRaa-02', 0011, p_CLOUDSOA_a02)
         CALL df_AERO%SETi('PTRaa-02', 0012, p_IEPOX_a02)
         CALL df_AERO%SETi('PTRaa-02', 0013, p_IEPOXOS_a02)
         CALL df_AERO%SETi('PTRaa-02', 0014, p_TETROL_a02)
         CALL df_AERO%SETi('PTRaa-02', 0015, p_GLY_a02)
         CALL df_AERO%SETi('PTRaa-02', 0016, p_P07U03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0017, p_P07X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0018, p_P07X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0019, p_S01U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0020, p_S01U02Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0021, p_S01U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0022, p_S01X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0023, p_S01X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0024, p_S01X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0025, p_S01X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0026, p_S01X01Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0027, p_S01X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0028, p_S01X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0029, p_S01X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0030, p_S01X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0031, p_S02U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0032, p_S02U02Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0033, p_S02U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0034, p_S02X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0035, p_S02X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0036, p_S02X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0037, p_S02X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0038, p_S02X01Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0039, p_S02X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0040, p_S02X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0041, p_S02X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0042, p_S02X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0043, p_S03U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0044, p_S03U02Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0045, p_S03U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0046, p_S03X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0047, p_S03X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0048, p_S03X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0049, p_S03X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0050, p_S03X01Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0051, p_S03X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0052, p_S03X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0053, p_S03X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0054, p_S03X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0055, p_S04U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0056, p_S04U02Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0057, p_S04U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0058, p_S04X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0059, p_S04X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0060, p_S04X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0061, p_S04X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0062, p_S04X01Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0063, p_S04X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0064, p_S04X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0065, p_S04X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0066, p_S04X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0067, p_S05U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0068, p_S05U02Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0069, p_S05U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0070, p_S05X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0071, p_S05X00Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0072, p_S05X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0073, p_S05X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0074, p_S05X01Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0075, p_S05X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0076, p_S05X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0077, p_S05X03Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0078, p_S05X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0079, p_S06U02Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0080, p_S06U02Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0081, p_S06U02Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0082, p_S06X00Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0083, p_S06X00Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0084, p_S06X00Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0085, p_S06X01Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0086, p_S06X01Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0087, p_S06X01Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0088, p_S06X03Y03_a02)
         CALL df_AERO%SETi('PTRaa-02', 0089, p_S06X03Y06_a02)
         CALL df_AERO%SETi('PTRaa-02', 0090, p_S06X03Y20_a02)
         CALL df_AERO%SETi('PTRaa-02', 0091, p_S07U02Y00_a02)
         CALL df_AERO%SETi('PTRaa-02', 0092, p_S07U02Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0093, p_S07U02Y10_a02)
         CALL df_AERO%SETi('PTRaa-02', 0094, p_S07X00Y00_a02)
         CALL df_AERO%SETi('PTRaa-02', 0095, p_S07X00Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0096, p_S07X00Y10_a02)
         CALL df_AERO%SETi('PTRaa-02', 0097, p_S07X01Y00_a02)
         CALL df_AERO%SETi('PTRaa-02', 0098, p_S07X01Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0099, p_S07X01Y10_a02)
         CALL df_AERO%SETi('PTRaa-02', 0100, p_S07X03Y00_a02)
         CALL df_AERO%SETi('PTRaa-02', 0101, p_S07X03Y05_a02)
         CALL df_AERO%SETi('PTRaa-02', 0102, p_S07X03Y10_a02)

         CALL df_AERO%SETi('PTRaa-03', 0001, p_SO4_a03)
         CALL df_AERO%SETi('PTRaa-03', 0002, p_NO3_a03)
         CALL df_AERO%SETi('PTRaa-03', 0003, p_CL_a03)
         CALL df_AERO%SETi('PTRaa-03', 0004, p_NH4_a03)
         CALL df_AERO%SETi('PTRaa-03', 0005, p_NA_a03)
         CALL df_AERO%SETi('PTRaa-03', 0006, p_OIN_a03)
         CALL df_AERO%SETi('PTRaa-03', 0007, p_OC_a03)
         CALL df_AERO%SETi('PTRaa-03', 0008, p_BC_a03)
         CALL df_AERO%SETi('PTRaa-03', 0009, p_HYSW_a03)
         CALL df_AERO%SETi('PTRaa-03', 0010, p_WATER_a03)
         CALL df_AERO%SETi('PTRaa-03', 0011, p_CLOUDSOA_a03)
         CALL df_AERO%SETi('PTRaa-03', 0012, p_IEPOX_a03)
         CALL df_AERO%SETi('PTRaa-03', 0013, p_IEPOXOS_a03)
         CALL df_AERO%SETi('PTRaa-03', 0014, p_TETROL_a03)
         CALL df_AERO%SETi('PTRaa-03', 0015, p_GLY_a03)
         CALL df_AERO%SETi('PTRaa-03', 0016, p_P07U03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0017, p_P07X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0018, p_P07X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0019, p_S01U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0020, p_S01U02Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0021, p_S01U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0022, p_S01X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0023, p_S01X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0024, p_S01X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0025, p_S01X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0026, p_S01X01Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0027, p_S01X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0028, p_S01X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0029, p_S01X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0030, p_S01X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0031, p_S02U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0032, p_S02U02Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0033, p_S02U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0034, p_S02X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0035, p_S02X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0036, p_S02X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0037, p_S02X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0038, p_S02X01Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0039, p_S02X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0040, p_S02X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0041, p_S02X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0042, p_S02X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0043, p_S03U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0044, p_S03U02Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0045, p_S03U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0046, p_S03X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0047, p_S03X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0048, p_S03X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0049, p_S03X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0050, p_S03X01Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0051, p_S03X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0052, p_S03X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0053, p_S03X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0054, p_S03X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0055, p_S04U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0056, p_S04U02Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0057, p_S04U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0058, p_S04X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0059, p_S04X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0060, p_S04X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0061, p_S04X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0062, p_S04X01Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0063, p_S04X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0064, p_S04X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0065, p_S04X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0066, p_S04X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0067, p_S05U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0068, p_S05U02Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0069, p_S05U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0070, p_S05X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0071, p_S05X00Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0072, p_S05X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0073, p_S05X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0074, p_S05X01Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0075, p_S05X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0076, p_S05X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0077, p_S05X03Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0078, p_S05X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0079, p_S06U02Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0080, p_S06U02Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0081, p_S06U02Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0082, p_S06X00Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0083, p_S06X00Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0084, p_S06X00Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0085, p_S06X01Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0086, p_S06X01Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0087, p_S06X01Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0088, p_S06X03Y03_a03)
         CALL df_AERO%SETi('PTRaa-03', 0089, p_S06X03Y06_a03)
         CALL df_AERO%SETi('PTRaa-03', 0090, p_S06X03Y20_a03)
         CALL df_AERO%SETi('PTRaa-03', 0091, p_S07U02Y00_a03)
         CALL df_AERO%SETi('PTRaa-03', 0092, p_S07U02Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0093, p_S07U02Y10_a03)
         CALL df_AERO%SETi('PTRaa-03', 0094, p_S07X00Y00_a03)
         CALL df_AERO%SETi('PTRaa-03', 0095, p_S07X00Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0096, p_S07X00Y10_a03)
         CALL df_AERO%SETi('PTRaa-03', 0097, p_S07X01Y00_a03)
         CALL df_AERO%SETi('PTRaa-03', 0098, p_S07X01Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0099, p_S07X01Y10_a03)
         CALL df_AERO%SETi('PTRaa-03', 0100, p_S07X03Y00_a03)
         CALL df_AERO%SETi('PTRaa-03', 0101, p_S07X03Y05_a03)
         CALL df_AERO%SETi('PTRaa-03', 0102, p_S07X03Y10_a03)

         CALL df_AERO%SETi('PTRaa-04', 0001, p_SO4_a04)
         CALL df_AERO%SETi('PTRaa-04', 0002, p_NO3_a04)
         CALL df_AERO%SETi('PTRaa-04', 0003, p_CL_a04)
         CALL df_AERO%SETi('PTRaa-04', 0004, p_NH4_a04)
         CALL df_AERO%SETi('PTRaa-04', 0005, p_NA_a04)
         CALL df_AERO%SETi('PTRaa-04', 0006, p_OIN_a04)
         CALL df_AERO%SETi('PTRaa-04', 0007, p_OC_a04)
         CALL df_AERO%SETi('PTRaa-04', 0008, p_BC_a04)
         CALL df_AERO%SETi('PTRaa-04', 0009, p_HYSW_a04)
         CALL df_AERO%SETi('PTRaa-04', 0010, p_WATER_a04)
         CALL df_AERO%SETi('PTRaa-04', 0011, p_CLOUDSOA_a04)
         CALL df_AERO%SETi('PTRaa-04', 0012, p_IEPOX_a04)
         CALL df_AERO%SETi('PTRaa-04', 0013, p_IEPOXOS_a04)
         CALL df_AERO%SETi('PTRaa-04', 0014, p_TETROL_a04)
         CALL df_AERO%SETi('PTRaa-04', 0015, p_GLY_a04)
         CALL df_AERO%SETi('PTRaa-04', 0016, p_P07U03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0017, p_P07X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0018, p_P07X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0019, p_S01U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0020, p_S01U02Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0021, p_S01U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0022, p_S01X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0023, p_S01X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0024, p_S01X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0025, p_S01X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0026, p_S01X01Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0027, p_S01X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0028, p_S01X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0029, p_S01X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0030, p_S01X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0031, p_S02U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0032, p_S02U02Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0033, p_S02U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0034, p_S02X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0035, p_S02X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0036, p_S02X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0037, p_S02X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0038, p_S02X01Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0039, p_S02X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0040, p_S02X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0041, p_S02X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0042, p_S02X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0043, p_S03U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0044, p_S03U02Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0045, p_S03U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0046, p_S03X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0047, p_S03X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0048, p_S03X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0049, p_S03X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0050, p_S03X01Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0051, p_S03X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0052, p_S03X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0053, p_S03X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0054, p_S03X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0055, p_S04U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0056, p_S04U02Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0057, p_S04U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0058, p_S04X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0059, p_S04X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0060, p_S04X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0061, p_S04X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0062, p_S04X01Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0063, p_S04X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0064, p_S04X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0065, p_S04X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0066, p_S04X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0067, p_S05U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0068, p_S05U02Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0069, p_S05U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0070, p_S05X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0071, p_S05X00Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0072, p_S05X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0073, p_S05X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0074, p_S05X01Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0075, p_S05X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0076, p_S05X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0077, p_S05X03Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0078, p_S05X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0079, p_S06U02Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0080, p_S06U02Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0081, p_S06U02Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0082, p_S06X00Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0083, p_S06X00Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0084, p_S06X00Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0085, p_S06X01Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0086, p_S06X01Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0087, p_S06X01Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0088, p_S06X03Y03_a04)
         CALL df_AERO%SETi('PTRaa-04', 0089, p_S06X03Y06_a04)
         CALL df_AERO%SETi('PTRaa-04', 0090, p_S06X03Y20_a04)
         CALL df_AERO%SETi('PTRaa-04', 0091, p_S07U02Y00_a04)
         CALL df_AERO%SETi('PTRaa-04', 0092, p_S07U02Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0093, p_S07U02Y10_a04)
         CALL df_AERO%SETi('PTRaa-04', 0094, p_S07X00Y00_a04)
         CALL df_AERO%SETi('PTRaa-04', 0095, p_S07X00Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0096, p_S07X00Y10_a04)
         CALL df_AERO%SETi('PTRaa-04', 0097, p_S07X01Y00_a04)
         CALL df_AERO%SETi('PTRaa-04', 0098, p_S07X01Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0099, p_S07X01Y10_a04)
         CALL df_AERO%SETi('PTRaa-04', 0100, p_S07X03Y00_a04)
         CALL df_AERO%SETi('PTRaa-04', 0101, p_S07X03Y05_a04)
         CALL df_AERO%SETi('PTRaa-04', 0102, p_S07X03Y10_a04)

!        SET COLUMN: df_AERO; CLOUD-PHASE POINTERS
!        ===============================================================
         CALL df_AERO%SETi('PTRcw-01', 0001, p_SO4_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0002, p_NO3_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0003, p_CL_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0004, p_NH4_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0005, p_NA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0006, p_OIN_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0007, p_OC_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0008, p_BC_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0009, 1)
         CALL df_AERO%SETi('PTRcw-01', 0010, 1)
         CALL df_AERO%SETi('PTRcw-01', 0011, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0012, p_IEPOX_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0013, p_IEPOXOS_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0014, p_TETROL_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0015, p_GLY_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0016, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0017, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0018, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0019, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0020, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0021, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0022, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0023, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0024, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0025, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0026, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0027, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0028, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0029, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0030, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0031, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0032, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0033, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0034, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0035, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0036, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0037, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0038, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0039, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0040, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0041, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0042, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0043, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0044, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0045, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0046, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0047, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0048, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0049, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0050, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0051, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0052, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0053, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0054, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0055, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0056, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0057, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0058, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0059, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0060, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0061, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0062, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0063, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0064, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0065, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0066, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0067, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0068, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0069, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0070, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0071, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0072, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0073, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0074, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0075, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0076, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0077, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0078, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0079, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0080, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0081, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0082, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0083, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0084, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0085, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0086, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0087, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0088, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0089, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0090, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0091, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0092, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0093, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0094, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0095, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0096, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0097, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0098, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0099, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0100, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0101, p_CLOUDSOA_cw01)
         CALL df_AERO%SETi('PTRcw-01', 0102, p_CLOUDSOA_cw01)

         CALL df_AERO%SETi('PTRcw-02', 0001, p_SO4_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0002, p_NO3_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0003, p_CL_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0004, p_NH4_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0005, p_NA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0006, p_OIN_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0007, p_OC_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0008, p_BC_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0009, 1)
         CALL df_AERO%SETi('PTRcw-02', 0010, 1)
         CALL df_AERO%SETi('PTRcw-02', 0011, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0012, p_IEPOX_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0013, p_IEPOXOS_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0014, p_TETROL_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0015, p_GLY_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0016, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0017, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0018, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0019, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0020, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0021, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0022, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0023, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0024, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0025, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0026, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0027, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0028, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0029, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0030, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0031, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0032, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0033, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0034, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0035, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0036, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0037, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0038, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0039, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0040, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0041, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0042, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0043, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0044, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0045, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0046, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0047, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0048, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0049, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0050, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0051, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0052, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0053, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0054, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0055, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0056, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0057, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0058, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0059, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0060, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0061, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0062, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0063, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0064, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0065, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0066, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0067, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0068, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0069, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0070, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0071, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0072, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0073, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0074, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0075, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0076, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0077, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0078, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0079, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0080, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0081, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0082, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0083, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0084, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0085, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0086, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0087, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0088, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0089, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0090, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0091, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0092, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0093, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0094, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0095, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0096, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0097, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0098, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0099, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0100, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0101, p_CLOUDSOA_cw02)
         CALL df_AERO%SETi('PTRcw-02', 0102, p_CLOUDSOA_cw02)

         CALL df_AERO%SETi('PTRcw-03', 0001, p_SO4_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0002, p_NO3_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0003, p_CL_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0004, p_NH4_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0005, p_NA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0006, p_OIN_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0007, p_OC_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0008, p_BC_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0009, 1)
         CALL df_AERO%SETi('PTRcw-03', 0010, 1)
         CALL df_AERO%SETi('PTRcw-03', 0011, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0012, p_IEPOX_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0013, p_IEPOXOS_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0014, p_TETROL_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0015, p_GLY_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0016, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0017, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0018, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0019, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0020, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0021, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0022, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0023, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0024, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0025, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0026, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0027, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0028, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0029, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0030, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0031, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0032, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0033, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0034, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0035, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0036, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0037, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0038, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0039, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0040, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0041, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0042, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0043, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0044, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0045, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0046, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0047, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0048, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0049, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0050, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0051, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0052, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0053, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0054, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0055, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0056, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0057, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0058, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0059, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0060, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0061, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0062, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0063, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0064, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0065, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0066, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0067, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0068, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0069, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0070, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0071, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0072, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0073, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0074, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0075, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0076, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0077, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0078, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0079, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0080, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0081, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0082, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0083, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0084, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0085, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0086, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0087, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0088, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0089, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0090, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0091, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0092, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0093, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0094, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0095, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0096, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0097, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0098, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0099, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0100, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0101, p_CLOUDSOA_cw03)
         CALL df_AERO%SETi('PTRcw-03', 0102, p_CLOUDSOA_cw03)

         CALL df_AERO%SETi('PTRcw-04', 0001, p_SO4_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0002, p_NO3_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0003, p_CL_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0004, p_NH4_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0005, p_NA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0006, p_OIN_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0007, p_OC_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0008, p_BC_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0009, 1)
         CALL df_AERO%SETi('PTRcw-04', 0010, 1)
         CALL df_AERO%SETi('PTRcw-04', 0011, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0012, p_IEPOX_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0013, p_IEPOXOS_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0014, p_TETROL_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0015, p_GLY_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0016, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0017, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0018, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0019, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0020, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0021, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0022, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0023, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0024, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0025, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0026, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0027, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0028, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0029, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0030, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0031, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0032, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0033, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0034, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0035, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0036, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0037, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0038, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0039, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0040, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0041, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0042, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0043, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0044, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0045, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0046, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0047, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0048, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0049, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0050, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0051, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0052, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0053, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0054, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0055, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0056, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0057, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0058, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0059, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0060, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0061, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0062, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0063, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0064, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0065, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0066, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0067, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0068, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0069, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0070, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0071, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0072, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0073, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0074, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0075, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0076, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0077, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0078, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0079, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0080, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0081, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0082, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0083, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0084, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0085, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0086, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0087, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0088, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0089, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0090, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0091, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0092, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0093, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0094, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0095, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0096, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0097, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0098, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0099, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0100, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0101, p_CLOUDSOA_cw04)
         CALL df_AERO%SETi('PTRcw-04', 0102, p_CLOUDSOA_cw04)

!        INITIALZE DATAFRAME: df_BINS
!        ===============================================================
!        INITIALIZE:
         CALL df_BINS%new()

!        CREATE COLUMNS:
         CALL df_BINS%APPEND_EMPTYi(nBINS, 'PTRnn')
         CALL df_BINS%APPEND_EMPTYi(nBINS, 'PTRcw')

!        CREATE COLUMNS: EMISSION FRACTIONS
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-01') ! << SORGAM-i
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-02') ! << SORGAM-j
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-03') ! << SORGAM-c
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-04') ! << MOSAIC-f
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-05') ! << MOSAIC-c 
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-06') ! << MOSAIC-BB-f
         CALL df_BINS%APPEND_EMPTYr(nBINS, 'EM-07') ! << MOSAIC-BB-c
         
!        SET COLUMN: df_BINS; BIN DIAMETERS [nm]
!        ===============================================================
         CALL df_BINS%APPEND(bCENTR, 'CENTRd')
         CALL df_BINS%APPEND(bUPPER, 'UPPERd')
         CALL df_BINS%APPEND(bLOWER, 'LOWERd')

!        SET COLUMN: df_BINS; BIN VOLUMES [um3]
!        ===============================================================
         CALL df_BINS%APPEND(bCENTRv, 'CENTRv')
         CALL df_BINS%APPEND(bUPPERv, 'UPPERv')
         CALL df_BINS%APPEND(bLOWERv, 'LOWERv')

!        SET COLUMN: df_BINS; AEROSOL-PHASE POINTERS
!        ===============================================================
         CALL df_BINS%SETi('PTRnn', 0001, p_NUM_a01)
         CALL df_BINS%SETi('PTRnn', 0002, p_NUM_a02)
         CALL df_BINS%SETi('PTRnn', 0003, p_NUM_a03)
         CALL df_BINS%SETi('PTRnn', 0004, p_NUM_a04)

!        SET COLUMN: df_BINS; CLOUD-PHASE POINTERS
!        ===============================================================
         CALL df_BINS%SETi('PTRcw', 0001, p_NUM_cw01)
         CALL df_BINS%SETi('PTRcw', 0002, p_NUM_cw02)
         CALL df_BINS%SETi('PTRcw', 0003, p_NUM_cw03)
         CALL df_BINS%SETi('PTRcw', 0004, p_NUM_cw04)

!        SET COLUMN: df_BINS; EMISSION FRACTIONS
!        ===============================================================
         CALL df_BINS%SETr('EM-01', 0001, 0.2499d0)
         CALL df_BINS%SETr('EM-01', 0002, 0.6490d0)
         CALL df_BINS%SETr('EM-01', 0003, 0.0838d0)
         CALL df_BINS%SETr('EM-01', 0004, 0.0002d0)
 
         CALL df_BINS%SETr('EM-02', 0001, 0.2499d0)
         CALL df_BINS%SETr('EM-02', 0002, 0.6490d0)
         CALL df_BINS%SETr('EM-02', 0003, 0.0838d0)
         CALL df_BINS%SETr('EM-02', 0004, 0.0002d0)
 
         CALL df_BINS%SETr('EM-03', 0001, 0.0000d0)
         CALL df_BINS%SETr('EM-03', 0002, 0.0042d0)
         CALL df_BINS%SETr('EM-03', 0003, 0.1701d0)
         CALL df_BINS%SETr('EM-03', 0004, 0.7932d0)
 
         CALL df_BINS%SETr('EM-04', 0001, 0.1221d0)
         CALL df_BINS%SETr('EM-04', 0002, 0.5419d0)
         CALL df_BINS%SETr('EM-04', 0003, 0.3014d0)
         CALL df_BINS%SETr('EM-04', 0004, 0.0244d0)
 
         CALL df_BINS%SETr('EM-05', 0001, 0.0000d0)
         CALL df_BINS%SETr('EM-05', 0002, 0.0000d0)
         CALL df_BINS%SETr('EM-05', 0003, 0.0539d0)
         CALL df_BINS%SETr('EM-05', 0004, 0.9091d0)
 
         CALL df_BINS%SETr('EM-06', 0001, 0.0724d0)
         CALL df_BINS%SETr('EM-06', 0002, 0.6516d0)
         CALL df_BINS%SETr('EM-06', 0003, 0.2707d0)
         CALL df_BINS%SETr('EM-06', 0004, 0.0043d0)
 
         CALL df_BINS%SETr('EM-07', 0001, 0.0000d0)
         CALL df_BINS%SETr('EM-07', 0002, 0.0000d0)
         CALL df_BINS%SETr('EM-07', 0003, 0.0004d0)
         CALL df_BINS%SETr('EM-07', 0004, 0.0005d0)
 

!        INITIALZE DATAFRAME: df_KNTOA
!        ===============================================================
!        INITIALIZE:
         CALL df_KNTOA%new()

!        CREATE COLUMNS:
         CALL df_KNTOA%APPEND_EMPTYh(nKNTOA, 'NAMES')
         CALL df_KNTOA%APPEND_EMPTYi(nKNTOA, 'PTRqq')
         CALL df_KNTOA%APPEND_EMPTYr(nKNTOA, 'MW')
         CALL df_KNTOA%APPEND_EMPTYr(nKNTOA, 'CSAT')

!        CREATE COLUMNS:
         CALL df_KNTOA%APPEND_EMPTYi(nKNTOA, 'PTRaa-01')
         CALL df_KNTOA%APPEND_EMPTYi(nKNTOA, 'PTRaa-02')
         CALL df_KNTOA%APPEND_EMPTYi(nKNTOA, 'PTRaa-03')
         CALL df_KNTOA%APPEND_EMPTYi(nKNTOA, 'PTRaa-04')

!        SET COLUMN: df_KNTOA; SPECIES NAMES
!        ===============================================================
         CALL df_KNTOA%SETh('NAMES', 0001, 'P07U03Y06')
         CALL df_KNTOA%SETh('NAMES', 0002, 'P07X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0003, 'P07X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0004, 'S01U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0005, 'S01U02Y06')
         CALL df_KNTOA%SETh('NAMES', 0006, 'S01U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0007, 'S01X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0008, 'S01X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0009, 'S01X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0010, 'S01X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0011, 'S01X01Y06')
         CALL df_KNTOA%SETh('NAMES', 0012, 'S01X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0013, 'S01X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0014, 'S01X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0015, 'S01X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0016, 'S02U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0017, 'S02U02Y06')
         CALL df_KNTOA%SETh('NAMES', 0018, 'S02U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0019, 'S02X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0020, 'S02X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0021, 'S02X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0022, 'S02X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0023, 'S02X01Y06')
         CALL df_KNTOA%SETh('NAMES', 0024, 'S02X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0025, 'S02X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0026, 'S02X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0027, 'S02X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0028, 'S03U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0029, 'S03U02Y06')
         CALL df_KNTOA%SETh('NAMES', 0030, 'S03U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0031, 'S03X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0032, 'S03X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0033, 'S03X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0034, 'S03X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0035, 'S03X01Y06')
         CALL df_KNTOA%SETh('NAMES', 0036, 'S03X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0037, 'S03X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0038, 'S03X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0039, 'S03X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0040, 'S04U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0041, 'S04U02Y06')
         CALL df_KNTOA%SETh('NAMES', 0042, 'S04U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0043, 'S04X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0044, 'S04X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0045, 'S04X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0046, 'S04X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0047, 'S04X01Y06')
         CALL df_KNTOA%SETh('NAMES', 0048, 'S04X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0049, 'S04X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0050, 'S04X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0051, 'S04X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0052, 'S05U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0053, 'S05U02Y05')
         CALL df_KNTOA%SETh('NAMES', 0054, 'S05U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0055, 'S05X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0056, 'S05X00Y05')
         CALL df_KNTOA%SETh('NAMES', 0057, 'S05X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0058, 'S05X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0059, 'S05X01Y05')
         CALL df_KNTOA%SETh('NAMES', 0060, 'S05X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0061, 'S05X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0062, 'S05X03Y05')
         CALL df_KNTOA%SETh('NAMES', 0063, 'S05X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0064, 'S06U02Y03')
         CALL df_KNTOA%SETh('NAMES', 0065, 'S06U02Y06')
         CALL df_KNTOA%SETh('NAMES', 0066, 'S06U02Y20')
         CALL df_KNTOA%SETh('NAMES', 0067, 'S06X00Y03')
         CALL df_KNTOA%SETh('NAMES', 0068, 'S06X00Y06')
         CALL df_KNTOA%SETh('NAMES', 0069, 'S06X00Y20')
         CALL df_KNTOA%SETh('NAMES', 0070, 'S06X01Y03')
         CALL df_KNTOA%SETh('NAMES', 0071, 'S06X01Y06')
         CALL df_KNTOA%SETh('NAMES', 0072, 'S06X01Y20')
         CALL df_KNTOA%SETh('NAMES', 0073, 'S06X03Y03')
         CALL df_KNTOA%SETh('NAMES', 0074, 'S06X03Y06')
         CALL df_KNTOA%SETh('NAMES', 0075, 'S06X03Y20')
         CALL df_KNTOA%SETh('NAMES', 0076, 'S07U02Y00')
         CALL df_KNTOA%SETh('NAMES', 0077, 'S07U02Y05')
         CALL df_KNTOA%SETh('NAMES', 0078, 'S07U02Y10')
         CALL df_KNTOA%SETh('NAMES', 0079, 'S07X00Y00')
         CALL df_KNTOA%SETh('NAMES', 0080, 'S07X00Y05')
         CALL df_KNTOA%SETh('NAMES', 0081, 'S07X00Y10')
         CALL df_KNTOA%SETh('NAMES', 0082, 'S07X01Y00')
         CALL df_KNTOA%SETh('NAMES', 0083, 'S07X01Y05')
         CALL df_KNTOA%SETh('NAMES', 0084, 'S07X01Y10')
         CALL df_KNTOA%SETh('NAMES', 0085, 'S07X03Y00')
         CALL df_KNTOA%SETh('NAMES', 0086, 'S07X03Y05')
         CALL df_KNTOA%SETh('NAMES', 0087, 'S07X03Y10')

!        SET COLUMN: df_KNTOA; POINTERS TO GAS-PHASE SPECIES
!        ===============================================================
         CALL df_KNTOA%SETi('PTRqq', 0001, p_P07U03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0002, p_P07X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0003, p_P07X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0004, p_S01U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0005, p_S01U02Y06)
         CALL df_KNTOA%SETi('PTRqq', 0006, p_S01U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0007, p_S01X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0008, p_S01X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0009, p_S01X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0010, p_S01X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0011, p_S01X01Y06)
         CALL df_KNTOA%SETi('PTRqq', 0012, p_S01X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0013, p_S01X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0014, p_S01X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0015, p_S01X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0016, p_S02U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0017, p_S02U02Y06)
         CALL df_KNTOA%SETi('PTRqq', 0018, p_S02U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0019, p_S02X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0020, p_S02X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0021, p_S02X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0022, p_S02X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0023, p_S02X01Y06)
         CALL df_KNTOA%SETi('PTRqq', 0024, p_S02X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0025, p_S02X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0026, p_S02X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0027, p_S02X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0028, p_S03U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0029, p_S03U02Y06)
         CALL df_KNTOA%SETi('PTRqq', 0030, p_S03U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0031, p_S03X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0032, p_S03X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0033, p_S03X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0034, p_S03X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0035, p_S03X01Y06)
         CALL df_KNTOA%SETi('PTRqq', 0036, p_S03X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0037, p_S03X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0038, p_S03X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0039, p_S03X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0040, p_S04U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0041, p_S04U02Y06)
         CALL df_KNTOA%SETi('PTRqq', 0042, p_S04U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0043, p_S04X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0044, p_S04X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0045, p_S04X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0046, p_S04X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0047, p_S04X01Y06)
         CALL df_KNTOA%SETi('PTRqq', 0048, p_S04X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0049, p_S04X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0050, p_S04X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0051, p_S04X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0052, p_S05U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0053, p_S05U02Y05)
         CALL df_KNTOA%SETi('PTRqq', 0054, p_S05U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0055, p_S05X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0056, p_S05X00Y05)
         CALL df_KNTOA%SETi('PTRqq', 0057, p_S05X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0058, p_S05X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0059, p_S05X01Y05)
         CALL df_KNTOA%SETi('PTRqq', 0060, p_S05X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0061, p_S05X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0062, p_S05X03Y05)
         CALL df_KNTOA%SETi('PTRqq', 0063, p_S05X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0064, p_S06U02Y03)
         CALL df_KNTOA%SETi('PTRqq', 0065, p_S06U02Y06)
         CALL df_KNTOA%SETi('PTRqq', 0066, p_S06U02Y20)
         CALL df_KNTOA%SETi('PTRqq', 0067, p_S06X00Y03)
         CALL df_KNTOA%SETi('PTRqq', 0068, p_S06X00Y06)
         CALL df_KNTOA%SETi('PTRqq', 0069, p_S06X00Y20)
         CALL df_KNTOA%SETi('PTRqq', 0070, p_S06X01Y03)
         CALL df_KNTOA%SETi('PTRqq', 0071, p_S06X01Y06)
         CALL df_KNTOA%SETi('PTRqq', 0072, p_S06X01Y20)
         CALL df_KNTOA%SETi('PTRqq', 0073, p_S06X03Y03)
         CALL df_KNTOA%SETi('PTRqq', 0074, p_S06X03Y06)
         CALL df_KNTOA%SETi('PTRqq', 0075, p_S06X03Y20)
         CALL df_KNTOA%SETi('PTRqq', 0076, p_S07U02Y00)
         CALL df_KNTOA%SETi('PTRqq', 0077, p_S07U02Y05)
         CALL df_KNTOA%SETi('PTRqq', 0078, p_S07U02Y10)
         CALL df_KNTOA%SETi('PTRqq', 0079, p_S07X00Y00)
         CALL df_KNTOA%SETi('PTRqq', 0080, p_S07X00Y05)
         CALL df_KNTOA%SETi('PTRqq', 0081, p_S07X00Y10)
         CALL df_KNTOA%SETi('PTRqq', 0082, p_S07X01Y00)
         CALL df_KNTOA%SETi('PTRqq', 0083, p_S07X01Y05)
         CALL df_KNTOA%SETi('PTRqq', 0084, p_S07X01Y10)
         CALL df_KNTOA%SETi('PTRqq', 0085, p_S07X03Y00)
         CALL df_KNTOA%SETi('PTRqq', 0086, p_S07X03Y05)
         CALL df_KNTOA%SETi('PTRqq', 0087, p_S07X03Y10)

!        SET COLUMN: df_KNTOA; VOLATILITY
!        ===============================================================
         CALL df_KNTOA%SETr('CSAT', 0001, 1.000d-03)
         CALL df_KNTOA%SETr('CSAT', 0002, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0003, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0004, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0005, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0006, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0007, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0008, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0009, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0010, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0011, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0012, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0013, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0014, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0015, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0016, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0017, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0018, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0019, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0020, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0021, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0022, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0023, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0024, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0025, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0026, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0027, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0028, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0029, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0030, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0031, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0032, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0033, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0034, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0035, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0036, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0037, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0038, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0039, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0040, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0041, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0042, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0043, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0044, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0045, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0046, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0047, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0048, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0049, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0050, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0051, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0052, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0053, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0054, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0055, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0056, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0057, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0058, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0059, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0060, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0061, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0062, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0063, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0064, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0065, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0066, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0067, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0068, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0069, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0070, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0071, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0072, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0073, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0074, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0075, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0076, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0077, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0078, 1.000d-02)
         CALL df_KNTOA%SETr('CSAT', 0079, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0080, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0081, 1.000d+00)
         CALL df_KNTOA%SETr('CSAT', 0082, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0083, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0084, 1.000d+01)
         CALL df_KNTOA%SETr('CSAT', 0085, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0086, 1.000d+03)
         CALL df_KNTOA%SETr('CSAT', 0087, 1.000d+03)

!        SET COLUMN: df_KNTOA; MOLECULAR WEIGHT
!        ===============================================================
         CALL df_KNTOA%SETr('MW', 0001, 202.30d0)
         CALL df_KNTOA%SETr('MW', 0002, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0003, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0004, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0005, 189.24d0)
         CALL df_KNTOA%SETr('MW', 0006, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0007, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0008, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0009, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0010, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0011, 150.09d0)
         CALL df_KNTOA%SETr('MW', 0012, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0013, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0014, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0015, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0016, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0017, 189.24d0)
         CALL df_KNTOA%SETr('MW', 0018, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0019, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0020, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0021, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0022, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0023, 150.09d0)
         CALL df_KNTOA%SETr('MW', 0024, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0025, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0026, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0027, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0028, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0029, 189.24d0)
         CALL df_KNTOA%SETr('MW', 0030, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0031, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0032, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0033, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0034, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0035, 150.09d0)
         CALL df_KNTOA%SETr('MW', 0036, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0037, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0038, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0039, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0040, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0041, 189.24d0)
         CALL df_KNTOA%SETr('MW', 0042, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0043, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0044, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0045, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0046, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0047, 150.09d0)
         CALL df_KNTOA%SETr('MW', 0048, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0049, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0050, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0051, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0052, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0053, 200.00d0)
         CALL df_KNTOA%SETr('MW', 0054, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0055, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0056, 172.41d0)
         CALL df_KNTOA%SETr('MW', 0057, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0058, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0059, 158.62d0)
         CALL df_KNTOA%SETr('MW', 0060, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0061, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0062, 131.03d0)
         CALL df_KNTOA%SETr('MW', 0063, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0064, 231.66d0)
         CALL df_KNTOA%SETr('MW', 0065, 189.24d0)
         CALL df_KNTOA%SETr('MW', 0066, 135.74d0)
         CALL df_KNTOA%SETr('MW', 0067, 199.71d0)
         CALL df_KNTOA%SETr('MW', 0068, 163.14d0)
         CALL df_KNTOA%SETr('MW', 0069, 117.02d0)
         CALL df_KNTOA%SETr('MW', 0070, 183.73d0)
         CALL df_KNTOA%SETr('MW', 0071, 150.09d0)
         CALL df_KNTOA%SETr('MW', 0072, 107.66d0)
         CALL df_KNTOA%SETr('MW', 0073, 151.78d0)
         CALL df_KNTOA%SETr('MW', 0074, 123.99d0)
         CALL df_KNTOA%SETr('MW', 0075, 88.94d0)
         CALL df_KNTOA%SETr('MW', 0076, 348.00d0)
         CALL df_KNTOA%SETr('MW', 0077, 200.00d0)
         CALL df_KNTOA%SETr('MW', 0078, 162.40d0)
         CALL df_KNTOA%SETr('MW', 0079, 300.00d0)
         CALL df_KNTOA%SETr('MW', 0080, 172.41d0)
         CALL df_KNTOA%SETr('MW', 0081, 140.00d0)
         CALL df_KNTOA%SETr('MW', 0082, 276.00d0)
         CALL df_KNTOA%SETr('MW', 0083, 158.62d0)
         CALL df_KNTOA%SETr('MW', 0084, 128.80d0)
         CALL df_KNTOA%SETr('MW', 0085, 228.00d0)
         CALL df_KNTOA%SETr('MW', 0086, 131.03d0)
         CALL df_KNTOA%SETr('MW', 0087, 106.40d0)

!        SET COLUMN: df_KNTOA; POINTERS TO AEROSOL-PHASE SPECIES
!        ===============================================================
         CALL df_KNTOA%SETi('PTRaa-01', 0001, p_P07U03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0001, p_P07U03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0001, p_P07U03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0001, p_P07U03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0002, p_P07X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0002, p_P07X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0002, p_P07X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0002, p_P07X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0003, p_P07X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0003, p_P07X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0003, p_P07X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0003, p_P07X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0004, p_S01U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0004, p_S01U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0004, p_S01U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0004, p_S01U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0005, p_S01U02Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0005, p_S01U02Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0005, p_S01U02Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0005, p_S01U02Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0006, p_S01U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0006, p_S01U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0006, p_S01U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0006, p_S01U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0007, p_S01X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0007, p_S01X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0007, p_S01X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0007, p_S01X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0008, p_S01X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0008, p_S01X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0008, p_S01X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0008, p_S01X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0009, p_S01X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0009, p_S01X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0009, p_S01X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0009, p_S01X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0010, p_S01X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0010, p_S01X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0010, p_S01X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0010, p_S01X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0011, p_S01X01Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0011, p_S01X01Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0011, p_S01X01Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0011, p_S01X01Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0012, p_S01X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0012, p_S01X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0012, p_S01X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0012, p_S01X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0013, p_S01X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0013, p_S01X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0013, p_S01X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0013, p_S01X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0014, p_S01X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0014, p_S01X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0014, p_S01X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0014, p_S01X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0015, p_S01X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0015, p_S01X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0015, p_S01X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0015, p_S01X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0016, p_S02U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0016, p_S02U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0016, p_S02U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0016, p_S02U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0017, p_S02U02Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0017, p_S02U02Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0017, p_S02U02Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0017, p_S02U02Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0018, p_S02U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0018, p_S02U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0018, p_S02U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0018, p_S02U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0019, p_S02X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0019, p_S02X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0019, p_S02X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0019, p_S02X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0020, p_S02X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0020, p_S02X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0020, p_S02X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0020, p_S02X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0021, p_S02X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0021, p_S02X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0021, p_S02X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0021, p_S02X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0022, p_S02X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0022, p_S02X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0022, p_S02X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0022, p_S02X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0023, p_S02X01Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0023, p_S02X01Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0023, p_S02X01Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0023, p_S02X01Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0024, p_S02X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0024, p_S02X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0024, p_S02X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0024, p_S02X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0025, p_S02X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0025, p_S02X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0025, p_S02X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0025, p_S02X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0026, p_S02X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0026, p_S02X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0026, p_S02X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0026, p_S02X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0027, p_S02X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0027, p_S02X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0027, p_S02X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0027, p_S02X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0028, p_S03U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0028, p_S03U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0028, p_S03U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0028, p_S03U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0029, p_S03U02Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0029, p_S03U02Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0029, p_S03U02Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0029, p_S03U02Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0030, p_S03U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0030, p_S03U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0030, p_S03U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0030, p_S03U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0031, p_S03X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0031, p_S03X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0031, p_S03X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0031, p_S03X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0032, p_S03X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0032, p_S03X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0032, p_S03X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0032, p_S03X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0033, p_S03X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0033, p_S03X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0033, p_S03X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0033, p_S03X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0034, p_S03X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0034, p_S03X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0034, p_S03X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0034, p_S03X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0035, p_S03X01Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0035, p_S03X01Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0035, p_S03X01Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0035, p_S03X01Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0036, p_S03X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0036, p_S03X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0036, p_S03X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0036, p_S03X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0037, p_S03X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0037, p_S03X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0037, p_S03X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0037, p_S03X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0038, p_S03X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0038, p_S03X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0038, p_S03X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0038, p_S03X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0039, p_S03X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0039, p_S03X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0039, p_S03X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0039, p_S03X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0040, p_S04U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0040, p_S04U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0040, p_S04U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0040, p_S04U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0041, p_S04U02Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0041, p_S04U02Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0041, p_S04U02Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0041, p_S04U02Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0042, p_S04U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0042, p_S04U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0042, p_S04U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0042, p_S04U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0043, p_S04X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0043, p_S04X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0043, p_S04X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0043, p_S04X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0044, p_S04X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0044, p_S04X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0044, p_S04X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0044, p_S04X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0045, p_S04X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0045, p_S04X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0045, p_S04X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0045, p_S04X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0046, p_S04X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0046, p_S04X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0046, p_S04X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0046, p_S04X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0047, p_S04X01Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0047, p_S04X01Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0047, p_S04X01Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0047, p_S04X01Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0048, p_S04X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0048, p_S04X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0048, p_S04X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0048, p_S04X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0049, p_S04X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0049, p_S04X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0049, p_S04X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0049, p_S04X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0050, p_S04X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0050, p_S04X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0050, p_S04X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0050, p_S04X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0051, p_S04X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0051, p_S04X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0051, p_S04X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0051, p_S04X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0052, p_S05U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0052, p_S05U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0052, p_S05U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0052, p_S05U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0053, p_S05U02Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0053, p_S05U02Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0053, p_S05U02Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0053, p_S05U02Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0054, p_S05U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0054, p_S05U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0054, p_S05U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0054, p_S05U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0055, p_S05X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0055, p_S05X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0055, p_S05X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0055, p_S05X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0056, p_S05X00Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0056, p_S05X00Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0056, p_S05X00Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0056, p_S05X00Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0057, p_S05X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0057, p_S05X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0057, p_S05X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0057, p_S05X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0058, p_S05X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0058, p_S05X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0058, p_S05X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0058, p_S05X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0059, p_S05X01Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0059, p_S05X01Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0059, p_S05X01Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0059, p_S05X01Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0060, p_S05X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0060, p_S05X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0060, p_S05X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0060, p_S05X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0061, p_S05X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0061, p_S05X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0061, p_S05X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0061, p_S05X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0062, p_S05X03Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0062, p_S05X03Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0062, p_S05X03Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0062, p_S05X03Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0063, p_S05X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0063, p_S05X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0063, p_S05X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0063, p_S05X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0064, p_S06U02Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0064, p_S06U02Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0064, p_S06U02Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0064, p_S06U02Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0065, p_S06U02Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0065, p_S06U02Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0065, p_S06U02Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0065, p_S06U02Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0066, p_S06U02Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0066, p_S06U02Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0066, p_S06U02Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0066, p_S06U02Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0067, p_S06X00Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0067, p_S06X00Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0067, p_S06X00Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0067, p_S06X00Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0068, p_S06X00Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0068, p_S06X00Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0068, p_S06X00Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0068, p_S06X00Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0069, p_S06X00Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0069, p_S06X00Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0069, p_S06X00Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0069, p_S06X00Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0070, p_S06X01Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0070, p_S06X01Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0070, p_S06X01Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0070, p_S06X01Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0071, p_S06X01Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0071, p_S06X01Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0071, p_S06X01Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0071, p_S06X01Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0072, p_S06X01Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0072, p_S06X01Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0072, p_S06X01Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0072, p_S06X01Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0073, p_S06X03Y03_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0073, p_S06X03Y03_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0073, p_S06X03Y03_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0073, p_S06X03Y03_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0074, p_S06X03Y06_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0074, p_S06X03Y06_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0074, p_S06X03Y06_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0074, p_S06X03Y06_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0075, p_S06X03Y20_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0075, p_S06X03Y20_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0075, p_S06X03Y20_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0075, p_S06X03Y20_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0076, p_S07U02Y00_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0076, p_S07U02Y00_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0076, p_S07U02Y00_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0076, p_S07U02Y00_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0077, p_S07U02Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0077, p_S07U02Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0077, p_S07U02Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0077, p_S07U02Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0078, p_S07U02Y10_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0078, p_S07U02Y10_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0078, p_S07U02Y10_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0078, p_S07U02Y10_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0079, p_S07X00Y00_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0079, p_S07X00Y00_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0079, p_S07X00Y00_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0079, p_S07X00Y00_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0080, p_S07X00Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0080, p_S07X00Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0080, p_S07X00Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0080, p_S07X00Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0081, p_S07X00Y10_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0081, p_S07X00Y10_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0081, p_S07X00Y10_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0081, p_S07X00Y10_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0082, p_S07X01Y00_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0082, p_S07X01Y00_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0082, p_S07X01Y00_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0082, p_S07X01Y00_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0083, p_S07X01Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0083, p_S07X01Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0083, p_S07X01Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0083, p_S07X01Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0084, p_S07X01Y10_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0084, p_S07X01Y10_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0084, p_S07X01Y10_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0084, p_S07X01Y10_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0085, p_S07X03Y00_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0085, p_S07X03Y00_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0085, p_S07X03Y00_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0085, p_S07X03Y00_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0086, p_S07X03Y05_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0086, p_S07X03Y05_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0086, p_S07X03Y05_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0086, p_S07X03Y05_a04)
 
         CALL df_KNTOA%SETi('PTRaa-01', 0087, p_S07X03Y10_a01)
         CALL df_KNTOA%SETi('PTRaa-02', 0087, p_S07X03Y10_a02)
         CALL df_KNTOA%SETi('PTRaa-03', 0087, p_S07X03Y10_a03)
         CALL df_KNTOA%SETi('PTRaa-04', 0087, p_S07X03Y10_a04)
 






!        SET COLUMN (AERO-COL01): SPECIES NAMES
!        ===============================================================
         df_AERO_COL01(0001) = 'SO4'
         df_AERO_COL01(0002) = 'NO3'
         df_AERO_COL01(0003) = 'CL'
         df_AERO_COL01(0004) = 'NH4'
         df_AERO_COL01(0005) = 'NA'
         df_AERO_COL01(0006) = 'OIN'
         df_AERO_COL01(0007) = 'OC'
         df_AERO_COL01(0008) = 'BC'
         df_AERO_COL01(0009) = 'HYSW'
         df_AERO_COL01(0010) = 'WATER'
         df_AERO_COL01(0011) = 'CLOUDSOA'
         df_AERO_COL01(0012) = 'IEPOX'
         df_AERO_COL01(0013) = 'IEPOXOS'
         df_AERO_COL01(0014) = 'TETROL'
         df_AERO_COL01(0015) = 'GLY'
         df_AERO_COL01(0016) = 'P07U03Y06'
         df_AERO_COL01(0017) = 'P07X00Y06'
         df_AERO_COL01(0018) = 'P07X03Y06'
         df_AERO_COL01(0019) = 'S01U02Y03'
         df_AERO_COL01(0020) = 'S01U02Y06'
         df_AERO_COL01(0021) = 'S01U02Y20'
         df_AERO_COL01(0022) = 'S01X00Y03'
         df_AERO_COL01(0023) = 'S01X00Y06'
         df_AERO_COL01(0024) = 'S01X00Y20'
         df_AERO_COL01(0025) = 'S01X01Y03'
         df_AERO_COL01(0026) = 'S01X01Y06'
         df_AERO_COL01(0027) = 'S01X01Y20'
         df_AERO_COL01(0028) = 'S01X03Y03'
         df_AERO_COL01(0029) = 'S01X03Y06'
         df_AERO_COL01(0030) = 'S01X03Y20'
         df_AERO_COL01(0031) = 'S02U02Y03'
         df_AERO_COL01(0032) = 'S02U02Y06'
         df_AERO_COL01(0033) = 'S02U02Y20'
         df_AERO_COL01(0034) = 'S02X00Y03'
         df_AERO_COL01(0035) = 'S02X00Y06'
         df_AERO_COL01(0036) = 'S02X00Y20'
         df_AERO_COL01(0037) = 'S02X01Y03'
         df_AERO_COL01(0038) = 'S02X01Y06'
         df_AERO_COL01(0039) = 'S02X01Y20'
         df_AERO_COL01(0040) = 'S02X03Y03'
         df_AERO_COL01(0041) = 'S02X03Y06'
         df_AERO_COL01(0042) = 'S02X03Y20'
         df_AERO_COL01(0043) = 'S03U02Y03'
         df_AERO_COL01(0044) = 'S03U02Y06'
         df_AERO_COL01(0045) = 'S03U02Y20'
         df_AERO_COL01(0046) = 'S03X00Y03'
         df_AERO_COL01(0047) = 'S03X00Y06'
         df_AERO_COL01(0048) = 'S03X00Y20'
         df_AERO_COL01(0049) = 'S03X01Y03'
         df_AERO_COL01(0050) = 'S03X01Y06'
         df_AERO_COL01(0051) = 'S03X01Y20'
         df_AERO_COL01(0052) = 'S03X03Y03'
         df_AERO_COL01(0053) = 'S03X03Y06'
         df_AERO_COL01(0054) = 'S03X03Y20'
         df_AERO_COL01(0055) = 'S04U02Y03'
         df_AERO_COL01(0056) = 'S04U02Y06'
         df_AERO_COL01(0057) = 'S04U02Y20'
         df_AERO_COL01(0058) = 'S04X00Y03'
         df_AERO_COL01(0059) = 'S04X00Y06'
         df_AERO_COL01(0060) = 'S04X00Y20'
         df_AERO_COL01(0061) = 'S04X01Y03'
         df_AERO_COL01(0062) = 'S04X01Y06'
         df_AERO_COL01(0063) = 'S04X01Y20'
         df_AERO_COL01(0064) = 'S04X03Y03'
         df_AERO_COL01(0065) = 'S04X03Y06'
         df_AERO_COL01(0066) = 'S04X03Y20'
         df_AERO_COL01(0067) = 'S05U02Y03'
         df_AERO_COL01(0068) = 'S05U02Y05'
         df_AERO_COL01(0069) = 'S05U02Y20'
         df_AERO_COL01(0070) = 'S05X00Y03'
         df_AERO_COL01(0071) = 'S05X00Y05'
         df_AERO_COL01(0072) = 'S05X00Y20'
         df_AERO_COL01(0073) = 'S05X01Y03'
         df_AERO_COL01(0074) = 'S05X01Y05'
         df_AERO_COL01(0075) = 'S05X01Y20'
         df_AERO_COL01(0076) = 'S05X03Y03'
         df_AERO_COL01(0077) = 'S05X03Y05'
         df_AERO_COL01(0078) = 'S05X03Y20'
         df_AERO_COL01(0079) = 'S06U02Y03'
         df_AERO_COL01(0080) = 'S06U02Y06'
         df_AERO_COL01(0081) = 'S06U02Y20'
         df_AERO_COL01(0082) = 'S06X00Y03'
         df_AERO_COL01(0083) = 'S06X00Y06'
         df_AERO_COL01(0084) = 'S06X00Y20'
         df_AERO_COL01(0085) = 'S06X01Y03'
         df_AERO_COL01(0086) = 'S06X01Y06'
         df_AERO_COL01(0087) = 'S06X01Y20'
         df_AERO_COL01(0088) = 'S06X03Y03'
         df_AERO_COL01(0089) = 'S06X03Y06'
         df_AERO_COL01(0090) = 'S06X03Y20'
         df_AERO_COL01(0091) = 'S07U02Y00'
         df_AERO_COL01(0092) = 'S07U02Y05'
         df_AERO_COL01(0093) = 'S07U02Y10'
         df_AERO_COL01(0094) = 'S07X00Y00'
         df_AERO_COL01(0095) = 'S07X00Y05'
         df_AERO_COL01(0096) = 'S07X00Y10'
         df_AERO_COL01(0097) = 'S07X01Y00'
         df_AERO_COL01(0098) = 'S07X01Y05'
         df_AERO_COL01(0099) = 'S07X01Y10'
         df_AERO_COL01(0100) = 'S07X03Y00'
         df_AERO_COL01(0101) = 'S07X03Y05'
         df_AERO_COL01(0102) = 'S07X03Y10'

!        SET COLUMN (AERO-COL02): CHEM-INDEX
!        ===============================================================
         df_AERO_COL02(0001,01) = p_SO4_a01
         df_AERO_COL02(0001,02) = p_SO4_a02
         df_AERO_COL02(0001,03) = p_SO4_a03
         df_AERO_COL02(0001,04) = p_SO4_a04

         df_AERO_COL02(0002,01) = p_NO3_a01
         df_AERO_COL02(0002,02) = p_NO3_a02
         df_AERO_COL02(0002,03) = p_NO3_a03
         df_AERO_COL02(0002,04) = p_NO3_a04

         df_AERO_COL02(0003,01) = p_CL_a01
         df_AERO_COL02(0003,02) = p_CL_a02
         df_AERO_COL02(0003,03) = p_CL_a03
         df_AERO_COL02(0003,04) = p_CL_a04

         df_AERO_COL02(0004,01) = p_NH4_a01
         df_AERO_COL02(0004,02) = p_NH4_a02
         df_AERO_COL02(0004,03) = p_NH4_a03
         df_AERO_COL02(0004,04) = p_NH4_a04

         df_AERO_COL02(0005,01) = p_NA_a01
         df_AERO_COL02(0005,02) = p_NA_a02
         df_AERO_COL02(0005,03) = p_NA_a03
         df_AERO_COL02(0005,04) = p_NA_a04

         df_AERO_COL02(0006,01) = p_OIN_a01
         df_AERO_COL02(0006,02) = p_OIN_a02
         df_AERO_COL02(0006,03) = p_OIN_a03
         df_AERO_COL02(0006,04) = p_OIN_a04

         df_AERO_COL02(0007,01) = p_OC_a01
         df_AERO_COL02(0007,02) = p_OC_a02
         df_AERO_COL02(0007,03) = p_OC_a03
         df_AERO_COL02(0007,04) = p_OC_a04

         df_AERO_COL02(0008,01) = p_BC_a01
         df_AERO_COL02(0008,02) = p_BC_a02
         df_AERO_COL02(0008,03) = p_BC_a03
         df_AERO_COL02(0008,04) = p_BC_a04

         df_AERO_COL02(0009,01) = p_HYSW_a01
         df_AERO_COL02(0009,02) = p_HYSW_a02
         df_AERO_COL02(0009,03) = p_HYSW_a03
         df_AERO_COL02(0009,04) = p_HYSW_a04

         df_AERO_COL02(0010,01) = p_WATER_a01
         df_AERO_COL02(0010,02) = p_WATER_a02
         df_AERO_COL02(0010,03) = p_WATER_a03
         df_AERO_COL02(0010,04) = p_WATER_a04

         df_AERO_COL02(0011,01) = p_CLOUDSOA_a01
         df_AERO_COL02(0011,02) = p_CLOUDSOA_a02
         df_AERO_COL02(0011,03) = p_CLOUDSOA_a03
         df_AERO_COL02(0011,04) = p_CLOUDSOA_a04

         df_AERO_COL02(0012,01) = p_IEPOX_a01
         df_AERO_COL02(0012,02) = p_IEPOX_a02
         df_AERO_COL02(0012,03) = p_IEPOX_a03
         df_AERO_COL02(0012,04) = p_IEPOX_a04

         df_AERO_COL02(0013,01) = p_IEPOXOS_a01
         df_AERO_COL02(0013,02) = p_IEPOXOS_a02
         df_AERO_COL02(0013,03) = p_IEPOXOS_a03
         df_AERO_COL02(0013,04) = p_IEPOXOS_a04

         df_AERO_COL02(0014,01) = p_TETROL_a01
         df_AERO_COL02(0014,02) = p_TETROL_a02
         df_AERO_COL02(0014,03) = p_TETROL_a03
         df_AERO_COL02(0014,04) = p_TETROL_a04

         df_AERO_COL02(0015,01) = p_GLY_a01
         df_AERO_COL02(0015,02) = p_GLY_a02
         df_AERO_COL02(0015,03) = p_GLY_a03
         df_AERO_COL02(0015,04) = p_GLY_a04

         df_AERO_COL02(0016,01) = p_P07U03Y06_a01
         df_AERO_COL02(0016,02) = p_P07U03Y06_a02
         df_AERO_COL02(0016,03) = p_P07U03Y06_a03
         df_AERO_COL02(0016,04) = p_P07U03Y06_a04

         df_AERO_COL02(0017,01) = p_P07X00Y06_a01
         df_AERO_COL02(0017,02) = p_P07X00Y06_a02
         df_AERO_COL02(0017,03) = p_P07X00Y06_a03
         df_AERO_COL02(0017,04) = p_P07X00Y06_a04

         df_AERO_COL02(0018,01) = p_P07X03Y06_a01
         df_AERO_COL02(0018,02) = p_P07X03Y06_a02
         df_AERO_COL02(0018,03) = p_P07X03Y06_a03
         df_AERO_COL02(0018,04) = p_P07X03Y06_a04

         df_AERO_COL02(0019,01) = p_S01U02Y03_a01
         df_AERO_COL02(0019,02) = p_S01U02Y03_a02
         df_AERO_COL02(0019,03) = p_S01U02Y03_a03
         df_AERO_COL02(0019,04) = p_S01U02Y03_a04

         df_AERO_COL02(0020,01) = p_S01U02Y06_a01
         df_AERO_COL02(0020,02) = p_S01U02Y06_a02
         df_AERO_COL02(0020,03) = p_S01U02Y06_a03
         df_AERO_COL02(0020,04) = p_S01U02Y06_a04

         df_AERO_COL02(0021,01) = p_S01U02Y20_a01
         df_AERO_COL02(0021,02) = p_S01U02Y20_a02
         df_AERO_COL02(0021,03) = p_S01U02Y20_a03
         df_AERO_COL02(0021,04) = p_S01U02Y20_a04

         df_AERO_COL02(0022,01) = p_S01X00Y03_a01
         df_AERO_COL02(0022,02) = p_S01X00Y03_a02
         df_AERO_COL02(0022,03) = p_S01X00Y03_a03
         df_AERO_COL02(0022,04) = p_S01X00Y03_a04

         df_AERO_COL02(0023,01) = p_S01X00Y06_a01
         df_AERO_COL02(0023,02) = p_S01X00Y06_a02
         df_AERO_COL02(0023,03) = p_S01X00Y06_a03
         df_AERO_COL02(0023,04) = p_S01X00Y06_a04

         df_AERO_COL02(0024,01) = p_S01X00Y20_a01
         df_AERO_COL02(0024,02) = p_S01X00Y20_a02
         df_AERO_COL02(0024,03) = p_S01X00Y20_a03
         df_AERO_COL02(0024,04) = p_S01X00Y20_a04

         df_AERO_COL02(0025,01) = p_S01X01Y03_a01
         df_AERO_COL02(0025,02) = p_S01X01Y03_a02
         df_AERO_COL02(0025,03) = p_S01X01Y03_a03
         df_AERO_COL02(0025,04) = p_S01X01Y03_a04

         df_AERO_COL02(0026,01) = p_S01X01Y06_a01
         df_AERO_COL02(0026,02) = p_S01X01Y06_a02
         df_AERO_COL02(0026,03) = p_S01X01Y06_a03
         df_AERO_COL02(0026,04) = p_S01X01Y06_a04

         df_AERO_COL02(0027,01) = p_S01X01Y20_a01
         df_AERO_COL02(0027,02) = p_S01X01Y20_a02
         df_AERO_COL02(0027,03) = p_S01X01Y20_a03
         df_AERO_COL02(0027,04) = p_S01X01Y20_a04

         df_AERO_COL02(0028,01) = p_S01X03Y03_a01
         df_AERO_COL02(0028,02) = p_S01X03Y03_a02
         df_AERO_COL02(0028,03) = p_S01X03Y03_a03
         df_AERO_COL02(0028,04) = p_S01X03Y03_a04

         df_AERO_COL02(0029,01) = p_S01X03Y06_a01
         df_AERO_COL02(0029,02) = p_S01X03Y06_a02
         df_AERO_COL02(0029,03) = p_S01X03Y06_a03
         df_AERO_COL02(0029,04) = p_S01X03Y06_a04

         df_AERO_COL02(0030,01) = p_S01X03Y20_a01
         df_AERO_COL02(0030,02) = p_S01X03Y20_a02
         df_AERO_COL02(0030,03) = p_S01X03Y20_a03
         df_AERO_COL02(0030,04) = p_S01X03Y20_a04

         df_AERO_COL02(0031,01) = p_S02U02Y03_a01
         df_AERO_COL02(0031,02) = p_S02U02Y03_a02
         df_AERO_COL02(0031,03) = p_S02U02Y03_a03
         df_AERO_COL02(0031,04) = p_S02U02Y03_a04

         df_AERO_COL02(0032,01) = p_S02U02Y06_a01
         df_AERO_COL02(0032,02) = p_S02U02Y06_a02
         df_AERO_COL02(0032,03) = p_S02U02Y06_a03
         df_AERO_COL02(0032,04) = p_S02U02Y06_a04

         df_AERO_COL02(0033,01) = p_S02U02Y20_a01
         df_AERO_COL02(0033,02) = p_S02U02Y20_a02
         df_AERO_COL02(0033,03) = p_S02U02Y20_a03
         df_AERO_COL02(0033,04) = p_S02U02Y20_a04

         df_AERO_COL02(0034,01) = p_S02X00Y03_a01
         df_AERO_COL02(0034,02) = p_S02X00Y03_a02
         df_AERO_COL02(0034,03) = p_S02X00Y03_a03
         df_AERO_COL02(0034,04) = p_S02X00Y03_a04

         df_AERO_COL02(0035,01) = p_S02X00Y06_a01
         df_AERO_COL02(0035,02) = p_S02X00Y06_a02
         df_AERO_COL02(0035,03) = p_S02X00Y06_a03
         df_AERO_COL02(0035,04) = p_S02X00Y06_a04

         df_AERO_COL02(0036,01) = p_S02X00Y20_a01
         df_AERO_COL02(0036,02) = p_S02X00Y20_a02
         df_AERO_COL02(0036,03) = p_S02X00Y20_a03
         df_AERO_COL02(0036,04) = p_S02X00Y20_a04

         df_AERO_COL02(0037,01) = p_S02X01Y03_a01
         df_AERO_COL02(0037,02) = p_S02X01Y03_a02
         df_AERO_COL02(0037,03) = p_S02X01Y03_a03
         df_AERO_COL02(0037,04) = p_S02X01Y03_a04

         df_AERO_COL02(0038,01) = p_S02X01Y06_a01
         df_AERO_COL02(0038,02) = p_S02X01Y06_a02
         df_AERO_COL02(0038,03) = p_S02X01Y06_a03
         df_AERO_COL02(0038,04) = p_S02X01Y06_a04

         df_AERO_COL02(0039,01) = p_S02X01Y20_a01
         df_AERO_COL02(0039,02) = p_S02X01Y20_a02
         df_AERO_COL02(0039,03) = p_S02X01Y20_a03
         df_AERO_COL02(0039,04) = p_S02X01Y20_a04

         df_AERO_COL02(0040,01) = p_S02X03Y03_a01
         df_AERO_COL02(0040,02) = p_S02X03Y03_a02
         df_AERO_COL02(0040,03) = p_S02X03Y03_a03
         df_AERO_COL02(0040,04) = p_S02X03Y03_a04

         df_AERO_COL02(0041,01) = p_S02X03Y06_a01
         df_AERO_COL02(0041,02) = p_S02X03Y06_a02
         df_AERO_COL02(0041,03) = p_S02X03Y06_a03
         df_AERO_COL02(0041,04) = p_S02X03Y06_a04

         df_AERO_COL02(0042,01) = p_S02X03Y20_a01
         df_AERO_COL02(0042,02) = p_S02X03Y20_a02
         df_AERO_COL02(0042,03) = p_S02X03Y20_a03
         df_AERO_COL02(0042,04) = p_S02X03Y20_a04

         df_AERO_COL02(0043,01) = p_S03U02Y03_a01
         df_AERO_COL02(0043,02) = p_S03U02Y03_a02
         df_AERO_COL02(0043,03) = p_S03U02Y03_a03
         df_AERO_COL02(0043,04) = p_S03U02Y03_a04

         df_AERO_COL02(0044,01) = p_S03U02Y06_a01
         df_AERO_COL02(0044,02) = p_S03U02Y06_a02
         df_AERO_COL02(0044,03) = p_S03U02Y06_a03
         df_AERO_COL02(0044,04) = p_S03U02Y06_a04

         df_AERO_COL02(0045,01) = p_S03U02Y20_a01
         df_AERO_COL02(0045,02) = p_S03U02Y20_a02
         df_AERO_COL02(0045,03) = p_S03U02Y20_a03
         df_AERO_COL02(0045,04) = p_S03U02Y20_a04

         df_AERO_COL02(0046,01) = p_S03X00Y03_a01
         df_AERO_COL02(0046,02) = p_S03X00Y03_a02
         df_AERO_COL02(0046,03) = p_S03X00Y03_a03
         df_AERO_COL02(0046,04) = p_S03X00Y03_a04

         df_AERO_COL02(0047,01) = p_S03X00Y06_a01
         df_AERO_COL02(0047,02) = p_S03X00Y06_a02
         df_AERO_COL02(0047,03) = p_S03X00Y06_a03
         df_AERO_COL02(0047,04) = p_S03X00Y06_a04

         df_AERO_COL02(0048,01) = p_S03X00Y20_a01
         df_AERO_COL02(0048,02) = p_S03X00Y20_a02
         df_AERO_COL02(0048,03) = p_S03X00Y20_a03
         df_AERO_COL02(0048,04) = p_S03X00Y20_a04

         df_AERO_COL02(0049,01) = p_S03X01Y03_a01
         df_AERO_COL02(0049,02) = p_S03X01Y03_a02
         df_AERO_COL02(0049,03) = p_S03X01Y03_a03
         df_AERO_COL02(0049,04) = p_S03X01Y03_a04

         df_AERO_COL02(0050,01) = p_S03X01Y06_a01
         df_AERO_COL02(0050,02) = p_S03X01Y06_a02
         df_AERO_COL02(0050,03) = p_S03X01Y06_a03
         df_AERO_COL02(0050,04) = p_S03X01Y06_a04

         df_AERO_COL02(0051,01) = p_S03X01Y20_a01
         df_AERO_COL02(0051,02) = p_S03X01Y20_a02
         df_AERO_COL02(0051,03) = p_S03X01Y20_a03
         df_AERO_COL02(0051,04) = p_S03X01Y20_a04

         df_AERO_COL02(0052,01) = p_S03X03Y03_a01
         df_AERO_COL02(0052,02) = p_S03X03Y03_a02
         df_AERO_COL02(0052,03) = p_S03X03Y03_a03
         df_AERO_COL02(0052,04) = p_S03X03Y03_a04

         df_AERO_COL02(0053,01) = p_S03X03Y06_a01
         df_AERO_COL02(0053,02) = p_S03X03Y06_a02
         df_AERO_COL02(0053,03) = p_S03X03Y06_a03
         df_AERO_COL02(0053,04) = p_S03X03Y06_a04

         df_AERO_COL02(0054,01) = p_S03X03Y20_a01
         df_AERO_COL02(0054,02) = p_S03X03Y20_a02
         df_AERO_COL02(0054,03) = p_S03X03Y20_a03
         df_AERO_COL02(0054,04) = p_S03X03Y20_a04

         df_AERO_COL02(0055,01) = p_S04U02Y03_a01
         df_AERO_COL02(0055,02) = p_S04U02Y03_a02
         df_AERO_COL02(0055,03) = p_S04U02Y03_a03
         df_AERO_COL02(0055,04) = p_S04U02Y03_a04

         df_AERO_COL02(0056,01) = p_S04U02Y06_a01
         df_AERO_COL02(0056,02) = p_S04U02Y06_a02
         df_AERO_COL02(0056,03) = p_S04U02Y06_a03
         df_AERO_COL02(0056,04) = p_S04U02Y06_a04

         df_AERO_COL02(0057,01) = p_S04U02Y20_a01
         df_AERO_COL02(0057,02) = p_S04U02Y20_a02
         df_AERO_COL02(0057,03) = p_S04U02Y20_a03
         df_AERO_COL02(0057,04) = p_S04U02Y20_a04

         df_AERO_COL02(0058,01) = p_S04X00Y03_a01
         df_AERO_COL02(0058,02) = p_S04X00Y03_a02
         df_AERO_COL02(0058,03) = p_S04X00Y03_a03
         df_AERO_COL02(0058,04) = p_S04X00Y03_a04

         df_AERO_COL02(0059,01) = p_S04X00Y06_a01
         df_AERO_COL02(0059,02) = p_S04X00Y06_a02
         df_AERO_COL02(0059,03) = p_S04X00Y06_a03
         df_AERO_COL02(0059,04) = p_S04X00Y06_a04

         df_AERO_COL02(0060,01) = p_S04X00Y20_a01
         df_AERO_COL02(0060,02) = p_S04X00Y20_a02
         df_AERO_COL02(0060,03) = p_S04X00Y20_a03
         df_AERO_COL02(0060,04) = p_S04X00Y20_a04

         df_AERO_COL02(0061,01) = p_S04X01Y03_a01
         df_AERO_COL02(0061,02) = p_S04X01Y03_a02
         df_AERO_COL02(0061,03) = p_S04X01Y03_a03
         df_AERO_COL02(0061,04) = p_S04X01Y03_a04

         df_AERO_COL02(0062,01) = p_S04X01Y06_a01
         df_AERO_COL02(0062,02) = p_S04X01Y06_a02
         df_AERO_COL02(0062,03) = p_S04X01Y06_a03
         df_AERO_COL02(0062,04) = p_S04X01Y06_a04

         df_AERO_COL02(0063,01) = p_S04X01Y20_a01
         df_AERO_COL02(0063,02) = p_S04X01Y20_a02
         df_AERO_COL02(0063,03) = p_S04X01Y20_a03
         df_AERO_COL02(0063,04) = p_S04X01Y20_a04

         df_AERO_COL02(0064,01) = p_S04X03Y03_a01
         df_AERO_COL02(0064,02) = p_S04X03Y03_a02
         df_AERO_COL02(0064,03) = p_S04X03Y03_a03
         df_AERO_COL02(0064,04) = p_S04X03Y03_a04

         df_AERO_COL02(0065,01) = p_S04X03Y06_a01
         df_AERO_COL02(0065,02) = p_S04X03Y06_a02
         df_AERO_COL02(0065,03) = p_S04X03Y06_a03
         df_AERO_COL02(0065,04) = p_S04X03Y06_a04

         df_AERO_COL02(0066,01) = p_S04X03Y20_a01
         df_AERO_COL02(0066,02) = p_S04X03Y20_a02
         df_AERO_COL02(0066,03) = p_S04X03Y20_a03
         df_AERO_COL02(0066,04) = p_S04X03Y20_a04

         df_AERO_COL02(0067,01) = p_S05U02Y03_a01
         df_AERO_COL02(0067,02) = p_S05U02Y03_a02
         df_AERO_COL02(0067,03) = p_S05U02Y03_a03
         df_AERO_COL02(0067,04) = p_S05U02Y03_a04

         df_AERO_COL02(0068,01) = p_S05U02Y05_a01
         df_AERO_COL02(0068,02) = p_S05U02Y05_a02
         df_AERO_COL02(0068,03) = p_S05U02Y05_a03
         df_AERO_COL02(0068,04) = p_S05U02Y05_a04

         df_AERO_COL02(0069,01) = p_S05U02Y20_a01
         df_AERO_COL02(0069,02) = p_S05U02Y20_a02
         df_AERO_COL02(0069,03) = p_S05U02Y20_a03
         df_AERO_COL02(0069,04) = p_S05U02Y20_a04

         df_AERO_COL02(0070,01) = p_S05X00Y03_a01
         df_AERO_COL02(0070,02) = p_S05X00Y03_a02
         df_AERO_COL02(0070,03) = p_S05X00Y03_a03
         df_AERO_COL02(0070,04) = p_S05X00Y03_a04

         df_AERO_COL02(0071,01) = p_S05X00Y05_a01
         df_AERO_COL02(0071,02) = p_S05X00Y05_a02
         df_AERO_COL02(0071,03) = p_S05X00Y05_a03
         df_AERO_COL02(0071,04) = p_S05X00Y05_a04

         df_AERO_COL02(0072,01) = p_S05X00Y20_a01
         df_AERO_COL02(0072,02) = p_S05X00Y20_a02
         df_AERO_COL02(0072,03) = p_S05X00Y20_a03
         df_AERO_COL02(0072,04) = p_S05X00Y20_a04

         df_AERO_COL02(0073,01) = p_S05X01Y03_a01
         df_AERO_COL02(0073,02) = p_S05X01Y03_a02
         df_AERO_COL02(0073,03) = p_S05X01Y03_a03
         df_AERO_COL02(0073,04) = p_S05X01Y03_a04

         df_AERO_COL02(0074,01) = p_S05X01Y05_a01
         df_AERO_COL02(0074,02) = p_S05X01Y05_a02
         df_AERO_COL02(0074,03) = p_S05X01Y05_a03
         df_AERO_COL02(0074,04) = p_S05X01Y05_a04

         df_AERO_COL02(0075,01) = p_S05X01Y20_a01
         df_AERO_COL02(0075,02) = p_S05X01Y20_a02
         df_AERO_COL02(0075,03) = p_S05X01Y20_a03
         df_AERO_COL02(0075,04) = p_S05X01Y20_a04

         df_AERO_COL02(0076,01) = p_S05X03Y03_a01
         df_AERO_COL02(0076,02) = p_S05X03Y03_a02
         df_AERO_COL02(0076,03) = p_S05X03Y03_a03
         df_AERO_COL02(0076,04) = p_S05X03Y03_a04

         df_AERO_COL02(0077,01) = p_S05X03Y05_a01
         df_AERO_COL02(0077,02) = p_S05X03Y05_a02
         df_AERO_COL02(0077,03) = p_S05X03Y05_a03
         df_AERO_COL02(0077,04) = p_S05X03Y05_a04

         df_AERO_COL02(0078,01) = p_S05X03Y20_a01
         df_AERO_COL02(0078,02) = p_S05X03Y20_a02
         df_AERO_COL02(0078,03) = p_S05X03Y20_a03
         df_AERO_COL02(0078,04) = p_S05X03Y20_a04

         df_AERO_COL02(0079,01) = p_S06U02Y03_a01
         df_AERO_COL02(0079,02) = p_S06U02Y03_a02
         df_AERO_COL02(0079,03) = p_S06U02Y03_a03
         df_AERO_COL02(0079,04) = p_S06U02Y03_a04

         df_AERO_COL02(0080,01) = p_S06U02Y06_a01
         df_AERO_COL02(0080,02) = p_S06U02Y06_a02
         df_AERO_COL02(0080,03) = p_S06U02Y06_a03
         df_AERO_COL02(0080,04) = p_S06U02Y06_a04

         df_AERO_COL02(0081,01) = p_S06U02Y20_a01
         df_AERO_COL02(0081,02) = p_S06U02Y20_a02
         df_AERO_COL02(0081,03) = p_S06U02Y20_a03
         df_AERO_COL02(0081,04) = p_S06U02Y20_a04

         df_AERO_COL02(0082,01) = p_S06X00Y03_a01
         df_AERO_COL02(0082,02) = p_S06X00Y03_a02
         df_AERO_COL02(0082,03) = p_S06X00Y03_a03
         df_AERO_COL02(0082,04) = p_S06X00Y03_a04

         df_AERO_COL02(0083,01) = p_S06X00Y06_a01
         df_AERO_COL02(0083,02) = p_S06X00Y06_a02
         df_AERO_COL02(0083,03) = p_S06X00Y06_a03
         df_AERO_COL02(0083,04) = p_S06X00Y06_a04

         df_AERO_COL02(0084,01) = p_S06X00Y20_a01
         df_AERO_COL02(0084,02) = p_S06X00Y20_a02
         df_AERO_COL02(0084,03) = p_S06X00Y20_a03
         df_AERO_COL02(0084,04) = p_S06X00Y20_a04

         df_AERO_COL02(0085,01) = p_S06X01Y03_a01
         df_AERO_COL02(0085,02) = p_S06X01Y03_a02
         df_AERO_COL02(0085,03) = p_S06X01Y03_a03
         df_AERO_COL02(0085,04) = p_S06X01Y03_a04

         df_AERO_COL02(0086,01) = p_S06X01Y06_a01
         df_AERO_COL02(0086,02) = p_S06X01Y06_a02
         df_AERO_COL02(0086,03) = p_S06X01Y06_a03
         df_AERO_COL02(0086,04) = p_S06X01Y06_a04

         df_AERO_COL02(0087,01) = p_S06X01Y20_a01
         df_AERO_COL02(0087,02) = p_S06X01Y20_a02
         df_AERO_COL02(0087,03) = p_S06X01Y20_a03
         df_AERO_COL02(0087,04) = p_S06X01Y20_a04

         df_AERO_COL02(0088,01) = p_S06X03Y03_a01
         df_AERO_COL02(0088,02) = p_S06X03Y03_a02
         df_AERO_COL02(0088,03) = p_S06X03Y03_a03
         df_AERO_COL02(0088,04) = p_S06X03Y03_a04

         df_AERO_COL02(0089,01) = p_S06X03Y06_a01
         df_AERO_COL02(0089,02) = p_S06X03Y06_a02
         df_AERO_COL02(0089,03) = p_S06X03Y06_a03
         df_AERO_COL02(0089,04) = p_S06X03Y06_a04

         df_AERO_COL02(0090,01) = p_S06X03Y20_a01
         df_AERO_COL02(0090,02) = p_S06X03Y20_a02
         df_AERO_COL02(0090,03) = p_S06X03Y20_a03
         df_AERO_COL02(0090,04) = p_S06X03Y20_a04

         df_AERO_COL02(0091,01) = p_S07U02Y00_a01
         df_AERO_COL02(0091,02) = p_S07U02Y00_a02
         df_AERO_COL02(0091,03) = p_S07U02Y00_a03
         df_AERO_COL02(0091,04) = p_S07U02Y00_a04

         df_AERO_COL02(0092,01) = p_S07U02Y05_a01
         df_AERO_COL02(0092,02) = p_S07U02Y05_a02
         df_AERO_COL02(0092,03) = p_S07U02Y05_a03
         df_AERO_COL02(0092,04) = p_S07U02Y05_a04

         df_AERO_COL02(0093,01) = p_S07U02Y10_a01
         df_AERO_COL02(0093,02) = p_S07U02Y10_a02
         df_AERO_COL02(0093,03) = p_S07U02Y10_a03
         df_AERO_COL02(0093,04) = p_S07U02Y10_a04

         df_AERO_COL02(0094,01) = p_S07X00Y00_a01
         df_AERO_COL02(0094,02) = p_S07X00Y00_a02
         df_AERO_COL02(0094,03) = p_S07X00Y00_a03
         df_AERO_COL02(0094,04) = p_S07X00Y00_a04

         df_AERO_COL02(0095,01) = p_S07X00Y05_a01
         df_AERO_COL02(0095,02) = p_S07X00Y05_a02
         df_AERO_COL02(0095,03) = p_S07X00Y05_a03
         df_AERO_COL02(0095,04) = p_S07X00Y05_a04

         df_AERO_COL02(0096,01) = p_S07X00Y10_a01
         df_AERO_COL02(0096,02) = p_S07X00Y10_a02
         df_AERO_COL02(0096,03) = p_S07X00Y10_a03
         df_AERO_COL02(0096,04) = p_S07X00Y10_a04

         df_AERO_COL02(0097,01) = p_S07X01Y00_a01
         df_AERO_COL02(0097,02) = p_S07X01Y00_a02
         df_AERO_COL02(0097,03) = p_S07X01Y00_a03
         df_AERO_COL02(0097,04) = p_S07X01Y00_a04

         df_AERO_COL02(0098,01) = p_S07X01Y05_a01
         df_AERO_COL02(0098,02) = p_S07X01Y05_a02
         df_AERO_COL02(0098,03) = p_S07X01Y05_a03
         df_AERO_COL02(0098,04) = p_S07X01Y05_a04

         df_AERO_COL02(0099,01) = p_S07X01Y10_a01
         df_AERO_COL02(0099,02) = p_S07X01Y10_a02
         df_AERO_COL02(0099,03) = p_S07X01Y10_a03
         df_AERO_COL02(0099,04) = p_S07X01Y10_a04

         df_AERO_COL02(0100,01) = p_S07X03Y00_a01
         df_AERO_COL02(0100,02) = p_S07X03Y00_a02
         df_AERO_COL02(0100,03) = p_S07X03Y00_a03
         df_AERO_COL02(0100,04) = p_S07X03Y00_a04

         df_AERO_COL02(0101,01) = p_S07X03Y05_a01
         df_AERO_COL02(0101,02) = p_S07X03Y05_a02
         df_AERO_COL02(0101,03) = p_S07X03Y05_a03
         df_AERO_COL02(0101,04) = p_S07X03Y05_a04

         df_AERO_COL02(0102,01) = p_S07X03Y10_a01
         df_AERO_COL02(0102,02) = p_S07X03Y10_a02
         df_AERO_COL02(0102,03) = p_S07X03Y10_a03
         df_AERO_COL02(0102,04) = p_S07X03Y10_a04

!        SET COLUMN (AERO-COL03): MOLECULAR WEIGHT
!        ===============================================================
         df_AERO_COL03(0001) = 9.60e+01 ! << SO4
         df_AERO_COL03(0002) = 6.20e+01 ! << NO3
         df_AERO_COL03(0003) = 3.55e+01 ! << CL
         df_AERO_COL03(0004) = 1.80e+01 ! << NH4
         df_AERO_COL03(0005) = 2.30e+01 ! << NA
         df_AERO_COL03(0006) = 1.00e+00 ! << OIN
         df_AERO_COL03(0007) = 2.50e+02 ! << OC
         df_AERO_COL03(0008) = 1.00e+00 ! << BC
         df_AERO_COL03(0009) = 1.80e+01 ! << HYSW
         df_AERO_COL03(0010) = 1.80e+01 ! << WATER
         df_AERO_COL03(0011) = 2.50e+02 ! << CLOUDSOA
         df_AERO_COL03(0012) = 1.18e+02 ! << IEPOX
         df_AERO_COL03(0013) = 1.26e+02 ! << IEPOXOS
         df_AERO_COL03(0014) = 1.36e+02 ! << TETROL
         df_AERO_COL03(0015) = 5.80e+01 ! << GLY
         df_AERO_COL03(0016) = 2.02e+02 ! << P07U03Y06
         df_AERO_COL03(0017) = 1.63e+02 ! << P07X00Y06
         df_AERO_COL03(0018) = 1.24e+02 ! << P07X03Y06
         df_AERO_COL03(0019) = 2.32e+02 ! << S01U02Y03
         df_AERO_COL03(0020) = 1.89e+02 ! << S01U02Y06
         df_AERO_COL03(0021) = 1.36e+02 ! << S01U02Y20
         df_AERO_COL03(0022) = 2.00e+02 ! << S01X00Y03
         df_AERO_COL03(0023) = 1.63e+02 ! << S01X00Y06
         df_AERO_COL03(0024) = 1.17e+02 ! << S01X00Y20
         df_AERO_COL03(0025) = 1.84e+02 ! << S01X01Y03
         df_AERO_COL03(0026) = 1.50e+02 ! << S01X01Y06
         df_AERO_COL03(0027) = 1.08e+02 ! << S01X01Y20
         df_AERO_COL03(0028) = 1.52e+02 ! << S01X03Y03
         df_AERO_COL03(0029) = 1.24e+02 ! << S01X03Y06
         df_AERO_COL03(0030) = 8.89e+01 ! << S01X03Y20
         df_AERO_COL03(0031) = 2.32e+02 ! << S02U02Y03
         df_AERO_COL03(0032) = 1.89e+02 ! << S02U02Y06
         df_AERO_COL03(0033) = 1.36e+02 ! << S02U02Y20
         df_AERO_COL03(0034) = 2.00e+02 ! << S02X00Y03
         df_AERO_COL03(0035) = 1.63e+02 ! << S02X00Y06
         df_AERO_COL03(0036) = 1.17e+02 ! << S02X00Y20
         df_AERO_COL03(0037) = 1.84e+02 ! << S02X01Y03
         df_AERO_COL03(0038) = 1.50e+02 ! << S02X01Y06
         df_AERO_COL03(0039) = 1.08e+02 ! << S02X01Y20
         df_AERO_COL03(0040) = 1.52e+02 ! << S02X03Y03
         df_AERO_COL03(0041) = 1.24e+02 ! << S02X03Y06
         df_AERO_COL03(0042) = 8.89e+01 ! << S02X03Y20
         df_AERO_COL03(0043) = 2.32e+02 ! << S03U02Y03
         df_AERO_COL03(0044) = 1.89e+02 ! << S03U02Y06
         df_AERO_COL03(0045) = 1.36e+02 ! << S03U02Y20
         df_AERO_COL03(0046) = 2.00e+02 ! << S03X00Y03
         df_AERO_COL03(0047) = 1.63e+02 ! << S03X00Y06
         df_AERO_COL03(0048) = 1.17e+02 ! << S03X00Y20
         df_AERO_COL03(0049) = 1.84e+02 ! << S03X01Y03
         df_AERO_COL03(0050) = 1.50e+02 ! << S03X01Y06
         df_AERO_COL03(0051) = 1.08e+02 ! << S03X01Y20
         df_AERO_COL03(0052) = 1.52e+02 ! << S03X03Y03
         df_AERO_COL03(0053) = 1.24e+02 ! << S03X03Y06
         df_AERO_COL03(0054) = 8.89e+01 ! << S03X03Y20
         df_AERO_COL03(0055) = 2.32e+02 ! << S04U02Y03
         df_AERO_COL03(0056) = 1.89e+02 ! << S04U02Y06
         df_AERO_COL03(0057) = 1.36e+02 ! << S04U02Y20
         df_AERO_COL03(0058) = 2.00e+02 ! << S04X00Y03
         df_AERO_COL03(0059) = 1.63e+02 ! << S04X00Y06
         df_AERO_COL03(0060) = 1.17e+02 ! << S04X00Y20
         df_AERO_COL03(0061) = 1.84e+02 ! << S04X01Y03
         df_AERO_COL03(0062) = 1.50e+02 ! << S04X01Y06
         df_AERO_COL03(0063) = 1.08e+02 ! << S04X01Y20
         df_AERO_COL03(0064) = 1.52e+02 ! << S04X03Y03
         df_AERO_COL03(0065) = 1.24e+02 ! << S04X03Y06
         df_AERO_COL03(0066) = 8.89e+01 ! << S04X03Y20
         df_AERO_COL03(0067) = 2.32e+02 ! << S05U02Y03
         df_AERO_COL03(0068) = 2.00e+02 ! << S05U02Y05
         df_AERO_COL03(0069) = 1.36e+02 ! << S05U02Y20
         df_AERO_COL03(0070) = 2.00e+02 ! << S05X00Y03
         df_AERO_COL03(0071) = 1.72e+02 ! << S05X00Y05
         df_AERO_COL03(0072) = 1.17e+02 ! << S05X00Y20
         df_AERO_COL03(0073) = 1.84e+02 ! << S05X01Y03
         df_AERO_COL03(0074) = 1.59e+02 ! << S05X01Y05
         df_AERO_COL03(0075) = 1.08e+02 ! << S05X01Y20
         df_AERO_COL03(0076) = 1.52e+02 ! << S05X03Y03
         df_AERO_COL03(0077) = 1.31e+02 ! << S05X03Y05
         df_AERO_COL03(0078) = 8.89e+01 ! << S05X03Y20
         df_AERO_COL03(0079) = 2.32e+02 ! << S06U02Y03
         df_AERO_COL03(0080) = 1.89e+02 ! << S06U02Y06
         df_AERO_COL03(0081) = 1.36e+02 ! << S06U02Y20
         df_AERO_COL03(0082) = 2.00e+02 ! << S06X00Y03
         df_AERO_COL03(0083) = 1.63e+02 ! << S06X00Y06
         df_AERO_COL03(0084) = 1.17e+02 ! << S06X00Y20
         df_AERO_COL03(0085) = 1.84e+02 ! << S06X01Y03
         df_AERO_COL03(0086) = 1.50e+02 ! << S06X01Y06
         df_AERO_COL03(0087) = 1.08e+02 ! << S06X01Y20
         df_AERO_COL03(0088) = 1.52e+02 ! << S06X03Y03
         df_AERO_COL03(0089) = 1.24e+02 ! << S06X03Y06
         df_AERO_COL03(0090) = 8.89e+01 ! << S06X03Y20
         df_AERO_COL03(0091) = 3.48e+02 ! << S07U02Y00
         df_AERO_COL03(0092) = 2.00e+02 ! << S07U02Y05
         df_AERO_COL03(0093) = 1.62e+02 ! << S07U02Y10
         df_AERO_COL03(0094) = 3.00e+02 ! << S07X00Y00
         df_AERO_COL03(0095) = 1.72e+02 ! << S07X00Y05
         df_AERO_COL03(0096) = 1.40e+02 ! << S07X00Y10
         df_AERO_COL03(0097) = 2.76e+02 ! << S07X01Y00
         df_AERO_COL03(0098) = 1.59e+02 ! << S07X01Y05
         df_AERO_COL03(0099) = 1.29e+02 ! << S07X01Y10
         df_AERO_COL03(0100) = 2.28e+02 ! << S07X03Y00
         df_AERO_COL03(0101) = 1.31e+02 ! << S07X03Y05
         df_AERO_COL03(0102) = 1.06e+02 ! << S07X03Y10

!        SET COLUMN (AERO-COL04): DENSITY
!        ===============================================================
         df_AERO_COL04(0001) = 1.80e+03 ! << SO4
         df_AERO_COL04(0002) = 1.80e+03 ! << NO3
         df_AERO_COL04(0003) = 2.20e+03 ! << CL
         df_AERO_COL04(0004) = 1.80e+03 ! << NH4
         df_AERO_COL04(0005) = 2.20e+03 ! << NA
         df_AERO_COL04(0006) = 2.60e+03 ! << OIN
         df_AERO_COL04(0007) = 1.00e+03 ! << OC
         df_AERO_COL04(0008) = 1.70e+03 ! << BC
         df_AERO_COL04(0009) = 1.00e+03 ! << HYSW
         df_AERO_COL04(0010) = 1.00e+03 ! << WATER
         df_AERO_COL04(0011) = 1.40e+03 ! << CLOUDSOA
         df_AERO_COL04(0012) = 1.40e+03 ! << IEPOX
         df_AERO_COL04(0013) = 1.40e+03 ! << IEPOXOS
         df_AERO_COL04(0014) = 1.40e+03 ! << TETROL
         df_AERO_COL04(0015) = 1.40e+03 ! << GLY
         df_AERO_COL04(0016) = 1.40e+03 ! << P07U03Y06
         df_AERO_COL04(0017) = 1.40e+03 ! << P07X00Y06
         df_AERO_COL04(0018) = 1.40e+03 ! << P07X03Y06
         df_AERO_COL04(0019) = 1.40e+03 ! << S01U02Y03
         df_AERO_COL04(0020) = 1.40e+03 ! << S01U02Y06
         df_AERO_COL04(0021) = 1.40e+03 ! << S01U02Y20
         df_AERO_COL04(0022) = 1.40e+03 ! << S01X00Y03
         df_AERO_COL04(0023) = 1.40e+03 ! << S01X00Y06
         df_AERO_COL04(0024) = 1.40e+03 ! << S01X00Y20
         df_AERO_COL04(0025) = 1.40e+03 ! << S01X01Y03
         df_AERO_COL04(0026) = 1.40e+03 ! << S01X01Y06
         df_AERO_COL04(0027) = 1.40e+03 ! << S01X01Y20
         df_AERO_COL04(0028) = 1.40e+03 ! << S01X03Y03
         df_AERO_COL04(0029) = 1.40e+03 ! << S01X03Y06
         df_AERO_COL04(0030) = 1.40e+03 ! << S01X03Y20
         df_AERO_COL04(0031) = 1.40e+03 ! << S02U02Y03
         df_AERO_COL04(0032) = 1.40e+03 ! << S02U02Y06
         df_AERO_COL04(0033) = 1.40e+03 ! << S02U02Y20
         df_AERO_COL04(0034) = 1.40e+03 ! << S02X00Y03
         df_AERO_COL04(0035) = 1.40e+03 ! << S02X00Y06
         df_AERO_COL04(0036) = 1.40e+03 ! << S02X00Y20
         df_AERO_COL04(0037) = 1.40e+03 ! << S02X01Y03
         df_AERO_COL04(0038) = 1.40e+03 ! << S02X01Y06
         df_AERO_COL04(0039) = 1.40e+03 ! << S02X01Y20
         df_AERO_COL04(0040) = 1.40e+03 ! << S02X03Y03
         df_AERO_COL04(0041) = 1.40e+03 ! << S02X03Y06
         df_AERO_COL04(0042) = 1.40e+03 ! << S02X03Y20
         df_AERO_COL04(0043) = 1.40e+03 ! << S03U02Y03
         df_AERO_COL04(0044) = 1.40e+03 ! << S03U02Y06
         df_AERO_COL04(0045) = 1.40e+03 ! << S03U02Y20
         df_AERO_COL04(0046) = 1.40e+03 ! << S03X00Y03
         df_AERO_COL04(0047) = 1.40e+03 ! << S03X00Y06
         df_AERO_COL04(0048) = 1.40e+03 ! << S03X00Y20
         df_AERO_COL04(0049) = 1.40e+03 ! << S03X01Y03
         df_AERO_COL04(0050) = 1.40e+03 ! << S03X01Y06
         df_AERO_COL04(0051) = 1.40e+03 ! << S03X01Y20
         df_AERO_COL04(0052) = 1.40e+03 ! << S03X03Y03
         df_AERO_COL04(0053) = 1.40e+03 ! << S03X03Y06
         df_AERO_COL04(0054) = 1.40e+03 ! << S03X03Y20
         df_AERO_COL04(0055) = 1.40e+03 ! << S04U02Y03
         df_AERO_COL04(0056) = 1.40e+03 ! << S04U02Y06
         df_AERO_COL04(0057) = 1.40e+03 ! << S04U02Y20
         df_AERO_COL04(0058) = 1.40e+03 ! << S04X00Y03
         df_AERO_COL04(0059) = 1.40e+03 ! << S04X00Y06
         df_AERO_COL04(0060) = 1.40e+03 ! << S04X00Y20
         df_AERO_COL04(0061) = 1.40e+03 ! << S04X01Y03
         df_AERO_COL04(0062) = 1.40e+03 ! << S04X01Y06
         df_AERO_COL04(0063) = 1.40e+03 ! << S04X01Y20
         df_AERO_COL04(0064) = 1.40e+03 ! << S04X03Y03
         df_AERO_COL04(0065) = 1.40e+03 ! << S04X03Y06
         df_AERO_COL04(0066) = 1.40e+03 ! << S04X03Y20
         df_AERO_COL04(0067) = 1.40e+03 ! << S05U02Y03
         df_AERO_COL04(0068) = 1.40e+03 ! << S05U02Y05
         df_AERO_COL04(0069) = 1.40e+03 ! << S05U02Y20
         df_AERO_COL04(0070) = 1.40e+03 ! << S05X00Y03
         df_AERO_COL04(0071) = 1.40e+03 ! << S05X00Y05
         df_AERO_COL04(0072) = 1.40e+03 ! << S05X00Y20
         df_AERO_COL04(0073) = 1.40e+03 ! << S05X01Y03
         df_AERO_COL04(0074) = 1.40e+03 ! << S05X01Y05
         df_AERO_COL04(0075) = 1.40e+03 ! << S05X01Y20
         df_AERO_COL04(0076) = 1.40e+03 ! << S05X03Y03
         df_AERO_COL04(0077) = 1.40e+03 ! << S05X03Y05
         df_AERO_COL04(0078) = 1.40e+03 ! << S05X03Y20
         df_AERO_COL04(0079) = 1.40e+03 ! << S06U02Y03
         df_AERO_COL04(0080) = 1.40e+03 ! << S06U02Y06
         df_AERO_COL04(0081) = 1.40e+03 ! << S06U02Y20
         df_AERO_COL04(0082) = 1.40e+03 ! << S06X00Y03
         df_AERO_COL04(0083) = 1.40e+03 ! << S06X00Y06
         df_AERO_COL04(0084) = 1.40e+03 ! << S06X00Y20
         df_AERO_COL04(0085) = 1.40e+03 ! << S06X01Y03
         df_AERO_COL04(0086) = 1.40e+03 ! << S06X01Y06
         df_AERO_COL04(0087) = 1.40e+03 ! << S06X01Y20
         df_AERO_COL04(0088) = 1.40e+03 ! << S06X03Y03
         df_AERO_COL04(0089) = 1.40e+03 ! << S06X03Y06
         df_AERO_COL04(0090) = 1.40e+03 ! << S06X03Y20
         df_AERO_COL04(0091) = 1.40e+03 ! << S07U02Y00
         df_AERO_COL04(0092) = 1.40e+03 ! << S07U02Y05
         df_AERO_COL04(0093) = 1.40e+03 ! << S07U02Y10
         df_AERO_COL04(0094) = 1.40e+03 ! << S07X00Y00
         df_AERO_COL04(0095) = 1.40e+03 ! << S07X00Y05
         df_AERO_COL04(0096) = 1.40e+03 ! << S07X00Y10
         df_AERO_COL04(0097) = 1.40e+03 ! << S07X01Y00
         df_AERO_COL04(0098) = 1.40e+03 ! << S07X01Y05
         df_AERO_COL04(0099) = 1.40e+03 ! << S07X01Y10
         df_AERO_COL04(0100) = 1.40e+03 ! << S07X03Y00
         df_AERO_COL04(0101) = 1.40e+03 ! << S07X03Y05
         df_AERO_COL04(0102) = 1.40e+03 ! << S07X03Y10

!        SET COLUMN (AERO-COL05): if-WET
!        ===============================================================
         df_AERO_COL05(0001) = 0 ! << SO4
         df_AERO_COL05(0002) = 0 ! << NO3
         df_AERO_COL05(0003) = 0 ! << CL
         df_AERO_COL05(0004) = 0 ! << NH4
         df_AERO_COL05(0005) = 0 ! << NA
         df_AERO_COL05(0006) = 0 ! << OIN
         df_AERO_COL05(0007) = 0 ! << OC
         df_AERO_COL05(0008) = 0 ! << BC
         df_AERO_COL05(0009) = 1 ! << HYSW
         df_AERO_COL05(0010) = 1 ! << WATER
         df_AERO_COL05(0011) = 0 ! << CLOUDSOA
         df_AERO_COL05(0012) = 0 ! << IEPOX
         df_AERO_COL05(0013) = 0 ! << IEPOXOS
         df_AERO_COL05(0014) = 0 ! << TETROL
         df_AERO_COL05(0015) = 0 ! << GLY
         df_AERO_COL05(0016) = 0 ! << P07U03Y06
         df_AERO_COL05(0017) = 0 ! << P07X00Y06
         df_AERO_COL05(0018) = 0 ! << P07X03Y06
         df_AERO_COL05(0019) = 0 ! << S01U02Y03
         df_AERO_COL05(0020) = 0 ! << S01U02Y06
         df_AERO_COL05(0021) = 0 ! << S01U02Y20
         df_AERO_COL05(0022) = 0 ! << S01X00Y03
         df_AERO_COL05(0023) = 0 ! << S01X00Y06
         df_AERO_COL05(0024) = 0 ! << S01X00Y20
         df_AERO_COL05(0025) = 0 ! << S01X01Y03
         df_AERO_COL05(0026) = 0 ! << S01X01Y06
         df_AERO_COL05(0027) = 0 ! << S01X01Y20
         df_AERO_COL05(0028) = 0 ! << S01X03Y03
         df_AERO_COL05(0029) = 0 ! << S01X03Y06
         df_AERO_COL05(0030) = 0 ! << S01X03Y20
         df_AERO_COL05(0031) = 0 ! << S02U02Y03
         df_AERO_COL05(0032) = 0 ! << S02U02Y06
         df_AERO_COL05(0033) = 0 ! << S02U02Y20
         df_AERO_COL05(0034) = 0 ! << S02X00Y03
         df_AERO_COL05(0035) = 0 ! << S02X00Y06
         df_AERO_COL05(0036) = 0 ! << S02X00Y20
         df_AERO_COL05(0037) = 0 ! << S02X01Y03
         df_AERO_COL05(0038) = 0 ! << S02X01Y06
         df_AERO_COL05(0039) = 0 ! << S02X01Y20
         df_AERO_COL05(0040) = 0 ! << S02X03Y03
         df_AERO_COL05(0041) = 0 ! << S02X03Y06
         df_AERO_COL05(0042) = 0 ! << S02X03Y20
         df_AERO_COL05(0043) = 0 ! << S03U02Y03
         df_AERO_COL05(0044) = 0 ! << S03U02Y06
         df_AERO_COL05(0045) = 0 ! << S03U02Y20
         df_AERO_COL05(0046) = 0 ! << S03X00Y03
         df_AERO_COL05(0047) = 0 ! << S03X00Y06
         df_AERO_COL05(0048) = 0 ! << S03X00Y20
         df_AERO_COL05(0049) = 0 ! << S03X01Y03
         df_AERO_COL05(0050) = 0 ! << S03X01Y06
         df_AERO_COL05(0051) = 0 ! << S03X01Y20
         df_AERO_COL05(0052) = 0 ! << S03X03Y03
         df_AERO_COL05(0053) = 0 ! << S03X03Y06
         df_AERO_COL05(0054) = 0 ! << S03X03Y20
         df_AERO_COL05(0055) = 0 ! << S04U02Y03
         df_AERO_COL05(0056) = 0 ! << S04U02Y06
         df_AERO_COL05(0057) = 0 ! << S04U02Y20
         df_AERO_COL05(0058) = 0 ! << S04X00Y03
         df_AERO_COL05(0059) = 0 ! << S04X00Y06
         df_AERO_COL05(0060) = 0 ! << S04X00Y20
         df_AERO_COL05(0061) = 0 ! << S04X01Y03
         df_AERO_COL05(0062) = 0 ! << S04X01Y06
         df_AERO_COL05(0063) = 0 ! << S04X01Y20
         df_AERO_COL05(0064) = 0 ! << S04X03Y03
         df_AERO_COL05(0065) = 0 ! << S04X03Y06
         df_AERO_COL05(0066) = 0 ! << S04X03Y20
         df_AERO_COL05(0067) = 0 ! << S05U02Y03
         df_AERO_COL05(0068) = 0 ! << S05U02Y05
         df_AERO_COL05(0069) = 0 ! << S05U02Y20
         df_AERO_COL05(0070) = 0 ! << S05X00Y03
         df_AERO_COL05(0071) = 0 ! << S05X00Y05
         df_AERO_COL05(0072) = 0 ! << S05X00Y20
         df_AERO_COL05(0073) = 0 ! << S05X01Y03
         df_AERO_COL05(0074) = 0 ! << S05X01Y05
         df_AERO_COL05(0075) = 0 ! << S05X01Y20
         df_AERO_COL05(0076) = 0 ! << S05X03Y03
         df_AERO_COL05(0077) = 0 ! << S05X03Y05
         df_AERO_COL05(0078) = 0 ! << S05X03Y20
         df_AERO_COL05(0079) = 0 ! << S06U02Y03
         df_AERO_COL05(0080) = 0 ! << S06U02Y06
         df_AERO_COL05(0081) = 0 ! << S06U02Y20
         df_AERO_COL05(0082) = 0 ! << S06X00Y03
         df_AERO_COL05(0083) = 0 ! << S06X00Y06
         df_AERO_COL05(0084) = 0 ! << S06X00Y20
         df_AERO_COL05(0085) = 0 ! << S06X01Y03
         df_AERO_COL05(0086) = 0 ! << S06X01Y06
         df_AERO_COL05(0087) = 0 ! << S06X01Y20
         df_AERO_COL05(0088) = 0 ! << S06X03Y03
         df_AERO_COL05(0089) = 0 ! << S06X03Y06
         df_AERO_COL05(0090) = 0 ! << S06X03Y20
         df_AERO_COL05(0091) = 0 ! << S07U02Y00
         df_AERO_COL05(0092) = 0 ! << S07U02Y05
         df_AERO_COL05(0093) = 0 ! << S07U02Y10
         df_AERO_COL05(0094) = 0 ! << S07X00Y00
         df_AERO_COL05(0095) = 0 ! << S07X00Y05
         df_AERO_COL05(0096) = 0 ! << S07X00Y10
         df_AERO_COL05(0097) = 0 ! << S07X01Y00
         df_AERO_COL05(0098) = 0 ! << S07X01Y05
         df_AERO_COL05(0099) = 0 ! << S07X01Y10
         df_AERO_COL05(0100) = 0 ! << S07X03Y00
         df_AERO_COL05(0101) = 0 ! << S07X03Y05
         df_AERO_COL05(0102) = 0 ! << S07X03Y10

!        SET COLUMN (AERO-COL06): IF ORGANIC SPECIES
!        ===============================================================
         df_AERO_COL06(0001) = 0 ! << SO4
         df_AERO_COL06(0002) = 0 ! << NO3
         df_AERO_COL06(0003) = 0 ! << CL
         df_AERO_COL06(0004) = 0 ! << NH4
         df_AERO_COL06(0005) = 0 ! << NA
         df_AERO_COL06(0006) = 0 ! << OIN
         df_AERO_COL06(0007) = 0 ! << OC
         df_AERO_COL06(0008) = 0 ! << BC
         df_AERO_COL06(0009) = 0 ! << HYSW
         df_AERO_COL06(0010) = 0 ! << WATER
         df_AERO_COL06(0011) = 1 ! << CLOUDSOA
         df_AERO_COL06(0012) = 0 ! << IEPOX
         df_AERO_COL06(0013) = 0 ! << IEPOXOS
         df_AERO_COL06(0014) = 0 ! << TETROL
         df_AERO_COL06(0015) = 0 ! << GLY
         df_AERO_COL06(0016) = 1 ! << P07U03Y06
         df_AERO_COL06(0017) = 1 ! << P07X00Y06
         df_AERO_COL06(0018) = 1 ! << P07X03Y06
         df_AERO_COL06(0019) = 1 ! << S01U02Y03
         df_AERO_COL06(0020) = 1 ! << S01U02Y06
         df_AERO_COL06(0021) = 1 ! << S01U02Y20
         df_AERO_COL06(0022) = 1 ! << S01X00Y03
         df_AERO_COL06(0023) = 1 ! << S01X00Y06
         df_AERO_COL06(0024) = 1 ! << S01X00Y20
         df_AERO_COL06(0025) = 1 ! << S01X01Y03
         df_AERO_COL06(0026) = 1 ! << S01X01Y06
         df_AERO_COL06(0027) = 1 ! << S01X01Y20
         df_AERO_COL06(0028) = 1 ! << S01X03Y03
         df_AERO_COL06(0029) = 1 ! << S01X03Y06
         df_AERO_COL06(0030) = 1 ! << S01X03Y20
         df_AERO_COL06(0031) = 1 ! << S02U02Y03
         df_AERO_COL06(0032) = 1 ! << S02U02Y06
         df_AERO_COL06(0033) = 1 ! << S02U02Y20
         df_AERO_COL06(0034) = 1 ! << S02X00Y03
         df_AERO_COL06(0035) = 1 ! << S02X00Y06
         df_AERO_COL06(0036) = 1 ! << S02X00Y20
         df_AERO_COL06(0037) = 1 ! << S02X01Y03
         df_AERO_COL06(0038) = 1 ! << S02X01Y06
         df_AERO_COL06(0039) = 1 ! << S02X01Y20
         df_AERO_COL06(0040) = 1 ! << S02X03Y03
         df_AERO_COL06(0041) = 1 ! << S02X03Y06
         df_AERO_COL06(0042) = 1 ! << S02X03Y20
         df_AERO_COL06(0043) = 1 ! << S03U02Y03
         df_AERO_COL06(0044) = 1 ! << S03U02Y06
         df_AERO_COL06(0045) = 1 ! << S03U02Y20
         df_AERO_COL06(0046) = 1 ! << S03X00Y03
         df_AERO_COL06(0047) = 1 ! << S03X00Y06
         df_AERO_COL06(0048) = 1 ! << S03X00Y20
         df_AERO_COL06(0049) = 1 ! << S03X01Y03
         df_AERO_COL06(0050) = 1 ! << S03X01Y06
         df_AERO_COL06(0051) = 1 ! << S03X01Y20
         df_AERO_COL06(0052) = 1 ! << S03X03Y03
         df_AERO_COL06(0053) = 1 ! << S03X03Y06
         df_AERO_COL06(0054) = 1 ! << S03X03Y20
         df_AERO_COL06(0055) = 1 ! << S04U02Y03
         df_AERO_COL06(0056) = 1 ! << S04U02Y06
         df_AERO_COL06(0057) = 1 ! << S04U02Y20
         df_AERO_COL06(0058) = 1 ! << S04X00Y03
         df_AERO_COL06(0059) = 1 ! << S04X00Y06
         df_AERO_COL06(0060) = 1 ! << S04X00Y20
         df_AERO_COL06(0061) = 1 ! << S04X01Y03
         df_AERO_COL06(0062) = 1 ! << S04X01Y06
         df_AERO_COL06(0063) = 1 ! << S04X01Y20
         df_AERO_COL06(0064) = 1 ! << S04X03Y03
         df_AERO_COL06(0065) = 1 ! << S04X03Y06
         df_AERO_COL06(0066) = 1 ! << S04X03Y20
         df_AERO_COL06(0067) = 1 ! << S05U02Y03
         df_AERO_COL06(0068) = 1 ! << S05U02Y05
         df_AERO_COL06(0069) = 1 ! << S05U02Y20
         df_AERO_COL06(0070) = 1 ! << S05X00Y03
         df_AERO_COL06(0071) = 1 ! << S05X00Y05
         df_AERO_COL06(0072) = 1 ! << S05X00Y20
         df_AERO_COL06(0073) = 1 ! << S05X01Y03
         df_AERO_COL06(0074) = 1 ! << S05X01Y05
         df_AERO_COL06(0075) = 1 ! << S05X01Y20
         df_AERO_COL06(0076) = 1 ! << S05X03Y03
         df_AERO_COL06(0077) = 1 ! << S05X03Y05
         df_AERO_COL06(0078) = 1 ! << S05X03Y20
         df_AERO_COL06(0079) = 1 ! << S06U02Y03
         df_AERO_COL06(0080) = 1 ! << S06U02Y06
         df_AERO_COL06(0081) = 1 ! << S06U02Y20
         df_AERO_COL06(0082) = 1 ! << S06X00Y03
         df_AERO_COL06(0083) = 1 ! << S06X00Y06
         df_AERO_COL06(0084) = 1 ! << S06X00Y20
         df_AERO_COL06(0085) = 1 ! << S06X01Y03
         df_AERO_COL06(0086) = 1 ! << S06X01Y06
         df_AERO_COL06(0087) = 1 ! << S06X01Y20
         df_AERO_COL06(0088) = 1 ! << S06X03Y03
         df_AERO_COL06(0089) = 1 ! << S06X03Y06
         df_AERO_COL06(0090) = 1 ! << S06X03Y20
         df_AERO_COL06(0091) = 1 ! << S07U02Y00
         df_AERO_COL06(0092) = 1 ! << S07U02Y05
         df_AERO_COL06(0093) = 1 ! << S07U02Y10
         df_AERO_COL06(0094) = 1 ! << S07X00Y00
         df_AERO_COL06(0095) = 1 ! << S07X00Y05
         df_AERO_COL06(0096) = 1 ! << S07X00Y10
         df_AERO_COL06(0097) = 1 ! << S07X01Y00
         df_AERO_COL06(0098) = 1 ! << S07X01Y05
         df_AERO_COL06(0099) = 1 ! << S07X01Y10
         df_AERO_COL06(0100) = 1 ! << S07X03Y00
         df_AERO_COL06(0101) = 1 ! << S07X03Y05
         df_AERO_COL06(0102) = 1 ! << S07X03Y10

!        DEFINE POINTER ARRAY: AEROSOL SPECIES
!        ===============================================================
         PTRARRAY_AERO(01,0001) = p_SO4_a01
         PTRARRAY_AERO(02,0001) = p_SO4_a02
         PTRARRAY_AERO(03,0001) = p_SO4_a03
         PTRARRAY_AERO(04,0001) = p_SO4_a04

         PTRARRAY_AERO(01,0002) = p_NO3_a01
         PTRARRAY_AERO(02,0002) = p_NO3_a02
         PTRARRAY_AERO(03,0002) = p_NO3_a03
         PTRARRAY_AERO(04,0002) = p_NO3_a04

         PTRARRAY_AERO(01,0003) = p_CL_a01
         PTRARRAY_AERO(02,0003) = p_CL_a02
         PTRARRAY_AERO(03,0003) = p_CL_a03
         PTRARRAY_AERO(04,0003) = p_CL_a04

         PTRARRAY_AERO(01,0004) = p_NH4_a01
         PTRARRAY_AERO(02,0004) = p_NH4_a02
         PTRARRAY_AERO(03,0004) = p_NH4_a03
         PTRARRAY_AERO(04,0004) = p_NH4_a04

         PTRARRAY_AERO(01,0005) = p_NA_a01
         PTRARRAY_AERO(02,0005) = p_NA_a02
         PTRARRAY_AERO(03,0005) = p_NA_a03
         PTRARRAY_AERO(04,0005) = p_NA_a04

         PTRARRAY_AERO(01,0006) = p_OIN_a01
         PTRARRAY_AERO(02,0006) = p_OIN_a02
         PTRARRAY_AERO(03,0006) = p_OIN_a03
         PTRARRAY_AERO(04,0006) = p_OIN_a04

         PTRARRAY_AERO(01,0007) = p_OC_a01
         PTRARRAY_AERO(02,0007) = p_OC_a02
         PTRARRAY_AERO(03,0007) = p_OC_a03
         PTRARRAY_AERO(04,0007) = p_OC_a04

         PTRARRAY_AERO(01,0008) = p_BC_a01
         PTRARRAY_AERO(02,0008) = p_BC_a02
         PTRARRAY_AERO(03,0008) = p_BC_a03
         PTRARRAY_AERO(04,0008) = p_BC_a04

         PTRARRAY_AERO(01,0009) = p_HYSW_a01
         PTRARRAY_AERO(02,0009) = p_HYSW_a02
         PTRARRAY_AERO(03,0009) = p_HYSW_a03
         PTRARRAY_AERO(04,0009) = p_HYSW_a04

         PTRARRAY_AERO(01,0010) = p_WATER_a01
         PTRARRAY_AERO(02,0010) = p_WATER_a02
         PTRARRAY_AERO(03,0010) = p_WATER_a03
         PTRARRAY_AERO(04,0010) = p_WATER_a04

         PTRARRAY_AERO(01,0011) = p_CLOUDSOA_a01
         PTRARRAY_AERO(02,0011) = p_CLOUDSOA_a02
         PTRARRAY_AERO(03,0011) = p_CLOUDSOA_a03
         PTRARRAY_AERO(04,0011) = p_CLOUDSOA_a04

         PTRARRAY_AERO(01,0012) = p_IEPOX_a01
         PTRARRAY_AERO(02,0012) = p_IEPOX_a02
         PTRARRAY_AERO(03,0012) = p_IEPOX_a03
         PTRARRAY_AERO(04,0012) = p_IEPOX_a04

         PTRARRAY_AERO(01,0013) = p_IEPOXOS_a01
         PTRARRAY_AERO(02,0013) = p_IEPOXOS_a02
         PTRARRAY_AERO(03,0013) = p_IEPOXOS_a03
         PTRARRAY_AERO(04,0013) = p_IEPOXOS_a04

         PTRARRAY_AERO(01,0014) = p_TETROL_a01
         PTRARRAY_AERO(02,0014) = p_TETROL_a02
         PTRARRAY_AERO(03,0014) = p_TETROL_a03
         PTRARRAY_AERO(04,0014) = p_TETROL_a04

         PTRARRAY_AERO(01,0015) = p_GLY_a01
         PTRARRAY_AERO(02,0015) = p_GLY_a02
         PTRARRAY_AERO(03,0015) = p_GLY_a03
         PTRARRAY_AERO(04,0015) = p_GLY_a04

         PTRARRAY_AERO(01,0016) = p_P07U03Y06_a01
         PTRARRAY_AERO(02,0016) = p_P07U03Y06_a02
         PTRARRAY_AERO(03,0016) = p_P07U03Y06_a03
         PTRARRAY_AERO(04,0016) = p_P07U03Y06_a04

         PTRARRAY_AERO(01,0017) = p_P07X00Y06_a01
         PTRARRAY_AERO(02,0017) = p_P07X00Y06_a02
         PTRARRAY_AERO(03,0017) = p_P07X00Y06_a03
         PTRARRAY_AERO(04,0017) = p_P07X00Y06_a04

         PTRARRAY_AERO(01,0018) = p_P07X03Y06_a01
         PTRARRAY_AERO(02,0018) = p_P07X03Y06_a02
         PTRARRAY_AERO(03,0018) = p_P07X03Y06_a03
         PTRARRAY_AERO(04,0018) = p_P07X03Y06_a04

         PTRARRAY_AERO(01,0019) = p_S01U02Y03_a01
         PTRARRAY_AERO(02,0019) = p_S01U02Y03_a02
         PTRARRAY_AERO(03,0019) = p_S01U02Y03_a03
         PTRARRAY_AERO(04,0019) = p_S01U02Y03_a04

         PTRARRAY_AERO(01,0020) = p_S01U02Y06_a01
         PTRARRAY_AERO(02,0020) = p_S01U02Y06_a02
         PTRARRAY_AERO(03,0020) = p_S01U02Y06_a03
         PTRARRAY_AERO(04,0020) = p_S01U02Y06_a04

         PTRARRAY_AERO(01,0021) = p_S01U02Y20_a01
         PTRARRAY_AERO(02,0021) = p_S01U02Y20_a02
         PTRARRAY_AERO(03,0021) = p_S01U02Y20_a03
         PTRARRAY_AERO(04,0021) = p_S01U02Y20_a04

         PTRARRAY_AERO(01,0022) = p_S01X00Y03_a01
         PTRARRAY_AERO(02,0022) = p_S01X00Y03_a02
         PTRARRAY_AERO(03,0022) = p_S01X00Y03_a03
         PTRARRAY_AERO(04,0022) = p_S01X00Y03_a04

         PTRARRAY_AERO(01,0023) = p_S01X00Y06_a01
         PTRARRAY_AERO(02,0023) = p_S01X00Y06_a02
         PTRARRAY_AERO(03,0023) = p_S01X00Y06_a03
         PTRARRAY_AERO(04,0023) = p_S01X00Y06_a04

         PTRARRAY_AERO(01,0024) = p_S01X00Y20_a01
         PTRARRAY_AERO(02,0024) = p_S01X00Y20_a02
         PTRARRAY_AERO(03,0024) = p_S01X00Y20_a03
         PTRARRAY_AERO(04,0024) = p_S01X00Y20_a04

         PTRARRAY_AERO(01,0025) = p_S01X01Y03_a01
         PTRARRAY_AERO(02,0025) = p_S01X01Y03_a02
         PTRARRAY_AERO(03,0025) = p_S01X01Y03_a03
         PTRARRAY_AERO(04,0025) = p_S01X01Y03_a04

         PTRARRAY_AERO(01,0026) = p_S01X01Y06_a01
         PTRARRAY_AERO(02,0026) = p_S01X01Y06_a02
         PTRARRAY_AERO(03,0026) = p_S01X01Y06_a03
         PTRARRAY_AERO(04,0026) = p_S01X01Y06_a04

         PTRARRAY_AERO(01,0027) = p_S01X01Y20_a01
         PTRARRAY_AERO(02,0027) = p_S01X01Y20_a02
         PTRARRAY_AERO(03,0027) = p_S01X01Y20_a03
         PTRARRAY_AERO(04,0027) = p_S01X01Y20_a04

         PTRARRAY_AERO(01,0028) = p_S01X03Y03_a01
         PTRARRAY_AERO(02,0028) = p_S01X03Y03_a02
         PTRARRAY_AERO(03,0028) = p_S01X03Y03_a03
         PTRARRAY_AERO(04,0028) = p_S01X03Y03_a04

         PTRARRAY_AERO(01,0029) = p_S01X03Y06_a01
         PTRARRAY_AERO(02,0029) = p_S01X03Y06_a02
         PTRARRAY_AERO(03,0029) = p_S01X03Y06_a03
         PTRARRAY_AERO(04,0029) = p_S01X03Y06_a04

         PTRARRAY_AERO(01,0030) = p_S01X03Y20_a01
         PTRARRAY_AERO(02,0030) = p_S01X03Y20_a02
         PTRARRAY_AERO(03,0030) = p_S01X03Y20_a03
         PTRARRAY_AERO(04,0030) = p_S01X03Y20_a04

         PTRARRAY_AERO(01,0031) = p_S02U02Y03_a01
         PTRARRAY_AERO(02,0031) = p_S02U02Y03_a02
         PTRARRAY_AERO(03,0031) = p_S02U02Y03_a03
         PTRARRAY_AERO(04,0031) = p_S02U02Y03_a04

         PTRARRAY_AERO(01,0032) = p_S02U02Y06_a01
         PTRARRAY_AERO(02,0032) = p_S02U02Y06_a02
         PTRARRAY_AERO(03,0032) = p_S02U02Y06_a03
         PTRARRAY_AERO(04,0032) = p_S02U02Y06_a04

         PTRARRAY_AERO(01,0033) = p_S02U02Y20_a01
         PTRARRAY_AERO(02,0033) = p_S02U02Y20_a02
         PTRARRAY_AERO(03,0033) = p_S02U02Y20_a03
         PTRARRAY_AERO(04,0033) = p_S02U02Y20_a04

         PTRARRAY_AERO(01,0034) = p_S02X00Y03_a01
         PTRARRAY_AERO(02,0034) = p_S02X00Y03_a02
         PTRARRAY_AERO(03,0034) = p_S02X00Y03_a03
         PTRARRAY_AERO(04,0034) = p_S02X00Y03_a04

         PTRARRAY_AERO(01,0035) = p_S02X00Y06_a01
         PTRARRAY_AERO(02,0035) = p_S02X00Y06_a02
         PTRARRAY_AERO(03,0035) = p_S02X00Y06_a03
         PTRARRAY_AERO(04,0035) = p_S02X00Y06_a04

         PTRARRAY_AERO(01,0036) = p_S02X00Y20_a01
         PTRARRAY_AERO(02,0036) = p_S02X00Y20_a02
         PTRARRAY_AERO(03,0036) = p_S02X00Y20_a03
         PTRARRAY_AERO(04,0036) = p_S02X00Y20_a04

         PTRARRAY_AERO(01,0037) = p_S02X01Y03_a01
         PTRARRAY_AERO(02,0037) = p_S02X01Y03_a02
         PTRARRAY_AERO(03,0037) = p_S02X01Y03_a03
         PTRARRAY_AERO(04,0037) = p_S02X01Y03_a04

         PTRARRAY_AERO(01,0038) = p_S02X01Y06_a01
         PTRARRAY_AERO(02,0038) = p_S02X01Y06_a02
         PTRARRAY_AERO(03,0038) = p_S02X01Y06_a03
         PTRARRAY_AERO(04,0038) = p_S02X01Y06_a04

         PTRARRAY_AERO(01,0039) = p_S02X01Y20_a01
         PTRARRAY_AERO(02,0039) = p_S02X01Y20_a02
         PTRARRAY_AERO(03,0039) = p_S02X01Y20_a03
         PTRARRAY_AERO(04,0039) = p_S02X01Y20_a04

         PTRARRAY_AERO(01,0040) = p_S02X03Y03_a01
         PTRARRAY_AERO(02,0040) = p_S02X03Y03_a02
         PTRARRAY_AERO(03,0040) = p_S02X03Y03_a03
         PTRARRAY_AERO(04,0040) = p_S02X03Y03_a04

         PTRARRAY_AERO(01,0041) = p_S02X03Y06_a01
         PTRARRAY_AERO(02,0041) = p_S02X03Y06_a02
         PTRARRAY_AERO(03,0041) = p_S02X03Y06_a03
         PTRARRAY_AERO(04,0041) = p_S02X03Y06_a04

         PTRARRAY_AERO(01,0042) = p_S02X03Y20_a01
         PTRARRAY_AERO(02,0042) = p_S02X03Y20_a02
         PTRARRAY_AERO(03,0042) = p_S02X03Y20_a03
         PTRARRAY_AERO(04,0042) = p_S02X03Y20_a04

         PTRARRAY_AERO(01,0043) = p_S03U02Y03_a01
         PTRARRAY_AERO(02,0043) = p_S03U02Y03_a02
         PTRARRAY_AERO(03,0043) = p_S03U02Y03_a03
         PTRARRAY_AERO(04,0043) = p_S03U02Y03_a04

         PTRARRAY_AERO(01,0044) = p_S03U02Y06_a01
         PTRARRAY_AERO(02,0044) = p_S03U02Y06_a02
         PTRARRAY_AERO(03,0044) = p_S03U02Y06_a03
         PTRARRAY_AERO(04,0044) = p_S03U02Y06_a04

         PTRARRAY_AERO(01,0045) = p_S03U02Y20_a01
         PTRARRAY_AERO(02,0045) = p_S03U02Y20_a02
         PTRARRAY_AERO(03,0045) = p_S03U02Y20_a03
         PTRARRAY_AERO(04,0045) = p_S03U02Y20_a04

         PTRARRAY_AERO(01,0046) = p_S03X00Y03_a01
         PTRARRAY_AERO(02,0046) = p_S03X00Y03_a02
         PTRARRAY_AERO(03,0046) = p_S03X00Y03_a03
         PTRARRAY_AERO(04,0046) = p_S03X00Y03_a04

         PTRARRAY_AERO(01,0047) = p_S03X00Y06_a01
         PTRARRAY_AERO(02,0047) = p_S03X00Y06_a02
         PTRARRAY_AERO(03,0047) = p_S03X00Y06_a03
         PTRARRAY_AERO(04,0047) = p_S03X00Y06_a04

         PTRARRAY_AERO(01,0048) = p_S03X00Y20_a01
         PTRARRAY_AERO(02,0048) = p_S03X00Y20_a02
         PTRARRAY_AERO(03,0048) = p_S03X00Y20_a03
         PTRARRAY_AERO(04,0048) = p_S03X00Y20_a04

         PTRARRAY_AERO(01,0049) = p_S03X01Y03_a01
         PTRARRAY_AERO(02,0049) = p_S03X01Y03_a02
         PTRARRAY_AERO(03,0049) = p_S03X01Y03_a03
         PTRARRAY_AERO(04,0049) = p_S03X01Y03_a04

         PTRARRAY_AERO(01,0050) = p_S03X01Y06_a01
         PTRARRAY_AERO(02,0050) = p_S03X01Y06_a02
         PTRARRAY_AERO(03,0050) = p_S03X01Y06_a03
         PTRARRAY_AERO(04,0050) = p_S03X01Y06_a04

         PTRARRAY_AERO(01,0051) = p_S03X01Y20_a01
         PTRARRAY_AERO(02,0051) = p_S03X01Y20_a02
         PTRARRAY_AERO(03,0051) = p_S03X01Y20_a03
         PTRARRAY_AERO(04,0051) = p_S03X01Y20_a04

         PTRARRAY_AERO(01,0052) = p_S03X03Y03_a01
         PTRARRAY_AERO(02,0052) = p_S03X03Y03_a02
         PTRARRAY_AERO(03,0052) = p_S03X03Y03_a03
         PTRARRAY_AERO(04,0052) = p_S03X03Y03_a04

         PTRARRAY_AERO(01,0053) = p_S03X03Y06_a01
         PTRARRAY_AERO(02,0053) = p_S03X03Y06_a02
         PTRARRAY_AERO(03,0053) = p_S03X03Y06_a03
         PTRARRAY_AERO(04,0053) = p_S03X03Y06_a04

         PTRARRAY_AERO(01,0054) = p_S03X03Y20_a01
         PTRARRAY_AERO(02,0054) = p_S03X03Y20_a02
         PTRARRAY_AERO(03,0054) = p_S03X03Y20_a03
         PTRARRAY_AERO(04,0054) = p_S03X03Y20_a04

         PTRARRAY_AERO(01,0055) = p_S04U02Y03_a01
         PTRARRAY_AERO(02,0055) = p_S04U02Y03_a02
         PTRARRAY_AERO(03,0055) = p_S04U02Y03_a03
         PTRARRAY_AERO(04,0055) = p_S04U02Y03_a04

         PTRARRAY_AERO(01,0056) = p_S04U02Y06_a01
         PTRARRAY_AERO(02,0056) = p_S04U02Y06_a02
         PTRARRAY_AERO(03,0056) = p_S04U02Y06_a03
         PTRARRAY_AERO(04,0056) = p_S04U02Y06_a04

         PTRARRAY_AERO(01,0057) = p_S04U02Y20_a01
         PTRARRAY_AERO(02,0057) = p_S04U02Y20_a02
         PTRARRAY_AERO(03,0057) = p_S04U02Y20_a03
         PTRARRAY_AERO(04,0057) = p_S04U02Y20_a04

         PTRARRAY_AERO(01,0058) = p_S04X00Y03_a01
         PTRARRAY_AERO(02,0058) = p_S04X00Y03_a02
         PTRARRAY_AERO(03,0058) = p_S04X00Y03_a03
         PTRARRAY_AERO(04,0058) = p_S04X00Y03_a04

         PTRARRAY_AERO(01,0059) = p_S04X00Y06_a01
         PTRARRAY_AERO(02,0059) = p_S04X00Y06_a02
         PTRARRAY_AERO(03,0059) = p_S04X00Y06_a03
         PTRARRAY_AERO(04,0059) = p_S04X00Y06_a04

         PTRARRAY_AERO(01,0060) = p_S04X00Y20_a01
         PTRARRAY_AERO(02,0060) = p_S04X00Y20_a02
         PTRARRAY_AERO(03,0060) = p_S04X00Y20_a03
         PTRARRAY_AERO(04,0060) = p_S04X00Y20_a04

         PTRARRAY_AERO(01,0061) = p_S04X01Y03_a01
         PTRARRAY_AERO(02,0061) = p_S04X01Y03_a02
         PTRARRAY_AERO(03,0061) = p_S04X01Y03_a03
         PTRARRAY_AERO(04,0061) = p_S04X01Y03_a04

         PTRARRAY_AERO(01,0062) = p_S04X01Y06_a01
         PTRARRAY_AERO(02,0062) = p_S04X01Y06_a02
         PTRARRAY_AERO(03,0062) = p_S04X01Y06_a03
         PTRARRAY_AERO(04,0062) = p_S04X01Y06_a04

         PTRARRAY_AERO(01,0063) = p_S04X01Y20_a01
         PTRARRAY_AERO(02,0063) = p_S04X01Y20_a02
         PTRARRAY_AERO(03,0063) = p_S04X01Y20_a03
         PTRARRAY_AERO(04,0063) = p_S04X01Y20_a04

         PTRARRAY_AERO(01,0064) = p_S04X03Y03_a01
         PTRARRAY_AERO(02,0064) = p_S04X03Y03_a02
         PTRARRAY_AERO(03,0064) = p_S04X03Y03_a03
         PTRARRAY_AERO(04,0064) = p_S04X03Y03_a04

         PTRARRAY_AERO(01,0065) = p_S04X03Y06_a01
         PTRARRAY_AERO(02,0065) = p_S04X03Y06_a02
         PTRARRAY_AERO(03,0065) = p_S04X03Y06_a03
         PTRARRAY_AERO(04,0065) = p_S04X03Y06_a04

         PTRARRAY_AERO(01,0066) = p_S04X03Y20_a01
         PTRARRAY_AERO(02,0066) = p_S04X03Y20_a02
         PTRARRAY_AERO(03,0066) = p_S04X03Y20_a03
         PTRARRAY_AERO(04,0066) = p_S04X03Y20_a04

         PTRARRAY_AERO(01,0067) = p_S05U02Y03_a01
         PTRARRAY_AERO(02,0067) = p_S05U02Y03_a02
         PTRARRAY_AERO(03,0067) = p_S05U02Y03_a03
         PTRARRAY_AERO(04,0067) = p_S05U02Y03_a04

         PTRARRAY_AERO(01,0068) = p_S05U02Y05_a01
         PTRARRAY_AERO(02,0068) = p_S05U02Y05_a02
         PTRARRAY_AERO(03,0068) = p_S05U02Y05_a03
         PTRARRAY_AERO(04,0068) = p_S05U02Y05_a04

         PTRARRAY_AERO(01,0069) = p_S05U02Y20_a01
         PTRARRAY_AERO(02,0069) = p_S05U02Y20_a02
         PTRARRAY_AERO(03,0069) = p_S05U02Y20_a03
         PTRARRAY_AERO(04,0069) = p_S05U02Y20_a04

         PTRARRAY_AERO(01,0070) = p_S05X00Y03_a01
         PTRARRAY_AERO(02,0070) = p_S05X00Y03_a02
         PTRARRAY_AERO(03,0070) = p_S05X00Y03_a03
         PTRARRAY_AERO(04,0070) = p_S05X00Y03_a04

         PTRARRAY_AERO(01,0071) = p_S05X00Y05_a01
         PTRARRAY_AERO(02,0071) = p_S05X00Y05_a02
         PTRARRAY_AERO(03,0071) = p_S05X00Y05_a03
         PTRARRAY_AERO(04,0071) = p_S05X00Y05_a04

         PTRARRAY_AERO(01,0072) = p_S05X00Y20_a01
         PTRARRAY_AERO(02,0072) = p_S05X00Y20_a02
         PTRARRAY_AERO(03,0072) = p_S05X00Y20_a03
         PTRARRAY_AERO(04,0072) = p_S05X00Y20_a04

         PTRARRAY_AERO(01,0073) = p_S05X01Y03_a01
         PTRARRAY_AERO(02,0073) = p_S05X01Y03_a02
         PTRARRAY_AERO(03,0073) = p_S05X01Y03_a03
         PTRARRAY_AERO(04,0073) = p_S05X01Y03_a04

         PTRARRAY_AERO(01,0074) = p_S05X01Y05_a01
         PTRARRAY_AERO(02,0074) = p_S05X01Y05_a02
         PTRARRAY_AERO(03,0074) = p_S05X01Y05_a03
         PTRARRAY_AERO(04,0074) = p_S05X01Y05_a04

         PTRARRAY_AERO(01,0075) = p_S05X01Y20_a01
         PTRARRAY_AERO(02,0075) = p_S05X01Y20_a02
         PTRARRAY_AERO(03,0075) = p_S05X01Y20_a03
         PTRARRAY_AERO(04,0075) = p_S05X01Y20_a04

         PTRARRAY_AERO(01,0076) = p_S05X03Y03_a01
         PTRARRAY_AERO(02,0076) = p_S05X03Y03_a02
         PTRARRAY_AERO(03,0076) = p_S05X03Y03_a03
         PTRARRAY_AERO(04,0076) = p_S05X03Y03_a04

         PTRARRAY_AERO(01,0077) = p_S05X03Y05_a01
         PTRARRAY_AERO(02,0077) = p_S05X03Y05_a02
         PTRARRAY_AERO(03,0077) = p_S05X03Y05_a03
         PTRARRAY_AERO(04,0077) = p_S05X03Y05_a04

         PTRARRAY_AERO(01,0078) = p_S05X03Y20_a01
         PTRARRAY_AERO(02,0078) = p_S05X03Y20_a02
         PTRARRAY_AERO(03,0078) = p_S05X03Y20_a03
         PTRARRAY_AERO(04,0078) = p_S05X03Y20_a04

         PTRARRAY_AERO(01,0079) = p_S06U02Y03_a01
         PTRARRAY_AERO(02,0079) = p_S06U02Y03_a02
         PTRARRAY_AERO(03,0079) = p_S06U02Y03_a03
         PTRARRAY_AERO(04,0079) = p_S06U02Y03_a04

         PTRARRAY_AERO(01,0080) = p_S06U02Y06_a01
         PTRARRAY_AERO(02,0080) = p_S06U02Y06_a02
         PTRARRAY_AERO(03,0080) = p_S06U02Y06_a03
         PTRARRAY_AERO(04,0080) = p_S06U02Y06_a04

         PTRARRAY_AERO(01,0081) = p_S06U02Y20_a01
         PTRARRAY_AERO(02,0081) = p_S06U02Y20_a02
         PTRARRAY_AERO(03,0081) = p_S06U02Y20_a03
         PTRARRAY_AERO(04,0081) = p_S06U02Y20_a04

         PTRARRAY_AERO(01,0082) = p_S06X00Y03_a01
         PTRARRAY_AERO(02,0082) = p_S06X00Y03_a02
         PTRARRAY_AERO(03,0082) = p_S06X00Y03_a03
         PTRARRAY_AERO(04,0082) = p_S06X00Y03_a04

         PTRARRAY_AERO(01,0083) = p_S06X00Y06_a01
         PTRARRAY_AERO(02,0083) = p_S06X00Y06_a02
         PTRARRAY_AERO(03,0083) = p_S06X00Y06_a03
         PTRARRAY_AERO(04,0083) = p_S06X00Y06_a04

         PTRARRAY_AERO(01,0084) = p_S06X00Y20_a01
         PTRARRAY_AERO(02,0084) = p_S06X00Y20_a02
         PTRARRAY_AERO(03,0084) = p_S06X00Y20_a03
         PTRARRAY_AERO(04,0084) = p_S06X00Y20_a04

         PTRARRAY_AERO(01,0085) = p_S06X01Y03_a01
         PTRARRAY_AERO(02,0085) = p_S06X01Y03_a02
         PTRARRAY_AERO(03,0085) = p_S06X01Y03_a03
         PTRARRAY_AERO(04,0085) = p_S06X01Y03_a04

         PTRARRAY_AERO(01,0086) = p_S06X01Y06_a01
         PTRARRAY_AERO(02,0086) = p_S06X01Y06_a02
         PTRARRAY_AERO(03,0086) = p_S06X01Y06_a03
         PTRARRAY_AERO(04,0086) = p_S06X01Y06_a04

         PTRARRAY_AERO(01,0087) = p_S06X01Y20_a01
         PTRARRAY_AERO(02,0087) = p_S06X01Y20_a02
         PTRARRAY_AERO(03,0087) = p_S06X01Y20_a03
         PTRARRAY_AERO(04,0087) = p_S06X01Y20_a04

         PTRARRAY_AERO(01,0088) = p_S06X03Y03_a01
         PTRARRAY_AERO(02,0088) = p_S06X03Y03_a02
         PTRARRAY_AERO(03,0088) = p_S06X03Y03_a03
         PTRARRAY_AERO(04,0088) = p_S06X03Y03_a04

         PTRARRAY_AERO(01,0089) = p_S06X03Y06_a01
         PTRARRAY_AERO(02,0089) = p_S06X03Y06_a02
         PTRARRAY_AERO(03,0089) = p_S06X03Y06_a03
         PTRARRAY_AERO(04,0089) = p_S06X03Y06_a04

         PTRARRAY_AERO(01,0090) = p_S06X03Y20_a01
         PTRARRAY_AERO(02,0090) = p_S06X03Y20_a02
         PTRARRAY_AERO(03,0090) = p_S06X03Y20_a03
         PTRARRAY_AERO(04,0090) = p_S06X03Y20_a04

         PTRARRAY_AERO(01,0091) = p_S07U02Y00_a01
         PTRARRAY_AERO(02,0091) = p_S07U02Y00_a02
         PTRARRAY_AERO(03,0091) = p_S07U02Y00_a03
         PTRARRAY_AERO(04,0091) = p_S07U02Y00_a04

         PTRARRAY_AERO(01,0092) = p_S07U02Y05_a01
         PTRARRAY_AERO(02,0092) = p_S07U02Y05_a02
         PTRARRAY_AERO(03,0092) = p_S07U02Y05_a03
         PTRARRAY_AERO(04,0092) = p_S07U02Y05_a04

         PTRARRAY_AERO(01,0093) = p_S07U02Y10_a01
         PTRARRAY_AERO(02,0093) = p_S07U02Y10_a02
         PTRARRAY_AERO(03,0093) = p_S07U02Y10_a03
         PTRARRAY_AERO(04,0093) = p_S07U02Y10_a04

         PTRARRAY_AERO(01,0094) = p_S07X00Y00_a01
         PTRARRAY_AERO(02,0094) = p_S07X00Y00_a02
         PTRARRAY_AERO(03,0094) = p_S07X00Y00_a03
         PTRARRAY_AERO(04,0094) = p_S07X00Y00_a04

         PTRARRAY_AERO(01,0095) = p_S07X00Y05_a01
         PTRARRAY_AERO(02,0095) = p_S07X00Y05_a02
         PTRARRAY_AERO(03,0095) = p_S07X00Y05_a03
         PTRARRAY_AERO(04,0095) = p_S07X00Y05_a04

         PTRARRAY_AERO(01,0096) = p_S07X00Y10_a01
         PTRARRAY_AERO(02,0096) = p_S07X00Y10_a02
         PTRARRAY_AERO(03,0096) = p_S07X00Y10_a03
         PTRARRAY_AERO(04,0096) = p_S07X00Y10_a04

         PTRARRAY_AERO(01,0097) = p_S07X01Y00_a01
         PTRARRAY_AERO(02,0097) = p_S07X01Y00_a02
         PTRARRAY_AERO(03,0097) = p_S07X01Y00_a03
         PTRARRAY_AERO(04,0097) = p_S07X01Y00_a04

         PTRARRAY_AERO(01,0098) = p_S07X01Y05_a01
         PTRARRAY_AERO(02,0098) = p_S07X01Y05_a02
         PTRARRAY_AERO(03,0098) = p_S07X01Y05_a03
         PTRARRAY_AERO(04,0098) = p_S07X01Y05_a04

         PTRARRAY_AERO(01,0099) = p_S07X01Y10_a01
         PTRARRAY_AERO(02,0099) = p_S07X01Y10_a02
         PTRARRAY_AERO(03,0099) = p_S07X01Y10_a03
         PTRARRAY_AERO(04,0099) = p_S07X01Y10_a04

         PTRARRAY_AERO(01,0100) = p_S07X03Y00_a01
         PTRARRAY_AERO(02,0100) = p_S07X03Y00_a02
         PTRARRAY_AERO(03,0100) = p_S07X03Y00_a03
         PTRARRAY_AERO(04,0100) = p_S07X03Y00_a04

         PTRARRAY_AERO(01,0101) = p_S07X03Y05_a01
         PTRARRAY_AERO(02,0101) = p_S07X03Y05_a02
         PTRARRAY_AERO(03,0101) = p_S07X03Y05_a03
         PTRARRAY_AERO(04,0101) = p_S07X03Y05_a04

         PTRARRAY_AERO(01,0102) = p_S07X03Y10_a01
         PTRARRAY_AERO(02,0102) = p_S07X03Y10_a02
         PTRARRAY_AERO(03,0102) = p_S07X03Y10_a03
         PTRARRAY_AERO(04,0102) = p_S07X03Y10_a04

!        DEFINE POINTER ARRAY: AEROSOL SPECIES IN CLOUD-PHASE
!        ===============================================================
         PTRARRAYaw(01,0001) = p_SO4_cw01
         PTRARRAYaw(02,0001) = p_SO4_cw02
         PTRARRAYaw(03,0001) = p_SO4_cw03
         PTRARRAYaw(04,0001) = p_SO4_cw04

         PTRARRAYaw(01,0002) = p_NO3_cw01
         PTRARRAYaw(02,0002) = p_NO3_cw02
         PTRARRAYaw(03,0002) = p_NO3_cw03
         PTRARRAYaw(04,0002) = p_NO3_cw04

         PTRARRAYaw(01,0003) = p_CL_cw01
         PTRARRAYaw(02,0003) = p_CL_cw02
         PTRARRAYaw(03,0003) = p_CL_cw03
         PTRARRAYaw(04,0003) = p_CL_cw04

         PTRARRAYaw(01,0004) = p_NH4_cw01
         PTRARRAYaw(02,0004) = p_NH4_cw02
         PTRARRAYaw(03,0004) = p_NH4_cw03
         PTRARRAYaw(04,0004) = p_NH4_cw04

         PTRARRAYaw(01,0005) = p_NA_cw01
         PTRARRAYaw(02,0005) = p_NA_cw02
         PTRARRAYaw(03,0005) = p_NA_cw03
         PTRARRAYaw(04,0005) = p_NA_cw04

         PTRARRAYaw(01,0006) = p_OIN_cw01
         PTRARRAYaw(02,0006) = p_OIN_cw02
         PTRARRAYaw(03,0006) = p_OIN_cw03
         PTRARRAYaw(04,0006) = p_OIN_cw04

         PTRARRAYaw(01,0007) = p_OC_cw01
         PTRARRAYaw(02,0007) = p_OC_cw02
         PTRARRAYaw(03,0007) = p_OC_cw03
         PTRARRAYaw(04,0007) = p_OC_cw04

         PTRARRAYaw(01,0008) = p_BC_cw01
         PTRARRAYaw(02,0008) = p_BC_cw02
         PTRARRAYaw(03,0008) = p_BC_cw03
         PTRARRAYaw(04,0008) = p_BC_cw04

         PTRARRAYaw(01,0009) = 1
         PTRARRAYaw(02,0009) = 1
         PTRARRAYaw(03,0009) = 1
         PTRARRAYaw(04,0009) = 1

         PTRARRAYaw(01,0010) = 1
         PTRARRAYaw(02,0010) = 1
         PTRARRAYaw(03,0010) = 1
         PTRARRAYaw(04,0010) = 1

         PTRARRAYaw(01,0011) = p_CLOUDSOA_cw01
         PTRARRAYaw(02,0011) = p_CLOUDSOA_cw02
         PTRARRAYaw(03,0011) = p_CLOUDSOA_cw03
         PTRARRAYaw(04,0011) = p_CLOUDSOA_cw04

         PTRARRAYaw(01,0012) = p_IEPOX_cw01
         PTRARRAYaw(02,0012) = p_IEPOX_cw02
         PTRARRAYaw(03,0012) = p_IEPOX_cw03
         PTRARRAYaw(04,0012) = p_IEPOX_cw04

         PTRARRAYaw(01,0013) = p_IEPOXOS_cw01
         PTRARRAYaw(02,0013) = p_IEPOXOS_cw02
         PTRARRAYaw(03,0013) = p_IEPOXOS_cw03
         PTRARRAYaw(04,0013) = p_IEPOXOS_cw04

         PTRARRAYaw(01,0014) = p_TETROL_cw01
         PTRARRAYaw(02,0014) = p_TETROL_cw02
         PTRARRAYaw(03,0014) = p_TETROL_cw03
         PTRARRAYaw(04,0014) = p_TETROL_cw04

         PTRARRAYaw(01,0015) = p_GLY_cw01
         PTRARRAYaw(02,0015) = p_GLY_cw02
         PTRARRAYaw(03,0015) = p_GLY_cw03
         PTRARRAYaw(04,0015) = p_GLY_cw04

         PTRARRAYaw(01,0016) = p_P07U03Y06_cw01
         PTRARRAYaw(02,0016) = p_P07U03Y06_cw02
         PTRARRAYaw(03,0016) = p_P07U03Y06_cw03
         PTRARRAYaw(04,0016) = p_P07U03Y06_cw04

         PTRARRAYaw(01,0017) = p_P07X00Y06_cw01
         PTRARRAYaw(02,0017) = p_P07X00Y06_cw02
         PTRARRAYaw(03,0017) = p_P07X00Y06_cw03
         PTRARRAYaw(04,0017) = p_P07X00Y06_cw04

         PTRARRAYaw(01,0018) = p_P07X03Y06_cw01
         PTRARRAYaw(02,0018) = p_P07X03Y06_cw02
         PTRARRAYaw(03,0018) = p_P07X03Y06_cw03
         PTRARRAYaw(04,0018) = p_P07X03Y06_cw04

         PTRARRAYaw(01,0019) = p_S01U02Y03_cw01
         PTRARRAYaw(02,0019) = p_S01U02Y03_cw02
         PTRARRAYaw(03,0019) = p_S01U02Y03_cw03
         PTRARRAYaw(04,0019) = p_S01U02Y03_cw04

         PTRARRAYaw(01,0020) = p_S01U02Y06_cw01
         PTRARRAYaw(02,0020) = p_S01U02Y06_cw02
         PTRARRAYaw(03,0020) = p_S01U02Y06_cw03
         PTRARRAYaw(04,0020) = p_S01U02Y06_cw04

         PTRARRAYaw(01,0021) = p_S01U02Y20_cw01
         PTRARRAYaw(02,0021) = p_S01U02Y20_cw02
         PTRARRAYaw(03,0021) = p_S01U02Y20_cw03
         PTRARRAYaw(04,0021) = p_S01U02Y20_cw04

         PTRARRAYaw(01,0022) = p_S01X00Y03_cw01
         PTRARRAYaw(02,0022) = p_S01X00Y03_cw02
         PTRARRAYaw(03,0022) = p_S01X00Y03_cw03
         PTRARRAYaw(04,0022) = p_S01X00Y03_cw04

         PTRARRAYaw(01,0023) = p_S01X00Y06_cw01
         PTRARRAYaw(02,0023) = p_S01X00Y06_cw02
         PTRARRAYaw(03,0023) = p_S01X00Y06_cw03
         PTRARRAYaw(04,0023) = p_S01X00Y06_cw04

         PTRARRAYaw(01,0024) = p_S01X00Y20_cw01
         PTRARRAYaw(02,0024) = p_S01X00Y20_cw02
         PTRARRAYaw(03,0024) = p_S01X00Y20_cw03
         PTRARRAYaw(04,0024) = p_S01X00Y20_cw04

         PTRARRAYaw(01,0025) = p_S01X01Y03_cw01
         PTRARRAYaw(02,0025) = p_S01X01Y03_cw02
         PTRARRAYaw(03,0025) = p_S01X01Y03_cw03
         PTRARRAYaw(04,0025) = p_S01X01Y03_cw04

         PTRARRAYaw(01,0026) = p_S01X01Y06_cw01
         PTRARRAYaw(02,0026) = p_S01X01Y06_cw02
         PTRARRAYaw(03,0026) = p_S01X01Y06_cw03
         PTRARRAYaw(04,0026) = p_S01X01Y06_cw04

         PTRARRAYaw(01,0027) = p_S01X01Y20_cw01
         PTRARRAYaw(02,0027) = p_S01X01Y20_cw02
         PTRARRAYaw(03,0027) = p_S01X01Y20_cw03
         PTRARRAYaw(04,0027) = p_S01X01Y20_cw04

         PTRARRAYaw(01,0028) = p_S01X03Y03_cw01
         PTRARRAYaw(02,0028) = p_S01X03Y03_cw02
         PTRARRAYaw(03,0028) = p_S01X03Y03_cw03
         PTRARRAYaw(04,0028) = p_S01X03Y03_cw04

         PTRARRAYaw(01,0029) = p_S01X03Y06_cw01
         PTRARRAYaw(02,0029) = p_S01X03Y06_cw02
         PTRARRAYaw(03,0029) = p_S01X03Y06_cw03
         PTRARRAYaw(04,0029) = p_S01X03Y06_cw04

         PTRARRAYaw(01,0030) = p_S01X03Y20_cw01
         PTRARRAYaw(02,0030) = p_S01X03Y20_cw02
         PTRARRAYaw(03,0030) = p_S01X03Y20_cw03
         PTRARRAYaw(04,0030) = p_S01X03Y20_cw04

         PTRARRAYaw(01,0031) = p_S02U02Y03_cw01
         PTRARRAYaw(02,0031) = p_S02U02Y03_cw02
         PTRARRAYaw(03,0031) = p_S02U02Y03_cw03
         PTRARRAYaw(04,0031) = p_S02U02Y03_cw04

         PTRARRAYaw(01,0032) = p_S02U02Y06_cw01
         PTRARRAYaw(02,0032) = p_S02U02Y06_cw02
         PTRARRAYaw(03,0032) = p_S02U02Y06_cw03
         PTRARRAYaw(04,0032) = p_S02U02Y06_cw04

         PTRARRAYaw(01,0033) = p_S02U02Y20_cw01
         PTRARRAYaw(02,0033) = p_S02U02Y20_cw02
         PTRARRAYaw(03,0033) = p_S02U02Y20_cw03
         PTRARRAYaw(04,0033) = p_S02U02Y20_cw04

         PTRARRAYaw(01,0034) = p_S02X00Y03_cw01
         PTRARRAYaw(02,0034) = p_S02X00Y03_cw02
         PTRARRAYaw(03,0034) = p_S02X00Y03_cw03
         PTRARRAYaw(04,0034) = p_S02X00Y03_cw04

         PTRARRAYaw(01,0035) = p_S02X00Y06_cw01
         PTRARRAYaw(02,0035) = p_S02X00Y06_cw02
         PTRARRAYaw(03,0035) = p_S02X00Y06_cw03
         PTRARRAYaw(04,0035) = p_S02X00Y06_cw04

         PTRARRAYaw(01,0036) = p_S02X00Y20_cw01
         PTRARRAYaw(02,0036) = p_S02X00Y20_cw02
         PTRARRAYaw(03,0036) = p_S02X00Y20_cw03
         PTRARRAYaw(04,0036) = p_S02X00Y20_cw04

         PTRARRAYaw(01,0037) = p_S02X01Y03_cw01
         PTRARRAYaw(02,0037) = p_S02X01Y03_cw02
         PTRARRAYaw(03,0037) = p_S02X01Y03_cw03
         PTRARRAYaw(04,0037) = p_S02X01Y03_cw04

         PTRARRAYaw(01,0038) = p_S02X01Y06_cw01
         PTRARRAYaw(02,0038) = p_S02X01Y06_cw02
         PTRARRAYaw(03,0038) = p_S02X01Y06_cw03
         PTRARRAYaw(04,0038) = p_S02X01Y06_cw04

         PTRARRAYaw(01,0039) = p_S02X01Y20_cw01
         PTRARRAYaw(02,0039) = p_S02X01Y20_cw02
         PTRARRAYaw(03,0039) = p_S02X01Y20_cw03
         PTRARRAYaw(04,0039) = p_S02X01Y20_cw04

         PTRARRAYaw(01,0040) = p_S02X03Y03_cw01
         PTRARRAYaw(02,0040) = p_S02X03Y03_cw02
         PTRARRAYaw(03,0040) = p_S02X03Y03_cw03
         PTRARRAYaw(04,0040) = p_S02X03Y03_cw04

         PTRARRAYaw(01,0041) = p_S02X03Y06_cw01
         PTRARRAYaw(02,0041) = p_S02X03Y06_cw02
         PTRARRAYaw(03,0041) = p_S02X03Y06_cw03
         PTRARRAYaw(04,0041) = p_S02X03Y06_cw04

         PTRARRAYaw(01,0042) = p_S02X03Y20_cw01
         PTRARRAYaw(02,0042) = p_S02X03Y20_cw02
         PTRARRAYaw(03,0042) = p_S02X03Y20_cw03
         PTRARRAYaw(04,0042) = p_S02X03Y20_cw04

         PTRARRAYaw(01,0043) = p_S03U02Y03_cw01
         PTRARRAYaw(02,0043) = p_S03U02Y03_cw02
         PTRARRAYaw(03,0043) = p_S03U02Y03_cw03
         PTRARRAYaw(04,0043) = p_S03U02Y03_cw04

         PTRARRAYaw(01,0044) = p_S03U02Y06_cw01
         PTRARRAYaw(02,0044) = p_S03U02Y06_cw02
         PTRARRAYaw(03,0044) = p_S03U02Y06_cw03
         PTRARRAYaw(04,0044) = p_S03U02Y06_cw04

         PTRARRAYaw(01,0045) = p_S03U02Y20_cw01
         PTRARRAYaw(02,0045) = p_S03U02Y20_cw02
         PTRARRAYaw(03,0045) = p_S03U02Y20_cw03
         PTRARRAYaw(04,0045) = p_S03U02Y20_cw04

         PTRARRAYaw(01,0046) = p_S03X00Y03_cw01
         PTRARRAYaw(02,0046) = p_S03X00Y03_cw02
         PTRARRAYaw(03,0046) = p_S03X00Y03_cw03
         PTRARRAYaw(04,0046) = p_S03X00Y03_cw04

         PTRARRAYaw(01,0047) = p_S03X00Y06_cw01
         PTRARRAYaw(02,0047) = p_S03X00Y06_cw02
         PTRARRAYaw(03,0047) = p_S03X00Y06_cw03
         PTRARRAYaw(04,0047) = p_S03X00Y06_cw04

         PTRARRAYaw(01,0048) = p_S03X00Y20_cw01
         PTRARRAYaw(02,0048) = p_S03X00Y20_cw02
         PTRARRAYaw(03,0048) = p_S03X00Y20_cw03
         PTRARRAYaw(04,0048) = p_S03X00Y20_cw04

         PTRARRAYaw(01,0049) = p_S03X01Y03_cw01
         PTRARRAYaw(02,0049) = p_S03X01Y03_cw02
         PTRARRAYaw(03,0049) = p_S03X01Y03_cw03
         PTRARRAYaw(04,0049) = p_S03X01Y03_cw04

         PTRARRAYaw(01,0050) = p_S03X01Y06_cw01
         PTRARRAYaw(02,0050) = p_S03X01Y06_cw02
         PTRARRAYaw(03,0050) = p_S03X01Y06_cw03
         PTRARRAYaw(04,0050) = p_S03X01Y06_cw04

         PTRARRAYaw(01,0051) = p_S03X01Y20_cw01
         PTRARRAYaw(02,0051) = p_S03X01Y20_cw02
         PTRARRAYaw(03,0051) = p_S03X01Y20_cw03
         PTRARRAYaw(04,0051) = p_S03X01Y20_cw04

         PTRARRAYaw(01,0052) = p_S03X03Y03_cw01
         PTRARRAYaw(02,0052) = p_S03X03Y03_cw02
         PTRARRAYaw(03,0052) = p_S03X03Y03_cw03
         PTRARRAYaw(04,0052) = p_S03X03Y03_cw04

         PTRARRAYaw(01,0053) = p_S03X03Y06_cw01
         PTRARRAYaw(02,0053) = p_S03X03Y06_cw02
         PTRARRAYaw(03,0053) = p_S03X03Y06_cw03
         PTRARRAYaw(04,0053) = p_S03X03Y06_cw04

         PTRARRAYaw(01,0054) = p_S03X03Y20_cw01
         PTRARRAYaw(02,0054) = p_S03X03Y20_cw02
         PTRARRAYaw(03,0054) = p_S03X03Y20_cw03
         PTRARRAYaw(04,0054) = p_S03X03Y20_cw04

         PTRARRAYaw(01,0055) = p_S04U02Y03_cw01
         PTRARRAYaw(02,0055) = p_S04U02Y03_cw02
         PTRARRAYaw(03,0055) = p_S04U02Y03_cw03
         PTRARRAYaw(04,0055) = p_S04U02Y03_cw04

         PTRARRAYaw(01,0056) = p_S04U02Y06_cw01
         PTRARRAYaw(02,0056) = p_S04U02Y06_cw02
         PTRARRAYaw(03,0056) = p_S04U02Y06_cw03
         PTRARRAYaw(04,0056) = p_S04U02Y06_cw04

         PTRARRAYaw(01,0057) = p_S04U02Y20_cw01
         PTRARRAYaw(02,0057) = p_S04U02Y20_cw02
         PTRARRAYaw(03,0057) = p_S04U02Y20_cw03
         PTRARRAYaw(04,0057) = p_S04U02Y20_cw04

         PTRARRAYaw(01,0058) = p_S04X00Y03_cw01
         PTRARRAYaw(02,0058) = p_S04X00Y03_cw02
         PTRARRAYaw(03,0058) = p_S04X00Y03_cw03
         PTRARRAYaw(04,0058) = p_S04X00Y03_cw04

         PTRARRAYaw(01,0059) = p_S04X00Y06_cw01
         PTRARRAYaw(02,0059) = p_S04X00Y06_cw02
         PTRARRAYaw(03,0059) = p_S04X00Y06_cw03
         PTRARRAYaw(04,0059) = p_S04X00Y06_cw04

         PTRARRAYaw(01,0060) = p_S04X00Y20_cw01
         PTRARRAYaw(02,0060) = p_S04X00Y20_cw02
         PTRARRAYaw(03,0060) = p_S04X00Y20_cw03
         PTRARRAYaw(04,0060) = p_S04X00Y20_cw04

         PTRARRAYaw(01,0061) = p_S04X01Y03_cw01
         PTRARRAYaw(02,0061) = p_S04X01Y03_cw02
         PTRARRAYaw(03,0061) = p_S04X01Y03_cw03
         PTRARRAYaw(04,0061) = p_S04X01Y03_cw04

         PTRARRAYaw(01,0062) = p_S04X01Y06_cw01
         PTRARRAYaw(02,0062) = p_S04X01Y06_cw02
         PTRARRAYaw(03,0062) = p_S04X01Y06_cw03
         PTRARRAYaw(04,0062) = p_S04X01Y06_cw04

         PTRARRAYaw(01,0063) = p_S04X01Y20_cw01
         PTRARRAYaw(02,0063) = p_S04X01Y20_cw02
         PTRARRAYaw(03,0063) = p_S04X01Y20_cw03
         PTRARRAYaw(04,0063) = p_S04X01Y20_cw04

         PTRARRAYaw(01,0064) = p_S04X03Y03_cw01
         PTRARRAYaw(02,0064) = p_S04X03Y03_cw02
         PTRARRAYaw(03,0064) = p_S04X03Y03_cw03
         PTRARRAYaw(04,0064) = p_S04X03Y03_cw04

         PTRARRAYaw(01,0065) = p_S04X03Y06_cw01
         PTRARRAYaw(02,0065) = p_S04X03Y06_cw02
         PTRARRAYaw(03,0065) = p_S04X03Y06_cw03
         PTRARRAYaw(04,0065) = p_S04X03Y06_cw04

         PTRARRAYaw(01,0066) = p_S04X03Y20_cw01
         PTRARRAYaw(02,0066) = p_S04X03Y20_cw02
         PTRARRAYaw(03,0066) = p_S04X03Y20_cw03
         PTRARRAYaw(04,0066) = p_S04X03Y20_cw04

         PTRARRAYaw(01,0067) = p_S05U02Y03_cw01
         PTRARRAYaw(02,0067) = p_S05U02Y03_cw02
         PTRARRAYaw(03,0067) = p_S05U02Y03_cw03
         PTRARRAYaw(04,0067) = p_S05U02Y03_cw04

         PTRARRAYaw(01,0068) = p_S05U02Y05_cw01
         PTRARRAYaw(02,0068) = p_S05U02Y05_cw02
         PTRARRAYaw(03,0068) = p_S05U02Y05_cw03
         PTRARRAYaw(04,0068) = p_S05U02Y05_cw04

         PTRARRAYaw(01,0069) = p_S05U02Y20_cw01
         PTRARRAYaw(02,0069) = p_S05U02Y20_cw02
         PTRARRAYaw(03,0069) = p_S05U02Y20_cw03
         PTRARRAYaw(04,0069) = p_S05U02Y20_cw04

         PTRARRAYaw(01,0070) = p_S05X00Y03_cw01
         PTRARRAYaw(02,0070) = p_S05X00Y03_cw02
         PTRARRAYaw(03,0070) = p_S05X00Y03_cw03
         PTRARRAYaw(04,0070) = p_S05X00Y03_cw04

         PTRARRAYaw(01,0071) = p_S05X00Y05_cw01
         PTRARRAYaw(02,0071) = p_S05X00Y05_cw02
         PTRARRAYaw(03,0071) = p_S05X00Y05_cw03
         PTRARRAYaw(04,0071) = p_S05X00Y05_cw04

         PTRARRAYaw(01,0072) = p_S05X00Y20_cw01
         PTRARRAYaw(02,0072) = p_S05X00Y20_cw02
         PTRARRAYaw(03,0072) = p_S05X00Y20_cw03
         PTRARRAYaw(04,0072) = p_S05X00Y20_cw04

         PTRARRAYaw(01,0073) = p_S05X01Y03_cw01
         PTRARRAYaw(02,0073) = p_S05X01Y03_cw02
         PTRARRAYaw(03,0073) = p_S05X01Y03_cw03
         PTRARRAYaw(04,0073) = p_S05X01Y03_cw04

         PTRARRAYaw(01,0074) = p_S05X01Y05_cw01
         PTRARRAYaw(02,0074) = p_S05X01Y05_cw02
         PTRARRAYaw(03,0074) = p_S05X01Y05_cw03
         PTRARRAYaw(04,0074) = p_S05X01Y05_cw04

         PTRARRAYaw(01,0075) = p_S05X01Y20_cw01
         PTRARRAYaw(02,0075) = p_S05X01Y20_cw02
         PTRARRAYaw(03,0075) = p_S05X01Y20_cw03
         PTRARRAYaw(04,0075) = p_S05X01Y20_cw04

         PTRARRAYaw(01,0076) = p_S05X03Y03_cw01
         PTRARRAYaw(02,0076) = p_S05X03Y03_cw02
         PTRARRAYaw(03,0076) = p_S05X03Y03_cw03
         PTRARRAYaw(04,0076) = p_S05X03Y03_cw04

         PTRARRAYaw(01,0077) = p_S05X03Y05_cw01
         PTRARRAYaw(02,0077) = p_S05X03Y05_cw02
         PTRARRAYaw(03,0077) = p_S05X03Y05_cw03
         PTRARRAYaw(04,0077) = p_S05X03Y05_cw04

         PTRARRAYaw(01,0078) = p_S05X03Y20_cw01
         PTRARRAYaw(02,0078) = p_S05X03Y20_cw02
         PTRARRAYaw(03,0078) = p_S05X03Y20_cw03
         PTRARRAYaw(04,0078) = p_S05X03Y20_cw04

         PTRARRAYaw(01,0079) = p_S06U02Y03_cw01
         PTRARRAYaw(02,0079) = p_S06U02Y03_cw02
         PTRARRAYaw(03,0079) = p_S06U02Y03_cw03
         PTRARRAYaw(04,0079) = p_S06U02Y03_cw04

         PTRARRAYaw(01,0080) = p_S06U02Y06_cw01
         PTRARRAYaw(02,0080) = p_S06U02Y06_cw02
         PTRARRAYaw(03,0080) = p_S06U02Y06_cw03
         PTRARRAYaw(04,0080) = p_S06U02Y06_cw04

         PTRARRAYaw(01,0081) = p_S06U02Y20_cw01
         PTRARRAYaw(02,0081) = p_S06U02Y20_cw02
         PTRARRAYaw(03,0081) = p_S06U02Y20_cw03
         PTRARRAYaw(04,0081) = p_S06U02Y20_cw04

         PTRARRAYaw(01,0082) = p_S06X00Y03_cw01
         PTRARRAYaw(02,0082) = p_S06X00Y03_cw02
         PTRARRAYaw(03,0082) = p_S06X00Y03_cw03
         PTRARRAYaw(04,0082) = p_S06X00Y03_cw04

         PTRARRAYaw(01,0083) = p_S06X00Y06_cw01
         PTRARRAYaw(02,0083) = p_S06X00Y06_cw02
         PTRARRAYaw(03,0083) = p_S06X00Y06_cw03
         PTRARRAYaw(04,0083) = p_S06X00Y06_cw04

         PTRARRAYaw(01,0084) = p_S06X00Y20_cw01
         PTRARRAYaw(02,0084) = p_S06X00Y20_cw02
         PTRARRAYaw(03,0084) = p_S06X00Y20_cw03
         PTRARRAYaw(04,0084) = p_S06X00Y20_cw04

         PTRARRAYaw(01,0085) = p_S06X01Y03_cw01
         PTRARRAYaw(02,0085) = p_S06X01Y03_cw02
         PTRARRAYaw(03,0085) = p_S06X01Y03_cw03
         PTRARRAYaw(04,0085) = p_S06X01Y03_cw04

         PTRARRAYaw(01,0086) = p_S06X01Y06_cw01
         PTRARRAYaw(02,0086) = p_S06X01Y06_cw02
         PTRARRAYaw(03,0086) = p_S06X01Y06_cw03
         PTRARRAYaw(04,0086) = p_S06X01Y06_cw04

         PTRARRAYaw(01,0087) = p_S06X01Y20_cw01
         PTRARRAYaw(02,0087) = p_S06X01Y20_cw02
         PTRARRAYaw(03,0087) = p_S06X01Y20_cw03
         PTRARRAYaw(04,0087) = p_S06X01Y20_cw04

         PTRARRAYaw(01,0088) = p_S06X03Y03_cw01
         PTRARRAYaw(02,0088) = p_S06X03Y03_cw02
         PTRARRAYaw(03,0088) = p_S06X03Y03_cw03
         PTRARRAYaw(04,0088) = p_S06X03Y03_cw04

         PTRARRAYaw(01,0089) = p_S06X03Y06_cw01
         PTRARRAYaw(02,0089) = p_S06X03Y06_cw02
         PTRARRAYaw(03,0089) = p_S06X03Y06_cw03
         PTRARRAYaw(04,0089) = p_S06X03Y06_cw04

         PTRARRAYaw(01,0090) = p_S06X03Y20_cw01
         PTRARRAYaw(02,0090) = p_S06X03Y20_cw02
         PTRARRAYaw(03,0090) = p_S06X03Y20_cw03
         PTRARRAYaw(04,0090) = p_S06X03Y20_cw04

         PTRARRAYaw(01,0091) = p_S07U02Y00_cw01
         PTRARRAYaw(02,0091) = p_S07U02Y00_cw02
         PTRARRAYaw(03,0091) = p_S07U02Y00_cw03
         PTRARRAYaw(04,0091) = p_S07U02Y00_cw04

         PTRARRAYaw(01,0092) = p_S07U02Y05_cw01
         PTRARRAYaw(02,0092) = p_S07U02Y05_cw02
         PTRARRAYaw(03,0092) = p_S07U02Y05_cw03
         PTRARRAYaw(04,0092) = p_S07U02Y05_cw04

         PTRARRAYaw(01,0093) = p_S07U02Y10_cw01
         PTRARRAYaw(02,0093) = p_S07U02Y10_cw02
         PTRARRAYaw(03,0093) = p_S07U02Y10_cw03
         PTRARRAYaw(04,0093) = p_S07U02Y10_cw04

         PTRARRAYaw(01,0094) = p_S07X00Y00_cw01
         PTRARRAYaw(02,0094) = p_S07X00Y00_cw02
         PTRARRAYaw(03,0094) = p_S07X00Y00_cw03
         PTRARRAYaw(04,0094) = p_S07X00Y00_cw04

         PTRARRAYaw(01,0095) = p_S07X00Y05_cw01
         PTRARRAYaw(02,0095) = p_S07X00Y05_cw02
         PTRARRAYaw(03,0095) = p_S07X00Y05_cw03
         PTRARRAYaw(04,0095) = p_S07X00Y05_cw04

         PTRARRAYaw(01,0096) = p_S07X00Y10_cw01
         PTRARRAYaw(02,0096) = p_S07X00Y10_cw02
         PTRARRAYaw(03,0096) = p_S07X00Y10_cw03
         PTRARRAYaw(04,0096) = p_S07X00Y10_cw04

         PTRARRAYaw(01,0097) = p_S07X01Y00_cw01
         PTRARRAYaw(02,0097) = p_S07X01Y00_cw02
         PTRARRAYaw(03,0097) = p_S07X01Y00_cw03
         PTRARRAYaw(04,0097) = p_S07X01Y00_cw04

         PTRARRAYaw(01,0098) = p_S07X01Y05_cw01
         PTRARRAYaw(02,0098) = p_S07X01Y05_cw02
         PTRARRAYaw(03,0098) = p_S07X01Y05_cw03
         PTRARRAYaw(04,0098) = p_S07X01Y05_cw04

         PTRARRAYaw(01,0099) = p_S07X01Y10_cw01
         PTRARRAYaw(02,0099) = p_S07X01Y10_cw02
         PTRARRAYaw(03,0099) = p_S07X01Y10_cw03
         PTRARRAYaw(04,0099) = p_S07X01Y10_cw04

         PTRARRAYaw(01,0100) = p_S07X03Y00_cw01
         PTRARRAYaw(02,0100) = p_S07X03Y00_cw02
         PTRARRAYaw(03,0100) = p_S07X03Y00_cw03
         PTRARRAYaw(04,0100) = p_S07X03Y00_cw04

         PTRARRAYaw(01,0101) = p_S07X03Y05_cw01
         PTRARRAYaw(02,0101) = p_S07X03Y05_cw02
         PTRARRAYaw(03,0101) = p_S07X03Y05_cw03
         PTRARRAYaw(04,0101) = p_S07X03Y05_cw04

         PTRARRAYaw(01,0102) = p_S07X03Y10_cw01
         PTRARRAYaw(02,0102) = p_S07X03Y10_cw02
         PTRARRAYaw(03,0102) = p_S07X03Y10_cw03
         PTRARRAYaw(04,0102) = p_S07X03Y10_cw04

!        DEFINE POINTER ARRAY: AEROSOL NUMBER
!        ===============================================================
         PTRARRAY_NUM(01) = p_NUM_a01
         PTRARRAY_NUM(02) = p_NUM_a02
         PTRARRAY_NUM(03) = p_NUM_a03
         PTRARRAY_NUM(04) = p_NUM_a04

!        DEFINE POINTER ARRAY: AEROSOL NUMBER IN CLOUD-PHASE
!        ===============================================================
         PTRARRAYnw(01) = p_NUM_cw01
         PTRARRAYnw(02) = p_NUM_cw02
         PTRARRAYnw(03) = p_NUM_cw03
         PTRARRAYnw(04) = p_NUM_cw04

!        SET ARRAY: WET AEROSOL BOOLEAN
!        ===============================================================
         ARRAY_WETBOOL(0001) = 0
         ARRAY_WETBOOL(0002) = 0
         ARRAY_WETBOOL(0003) = 0
         ARRAY_WETBOOL(0004) = 0
         ARRAY_WETBOOL(0005) = 0
         ARRAY_WETBOOL(0006) = 0
         ARRAY_WETBOOL(0007) = 0
         ARRAY_WETBOOL(0008) = 0
         ARRAY_WETBOOL(0009) = 1
         ARRAY_WETBOOL(0010) = 1
         ARRAY_WETBOOL(0011) = 0
         ARRAY_WETBOOL(0012) = 0
         ARRAY_WETBOOL(0013) = 0
         ARRAY_WETBOOL(0014) = 0
         ARRAY_WETBOOL(0015) = 0
         ARRAY_WETBOOL(0016) = 0
         ARRAY_WETBOOL(0017) = 0
         ARRAY_WETBOOL(0018) = 0
         ARRAY_WETBOOL(0019) = 0
         ARRAY_WETBOOL(0020) = 0
         ARRAY_WETBOOL(0021) = 0
         ARRAY_WETBOOL(0022) = 0
         ARRAY_WETBOOL(0023) = 0
         ARRAY_WETBOOL(0024) = 0
         ARRAY_WETBOOL(0025) = 0
         ARRAY_WETBOOL(0026) = 0
         ARRAY_WETBOOL(0027) = 0
         ARRAY_WETBOOL(0028) = 0
         ARRAY_WETBOOL(0029) = 0
         ARRAY_WETBOOL(0030) = 0
         ARRAY_WETBOOL(0031) = 0
         ARRAY_WETBOOL(0032) = 0
         ARRAY_WETBOOL(0033) = 0
         ARRAY_WETBOOL(0034) = 0
         ARRAY_WETBOOL(0035) = 0
         ARRAY_WETBOOL(0036) = 0
         ARRAY_WETBOOL(0037) = 0
         ARRAY_WETBOOL(0038) = 0
         ARRAY_WETBOOL(0039) = 0
         ARRAY_WETBOOL(0040) = 0
         ARRAY_WETBOOL(0041) = 0
         ARRAY_WETBOOL(0042) = 0
         ARRAY_WETBOOL(0043) = 0
         ARRAY_WETBOOL(0044) = 0
         ARRAY_WETBOOL(0045) = 0
         ARRAY_WETBOOL(0046) = 0
         ARRAY_WETBOOL(0047) = 0
         ARRAY_WETBOOL(0048) = 0
         ARRAY_WETBOOL(0049) = 0
         ARRAY_WETBOOL(0050) = 0
         ARRAY_WETBOOL(0051) = 0
         ARRAY_WETBOOL(0052) = 0
         ARRAY_WETBOOL(0053) = 0
         ARRAY_WETBOOL(0054) = 0
         ARRAY_WETBOOL(0055) = 0
         ARRAY_WETBOOL(0056) = 0
         ARRAY_WETBOOL(0057) = 0
         ARRAY_WETBOOL(0058) = 0
         ARRAY_WETBOOL(0059) = 0
         ARRAY_WETBOOL(0060) = 0
         ARRAY_WETBOOL(0061) = 0
         ARRAY_WETBOOL(0062) = 0
         ARRAY_WETBOOL(0063) = 0
         ARRAY_WETBOOL(0064) = 0
         ARRAY_WETBOOL(0065) = 0
         ARRAY_WETBOOL(0066) = 0
         ARRAY_WETBOOL(0067) = 0
         ARRAY_WETBOOL(0068) = 0
         ARRAY_WETBOOL(0069) = 0
         ARRAY_WETBOOL(0070) = 0
         ARRAY_WETBOOL(0071) = 0
         ARRAY_WETBOOL(0072) = 0
         ARRAY_WETBOOL(0073) = 0
         ARRAY_WETBOOL(0074) = 0
         ARRAY_WETBOOL(0075) = 0
         ARRAY_WETBOOL(0076) = 0
         ARRAY_WETBOOL(0077) = 0
         ARRAY_WETBOOL(0078) = 0
         ARRAY_WETBOOL(0079) = 0
         ARRAY_WETBOOL(0080) = 0
         ARRAY_WETBOOL(0081) = 0
         ARRAY_WETBOOL(0082) = 0
         ARRAY_WETBOOL(0083) = 0
         ARRAY_WETBOOL(0084) = 0
         ARRAY_WETBOOL(0085) = 0
         ARRAY_WETBOOL(0086) = 0
         ARRAY_WETBOOL(0087) = 0
         ARRAY_WETBOOL(0088) = 0
         ARRAY_WETBOOL(0089) = 0
         ARRAY_WETBOOL(0090) = 0
         ARRAY_WETBOOL(0091) = 0
         ARRAY_WETBOOL(0092) = 0
         ARRAY_WETBOOL(0093) = 0
         ARRAY_WETBOOL(0094) = 0
         ARRAY_WETBOOL(0095) = 0
         ARRAY_WETBOOL(0096) = 0
         ARRAY_WETBOOL(0097) = 0
         ARRAY_WETBOOL(0098) = 0
         ARRAY_WETBOOL(0099) = 0
         ARRAY_WETBOOL(0100) = 0
         ARRAY_WETBOOL(0101) = 0
         ARRAY_WETBOOL(0102) = 0

!        SET ARRAY: AEROSOL DENSITIES [kg m-3]
!        ===============================================================
         ARRAY_AERODENS(0001) = 1800.0
         ARRAY_AERODENS(0002) = 1800.0
         ARRAY_AERODENS(0003) = 2200.0
         ARRAY_AERODENS(0004) = 1800.0
         ARRAY_AERODENS(0005) = 2200.0
         ARRAY_AERODENS(0006) = 2600.0
         ARRAY_AERODENS(0007) = 1000.0
         ARRAY_AERODENS(0008) = 1700.0
         ARRAY_AERODENS(0009) = 1000.0
         ARRAY_AERODENS(0010) = 1000.0
         ARRAY_AERODENS(0011) = 1400.0
         ARRAY_AERODENS(0012) = 1400.0
         ARRAY_AERODENS(0013) = 1400.0
         ARRAY_AERODENS(0014) = 1400.0
         ARRAY_AERODENS(0015) = 1400.0
         ARRAY_AERODENS(0016) = 1400.0
         ARRAY_AERODENS(0017) = 1400.0
         ARRAY_AERODENS(0018) = 1400.0
         ARRAY_AERODENS(0019) = 1400.0
         ARRAY_AERODENS(0020) = 1400.0
         ARRAY_AERODENS(0021) = 1400.0
         ARRAY_AERODENS(0022) = 1400.0
         ARRAY_AERODENS(0023) = 1400.0
         ARRAY_AERODENS(0024) = 1400.0
         ARRAY_AERODENS(0025) = 1400.0
         ARRAY_AERODENS(0026) = 1400.0
         ARRAY_AERODENS(0027) = 1400.0
         ARRAY_AERODENS(0028) = 1400.0
         ARRAY_AERODENS(0029) = 1400.0
         ARRAY_AERODENS(0030) = 1400.0
         ARRAY_AERODENS(0031) = 1400.0
         ARRAY_AERODENS(0032) = 1400.0
         ARRAY_AERODENS(0033) = 1400.0
         ARRAY_AERODENS(0034) = 1400.0
         ARRAY_AERODENS(0035) = 1400.0
         ARRAY_AERODENS(0036) = 1400.0
         ARRAY_AERODENS(0037) = 1400.0
         ARRAY_AERODENS(0038) = 1400.0
         ARRAY_AERODENS(0039) = 1400.0
         ARRAY_AERODENS(0040) = 1400.0
         ARRAY_AERODENS(0041) = 1400.0
         ARRAY_AERODENS(0042) = 1400.0
         ARRAY_AERODENS(0043) = 1400.0
         ARRAY_AERODENS(0044) = 1400.0
         ARRAY_AERODENS(0045) = 1400.0
         ARRAY_AERODENS(0046) = 1400.0
         ARRAY_AERODENS(0047) = 1400.0
         ARRAY_AERODENS(0048) = 1400.0
         ARRAY_AERODENS(0049) = 1400.0
         ARRAY_AERODENS(0050) = 1400.0
         ARRAY_AERODENS(0051) = 1400.0
         ARRAY_AERODENS(0052) = 1400.0
         ARRAY_AERODENS(0053) = 1400.0
         ARRAY_AERODENS(0054) = 1400.0
         ARRAY_AERODENS(0055) = 1400.0
         ARRAY_AERODENS(0056) = 1400.0
         ARRAY_AERODENS(0057) = 1400.0
         ARRAY_AERODENS(0058) = 1400.0
         ARRAY_AERODENS(0059) = 1400.0
         ARRAY_AERODENS(0060) = 1400.0
         ARRAY_AERODENS(0061) = 1400.0
         ARRAY_AERODENS(0062) = 1400.0
         ARRAY_AERODENS(0063) = 1400.0
         ARRAY_AERODENS(0064) = 1400.0
         ARRAY_AERODENS(0065) = 1400.0
         ARRAY_AERODENS(0066) = 1400.0
         ARRAY_AERODENS(0067) = 1400.0
         ARRAY_AERODENS(0068) = 1400.0
         ARRAY_AERODENS(0069) = 1400.0
         ARRAY_AERODENS(0070) = 1400.0
         ARRAY_AERODENS(0071) = 1400.0
         ARRAY_AERODENS(0072) = 1400.0
         ARRAY_AERODENS(0073) = 1400.0
         ARRAY_AERODENS(0074) = 1400.0
         ARRAY_AERODENS(0075) = 1400.0
         ARRAY_AERODENS(0076) = 1400.0
         ARRAY_AERODENS(0077) = 1400.0
         ARRAY_AERODENS(0078) = 1400.0
         ARRAY_AERODENS(0079) = 1400.0
         ARRAY_AERODENS(0080) = 1400.0
         ARRAY_AERODENS(0081) = 1400.0
         ARRAY_AERODENS(0082) = 1400.0
         ARRAY_AERODENS(0083) = 1400.0
         ARRAY_AERODENS(0084) = 1400.0
         ARRAY_AERODENS(0085) = 1400.0
         ARRAY_AERODENS(0086) = 1400.0
         ARRAY_AERODENS(0087) = 1400.0
         ARRAY_AERODENS(0088) = 1400.0
         ARRAY_AERODENS(0089) = 1400.0
         ARRAY_AERODENS(0090) = 1400.0
         ARRAY_AERODENS(0091) = 1400.0
         ARRAY_AERODENS(0092) = 1400.0
         ARRAY_AERODENS(0093) = 1400.0
         ARRAY_AERODENS(0094) = 1400.0
         ARRAY_AERODENS(0095) = 1400.0
         ARRAY_AERODENS(0096) = 1400.0
         ARRAY_AERODENS(0097) = 1400.0
         ARRAY_AERODENS(0098) = 1400.0
         ARRAY_AERODENS(0099) = 1400.0
         ARRAY_AERODENS(0100) = 1400.0
         ARRAY_AERODENS(0101) = 1400.0
         ARRAY_AERODENS(0102) = 1400.0

!        SET ARRAY: STACKED EMISSION MODES
!        ===============================================================
         ARRxEMISSMODE(01,:) = M01
         ARRxEMISSMODE(02,:) = M02
         ARRxEMISSMODE(03,:) = M03
         ARRxEMISSMODE(04,:) = M04
         ARRxEMISSMODE(05,:) = M05
         ARRxEMISSMODE(06,:) = M06
         ARRxEMISSMODE(07,:) = M07

!        SET COLUMN (EM-BB): INDEX FOR TARGETS
!        ===============================================================
         df_EMbb_COL01(0001) = 0006
         df_EMbb_COL01(0002) = 0006
         df_EMbb_COL01(0003) = 0008
         df_EMbb_COL01(0004) = 0016
         df_EMbb_COL01(0005) = 0017
         df_EMbb_COL01(0006) = 0018

!        SET COLUMN (EM-AA): INDEX FOR TARGETS
!        ===============================================================
         df_EMaa_COL01(0001) = 0006
         df_EMaa_COL01(0002) = 0006
         df_EMaa_COL01(0003) = 0006
         df_EMaa_COL01(0004) = 0008
         df_EMaa_COL01(0005) = 0008
         df_EMaa_COL01(0006) = 0016
         df_EMaa_COL01(0007) = 0017
         df_EMaa_COL01(0008) = 0018

!        SET COLUMN (EM-BB): INDEX FOR SOURCES
!        ===============================================================
         df_EMbb_COL02(0001) = p_EBU_PM25
         df_EMbb_COL02(0002) = p_EBU_PM10
         df_EMbb_COL02(0003) = p_EBU_BC
         df_EMbb_COL02(0004) = p_EBU_P07U03Y06
         df_EMbb_COL02(0005) = p_EBU_P07X00Y06
         df_EMbb_COL02(0006) = p_EBU_P07X03Y06

!        SET COLUMN (EM-AA): INDEX FOR SOURCES
!        ===============================================================
         df_EMaa_COL02(0001) = p_E_PM25i
         df_EMaa_COL02(0002) = p_E_PM25j
         df_EMaa_COL02(0003) = p_E_PM10
         df_EMaa_COL02(0004) = p_E_ECi
         df_EMaa_COL02(0005) = p_E_ECj
         df_EMaa_COL02(0006) = p_E_P07U03Y06
         df_EMaa_COL02(0007) = p_E_P07X00Y06
         df_EMaa_COL02(0008) = p_E_P07X03Y06

!        SET COLUMN (EM-BB): INDEX FOR EMISSION MASS MODE
!        ===============================================================
         df_EMbb_COL03(0001) = 0006
         df_EMbb_COL03(0002) = 0007
         df_EMbb_COL03(0003) = 0006
         df_EMbb_COL03(0004) = 0006
         df_EMbb_COL03(0005) = 0006
         df_EMbb_COL03(0006) = 0006

!        SET COLUMN (EM-AA): INDEX FOR EMISSION MASS MODE
!        ===============================================================
         df_EMaa_COL03(0001) = 0001
         df_EMaa_COL03(0002) = 0002
         df_EMaa_COL03(0003) = 0003
         df_EMaa_COL03(0004) = 0001
         df_EMaa_COL03(0005) = 0002
         df_EMaa_COL03(0006) = 0006
         df_EMaa_COL03(0007) = 0006
         df_EMaa_COL03(0008) = 0006

!        SET COLUMN (EM-BB): EMISSION PHASE
!        ===============================================================
         df_EMbb_COL04(0001) = 0000
         df_EMbb_COL04(0002) = 0000
         df_EMbb_COL04(0003) = 0000
         df_EMbb_COL04(0004) = 0001
         df_EMbb_COL04(0005) = 0001
         df_EMbb_COL04(0006) = 0001

!        SET COLUMN (EM-AA): EMISSION PHASE
!        ===============================================================
         df_EMaa_COL04(0001) = 0000
         df_EMaa_COL04(0002) = 0000
         df_EMaa_COL04(0003) = 0000
         df_EMaa_COL04(0004) = 0000
         df_EMaa_COL04(0005) = 0000
         df_EMaa_COL04(0006) = 0001
         df_EMaa_COL04(0007) = 0001
         df_EMaa_COL04(0008) = 0001

!        SET COLUMN (EM-BB): VOLATILITY
!        ===============================================================
         df_EMbb_COL05(0001) = -9.99e+02
         df_EMbb_COL05(0002) = -9.99e+02
         df_EMbb_COL05(0003) = -9.99e+02
         df_EMbb_COL05(0004) = -3.00e+00
         df_EMbb_COL05(0005) = 0.00e+00
         df_EMbb_COL05(0006) = 3.00e+00

!        SET COLUMN (EM-AA): VOLATILITY
!        ===============================================================
         df_EMaa_COL05(0001) = -9.99e+02
         df_EMaa_COL05(0002) = -9.99e+02
         df_EMaa_COL05(0003) = -9.99e+02
         df_EMaa_COL05(0004) = -9.99e+02
         df_EMaa_COL05(0005) = -9.99e+02
         df_EMaa_COL05(0006) = -3.00e+00
         df_EMaa_COL05(0007) = 0.00e+00
         df_EMaa_COL05(0008) = 3.00e+00

!        SET COLUMN (EM-BB): MOLECULAR WEIGHT
!        ===============================================================
         df_EMbb_COL06(0001) = 1.00e+00
         df_EMbb_COL06(0002) = 1.00e+00
         df_EMbb_COL06(0003) = 1.00e+00
         df_EMbb_COL06(0004) = 2.02e+02
         df_EMbb_COL06(0005) = 1.63e+02
         df_EMbb_COL06(0006) = 1.24e+02
        
!        SET COLUMN (EM-AA): MOLECULAR WEIGHT
!        ===============================================================
         df_EMaa_COL06(0001) = 1.00e+00
         df_EMaa_COL06(0002) = 1.00e+00
         df_EMaa_COL06(0003) = 1.00e+00
         df_EMaa_COL06(0004) = 1.00e+00
         df_EMaa_COL06(0005) = 1.00e+00
         df_EMaa_COL06(0006) = 2.02e+02
         df_EMaa_COL06(0007) = 1.63e+02
         df_EMaa_COL06(0008) = 1.24e+02

      END SUBROUTINE f_INIT_PTRARRAY


      END MODULE mod_AERO_CONFIG
