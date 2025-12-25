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
      INTEGER,PARAMETER :: nCOMP = 0000 !<FLAG>

!     NUMBER OF SIZE BINS:
      INTEGER,PARAMETER :: nBINS = 0000 !<FLAG>

!     NUMBER OF KINETIC ORGANIC SPECIES:
      INTEGER,PARAMETER :: nKNTOA = 0000 !<FLAG>

!     NUMBER OF EMISSION TARGET-SOURCE PAIRS FOR 
!     BIOMASS BURNING AND ANTHROPOGENIC SOURCES:
      INTEGER,PARAMETER :: nEMISSbb = 0000 !<FLAG>
      INTEGER,PARAMETER :: nEMISSaa = 0000 !<FLAG>

!     NUMBER OF EMISSION MASS MODES:
      INTEGER,PARAMETER :: nEMISSMODE = 0000 !<FLAG>

!     DEFINE: SIZE BINS
!     ==================================================================
!     LOWEST AND HIGHEST SIZE BOUND [nm]:
      REAL(8),PARAMETER :: LOWERx = 0000 !<FLAG>
      REAL(8),PARAMETER :: UPPERx = 0000 !<FLAG>

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
!     <EMISS_MODES>

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
!        <df_AERO: APPEND-PTRaa>

!        SET COLUMNS:
!        <df_AERO: APPEND-PTRcw>

!        SET COLUMN: df_AERO; SPECIES NAMES
!        ===============================================================
!        <df_AERO; NAMES>

!        SET COLUMN: df_AERO; MOLECULAR WEIGHT [g mol-1]
!        ===============================================================
!        <df_AERO; MW>

!        SET COLUMN: df_AERO; DENSITY [kg m-3]
!        ===============================================================
!        <df_AERO; RHO>

!        SET COLUMN: df_AERO; WET FLAG
!        ===============================================================
!        <df_AERO; IF-WET>

!        SET COLUMN: df_AERO; ABSORBING FLAG
!        ===============================================================
!        <df_AERO; IF-SORB>

!        SET COLUMN: df_AERO COLUMN={if-CLOUD-ACT}
!        NOTE: FOR WHETHER THE SPECIES EXISTS IN ACTIVATED DROPLETS
!        ===============================================================
!        <df_AERO; IF-CLOUD-ACT>

!        SET COLUMN: df_AERO; AEROSOL-PHASE POINTERS
!        ===============================================================
!        <df_AERO; PTRaa>

!        SET COLUMN: df_AERO; CLOUD-PHASE POINTERS
!        ===============================================================
!        <df_AERO; PTRcw>

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
!        <df_BINS; PTRnn>

!        SET COLUMN: df_BINS; CLOUD-PHASE POINTERS
!        ===============================================================
!        <df_BINS; PTRcw>

!        SET COLUMN: df_BINS; EMISSION FRACTIONS
!        ===============================================================
!        <df_BINS; EM>

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
!        <df_KNTOA: APPEND-PTRaa>

!        SET COLUMN: df_KNTOA; SPECIES NAMES
!        ===============================================================
!        <df_KNTOA; NAMES>

!        SET COLUMN: df_KNTOA; POINTERS TO GAS-PHASE SPECIES
!        ===============================================================
!        <df_KNTOA; PTRqq>

!        SET COLUMN: df_KNTOA; VOLATILITY
!        ===============================================================
!        <df_KNTOA; CSAT>

!        SET COLUMN: df_KNTOA; MOLECULAR WEIGHT
!        ===============================================================
!        <df_KNTOA; MW>

!        SET COLUMN: df_KNTOA; POINTERS TO AEROSOL-PHASE SPECIES
!        ===============================================================
!        <df_KNTOA; PTRaa>






!        SET COLUMN (AERO-COL01): SPECIES NAMES
!        ===============================================================
!        <df_AERO_COL01>

!        SET COLUMN (AERO-COL02): CHEM-INDEX
!        ===============================================================
!        <df_AERO_COL02>

!        SET COLUMN (AERO-COL03): MOLECULAR WEIGHT
!        ===============================================================
!        <df_AERO_COL03>

!        SET COLUMN (AERO-COL04): DENSITY
!        ===============================================================
!        <df_AERO_COL04>

!        SET COLUMN (AERO-COL05): if-WET
!        ===============================================================
!        <df_AERO_COL05>

!        SET COLUMN (AERO-COL06): IF ORGANIC SPECIES
!        ===============================================================
!        <df_AERO_COL06>

!        DEFINE POINTER ARRAY: AEROSOL SPECIES
!        ===============================================================
!        <PTRARRAY_AERO>

!        DEFINE POINTER ARRAY: AEROSOL SPECIES IN CLOUD-PHASE
!        ===============================================================
!        <PTRARRAYaw>

!        DEFINE POINTER ARRAY: AEROSOL NUMBER
!        ===============================================================
!        <PTRARRAY_NUM>

!        DEFINE POINTER ARRAY: AEROSOL NUMBER IN CLOUD-PHASE
!        ===============================================================
!        <PTRARRAYnw>

!        SET ARRAY: WET AEROSOL BOOLEAN
!        ===============================================================
!        <ARRAY_WETBOOL>

!        SET ARRAY: AEROSOL DENSITIES [kg m-3]
!        ===============================================================
!        <ARRAY_AERODENS>

!        SET ARRAY: STACKED EMISSION MODES
!        ===============================================================
!        <ARRxEMISSMODE>

!        SET COLUMN (EM-BB): INDEX FOR TARGETS
!        ===============================================================
!        <df_EMbb_COL01>

!        SET COLUMN (EM-AA): INDEX FOR TARGETS
!        ===============================================================
!        <df_EMaa_COL01>

!        SET COLUMN (EM-BB): INDEX FOR SOURCES
!        ===============================================================
!        <df_EMbb_COL02>

!        SET COLUMN (EM-AA): INDEX FOR SOURCES
!        ===============================================================
!        <df_EMaa_COL02>

!        SET COLUMN (EM-BB): INDEX FOR EMISSION MASS MODE
!        ===============================================================
!        <df_EMbb_COL03>

!        SET COLUMN (EM-AA): INDEX FOR EMISSION MASS MODE
!        ===============================================================
!        <df_EMaa_COL03>

!        SET COLUMN (EM-BB): EMISSION PHASE
!        ===============================================================
!        <df_EMbb_COL04>

!        SET COLUMN (EM-AA): EMISSION PHASE
!        ===============================================================
!        <df_EMaa_COL04>

!        SET COLUMN (EM-BB): VOLATILITY
!        ===============================================================
!        <df_EMbb_COL05>

!        SET COLUMN (EM-AA): VOLATILITY
!        ===============================================================
!        <df_EMaa_COL05>

!        SET COLUMN (EM-BB): MOLECULAR WEIGHT
!        ===============================================================
!        <df_EMbb_COL06>
        
!        SET COLUMN (EM-AA): MOLECULAR WEIGHT
!        ===============================================================
!        <df_EMaa_COL06>

      END SUBROUTINE f_INIT_PTRARRAY


      END MODULE mod_AERO_CONFIG
