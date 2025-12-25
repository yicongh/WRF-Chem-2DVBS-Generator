!     ==================================================================
!                MODULE FOR GAS-PHASE SPECIES CONFIGURATIONS
!     ==================================================================
!     NOTE: THIS SCRIPT IS AUTOMATICALLY GENERATED. CHANGES WILL BE LOST.

      MODULE mod_CONFIG_GAS

!     USE MODULES:
!     ==================================================================
      USE MODULE_CONFIGURE

!     DEFINE: ARRAY DIMENSIONS
!     ==================================================================
!     NUMBER OF GAS-PHASE SPECIES:
      INTEGER,PARAMETER :: nCOMP = 0000 !<FLAG>

!     DEFINE: PSEUDO-DATAFRAME FOR GAS-PHASE SPECIES
!     ==================================================================
!     DEFINE: SPECIES NAME
      CHARACTER(len=30),DIMENSION(nCOMP) :: df_GAS_COL01

!     DEFINE: OTHER COLUMNS
      INTEGER,DIMENSION(nCOMP) :: df_GAS_COL02 ! << INDEX IN CHEM-ARRAY
      REAL(8),DIMENSION(nCOMP) :: df_GAS_COL03 ! << HENRY'S LAW CONSTANT [M atm-1]
      REAL(8),DIMENSION(nCOMP) :: df_GAS_COL04 ! << DESORPTION ENTHALPY OVER GAS CONSTANT [K]
      REAL(8),DIMENSION(nCOMP) :: df_GAS_COL05 ! << SURFACE REACTIVITY
      REAL(8),DIMENSION(nCOMP) :: df_GAS_COL06 ! << BROWNIAN DIFFUSIVITY [cm2 s-1]

!     DEFINE SUBROUTINE:
!     ==================================================================
      CONTAINS

      SUBROUTINE f_INIT_CONFIG

!        SET DATAFRAME (GAS-COL01): SPECIES NAMES
!        ===============================================================
!        <df_GAS_COL01>

!        SET DATAFRAME (GAS-COL02): INDEX IN CHEM-ARRAY
!        ===============================================================
!        <df_GAS_COL02>

!        SET DATAFRAME (GAS-COL03): HENRY'S LAW CONSTANT
!        ===============================================================
!        <df_GAS_COL03>

!        SET DATAFRAME (GAS-COL04): ENTAHALY OVER GAS CONSTANT
!        ===============================================================
!        <df_GAS_COL04>

!        SET DATAFRAME (GAS-COL05): SURFACE REACTIVITY
!        ===============================================================
!        <df_GAS_COL05>

!        SET DATAFRAME (GAS-COL06): BROWNIAN DIFFUSIVITY
!        ===============================================================
!        <df_GAS_COL06>

      END SUBROUTINE f_INIT_CONFIG

      END MODULE mod_CONFIG_GAS
