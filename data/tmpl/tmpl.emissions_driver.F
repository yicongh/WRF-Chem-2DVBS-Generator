!
!   WRF-chem V3.0 : Original version of emission_driver written by Georg Grell (ESRL/GSD)
!                   Further developments, bugfixes and improvements  by
!                   William Gustafson (PNNL), Serena Chung (WSU), 
!                   Saulo Freitas (CPTEC), and Georg Grell
!
!
      MODULE module_emissions_driver

         IMPLICIT NONE

         CONTAINS

         SUBROUTINE emissions_driver(id,ktau,dtstep,DX,                         &
         adapt_step_flag,curr_secs,                                        &
         plumerisefire_frq,stepfirepl,                                     &
         bioemdt,stepbioe,                                                 &
         config_flags,gmt,julday,alt,t_phy,moist,p8w,t8w,u_phy,            &
         v_phy,vvel,e_bio,p_phy,chem,rho_phy,dz8w,ne_area,emis_ant,        &
         emis_vol,tsk,erod,erod_dri,lai_vegmask,                           &
         g,emis_seas,emis_dust,tracer,                                     &
         emis_seas2,                                                       &
         ebu, ebu_in,mean_fct_agtf,mean_fct_agef,                          &
         mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,          &
         firesize_agsv,firesize_aggr,                                      &
         u10,v10,ivgtyp,isltyp,gsw,vegfra,rmol,ust,znt,dms_0,              &
         erup_beg,erup_end,                                                &
         xland,xlat,xlong,z_at_w,z,smois,dustin,seasin,                    &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &   
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         sebio_sesq,sebio_mbo,                                             & 
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         ebio_sesq, ebio_mbo,ebio_bpi,ebio_myrc,                           &
         ebio_c10h16,ebio_tol,ebio_bigalk,ebio_ch3oh,ebio_acet,            &
         ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,              &
         ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,ebio_so2,     &
         ebio_dms,ebio_hcn,                                                &
         ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,            &    
         ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,                       &    
         ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,                    &    
         ebio_cco_oh, ebio_rco_oh,                                         &    
         clayfrac,sandfrac,dust_alpha,dust_gamma,dust_smtune,dust_ustune,  &
         clayfrac_nga,sandfrac_nga,                                        &
         snowh,zs,afwa_dustloft,tot_dust,tot_edust,vis_dust,               &
         soil_top_cat, ust_t, rough_cor, smois_cor,                        & 
         ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,                 &
         ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho,              &
         ebio_nc4h10,							   &
         ! stuff for MEGAN v2.04                                                 
         T2,swdown,                                                        &
         nmegan,EFmegan,                                                   &
         msebio_isop,                                                      &
         mlai,                                                             &
         pftp_bt, pftp_nt, pftp_sb, pftp_hb,                               &
         mtsa,                                                             &
         mswdown,                                                          &
         mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                   &
         mebio_acet, mebio_mbo, mebio_no,                                  &
         current_month,                                                    &
         ! end stuff for MEGAN v2.04
         ! stuff for LNOx emissions
         ht, refl_10cm,                                                    &
         ic_flashrate, cg_flashrate,                                       &
         ! end stuff for LNOx emissions
         ! stuff for aircraft emissions
         emis_aircraft,                                                    &
         !  stuff for soil NO emissions  BJG  03/13/2020
         sf_no, sf_n2o, sf_hono, sf_nh3,                                   &
         ! stuff for GHG fluxes
         vprm_in,rad_vprm,lambda_vprm,alpha_vprm,resp_vprm,               &
         xtime,tslb,wet_in,rainc,rainnc,potevp,sfcevp,lu_index,            &
         biomt_par,emit_par,ebio_co2oce,eghg_bio,                          &
         dust_flux, seas_flux,                                             &
!---YZ0220:wildfire heat------------------------------------------------------@
         plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,plume_hgt_aggr,      &
!-----------------------------------------------------------------------------@
         landusef, num_land_cat,                                           &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam, only : mw_so4_aer,anthfac,factnumn,factnuma,factnumc
  USE module_model_constants, only : mwdry
  USE module_emissions_anthropogenics
  USE module_bioemi_simple
  USE module_bioemi_beis314
  USE module_bioemi_megan2
  USE module_aerosols_sorgam, only: sorgam_addemiss
  USE module_cbmz_addemiss
  USE module_cb05_addemiss
  USE gocart_dust
  USE gocart_dust_afwa
  USE gocart_seasalt
  USE uoc_dust  
  USE module_dms_emis
  USE module_mosaic_addemiss
  USE module_add_emis_cptec
  USE module_add_emiss_burn
  USE module_plumerise1
  USE module_aerosols_sorgam_vbs, only: sorgam_vbs_addemiss
  USE module_aerosols_soa_vbs, only: soa_vbs_addemiss
  USE module_ghg_fluxes
  USE module_lightning_nox_driver
  USE module_cam_mam_addemiss, only: cam_mam_addemiss
!sw++
#if( WRF_USE_CLM == 1)
  USE shr_megan_mod, only : shr_megan_mechcomps_n, shr_megan_mechcomps
#endif
!sw--
  
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,julday, ne_area,                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                          &
                                  ktau,stepbioe,stepfirepl
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_tracer ),               &
         INTENT(INOUT ) ::                                   tracer
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ),                  &
         INTENT(INOUT ) ::                                   ebu
   REAL, DIMENSION( ims:ime, 1, jms:jme, num_ebu_in ),                     &
         INTENT(INOUT ) ::                                   ebu_in
   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                           &
         INTENT(INOUT ) ::                               e_bio
!---YZ0220:wildfire heat------------------------------------------------------@
   REAL, DIMENSION( ims:ime, jms:jme),                     &
         INTENT(INOUT ) :: plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,plume_hgt_aggr
!-----------------------------------------------------------------------------@
   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant),&
         INTENT(IN ) ::                                                    &
         emis_ant
   REAL, DIMENSION( ims:ime,  kms:kme, jms:jme,num_emis_vol),              &
         INTENT(INOUT ) ::                                                 &
         emis_vol
   REAL, DIMENSION( ims:ime, jms:jme),&
         INTENT(IN ) ::                                                 &
         dms_0,tsk,erup_beg,erup_end
   REAL, DIMENSION( ims:ime, jms:jme,3),&
         INTENT(IN ) ::                                                 &
         erod, erod_dri
   REAL, DIMENSION( ims:ime, jms:jme), &
         INTENT(IN ) ::                                                    &
         lai_vegmask
   REAL, DIMENSION( ims:ime, jms:jme,5),&
         INTENT(INOUT ) ::                                                 &
         dustin,seasin
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_dust),   &
         OPTIONAL, INTENT(INOUT ) ::                                       &
         emis_dust
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_seas),   &
         OPTIONAL,                                                         &
         INTENT(INOUT ) ::                                                 &
         emis_seas
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_seas2),   &
         OPTIONAL,                                                         &
         INTENT(INOUT ) ::                                                 &
         emis_seas2
   REAL, DIMENSION( ims:ime,  jms:jme ),                                   &
         OPTIONAL,                                                         &
         INTENT(IN ) ::                                                    &
           mean_fct_agtf,mean_fct_agef,                                    &
           mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,        &
           firesize_agsv,firesize_aggr

!
! 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                        alt,               &
                                                      t_phy,               &
                                                      p_phy,               &
                                                      dz8w,                &
                                              t8w,p8w,z_at_w , z ,         &
                                              u_phy,v_phy,vvel,rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp,               &
                                                     isltyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN   ) ::                                                 &
                                                     u10,                  &
                                                     v10,                  &
                                                     gsw,                  &
                                                  vegfra,                  &
                                                     rmol,                 &
                                                     ust,                  &
                                                     xland,                &
                                                     xlat,                 &
                                                     xlong,                &
                                                     znt,                  &

! Add for the GHG_tracer option
                                                     rainc,                &
                                                     rainnc,               &
                                                     potevp,               &
                                                     sfcevp,               &
                                                     lu_index

   REAL, DIMENSION( ims:ime , jms:jme )                  ,                 &
         OPTIONAL,                                                         &
         INTENT(IN   ) ::                                                  &
                                                     clayfrac,             &
                                                     sandfrac,             &
                                                     clayfrac_nga,         &
                                                     sandfrac_nga,         &
                                                     snowh
   REAL, INTENT(IN   ) ::                            dust_alpha,           &
                                                     dust_gamma,           &
                                                     dust_smtune,          &
                                                     dust_ustune

  REAL, DIMENSION( config_flags%num_soil_layers ) ,                        &
      INTENT(IN   ) ::                               zs
  REAL, DIMENSION( ims:ime , jms:jme )                   ,                 &
         OPTIONAL,                                                         &
         INTENT(  OUT) ::                                                  &
                                                     tot_edust,            &
                                                     afwa_dustloft
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme )        ,                 &
         OPTIONAL,                                                         &
         INTENT(  OUT) ::                                                  &
                                                     tot_dust,             &
                                                     vis_dust
  REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,      &
      INTENT(INOUT ) ::                             smois, tslb    

   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
         OPTIONAL,                                                         &
          INTENT(INOUT   ) ::                                                 &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               sebio_sesq,sebio_mbo,                                   & 
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,   &
               ebio_sesq,ebio_mbo,ebio_bpi,ebio_myrc,                  &
               ebio_c10h16,ebio_tol,ebio_bigalk, ebio_ch3oh,ebio_acet, &
               ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,    &
               ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,    &
               ebio_so2,ebio_dms, ebio_co2oce , ebio_hcn,              &
               ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,  &    
               ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,             &    
               ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,          &    
               ebio_cco_oh, ebio_rco_oh,                               &
               ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,       &
               ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho,    &
               ebio_nc4h10
               
   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL  ,                 &
          INTENT(INOUT) ::                                    ust_t,   & 
                                                          rough_cor,   &
                                                          smois_cor          
! dust source area information from WPS        
   REAL, DIMENSION(ims:ime,1:config_flags%num_soil_cat,jms:jme) ,      & 
          INTENT(IN)::                                 soil_top_cat 

   ! stuff for MEGAN v2.04...most of these arrays are optional and package dependent 
   !  as declared in registry.chem

   integer, intent(in   ) :: nmegan
   real, dimension (ims:ime, jms:jme , nmegan) ,                       &
         OPTIONAL,                                                         &
        intent(inout) ::                                               &
        EFmegan


   real, dimension (ims:ime, jms:jme ) ,                               &
         OPTIONAL,                                                         &
        intent(in) ::                                                  &
        msebio_isop,                                                   &
        pftp_bt, pftp_nt, pftp_sb, pftp_hb

   real, dimension (ims:ime, jms:jme, 12 ) ,                           &
         OPTIONAL,                                                         &
        intent(in) ::                                                  &
        mlai, mtsa, mswdown

   real, dimension (ims:ime, jms:jme ) ,                               &
         OPTIONAL,                                                         &
        intent(inout) ::                                               &
        mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                &
        mebio_acet, mebio_mbo, mebio_no

   real, dimension (ims:ime, jms:jme ) ,                               &
        intent(in) ::                                                  &
        T2, swdown

   integer, intent(in) :: current_month

   INTEGER, INTENT(IN)::   num_land_cat
   REAL, DIMENSION( ims:ime, num_land_cat, jms:jme ) , OPTIONAL, INTENT(INOUT):: landusef

   ! end stuff for MEGAN v2.04

      REAL(KIND=8), INTENT(IN   ) ::                                   &
           curr_secs

      REAL :: gmtp,gmtm
      integer :: curr_hours,ivolcano
      Integer :: endhr,endmin,beghr,begmin,ko,kk4,kl,k_initial,k_final
      real :: emiss_ash_mass,emiss_ash_height,so2_mass,vert_mass_dist(kts:kte)
      real :: eh
      real :: area,x1,percen_mass_umbrel,base_umbrel,ashz_above_vent

      REAL, INTENT(IN   ) ::                                           &
           bioemdt, dtstep, dx, gmt, g

      INTEGER, INTENT(IN   ) ::                                        &
           plumerisefire_frq

      LOGICAL, INTENT(IN   ) ::                                        &
           adapt_step_flag
!     stuff for aircraft emissions

      REAL, DIMENSION( ims:ime, 1:config_flags%kemit_aircraft, jms:jme,num_emis_aircraft), &
            OPTIONAL, INTENT(IN ) :: emis_aircraft 

!   stuff for soil NO emissions   BJG 03/13/2020
  
      REAL, DIMENSION( ims:ime, jms:jme), INTENT(IN ) :: sf_no
      REAL, DIMENSION( ims:ime, jms:jme), INTENT(IN ) :: sf_n2o
      REAL, DIMENSION( ims:ime, jms:jme), INTENT(IN ) :: sf_hono
      REAL, DIMENSION( ims:ime, jms:jme), INTENT(IN ) :: sf_nh3

!
! stuff for ghg tracer

      REAL, DIMENSION(ims:ime, 8, jms:jme, num_vprm_in), INTENT(IN)     ::  vprm_in
      REAL, DIMENSION(ims:ime, 1,jms:jme, num_eghg_bio), INTENT(INOUT ) ::  eghg_bio

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)   :: dust_flux, seas_flux
! CO2
      REAL, DIMENSION(8) :: rad_vprm,lambda_vprm,alpha_vprm,resp_vprm
! CH4
      REAL, DIMENSION(14), INTENT(IN) :: biomt_par, emit_par
      REAL, DIMENSION(ims:ime,1,jms:jme,num_wet_in), INTENT(IN)  :: wet_in
      REAL, INTENT(IN) :: xtime
! end ghg tracer stuff
! stuff for lightning NOx
     REAL, DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: ht, ic_flashrate, cg_flashrate
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: refl_10cm
! end stuff for lightning NOx
!
! Local variables...
!
      INTEGER :: begday,endday,i, j, k, m, p_in_chem, ksub, dust_emiss_active, seasalt_emiss_active,emiss_ash_hgt
      REAL :: conv,conv3,conv4,oconv3,oconv4
!sw++
#if( WRF_USE_CLM == 1)
      REAL :: convert2(its:ite,jts:jte)
#endif
!sw--
      CHARACTER (LEN=80) :: message
      LOGICAL :: do_bioemiss, do_plumerisefire,do_ex_volcanoe

      INTEGER :: imod  ! dust scheme option from namelist


!     BACKGROUND ABSORBING MASS: [ug m-3]
      REAL(8),PARAMETER :: ABSORB = 10.0

            
! ..
! ..
! .. Intrinsic Functions ..
!     INTRINSIC max, min
! ..

! Flags for turning on or off sea salt and/or dust emissions...
! for PNNL modules:   >0 = sea salt/dust emissions turned on
! As of NOV 2008 these only are used for MOSAIC and SORGAM
! gocart  dust and seasalt will only work for GOCART and SORGAM
! DL - 06/02/2013 - added option for MOSAIC-PDFiTE seasalt emissions (with organic fraction)
       percen_mass_umbrel=.75
       base_umbrel=.25    ! fraction

      ivolcano=0
      area=dx*dx
      dust_emiss_active    = 0
      seasalt_emiss_active = 0

      if(config_flags%dust_opt >= 2 )dust_emiss_active    = 1
      if(config_flags%seas_opt == 2 )seasalt_emiss_active = 1
      if(config_flags%seas_opt == 3 )seasalt_emiss_active = 3
      if(config_flags%seas_opt == 4 )seasalt_emiss_active = 4

!
! Setup the timing flags...
! (methodology is adapated from module_radiation_driver.F)
      gmtp=curr_secs/3600.
      curr_hours=curr_secs/3600.
      gmtp=mod(gmt+gmtp,24.)
      gmtm=mod(gmtp,60.)
!     write(0,*) 'gmtp,gmtm,curr_secs = ',gmtp,gmtm,curr_secs

      do_plumerisefire = .false.

      IF ( config_flags%biomass_burn_opt == BIOMASSB_MOZC .OR. &
           config_flags%biomass_burn_opt == BIOMASSB_T1_MOZCART  .OR. &
           config_flags%biomass_burn_opt == BIOMASSB_MOZ  .OR. &
           config_flags%biomass_burn_opt == BIOMASSB_SAPRC  .OR. &
           config_flags%biomass_burn_opt == BIOMASSB_GHG  .OR. &
           config_flags%biomass_burn_opt == BIOMASSB ) then

        IF ( ktau==1 ) then
           do_plumerisefire = .true.
        ELSE IF ( adapt_step_flag ) THEN
           IF ( (plumerisefire_frq<=0) .or. &
                ( curr_secs+real(dtstep,8)+0.01 >= &
                ( INT( curr_secs/real(plumerisefire_frq*60.,8)+1,8 )*real(plumerisefire_frq*60.,8) ) ) &
                ) then
              do_plumerisefire = .true.
           ENDIF
        ELSE IF ( (MOD(ktau,stepfirepl)==0) .or. (stepfirepl==1) ) THEN
           do_plumerisefire = .true.
        ENDIF
      ENDIF



      do_bioemiss = .false.

      IF ( ktau==1 ) then
         do_bioemiss = .true.
      ELSE IF ( adapt_step_flag ) THEN
         IF ( (bioemdt<=0) .or. &
              ( curr_secs+real(dtstep,8)+0.01 >= &
              ( INT( curr_secs/real(bioemdt*60.,8)+1,8 )*real(bioemdt*60.,8) ) ) &
              ) then
            do_bioemiss = .true.
         ENDIF
      ELSE IF ( (MOD(ktau,stepbioe)==0) .or. (stepbioe==1) ) THEN
         do_bioemiss = .true.
      ENDIF
!
! we are doing the plumerise/fire emissions first, they may be needed for chem and tracer arrays
!

       if( do_plumerisefire )then
          CALL wrf_debug(15,'fire emissions: calling biomassb')
          write(0,*)ktau,stepfirepl
          call plumerise_driver (id,ktau,dtstep,                           &
           ebu,ebu_in,                                                     &
           mean_fct_agtf,mean_fct_agef,mean_fct_agsv,mean_fct_aggr,        &
           firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr,        &
           config_flags, t_phy,moist,                                      &
           rho_phy,vvel,u_phy,v_phy,p_phy,                                 &
           emis_ant,z_at_w,z,config_flags%scale_fire_emiss,                &
!---YZ0220:wildfire heat-------------------------------------------------------@
           plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,plume_hgt_aggr,    &
!------------------------------------------------------------------------------@
           ids,ide, jds,jde, kds,kde,                                      &
           ims,ime, jms,jme, kms,kme,                                      &
           its,ite, jts,jte, kts,kte                                       )
        endif

      dms_select: SELECT CASE(config_flags%dmsemis_opt)
      CASE (DMSGOCART)
         CALL wrf_debug(15,'Gocart dms emissions')
         call gocart_dmsemis(dtstep,config_flags,alt,t_phy,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,dms_0,tsk,                  &
         ivgtyp,isltyp,xland,dx,g, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )

      CASE DEFAULT 
         CALL wrf_debug(15,'no dms emissions')
      END SELECT dms_select

      ksub=0

!     ADD EMISSIONS: BIOMASS BURNING.
!     =====================================================================
      SELECT3: SELECT CASE (config_flags%biomass_burn_opt)
      
      CASE (BIOMASSB, BIOMASSB_MOZC, BIOMASSB_MOZ, &
            BIOMASSB_T1_MOZCART, BIOMASSB_GHG, BIOMASSB_SAPRC)

         CALL wrf_debug(15,'fire emissions: adding biomassb emissions')
       

         CALL add_emis_burn(id,dtstep,ktau,dz8w,rho_phy,chem,&
            julday,gmt,xlat,xlong,t_phy,p_phy,                           &
            ebu,config_flags%chem_opt,0,config_flags%biomass_burn_opt,     &
            num_chem,ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )

      CASE DEFAULT 
         CALL wrf_debug(15,'no biomass burning')
      END SELECT SELECT3


!
    bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
          CALL wrf_debug(15,'biogenic emissions: calling Gunther1')
          call bio_emissions(id,ktau,dtstep,DX,config_flags,               &
               gmt,julday,t_phy,moist,p8w,t8w,                             &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                      &
               ivgtyp,gsw,vegfra,rmol,ust,znt,xlat,xlong,z_at_w,           &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
       endif
     CASE (BEIS314)
       if( do_bioemiss ) then
         beis314_check_mechanism_ok: SELECT CASE(config_flags%chem_opt) 
            CASE (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
                  RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ,RACMSORG_AQCHEM_KPP,        &
                  RACM_ESRLSORG_AQCHEM_KPP, RACMSORG_KPP,RACM_ESRLSORG_KPP, RACM_SOA_VBS_KPP, &
                  RACM_SOA_VBS_AQCHEM_KPP, RACM_SOA_VBS_HET_KPP, CBM4_KPP, NMHC9_KPP, GOCARTRACM_KPP,GOCARTRADM2)
            CASE DEFAULT 
               CALL wrf_error_fatal( &
                  "emissions_driver: beis3.1.4 biogenic emis. implemented for RADM2 & RACM only")
         END SELECT beis314_check_mechanism_ok
         CALL wrf_debug(15,'biogenic emissions: calling beis3.1.4')
         call bio_emissions_beis314(id,config_flags,ktau,curr_secs,    &
               dtstep,julday,gmt,xlat,xlong,t_phy,p_phy,gsw,           &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               sebio_sesq,sebio_mbo,                                   &
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,   &
               ebio_sesq,ebio_mbo,                                     &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
       endif

     CASE (MEGAN2)
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then                        
         CALL wrf_debug(15,'biogenic emissions: calling megan v2.04')  
         call bio_emissions_megan2(id,config_flags,ktau,dtstep,        &
               curr_secs,julday,gmt,xlat,xlong,p_phy,rho_phy,dz8w,     &
               chem,ne_area,                                           &
               current_month,                                          &
               T2,swdown,                                              &
               nmegan, EFmegan, msebio_isop,                           &
               mlai,                                                   &
               pftp_bt, pftp_nt, pftp_sb, pftp_hb,                     &
               mtsa,                                                   &
               mswdown,                                                &
               mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,         &
               mebio_acet, mebio_mbo, mebio_no,                        &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,                    &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_no,           &
               ebio_c10h16,ebio_tol,ebio_bigalk, ebio_ch3oh,ebio_acet,         &
               ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,            &
               ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,ebio_so2,   &
               ebio_dms,ebio_hcn,                                              &
               ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,       &
               ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho,    &
               ebio_nc4h10, &
               ebio_sesq, ebio_mbo,ebio_bpi,ebio_myrc,                 &
               ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,    &    
               ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,               &    
               ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,            &    
               ebio_cco_oh, ebio_rco_oh,                                 &
               e_bio,                                                  &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
       endif
!sw++
#if( WRF_USE_CLM == 1)
     CASE (MEGAN2_CLM)
!...conversion factor from ppm m min-1 to mol km-2 hr-1
!...(e_bio is in units of ppm m min-1)
       convert2(its:ite,jts:jte) = rho_phy(its:ite,kts,jts:jte)*60./.02897

       do m = 1,shr_megan_mechcomps_n
         p_in_chem = shr_megan_mechcomps(m)%index

         IF ( p_in_chem+1 == p_no ) THEN
           ebio_no(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_no2 ) THEN
           ebio_no2(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_co ) THEN
           ebio_co(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_hcho ) THEN
           ebio_hcho(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_ald ) THEN
           ebio_ald(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_acet ) THEN
           ebio_acet(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_tol ) THEN
           ebio_tol(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c10h16 ) THEN
           ebio_c10h16(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_so2 ) THEN
           ebio_so2(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_dms ) THEN
           ebio_dms(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_bigalk ) THEN
           ebio_bigalk(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_bigene ) THEN
           ebio_bigene(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_nh3 ) THEN
           ebio_nh3(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_ch3oh ) THEN
           ebio_ch3oh(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c2h5oh ) THEN
           ebio_c2h5oh(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_ch3co2h ) THEN
           ebio_ch3cooh(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_mek ) THEN
           ebio_mek(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c2h4 ) THEN
           ebio_c2h4(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c2h6 ) THEN
           ebio_c2h6(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c3h6 ) THEN
           ebio_c3h6(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ELSEIF ( p_in_chem+1 == p_c3h8 ) THEN
           ebio_c3h8(its:ite,jts:jte) = e_bio(its:ite,jts:jte,p_in_chem)*convert2(its:ite,jts:jte)
         ENDIF

       end do
#endif
!sw--

     CASE DEFAULT 
       if( do_bioemiss ) e_bio(its:ite,jts:jte,1:ne_area) = 0.
!wig: May need to zero out all ebio_xxx arrays too if they are incorporated
!     into CBMZ/MOSAIC.
                                                     
    END SELECT bioem_select

!!! **************** END BIOGENICS, ADD EMISSIONS FOR VARIOUS PACKAGES
!


!     ADD EMISSSIONS: ANTHRO. AND BIO. GAS PHASE.
!     =====================================================================     
      SELECT0: SELECT CASE(config_flags%chem_opt)
      
      CASE (SAPRC99_MOSAIC_4BIN_VBS2_KPP, SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP, &
            SAPRC99_MOSAIC_8BIN_VBS2_KPP, SAPRC99_MOSAIC_20BIN_VBS2_KPP, &
            SAPRC99_MOSAIC_12BIN_VBS2_KPP, SAPRC99_MOSAIC_4BIN_2DVBS_KPP)

     if(config_flags%emiss_opt == 13 ) then


       do j=jts,jte
          do i=its,ite
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                conv3 = (dtstep/dz8w(i,k,j))*alt(i,k,j)*28/250*1e-3! Molecular weight of C(15)H(27)N(0.3)O(4.5)
                conv4= (dtstep/dz8w(i,k,j))*alt(i,k,j)*28/226*1e-3  ! Molecular weight of C(15)H(27)N(0.3)O(0.9)
                oconv3= (dtstep/dz8w(i,k,j))*alt(i,k,j)*28/283*1e-3*4.5 !For biomass there are 4.5 moles of ) /mole of C15H27N0.3
                oconv4=(dtstep/dz8w(i,k,j))*alt(i,k,j)*28/226*1e-3*0.9  !For fossil there are 0.9 moles of O per mole of C15H27N0.3
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                             &
                     +emis_ant(i,k,j,p_e_so2)*conv
                chem(i,k,j,p_c2h6)  = chem(i,k,j,p_c2h6)                           &
                     +emis_ant(i,k,j,p_e_c2h6)*conv
                chem(i,k,j,p_c3h8)  = chem(i,k,j,p_c3h8)                           &
                     +emis_ant(i,k,j,p_e_c3h8)*conv
                chem(i,k,j,p_c2h2)  = chem(i,k,j,p_c2h2)                           &
                     +emis_ant(i,k,j,p_e_c2h2)*conv
                chem(i,k,j,p_alk3)  = chem(i,k,j,p_alk3)                           &
                     +emis_ant(i,k,j,p_e_alk3)*conv
                chem(i,k,j,p_alk4)  = chem(i,k,j,p_alk4)                           &
                     +emis_ant(i,k,j,p_e_alk4)*conv
                chem(i,k,j,p_alk5)  = chem(i,k,j,p_alk5)                           &
                     +emis_ant(i,k,j,p_e_alk5)*conv
                chem(i,k,j,p_ethene)  = chem(i,k,j,p_ethene)                       &
                     +emis_ant(i,k,j,p_e_ethene)*conv
                chem(i,k,j,p_c3h6)  = chem(i,k,j,p_c3h6)                           &
                     +emis_ant(i,k,j,p_e_c3h6)*conv
                chem(i,k,j,p_ole1)  = chem(i,k,j,p_ole1)                           &
                     +emis_ant(i,k,j,p_e_ole1)*conv
                chem(i,k,j,p_ole2)  = chem(i,k,j,p_ole2)                           &
                     +emis_ant(i,k,j,p_e_ole2)*conv
                chem(i,k,j,p_aro1)  = chem(i,k,j,p_aro1)                           &
                     +emis_ant(i,k,j,p_e_aro1)*conv
                chem(i,k,j,p_aro2)  = chem(i,k,j,p_aro2)                           &
                     +emis_ant(i,k,j,p_e_aro2)*conv
                chem(i,k,j,p_hcho)  = chem(i,k,j,p_hcho)                           &
                     +emis_ant(i,k,j,p_e_hcho)*conv
                chem(i,k,j,p_ccho)  = chem(i,k,j,p_ccho)                           &
                     +emis_ant(i,k,j,p_e_ccho)*conv
                chem(i,k,j,p_rcho)  = chem(i,k,j,p_rcho)                           &
                     +emis_ant(i,k,j,p_e_rcho)*conv
                chem(i,k,j,p_acet)  = chem(i,k,j,p_acet)                           &
                     +emis_ant(i,k,j,p_e_acet)*conv
                chem(i,k,j,p_mek)  = chem(i,k,j,p_mek)                             &
                     +emis_ant(i,k,j,p_e_mek)*conv
                chem(i,k,j,p_isoprene)  = chem(i,k,j,p_isoprene)                   &
                     +emis_ant(i,k,j,p_e_isoprene)*conv
                chem(i,k,j,p_terp)  = chem(i,k,j,p_terp)                           &
                     +emis_ant(i,k,j,p_e_terp)*conv
                chem(i,k,j,p_sesq)  = chem(i,k,j,p_sesq)                           &
                     +emis_ant(i,k,j,p_e_sesq)*conv
                chem(i,k,j,p_co)  = chem(i,k,j,p_co)                               &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_no)  = chem(i,k,j,p_no)                               &
                     +emis_ant(i,k,j,p_e_no)*conv

!--------------------------------------------------------------------------------------------------------------------------------------------------------------
!         Add soil NO based on Alex Guenther's parameterization and Williams et al.
!         BJG 3/13/2020 -- replace by Rasool et al. soil NO scheme.  

!              if((k.eq.kts).and.xland(i,j).eq.1.0)        & !Add soil NO to the lowest layer k=kts
!!!!!              chem(i,k,j,p_no)  = chem(i,k,j,p_no) +15.0/30.01*conv ! & !convert 15.0 ug/m2/hr to mol/km2/hr (dived by MW of NO=30.01), then multiply by conv
!              chem(i,k,j,p_no)  = chem(i,k,j,p_no) +sf_no(i,j)*3600.0*1.e6*conv/14.01 ! & !convert g-N/(m2-s) to mol/km2/hr (divide by MW of N=14.01), then multiply by conv
!              if((k.eq.kts).and.xland(i,j).eq.1.0)        & 
!              chem(i,k,j,p_n2o)  = chem(i,k,j,p_n2o) +sf_n2o(i,j)*3600.0*1.e6*conv/14.01 
!              if((k.eq.kts).and.xland(i,j).eq.1.0)        & 
!              chem(i,k,j,p_hono)  = chem(i,k,j,p_no) +sf_hono(i,j)*3600.0*1.e6*conv/14.01 
!!!!  BJG -- changed below soil nh3 emissions, based on Dubache et al. (2019)
!              if((k.eq.kts).and.xland(i,j).eq.1.0)        & !Based on Alex Guenther's suggestion, without canopy reduction since ammonia can revolatilize
!!!!!!!!!              chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3) +15.0/17.0*conv
              chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3) +sf_nh3(i,j)*3600.0*1.e6*conv/14.01 

              if((k.eq.kts).and.xland(i,j).eq.1.0)        & !Based on Jardine et al. 2015, soil DMS source
              chem(i,k,j,p_dms)  = chem(i,k,j,p_dms) +0.24*conv  !convert 0.8ngC/m2/s to mol/km2/hr
!--------------------------------------------------------------------------------------------------------------------------------------------------------



!!!  YICONG HE: BASED ON KE PIAOPIAO ET AL. (2022)
              if((k.eq.kts).and.xland(i,j).eq.1.0) chem(i,k,j,p_no)  = chem(i,k,j,p_no) + 7.5/30.01*conv










                chem(i,k,j,p_no2)  = chem(i,k,j,p_no2)                             &
                     +emis_ant(i,k,j,p_e_no2)*conv
                chem(i,k,j,p_phen)  = chem(i,k,j,p_phen)                           &
                     +emis_ant(i,k,j,p_e_phen)*conv
                chem(i,k,j,p_cres)  = chem(i,k,j,p_cres)                           &
                     +emis_ant(i,k,j,p_e_cres)*conv
                chem(i,k,j,p_meoh)  = chem(i,k,j,p_meoh)                           &
                     +emis_ant(i,k,j,p_e_meoh)*conv
                chem(i,k,j,p_gly)  = chem(i,k,j,p_gly)                             &
                     +emis_ant(i,k,j,p_e_gly)*conv
                chem(i,k,j,p_mgly)  = chem(i,k,j,p_mgly)                           &
                     +emis_ant(i,k,j,p_e_mgly)*conv
                chem(i,k,j,p_bacl)  = chem(i,k,j,p_bacl)                           &
                     +emis_ant(i,k,j,p_e_bacl)*conv
                chem(i,k,j,p_isoprod)  = chem(i,k,j,p_isoprod)                     &
                     +emis_ant(i,k,j,p_e_isoprod)*conv
                chem(i,k,j,p_methacro)  = chem(i,k,j,p_methacro)                   &
                     +emis_ant(i,k,j,p_e_methacro)*conv
                chem(i,k,j,p_mvk)  = chem(i,k,j,p_mvk)                             &
                     +emis_ant(i,k,j,p_e_mvk)*conv
                chem(i,k,j,p_prod2)  = chem(i,k,j,p_prod2)                         &
                     +emis_ant(i,k,j,p_e_prod2)*conv
                chem(i,k,j,p_ch4)  = chem(i,k,j,p_ch4)                             &
                     +emis_ant(i,k,j,p_e_ch4)*conv
                chem(i,k,j,p_bald)  = chem(i,k,j,p_bald)                           &
                     +emis_ant(i,k,j,p_e_bald)*conv
                chem(i,k,j,p_hcooh)  = chem(i,k,j,p_hcooh)                         &
                     +emis_ant(i,k,j,p_e_hcooh)*conv
                chem(i,k,j,p_cco_oh)  = chem(i,k,j,p_cco_oh)                       &
                     +emis_ant(i,k,j,p_e_cco_oh)*conv
                chem(i,k,j,p_rco_oh)  = chem(i,k,j,p_rco_oh)                       &
                     +emis_ant(i,k,j,p_e_rco_oh)*conv
                chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                       &
                     +emis_ant(i,k,j,p_e_nh3)*conv

!!!!!   YICONG HE:
!!              <FLAG>




             end do
          end do
       end do

      else
       do j=jts,jte
          do i=its,ite
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                   &
                     +emis_ant(i,k,j,p_e_so2)*conv
                chem(i,k,j,p_co)  = chem(i,k,j,p_co)                     &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_no)  = chem(i,k,j,p_no)                     &
                     +emis_ant(i,k,j,p_e_no)*conv
                chem(i,k,j,p_hcho)  = chem(i,k,j,p_hcho)                 &
                     +emis_ant(i,k,j,p_e_hcho)*conv
             end do
          end do
       end do
      endif

      !BSINGH - Adding add_biogenics call
      call add_biogenics(id,dtstep,dz8w,config_flags, rho_phy,chem,     &
            e_bio,ne_area,                                              &
            ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
            ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
            ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
            ebio_sesq,ebio_mbo,                                         & 
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )

      CASE DEFAULT
         PRINT*, 'ONLY SAPRC99 SUPPORTED FOR NOW. STOP.'
         STOP
      END SELECT SELECT0

!     ADD AEROSOL EMISSIONS:
!     =====================================================================
      SELECT1: SELECT CASE (config_flags%chem_opt)

      CASE (SAPRC99_MOSAIC_4BIN_2DVBS_KPP)

         CALL MOSAIC_ADDEMISS(iTS,iTE,jTS,jTE,kTS,kTE, &
              iMS,iME,jMS,jME,kMS,kME, chem, emis_ant, ebu, config_flags, &
              dtstep, dz8w, alt)

      CASE DEFAULT
         PRINT*, 'ONLY SAPRC99_MOSAIC_4BIN_2DVBS SUPPORTED FOR NOW. STOP.'
         STOP 
      END SELECT SELECT1

      END SUBROUTINE emissions_driver

      END MODULE module_emissions_driver
