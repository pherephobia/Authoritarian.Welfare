## Project: Why do authoritarian regimes provide welfare programs? -------------
##          1. Raw data imports   ----------------------------------------------
## Author:
##   - Sang-Hoon Park (UofSC)
## Date: 13th Feb. 2022
rm(list=ls())

## Import Packages to use ------------------------------------------------------
pacman::p_load(ggplot2, grid, pBrackets, ezpickr, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)

## Import dataset to use -------------------------------------------------------
### Varieties of Democracy Database --------------------------------------------

Vdem <- vdemdata::vdem
Vdem_subset <- 
  Vdem %>% 
  select(country_name, COWcode, year, e_lexical_index, v2x_polyarchy,
         v2xps_party, v2dlunivl,
         v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv, 
         v2dlencmps, v2regimpgroup, v2regsupgroupssize,
         e_cow_exports, e_cow_imports, e_migdpgro, e_migdppc, e_mipopula,
         e_miurbpop, e_miinteco, e_civil_war, v2x_corr, v2clrspct,
         v2x_genpp, v2x_cspart, v2regendtype, v2csantimv)
rm(Vdem)

### Welfare coverage and law ---------------------------------------------------
SPaW <- pick("Original_Data/SPaW_ver2.dta")

### Resources and Military size ------------------------------------------------
Miller <- pick("Original_Data/Miller2015.dta")

Miller <- Miller %>% 
  select(ccode, year, resdep2, urban, cow_milsize)

### Control Vaiables -----------------------------------------------------------

#QOG <-  pick("qog_std_ts_jan19.csv")
#QOG.ctrl <- QOG %>% 
#  select(ccodecow, year, 
#         wdi_trade, wdi_pop14, wdi_pop65, wdi_expmil)

#############################################################
### Make a Base_line dataset
#############################################################

### Merge Vdem and SPaW
mdata_Vdem_SPaW <- Vdem_subset %>% 
  left_join(SPaW, by = c("COWcode" = "Ccodecow", "year"))

### Merge Miller with previous one
mdata_Vdem_SPaW_Miller <- mdata_Vdem_SPaW %>%
  left_join(Miller, by = c("COWcode" = "ccode", "year"))

### Store the finalized data, naming Analysis_data_noclean
mdata_Vdem_SPaW_Miller -> Analysis_data_noclean

Analysis_data_noclean <- Analysis_data_noclean %>% select(
  everything(), e_cow_exports, e_cow_imports, v2x_corr, v2clrspct,
  v2x_genpp, v2x_cspart
)

foreign::write.dta(Analysis_data_noclean, "Analysis_Data/Analysis_data_noclean.dta")
