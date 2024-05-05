## Project: Why do authoritarian regimes provide welfare programs? ------------------
##          1. Raw data imports and manipulation  -----------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:
rm(list=ls())

## Import Packages to use ------------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")
pacman::p_load(ggplot2, grid, pBrackets, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)

## Import data sets to use -----------------------------------------------------
### Varieties of Democracy Database --------------------------------------------
vdem <- vdemdata::vdem
vdem |> dplyr::select(contains("v2regsupgroups_")) |> pull(v2regsupgroups_0) |> table()
#### V-dem: Subset with variables in use----------------------------------------
vdem_subset <- 
  vdem |> 
  dplyr::select(country_name, COWcode, year, e_lexical_index, v2x_polyarchy,
                v2xps_party, v2dlunivl, e_boix_regime, v2x_freexp_altinf,
                v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv, 
                v2dlencmps, v2regimpgroup,
                contains("v2regsupgroups_"), -v2regsupgroups_nr,
                v2regsupgroupssize, v2x_clphy,
                e_cow_exports, e_cow_imports, e_gdp, e_gdppc, e_pop,
                e_total_fuel_income_pc, e_total_resources_income_pc, e_total_oil_income_pc,
                v2x_corr, v2cscnsult, v2csprtcpt, v2x_regime,
                v2x_ex_hereditary, v2x_ex_military, v2x_ex_party)

#### V-dem: Rename the variables -----------------------------------------------
names(vdem_subset) <-
  c("country_name", "COWcode", "year", "lexical_demo", 
    "edi", "party_inst", "universality", "regime_boix", "freedom", "party_org", "party_brch", "party_link",
    "party_platform", "party_cohesv", "public_goods", "coalition", 
    "Aristocracy", "Agrarian elites", "Party elites", "Business elites",
    "State bureaucracy", "The military", "Ethnic/racial group", "Religious group",
    "Local elites", "Urban working", "Urban middle", "Rural working", "Rural middle", "Foregin govt.",
    "coalition_size", "repression",
    "export", "import",
    "gdp", "gdppc", "pop", "fuel_income", "resource_income", "oil_income", "corruption",
    "CSOconsult", "CSOpart", "vdem_regime",
    "hereditary_index", "military_index", "party_index")

### Varieties of Party Database ------------------------------------------------
vparty <- vdemdata::vparty

#### V-Party: Transform the data as a form of tibble ---------------------------
vparty |> as_tibble() -> vparty_cl

#### V-Party: Subset with variables in use--------------------------------------
vparty_cl |> 
  dplyr::select(COWcode, year, v2paactcom, v2palocoff, v2pasoctie, v2pagovsup) ->
  vparty_sub

#### V-Party: Filtering data whether the party is supporting government --------
####          0 = Yes, as senior partner. 
####          The Head of Government belongs to this party.
vparty_sub |> 
  dplyr::filter(v2pagovsup == 0) |> 
  dplyr::select(-v2pagovsup) -> vparty_sub

### V-Dem & V-Party: Combine two different unit of anaylsis into one -----------
#### V-Merge: Reference will be V-Dem of country-year --------------------------
##### V-Merge: Get the unique set of countries in the data based on COW --------
COWcode <- unique(vdem$COWcode)

##### V-Merged: Sort the COWcode in order --------------------------------------
COWcode <- sort(COWcode)
##### V-Merged: Set annual year point between 1900 and 2019 --------------------
year <- 1900:2019

##### V-Merged: Make a complete set of COWcode and years -----------------------
combination <- data.frame(
  COWcode = c(rep(COWcode, each = length(year))),
  year = c(rep(year, time = length(COWcode)))
) |> as_tibble()

##### V-Merged: Merge empty complete set with V-Party subset -------------------
combination |> left_join(vparty_sub, by = c("COWcode", "year")) |> 
  group_by(COWcode) |> 
  # Only merge using V-Party with mass party organization measurements
  # As V-Party is party-election year level data, I filled with NAs based on the
  # previous year's observation.
  tidyr::fill(c(v2paactcom, v2palocoff, v2pasoctie), .direction = "down") ->
  combination2

##### V-Merged: drop missings and ungroup it, store it as V-Party subset -------
combination2 |> drop_na() |> ungroup() -> vparty_comb

##### V-Merged: Summarize the V-Party subset at COWcode-year level -------------
vparty_comb |> 
  group_by(COWcode, year) |> 
  summarize(
    v2paactcom = mean(v2paactcom, na.rm = T),
    v2palocoff = mean(v2palocoff, na.rm = T),
    v2pasoctie = mean(v2pasoctie, na.rm = T)) |> 
  ungroup() -> vparty_comb

plot(vparty_comb$v2paactcom, vparty_comb$v2palocoff)

##### V-Merge: What the Mass Party Organization Index should have --------------
#####.         See Reuter (2022) Democratization for sure.
### Local organizational strength (C) (v2paactcom)
### Local party office (C) (v2palocoff)
### Affiliate organizations (C) (v2pasoctie)

### Other Data Sources ---------------------------------------------------------
#### SPAW: Alternative DV for Social Policies around the World -----------------
##### Welfare coverage and law -------------------------------------------------
# SPaW <- ezpickr::pick("Original_Data/SPaW_ver2.dta")

#### Miller (2015): Resources and Military size --------------------------------
# Miller <- ezpickr::pick("Original_Data/Miller2015.dta")
# Miller <- Miller |> 
#   select(ccode, year, resdep2, urban, cow_milsize)

### Control Variables ----------------------------------------------------------
#### Quality of Government Dataset for a set of control variables --------------
qog <- rqog::read_qog("standard", "time-series")

##### QoG: Civil war experience ------------------------------------------------
qog |> 
  dplyr::select(COWcode = ccodecow, year, 
                #gdp = wdi_gdppppcon2017,
                #pop = wdi_pop, 3.19
                # Internal conflict existence
                civil_war = ucdp_type3) ->
  qog_sub

###### QoG: Manupulate the civil war experience variable as a binaryz
qog_sub |> mutate(civilwar = if_else(!is.na(civil_war), 1L, 0L)) -> qog_sub

#### Hanson and Sigman (2021): State capacity -> repressive capacity -----------
state_cap <- 
  rio::import("Original_data/StateCapacityDataset_v1.dta/StateCapacityDataset_v1.dta")
state_cap_sub <- state_cap |> dplyr::select(COWcode = ccode, year, Capacity)

#### Ross and Mahdavi (2015): Resource dependency ------------------------------
resource <- 
  read_csv("Original_data/dataverse_files/Ross-Mahdavi Oil and Gas 1932-2014.csv")

resource |> 
  mutate(COWcode = countrycode::countrycode(iso3numeric, "iso3n", "cown"),
         lnresource = log(oil_gas_valuePOP_2014+1))  ->
  resource_clean

#### Chin et al. (2022): Personalization measurement ---------------------------
personalization <- rio::import("Analysis_data/personalization.dta")

#### Geddes et al. (2014): Subtype of dictatorships ----------------------------
# remotes::install_github("xmarquez/democracyData")
gwf <- democracyData::gwf_all
gwf |> 
  dplyr::select(gwf_cowcode, year, 
                gwf_nonautocracy,
                gwf_party, gwf_military, 
                gwf_monarchy, gwf_personal) ->
  gwf_sub

#### Farris (2014): Human rights score -----------------------------------------
hrs <- read_csv("Original_data/HumanRightsProtectionScores_v4.01.csv")
##### Farris (2014): Manipulate standardized human rights score ----------------
hrs |> dplyr::select(COWcode = COW, year = YEAR, hrs = theta_mean) |> 
  mutate(repression = (hrs - mean(hrs, na.rm = T)) / sd(hrs, na.rm = T) * -1) ->
  hrs

#### V-Party: Mass Party Organization Index ------------------------------------
vparty_comb |>
  mutate(massparty = v2paactcom + v2palocoff + v2pasoctie) -> vparty_comb

#### V-Dem: Will contain and manipulate followings: ----------------------------
##### ln(GDPpc) and GDP growth: Growth Rate = (GDP_Year2/ GDP_Year 1) - 1
##### Civil war experience
##### Civil Society Strength
#####  - CSO consultation (C) (v2cscnsult)
#####  - CSO participatory environment (C) (v2csprtcpt)

## Final Merge for Analysis ----------------------------------------------------
### Variables that I need to manipulate 
### DV: Welfare universality: universality
### EV 1: Ruling coalition; mass-based vs. institution-based: coalition
### EV 2: Mass party organization: vparty
### FinalMerge: V-Dem manipulation for key variables: --------------------------
###           - Ruling coalition
###           - Regime classification: LEID and BRM
vdem_subset |> mutate(
  mass_coalition = case_when(
    coalition %in% c(9, 10, 11, 12) ~ 1L,
    coalition %in% c(2, 5) ~ 0L,
    T ~ NA_integer_),
  lexi_regime = case_when(
    lexical_demo <  6 ~ "Autocracies (LIED < 6)",
    lexical_demo >  6 ~ "Democracies (LIED > 6)",
    T ~ NA_character_),
  lexi_regime = factor(lexi_regime, 
                       levels = c("Autocracies (LIED < 6)", 
                                  "Democracies (LIED > 6)")),
  brm_regime = factor(regime_boix,
               levels = c(0, 1),
               labels = c("Autocracies (BRM)", "Democracies (BRM)"))
) |> dplyr::select(COWcode, country_name, year, lexi_regime, brm_regime, vdem_regime, universality, coalition,
                   mass_coalition, 17:30,
                   gdppc, gdp, resource_income, CSOconsult, CSOpart, 
                   hereditary_index, military_index, party_index) -> base
names(base)
### Now we have a baseline data set to be reference!

### FinalMerge: dem_party = Baseline + V-Party ---------------------------------
base |> left_join(vparty_comb |> drop_na(COWcode, year), 
                  by = c("COWcode", "year")) |> drop_na(COWcode, year) -> 
  dem_party

### FinalMerge: dem_party_cap = dem_party + state capacity ---------------------
dem_party |> left_join(
  state_cap_sub |> drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap

### FinalMerge: dem_party_cap_hrs = dem_party_cap + Human Rights Scores --------
dem_party_cap |> left_join(
  hrs |> dplyr::select(COWcode, year, repression) |> 
    drop_na(COWcode, year),
  by = c("COWcode", "year")
) -> dem_party_cap_hrs

### FinalMerge: dem_party_cap_hrs_qog = dem_party_cap_hrs + QoGs ---------------
dem_party_cap_hrs |> left_join(
  qog_sub |> drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap_hrs_qog

### FinalMerge: dem_party_cap_hrs_qog_resource = -------------------------------
### dem_party_cap_hrs_qog + Resource 
dem_party_cap_hrs_qog |> left_join(
  resource_clean |> dplyr::select(COWcode, year, lnresource) |> 
    drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap_hrs_qog_resource

### FinalMerge: dem_party_cap_hrs_resource_gwf = -------------------------------
### dem_party_cap_hrs_qog_resource + gwf_sub
dem_party_cap_hrs_qog_resource |> left_join(
  gwf_sub |> drop_na(gwf_cowcode, year), by = c("COWcode" = "gwf_cowcode", "year")
) -> dem_party_cap_hrs_resource_gwf

### FinalMerge: dem_party_cap_hrs_res_gwf_per = -------------------------------
### dem_party_cap_hrs_resource_gwf + personalization

dem_party_cap_hrs_resource_gwf |> left_join(
  personalization |> drop_na(cowcode, year),
  by = c("COWcode" = "cowcode", "year")
) -> dem_party_cap_hrs_res_gwf_per

### Baseline: Select necessary variables and store it as baseline --------------
dem_party_cap_hrs_res_gwf_per |> dplyr::select(
  COWcode, year,
  universality, mass_coalition, coalition, 10:23, massparty, gdppc, civilwar, repression, 
  lnresource, CSOconsult, CSOpart, Capacity,
  v2paactcom, v2palocoff, v2pasoctie,
  lexi_regime, brm_regime, vdem_regime,
  contains("index"), personindex = xpers1,
  contains("gwf_")
) -> baseline

baseline |> group_by(COWcode) |> 
  mutate(lgdppc = dplyr::lag(gdppc, n = 1, order_by = year)) |> 
  ungroup() |>
  mutate(
    #### Manipulate ln(GDPpc)
    gdpgrth = (gdppc/lgdppc)-1,
    mass_coalition = factor(mass_coalition,
                            levels = c(0, 1),
                            labels = c("Institution-based", "Mass-based")),
    gwf_regime = case_when(
      gwf_party == 1L | gwf_military == 1L | gwf_monarchy == 1L | gwf_personal == 1L ~ "Autocracy (GWF)",
      gwf_nonautocracy == 1L ~ "Democracy (GWF)",
      T ~ NA_character_),
    gwf_regime = factor(gwf_regime, levels = c("Autocracy (GWF)", "Democracy (GWF)")),
    alt_mass_part_coalition = case_when(
      `Urban working` >= 0.5 | `Urban middle` >= 0.5 | 
        `Rural working` >= 0.5 | `Rural middle` >= 0.5 ~ 1L,
      T ~ 0L)) -> 
  baseline

baseline |> 
  group_by(COWcode) |> 
  mutate(
    l_universality    = dplyr::lag(universality, n = 1, order_by = year),
    l_mass_coalition = dplyr::lag(mass_coalition, n = 1, order_by = year),
    l_alt_masscoalition = dplyr::lag(alt_mass_part_coalition, n = 1, order_by = year),
    l_elite_military  = dplyr::lag(`The military`, n = 1, order_by = year),
    l_elite_party  = dplyr::lag(`Party elites`, n = 1, order_by = year),
    l_elite_business  = dplyr::lag(`Business elites`, n = 1, order_by = year),
    l_elite_agrarian  = dplyr::lag(`Agrarian elites`, n = 1, order_by = year),
    l_elite_aristocracy  = dplyr::lag(Aristocracy, n = 1, order_by = year),
    l_elite_local     = dplyr::lag(`Local elites`, n = 1, order_by = year),
    l_mass_urbanworking  = dplyr::lag(`Urban working`, n = 1, order_by = year),
    l_mass_urbanmiddle  = dplyr::lag(`Urban middle`, n = 1, order_by = year),
    l_mass_ruralworking  = dplyr::lag(`Rural working`, n = 1, order_by = year),
    l_mass_ruralmiddle  = dplyr::lag(`Rural middle`, n = 1, order_by = year),
    l_state_bureau  = dplyr::lag(`State bureaucracy`, n = 1, order_by = year),
    l_ethnicracial    = dplyr::lag(`Ethnic/racial group`, n = 1, order_by = year),
    l_religious    = dplyr::lag(`Religious group`, n = 1, order_by = year),
    l_foreigngovt    = dplyr::lag(`Foregin govt.`, n = 1, order_by = year),
    l_massparty       = dplyr::lag(massparty, n = 1, order_by = year),
    l_gdppc           = dplyr::lag(gdppc, n = 1, order_by = year),
    l_gdpgrth         = dplyr::lag(gdpgrth, n = 1, order_by = year),
    l_lnresource      = dplyr::lag(lnresource, n = 1, order_by = year),
    l_civilwar        = dplyr::lag(civilwar, n = 1, order_by = year),
    l_repression      = dplyr::lag(repression, n = 1, order_by = year),
    l_capacity        = dplyr::lag(Capacity, n = 1, order_by = year),
    l_CSOconsult      = dplyr::lag(CSOconsult, n = 1, order_by = year),
    l_CSOpart         = dplyr::lag(CSOpart, n = 1, order_by = year),
    l_hereditaryindex = dplyr::lag(hereditary_index, n = 1, order_by = year),
    l_militaryindex   = dplyr::lag(military_index, n = 1, order_by = year),
    l_partyindex      = dplyr::lag(party_index, n = 1, order_by = year),
    l_personindex     = dplyr::lag(personindex, n = 1, order_by = year),
    l_royal           = dplyr::lag(gwf_monarchy, n = 1, order_by = year),
    l_mil             = dplyr::lag(gwf_military, n = 1, order_by = year),
    l_per             = dplyr::lag(gwf_monarchy, n = 1, order_by = year),
    l_party           = dplyr::lag(gwf_party, n = 1, order_by = year)) |>
  ungroup() -> baseline

baseline |> 
  mutate(
    coalition_alt = case_when(
      alt_mass_part_coalition %in% 1L ~ "Mass-inclusive coalition",
      alt_mass_part_coalition %in% 0L ~ "Non-mass coalition",
      T ~ NA_character_),
    coalition_alt = factor(coalition_alt,
                           levels = c("Non-mass coalition", "Mass-inclusive coalition"))) ->
  baseline

saveRDS(baseline, "Analysis_data/baseline.RDS")
