## Project: Why do authoritarian regimes provide welfare programs? -------------
##          1. Raw data imports   ----------------------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:

rm(list=ls())

## Import Packages to use ------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")
pacman::p_load(ggplot2, grid, pBrackets, ezpickr, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)

theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "black"),
        plot.subtitle = element_text(face = "bold", color = "black"),
        axis.title = element_text(family = "Barlow Semi Condensed Medium", color = "black",
                                  margin = margin(t = 10, b = 10), size = rel(2)),
        axis.text = element_text(family = "Barlow Semi Condensed Medium", color = "black",
                                 size = rel(1.8)),
        legend.text = element_text(family = "Barlow Semi Condensed Medium", color = "black", 
                                   size = rel(1.8)),
        strip.text = element_text(family = "Barlow Semi Condensed",
                                  face = "bold", size = rel(2), hjust = 0,
                                  color = "black"),
        strip.background = element_rect(fill = "white", color = NA),
        plot.caption = element_text(hjust = 0, color = "black"),
        legend.position = "bottom")
}

showtext::showtext_auto(T)

theme_set(theme_clean())

## Import dataset to use -------------------------------------------------------
### Varieties of Democracy Database --------------------------------------------

vdem |> dplyr::select(COWcode, year, 
                      v2regimpgroup,
                      v2regsupgroups_2, v2regsupgroups_5,
                      v2regsupgroups_9, v2regsupgroups_10,
                      v2regsupgroups_11, v2regsupgroups_12) |> 
  dplyr::filter(year > 1964) |> 
  dplyr::filter(v2regsupgroups_2 == 1L | v2regsupgroups_9 == 1L)
 

vdem <- vdemdata::vdem

vdem_subset <- 
  vdem |> 
  dplyr::select(country_name, COWcode, year, e_lexical_index, v2x_polyarchy,
                v2xps_party, v2dlunivl, e_boix_regime, v2x_freexp_altinf,
                v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv, 
                v2dlencmps, v2regimpgroup, v2regsupgroupssize, v2x_clphy,
                e_cow_exports, e_cow_imports, e_gdp, e_gdppc, e_pop,
                e_total_fuel_income_pc, e_total_resources_income_pc, e_total_oil_income_pc,
                v2x_corr, v2cscnsult, v2csprtcpt,
                v2x_ex_hereditary, v2x_ex_military, v2x_ex_party)

names(vdem_subset) <-
  c("country_name", "COWcode", "year", "lexical_demo", 
    "edi", "party_inst", "universality", "regime_boix", "freedom", "party_org", "party_brch", "party_link",
    "party_platform", "party_cohesv", "public_goods", "coalition", "coalition_size", 
    "repression",
    "export", "import",
    "gdp", "gdppc", "pop", "fuel_income", "resource_income", "oil_income", "corruption",
    "CSOconsult", "CSOpart", "hereditary_index", "military_index", "party_index")

vparty <- vdemdata::vparty

vparty |> as_tibble() -> vparty_cl

vparty_cl |> dplyr::select(COWcode, year, v2paactcom, v2palocoff, v2pasoctie, v2pagovsup) ->
  vparty_sub

vparty_sub |> dplyr::filter(v2pagovsup == 0) |> dplyr::select(-v2pagovsup) -> vparty_sub

COWcode <- unique(vdem$COWcode)
COWcode <- sort(COWcode)
year <- 1900:2019

combination <- data.frame(
  COWcode = c(rep(COWcode, each = length(year))),
  year = c(rep(year, time = length(COWcode)))
) |> as_tibble()
length(c(rep(year, time = length(COWcode))))
length(c(rep(COWcode, each = length(year))))

combination |> left_join(vparty_sub, by = c("COWcode", "year")) |> 
  group_by(COWcode) |> 
  tidyr::fill(c(v2paactcom, v2palocoff, v2pasoctie), .direction = "down") ->
  combination2

combination2 |> drop_na() |> ungroup() -> vparty_comb

vparty_comb |> group_by(COWcode, year) |> summarize(
  v2paactcom = mean(v2paactcom, na.rm = T),
  v2palocoff = mean(v2palocoff, na.rm = T),
  v2pasoctie = mean(v2pasoctie, na.rm = T)
) |> ungroup() -> vparty_comb

### Local organizational strength (C) (v2paactcom)
### Local party office (C) (v2palocoff)
### Affiliate organizations (C) (v2pasoctie)
### Welfare (C) (v2pawelf)
### Government support (A,C) (v2pagovsup) < 3
### Manipulated party duration

### Welfare coverage and law ---------------------------------------------------
#SPaW <- ezpickr::pick("Original_Data/SPaW_ver2.dta")

### Resources and Military size ------------------------------------------------
Miller <- ezpickr::pick("Original_Data/Miller2015.dta")

Miller <- Miller |> 
  select(ccode, year, resdep2, urban, cow_milsize)

### Control Vaiables -----------------------------------------------------------

qog <- ezpickr::pick(
  "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan23.dta"
  )
qog |> dplyr::select(COWcode = ccodecow, year, 
                     #gdp = wdi_gdppppcon2017,
                     #pop = wdi_pop, 
                     civil_war = ucdp_type3) ->
  qog_sub

qog_sub |> mutate(civilwar = if_else(!is.na(civil_war), 1L, 0L)) -> qog_sub

# civil war from qog
  
#QOG.ctrl <- QOG %>% 
#  select(ccodecow, year, 
#         wdi_trade, wdi_pop14, wdi_pop65, wdi_expmil)

vdem_subset |> mutate(
  regime = case_when(
    lexical_demo <  6 ~ "Autocracies (LIED < 3)",
    lexical_demo == 6 ~ "Democracies (LIED > 2)",
    T ~ NA_character_),
  regime = factor(regime, levels = c("Autocracies (LIED < 3)", "Democracies (LIED > 2)")),
  brm = factor(regime_boix,
               levels = c(0, 1),
               labels = c("Autocracies (BRM)", "Democracies (BRM)"))
) |> dplyr::select(country_name, year, regime, brm, edi) ->
  regime_subset


vdem_subset |> 
  dplyr::filter(
    (regime_boix %in% 0 & lexical_demo %in% 6) |
      (regime_boix %in% 1 & lexical_demo < 6)
  ) |> dplyr::select(country_name, year, regime_boix, lexical_demo, edi) -> counters

regime_subset |> drop_na(brm, regime) |> group_by(brm, regime) |> count() |> 
  group_by(regime) |> mutate(total = sum(n),
                             ratio = n/total) ->
  regime_summary

regime_subset |> drop_na(brm, regime) |> 
  ggplot(aes(x = fct_rev(regime), fill = fct_rev(brm))) +
  geom_bar(position = "fill", alpha = 0.9) +
  geom_text(
    data = regime_summary,
    aes(x = regime, y = ratio, 
        label = paste0(round(ratio*100, 2), "% ", "(",
                       format(n, big.mark = ",", scientific = F), ")")),
    position = position_stack(0.6), color = "white", size = 5, 
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggsci::scale_fill_lancet() +
  labs(x = "Dichotomous Regime Measurement using Lexical Index",
       y = "% of Observations") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "top")

ggsave("Documents/2_Manuscript/2_Figures/Fig1.pdf", width = 7, height = 4.5, dpi = "retina")



### Variables that I need to manipulate ----------------------------------------
##### DV: Welfare universality: universality
##### EV 1: Ruling coalition; mass-based vs. institution-based: coalition
##### EV 2: Mass party organization: vparty

vdem_subset |> mutate(
  mass_coalition = case_when(
    coalition %in% c(9, 10, 11, 12) ~ 1L,
    coalition %in% c(2, 5) ~ 0L,
    T ~ NA_integer_),
  regime = case_when(
    lexical_demo <  6 ~ "Autocracies (LIED < 3)",
    lexical_demo == 6 ~ "Democracies (LIED > 2)",
    T ~ NA_character_),
  regime = factor(regime, levels = c("Autocracies (LIED < 3)", "Democracies (LIED > 2)")),
  brm = factor(regime_boix,
               levels = c(0, 1),
               labels = c("Autocracies (BRM)", "Democracies (BRM)"))
) |> dplyr::select(COWcode, year, regime, brm, universality, coalition, mass_coalition, 
                   gdppc, gdp, resource_income, CSOconsult, CSOpart, 
                   hereditary_index, military_index, party_index) -> base

#### Figure 1 ####http://127.0.0.1:44169/graphics/plot_zoom_png?width=2834&height=1280

base |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> 
  #drop_na(year, mass_coalition) |> 
  dplyr::filter(year > 1964) |> 
  dplyr::filter(coalition %in% c(2, 5, 9, 10, 11, 12)) |> 
  #group_by(year, mass_coalition) |> count() |> ungroup() -> figure1
  drop_na(coalition, year) |> 
  mutate(
    coalition_agg = case_when(
      coalition %in% 2L ~ "Party elites",
      coalition %in% 5L ~ "Military elites",
      coalition %in% c(9L, 11L) ~ "Working class",
      coalition %in% c(10L, 12L) ~ "Middle class",
      T ~ NA_character_
    ),
    coalition_agg = factor(coalition_agg,
                       levels = c("Party elites", "Military elites",
                                  "Working class", "Middle class"))) |> 
  group_by(coalition_agg, year) |> 
  count() -> figure1


figure1 |> ungroup() |> group_by(coalition_agg) |> 
  dplyr::filter(year < 2005) |> 
  mutate(
    label = if_else(year == max(year), 
                    as.character(coalition_agg), NA_character_)) |> ungroup() -> figure1
figure1 |> 
  ggplot(aes(x = year, y = n)) +
  geom_path(aes(color = coalition_agg, 
                fill = coalition_agg), show.legend = F) +
  ggrepel::geom_label_repel(aes(x = year, label = label, color = coalition_agg), size = 5,
                            min.segment.length = Inf, 
                            nudge_y = -3.5, na.rm = TRUE, show.legend = F) + 
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  scale_y_continuous(breaks = c(seq(0, 60, 10))) +
  scale_x_continuous(breaks = c(seq(1965, 2015, 5))) +
  labs(y = "Frequency\n", x = "\nYear") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
        

ggsave("Documents/2_Manuscript/2_Figures/fig1_spsa.pdf",
       width = 8.5, height = 3.5, dpi = "retina")
getwd()

figure1 |> dplyr::select(coalition_agg, year) |>
  group_by(coalition_agg, year) |>
  drop_na(coalition_agg, year) |> 
  dplyr::filter(year > 1964) |> count() |> print(n = Inf)

base |> dplyr::select(COWcode, year, regime, brm, mass_coalition, coalition, year) |>
  drop_na(mass_coalition, year, coalition) |> 
  group_by(year) |> 
  #dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> 
  dplyr::filter(brm %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1964) |> mutate(n = length(mass_coalition),
                                       part_n = length(coalition)) |> 
  arrange(year, n, mass_coalition, coalition) -> select

#### Figure2 ####

base |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> 
  drop_na(year, mass_coalition) |> 
  dplyr::filter(year > 1964 & year < 2006) |> ungroup() |> 
  mutate(
    mass_coalition = case_when(
      mass_coalition %in% 1L ~ "Mass-based coalition",
      mass_coalition %in% 0L ~ "Elite-based coalition",
      T ~ NA_character_
    ),
    mass_coalition = factor(mass_coalition,
                            levels = c("Mass-based coalition",
                                       "Elite-based coalition"))) -> figure2

summary_fig2 <- figure2 |> group_by(mass_coalition) |> 
  summarize(mean = mean(universality, na.rm = T))

figure2 |> group_by(mass_coalition) |> 
  ggplot(aes(x = universality)) +
  geom_density(aes(color = mass_coalition, 
                   fill = mass_coalition), show.legend = F, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(
    data = summary_fig2,
    aes(xintercept = mean,
        color = mass_coalition, 
        shape = mass_coalition), show.legend = F) +
  ggrepel::geom_label_repel(
    data = summary_fig2,
    aes(x = mean, 
        color = mass_coalition,
        label = as.character(round(mean, 2))),
    y = 0.5,
    size = 5,
    nudge_y = 0.5,
    show.legend = F) +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  facet_wrap(~mass_coalition) +
  scale_y_continuous(label = scales::percent_format()) +
  labs(y = NULL, x = "\nUniversalism index of V-Dem") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))


ggsave("Documents/2_Manuscript/2_Figures/fig2_spsa.pdf",
       width = 6, height = 3.5, dpi = "retina")


##### Ctrl: State capacity -> repressive capacity
state_cap <- ezpickr::pick("Original_data/StateCapacityDataset_v1.dta/StateCapacityDataset_v1.dta")
state_cap_sub <- state_cap |> dplyr::select(COWcode = ccode, year, Capacity)

#####       ln(GDPpc)
#####       GDP growth: Growth Rate = (GDP_Year2/ GDP_Year 1) - 1
#####       Civil war experience
#####       Resource dependency

resource <- read_csv("Original_data/dataverse_files/Ross-Mahdavi Oil and Gas 1932-2014.csv")

resource |> mutate(COWcode = countrycode::countrycode(iso3numeric, "iso3n", "cown"),
                  lnresource = log(oil_gas_valuePOP_2014+1))  ->
  resource_clean

##### Personalization measurement

personalization <- rio::import("Analysis_data/personalization.dta")


#####       Type of regime

# remotes::install_github("xmarquez/democracyData")

gwf <- democracyData::gwf_all

gwf |> dplyr::select(gwf_cowcode, year, gwf_party, gwf_military, gwf_monarchy, gwf_personal) ->
  gwf_sub

#####       Past welfare
#####       Human rights score (Farris 2014)

hrs <- read_csv("Original_data/HumanRightsProtectionScores_v4.01.csv")

#####       Civil Society Strength
#####       - CSO consultation (C) (v2cscnsult)
#####       - CSO participatory environment (C) (v2csprtcpt)

vparty_comb |>
  mutate(massparty = v2paactcom + v2palocoff + v2pasoctie) -> vparty_comb

base |> left_join(vparty_comb |> drop_na(COWcode, year), 
                  by = c("COWcode", "year")) |> drop_na(COWcode, year) -> dem_party

dem_party |> left_join(
  state_cap_sub |> drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap

dem_party_cap |> left_join(
  hrs |> dplyr::select(COWcode = COW, year = YEAR, hrs = theta_mean) |> 
    drop_na(COWcode, year),
  by = c("COWcode", "year")
) -> dem_party_cap_hrs

dem_party_cap_hrs |> left_join(
  qog_sub |> drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap_hrs

dem_party_cap_hrs |> left_join(
  resource_clean |> dplyr::select(COWcode, year, lnresource) |> 
    drop_na(COWcode, year), by = c("COWcode", "year")
) -> dem_party_cap_hrs_resource

dem_party_cap_hrs_resource |> left_join(
  gwf_sub |> drop_na(gwf_cowcode, year), by = c("COWcode" = "gwf_cowcode", "year")
) -> dem_party_cap_hrs_resource_gwf

dem_party_cap_hrs_resource_gwf |> left_join(
  personalization |> drop_na(cowcode, year),
  by = c("COWcode" = "cowcode", "year")
) -> dem_party_cap_hrs_res_gwf_per


dem_party_cap_hrs_res_gwf_per |> dplyr::select(
  COWcode, year,
  universality, mass_coalition, massparty, gdppc, civilwar, hrs, lnresource, CSOconsult, CSOpart, Capacity,
  regime,
  contains("index"), personindex = xpers1,
  contains("gwf_")
) -> baseline

baseline |> group_by(COWcode) |> 
  mutate(lgdppc = dplyr::lag(gdppc, n = 1, order_by = year)) |> 
  ungroup() |>
  mutate(gdpgrth = (gdppc/lgdppc)-1,
         mass_coalition = factor(mass_coalition,
                                 levels = c(0, 1),
                                 labels = c("Institution-based", "Mass-based")),
         repression =  (hrs - mean(hrs, na.rm = T)) / sd(hrs, na.rm = T) * -1) -> 
  baseline

summary(baseline$repression)
baseline |> group_by(COWcode) |> 
  mutate(
    l_universality = dplyr::lag(universality, n = 1, order_by = year),
    l_mass_coalition = dplyr::lag(mass_coalition, n = 1, order_by = year),
    l_massparty = dplyr::lag(massparty, n = 1, order_by = year),
    l_gdppc = dplyr::lag(gdppc, n = 1, order_by = year),
    l_gdpgrth = dplyr::lag(gdpgrth, n = 1, order_by = year),
    l_lnresource = dplyr::lag(lnresource, n = 1, order_by = year),
    l_civilwar = dplyr::lag(civilwar, n = 1, order_by = year),
    l_hrs = dplyr::lag(hrs, n = 1, order_by = year),
    l_repression = dplyr::lag(repression, n = 1, order_by = year),
    l_capacity = dplyr::lag(Capacity, n = 1, order_by = year),
    l_CSOconsult = dplyr::lag(CSOconsult, n = 1, order_by = year),
    l_CSOpart = dplyr::lag(CSOpart, n = 1, order_by = year),
    l_hereditaryindex = dplyr::lag(hereditary_index, n = 1, order_by = year),
    l_militaryindex = dplyr::lag(military_index, n = 1, order_by = year),
    l_partyindex = dplyr::lag(party_index, n = 1, order_by = year),
    l_personindex = dplyr::lag(personindex, n = 1, order_by = year),
    l_royal = dplyr::lag(gwf_monarchy, n = 1, order_by = year),
    l_mil = dplyr::lag(gwf_military, n = 1, order_by = year),
    l_per = dplyr::lag(gwf_monarchy, n = 1, order_by = year),
    l_party = dplyr::lag(gwf_party, n = 1, order_by = year)
  ) |> ungroup() -> baseline
summary(baseline$l_militaryindex)

summary(baseline$hrs)

naniar::gg_miss_var(baseline)

### Baseline additive models without controls but fixed effects

glm(universality ~ l_mass_coalition + 
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model1

glm(universality ~ l_massparty + 
      as.factor(COWcode) + as.factor(year), data = baseline|> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model2

glm(universality ~ l_mass_coalition + l_massparty + 
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model3

texreg::screenreg(list(model1, model2, model3), omit.coef = "as.factor")

glm(universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model4
texreg::screenreg(model4, omit.coef = "as.factor")

baseline |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> 
  drop_na(universality, l_massparty, l_mass_coalition,
          l_gdppc, l_gdpgrth, l_lnresource,
          l_civilwar, l_repression, l_capacity, l_CSOconsult, l_CSOpart,
          l_royal, l_party, l_mil) |> 
  dplyr::select(COWcode) |> unique()

glm(universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model5

glm(universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)")) -> model6

glm(universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> 
      mutate(l_mass_coalition = as.numeric(l_mass_coalition)))  -> model7

# baseline |> 
#   dplyr::filter(regime %in% "Autocracies (LIED < 3)") |> group_by(mass_coalition) |> 
#   summarize(mean = mean(personindex, na.rm = T),
#             n = n())

glm(universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_hrs + l_CSOconsult + l_CSOpart + 
      l_mil + l_party + l_mon +
      #l_hereditaryindex + l_militaryindex + l_partyindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Democracies (LIED > 2)")) -> model7
#  texreg::screenreg(omit.coef = "as.factor")

glm(universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_hrs + l_CSOconsult + l_CSOpart +  l_hereditaryindex + l_militaryindex + l_partyindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Democracies (LIED > 2)")) -> model8

glm(universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_hrs + l_CSOconsult + l_CSOpart + l_hereditaryindex + l_militaryindex + l_partyindex +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> 
      dplyr::filter(regime %in% "Democracies (LIED > 2)")) -> model9

texreg::screenreg(list(model7, model8, model9),
                  omit.coef = "as.factor")  

glm(universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_hrs + l_CSOconsult + l_CSOpart +  + l_mil + l_royal + l_party +
      as.factor(COWcode) + as.factor(year), data = baseline |> mutate(l_mass_coalition = as.numeric(l_mass_coalition))) |> 
  ggeffects::ggpredict(terms = c( "l_massparty", "l_mass_coalition")) |> plot()

texreg::texreg(list(model1, model2, model3, model4, model5, model6, model7),
                  omit.coef = "as.factor",
               single.row = F,
                  custom.coef.names = c(
                    "(Intercept)",
                    "Mass-based coalition (vs. Elite-based)",
                    "Mass party organization",
                    "Ln(GDPpc)",
                    "Annual GDP growth",
                    "Resource Dependence (per GDPpc)",
                    "Civil war experience",
                    "Repression: Human Rights",
                    "Repression: State Capacity",
                    "CSO Consulation",
                    "CSO participatory environment",
                    "Monarchy (vs. Personalist)",
                    "Dominant party (vs. Personalist)",
                    "Junta (vs. Personalist)",
                    "Mass-based Coalition$\\times$Mass party organization"),
                  reorder.coef = c(2, 3, 15, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1),
                  custom.gof.rows = 
                    list("Country-fixed" = c("YES", "YES", "YES", "YES", "YES", "YES", "Yes"),
                         "Year-fixed" = c("YES", "YES", "YES", "YES", "YES", "YES", "Yes"),
                         "No. of countries" = c(
                           "141", "110", "99", "97", "93", "85", "85")))
                  

texreg::screenreg(
  list(model1, model2, model3, model4, model5, model6, model7, model8, model9),
  include.ci = F,
  omit.coef = "as.factor|(Intercept)",
  custom.coef.names = c(
    "Ruling Coalition (Mass-based = 1)",
    "Party Institutionalization",
    "Surveillance",
    "State Capacity",
    #"Ln(GDP)",
    "Ln(GDPpc)",
    #"Ln(Population)",
    "GDP growth",
    "Civil war experience",
    "Resource Dependence (per GDPpc)",
    #"Cold War Period",
    "Mass-based Coalition$\\times$Party Institutionalization",
    "Mass-based Coalition$\\times$Surveillance"),
  reorder.coef = c(1, 2, 3, 9, 10, 4, 5, 6, 7, 8),
  custom.gof.rows = 
    list("Country-fixed" = c("YES", "YES", "YES", "YES", "YES", 
                             "YES", "YES", "YES", "YES"),
         "No. of countries" = c(
           "129", "142", "142", "128", "129", "128", "100", "100", "100")))



texreg::texreg(list(model1, model2, model3, model4, model5, model6),
                  omit.coef = "as.factor")

interplot::interplot(model7, var1 = "l_mass_coalition", var2 = "l_massparty", 
                     plot = T,
                     hist = T, sims = 4000,
                     xmin = -8, xmax = 10,
                     ercolor = "#275D8E",
                     ralpha = 0.6,
                     rfill = "#275D8E") +
  labs(y = "Estimated Coefficient for Mass Coalition\n",
       title = NULL, x = "\nLevels of Mass Party Organization",
       subtitle = NULL, caption = NULL) + 
  scale_x_continuous(breaks = c(seq(-5, 10, 5))) +
  scale_y_continuous(breaks = c(seq(-2, 6, 1))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

ggsave("Documents/2_Manuscript/2_Figures/fig3_spsa.pdf", width = 7, height = 4.5, dpi = "retina")

interplot::interplot(model9, var1 = "l_mass_coalition", var2 = "", 
                     plot = T,
                     hist = T, sims = 4000,
                     xmin = -8, xmax = 10,
                     ercolor = "#275D8E",
                     ralpha = 0.6,
                     rfill = "#275D8E") +
  ylab("Estimated Coefficient for Mass Coalition\n") + 
  geom_hline(yintercept = 0, linetype = "dashed")



hist(baseline$massparty)
glm(universality ~ 
      l_mass_coalition + 
      l_massparty + 
      log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> dplyr::filter(regime %in% "Democracies (LIED > 2)")) |>   
  texreg::screenreg(omit.coef = "as.factor")

glm(universality ~ 
      l_mass_coalition*l_massparty + 
      log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> dplyr::filter(regime %in% "Autocracies (LIED < 3)")) |> 
    #data = baseline |> dplyr::filter(regime %in% "Democracies (LIED > 2)")) |> 
  ggeffects::ggpredict(terms = c("l_massparty", "l_mass_coalition")) |> plot() +
  facet_wrap(~group)
  #texreg::screenreg(omit.coef = "as.factor")
  #interplot(var1 = "l_mass_coalition", var2 = "l_massparty", plot = T, hist = T) +
    #geom_hline(yintercept = 0)


m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
summary(m_cyl)

library(interplot)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt")

baseline |> dplyr::select(
  COWcode, year,
  universality, mass_coalition, massparty, l_gdppc, gdpgrth, lnresource,
    civilwar, l_hrs, CSOconsult, CSOpart, hereditary_index, military_index, party_index) |> 
  drop_na() |> dplyr::select(year) |> summary()

#  [1]  20  40  41  42  51  52  53  70  90  91  92  93  94  95 100 101 130 135 
#      140 145 150 155 160 165 205 210 211 212 220 225 230 235 255 290 305 310 
#      316 317 325 338 339 341 343 344 345 349 350 352 355 359 360 365 366 367 
#      368 369 371 372 373 375 380 385 390 395 402 403 404 411 420 432 433 434 
#      435 436 437 438 439 450 451 452 461 471 475 481 482 483 484 490 500 501
#      510 516 517 520 522 530 531 540 541 551 552 553 560 570 571 580 581 590 
#      591 615 616 620 625 630 640 645 651 652 660 679 702 704 710 712 713 731
#      732 740 750 770 771 775 780 781 790 800 811 812 816 830 840 850 860 900 
#      920 935 950

#[1] 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979
#    1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994
#    1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 
#    2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

panelView::panelview(universality ~ mass_coalition,
                     #+ massparty + gdppc + civilwar + 
                      # hrs + CSOconsult + CSOpart, 
          data = dem_party_cap_hrs, index = c("COWcode","year"), 
          xlab = "Year", ylab = "State", by.timing = TRUE, 
          legend.labs = c("Institution-based", "Mass-based", "Missings"), background = "white", 
          cex.main = 20, cex.axis= 8, cex.lab = 12, cex.legend = 12)

bind_rows(regime_subset |> drop_na(brm, regime) |> 
            dplyr::select(regime = brm, edi) |> 
            mutate(indicator = "BRM",
                   regime_id = if_else(grepl("Democrac", regime), "Democracies", "Autocracies")),
          regime_subset |> drop_na(brm, regime) |> 
            dplyr::select(regime, edi) |> 
            mutate(indicator = "LID",
                   regime_id = if_else(grepl("Democrac", regime), "Democracies", "Autocracies"))) ->
  regime_diff_edi 

regime_diff_edi |> group_by(regime_id, regime, indicator) |>  
  summarize(edi_mean = mean(edi, na.rm= T)) -> regime_mean_edi

regime_diff_edi  |> 
  ggplot(aes(x = edi, group = fct_rev(regime), fill = fct_rev(regime_id), color = fct_rev(regime_id))) +
  geom_density(aes(y = ..scaled..), alpha = 0.8) +
  labs(x = "\nElectoral Democracy Index",
       y = "Density") +
  geom_vline(
    data = regime_mean_edi,
    aes(xintercept = edi_mean, color = regime_id), show.legend = F) +
  geom_text(
    data = regime_mean_edi,
    aes(x = edi_mean, y = 1.1, label = round(edi_mean, 3)),
    nudge_x = 0.1, show.legend = F) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~indicator) + 
  ggsci::scale_fill_lancet() +
  ggsci::scale_color_lancet() +
  theme(legend.title = element_blank())

ggsave("Documents/2_Manuscript/2_Figures/Fig2.pdf", width = 7, height = 4.5, dpi = "retina")

table(vdem_subset$regime_boix)

table(baseline$regime)


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

Analysis_data_noclean <- 
  Analysis_data_noclean %>% 
  dplyr::select(
    everything(), export, import, corruption, clientalism,
    women_polpar, civil_par
  )

foreign::write.dta(Analysis_data_noclean, "Analysis_Data/Analysis_data_noclean.dta")

