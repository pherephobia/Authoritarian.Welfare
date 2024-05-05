## Project: Why do authoritarian regimes provide welfare programs? -------------
##          4. Robustness Checks  --------------------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:

## Import Packages to use ------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")

pacman::p_load(ggplot2, grid, pBrackets, ezpickr, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)
#options("modelsummary_format_numeric_latex" = "plain")

## Set the plot theme
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

## For the fonts on ggplot axis labels
showtext::showtext_auto(T)

## Apply the theme_clean() to ggplot() in general
theme_set(theme_bw())

## Import Data
baseline <- readRDS("Analysis_data/baseline.RDS")



## Different Regime Measurements ------------------------------------------------------------------
### V-Dem RoW Regime -------------------------------------------------------------------------------
#### Benchmark -------------------------------------------------------------------------------------
bench1_rand <- plm::plm(universality ~ l_mass_coalition,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                          dplyr::filter(vdem_regime %in% c(0, 1)))

bench2_rand <- plm::plm(universality ~ l_massparty,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                          dplyr::filter(vdem_regime %in% c(0, 1)))

bench3_rand <- plm(universality ~ l_mass_coalition + l_massparty,
                   index = c("COWcode", "year"),
                   model = "random", random.method = "walhus",
                   data = baseline |> 
                     dplyr::filter(vdem_regime %in% c(0, 1)))

bench4_rand <- plm(universality ~ l_mass_coalition*l_massparty,
                   index = c("COWcode", "year"),
                   model = "random", random.method = "walhus",
                   data = baseline |> 
                     dplyr::filter(vdem_regime %in% c(0, 1)))

bench5_cfixed <- plm(universality ~ l_mass_coalition,
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(vdem_regime %in% c(0, 1)))

bench6_cfixed <- plm(universality ~ l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(vdem_regime %in% c(0, 1)))

bench7_cfixed <- plm(universality ~ l_mass_coalition + l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(vdem_regime %in% c(0, 1)))

bench8_cfixed <- plm(universality ~ l_mass_coalition*l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(vdem_regime %in% c(0, 1)))

bench9_tfixed <- plm(universality ~ l_mass_coalition +
                       as.factor(COWcode) + as.factor(year),
                     index = c("COWcode", "year"),
                     effect = "twoways",
                     data = baseline |> 
                       dplyr::filter(vdem_regime %in% c(0, 1)))

bench10_tfixed <- plm(universality ~ l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(vdem_regime %in% c(0, 1)))

bench11_tfixed <- plm(universality ~ l_mass_coalition + l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(vdem_regime %in% c(0, 1)))

bench12_tfixed <- plm(universality ~ l_mass_coalition*l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(vdem_regime %in% c(0, 1)))

panels <- list(
  "Random Effects" = list(
    bench1_rand, bench2_rand, bench3_rand, bench4_rand),
  "Country-Fixed" = list(
    bench5_cfixed, bench6_cfixed, bench7_cfixed, bench8_cfixed),
  "Country-Year-Fixed" = list(
    bench9_tfixed, bench10_tfixed, bench11_tfixed, bench12_tfixed)
)
rows1 <- tribble(~term,        
                 ~bench1_rand,  ~bench2_rand, ~bench3_rand, ~bench4_rand,
                 ~bench5_cfixed, ~bench6_cfixed, ~bench7_cfixed, ~bench8_cfixed,
                 ~bench9_tfixed, ~bench10_tfixed, ~bench11_tfixed, ~bench12_tfixed,
                 'Controls', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO')
attr(rows1, 'position') <- c(7)
modelsummary::modelsummary(
  panels, stars = TRUE, shape = "rbind",
  coef_map = c("l_mass_coalitionMass-based" = "Mass-Based Coalition (vs. Elite-Based)",
               "l_massparty" = "Mass Party Organization Index",
               "l_mass_coalitionMass-based:l_massparty" = 
                 "Mass-Based Coalition\\times Mass Party Organization Index"),
  #add_rows = rows1,
  coef_omit = "as.factor",
  gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex'
)

#### Full models -----------------------------------------------------------------------------------

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed4


panels <- list(
  "Random Effects" = list(
    "Model 1" = full_random1, "Model 2" = full_random2),
  "Country-Fixed Effects" = list(
    "Model 3" = full_cfixed1, "Model 4" = full_cfixed2), 
  "Country-Year-Fixed Effeects" = list(
    "Model 5" = full_tfixed1, "Model 6" = full_tfixed2)
)

library(tibble)
rows <- tribble(~term,        
                ~full_random1,  ~full_random2, ~full_random3,  ~full_random4,  
                ~full_cfixed1, ~full_cfixed2, ~full_cfixed3, ~full_cfixed4, 
                ~full_tfixed1, ~full_tfixed2,~full_tfixed3, ~full_tfixed4,
                'Random Effects', 
                'YES', 'YES', 'YES', 'YES',
                'NO', 'NO', 'NO', 'NO',
                'NO', 'NO', 'NO', 'NO',
                'Country-Fixed', 
                'No',  'No', 'No',  'No',
                'YES', 'YES', 'YES', 'YES',
                'YES', 'YES', 'YES', 'YES',
                'Year-Fixed', 
                'No', 'No', 'No', 'No', 
                'No', 'No', 'No', 'No',
                'YES', 'YES', 'YES', 'YES')
attr(rows, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(full_random1, full_random2, full_random3, full_random4,  
                                full_cfixed1, full_cfixed2, full_cfixed3, full_cfixed4,  
                                full_tfixed1, full_tfixed2, full_tfixed3, full_tfixed4), 
                           stars = TRUE,
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')

### Boix et al. (2022): BRM Regime ---------------------------------------------
#### Benchmark --------------------------------------------
bench1_rand_brm <- plm::plm(universality ~ l_mass_coalition,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                          dplyr::filter(brm_regime %in% 
                                          c("Autocracies (BRM)")))

bench2_rand_brm <- plm::plm(universality ~ l_massparty,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                          dplyr::filter(brm_regime %in% 
                                          c("Autocracies (BRM)")))

bench3_rand_brm <- plm(universality ~ l_mass_coalition + l_massparty,
                   index = c("COWcode", "year"),
                   model = "random", random.method = "walhus",
                   data = baseline |> 
                     dplyr::filter(brm_regime %in% 
                                     c("Autocracies (BRM)")))

bench4_rand_brm <- plm(universality ~ l_mass_coalition*l_massparty,
                   index = c("COWcode", "year"),
                   model = "random", random.method = "walhus",
                   data = baseline |> 
                     dplyr::filter(brm_regime %in% 
                                     c("Autocracies (BRM)")))

bench5_cfixed_brm <- plm(universality ~ l_mass_coalition,
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(brm_regime %in% 
                                       c("Autocracies (BRM)")))

bench6_cfixed_brm <- plm(universality ~ l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(brm_regime %in% 
                                       c("Autocracies (BRM)")))

bench7_cfixed_brm <- plm(universality ~ l_mass_coalition + l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(brm_regime %in% 
                                       c("Autocracies (BRM)")))

bench8_cfixed_brm <- plm(universality ~ l_mass_coalition*l_massparty +
                       as.factor(COWcode),
                     index = c("COWcode", "year"),
                     effect = "individual",
                     data = baseline |> 
                       dplyr::filter(brm_regime %in% 
                                       c("Autocracies (BRM)")))

bench9_tfixed_brm <- plm(universality ~ l_mass_coalition +
                       as.factor(COWcode) + as.factor(year),
                     index = c("COWcode", "year"),
                     effect = "twoways",
                     data = baseline |> 
                       dplyr::filter(brm_regime %in% 
                                       c("Autocracies (BRM)")))

bench10_tfixed_brm <- plm(universality ~ l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(brm_regime %in% 
                                        c("Autocracies (BRM)")))

bench11_tfixed_brm <- plm(universality ~ l_mass_coalition + l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(brm_regime %in% 
                                        c("Autocracies (BRM)")))

bench12_tfixed_brm <- plm(universality ~ l_mass_coalition*l_massparty +
                        as.factor(COWcode) + as.factor(year),
                      index = c("COWcode", "year"),
                      effect = "twoways",
                      data = baseline |> 
                        dplyr::filter(brm_regime %in% 
                                        c("Autocracies (BRM)")))

panels <- list(
  "Random Effects" = list(
    bench1_rand_brm, bench2_rand_brm, bench3_rand_brm, bench4_rand_brm),
  "Country-Fixed" = list(
    bench5_cfixed_brm, bench6_cfixed_brm, bench7_cfixed_brm, bench8_cfixed_brm),
  "Country-Year-Fixed" = list(
    bench9_tfixed_brm, bench10_tfixed_brm, bench11_tfixed_brm, bench12_tfixed_brm)
)
rows1 <- tribble(~term,        
                 ~bench1_rand_brm,  ~bench2_rand_brm, ~bench3_rand_brm, ~bench4_rand_brm,
                 ~bench5_cfixed_brm, ~bench6_cfixed_brm, ~bench7_cfixed_brm, ~bench8_cfixed_brm,
                 ~bench9_tfixed_brm, ~bench10_tfixed_brm, ~bench11_tfixed_brm, ~bench12_tfixed_brm,
                 'Controls', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO')
attr(rows1, 'position') <- c(7)

# save(list = c("bench1_rand_brm",   "bench2_rand_brm",    "bench3_rand_brm",    "bench4_rand_brm",
#               "bench5_cfixed_brm", "bench6_cfixed_brm",  "bench7_cfixed_brm",  "bench8_cfixed_brm",
#               "bench9_tfixed_brm", "bench10_tfixed_brm", "bench11_tfixed_brm", "bench12_tfixed_brm"),
#      file = "Analysis_data/brm_robust.RData")

modelsummary::modelsummary(
  panels, stars = TRUE, shape = "rbind",
  coef_map = c("l_mass_coalitionMass-based" = "Mass-Based Coalition (vs. Elite-Based)",
               "l_massparty" = "Mass Party Organization Index",
               "l_mass_coalitionMass-based:l_massparty" = 
                 "Mass-Based Coalition$\\times$ Mass Party Organization Index"),
  #add_rows = rows1,
  coef_omit = "as.factor",
  gof_omit = 'DF|Deviance|R2|adjR2', 
  #output = "html"
  #output = 'latex'
  output = "latex"
)


#### Full models -----------------

baseline |> 
  dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()
  #unique() |> length()

## additive: 94         1966-2011
## MPO only: 100        1966-2011
## Coalition only: 100  1947-2011

baseline |> 
  dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_random1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_random2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_random3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_random4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_cfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_cfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_cfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_cfixed4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_tfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_tfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_tfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> brm_full_tfixed4

library(tibble)
rows <- tribble(~term,        
                ~brm_full_random1,  ~brm_full_random2, ~brm_full_random3,  ~brm_full_random4,  
                ~brm_full_cfixed1, ~brm_full_cfixed2, ~brm_full_cfixed3, ~brm_full_cfixed4, 
                ~brm_full_tfixed1, ~brm_full_tfixed2,~brm_full_tfixed3, ~brm_full_tfixed4,
                'Random Effects', 
                'YES', 'YES', 'YES', 'YES',
                'NO', 'NO', 'NO', 'NO',
                'NO', 'NO', 'NO', 'NO',
                'Country-Fixed', 
                'No',  'No', 'No',  'No',
                'YES', 'YES', 'YES', 'YES',
                'YES', 'YES', 'YES', 'YES',
                'Year-Fixed', 
                'No', 'No', 'No', 'No', 
                'No', 'No', 'No', 'No',
                'YES', 'YES', 'YES', 'YES')
attr(rows, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(brm_full_random1, brm_full_random2, brm_full_random3, brm_full_random4,  
                                brm_full_cfixed1, brm_full_cfixed2, brm_full_cfixed3, brm_full_cfixed4,  
                                brm_full_tfixed1, brm_full_tfixed2, brm_full_tfixed3, brm_full_tfixed4), 
                           stars = TRUE,
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')

### Jack knife using BRM ---------------------------------------------------------------------------

baseline |> 
  dplyr::select(
    COWcode, year, universality, 
    l_mass_coalition,
    l_massparty,
    l_gdppc, l_gdpgrth,
    l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
    l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex,
    COWcode, year, brm_regime) |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> drop_na() |> pull(COWcode) |> 
  unique() -> unique_COWfull_brm

texreg::texreg(list(full_model3, full_model4),
               omit.coef = "as.factor",
               single.row = T,
               custom.coef.names = c(
                 "(Intercept)",
                 "Mass-Based Coalition (vs. Elite-Based)",
                 "Mass Party Organization Index",
                 "Ln(GDPpc + 1)",
                 "Annual GDP Growth",
                 "Resource Dependence (Per GDPpc)",
                 "Civil War Experience",
                 "Repression: Human Rights",
                 #"Repression: State Capacity",
                 "CSO Consulation",
                 "CSO Participatory Environment",
                 "Hereditary Dimension",
                 "Military Dimension",
                 "Ruling Party Dimension",
                 "Personalist Dimension",
                 "Mass-Based Coalition\\times Mass Party Organization Index"),
               reorder.coef = c(2, 3, 15, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1),
               custom.gof.rows = 
                 list("Country-fixed" = c("YES", "YES"),
                      "Year-fixed" = c("YES", "YES"),
                      "No. of countries" = c("88", "88"),
                      "Year coverage" = c("1966-2011", "1966-2011")))





#### Jackknife for full model 4 -------------------------------------------------------------------- 
jk_full4_brm <- data.frame()
# 
# glm(universality ~ l_mass_coalition*(l_massparty + I(l_massparty^2)) + log(l_gdppc + 1) + 
#       l_gdpgrth + l_lnresource + l_civilwar + l_repression + 
#       l_CSOconsult + l_CSOpart + l_hereditaryindex + l_militaryindex + 
#       l_partyindex + l_personindex + as.factor(COWcode) + as.factor(year),
#     #   index = c("COWcode", "year"),
#     #model = "random", , random.method = "walhus",
#     #  effect = "twoways",
#     data = baseline |> 
#       dplyr::filter(vdem_regime %in% c(0, 1) & !COWcode %in% 816) |> 
#       dplyr::mutate(l_mass_coalition = as.numeric(l_mass_coalition))) |> 
#   #broom::augment() -> check
#   interplot::interplot(var1 = "l_mass_coalition", var2 = "l_massparty")
# commun |> dplyr::select(COWcode) |> unique()

# baseline |> drop_na(l_mass_coalition) |> 
#   mutate(cut_party = cut(l_massparty, breaks = c(seq(-5, 10, 1)))) |> 
#   group_by(l_mass_coalition, cut_party) |> 
#   summarize(median = median(universality, na.rm = T),
#             ll = quantile(universality, probs = 0.025, na.rm = T),
#             ul = quantile(universality, probs = 0.975, na.rm = T)) |> ungroup() |> 
#   ggplot(aes(x = cut_party, y = median, color = as.factor(l_mass_coalition), group = 1)) + 
#   geom_line() + geom_line(aes(y = ll)) + geom_line(aes(y = ul)) + geom_point() + facet_wrap(~l_mass_coalition)
# names(check)
# baseline |> 
#   dplyr::filter(vdem_regime %in% c(0, 1) & COWcode %in% 816) |> 
#   ggplot(aes(x = l_massparty, y = universality))
# 
# baseline |> 
#   dplyr::filter(vdem_regime %in% c(0, 1)) |> 
#   dplyr::filter(year > 1965) |> 
#   dplyr::select(COWcode, universality, l_mass_coalition, l_massparty,
#                 l_gdppc, l_gdpgrth, l_lnresource, l_civilwar, l_repression, 
#                 l_CSOconsult, l_CSOpart, l_hereditaryindex, l_militaryindex, 
#                 l_partyindex, l_personindex) |> drop_na() |> 
#   dplyr::select(COWcode) |> 
#   mutate(cname = countrycode::countrycode(COWcode, "cown", "country.name")) |> 
#   unique() |> 
#   arrange(COWcode) |> 
#   dplyr::filter(grepl("Ukri", cname)) |> 
#   print(n = Inf) 
# 
# commun <- readxl::read_xlsx("Analysis_data/communist_countries_data.xlsx")
# 
# baseline |> mutate(
#   communism = case_when(
#     COWcode == 365 & year <= 1991 ~ 1L,
#     COWcode == 710 ~ 1L,
#     COWcode == 40 ~ 1L,
#     COWcode == 816 ~ 1L,
#     COWcode == 731 ~ 1L,
#     COWcode == 265 & year <= 1990 ~ 1L,
#     COWcode == 360 & year <= 1989 ~ 1L,
#     COWcode == 355 & year <= 1990 ~ 1L,
#     COWcode == 310 & year <= 1989 ~ 1L,
#     COWcode == 290 & year <= 1989 ~ 1L,
#     COWcode == 315 & year <= 1989 ~ 1L,
#     COWcode == 812 & year >= 1975 ~ 1L,
#     COWcode == 811 & year >= 1975 & year <= 1989 ~ 1L,
#     COWcode == 700 & year >= 1978 & year <= 1992 ~ 1L,
#     COWcode ==  370 ~ 1L,
#     COWcode ==  369 ~ 1L,
#     COWcode ==  704 ~ 1L,
#     COWcode ==  703 ~ 1L,
#     COWcode ==  372 ~ 1L,
#     COWcode ==  373 ~ 1L,
#     COWcode ==  368 ~ 1L,
#     COWcode ==  359 ~ 1L,
#     COWcode ==  367 ~ 1L,
#     COWcode ==  702 ~ 1L,
#     COWcode ==  701 ~ 1L,
#     COWcode ==  371 ~ 1L,
#     COWcode ==  705 ~ 1L,
#     COWcode ==  366 ~ 1L,
#     T ~ 0L
#   )) |> group_by(COWcode) |> mutate(communism = dplyr::lag(
#     communism, order_by = year, 1
#   )) |> ungroup() -> baseline 

mpo_int <- c(seq(-5, 10, 0.01))
coef_f4main_brm <- MASS::mvrnorm(n = 4000, mu = coef(brm_full_tfixed4), Sigma = vcov(brm_full_tfixed4))
me_f4main_brm <- coef_f4main_brm[, c("l_mass_coalition", "l_mass_coalition:l_massparty")] %*% rbind(1, mpo_int) 
f4main_brm_df <- data.frame(
  MPO = mpo_int,
  Mean = apply(me_f4main_brm, 2, mean, na.rm = T),
  lower = apply(me_f4main_brm, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(me_f4main_brm, 2, quantile, probs = 0.975, na.rm = T)
) |> mutate(Exclude = paste0("No County excluded"),
            id = paste0("0"))

for (i in 1:length(unique_COWfull_brm)) {
  tryCatch({
    model <- glm(
      universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
        l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
        l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
        as.factor(COWcode) + as.factor(year),
      data = baseline |> 
        dplyr::filter(brm_regime %in% "Autocracies (BRM)" & !COWcode %in% unique_COWfull_brm[i]))
    mpo_int <- c(seq(-5, 10, 0.01))
    coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
    me <- coef[, c("l_mass_coalitionMass-based", "l_mass_coalitionMass-based:l_massparty")] %*% rbind(1, mpo_int) 
    temp_df <- data.frame(
      MPO = mpo_int,
      Mean = apply(me, 2, mean, na.rm = T),
      lower = apply(me, 2, quantile, probs = 0.025, na.rm = T),
      upper = apply(me, 2, quantile, probs = 0.975, na.rm = T)
    ) |> 
      mutate(Exclude = paste0("County ", unique_COWfull_brm[i], " excluded"),
             id = paste0(unique_COWfull_brm[i]))
    jk_full4_brm <- jk_full4_brm |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}

unique_COWfull_brm[17];unique_COWfull_brm[36]
jk_full4_brm |> bind_rows(f4main_brm_df) ->
  jk_full4_brm_final

baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::select(COWcode, universality, l_mass_coalition, l_massparty,
                l_gdppc, l_gdpgrth, l_lnresource, l_civilwar, l_repression, 
                l_CSOconsult, l_CSOpart, l_hereditaryindex, l_militaryindex, 
                l_partyindex, l_personindex) |> drop_na() |> 
  summarize(
    massparty = quantile(l_massparty, probs = c(0.1, 0.9), na.rm = T),
    meanlngdppc = mean(log(l_gdppc + 1), na.rm = T),
    meangdpgrth = mean(l_gdpgrth, na.rm = T),
    meanresource = mean(l_lnresource, na.rm = T),
    meancivilwar = median(l_civilwar, na.rm = T),
    meanrepression = mean(l_repression, na.rm = T),
    meancsocon = mean(l_CSOconsult, na.rm = T),
    meancsopart = mean(l_CSOpart, na.rm = T),
    meanhereditary = mean(l_hereditaryindex, na.rm = T),
    meanmilitary = mean(l_militaryindex, na.rm = T),
    meanparty = mean(l_partyindex, na.rm = T),
    meanpersonal = mean(l_personindex, na.rm = T)
  ) -> check_brm

coef(brm_full_tfixed4)[1]*1 + coef(brm_full_tfixed4)[14]*as.numeric(check_brm[1, 1])  -> low

coef(brm_full_tfixed4)[1]*1 + coef(brm_full_tfixed4)[14]*check_brm[2, 1] -> high

point <- data.frame(MPO = c(as.numeric(check_brm[1, 1]), as.numeric(check_brm[2, 1])),
                    Mean =c(as.numeric(low), as.numeric(high)) ,
                    id = c("0", "0"))

jk_full4_brm_final |> 
  ggplot(aes(x = MPO, y = Mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  geom_line(aes(color = id), linetype = "dashed", show.legend = F) +
  geom_line(aes(y = lower, color = id), show.legend = F) +
  geom_line(aes(y = upper, color = id), show.legend = F) +
  geom_point(data = point, aes(MPO, Mean, fill = id),
             shape = 21, size = 3, show.legend = F) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(seq(-5, 10, 1)), expand = c(.05,.05)) +
  scale_y_continuous(breaks = c(seq(0, 7, 1))) +
  labs(x = NULL, y = "Estimated marginal effects\n") +
  gghighlight::gghighlight(id %in% "0", use_direct_label = FALSE) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.5)),
        plot.caption = element_text(size = rel(1.3))) -> ME_sim


baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  ggplot(aes(x = l_massparty, y = ..density..)) + 
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(breaks = c(seq(-5, 10, 1)), lim = c(-5, 10), expand = c(.05,.05)) +
  labs(x = expression('Mass Party Organization Index'[t-1]), y = "Density\n",
       caption = str_wrap("\nNote: The figure depicts the impact of mass-based coalitions on welfare universalism, with the main result from the full model highlighted in color. Each line plot is shown with 95% confidence intervals. The colored points represent the estimated marginal effects at the bottom and top ten percentiles of the mass party organization index. Grey lines represent the marginal effects of models using the jackknife method, with each model excluding one country from the analysis.", 100, exdent = 10)) +
  theme(plot.caption = element_text(hjust = 0, size = rel(1.3)),
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.5)))-> MPO_hist

library(patchwork)
library(ggExtra)

ME_sim + MPO_hist +
  patchwork::plot_layout(
    ncol = 1, 
    nrow = 2, 
    widths = 4,
    heights = c(6.5, 1.5)
  )

ggsave("Documents/2_Manuscript/2_Figures/Fig_me_jk_brm.pdf",
       width = 9, height = 7, dpi = 1600)

#### Jackknife Regression Model Aggregation ----------------------------------------------

main_brm_bench <- glm(
  universality ~ l_mass_coalition + l_massparty + 
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)" & 
                    !COWcode %in% unique_COWfullnoctrl_brm[i])
)



bind_rows(
  bench11_tfixed_brm |> broom::tidy() |> bind_cols(
    broom::confint_tidy(bench11_tfixed_brm)) |> 
    dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
    mutate(Exclude = "None", id = "0") |> 
    mutate(term = if_else(term %in% "l_mass_coalitionMass-based", "l_mass_coalition", term)) |> 
    dplyr::filter(term %in% c("l_mass_coalition", "l_massparty")) |> 
    mutate(term = case_when(
      term == "l_mass_coalition" ~ "Mass Coalition",
      term == "l_massparty" ~ "Mass Party Org."),
      Model = "Model 1 (without Controls)"),
  bench12_tfixed_brm |> broom::tidy() |> bind_cols(
    broom::confint_tidy(bench12_tfixed_brm)) |> 
    dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
    mutate(Exclude = "None", id = "0") |> 
    mutate(term = if_else(term %in% "l_mass_coalitionMass-based", "l_mass_coalition", 
                          if_else(term %in% "l_mass_coalitionMass-based:l_massparty",
                                  "l_mass_coalition:l_massparty", term))) |> 
    dplyr::filter(term %in% c("l_mass_coalition", "l_massparty", "l_mass_coalition:l_massparty")) |> 
    mutate(term = case_when(
      term == "l_mass_coalition" ~ "Mass Coalition",
      term == "l_massparty" ~ "Mass Party Org.",
      term == "l_mass_coalition:l_massparty" ~ "Mass Coalition X Mass Party Org."),
      Model = "Model 2 (without Controls)"),
  brm_full_tfixed3 |> broom::tidy() |> bind_cols(
    broom::confint_tidy(brm_full_tfixed3)) |> 
    dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
    mutate(Exclude = "None", id = "0") |> 
    mutate(term = if_else(term %in% "l_mass_coalitionMass-based", "l_mass_coalition", 
                          if_else(term %in% "l_mass_coalitionMass-based:l_massparty",
                                  "l_mass_coalition:l_massparty", term))) |> 
    dplyr::filter(term %in% c("l_mass_coalition", "l_massparty")) |> 
    mutate(term = case_when(
      term == "l_mass_coalition" ~ "Mass Coalition",
      term == "l_massparty" ~ "Mass Party Org."),
      Model = "Model 3 (with Controls)"),
  brm_full_tfixed4 |> broom::tidy() |> bind_cols(
    broom::confint_tidy(brm_full_tfixed4)) |> 
    dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
    mutate(Exclude = "None", id = "0") |> 
    mutate(term = if_else(term %in% "l_mass_coalitionMass-based", "l_mass_coalition", 
                          if_else(term %in% "l_mass_coalitionMass-based:l_massparty",
                                  "l_mass_coalition:l_massparty", term))) |> 
    dplyr::filter(term %in% c("l_mass_coalition", "l_massparty", "l_mass_coalition:l_massparty")) |> 
    mutate(term = case_when(
      term == "l_mass_coalition" ~ "Mass Coalition",
      term == "l_massparty" ~ "Mass Party Org.",
      term == "l_mass_coalition:l_massparty" ~ "Mass Coalition X Mass Party Org."),
      Model = "Model 4 (with Controls)")
) -> complete_addtive


bind_rows(
  "Model 1 (without Controls)" = jk_full1_brm |> rowid_to_column(),
  "Model 2 (without Controls)" = jk_full2_brm |> rowid_to_column(),
  "Model 3 (with Controls)" = jk_full3_brm |> rowid_to_column(),
  "Model 4 (with Controls)" = jk_full4_brm |> rowid_to_column(),
  .id = "Model"
) -> additive

bind_rows(
  complete_addtive, additive
) -> additive_fullset

library(gghighlight)
additive_fullset |> 
  mutate(
    base = if_else(id == "0", 1L, 0L)
    # country = if_else(insig == 1L & id == 93, "!Nicaragua",
    #                   if_else(
    #                     insig == 1L & id == 816, "!Vietnam", NA_character_)),
    # id = if_else(insig == 1L, id, NA_character_)
  ) ->
  additive_fullset
additive_fullset |> dplyr::filter(term %in% "Mass Coalition X Mass Party Org.") |> 
  dplyr::filter(ll < 0 & ul > 0) |>
  dplyr::filter(Model %in% "Model 4 (with Controls)") |> print(n = Inf) |> count()
additive_fullset |> dplyr::filter(term %in% "Mass Coalition X Mass Party Org.") |> count()
additive_fullset |> ungroup() |> 
  mutate(term = factor(term,
                       levels = c("Mass Coalition", 
                                  "Mass Party Org.", 
                                  "Mass Coalition X Mass Party Org."))) |> 
  ggplot(aes(y = median, x = term, color = term)) +
  geom_linerange(aes(ymin = ll, ymax = ul),
                 position = position_dodge2(0.5),
                 show.legend = F) +
  geom_pointrange(aes(ymin = ll, ymax = ul),
                  position = position_dodge2(0.5),
                  show.legend = F) +
  geom_point(position = position_dodge2(0.5),
             show.legend = F) + 
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  # ggrepel::geom_label_repel(
  #   aes(y = median, x = term, label = country), max.overlaps = Inf,
  #   color = "darkred", fill = "white", # Ensure these colors stand out against the grey unhighlight
  #   #position = position_dodge2(0.5),
  #   box.padding = 1,
  #   nudge_y = 0,
  #   nudge_x = 0,
  #   direction = 'x',
  #   min.segment.length = 0,
  #   size = 2.5, show.legend = F) +
  gghighlight::gghighlight(base == 1L,
                           calculate_per_facet = T,
                           use_direct_label = F,
                           unhighlighted_params = list(color = "grey80", fill = "white")) +
  labs(y = "\nEstimates", x = NULL,
       caption = str_wrap("Note: The colored estimates for key explanatory variables are derived from models with/without control variables and fixed effects. The cross-sectional Jackknife analysis yielded grey-colored estimates.", 95,
                          exdent = 10)) +
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3],
                                futurevisions::futurevisions("mars")[4])) +
  facet_wrap(~Model, ncol = 2) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0, size = 10))

ggsave("Documents/2_Manuscript/2_Figures/Fig_coef_jk.pdf",
       width = 8, height = 5, dpi = 1600)

ggsave("Documents/2_Manuscript/2_Figures/Fig_coef_jk.png",
       width = 8, height = 5, dpi = 1600)

jk_df |> 
  dplyr::filter(term %in% "Mass Coalition") |> 
  mutate(id = as.numeric(id)) |> 
  ggplot(aes(x = id, y = median)) +
  geom_point() +
  geom_pointrange(aes(ymin = ll, ymax = ul)) +
  geom_linerange(aes(ymin = ll, ymax = ul)) +
  geom_hline(yintercept = coef(full_model)["l_mass_coalitionMass-based"],
             color = "darkred") +
  scale_y_continuous(limits = c(1.5, 3.5)) +
  scale_x_continuous(limits = c(1, 85),
                     breaks = c(seq(1, 85, 1))) +
  coord_flip() +
  labs(x = "\nExcluded COW code", y = "Simulated Coefficients of Mass Coalition")

library(ghibli)

additive_fullset |> dplyr::filter(grepl("Model 4", Model)) |> 
  dplyr::filter(id %in% c("93", "816"))

jk_df |> 
  dplyr::filter(term %in% "Mass Coalition X Mass Party Org.") |> 
  ggplot(aes(median, y = ..scaled..)) + 
  geom_density(color = "#BBA78CFF", fill = "#BBA78CFF", alpha = 0.4) +
  geom_vline(xintercept = coef(full_model)["l_mass_coalitionMass-based:l_massparty"],
             color = "darkred") +
  scale_x_continuous(limits = c(-0.16, -0.15),
                     breaks = c(seq(-0.16, -0.15, 0.01))) +
  facet_wrap(~term, ncol = 3) + labs(y = NULL)

### Lexical Index (LIED) ----------------------------------------------------------------------------------
#### Benchmark ---------
bench1_rand_lexi <- plm::plm(universality ~ l_mass_coalition,
                            index = c("COWcode", "year"),
                            model = "random", random.method = "walhus",
                            data = baseline |> 
                              dplyr::filter(lexi_regime %in% 
                                              c("Autocracies (LIED < 6)")))

bench2_rand_lexi <- plm::plm(universality ~ l_massparty,
                            index = c("COWcode", "year"),
                            model = "random", random.method = "walhus",
                            data = baseline |> 
                              dplyr::filter(lexi_regime %in% 
                                              c("Autocracies (LIED < 6)")))

bench3_rand_lexi <- plm(universality ~ l_mass_coalition + l_massparty,
                       index = c("COWcode", "year"),
                       model = "random", random.method = "walhus",
                       data = baseline |> 
                         dplyr::filter(lexi_regime %in% 
                                         c("Autocracies (LIED < 6)")))

bench4_rand_lexi <- plm(universality ~ l_mass_coalition*l_massparty,
                       index = c("COWcode", "year"),
                       model = "random", random.method = "walhus",
                       data = baseline |> 
                         dplyr::filter(lexi_regime %in% 
                                         c("Autocracies (LIED < 6)")))

bench5_cfixed_lexi <- plm(universality ~ l_mass_coalition,
                         index = c("COWcode", "year"),
                         effect = "individual",
                         data = baseline |> 
                           dplyr::filter(lexi_regime %in% 
                                           c("Autocracies (LIED < 6)")))

bench6_cfixed_lexi <- plm(universality ~ l_massparty +
                           as.factor(COWcode),
                         index = c("COWcode", "year"),
                         effect = "individual",
                         data = baseline |> 
                           dplyr::filter(lexi_regime %in% 
                                           c("Autocracies (LIED < 6)")))

bench7_cfixed_lexi <- plm(universality ~ l_mass_coalition + l_massparty +
                           as.factor(COWcode),
                         index = c("COWcode", "year"),
                         effect = "individual",
                         data = baseline |> 
                           dplyr::filter(lexi_regime %in% 
                                           c("Autocracies (LIED < 6)")))

bench8_cfixed_lexi <- plm(universality ~ l_mass_coalition*l_massparty +
                           as.factor(COWcode),
                         index = c("COWcode", "year"),
                         effect = "individual",
                         data = baseline |> 
                           dplyr::filter(lexi_regime %in% 
                                           c("Autocracies (LIED < 6)")))

bench9_tfixed_lexi <- plm(universality ~ l_mass_coalition +
                           as.factor(COWcode) + as.factor(year),
                         index = c("COWcode", "year"),
                         effect = "twoways",
                         data = baseline |> 
                           dplyr::filter(lexi_regime %in% 
                                           c("Autocracies (LIED < 6)")))

bench10_tfixed_lexi <- plm(universality ~ l_massparty +
                            as.factor(COWcode) + as.factor(year),
                          index = c("COWcode", "year"),
                          effect = "twoways",
                          data = baseline |> 
                            dplyr::filter(lexi_regime %in% 
                                            c("Autocracies (LIED < 6)")))

bench11_tfixed_lexi <- plm(universality ~ l_mass_coalition + l_massparty +
                            as.factor(COWcode) + as.factor(year),
                          index = c("COWcode", "year"),
                          effect = "twoways",
                          data = baseline |> 
                            dplyr::filter(lexi_regime %in% 
                                            c("Autocracies (LIED < 6)")))

bench12_tfixed_lexi <- plm(universality ~ l_mass_coalition*l_massparty +
                            as.factor(COWcode) + as.factor(year),
                          index = c("COWcode", "year"),
                          effect = "twoways",
                          data = baseline |> 
                            dplyr::filter(lexi_regime %in% 
                                            c("Autocracies (LIED < 6)")))

panels <- list(
  "Random Effects" = list(
    bench1_rand_lexi, bench2_rand_lexi, bench3_rand_lexi, bench4_rand_lexi),
  "Country-Fixed" = list(
    bench5_cfixed_lexi, bench6_cfixed_lexi, bench7_cfixed_lexi, bench8_cfixed_lexi),
  "Country-Year-Fixed" = list(
    bench9_tfixed_lexi, bench10_tfixed_lexi, bench11_tfixed_lexi, bench12_tfixed_lexi)
)
rows1 <- tribble(~term,        
                 ~bench1_rand_lexi,  ~bench2_rand_lexi, ~bench3_rand_lexi, ~bench4_rand_lexi,
                 ~bench5_cfixed_lexi, ~bench6_cfixed_lexi, ~bench7_cfixed_lexi, ~bench8_cfixed_lexi,
                 ~bench9_tfixed_lexi, ~bench10_tfixed_lexi, ~bench11_tfixed_lexi, ~bench12_tfixed_lexi,
                 'Controls', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO')
attr(rows1, 'position') <- c(7)

save(list = c("bench1_rand_lexi",   "bench2_rand_lexi",    "bench3_rand_lexi",    "bench4_rand_lexi",
              "bench5_cfixed_lexi", "bench6_cfixed_lexi",  "bench7_cfixed_lexi",  "bench8_cfixed_lexi",
              "bench9_tfixed_lexi", "bench10_tfixed_lexi", "bench11_tfixed_lexi", "bench12_tfixed_lexi"), 
     file = "Analysis_data/lexi_robust.RData")

modelsummary::modelsummary(
  panels, stars = TRUE, shape = "rbind",
  coef_map = c("l_mass_coalitionMass-based" = "Mass-Based Coalition (vs. Elite-Based)",
               "l_massparty" = "Mass Party Organization Index",
               "l_mass_coalitionMass-based:l_massparty" = 
                 "Mass-Based Coalition$\\times$ Mass Party Organization Index"),
  #add_rows = rows1,
  coef_omit = "as.factor",
  gof_omit = 'DF|Deviance|R2|adjR2', 
  #output = "html"
  output = 'latex'
)

#### Full models ----------------------------------------------------------

baseline |> 
  dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(COWcode) |>
  #summary()
  unique() |> length()

## additive: 87         1966-2011
## MPO only: 93         1966-2011
## Coalition only: 98   1947-2011

baseline |> 
  dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_random1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_random2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_random3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_random4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_cfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_cfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_cfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_cfixed4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_tfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_tfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_tfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(lexi_regime %in% c("Autocracies (LIED < 6)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> lexi_full_tfixed4

library(tibble)
rows <- tribble(~term,        
                ~lexi_full_random1,  ~lexi_full_random2, ~lexi_full_random3,  ~lexi_full_random4,  
                ~lexi_full_cfixed1, ~lexi_full_cfixed2, ~lexi_full_cfixed3, ~lexi_full_cfixed4, 
                ~lexi_full_tfixed1, ~lexi_full_tfixed2,~lexi_full_tfixed3, ~lexi_full_tfixed4,
                'Random Effects', 
                'YES', 'YES', 'YES', 'YES',
                'NO', 'NO', 'NO', 'NO',
                'NO', 'NO', 'NO', 'NO',
                'Country-Fixed', 
                'No',  'No', 'No',  'No',
                'YES', 'YES', 'YES', 'YES',
                'YES', 'YES', 'YES', 'YES',
                'Year-Fixed', 
                'No', 'No', 'No', 'No', 
                'No', 'No', 'No', 'No',
                'YES', 'YES', 'YES', 'YES')
attr(rows, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(lexi_full_random1, lexi_full_random2, lexi_full_random3, lexi_full_random4,  
                                lexi_full_cfixed1, lexi_full_cfixed2, lexi_full_cfixed3, lexi_full_cfixed4,  
                                lexi_full_tfixed1, lexi_full_tfixed2, lexi_full_tfixed3, lexi_full_tfixed4), 
                           stars = TRUE,
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')


### Geddes et al. (GWF) ---------
#### Benchmark ------

bench1_rand_gwf <- plm::plm(universality ~ l_mass_coalition,
                             index = c("COWcode", "year"),
                             model = "random", random.method = "walhus",
                             data = baseline |> 
                               dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench2_rand_gwf <- plm::plm(universality ~ l_massparty,
                             index = c("COWcode", "year"),
                             model = "random", random.method = "walhus",
                             data = baseline |> 
                              dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench3_rand_gwf <- plm(universality ~ l_mass_coalition + l_massparty,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                         dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench4_rand_gwf <- plm(universality ~ l_mass_coalition*l_massparty,
                        index = c("COWcode", "year"),
                        model = "random", random.method = "walhus",
                        data = baseline |> 
                         dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench5_cfixed_gwf <- plm(universality ~ l_mass_coalition,
                          index = c("COWcode", "year"),
                          effect = "individual",
                          data = baseline |> 
                           dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench6_cfixed_gwf <- plm(universality ~ l_massparty +
                            as.factor(COWcode),
                          index = c("COWcode", "year"),
                          effect = "individual",
                          data = baseline |> 
                           dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench7_cfixed_gwf <- plm(universality ~ l_mass_coalition + l_massparty +
                            as.factor(COWcode),
                          index = c("COWcode", "year"),
                          effect = "individual",
                          data = baseline |> 
                           dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench8_cfixed_gwf <- plm(universality ~ l_mass_coalition*l_massparty +
                            as.factor(COWcode),
                          index = c("COWcode", "year"),
                          effect = "individual",
                          data = baseline |> 
                           dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench9_tfixed_gwf <- plm(universality ~ l_mass_coalition +
                            as.factor(COWcode) + as.factor(year),
                          index = c("COWcode", "year"),
                          effect = "twoways",
                          data = baseline |> 
                           dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench10_tfixed_gwf <- plm(universality ~ l_massparty +
                             as.factor(COWcode) + as.factor(year),
                           index = c("COWcode", "year"),
                           effect = "twoways",
                           data = baseline |> 
                            dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench11_tfixed_gwf <- plm(universality ~ l_mass_coalition + l_massparty +
                             as.factor(COWcode) + as.factor(year),
                           index = c("COWcode", "year"),
                           effect = "twoways",
                           data = baseline |> 
                            dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

bench12_tfixed_gwf <- plm(universality ~ l_mass_coalition*l_massparty +
                             as.factor(COWcode) + as.factor(year),
                           index = c("COWcode", "year"),
                           effect = "twoways",
                           data = baseline |> 
                            dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")))

panels <- list(
  "Random Effects" = list(
    bench1_rand_gwf, bench2_rand_gwf, bench3_rand_gwf, bench4_rand_gwf),
  "Country-Fixed" = list(
    bench5_cfixed_gwf, bench6_cfixed_gwf, bench7_cfixed_gwf, bench8_cfixed_gwf),
  "Country-Year-Fixed" = list(
    bench9_tfixed_gwf, bench10_tfixed_gwf, bench11_tfixed_gwf, bench12_tfixed_gwf)
)
rows1 <- tribble(~term,        
                 ~bench1_rand_gwf,  ~bench2_rand_gwf, ~bench3_rand_gwf, ~bench4_rand_gwf,
                 ~bench5_cfixed_gwf, ~bench6_cfixed_gwf, ~bench7_cfixed_gwf, ~bench8_cfixed_gwf,
                 ~bench9_tfixed_gwf, ~bench10_tfixed_gwf, ~bench11_tfixed_gwf, ~bench12_tfixed_gwf,
                 'Controls', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO')
attr(rows1, 'position') <- c(7)

save(list = c("bench1_rand_gwf",   "bench2_rand_gwf",    "bench3_rand_gwf",    "bench4_rand_gwf",
              "bench5_cfixed_gwf", "bench6_cfixed_gwf",  "bench7_cfixed_gwf",  "bench8_cfixed_gwf",
              "bench9_tfixed_gwf", "bench10_tfixed_gwf", "bench11_tfixed_gwf", "bench12_tfixed_gwf"), 
     file = "Analysis_data/gwf_robust.RData")

modelsummary::modelsummary(
  panels, stars = TRUE, shape = "rbind",
  coef_map = c("l_mass_coalitionMass-based" = "Mass-Based Coalition (vs. Elite-Based)",
               "l_massparty" = "Mass Party Organization Index",
               "l_mass_coalitionMass-based:l_massparty" = 
                 "Mass-Based Coalition\\times Mass Party Organization Index"),
  #add_rows = rows1,
  coef_omit = "as.factor",
  gof_omit = 'DF|Deviance|R2|adjR2', 
  #output = "html"
  output = 'latex'
)

#### Full models ----------------------


baseline |> 
  dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                #l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(COWcode) |>
  #summary()
  unique() |> length()

## additive: 89         1966-2010
## MPO only: 96         1966-2010
## Coalition only: 99   1947-2010

baseline |> 
  dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                #l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_random1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_random2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_random3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_random4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_cfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_cfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_cfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_cfixed4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_tfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_tfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_tfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(gwf_regime %in% c("Autocracy (GWF)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> gwf_full_tfixed4

library(tibble)
rows <- tribble(~term,        
                ~gwf_full_random1,  ~gwf_full_random2, ~gwf_full_random3,  ~gwf_full_random4,  
                ~gwf_full_cfixed1, ~gwf_full_cfixed2, ~gwf_full_cfixed3, ~gwf_full_cfixed4, 
                ~gwf_full_tfixed1, ~gwf_full_tfixed2,~gwf_full_tfixed3, ~gwf_full_tfixed4,
                'Random Effects', 
                'YES', 'YES', 'YES', 'YES',
                'NO', 'NO', 'NO', 'NO',
                'NO', 'NO', 'NO', 'NO',
                'Country-Fixed', 
                'No',  'No', 'No',  'No',
                'YES', 'YES', 'YES', 'YES',
                'YES', 'YES', 'YES', 'YES',
                'Year-Fixed', 
                'No', 'No', 'No', 'No', 
                'No', 'No', 'No', 'No',
                'YES', 'YES', 'YES', 'YES')
attr(rows, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(gwf_full_random1, gwf_full_random2, gwf_full_random3, gwf_full_random4,  
                                gwf_full_cfixed1, gwf_full_cfixed2, gwf_full_cfixed3, gwf_full_cfixed4,  
                                gwf_full_tfixed1, gwf_full_tfixed2, gwf_full_tfixed3, gwf_full_tfixed4), 
                           stars = TRUE,
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')

## Controls ----------------------------------
### Without Repression --------------------------------

fixed1_wo_rep1 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

fixed1_wo_rep2 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

### Without Civil War --------------------------------

fixed1_wo_civil1 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

fixed1_wo_civil2 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

### Without CSOs --------------------------------


cso_model_fixed1 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

cso_model_fixed2 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

cso_model_fixed3 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + 
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

cso_model_fixed4 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + 
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)


cso_model_fixed5 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + 
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

cso_model_fixed6 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + 
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)


### Without Nothing --------------------------------


full_model_fixed1 <- plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

full_model_fixed2 <- plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

panels_ctrls <- list(
  "Baseline: Full Model" = list(
    full_model_fixed1, full_model_fixed2),
  "Without Repression" = list(
    fixed1_wo_rep1, fixed1_wo_rep2),
  "Without Civil War Experience" = list(
    fixed1_wo_civil1, fixed1_wo_civil2),
  "Without CSO Consults" = list(
    cso_model_fixed1, cso_model_fixed2),
  "Without CSO Participation" = list(
    cso_model_fixed3, cso_model_fixed4),
  "Without CSOs Both" = list(
    cso_model_fixed5, cso_model_fixed6)
)

rows <- 
  tribble(~term,  ~`Model 1`,  ~`Model 2`, ~`Model 3`, ~`Model 4`, 
          ~`Model 5`, ~`Model 6`, ~`Model 7`, ~`Model 8`, ~`Model 9`, ~`Model 10`,
          ~`Model 11`, ~`Model 12`,
          'Omitted Controls', 
          'W/o Repression',   'W/o Civil War', 
          'W/o CSO consults',   'W/o CSO participation', 'W/o CSO Both', '-', 
          'W/o Repression',   'W/o Civil War', 
          'W/o CSO consults',   'W/o CSO participation', 'W/o CSO Both', '-', 
          "Country-fixed",  "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES",
          "Year-fixed",     "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES")
attr(rows, 'position') <- c(7, 8, 9)

rows1 <- tribble(~term,        
                 ~bench1_rand_gwf,  ~bench2_rand_gwf, ~bench3_rand_gwf, ~bench4_rand_gwf,
                 ~bench5_cfixed_gwf, ~bench6_cfixed_gwf, ~bench7_cfixed_gwf, ~bench8_cfixed_gwf,
                 ~bench9_tfixed_gwf, ~bench10_tfixed_gwf, ~bench11_tfixed_gwf, ~bench12_tfixed_gwf,
                 'Controls', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO', 'No', 'No', 'NO', 'NO')
attr(rows1, 'position') <- c(7)

modelsummary::modelsummary(
  panels_ctrls, stars = TRUE, shape = "rbind",
  fmt = 3,
  "{estimate}{stars} ({std.error})",
  coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
               "l_massparty" = "Mass Party Organization Index",
               "l_mass_coalition:l_massparty" = 
                 "Mass-Based Coalition\\times Mass Party Organization Index"),
  #add_rows = rows1,
  coef_omit = "as.factor",
  gof_omit = 'DF|Deviance|R2|adjR2', 
  #output = "html"
  output = 'latex'
)
robust_alt |> 
  dplyr::filter(year > 1959 & year < 2023) |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  group_by(mass_inclusive_T050, year) |> count() |> drop_na() |> 
  group_by(year) |> mutate(total = sum(n),
                           ratio = n/total) |> arrange(mass_inclusive_T050, desc(ratio)) |> 
  print(n = Inf)


baseline |> 
  dplyr::filter(gwf_regime %in% "Autocracy (GWF)") |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()
unique() |> length()

## Coalition only 
## MPO only
## Additive 1966-2010


texreg::texreg(
  list(bench_model1, bench_model2, bench_model3, bench_model4, 
       bench_model5, bench_model6, bench_model7, bench_model8, 
       bench_model9, bench_model10, bench_model11, bench_model12),
  omit.coef = "as.factor",
  custom.coef.names = c(
    "(Intercept)",
    "Mass-Based Coalition (vs. Elite-Based)",
    "Mass Party Organization Index",
    "Mass-Based Coalition$\\times$Mass Party Organization Index"),
  reorder.coef = c(2, 3, 4, 1),
  custom.gof.rows = 
    list(
      "Controls" = c("No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No"),
      "Country-fixed" = c("No", "No", "No", "No", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
      "Year-fixed"    = c("No", "No", "No", "No", "No", "No", "No", "No", "YES", "YES", "YES", "YES"),
      "No. of countries" = c(
        "146", "119", "110", "110", "146", "119", "110", "110", "146", "119", "110", "110"),
      "Year coverage" = c("1900-2023", "1966-2020", "1966-2020", "1966-2020",
                          "1900-2023", "1966-2020", "1966-2020", "1966-2020",
                          "1900-2023", "1966-2020", "1966-2020", "1966-2020")))

fixed1 <- plm::plm(universality ~ l_mass_coalition*l_massparty +
                     as.factor(COWcode) + as.factor(year),
                   data = baseline |> 
                     dplyr::filter(vdem_regime %in% c(0, 1)), index = c("COWcode", "year"),
                   effect = "twoways")
random1 <- plm::plm(universality ~ l_mass_coalition*l_massparty +
                      as.factor(COWcode) + as.factor(year),
                    data = baseline |> 
                      dplyr::filter(vdem_regime %in% c(0, 1)), index = c("COWcode", "year"),
                    model = "random", random.method = "walhus")

texreg::screenreg(list(fixed1, bench_model12, random1, bench_model4), omit.coef = "as.factor")
### Base: Without Ctrls but fixed effects --------------------------------------
#### Base: Country-fixed -------------------------------------------------------------

### Full set for main results ----------------------------------------------------------------------

baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(COWcode) |> 
  #summary()
  unique() |> length()

baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  dplyr::select(universality, COWcode, year, 
                l_mass_coalition,
                l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(year) |> 
  summary()

universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
  l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
  #l_royal + l_party + l_mil +
  l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
  as.factor(COWcode) + as.factor(year)

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year) -1, 
  index = c("COWcode", "year"),
  model = "random", random.method = "walhus",
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_random4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "individual", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_cfixed4

plm::plm(
  universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed1

plm::plm(
  universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed2

plm::plm(
  universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed3

plm::plm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_tfixed4

#### Full models ------

panels <- list(
  "Random Effects" = list(
    "Model 1" = full_random1, "Model 2" = full_random2),
  "Country-Fixed Effects" = list(
    "Model 3" = full_cfixed1, "Model 4" = full_cfixed2), 
  "Country-Year-Fixed Effeects" = list(
    "Model 5" = full_tfixed1, "Model 6" = full_tfixed2)
)

library(tibble)
rows <- tribble(~term,        
                ~full_random1,  ~full_random2, ~full_random3,  ~full_random4,  
                ~full_cfixed1, ~full_cfixed2, ~full_cfixed3, ~full_cfixed4, 
                ~full_tfixed1, ~full_tfixed2,~full_tfixed3, ~full_tfixed4,
                'Random Effects', 
                'YES', 'YES', 'YES', 'YES',
                'NO', 'NO', 'NO', 'NO',
                'NO', 'NO', 'NO', 'NO',
                'Country-Fixed', 
                'No',  'No', 'No',  'No',
                'YES', 'YES', 'YES', 'YES',
                'YES', 'YES', 'YES', 'YES',
                'Year-Fixed', 
                'No', 'No', 'No', 'No', 
                'No', 'No', 'No', 'No',
                'YES', 'YES', 'YES', 'YES')
attr(rows, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(full_random1, full_random2, full_random3, full_random4,  
                                full_cfixed1, full_cfixed2, full_cfixed3, full_cfixed4,  
                                full_tfixed1, full_tfixed2, full_tfixed3, full_tfixed4), 
                           stars = TRUE,
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')

rows_main <- tribble(~term,   ~full_tfixed1, ~full_tfixed2, ~full_tfixed3, ~full_tfixed4, 
                     'Country-Fixed', 'YES', 'YES', 'YES', 'YES',
                     'Year-Fixed',    'YES', 'YES', 'YES', 'YES',)
attr(rows_main, 'position') <- c(15, 16)

library(modelsummary)
options(modelsummary_format_numeric_latex = "plain")
modelsummary::modelsummary(list(full_tfixed1, full_tfixed2, full_tfixed3, full_tfixed4),
                           stars = TRUE,
                           #estimate  = c("{estimate}{stars} ({std.error})"),
                           coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')


texreg::screenreg(list(
  
  full_random1, full_random2, full_cfixed1, full_cfixed2, full_tfixed1, full_tfixed2),
  omit.coef = "as.factor",
  single.row = T,
  custom.coef.names = c(
    "(Intercept)",
    "Mass-Based Coalition (vs. Elite-Based)",
    "Mass Party Organization Index",
    "Ln(GDPpc + 1)",
    "Annual GDP Growth",
    "Resource Dependence (Per GDPpc)",
    "Civil War Experience",
    "Repression: Human Rights",
    #"Repression: State Capacity",
    "CSO Consulation",
    "CSO Participatory Environment",
    "Hereditary Dimension",
    "Military Dimension",
    "Ruling Party Dimension",
    "Personalist Dimension",
    "Mass-Based Coalition\\times Mass Party Organization Index"),
  reorder.coef = c(2, 3, 15, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1),
  custom.gof.rows = 
    list("Country-fixed" = c("YES", "YES"),
         "Year-fixed" = c("YES", "YES"),
         "No. of countries" = c("88", "88"),
         "Year coverage" = c("1966-2011", "1966-2011")))






library(interflex)
summary(baseline$massparty)
quantile(sample$l_massparty, probs = c(1/2), na.rm = T)
glm(universality ~ l_mass_coalition*massparty_bin + 
  log(l_gdppc+1) + l_gdpgrth + 
  l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
  #l_royal + l_party + l_mil +
  l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
  as.factor(COWcode) + as.factor(year), 
data = baseline |> 
  #dplyr::filter(brm %in% "Autocracies (BRM)")
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  mutate(l_mass_coalition = as.numeric(l_mass_coalition))) -> final

summary(baseline$massparty)
quantile(sample$l_massparty, probs = c(1/2), na.rm = T)

baseline |> 
  #dplyr::filter(brm %in% "Autocracies (BRM)")
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  ungroup() |> 
  mutate(massparty_bin = case_when(
    is.na(l_massparty) ~ NA_integer_,
    l_massparty > as.numeric(quantile(sample$l_massparty, probs = c(1/2), na.rm = T)) ~ 1L,
    T ~ 0L)) |> 
  group_by(massparty_bin) |> drop_na(massparty_bin) |> 
  mutate(median = median(l_massparty, na.rm = T)) -> baseline

glm(universality ~ (l_mass_coalition + (l_massparty - median) + l_mass_coalition*(l_massparty - median))*massparty_bin +
      log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
      #l_royal + l_party + l_mil +
      l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> 
      #dplyr::filter(brm %in% "Autocracies (BRM)")
      dplyr::filter(vdem_regime %in% c(0, 1)) |> 
      mutate(l_mass_coalition = as.numeric(l_mass_coalition))) -> final


texreg::screenreg(final, omit.coef = "as.factor")

final |> ggeffects::ggpredict(terms = c("massparty_bin", "l_mass_coalition")) |> 
  plot()

baseline |> mutate(
  l_mass_coalition = as.numeric(l_mass_coalition)
) -> baseline

baseline |> mutate(
  l_mass_coalition = if_else(l_mass_coalition == 1L, 0L,
                             if_else(l_mass_coalition == 2L, 1L, NA_integer_)
)) -> baseline

summary(final)

names(baseline)

### Lags -------------------------------------------------------------------------------------------

# Define the variables you want to lag
variables_to_lag <- c("mass_coalition", "alt_mass_part_coalition", "massparty",
                      "gdppc", "gdpgrth", "lnresource", "civilwar", "repression",
                      "Capacity", "CSOconsult", "CSOpart", "hereditary_index",
                      "military_index", "party_index", "personindex")

# Number of lags
max_lag <- 5

# Adding lagged variables
lagged_data <- baseline |> 
  group_by(COWcode) |> 
  mutate(across(.cols = all_of(variables_to_lag),
                .fns = list(l2 = ~lag(., n = 2, order_by = year),
                            l3 = ~lag(., n = 3, order_by = year),
                            l4 = ~lag(., n = 4, order_by = year),
                            l5 = ~lag(., n = 5, order_by = year),
                            l6 = ~lag(., n = 6, order_by = year),
                            l7 = ~lag(., n = 7, order_by = year),
                            l8 = ~lag(., n = 8, order_by = year),
                            l9 = ~lag(., n = 9, order_by = year),
                            l10 = ~lag(., n = 10, order_by = year)),
                .names = "{.col}_{.fn}")) |> 
  ungroup()

lag2model <- plm::plm(
  universality ~ mass_coalition_l2*massparty_l2 + log(gdppc_l2+1) + gdpgrth_l2 + 
    lnresource_l2 + civilwar_l2 + repression_l2 + CSOconsult_l2 + CSOpart_l2 +
    hereditary_index_l2 + military_index_l2 + party_index_l2 + personindex_l2 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l2 = as.numeric(mass_coalition_l2))
)

lag3model <- plm::plm(
  universality ~ mass_coalition_l3*massparty_l3 + log(gdppc_l3+1) + gdpgrth_l3 + 
    lnresource_l3 + civilwar_l3 + repression_l3 + CSOconsult_l3 + CSOpart_l3 +
    hereditary_index_l3 + military_index_l3 + party_index_l3 + personindex_l3 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l3 = as.numeric(mass_coalition_l3))
)

lag4model <- plm::plm(
  universality ~ mass_coalition_l4*massparty_l4 + log(gdppc_l4+1) + gdpgrth_l4 + 
    lnresource_l4 + civilwar_l4 + repression_l4 + CSOconsult_l4 + CSOpart_l4 +
    hereditary_index_l4 + military_index_l4 + party_index_l4 + personindex_l4 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l4 = as.numeric(mass_coalition_l4))
)

lag5model <- plm::plm(
  universality ~ mass_coalition_l5*massparty_l5 + log(gdppc_l5+1) + gdpgrth_l5 + 
    lnresource_l5 + civilwar_l5 + repression_l5 + CSOconsult_l5 + CSOpart_l5 +
    hereditary_index_l5 + military_index_l5 + party_index_l5 + personindex_l5 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l5 = as.numeric(mass_coalition_l5))
)

lag6model <- plm::plm(
  universality ~ mass_coalition_l6*massparty_l6 + log(gdppc_l6+1) + gdpgrth_l6 + 
    lnresource_l6 + civilwar_l6 + repression_l6 + CSOconsult_l6 + CSOpart_l6 +
    hereditary_index_l6 + military_index_l6 + party_index_l6 + personindex_l6 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l6 = as.numeric(mass_coalition_l6))
)

lag7model <- plm::plm(
  universality ~ mass_coalition_l7*massparty_l7 + log(gdppc_l7+1) + gdpgrth_l7 + 
    lnresource_l7 + civilwar_l7 + repression_l7 + CSOconsult_l7 + CSOpart_l7 +
    hereditary_index_l7 + military_index_l7 + party_index_l7 + personindex_l7 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l7 = as.numeric(mass_coalition_l7))
)

lag8model <- plm::plm(
  universality ~ mass_coalition_l8*massparty_l8 + log(gdppc_l8+1) + gdpgrth_l8 + 
    lnresource_l8 + civilwar_l8 + repression_l8 + CSOconsult_l8 + CSOpart_l8 +
    hereditary_index_l8 + military_index_l8 + party_index_l8 + personindex_l8 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l8 = as.numeric(mass_coalition_l8))
)

lag9model <- plm::plm(
  universality ~ mass_coalition_l9*massparty_l9 + log(gdppc_l9+1) + gdpgrth_l9 + 
    lnresource_l9 + civilwar_l9 + repression_l9 + CSOconsult_l9 + CSOpart_l9 +
    hereditary_index_l9 + military_index_l9 + party_index_l9 + personindex_l9 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l9 = as.numeric(mass_coalition_l9))
)

lag10model <- plm::plm(
  universality ~ mass_coalition_l10*massparty_l10 + log(gdppc_l10+1) + gdpgrth_l10 + 
    lnresource_l10 + civilwar_l10 + repression_l10 + CSOconsult_l10 + CSOpart_l10 +
    hereditary_index_l10 + military_index_l10 + party_index_l10 + personindex_l10 +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = lagged_data |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(mass_coalition_l10 = as.numeric(mass_coalition_l10))
)

texreg::screenreg(list(lag2model, lag3model, lag4model, lag5model, 
                       lag6model, lag7model, lag8model, lag9model, lag10model),
                  omit.coef = "as.factor")

bind_rows(
  brm_full_tfixed4 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(brm_full_tfixed4)) |> 
    dplyr::filter(grepl(c("mass_coalition|massparty"), term)) |> 
    mutate(term = c("Mass-based coalition", "Mass Party Organization", 
                    "Mass-based coalition X Mass Party Organization"),
           lag = 1),
  lag2model |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(lag2model)) |> 
    dplyr::filter(grepl(c("mass_coalition|massparty"), term)) |> 
    mutate(term = c("Mass-based coalition", "Mass Party Organization", 
                    "Mass-based coalition X Mass Party Organization"),
           lag = 2),
  lag3model |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(lag3model)) |> 
    dplyr::filter(grepl(c("mass_coalition|massparty"), term)) |> 
    mutate(term = c("Mass-based coalition", "Mass Party Organization", 
                    "Mass-based coalition X Mass Party Organization"),
           lag = 3),
  lag4model |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(lag4model)) |> 
    dplyr::filter(grepl(c("mass_coalition|massparty"), term)) |> 
    mutate(term = c("Mass-based coalition", "Mass Party Organization", 
                    "Mass-based coalition X Mass Party Organization"),
           lag = 4),
  lag5model |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(lag5model)) |> 
    dplyr::filter(grepl(c("mass_coalition|massparty"), term)) |> 
    mutate(term = c("Mass-based coalition", "Mass Party Organization", 
                    "Mass-based coalition X Mass Party Organization"),
           lag = 5)
) -> lagged_agg

delete_no_display <- function(v) {
  if_else(str_detect(v, 'no_display'), '', v)
}


lagged_agg |> 
  mutate(
    term = factor(term, levels = c("Mass-based coalition", 
                                   "Mass Party Organization", 
                                   "Mass-based coalition X Mass Party Organization")),
    lag = factor(lag, levels = c(1, 2, 3, 4, 5),
                 labels = c("T1", "T2", "T3", "T4", "T5")),
    lag = if_else(!term %in% c("Mass-based coalition", "Mass Party Organization"),
                  lag,
                  paste0(as.character(lag), 'no_display'))) |> 
  ggplot(aes(x = lag, y = estimate, color = term)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = F) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), show.legend = F, size = 1.3) +
  geom_point(size = 5, shape = 21, fill = "white", show.legend = F) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_x_discrete(expand = c(0.1,0.1), label = delete_no_display) +
  labs(x = "\n", y = "Estimates\n",
       caption = str_wrap("Note: Dots show the coefficient estimates on universal welfare provision, and vertical lines display the 95% confidence intervals.",
                          exdent = 10, 80)) +
  facet_wrap(~term, ncol = 1, scales = "free") +
  theme(
    panel.margin = unit(0.8, "lines"),
    strip.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1.4)),
    plot.caption = element_text(size = rel(1.4)))


ggsave("Documents/2_Manuscript/2_Figures/Fig_lag.pdf",
       width = 8, height = 7, dpi = 1600)


### Different Thresholds for Alternative Predictor ---------------------------------------

# List of thresholds
thresholds <- seq(0.5, 0.75, by = 0.05)

# Function to apply threshold and create binary variable
binarize <- function(data, threshold) {
  baseline %>%
    mutate(across(c(l_elite_aristocracy, l_elite_agrarian, l_elite_party, l_elite_business, 
                    l_state_bureau, l_elite_military, l_ethnicracial, 
                    l_religious, l_elite_local, l_mass_urbanworking, l_mass_urbanmiddle, 
                    l_mass_ruralworking, l_mass_ruralmiddle, l_foreigngovt),
                  ~ as.integer(. > threshold),
                  .names = "T{round(threshold * 100)}_{.col}"))
}

# Apply the function across each threshold and store the results
results <- map(thresholds, ~ binarize(df, .x))

# Reduce the results into one dataframe by merging all new columns into the original dataframe
df_with_binaries <- reduce(results, full_join, by = row.names(df))

# Drop the row names column if it was added during the join (optional)

names(df_with_binaries)
df_with_binaries |> 
  mutate(
    mass_inclusive_T050 = case_when(
      T50_l_mass_urbanworking == 1L | T50_l_mass_urbanmiddle == 1L |
        T50_l_mass_ruralworking == 1L | T50_l_mass_ruralmiddle == 1L ~ 1L,
      T50_l_mass_urbanworking == 0L & T50_l_mass_urbanmiddle == 0L &
        T50_l_mass_ruralworking == 0L & T50_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
    mass_inclusive_T055 = case_when(
      T55_l_mass_urbanworking == 1L | T55_l_mass_urbanmiddle == 1L |
        T55_l_mass_ruralworking == 1L | T55_l_mass_ruralmiddle == 1L ~ 1L,
      T55_l_mass_urbanworking == 0L & T55_l_mass_urbanmiddle == 0L &
        T55_l_mass_ruralworking == 0L & T55_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
    mass_inclusive_T060 = case_when(
      T60_l_mass_urbanworking == 1L | T60_l_mass_urbanmiddle == 1L |
        T60_l_mass_ruralworking == 1L | T60_l_mass_ruralmiddle == 1L ~ 1L,
      T60_l_mass_urbanworking == 0L & T60_l_mass_urbanmiddle == 0L &
        T60_l_mass_ruralworking == 0L & T60_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
    mass_inclusive_T065 = case_when(
      T65_l_mass_urbanworking == 1L | T65_l_mass_urbanmiddle == 1L |
        T65_l_mass_ruralworking == 1L | T65_l_mass_ruralmiddle == 1L ~ 1L,
      T65_l_mass_urbanworking == 0L & T65_l_mass_urbanmiddle == 0L &
        T65_l_mass_ruralworking == 0L & T65_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
    mass_inclusive_T070 = case_when(
      T70_l_mass_urbanworking == 1L | T70_l_mass_urbanmiddle == 1L |
        T70_l_mass_ruralworking == 1L | T70_l_mass_ruralmiddle == 1L ~ 1L,
      T70_l_mass_urbanworking == 0L & T70_l_mass_urbanmiddle == 0L &
        T70_l_mass_ruralworking == 0L & T70_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
    mass_inclusive_T075 = case_when(
      T75_l_mass_urbanworking == 1L | T75_l_mass_urbanmiddle == 1L |
        T75_l_mass_ruralworking == 1L | T75_l_mass_ruralmiddle == 1L ~ 1L,
      T75_l_mass_urbanworking == 0L & T75_l_mass_urbanmiddle == 0L &
        T75_l_mass_ruralworking == 0L & T75_l_mass_ruralmiddle == 0L ~ 0L,
      T ~ NA_integer_),
  ) -> robust_alt

table(robust_alt$mass_inclusive_T050)
table(robust_alt$mass_inclusive_T055)
table(robust_alt$mass_inclusive_T060)
table(robust_alt$mass_inclusive_T065)
table(robust_alt$mass_inclusive_T070)
table(robust_alt$mass_inclusive_T075)
table(baseline$l_mass_coalition)


#### Threshold of 0.50 -------
plm::plm(
  universality ~ mass_inclusive_T050 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T50_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T050 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T50_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T050*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T50_alt_tfixed3

#### Threshold of 0.55 -------
plm::plm(
  universality ~ mass_inclusive_T055 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T55_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T055 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T55_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T055*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T55_alt_tfixed3

#### Threshold of 0.60 -------
plm::plm(
  universality ~ mass_inclusive_T060 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T60_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T060 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T60_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T060*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T60_alt_tfixed3

#### Threshold of 0.65 -------
plm::plm(
  universality ~ mass_inclusive_T065 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T65_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T065 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T65_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T065*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T65_alt_tfixed3

#### Threshold of 0.70 -------
plm::plm(
  universality ~ mass_inclusive_T070 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T70_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T070 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T70_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T070*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T70_alt_tfixed3

#### Threshold of 0.75 -------
plm::plm(
  universality ~ mass_inclusive_T075 + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> T75_alt_tfixed1

plm::plm(
  universality ~ mass_inclusive_T075 + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T75_alt_tfixed2

plm::plm(
  universality ~ mass_inclusive_T075*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  index = c("COWcode", "year"),
  effect = "twoways", 
  data = robust_alt |> 
    dplyr::filter(brm_regime %in% c("Autocracies (BRM)"))
) -> T75_alt_tfixed3

texreg::screenreg(list(T50_alt_tfixed1, T50_alt_tfixed2, T50_alt_tfixed3), omit.coef = "as.factor")
texreg::screenreg(list(T55_alt_tfixed1, T55_alt_tfixed2, T55_alt_tfixed3), omit.coef = "as.factor")
texreg::screenreg(list(T60_alt_tfixed1, T60_alt_tfixed2, T60_alt_tfixed3), omit.coef = "as.factor")
texreg::screenreg(list(T65_alt_tfixed1, T65_alt_tfixed2, T65_alt_tfixed3), omit.coef = "as.factor")
texreg::screenreg(list(T70_alt_tfixed1, T70_alt_tfixed2, T70_alt_tfixed3), omit.coef = "as.factor")
texreg::screenreg(list(T75_alt_tfixed1, T75_alt_tfixed2, T75_alt_tfixed3), omit.coef = "as.factor")

robust_alt |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1965 & year < 2012) |> 
  dplyr::select(contains("mass_inclusive_T0")) |> 
  gather(coalition, value) |> group_by(coalition, value) |> count() |> drop_na() |> 
  group_by(coalition) |> mutate(total = sum(n)) |> 
  mutate(value = factor(value, levels = c(1, 0),
                        labels = c("Mass-inclusive coalition", "Non-mass coalition"))) |> 
  mutate(ratio = n/total,
         coalition = factor(coalition,
                            levels = c("mass_inclusive_T050",
                                       "mass_inclusive_T055",
                                       "mass_inclusive_T060",
                                       "mass_inclusive_T065",
                                       "mass_inclusive_T070",
                                       "mass_inclusive_T075"),
                            labels = c("x >= 0.5", "x >= 0.55", "x >= 0.60", "x >= 0.65", 
                                       "x >= 0.70", "x >= 0.75"))) |> 
  ggplot(aes(x = coalition, y = n, color = value, fill = value)) +
  geom_col() +
  labs(y = NULL, x = "\nThreshold of Mass-Inclusive Coalition\n",
       caption = str_wrap("\nNote: This figure illustrates the distribution of mass-inclusive coalitions as thresholds vary from 0.5 to 0.75. Each bar represents the percentage of coalitions that include at least one mass-based social group or include no mass-based social group, demonstrating how sensitivity to group inclusion changes with different threshold settings.",
                          exdent = 10, 110)) +
  geom_text(aes(label = paste0(n, "\n", "(", round(ratio, 2)*100, "%)")),
            color = "black", position = position_stack(0.8), size = 5) +
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[3],
                               futurevisions::futurevisions("mars")[1])) +
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[3],
                                futurevisions::futurevisions("mars")[1])) +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        plot.caption = element_text(size = rel(1.3), hjust = 0))

ggsave("Documents/2_Manuscript/2_Figures/Fig_alt_coalition.pdf",
       width = 9, height = 6, dpi = 1600)

baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1965 & year < 2012) |> 
  group_by(mass_coalition) |> count() |> drop_na() |> ungroup() |> 
  mutate(total = sum(n), ratio = n/total) 
  dplyr::filter(mass_coalition %in% "Mass-based") |> 
  pull(COWcode) |> unique()

# 816  93 438 616 371 517 360
# Vietnam, Nicaragua Guinea Tunisia Armenia Rwanda Romania
sort(c("Vietnam", "Nicaragua", "Guinea", "Tunisia", "Armenia", "Rwanda", "Romania"))

robust_alt |> 
  dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
  dplyr::filter(year > 1965 & year < 2012) |> 
  dplyr::select(universality, COWcode, year, 
                mass_inclusive_T075,
                #l_massparty,
                l_gdppc, l_gdpgrth, 
                l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
                l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
  ) |> 
  drop_na() |> 
  pull(COWcode) |> unique() |> length()

texreg::texreg(list(T50_alt_tfixed1, T50_alt_tfixed2, T50_alt_tfixed3),
               omit.coef = "as.factor",
               single.row = F,
               custom.coef.names = c(
                 "Mass-Inclusive Coalition (vs. Non-Mass)",
                 "Ln(GDPpc + 1)",
                 "Annual GDP Growth",
                 "Resource Dependence (Per GDPpc)",
                 "Civil War Experience",
                 "Repression: Human Rights",
                 #"Repression: State Capacity",
                 "CSO Consulation",
                 "CSO Participatory Environment",
                 "Hereditary Dimension",
                 "Military Dimension",
                 "Ruling Party Dimension",
                 "Personalist Dimension",
                 "Mass Party Organization Index",
                 "Mass-Based Coalition\\times Mass Party Organization Index"),
               reorder.coef = c(1, 13, 14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
               custom.gof.rows = 
                 list("Country-fixed" = c("YES", "YES", "YES"),
                      "Year-fixed" = c("YES", "YES", "YES"),
                      "Year coverage" = c("1947-2011", "1966-2011", "1966-2011"),
                      "No. of countries" = c("107", "93", "93"),
                      "AIC" = c(AIC(T50_alt_tfixed1), AIC(T50_alt_tfixed2), 
                                AIC(T50_alt_tfixed3)),
                      "BIC" = c(BIC(T50_alt_tfixed1), BIC(T50_alt_tfixed2), 
                                BIC(T50_alt_tfixed3))))

### Thresholds plot -----------------------------------------------

bind_rows(
  T50_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T50_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.5),
  T55_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T55_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.55),
  T60_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T60_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.60),
  T65_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T65_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.65),
  T70_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T70_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.70),
  T75_alt_tfixed3 |> broom::tidy() |> 
    bind_cols(broom::confint_tidy(T75_alt_tfixed3)) |> 
    dplyr::filter(grepl(c("mass_inclusive|massparty"), term)) |> 
    mutate(term = c("Mass-inclusive coalition", "Mass Party Organization", 
                    "Mass-inclusive coalition X Mass Party Organization"),
           threshold = 0.75)
) -> threshold_agg


threshold_agg |> 
  mutate(
    term = factor(term, levels = c("Mass-inclusive coalition", 
                                   "Mass Party Organization", 
                                   "Mass-inclusive coalition X Mass Party Organization")),
    threshold = factor(threshold, levels = c(0.5, 0.55, 0.60, 0.65, 0.70, 0.75),
                 labels = c("x >= 0.5", "x >= 0.55", "x >= 0.60", "x >= 0.65", "x >= 0.70", "x >= 0.75")),
    threshold = if_else(!term %in% c("Mass-inclusive coalition", "Mass Party Organization"),
                        threshold,
                  paste0(as.character(threshold), 'no_display'))) |> 
  ggplot(aes(x = threshold, y = estimate, color = term)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(estimate, 2)), angle = 90, nudge_x = -0.1, show.legend = F) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = F) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), show.legend = F, size = 1.3) +
  geom_point(size = 5, shape = 21, fill = "white", show.legend = F) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_x_discrete(expand = c(0.1,0.1), label = delete_no_display) +
  labs(x = "\n", y = "Estimates\n",
       caption = str_wrap("Note: Dots show the coefficient estimates on universal welfare provision, and vertical lines display the 95% confidence intervals.",
                          exdent = 10, 80)) +
  facet_wrap(~term, ncol = 1, scales = "free") +
  theme(
    panel.margin = unit(0.8, "lines"),
    strip.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1.4)),
    plot.caption = element_text(size = rel(1.4)))


ggsave("Documents/2_Manuscript/2_Figures/Fig_threshold.pdf",
       width = 8, height = 7, dpi = 1600)


panels <- list(
  "Random Effects" = list(
    "Model 1" = full_random1, "Model 2" = full_random2),
  "Country-Fixed Effects" = list(
    "Model 3" = full_cfixed1, "Model 4" = full_cfixed2), 
  "Country-Year-Fixed Effeects" = list(
    "Model 5" = full_tfixed1, "Model 6" = full_tfixed2)
)

#### tex output for full threshold models
rows_threshold <- tribble(~term,        
                ~T50_alt_tfixed1,  ~T50_alt_tfixed2, ~T50_alt_tfixed3,  
                ~T55_alt_tfixed1,  ~T55_alt_tfixed2, ~T55_alt_tfixed3,  
                ~T60_alt_tfixed1,  ~T60_alt_tfixed2, ~T60_alt_tfixed3,  
                ~T65_alt_tfixed1,  ~T65_alt_tfixed2, ~T65_alt_tfixed3,  
                ~T70_alt_tfixed1,  ~T70_alt_tfixed2, ~T70_alt_tfixed3,  
                ~T75_alt_tfixed1,  ~T75_alt_tfixed2, ~T76_alt_tfixed3,  
                'Threshold', 
                '0.50', '0.50', '0.50', 
                '0.55', '0.55', '0.55',
                '0.60', '0.60', '0.60',
                '0.65', '0.65', '0.65',
                '0.70', '0.70', '0.70',
                '0.75', '0.75', '0.75',
                'Country-Fixed', 
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'Year-Fixed', 
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES',
                'YES', 'YES', 'YES')
attr(rows_threshold, 'position') <- c(29, 30, 31)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
modelsummary::modelsummary(list(T50_alt_tfixed1,  T50_alt_tfixed2, T50_alt_tfixed3,  
                                T55_alt_tfixed1,  T55_alt_tfixed2, T55_alt_tfixed3,  
                                T60_alt_tfixed1,  T60_alt_tfixed2, T60_alt_tfixed3,  
                                T65_alt_tfixed1,  T65_alt_tfixed2, T65_alt_tfixed3,  
                                T70_alt_tfixed1,  T70_alt_tfixed2, T70_alt_tfixed3,  
                                T75_alt_tfixed1,  T75_alt_tfixed2, T75_alt_tfixed3),
                           stars = TRUE,
                           coef_map = c("mass_inclusive_T050" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "mass_inclusive_T055" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "mass_inclusive_T060" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "mass_inclusive_T065" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "mass_inclusive_T070" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "mass_inclusive_T075" = "Mass-Inclusive Coalition (vs. Non-Mass)",
                                        "l_massparty" = "Mass Party Organization Index",
                                        "mass_inclusive_T050:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "mass_inclusive_T055:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "mass_inclusive_T060:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "mass_inclusive_T065:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "mass_inclusive_T070:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "mass_inclusive_T075:l_massparty" = "Mass-Inclusive Coalition\\times Mass Party Organization Index",
                                        "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
                                        "l_gdpgrth" = "Annual GDP Growth",
                                        "l_lnresource" = "Resource Dependence (Per GDPpc)",
                                        "l_civilwar" = "Civil War Experience",
                                        "l_repression" = "Repression: Human Rights",
                                        #"Repression: State Capacity",
                                        "l_CSOconsult" = "CSO Consulation",
                                        "l_CSOpart" = "CSO Participatory Environment",
                                        "l_hereditaryindex" = "Hereditary Dimension",
                                        "l_militaryindex" = "Military Dimension",
                                        "l_partyindex" = "Ruling Party Dimension",
                                        "l_personindex" = "Personalist Dimension"),
                           add_rows = rows_threshold,
                           coef_omit = "as.factor",
                           gof_omit = 'DF|Deviance|R2|adjR2', output = 'latex')

