## Project: Why do authoritarian regimes provide welfare programs? -------------
##          3. Empirical Analyses  --------------------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:

## Import Packages to use ------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")
pacman::p_load(ggplot2, grid, pBrackets, ezpickr, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)

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

## Data Imports: Baseline ------------------------------------------------------
baseline <- readRDS("Analysis_data/baseline.RDS")

## Model Specification ---------------------------------------------------------
#### Benchmark Models: Without Controls ----------------------------------------
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

### Full Models ------------------------------------------------------------------------------------

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
#### Full Models with BRM Table --------------------------------------------------------------------

texreg::texreg(list(brm_full_tfixed1, brm_full_tfixed2, brm_full_tfixed3, brm_full_tfixed4),
               omit.coef = "as.factor",
               single.row = F,
               custom.coef.names = c(
                 "Mass-Based Coalition (vs. Elite-Based)",
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
                 list("Country-fixed" = c("YES", "YES", "YES", "YES"),
                      "Year-fixed" = c("YES", "YES", "YES", "YES"),
                      "Year coverage" = c("1947-2011", "1966-2011", "1966-2011", "1966-2011"),
                      "No. of countries" = c("100", "100", "94", "94"),
                      "AIC" = c(AIC(brm_full_tfixed1), AIC(brm_full_tfixed2), 
                                AIC(brm_full_tfixed3), AIC(brm_full_tfixed4)),
                      "BIC" = c(BIC(brm_full_tfixed1), BIC(brm_full_tfixed2), 
                                BIC(brm_full_tfixed3), BIC(brm_full_tfixed4))))
                      


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



beta_sim <- MASS::mvrnorm(4000, mu = coef(full_model4), Sigma = vcov(full_model4))
length(beta_sim[1, ])
beta_me <- beta_sim[, c(3, 146)]
summary()
baseline |> 
  #dplyr::filter(brm %in% "Autocracies (BRM)")
  dplyr::filter(vdem_regime %in% c(0, 1)) |> pull(l_massparty) |> summary()

mpo_index <- c(seq(-6, 10, 0.01))

cbind(
  1, mpo_index
) -> X_pile


dim(beta_me)
dim(X_pile)
beta_me %*% t(X_pile) -> ME

tibble(
  Moderator = mpo_index,
  Mean = apply(ME, 2, mean),
  ll = apply(ME, 2, quantile, probs = 0.025),
  ul = apply(ME, 2, quantile, probs = 0.975),
) |> 
  ggplot(aes(x = Moderator, y = Mean)) +
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.2) +
  scale_y_continuous(breaks = c(seq(-4, 3, 1))) +
  scale_x_continuous(breaks = c(seq(-6, 10, 1))) +
  geom_hline(yintercept = 0) +
  labs(x = "\n Levels of Mass Party Organization",
       y = "Estimated Coefficient for Mass Coalition\n")

library(interplot)

glm(
  universality ~ l_mass_coalition*massparty_bin + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> full_model4_bin

baseline |> 
  #dplyr::filter(brm %in% "Autocracies (BRM)")
  dplyr::filter(vdem_regime %in% c(0, 1)) |> mutate(
    massparty_bin = case_when(
      l_massparty == quantile(sample$l_massparty, probs = c(1/2), na.rm = T) ~ 2L,
      l_massparty > quantile(sample$l_massparty, probs = c(1/2), na.rm = T) ~ 3L,
      l_massparty < quantile(sample$l_massparty, probs = c(1/2), na.rm = T) ~ 1L,
      is.na(l_massparty) ~ NA_integer_),
    massparty_bin = factor(massparty_bin, levels = c(2, 1, 3),
                           labels = c("Medium", "Low", "High"))) ->
  baseline

library(interplot)
interplot(m = full_model4, var1 = "l_mass_coalition", var2 = "l_massparty", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none") +  # geom_line(color = "pink") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = points, aes(x = median, y = mean))

baseline |> 
  #dplyr::filter(brm %in% "Autocracies (BRM)")
  dplyr::filter(vdem_regime %in% c(0, 1)) -> sample

sample |> 
  mutate(sep = if_else(
  l_massparty > quantile(sample$l_massparty, probs = c(1/2), na.rm = T), 1L, 0L)) |> 
  group_by(sep) |> drop_na(sep) |> 
  summarize(median = median(l_massparty, na.rm = T)) -> bins
bins
table(baseline$massparty_bin)
length(coef(final))
beta_sim <- MASS::mvrnorm(4000, mu = coef(full_model4_bin), Sigma = vcov(full_model4_bin))
length(beta_sim[1, ])
beta_me <- beta_sim[, c(3, 146)]

low_point <- beta_me[, 1]*1 + beta_me[, 2]*2.5
high_point <- beta_me[, 1]*1 + beta_me[, 2]*as.numeric(bins[2, 2])




quantile(sample$l_massparty, probs = 0.5, na.rm = T)  
mutate(separater = if_else(l_massparty > quantile))
coef(full_model4)[2]*1 + coef(full_model)[146]*1*2.5
coef(full_model4)[2]*1 + coef(full_model)[146]*1*bins[3, 2]
vcov(full_model4)
data$data
texreg::screenreg(list(full_model1, full_model2, full_model3, full_model4),
                  omit.coef = "as.factor")

set.seed(110)
n <- 2000
x <- runif(n,min=-3, max = 3)
d1 <- sample(x = c(0,1),size = n,replace = T)
d2 <- runif(min = 0,max = 1,n = n)
d3 <- sample(x = c(0,1,2),size = n,replace = T)
Z1 <- runif(min = 0,max = 1,n = n)
Z2 <- rnorm(n,3,1)


link1 <- -1+d1+x+d1*x
prob1 <- exp(link1)/(1+exp(link1))
rand.u <- runif(min = 0,max = 1,n = n)
y1 <- as.numeric(rand.u<prob1)
s6 <- cbind.data.frame(X=x,D=d1,Z1=Z1,Z2=Z2,Y=y1)



texreg::screenreg(full_model)
## Without controls, mass-based coalition countries are:
##   Bolivia 2019        (145)
##   Vietnam 1972-2020   (816)
##   Argentina 1976      (160)# It survives when use GWF with Nicaragua
##   Nicaragua 1981-1989 (93) # If use continuous vars for subtype, it only remains.
##   Ukraine 2016-2019   (369)
##   Romania 1991        (360)
##   Tunisia (2012-2014); BRM

baseline |> 
#  dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> 
#  dplyr::filter(brm %in% "Autocracies (BRM)") |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
#  dplyr::filter(gwf_party == 1L | gwf_military == 1L | gwf_personal == 1L | gwf_monarchy == 1L) |> 
  # dplyr::select(
  #   COWcode, year,
  #   universality, l_mass_coalition, l_massparty) |> drop_na() |> 
  dplyr::select(
    COWcode, year,
    universality, l_mass_coalition, l_massparty, 
    #l_gdppc,         # 51
    #l_gdpgrth,       # 51
    #l_lnresource,     # 44
    #l_civilwar,       # 45
    #l_repression,      # 51
    #l_capacity,        # 17
    #l_CSOconsult,     # 51
    #l_CSOpart,        # 51
    #l_hereditaryindex, # 51
    #l_militaryindex,  # 51
    #l_partyindex,     # 51
    #l_personindex,    # 43
    #l_royal,          # 44
    #l_party,          # 44
    #l_mil             # 44
    ) |> drop_na() |> 
  dplyr::filter(l_mass_coalition == "Mass-based") |> print(n = Inf)

               

vdemdata::vdem -> vdem
vdem |> as.data.frame() |> as_tibble() |>
  dplyr::filter(e_boix_regime == 0L & year > 1960 & year < 2012) |> 
  dplyr::filter(e_lexical_index < 3 & year > 1960 & year < 2012) |> 
  pull(v2regimpoppgroup) |> table()

vdem |> dplyr::select(contains("v2regsupgroups"))

glm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + 
    #l_repression + 
    l_capacity + l_CSOconsult + l_CSOpart +
    l_royal + l_party + l_mil +
    #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), data = baseline |> 
    dplyr::filter(vdem_regime %in% c(0, 1))) |> 
  texreg::screenreg(omit.coef = "as.factor")

full_model |> interplot::interplot(var1 = "l_mass_coalition", var2 = "l_massparty", 
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

baseline |> 
  dplyr::select(
    COWcode, year, universality, 
    l_mass_coalition,
    l_massparty,
    # l_gdppc, l_gdpgrth,
    # l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
    # l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex,
    COWcode, year, brm_regime) |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> drop_na() |> pull(COWcode) |> 
  unique() -> unique_COWfullnoctrl_brm

#### Tex output -----
full_model4 |> broom::augment() |> dplyr::select(16) |> unique() |> count()
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



#### Jackknife for full model 1 -------------------------------------------------------------------- 
jk_full1_brm <- data.frame()

for (i in 1:length(unique_COWfullnoctrl_brm)) {
  tryCatch({
    model <- plm::plm(
      universality ~ l_mass_coalition + l_massparty + 
        as.factor(COWcode) + as.factor(year), 
      index = c("COWcode", "year"),
      effect = "twoways", 
      data = baseline |> 
        dplyr::filter(brm_regime %in% "Autocracies (BRM)" & 
                        !COWcode %in% unique_COWfullnoctrl_brm[i])
    )
    
    coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
    temp_df <- data.frame(
      term = c("Mass Coalition", "Mass Party Org."),
      median = c(mean(coef[, "l_mass_coalitionMass-based"], na.rm = T), 
                 mean(coef[, "l_massparty"], na.rm = T)),
      ll = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.025), 
             quantile(coef[, "l_massparty"], na.rm = T, probs = 0.025)),
      ul = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.975), 
             quantile(coef[, "l_massparty"], na.rm = T, probs = 0.975))
    ) |> 
      mutate(Exclude = paste0("County ", unique_COWfullnoctrl_brm[i], " excluded"),
             id = paste0(unique_COWfullnoctrl_brm[i]))
    jk_full1_brm <- jk_full1_brm |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}


#### Jackknife for full model 2 -------------------------------------------------------------------- 
jk_full2_brm <- data.frame()

for (i in 1:length(unique_COWfullnoctrl_brm)) {
  tryCatch({
  model <- plm::plm(
    universality ~ l_mass_coalition*l_massparty +
      as.factor(COWcode) + as.factor(year), 
    index = c("COWcode", "year"),
    effect = "twoways",     data = baseline |> 
      dplyr::filter(brm_regime %in% "Autocracies (BRM)" & 
                      !COWcode %in% unique_COWfullnoctrl_brm[i]))
  
  coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
  temp_df <- data.frame(
    term = c("Mass Coalition", "Mass Party Org.", "Mass Coalition X Mass Party Org."),
    median = c(mean(coef[, "l_mass_coalitionMass-based"], na.rm = T), 
               mean(coef[, "l_massparty"], na.rm = T), 
               mean(coef[, "l_mass_coalitionMass-based:l_massparty"], na.rm = T)),
    ll = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.025), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.025), 
           quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                    na.rm = T, probs = 0.025)),
    ul = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.975), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.975), 
           quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                    na.rm = T, probs = 0.975))
  ) |> 
    mutate(Exclude = paste0("County ", unique_COWfullnoctrl_brm[i], " excluded"),
           id = paste0(unique_COWfullnoctrl_brm[i]))
  jk_full2_brm <- jk_full2_brm |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}
# Error when I exclude Nicaragua

#### Jackknife for full model 3 -------------------------------------------------------------------- 
jk_full3_brm <- data.frame()

for (i in 1:length(unique_COWfull_brm)) {
  tryCatch({
  model <- plm::plm(
    universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
      l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), 
    index = c("COWcode", "year"),
    effect = "twoways", 
    data = baseline |> 
      dplyr::filter(brm_regime %in% "Autocracies (BRM)" & 
                      !COWcode %in% unique_COWfull_brm[i]))
  
  coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
  temp_df <- data.frame(
    term = c("Mass Coalition", "Mass Party Org."),
    median = c(mean(coef[, "l_mass_coalitionMass-based"], na.rm = T), 
               mean(coef[, "l_massparty"], na.rm = T)),
    ll = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.025), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.025)),
    ul = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.975), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.975))
  ) |> 
    mutate(Exclude = paste0("County ", unique_COWfull_brm[i], " excluded"),
           id = paste0(unique_COWfull_brm[i]))
  jk_full3_brm <- jk_full3_brm |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}

#### Jackknife for full model 4 -------------------------------------------------------------------- 
jk_full4_brm <- data.frame()

for (i in 1:length(unique_COWfull_brm)) {
  tryCatch({
    model <- plm::plm(
      universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
        l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
        l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
        as.factor(COWcode) + as.factor(year), 
      index = c("COWcode", "year"),
      effect = "twoways", 
      data = baseline |> 
        dplyr::filter(brm_regime %in% "Autocracies (BRM)" & 
                        !COWcode %in% unique_COWfull_brm[i]))
    
    coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
    temp_df <- data.frame(
      term = c("Mass Coalition", "Mass Party Org.", "Mass Coalition X Mass Party Org."),
      median = c(mean(coef[, "l_mass_coalitionMass-based"], na.rm = T), 
                 mean(coef[, "l_massparty"], na.rm = T), 
                 mean(coef[, "l_mass_coalitionMass-based:l_massparty"], na.rm = T)),
      ll = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.025), 
             quantile(coef[, "l_massparty"], na.rm = T, probs = 0.025), 
             quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                      na.rm = T, probs = 0.025)),
      ul = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.975), 
             quantile(coef[, "l_massparty"], na.rm = T, probs = 0.975), 
             quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                      na.rm = T, probs = 0.975))
    ) |> 
      mutate(Exclude = paste0("County ", unique_COWfull_brm[i], " excluded"),
             id = paste0(unique_COWfull_brm[i]))
    jk_full4_brm <- jk_full4_brm |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}


glm(universality ~ l_mass_coalition*(l_massparty + I(l_massparty^2)) + log(l_gdppc + 1) + 
      l_gdpgrth + l_lnresource + l_civilwar + l_repression + 
      l_CSOconsult + l_CSOpart + l_hereditaryindex + l_militaryindex + 
      l_partyindex + l_personindex + as.factor(COWcode) + as.factor(year),
 #   index = c("COWcode", "year"),
    #model = "random", , random.method = "walhus",
  #  effect = "twoways",
    data = baseline |> 
      dplyr::filter(vdem_regime %in% c(0, 1) & !COWcode %in% 816) |> 
      dplyr::mutate(l_mass_coalition = as.numeric(l_mass_coalition))) |> 
  #broom::augment() -> check
  interplot::interplot(var1 = "l_mass_coalition", var2 = "l_massparty")
commun |> dplyr::select(COWcode) |> unique()

baseline |> drop_na(l_mass_coalition) |> 
  mutate(cut_party = cut(l_massparty, breaks = c(seq(-5, 10, 1)))) |> 
  group_by(l_mass_coalition, cut_party) |> 
  summarize(median = median(universality, na.rm = T),
            ll = quantile(universality, probs = 0.025, na.rm = T),
            ul = quantile(universality, probs = 0.975, na.rm = T)) |> ungroup() |> 
  ggplot(aes(x = cut_party, y = median, color = as.factor(l_mass_coalition), group = 1)) + 
  geom_line() + geom_line(aes(y = ll)) + geom_line(aes(y = ul)) + geom_point() + facet_wrap(~l_mass_coalition)
names(check)
baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1) & COWcode %in% 816) |> 
  ggplot(aes(x = l_massparty, y = universality))

baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  dplyr::filter(year > 1965) |> 
  dplyr::select(COWcode, universality, l_mass_coalition, l_massparty,
                l_gdppc, l_gdpgrth, l_lnresource, l_civilwar, l_repression, 
                  l_CSOconsult, l_CSOpart, l_hereditaryindex, l_militaryindex, 
                  l_partyindex, l_personindex) |> drop_na() |> 
  dplyr::select(COWcode) |> 
  mutate(cname = countrycode::countrycode(COWcode, "cown", "country.name")) |> 
  unique() |> 
  arrange(COWcode) |> 
  dplyr::filter(grepl("Ukri", cname)) |> 
  print(n = Inf) 

commun <- readxl::read_xlsx("Analysis_data/communist_countries_data.xlsx")

baseline |> mutate(
  communism = case_when(
    COWcode == 365 & year <= 1991 ~ 1L,
    COWcode == 710 ~ 1L,
    COWcode == 40 ~ 1L,
    COWcode == 816 ~ 1L,
    COWcode == 731 ~ 1L,
    COWcode == 265 & year <= 1990 ~ 1L,
    COWcode == 360 & year <= 1989 ~ 1L,
    COWcode == 355 & year <= 1990 ~ 1L,
    COWcode == 310 & year <= 1989 ~ 1L,
    COWcode == 290 & year <= 1989 ~ 1L,
    COWcode == 315 & year <= 1989 ~ 1L,
    COWcode == 812 & year >= 1975 ~ 1L,
    COWcode == 811 & year >= 1975 & year <= 1989 ~ 1L,
    COWcode == 700 & year >= 1978 & year <= 1992 ~ 1L,
    COWcode ==  370 ~ 1L,
    COWcode ==  369 ~ 1L,
    COWcode ==  704 ~ 1L,
    COWcode ==  703 ~ 1L,
    COWcode ==  372 ~ 1L,
    COWcode ==  373 ~ 1L,
    COWcode ==  368 ~ 1L,
    COWcode ==  359 ~ 1L,
    COWcode ==  367 ~ 1L,
    COWcode ==  702 ~ 1L,
    COWcode ==  701 ~ 1L,
    COWcode ==  371 ~ 1L,
    COWcode ==  705 ~ 1L,
    COWcode ==  366 ~ 1L,
    T ~ 0L
  )) |> group_by(COWcode) |> mutate(communism = dplyr::lag(
    communism, order_by = year, 1
  )) |> ungroup() -> baseline 



mpo_int <- c(seq(-5, 10, 0.01))
coef_f4main <- MASS::mvrnorm(n = 4000, mu = coef(full_tfixed2), Sigma = vcov(full_tfixed2))
me_f4main <- coef_f4main[, c("l_mass_coalition", "l_mass_coalition:l_massparty")] %*% rbind(1, mpo_int) 
f4main_df <- data.frame(
  MPO = mpo_int,
  Mean = apply(me_f4main, 2, mean, na.rm = T),
  lower = apply(me_f4main, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(me_f4main, 2, quantile, probs = 0.975, na.rm = T)
) |> mutate(Exclude = paste0("No County excluded"),
            id = paste0("0"))

for (i in 1:length(unique_COWfull)) {
  tryCatch({
    model <- glm(
    universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
      l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year),
    data = baseline |> 
      dplyr::filter(vdem_regime %in% c(0, 1) & !COWcode %in% unique_COWfull[i]))
  mpo_int <- c(seq(-5, 10, 0.01))
  coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
  me <- coef[, c("l_mass_coalitionMass-based", "l_mass_coalitionMass-based:l_massparty")] %*% rbind(1, mpo_int) 
  temp_df <- data.frame(
    MPO = mpo_int,
    Mean = apply(me, 2, mean, na.rm = T),
    lower = apply(me, 2, quantile, probs = 0.025, na.rm = T),
    upper = apply(me, 2, quantile, probs = 0.975, na.rm = T)
  ) |> 
    mutate(Exclude = paste0("County ", unique_COWfull[i], " excluded"),
           id = paste0(unique_COWfull[i]))
  jk_full4 <- jk_full4 |> bind_rows(temp_df)
  }, error = function(e) {
  message(paste("Error in iteration", i, ":", e$message))
  # Optionally, you can log or handle the error here
})
}

jk_full4 |> bind_rows(f4main_df) ->
  jk_full4_final

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())
baseline
coef(full_tfixed2)
baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
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
  ) -> check
  
coef(full_tfixed4)[1]*1 + coef(full_tfixed4)[14]*as.numeric(check[1, 1])  -> low

coef(full_tfixed4)[1]*1 + coef(full_tfixed4)[14]*check[2, 1] -> high

point <- data.frame(MPO = c(as.numeric(check[1, 1]), as.numeric(check[2, 1])),
                    Mean =c(as.numeric(low), as.numeric(high)) ,
                    id = c("0", "0"))

jk_full4_final |> 
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
  labs(x = NULL, y = "Estimated marginal effects") +
  gghighlight::gghighlight(id %in% "0", use_direct_label = FALSE) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) -> ME_sim


baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  ggplot(aes(x = l_massparty, y = ..density..)) + 
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(breaks = c(seq(-5, 10, 1)), lim = c(-5, 10), expand = c(.05,.05)) +
  labs(x = expression('Mass Party Organization Index'[t-1]), y = "Density",
       caption = str_wrap("\nNote: The figure depicts the impact of mass-based coalitions on welfare universalism, with the main result from the full model highlighted in color. Each line plot is shown with 95% confidence intervals. The colored points represent the estimated marginal effects at the bottom and top ten percentiles of the mass party organization index. Grey lines represent the marginal effects of models using the jackknife method, with each model excluding one country from the analysis. Excluding Vietnam represents the marginal effect of mass-based coalitions and mass party organization statistically insignificant", 100, exdent = 10)) +
  theme(plot.caption = element_text(hjust = 0, size = 12)) -> MPO_hist

library(patchwork)
library(ggExtra)

ME_sim + MPO_hist +
  patchwork::plot_layout(
    ncol = 1, 
    nrow = 2, 
    widths = 4,
    heights = c(6.5, 1.5)
)
ggsave("Documents/2_Manuscript/2_Figures/Fig_me_jk.pdf",
       width = 8, height = 6, dpi = 1600)

unique(hist$breaks)
m2 <- m1 + geom_histogram(aes(l_massparty), position="identity", linetype=1,
                          fill="gray60", data = baseline |> 
                            dplyr::filter(vdem_regime %in% c(0, 1)), alpha=0.5, bins = 30) +
  scale_y_continuous(expand = c(0,0))


g1 <- ggplot_gtable(ggplot_build(m1))
g2 <- ggplot_gtable(ggplot_build(m2))

library(gtable)

pp <- c(subset(g1$layout, name == "panel", se = t:r))
# The link uses g2$grobs[[...]] but it doesn't seem to work... single bracket works, on the other hand....
g <- gtable_add_grob(g1, g2$grobs[which(g2$layout$name == "panel")], pp$t, pp$l, pp$b, pp$l)

library(grid)
grid.draw(g)


baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1))
# Exclude Nicaragua makes error
baseline |> 
  dplyr::filter(vdem_regime %in% c(0, 1) & COWcode %in% unique_COWfull[37]) |> 
  janitor::tabyl(l_mass_coalition)

bind_rows(
full_model1 |> broom::tidy() |> bind_cols(
  broom::confint_tidy(full_model1)) |> 
  dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
  mutate(Exclude = "None", id = "0") |> 
  dplyr::filter(term %in% c("l_mass_coalition", "l_massparty")) |> 
  mutate(term = case_when(
    term == "l_mass_coalition" ~ "Mass Coalition",
    term == "l_massparty" ~ "Mass Party Org."),
    Model = "Model 1 (without Controls)"),
full_model2 |> broom::tidy() |> bind_cols(
  broom::confint_tidy(full_model2)) |> 
  dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
  mutate(Exclude = "None", id = "0") |> 
  dplyr::filter(term %in% c("l_mass_coalition", "l_massparty", "l_mass_coalition:l_massparty")) |> 
  mutate(term = case_when(
    term == "l_mass_coalition" ~ "Mass Coalition",
    term == "l_massparty" ~ "Mass Party Org.",
    term == "l_mass_coalition:l_massparty" ~ "Mass Coalition X Mass Party Org."),
    Model = "Model 2 (without Controls)"),
full_model3 |> broom::tidy() |> bind_cols(
  broom::confint_tidy(full_model3)) |> 
  dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
  mutate(Exclude = "None", id = "0") |> 
  dplyr::filter(term %in% c("l_mass_coalition", "l_massparty")) |> 
  mutate(term = case_when(
    term == "l_mass_coalition" ~ "Mass Coalition",
    term == "l_massparty" ~ "Mass Party Org."),
    Model = "Model 3 (with Controls)"),
full_model4 |> broom::tidy() |> bind_cols(
  broom::confint_tidy(full_model4)) |> 
  dplyr::select(term, median = estimate, ll = conf.low, ul = conf.high) |> 
  mutate(Exclude = "None", id = "0") |> 
  dplyr::filter(term %in% c("l_mass_coalition", "l_massparty", "l_mass_coalition:l_massparty")) |> 
  mutate(term = case_when(
    term == "l_mass_coalition" ~ "Mass Coalition",
    term == "l_massparty" ~ "Mass Party Org.",
    term == "l_mass_coalition:l_massparty" ~ "Mass Coalition X Mass Party Org."),
    Model = "Model 4 (with Controls)")
) -> complete_addtive


bind_rows(
  "Model 1 (without Controls)" = jk_full1 |> rowid_to_column(),
  "Model 2 (without Controls)" = jk_full2 |> rowid_to_column(),
  "Model 3 (with Controls)" = jk_full3 |> rowid_to_column(),
  "Model 4 (with Controls)" = jk_full4 |> rowid_to_column(),
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

ghibli::ghibli_palette("PonyoLight", direction = 1, n = 5)

glm(universality ~ l_mass_coalition + 
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model1

glm(universality ~ l_massparty + 
      as.factor(COWcode) + as.factor(year), data = baseline|> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model2

glm(universality ~ l_mass_coalition + l_massparty + 
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model3

texreg::screenreg(list(model1, model2, model3), omit.coef = "as.factor")

glm(universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model4
texreg::screenreg(model4, omit.coef = "as.factor")

baseline |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> 
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
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model5

glm(universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")) -> model6

glm(universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart + 
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> 
      mutate(l_mass_coalition = as.numeric(l_mass_coalition)))  -> model7

# baseline |> 
#   dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> group_by(mass_coalition) |> 
#   summarize(mean = mean(personindex, na.rm = T),
#             n = n())


glm(universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + l_lnresource +
      l_civilwar + l_repression + l_CSOconsult + l_CSOpart +  + l_mil + l_royal + l_party +
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

interplot::interplot(full_model4, var1 = "l_mass_coalition", var2 = "l_massparty", 
                     plot = T,
                     hist = T, sims = 4000,
                     #xmin = -8, xmax = 10,
                     ercolor = "#275D8E",
                     ralpha = 0.6,
                     rfill = "#275D8E") +
  labs(y = "Estimated Coefficient for Mass Coalition\n",
       title = NULL, x = "\nLevels of Mass Party Organization",
       subtitle = NULL, caption = NULL) + 
#  scale_x_continuous(breaks = c(seq(-5, 10, 5))) +
#  scale_y_continuous(breaks = c(seq(-2, 6, 1))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))


### Interaction term's Linearity Assumtion check
#devtools::install_github('xuyiqing/interflex')

broom::augment(full_model) -> aug_full
table(aug_full$l_mass_coalition)
aug_full |> dplyr::filter(l_mass_coalition == 2) |> pull(universality) |> length()
table(baseline$mass_coalition)
library(tidyverse)
baseline |> drop_na(mass_coalition) |> 
  mutate(mass_coalition = if_else(mass_coalition == "Institution-based",
                                  "Elite-based coalition", "Mass-based coalition"),
         massparty_bin = case_when(
           massparty < quantile(massparty, probs = 1/3, na.rm = T) ~ "Low",
           massparty > quantile(massparty, probs = 2/3, na.rm = T) ~ "High",
           is.na(massparty) ~ NA_character_,
           T ~ "Medium"),
         massparty_bin = factor(massparty_bin, levels = c("Low", "Medium", "High"))) |> 
  drop_na(massparty_bin) |> 
  ggplot(aes(x = massparty_bin, y = universality)) +
  geom_boxplot() + facet_wrap(~mass_coalition) +
  geom_smooth(method = "lm", se = F,
              color = futurevisions::futurevisions("mars")[1]) +
  geom_smooth(method = "loess", se = F,
              color = futurevisions::futurevisions("mars")[3])

baseline |> 
  mutate(mass_coalition = if_else(mass_coalition == "Institution-based",
                                  "Elite-based coalition", "Mass-based coalition"),
         massparty_bin = case_when(
           massparty < quantile(massparty, probs = 1/3, na.rm = T) ~ "Low",
           massparty > quantile(massparty, probs = 2/3, na.rm = T) ~ "High",
           is.na(massparty) ~ NA_character_,
           T ~ "Medium"),
         massparty_bin = factor(massparty_bin, levels = c("Low", "Medium", "High"))) ->
  baseline
baseline |> mutate(
  masspartyL = if_else(massparty_bin == "Low", 1L, 0L),
  masspartyM = if_else(massparty_bin == "Medium", 1L, 0L),
  masspartyH = if_else(massparty_bin == "High", 1L, 0L)
) -> baseline

baseline |> group_by(massparty_bin) |> 
  mutate(median = if_else(masspartyL == 1L, median(l_massparty, na.rm = T),
                          if_else(masspartyM == 1L, median(l_massparty, na.rm = T),
                                  if_else(masspartyH == 1L, median(l_massparty, na.rm = T), NA_real_)))) ->
  baseline
table(baseline$median)
glm(
  universality ~ ((masspartyM + masspartyH)*(l_mass_coalition*(l_massparty - median))) + 
    log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
) -> LIE
texreg::screenreg(LIE, omit.coef = "as.factor")
interplot::interplot(LIE, var1 = "l_mass_coalition", var2 = "l_massparty", 
                     plot = T,
                     hist = T, sims = 4000,
                     xmin = -8, xmax = 10,
                     ercolor = "#275D8E",
                     ralpha = 0.6,
                     rfill = "#275D8E") +
  labs(y = "Estimated Coefficient for Mass Coalition\n",
       title = NULL, x = "\nLevels of Mass Party Organization",
       subtitle = NULL, caption = NULL) + 
  geom_point(x = -4, y = 5, size = 3) +
  scale_x_continuous(breaks = c(seq(-5, 10, 5))) +
  scale_y_continuous(breaks = c(seq(-2, 6, 1))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

table(baseline$median)
# MasspartyL
coef(LIE)[1] + 
  coef(LIE)[2]*0 +
  coef(LIE)[4]*0 +
  coef(LIE)[5]*mean(baseline$l_massparty, na.rm = T) +
  coef(LIE)[6]*mean(log(baseline$l_gdppc + 1), na.rm = T) +
  coef(LIE)[7]*mean(baseline$l_gdpgrth, na.rm = T) +
  coef(LIE)[8]*mean(baseline$l_lnresource, na.rm = T) +
  coef(LIE)[9]*mean(baseline$l_civilwar, na.rm = T) +
  coef(LIE)[10]*mean(baseline$l_repression, na.rm = T) +
  coef(LIE)[11]*mean(baseline$l_CSOconsult, na.rm = T) +
  coef(LIE)[12]*mean(baseline$l_CSOpart, na.rm = T) +
  coef(LIE)[13]*mean(baseline$l_hereditaryindex, na.rm = T) +
  coef(LIE)[14]*mean(baseline$l_militaryindex, na.rm = T) +
  coef(LIE)[15]*mean(baseline$l_partyindex, na.rm = T) +
  coef(LIE)[16]*mean(baseline$l_personindex, na.rm = T) +
  coef(LIE)[148]*0 +
  coef(LIE)[149]*0 + coef(LIE)[150]*0 +  
  coef(LIE)[152]*0 

# MasspartyM
coef(LIE)[1] + 
  coef(LIE)[2]*1 +
  coef(LIE)[3]*0 +
  coef(LIE)[4]*0 +
  coef(LIE)[5]*mean(baseline$l_massparty, na.rm = T) +
  coef(LIE)[6]*mean(log(baseline$l_gdppc + 1), na.rm = T) +
  coef(LIE)[7]*mean(baseline$l_gdpgrth, na.rm = T) +
  coef(LIE)[8]*mean(baseline$l_lnresource, na.rm = T) +
  coef(LIE)[9]*mean(baseline$l_civilwar, na.rm = T) +
  coef(LIE)[10]*mean(baseline$l_repression, na.rm = T) +
  coef(LIE)[11]*mean(baseline$l_CSOconsult, na.rm = T) +
  coef(LIE)[12]*mean(baseline$l_CSOpart, na.rm = T) +
  coef(LIE)[13]*mean(baseline$l_hereditaryindex, na.rm = T) +
  coef(LIE)[14]*mean(baseline$l_militaryindex, na.rm = T) +
  coef(LIE)[15]*mean(baseline$l_partyindex, na.rm = T) +
  coef(LIE)[16]*mean(baseline$l_personindex, na.rm = T) +
  coef(LIE)[148]*0 +
  coef(LIE)[149]*0 + coef(LIE)[150]*1*mean(baseline$l_massparty, na.rm = T) +  
  #coef(LIE)[151]*0 
  coef(LIE)[152]*0 
  
# MasspartyH
coef(LIE)[1] + 
  coef(LIE)[2]*0 +
  coef(LIE)[3]*1 +
  coef(LIE)[4]*0 +
  coef(LIE)[5]*mean(baseline$l_massparty, na.rm = T) +
  coef(LIE)[6]*mean(log(baseline$l_gdppc + 1), na.rm = T) +
  coef(LIE)[7]*mean(baseline$l_gdpgrth, na.rm = T) +
  coef(LIE)[8]*mean(baseline$l_lnresource, na.rm = T) +
  coef(LIE)[9]*mean(baseline$l_civilwar, na.rm = T) +
  coef(LIE)[10]*mean(baseline$l_repression, na.rm = T) +
  coef(LIE)[11]*mean(baseline$l_CSOconsult, na.rm = T) +
  coef(LIE)[12]*mean(baseline$l_CSOpart, na.rm = T) +
  coef(LIE)[13]*mean(baseline$l_hereditaryindex, na.rm = T) +
  coef(LIE)[14]*mean(baseline$l_militaryindex, na.rm = T) +
  coef(LIE)[15]*mean(baseline$l_partyindex, na.rm = T) +
  coef(LIE)[16]*mean(baseline$l_personindex, na.rm = T) +
  coef(LIE)[148]*0 +
  coef(LIE)[149]*0 + coef(LIE)[150]*0*mean(baseline$l_massparty, na.rm = T) +  
  #coef(LIE)[151]*0 
  coef(LIE)[152]*1*mean(baseline$l_massparty, na.rm = T)

# MasspartyL (0.141)  ~ 0.7497444
# MasspartyM (3.6025) ~ 1.018437
# MasspartyM (6.947)  ~ 0.9801487


ggsave("Documents/2_Manuscript/2_Figures/fig3_spsa.pdf", width = 7, height = 4.5, dpi = "retina")


glm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
)

glm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
    l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    dplyr::filter(vdem_regime %in% c(0, 1) & !COWcode %in% 93))

## Without controls, mass-based coalition countries are:
##   Bolivia 2019        (145)
##   Vietnam 1972-2020   (816)
##   Argentina 1976      (160)# It survives when use GWF with Nicaragua
##   Nicaragua 1981-1989 (93) # If use continuous vars for subtype, it only remains.
##   Ukraine 2016-2019   (369)
##   Romania 1991        (360)

baseline |> 
  mutate(id= case_when(
    COWcode == 93 ~ "Nicaragua",
    COWcode == 145 ~ "Bolivia",
    COWcode == 816 ~ "Vietnam",
    T ~ "Others"),
    id = factor(id, levels = c("Nicaragua", "Bolivia", "Vietnam",
                               "Others"))) |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> 
  drop_na(universality,
          l_mass_coalition, l_massparty, l_gdppc, l_gdpgrth, 
          l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
          l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex) |> 
  ggplot(aes(x = l_mass_coalition, y = universality, color= as.factor(id))) + 
  geom_jitter(alpha = 0.9) +
  
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3],
                                futurevisions::futurevisions("mars")[2],
                                "grey80"))
