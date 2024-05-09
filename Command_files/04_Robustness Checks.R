## Project: Why do authoritarian regimes provide welfare programs? -------------
##          4. Robustness Checks  --------------------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:

## Import Packages to use ------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")

pacman::p_load(ggplot2, grid, pBrackets, rio, countrycode, reshape2, 
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
robust_alt <- readRDS("Analysis_data/robust_alt.RDS")

## Distribution of social groups in ruling coalitions, 1960-2020 --------
##### Figure A1 The share of mass-based and elite-based coalitions, 1960-2020 ----------------------
##### 2.2 %
baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  #drop_na(year, mass_coalition) |> 
  dplyr::filter(year > 1959 & year < 2021) |> 
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
  count()  |> ungroup() |> 
  group_by(year) |> mutate(total = sum(n),
                           share = n/total) |> ungroup() |> 
  group_by(coalition_agg) |> 
  mutate(
    label = if_else(year == max(year), 
                    as.character(coalition_agg), NA_character_)) |> ungroup() -> figure1

figure1 |> dplyr::filter(coalition_agg %in% c("Working class", "Middle class")) |> 
  print(n = Inf)

figure1 |> 
  ggplot(aes(x = year, y = share)) +
  geom_path(aes(color = coalition_agg, 
                fill = coalition_agg), show.legend = F, alpha = 1) +
  ggrepel::geom_label_repel(aes(x = year, label = label, color = coalition_agg), size = 5,
                            min.segment.length = Inf, 
                            na.rm = TRUE, show.legend = F) + 
  #ggsci::scale_color_nejm() +
  #ggsci::scale_fill_nejm() +
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3],
                                futurevisions::futurevisions("mars")[4],
                                "darkgreen")) +
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[1],
                               futurevisions::futurevisions("mars")[3],
                               futurevisions::futurevisions("mars")[4],
                               "darkgreen")) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(seq(0, 1, 0.1))) +
  scale_x_continuous(breaks = c(seq(1900, 2020, 10))) +
  labs(y = "Share of countries with group in ruling coalition\n", x = "\nYear",
       #title = "Social groups of Masses and Elites in Regime Coalitions, Globally. 1789-2020"
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

ggsave("Documents/2_Manuscript/2_Figures/fig1A.pdf",
       width = 8.5, height = 5, dpi = "retina")

## Benchmark and Full Models -----------------------------------------------------------------------
### Model Specification ---------------------------------------------------------
#### Benchmark Models: Without Controls ----------------------------------------

load("Analysis_data/main_bench.RData")
# 
# bench1_rand_brm <- plm::plm(universality ~ l_mass_coalition,
#                             index = c("COWcode", "year"),
#                             model = "random", random.method = "walhus",
#                             data = baseline |> 
#                               dplyr::filter(brm_regime %in% 
#                                               c("Autocracies (BRM)")))
# 
# bench2_rand_brm <- plm::plm(universality ~ l_massparty,
#                             index = c("COWcode", "year"),
#                             model = "random", random.method = "walhus",
#                             data = baseline |> 
#                               dplyr::filter(brm_regime %in% 
#                                               c("Autocracies (BRM)")))
# 
# bench3_rand_brm <- plm(universality ~ l_mass_coalition + l_massparty,
#                        index = c("COWcode", "year"),
#                        model = "random", random.method = "walhus",
#                        data = baseline |> 
#                          dplyr::filter(brm_regime %in% 
#                                          c("Autocracies (BRM)")))
# 
# bench4_rand_brm <- plm(universality ~ l_mass_coalition*l_massparty,
#                        index = c("COWcode", "year"),
#                        model = "random", random.method = "walhus",
#                        data = baseline |> 
#                          dplyr::filter(brm_regime %in% 
#                                          c("Autocracies (BRM)")))
# 
# bench5_cfixed_brm <- plm(universality ~ l_mass_coalition,
#                          index = c("COWcode", "year"),
#                          effect = "individual",
#                          data = baseline |> 
#                            dplyr::filter(brm_regime %in% 
#                                            c("Autocracies (BRM)")))
# 
# bench6_cfixed_brm <- plm(universality ~ l_massparty +
#                            as.factor(COWcode),
#                          index = c("COWcode", "year"),
#                          effect = "individual",
#                          data = baseline |> 
#                            dplyr::filter(brm_regime %in% 
#                                            c("Autocracies (BRM)")))
# 
# bench7_cfixed_brm <- plm(universality ~ l_mass_coalition + l_massparty +
#                            as.factor(COWcode),
#                          index = c("COWcode", "year"),
#                          effect = "individual",
#                          data = baseline |> 
#                            dplyr::filter(brm_regime %in% 
#                                            c("Autocracies (BRM)")))
# 
# bench8_cfixed_brm <- plm(universality ~ l_mass_coalition*l_massparty +
#                            as.factor(COWcode),
#                          index = c("COWcode", "year"),
#                          effect = "individual",
#                          data = baseline |> 
#                            dplyr::filter(brm_regime %in% 
#                                            c("Autocracies (BRM)")))
# 
# bench9_tfixed_brm <- plm(universality ~ l_mass_coalition +
#                            as.factor(COWcode) + as.factor(year),
#                          index = c("COWcode", "year"),
#                          effect = "twoways",
#                          data = baseline |> 
#                            dplyr::filter(brm_regime %in% 
#                                            c("Autocracies (BRM)")))
# 
# bench10_tfixed_brm <- plm(universality ~ l_massparty +
#                             as.factor(COWcode) + as.factor(year),
#                           index = c("COWcode", "year"),
#                           effect = "twoways",
#                           data = baseline |> 
#                             dplyr::filter(brm_regime %in% 
#                                             c("Autocracies (BRM)")))
# 
# bench11_tfixed_brm <- plm(universality ~ l_mass_coalition + l_massparty +
#                             as.factor(COWcode) + as.factor(year),
#                           index = c("COWcode", "year"),
#                           effect = "twoways",
#                           data = baseline |> 
#                             dplyr::filter(brm_regime %in% 
#                                             c("Autocracies (BRM)")))
# 
# bench12_tfixed_brm <- plm(universality ~ l_mass_coalition*l_massparty +
#                             as.factor(COWcode) + as.factor(year),
#                           index = c("COWcode", "year"),
#                           effect = "twoways",
#                           data = baseline |> 
#                             dplyr::filter(brm_regime %in% 
#                                             c("Autocracies (BRM)")))

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

##### Table B1: Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions ------
#####           in Autocracies
# modelsummary::modelsummary(
#   panels, stars = TRUE, shape = "rbind",
#   coef_map = c("l_mass_coalitionMass-based" = "Mass-Based Coalition (vs. Elite-Based)",
#                "l_massparty" = "Mass Party Organization Index",
#                "l_mass_coalitionMass-based:l_massparty" = 
#                  "Mass-Based Coalition$\\times$ Mass Party Organization Index"),
#   coef_omit = "as.factor",
#   gof_omit = 'DF|Deviance|R2|adjR2', 
#   #output = "html"
#   #output = "latex"
#   #output = "gt"
# )

texreg::screenreg(list(bench1_rand_brm,  bench2_rand_brm, bench3_rand_brm, bench4_rand_brm,
                       bench5_cfixed_brm, bench6_cfixed_brm, bench7_cfixed_brm, bench8_cfixed_brm,
                       bench9_tfixed_brm, bench10_tfixed_brm, bench11_tfixed_brm, bench12_tfixed_brm))

#### Full Models -----------------------------------------------------------------------------------

## additive: 94         1966-2011
## MPO only: 100        1966-2011
## Coalition only: 100  1947-2011
# 
# baseline |> 
#   dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#   dplyr::select(universality, COWcode, year, 
#                 l_mass_coalition,
#                 l_massparty,
#                 l_gdppc, l_gdpgrth, 
#                 l_lnresource, l_civilwar, l_repression, l_CSOconsult, l_CSOpart,
#                 l_hereditaryindex, l_militaryindex, l_partyindex, l_personindex
#   ) |> 
#   drop_na() |> 
#   pull(year) |> 
#   summary()
# 
# plm::plm(
#   universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year) -1, 
#   index = c("COWcode", "year"),
#   model = "random", random.method = "walhus",
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_random1
# 
# plm::plm(
#   universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year) -1, 
#   index = c("COWcode", "year"),
#   model = "random", random.method = "walhus",
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_random2
# 
# plm::plm(
#   universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year) -1, 
#   index = c("COWcode", "year"),
#   model = "random", random.method = "walhus",
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_random3
# 
# plm::plm(
#   universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year) -1, 
#   index = c("COWcode", "year"),
#   model = "random", random.method = "walhus",
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_random4
# 
# plm::plm(
#   universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "individual", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_cfixed1
# 
# plm::plm(
#   universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "individual", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_cfixed2
# 
# plm::plm(
#   universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "individual", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_cfixed3
# 
# plm::plm(
#   universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "individual", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_cfixed4
# 
# plm::plm(
#   universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "twoways", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_tfixed1
# 
# plm::plm(
#   universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "twoways", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_tfixed2
# 
# plm::plm(
#   universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "twoways", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_tfixed3
# 
# plm::plm(
#   universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#     l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#     #l_royal + l_party + l_mil +
#     l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#     as.factor(COWcode) + as.factor(year), 
#   index = c("COWcode", "year"),
#   effect = "twoways", 
#   data = baseline |> 
#     dplyr::filter(brm_regime %in% c("Autocracies (BRM)")) |> 
#     mutate(l_mass_coalition = as.numeric(l_mass_coalition))
# ) -> brm_full_tfixed4

load("Analysis_data/main_full.RData")

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

##### Table B2: Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions ------
#####           in Autocracies
# modelsummary::modelsummary(list(brm_full_random1, brm_full_random2, brm_full_random3, brm_full_random4,  
#                                 brm_full_cfixed1, brm_full_cfixed2, brm_full_cfixed3, brm_full_cfixed4,  
#                                 brm_full_tfixed1, brm_full_tfixed2, brm_full_tfixed3, brm_full_tfixed4), 
#                            stars = TRUE,
#                            coef_map = c("l_mass_coalition" = "Mass-Based Coalition (vs. Elite-Based)",
#                                         "l_massparty" = "Mass Party Organization Index",
#                                         "l_mass_coalition:l_massparty" = "Mass-Based Coalition\\times Mass Party Organization Index",
#                                         "log(l_gdppc + 1)" = "Ln(GDPpc + 1)",
#                                         "l_gdpgrth" = "Annual GDP Growth",
#                                         "l_lnresource" = "Resource Dependence (Per GDPpc)",
#                                         "l_civilwar" = "Civil War Experience",
#                                         "l_repression" = "Repression: Human Rights",
#                                         #"Repression: State Capacity",
#                                         "l_CSOconsult" = "CSO Consulation",
#                                         "l_CSOpart" = "CSO Participatory Environment",
#                                         "l_hereditaryindex" = "Hereditary Dimension",
#                                         "l_militaryindex" = "Military Dimension",
#                                         "l_partyindex" = "Ruling Party Dimension",
#                                         "l_personindex" = "Personalist Dimension"),
#                            add_rows = rows,
#                            coef_omit = "as.factor",
#                            gof_omit = 'DF|Deviance|R2|adjR2', 
#                            #output = 'latex',
#                            output = "gt")

texreg::screenreg(list(brm_full_random1, brm_full_random2, brm_full_random3, brm_full_random4,  
                       brm_full_cfixed1, brm_full_cfixed2, brm_full_cfixed3, brm_full_cfixed4,  
                       brm_full_tfixed1, brm_full_tfixed2, brm_full_tfixed3, brm_full_tfixed4))

## Robustness Checks ------------------------------------------------------------------------------- 
### Different Regime Measurements ------------------------------------------------------------------
#### V-Dem RoW Regime ------------------------------------------------------------------------------
##### Benchmark ------------------------------------------------------------------------------------
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


##### Table C3 Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions -------
#####          in Autocracies

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

##### Full models ----------------------------------------------------------------------------------

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

##### Table C5 Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions -------
#####          in Autocracies: RoW

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

#### Lexical Index (LIED) ----------------------------------------------------------------------------------
##### Benchmark ---------
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

##### Table C4 Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions -------
#####          in Autocracies: LIED from Skaaning et al. (2015)

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

##### Full models ----------------------------------------------------------

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

##### Table C6 Ruling Coalitions, Mass Party Organizations, and UniversalWelfare Provisions --------
#####       in Autocracies: LIED

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




## Different Thresholds for Alternative Predictor ---------------------------------------

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

saveRDS(robust_alt, "Analysis_data/robust_alt.RDS")

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

##### Table C7 Alternative Measurement of Ruling Coalitions, Mass Party Organizations, and ---------
#####          Universal Welfare Provisions in Autocracies

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

#### Figure C3 The Share of countries with mass-inclusive and non-mass groups in the ruling  -------
####           coalition, 1960-2020
baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) |> 
  drop_na(alt_mass_part_coalition, year) |> 
  mutate(
    coalition_alt = case_when(
      alt_mass_part_coalition %in% 1L ~ "Mass-inclusive coalition",
      alt_mass_part_coalition %in% 0L ~ "Non-mass coalition",
      T ~ NA_character_),
    coalition_alt = factor(coalition_alt,
                           levels = c("Non-mass coalition", "Mass-inclusive coalition"))) |> 
  group_by(coalition_alt, year) |> 
  count()  |> ungroup() |> 
  group_by(year) |> mutate(total = sum(n),
                           share = n/total) |> ungroup() |> 
  group_by(coalition_alt) |> 
  mutate(
    label = if_else(year == max(year), 
                    as.character(coalition_alt), NA_character_)) |> ungroup() -> figure1A_robust
figure1A_robust |> 
  dplyr::filter(coalition_alt %in% c("Non-mass coalition", "Mass-inclusive coalition")) |> 
  print(n = Inf)

figure1A_robust |> 
  ggplot(aes(x = year, y = share)) +
  geom_path(aes(color = coalition_alt, 
                fill = coalition_alt), show.legend = F, alpha = 1) +
  ggrepel::geom_label_repel(aes(x = year, label = label, color = coalition_alt), size = 5,
                            min.segment.length = Inf, 
                            nudge_y = 0.1,
                            na.rm = TRUE, show.legend = F) + 
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3])) +
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[1],
                               futurevisions::futurevisions("mars")[3])) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(seq(0, 1, 0.1))) +
  scale_x_continuous(breaks = c(seq(1900, 2020, 10))) +
  labs(y = "Share of countries with mass-inclusive and\nnon-mass groups in the ruling coalition\n", x = "\nYear",
       #title = "Social groups of Masses and Elites in Regime Coalitions, Globally. 1789-2020"
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

ggsave("Documents/2_Manuscript/2_Figures/figC3.pdf",
       width = 8.5, height = 5, dpi = "retina")


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

##### Figure C4 Distribution of the universalism index of V-Dem by mass-based coalition and --------
#####        elite-based coalition, 1960-2020

baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) |> 
  drop_na(year, mass_coalition) |>  ungroup() |> 
  mutate(
    mass_coalition = factor(mass_coalition,
                            levels = c("Institution-based",
                                       "Mass-based"),
                            labels = c("Elite-based coalition",
                                       "Mass-based coalition"))) -> figureC4A

summary_figC4A <- figureC4A |> group_by(mass_coalition) |> 
  summarize(mean = mean(universality, na.rm = T))

robust_alt |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) -> figureC4B

summary_figC4B <- figureC4B |> group_by(mass_inclusive_T050) |> 
  summarize(mean = mean(universality, na.rm = T)) |> drop_na()

bind_rows(summary_figC4A |> mutate(group = c(1, 2)) |> 
            rename(coalition = mass_coalition),
          summary_figC4B |> mutate(group = c(1, 2),
                                  mass_inclusive_T050 = factor(
                                    mass_inclusive_T050, levels = c(0, 1),
                                    labels = c("Non-mass coalition", "Mass-inclusive coalition"))) |> 
            rename(coalition = mass_inclusive_T050)) ->
  summary_figC4_comb

bind_rows(figureC4A |> dplyr::select(universality, coalition = mass_coalition) |> 
            mutate(group = case_when(
              coalition %in% "Elite-based coalition" ~ 1L,
              coalition %in% "Mass-based coalition" ~ 2L)),
          figureC4B |> dplyr::select(universality, coalition = coalition_alt) |> 
            mutate(group = case_when(
              coalition %in% "Non-mass coalition" ~ 1L,
              coalition %in% "Mass-inclusive coalition" ~ 2L))) ->
  figureC4_comb

figureC4_comb |> drop_na(group) |> 
  mutate(group = factor(group,
                        levels = c(1, 2),
                        labels = c("Elite-based coalition", "Mass-based coalition"))) |>
  ggplot(aes(x = universality, color = coalition)) +
  geom_density(aes(fill = coalition), show.legend = F, alpha = 0.6) +
  #geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(
    data = summary_figC4_comb |> 
      mutate(group = factor(group,
                            levels = c(1, 2),
                            labels = c("Elite-based coalition", "Mass-based coalition"))),
    aes(xintercept = mean,
        color = coalition), show.legend = F) +
geom_text(
  data = summary_figC4_comb |>
    mutate(group = factor(group,
                          levels = c(1, 2),
                          labels = c("Elite-based coalition", "Mass-based coalition"))),
  aes(x = -3.5,
      label = paste0("Mean of U.I. for\n", coalition, "\n= ", as.character(round(mean, 2)))),
  y = c(0.65,  0.65, 0.48, 0.48),
  color = "black",
  size = 4,
  lineheight = 0.7,
  # nudge_y = 0.5,
  # nudge_x = 0.5,
  hjust = 0,
  show.legend = F) +
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3],
                                "grey60", "#BECEDD")) +
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[1],
                               futurevisions::futurevisions("mars")[3],
                               "grey60", "#BECEDD")) +
  scale_y_continuous() +
  labs(y = NULL, x = "\nUniversalism index of V-Dem\n",
       caption = str_wrap("\nNote: The figure presents two panels comparing the distribution of welfare universalism in elite-based (left) and mass-based (right) coalitions. The solid areas represent the primary data for each type of coalition, while the lighter shaded areas show alternative measurements for non-mass and mass-inclusive coalitions, respectively. The value of zero indicates the point where welfare policies are equally divided between means-tested and universalistic approaches.",
                          100, exdent = 10)) +
  facet_wrap(~group) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        strip.text = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

ggsave("Documents/2_Manuscript/2_Figures/figC4.pdf", 
       width = 7, height = 5.5, dpi = "retina")


##### Figure C5 Threshold effects of mass-inclusive coalition on universal welfare provision -------
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


ggsave("Documents/2_Manuscript/2_Figures/FigC5.pdf",
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

##### Table C8 Alternative Measurement of Ruling Coalitions, Mass Party Organizations, and  -------
#####          Universal Welfare Provisions in Autocracies
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

## Lags -------------------------------------------------------------------------------------------

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

##### Figure C6 Temporal effects of ruling coalition and mass party organizations on universal -----
#####           welfare provision in autocracies

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

ggsave("Documents/2_Manuscript/2_Figures/FigC6.pdf",
       width = 8, height = 7, dpi = 1600)

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

##### Table C9 Ruling Coalitions, Mass Party Organizations, and UniversalWelfare Provisions --------
#####          in Autocracies: Control Respecifications
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
