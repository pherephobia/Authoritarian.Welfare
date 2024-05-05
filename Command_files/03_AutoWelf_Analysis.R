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

## Data Imports: Baseline & Robust ---------------------------------------------
baseline <- readRDS("Analysis_data/baseline.RDS")
robust_alt <- readRDS("Analysis_data/robust_alt.RDS")

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
  #output = "latex"
  output = "gt"
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
                           gof_omit = 'DF|Deviance|R2|adjR2', 
                           #output = 'latex',
                           output = "gt")

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

### Marginal Effects -------------------------------------------------------------------------------         


beta_sim <- MASS::mvrnorm(4000, mu = coef(brm_full_tfixed4), Sigma = vcov(brm_full_tfixed4))
length(beta_sim[1, ])
beta_me <- beta_sim[, c(1, 14)]

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

#### Jackknife -----------------------------------------------------------------------
##### Make a country sets ------
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

#### Make a Jackknife ME plot -------------------------------------------------

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

jk_full4 |> bind_rows(f4main_df) ->
  jk_full4_final

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

