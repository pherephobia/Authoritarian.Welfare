## Project: Why do authoritarian regimes provide welfare programs? -------------
##          2. Model specification  --------------------------------------------
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
theme_set(theme_clean())

## Data Imports: Baseline ------------------------------------------------------
baseline <- readRDS("Analysis_data/baseline.RDS")

## Model Specification ---------------------------------------------------------
### Base: Without Ctrls and fixed effects --------------------------------------
### Base: Model 1: 1900-2021 / Model 2: 1966-2020 / 
### No. of country and year check
baseline |> dplyr::select(universality, l_mass_coalition, l_massparty, COWcode, year, regime) |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> drop_na() |> pull(COWcode) |> 
  unique() |> length()

list(glm(universality ~ l_mass_coalition, data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_massparty, data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition + l_massparty, data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition*l_massparty, data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)"))
     )|> texreg::screenreg(
       custom.coef.names = c(
         "(Intercept)",
         "Mass-based coalition (vs. Elite-based)",
         "Mass party organization",
         "Mass-based Coalition$\\times$Mass party organization"),
       reorder.coef = c(2, 3, 4, 1),
       custom.gof.rows = 
         list("Country-fixed" = c("No", "No", "No", "No"),
              "Year-fixed"    = c("No", "No", "No", "No"),
              "No. of countries" = c(
                "141", "110", "99", "99"),
              "Year coverage" = c("1900-2021", "1966-2020", "1966-2020", "1966-2020")))

### Base: Without Ctrls but fixed effects --------------------------------------
#### Base: Country-fixed -------------------------------------------------------------

list(glm(universality ~ l_mass_coalition + as.factor(COWcode), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_massparty + as.factor(COWcode), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition + l_massparty + as.factor(COWcode), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition*l_massparty + as.factor(COWcode), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)"))
)|> texreg::screenreg(
  omit.coef = "as.factor",
  custom.coef.names = c(
    "(Intercept)",
    "Mass-based coalition (vs. Elite-based)",
    "Mass party organization",
    "Mass-based Coalition$\\times$Mass party organization"),
  reorder.coef = c(2, 3, 4, 1),
  custom.gof.rows = 
    list("Country-fixed" = c("Yes", "Yes", "Yes", "Yes"),
         "Year-fixed"    = c("No", "No", "No", "No"),
         "No. of countries" = c(
           "141", "110", "99", "99"),
         "Year coverage" = c("1900-2021", "1966-2020", "1966-2020", "1966-2020")))

#### Base: Country-fixed + Year-fixed ---------------------------------------------

list(glm(universality ~ l_mass_coalition + 
           as.factor(COWcode) + as.factor(year), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_massparty + 
           as.factor(COWcode) + as.factor(year), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition + l_massparty + 
           as.factor(COWcode) + as.factor(year), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition*l_massparty + 
           as.factor(COWcode) + as.factor(year), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)"))
)|> texreg::screenreg(
  omit.coef = "as.factor",
  custom.coef.names = c(
    "(Intercept)",
    "Mass-based coalition (vs. Elite-based)",
    "Mass party organization",
    "Mass-based Coalition$\\times$Mass party organization"),
  reorder.coef = c(2, 3, 4, 1),
  custom.gof.rows = 
    list("Country-fixed" = c("Yes", "Yes", "Yes", "Yes"),
         "Year-fixed"    = c("Yes", "Yes", "Yes", "Yes"),
         "No. of countries" = c(
           "141", "110", "99", "99"),
         "Year coverage" = c("1900-2021", "1966-2020", "1966-2020", "1966-2020"))) 

#### Base: Country-fixed  + Year-trend ------------------------------------------------------

list(glm(universality ~ l_mass_coalition + 
           as.factor(COWcode) + year + I(year^2) + I(year^3), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_massparty + 
           as.factor(COWcode) + year + I(year^2) + I(year^3), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition + l_massparty + 
           as.factor(COWcode) + year + I(year^2) + I(year^3), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
     glm(universality ~ l_mass_coalition*l_massparty + 
           as.factor(COWcode) + year + I(year^2) + I(year^3), 
         data = baseline |> 
           dplyr::filter(regime %in% "Autocracies (LIED < 6)"))
)|> texreg::screenreg(
  omit.coef = "as.factor",
  custom.coef.names = c(
    "(Intercept)",
    "Mass-based coalition (vs. Elite-based)",
    "Year", "Year$^2$", "Year$^3$",
    "Mass party organization",
    "Mass-based Coalition$\\times$Mass party organization"),
  reorder.coef = c(2, 6, 7, 3, 4, 5, 1),
  custom.gof.rows = 
    list("Country-fixed" = c("Yes", "Yes", "Yes", "Yes"),
         "Year-fixed"    = c("Yes", "Yes", "Yes", "Yes"),
         "No. of countries" = c(
           "141", "110", "99", "99"),
         "Year coverage" = c("1900-2021", "1966-2020", "1966-2020", "1966-2020")))

### Full: With Ctrls but not fixed effects ----------------------------------------
baseline |> 
  dplyr::select(
    COWcode, year, universality, 
    l_mass_coalition,
    l_massparty,
    l_gdppc, l_gdpgrth,
    l_lnresource, l_civilwar, l_repression, l_capacity, l_CSOconsult, l_CSOpart,
    l_royal, l_party, l_mil, COWcode, year, regime) |> 
  dplyr::filter(regime %in% "Autocracies (LIED < 6)") |> drop_na() |> pull(year) |> 
  summary()
  
  glm(
    universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(vdem_regime %in% c(1, 2))) |> 
    texreg::screenreg(omit.coef = "as.factor")

  
    
  
list(
  glm(
    universality ~ l_mass_coalition + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
  glm(
    universality ~ l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
  glm(
    universality ~ l_mass_coalition + l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)")),
  glm(
    universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), data = baseline |> 
      dplyr::filter(regime %in% "Autocracies (LIED < 6)"))) |> 
  texreg::screenreg(omit.coef = "as.factor",
                    single.row = F,
                    custom.coef.names = c(
                      "(Intercept)",
                      "Mass-based coalition (vs. Elite-based)",
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
                      "Mass party organization",
                      "Mass-based Coalition$\\times$Mass party organization"),
                    reorder.coef = c(2, 14, 15, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1),
                    custom.gof.rows = 
                      list("Country-fixed" = c("YES", "YES", "YES", "YES"),
                           "Year-fixed" = c("YES", "YES", "YES", "YES"),
                           "No. of countries" = c("97", "93", "85", "85"),
                           "Year coverage" = c("1961-2011", "1966-2011", "1966-2011", "1966-2011")))

glm(
  universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_capacity + l_CSOconsult + l_CSOpart +
    l_royal + l_party + l_mil +
    #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))
    ) -> full_model

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
    l_lnresource, l_civilwar, l_repression, l_capacity, l_CSOconsult, l_CSOpart,
    l_royal, l_party, l_mil, COWcode, year, vdem_regime) |> 
  dplyr::filter(vdem_regime %in% c(0, 1)) |> drop_na() |> pull(COWcode) |> 
  unique() -> unique_COWfull

jk_df <- data.frame()

for (i in 1:length(unique_COWfull)) {
  model <- glm(
    universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
      l_lnresource + l_civilwar + l_repression + l_capacity + l_CSOconsult + l_CSOpart +
      l_royal + l_party + l_mil +
      #l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
      as.factor(COWcode) + as.factor(year), 
    data = baseline |> 
      dplyr::filter(vdem_regime %in% c(0, 1) & !COWcode %in% unique_COWfull[1]))
  
  coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
  temp_df <- data.frame(
    term = c("Mass Coalition", "Mass Party Org.", "Mass Coalition X Mass Party Org."),
    median = c(median(coef[, "l_mass_coalitionMass-based"], na.rm = T), 
               median(coef[, "l_massparty"], na.rm = T), 
               median(coef[, "l_mass_coalitionMass-based:l_massparty"], na.rm = T)),
    ll = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.025), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.025), 
           quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                    na.rm = T, probs = 0.025)),
    ul = c(quantile(coef[, "l_mass_coalitionMass-based"], na.rm = T, probs = 0.975), 
           quantile(coef[, "l_massparty"], na.rm = T, probs = 0.975), 
           quantile(coef[, "l_mass_coalitionMass-based:l_massparty"], 
                    na.rm = T, probs = 0.975))
  ) |> 
    mutate(Exclude = paste0("County ", unique_COWfull[i], " excluded"),
           id = paste0(unique_COWfull[i]))
  jk_df <- jk_df |> bind_rows(temp_df)
}

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



glm(
  universality ~ l_personindex + log(l_gdppc+1) + l_gdpgrth + 
    l_lnresource + l_civilwar + l_capacity + l_CSOconsult + l_CSOpart +
    #l_royal + l_party + l_mil +
    l_hereditaryindex + l_militaryindex + l_partyindex + #l_personindex +
    as.factor(COWcode) + as.factor(year), 
  data = baseline |> 
    #dplyr::filter(brm %in% "Autocracies (BRM)")
    dplyr::filter(vdem_regime %in% c(0, 1)) |> 
    mutate(l_mass_coalition = as.numeric(l_mass_coalition))) |> texreg::screenreg(omit.coef = "as.factor")
)
ggsave("Documents/2_Manuscript/2_Figures/fig3_spsa.pdf", width = 7, height = 4.5, dpi = "retina")