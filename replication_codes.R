## Project: Why do authoritarian regimes provide welfare programs? -------------
##          Replication file for the figures   ---------------------------------
## Author:
##   - Sang-Hoon Park (UofSC)
## Date: 6th Apr. 2023

## As you do not have the datasets to merge, you cannot run following codes:

rm(list=ls())

## Import Packages to use ------------------------------------------------------
pacman::p_load(ggplot2, grid, pBrackets, ezpickr, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)
## Some of the packages should be installed via devtools::install_github()

## Import dataset to use -------------------------------------------------------
### Varieties of Democracy Database --------------------------------------------

Vdem <- vdemdata::vdem

Vdem_subset <- 
  Vdem %>% 
  dplyr::select(country_name, COWcode, year, e_lexical_index, v2x_polyarchy,
                v2xps_party, v2dlunivl, e_boix_regime, v2x_freexp_altinf,
                v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv, 
                v2dlencmps, v2regimpgroup, v2regsupgroupssize, v2x_clphy,
                e_cow_exports, e_cow_imports, e_gdp, e_gdppc, e_pop,
                e_total_fuel_income_pc, e_total_resources_income_pc, e_total_oil_income_pc,
                e_civil_war, v2x_corr, v2clrspct, 
                v2xme_altinf,
                v2smgovsmalt, v2smgovsmmon,v2smgovsmcenprc,
                v2x_genpp, v2x_cspart, v2regendtype, v2csantimv)

names(Vdem_subset) <-
  c("country_name", "COWcode", "year", "lexical_demo", 
    "edi", "party_inst", "universality", "regime_boix", "freedom", "party_org", "party_brch", "party_link",
    "party_platform", "party_cohesv", "public_goods", "coalition", "coalition_size", 
    "repression",
    "export", "import",
    "gdp", "gdppc", "pop", "fuel_income", "resource_income", "oil_income", "civilwar", "corruption",
    "clientalism", 
    "alternative",
    "govt_alternative", "govt_monitoring", "govt_censor", "women_polpar", "civil_par", "regime_endtype",
    "cso-anti-system_move")

### Welfare coverage and law ---------------------------------------------------
SPaW <- ezpickr::pick("Original_Data/SPaW_ver2.dta")

### Resources and Military size ------------------------------------------------
Miller <- ezpickr::pick("Original_Data/Miller2015.dta")

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

Analysis_data_noclean <- 
  Analysis_data_noclean %>% 
  dplyr::select(
    everything(), export, import, corruption, clientalism,
    women_polpar, civil_par
  )

foreign::write.dta(Analysis_data_noclean, "Analysis_Data/Analysis_data_noclean.dta")

### You can replicate from the following:

### Plot theme set
library(tidyverse)
ggthemr::ggthemr("fresh")

### Load the analysis data -----------------------------------------------------
Analysis_data_noclean <- ezpickr::pick("Analysis_data/Analysis_data_noclean.dta")

### Aggregate welfare variables ------------------------------------------------

Analysis_data_noclean <- 
  Analysis_data_noclean %>% mutate(
    totalunivers = univers_oldageprog + 
      univers_familiy_prog + univers_mater_prog + 
      univers_oldageprog + univers_sick_prog + 
      univers_unemp_prog + univers_working_prog)

### Manipulate the regime type variable using LIED -----------------------------

Analysis_data_noclean <- 
  Analysis_data_noclean %>% mutate(
    regime = case_when(
      lexical_demo <  6 ~ "Autocracies",
      lexical_demo == 6 ~ "Democracies"
    ) %>% parse_factor(., levels = c("Autocracies", "Democracies"),
                       include_na = F, ordered = T))

#### Regime type and Party Institutionalization Index --------------------------

Figure1_cutpoint <- Analysis_data_noclean %>% 
  group_by(regime) %>% drop_na() |> 
  dplyr::select(edi, party_inst) %>%
  dplyr::summarize(edi_per = 
                     quantile(edi, probs = c(0.5), na.rm = T),
                   party_inst_per = 
                     quantile(party_inst, probs = c(0.5), na.rm = T))
Figure1_cutpoint

Analysis_data_noclean %>% 
  drop_na(regime) %>% 
  group_by(regime) %>% count() %>% ungroup() -> regime_count

Analysis_data_noclean %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Democracies" & edi < 0.5) %>% 
  select(edi) %>% count() %>% rename(demo_under = n) -> demo_under

Analysis_data_noclean %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Autocracies" & edi > 0.5) %>% 
  select(edi) %>% count() %>% rename(auto_upper = n) -> auto_upper

regime_count[1, 3] <- auto_upper
regime_count[2, 3] <- demo_under

regime_count %>% rename(c(total_n = n, deviants = auto_upper)) %>%
  mutate(ratio = round(deviants/total_n, 2)) ->
  regime_count

library(futurevisions)
Figure1 <- Analysis_data_noclean %>% drop_na(regime) %>%
  ggplot(aes(y = edi, x = regime, color = regime)) + 
  geom_jitter(alpha=0.2, position=position_jitter(0.2), show.legend = FALSE) +   
  geom_boxplot(alpha=0.2, 
               position=position_dodge(0.8), width = 0.4, color = "black") +
  stat_summary(fun=mean, geom="point", shape=16,
               size=3, color="red") + 
  geom_hline(yintercept = 0.5, color = "red") + 
  labs(x = "", y = "Electoral Democracy Index") + 
  scale_x_discrete(labels=c("Autocracies" = paste0("Autocracies (n = ", regime_count[1, 2], ")"), 
                            "Democracies" = paste0("Democracies (n = ", regime_count[2, 2], ")"))) + 
  scale_color_manual(values = futurevisions("Mars")) + 
  annotate(geom = "rect", xmin = 0.75, xmax = 1.25,
           ymin = 0.5, ymax = 0.85, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 1, y = 0.9, label = paste0(regime_count[1, 4]*100, "% (", 
                                           regime_count[1, 3], ") of Autocracies")) +
  annotate(geom = "rect", xmin = 1.75, xmax = 2.25,
           ymin = 0.1, ymax = 0.5, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 2, y = 0.05, label = paste0(regime_count[2, 4]*100, "% (", 
                                            regime_count[2, 3], ") of Democracies")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 3))
Figure1

#### Save Figure 1. The distribution of democracies and autocracies ------------
ggsave("Documents/2_Manuscript/2_Figures/Figure1.pdf",
       width = 8.5, height = 4)

### Supporting Groups ----------------------------------------------------------
#### All regimes ---------------------------------------------------------------

Analysis_allregime <- Analysis_data_noclean %>% mutate(
  class_raw = case_when(
    coalition == 2L ~ "Party elites",
    coalition == 5L ~ "The military",
    coalition == 6L ~ "The ethnic/racial groups",
    coalition == 7L ~ "The religous groups",
    coalition == 1L ~ "Agrarian elites",
    coalition == 8L ~ "Local elites",
    coalition == 3L ~ "Business elites",
    coalition == 4L ~ "Civil servants",
    coalition == 9L ~ "Urban Working",
    coalition == 11L ~ "Rural Working",
    coalition == 10L ~ "Urban Middle",
    coalition == 12L ~ "Rural Middle",
    coalition == 0L ~ "The aristocracy",
    coalition == 13L ~ "A foreign govt or colonial",
    coalition == NA ~ NA_character_,
    T ~ NA_character_
  ) %>% 
    parse_factor(., levels = c("Party elites",
                               "The military",
                               "The ethnic/racial groups",
                               "The religous groups",
                               "Agrarian elites",
                               "Local elites",
                               "Business elites",
                               "Civil servants",
                               "Urban Working",
                               "Rural Working",
                               "Urban Middle",
                               "Rural Middle","The aristocracy",
                               "A foreign govt or colonial"), include_na = F, ordered = T),
  class_raw.nu = as.numeric(class_raw)
)
table(Analysis_allregime$class_raw)

Analysis_allregime <- Analysis_allregime %>% mutate(
  class = case_when(
    coalition == 2L ~ "Party elites",
    coalition == 5L ~ "The military",
    coalition == 6L ~ "The ethnic/racial/religous groups",
    coalition == 7L ~ "The ethnic/racial/religous groups",
    coalition == 1L ~ "Agrarian/local elites",
    coalition == 8L ~ "Agrarian/local elites",
    coalition == 3L ~ "Business elites/civil servants",
    coalition == 4L ~ "Business elites/civil servants",
    coalition == 9L ~ "Urban Working",
    coalition == 11L ~ "Rural Working",
    coalition == 10L ~ "Urban Middle",
    coalition == 12L ~ "Rural Middle",
    coalition == 0L ~ "The aristocracy",
    coalition == 13L ~ "A foreign govt. or colonial power",
    T ~ NA_character_
  ) %>% 
    parse_factor(., levels = c("Party elites",
                               "The military",
                               "The ethnic/racial/religous groups",
                               "Agrarian/local elites",
                               "Business elites/civil servants",
                               "Urban Working",
                               "Rural Working",
                               "Urban Middle",
                               "Rural Middle",
                               "The aristocracy",
                               "A foreign govt. or colonial power"), 
                 include_na = F, ordered = T),
  class.nu = as.numeric(class)
) %>% drop_na(class)

Analysis_allregime <- Analysis_allregime %>% mutate(
  inst_mass = case_when(
    class == "Party elites" ~ "Institution-based",
    class == "The military" ~ "Institution-based",
    class == "Urban Working" ~ "Mass-based",
    class == "Rural Working" ~ "Mass-based",
    class == "Urban Middle"  ~ "Mass-based",
    class == "Rural Middle"  ~ "Mass-based",
    T ~ NA_character_
  ),
  inst_mass = factor(inst_mass, levels = c("Mass-based", "Institution-based")))

table(Analysis_allregime$inst_mass)

Analysis_allregime <- Analysis_allregime %>% 
  dplyr::select(
    country_name, year, lexical_demo, regime_boix, edi, party_inst, repression, freedom,
    party_org, party_brch, party_link, party_platform, party_cohesv,
    universality, totalunivers, coalition, coalition_size,
    contains("govt"), alternative,
    class, resdep2, urban, cow_milsize, gdp, gdppc, pop, civilwar,
    everything())

### Three samples by regime ----------------------------------------------------
Analysis_allregime
Analysis_allregime %>% dplyr::filter(regime %in% "Autocracies") -> Analysis_auto
Analysis_allregime %>% dplyr::filter(regime %in% "Democracies") -> Analysis_demo

### Yearly Trends of Essential Class Coalitions in Autocracies -----------------
#### Yearly frequencies --------------------------------------------------------
class_year_auto <- Analysis_auto %>% 
  select(inst_mass, year, COWcode, country_name) %>%
  group_by(inst_mass, year) %>% summarize(freq = n())

#### Path Plot -----------------------------------------------------------------
class_year_auto %>% drop_na(inst_mass) |> 
  mutate(
    label = if_else(year == max(year), 
                    as.character(inst_mass), NA_character_)) %>%
  ggplot(aes(x = year, y = freq, color = inst_mass)) + 
  geom_path(show.legend = F) +
  scale_x_continuous(
    limits = c(1880, 2020),
    breaks = c(seq(1880, 2020, 20), 2020)) + 
  #  facet_wrap(~class) + 
  ggrepel::geom_label_repel(aes(label = label), size = 5,
                            min.segment.length = Inf, 
                            nudge_y = -3.5, na.rm = TRUE, show.legend = F) + 
  # geom_vline(xintercept = 1991, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  scale_y_continuous(breaks = c(seq(0, 80, 10)),
                     limits = c(0, 60)) +
  labs(y = "Frequency", x = "Year") + 
  # annotate("segment", x = 1923, xend = 1917, y = 28, yend = 30, 
  #          color = "red", size=0.3, arrow=arrow(length = unit(0.02, "npc"))) + 
  # geom_text(aes(x=1993, y=20), 
  #          label="Year of 1991, the USSR collapsed", 
  #          color = "red", size = 4) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

###### Save Figure 2. The numbers of class coalitions across the time ----------
ggsave("Documents/2_Manuscript/2_Figures/MPSA23_Figure2.pdf", 
       width = 8.5, height = 3.5, dpi = "retina")

### Yearly Trends of Essential Class Coalitions in Democracies -----------------
#### Yearly frequencies --------------------------------------------------------
class_year_demo <- Analysis_demo %>% 
  select(inst_mass, year, COWcode, country_name) %>%
  group_by(inst_mass, year) %>% summarize(freq = n())

#### Path Plot -----------------------------------------------------------------
class_year_demo %>% drop_na(inst_mass) |> 
  mutate(
    label = if_else(year == max(year), 
                    as.character(inst_mass), NA_character_)) %>%
  ggplot(aes(x = year, y = freq, color = inst_mass)) + 
  geom_path(show.legend = F) +
  scale_x_continuous(
    limits = c(1910, 2020),
    breaks = c(seq(1910, 2020, 20), 2020)) + 
  #  facet_wrap(~class) + 
  ggrepel::geom_label_repel(aes(label = label), size = 2.5,
                            nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  # geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))

###### Save Figure 2A. The numbers of class coalitions across the time ----------
ggsave("Documents/2_Manuscript/2_Figures/Appendix/MPSA23_Appendix_Figure2A.pdf", 
       width = 8.5, height = 5, dpi = "retina")

### Yearly Trends of Essential Class Coalitions by Regimes ---------------------
#### Yearly frequencies --------------------------------------------------------
class_year <- Analysis_allregime %>% 
  select(inst_mass, year, COWcode, country_name, regime) %>%
  group_by(inst_mass, regime, year) %>% summarize(freq = n())

#### Path Plot -----------------------------------------------------------------
class_year %>% 
  mutate(
    label = if_else(year == max(year), 
                    as.character(inst_mass), NA_character_)) %>%
  drop_na(regime) %>%
  drop_na(inst_mass) |> 
  ggplot(aes(x = year, y = freq, color = inst_mass)) + 
  geom_path() +
  scale_x_continuous(
    limits = c(1910, 2020),
    breaks = c(seq(1910, 2020, 20), 2020)) + 
  facet_wrap(~regime) + 
  ggrepel::geom_label_repel(aes(label = label), size = 2.5,
                            nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  # geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))

###### Save Figure 2B. The numbers of class coalitions across the time by regime ----------
ggsave("Documents/2_Manuscript/2_Figures/Appendix/Appendix_Figure2B.pdf", 
       width = 13, height = 5, dpi = "retina")

### Bivariate relationship between Welfare and Classes -------------------------
##### Essential class summaries ------------------------------------------------

class_labels <-  c("Party elites", "The military",
                   "Urban Working", "Urban Middle")
class_summary_allregime <- Analysis_allregime %>% 
  select(inst_mass, universality, coalition_size, year) %>%
  group_by(inst_mass) %>% drop_na(universality) |> 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
class_summary_allregime

class_summary_auto <- Analysis_auto %>% 
  select(inst_mass, universality, coalition_size, year) %>%
  group_by(inst_mass) %>% drop_na(inst_mass) |> 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
class_summary_auto

class_summary_demo <- Analysis_demo %>% 
  select(inst_mass, universality, coalition_size, year) %>%
  group_by(inst_mass) %>% drop_na(inst_mass) |> 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
class_summary_demo

##### Other class summaries ----------------------------------------------------

others_labels <- c("The ethnic/racial/religous groups",
                   "Agrarian/local elites",
                   "Business elites/civil servants", 
                   "The aristocracy",
                   "A foreign govt. or colonial power")

other_summary_allregime <- Analysis_allregime %>% 
  select(class, universality, coalition_size, year) %>%
  dplyr::filter(class %in% others_labels) %>%
  group_by(class) %>% 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
other_summary_allregime

other_summary_auto <- Analysis_auto %>% 
  select(class, universality, coalition_size, year) %>%
  dplyr::filter(class %in% others_labels) %>%
  group_by(class) %>% 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
other_summary_auto

other_summary_demo <- Analysis_demo %>% 
  select(class, universality, coalition_size, year) %>%
  dplyr::filter(class %in% others_labels) %>%
  group_by(class) %>% 
  summarise(mean.w2 = mean(universality, na.rm = T),
            sd.w2 = sd(universality, na.rm = T),
            lower.w2 = mean(universality, na.rm = T) - sd(universality, na.rm = T),
            upper.w2 = mean(universality, na.rm = T) + sd(universality, na.rm = T))
other_summary_demo

#### Figure 3 (Universality between Autocracy and Essential class coalition) -----------------------

Analysis_auto %>% 
  dplyr::filter(year > 1879) %>% 
  drop_na(inst_mass) |> 
  ggplot(aes(x = universality)) + 
  geom_density(aes(x = universality, color = inst_mass, fill = inst_mass), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(inst_mass)) + 
  geom_vline(
    data = class_summary_auto %>% 
      dplyr::filter(inst_mass %in% "Mass-based"), 
    aes(xintercept=mean.w2), color= futurevisions("mars")[1]) +
  geom_text(
    data = class_summary_auto %>% 
      dplyr::filter(inst_mass %in% "Mass-based"),
    map = aes(x = as.numeric(class_summary_auto[1, 2]) - 1.5, y = 0.3), size = 4,
    color = futurevisions("mars")[1],
    label = paste0("Mean U.I. = ", round(class_summary_auto[1, 2], 3))) +
  geom_vline(
    data= class_summary_auto %>% 
      dplyr::filter(inst_mass %in% "Institution-based"), 
    aes(xintercept=mean.w2), color= futurevisions("mars")[3]) + 
  geom_text(
    data= class_summary_auto %>% 
      dplyr::filter(inst_mass %in% "Institution-based"), 
    map = aes(x =  as.numeric(class_summary_auto[2, 2]) - 1.5, y = 0.3), size = 4,
    color = futurevisions("mars")[3],
    label = paste0("Mean U.I. = ", round(class_summary_auto[2, 2], 3))) + 
  scale_color_manual(values = c(futurevisions("mars")[1],
                                futurevisions("mars")[3]))+ 
  scale_fill_manual(values = c(futurevisions("mars")[1],
                               futurevisions("mars")[3]))+ 
  labs(x = "Universalism Index of V-Dem",
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

#### Save Figure 3. The Distribution of Universalism index by Class ------------
####                                      Coalitions in Autocracies
ggsave("Documents/2_Manuscript/2_Figures/MPSA23_Figure3.pdf", 
       width = 8.5, height = 4, dpi = "retina")
