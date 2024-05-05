## Project: Why do authoritarian regimes provide welfare programs? -------------
##          2. Variable Manipulations   ----------------------------------------
## Author:
##   - Sang-Hoon Park (UofSC)
## Date: 6th Apr. 2023

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
  # annotate("segment", x = 1923, xend = 1917, y = 28, yend = 30, 
  #          color = "red", size=0.3, arrow=arrow(length = unit(0.02, "npc"))) + 
  # annotate("text", x=1924, y=27.5, 
  #          label="Year of 1917 the Russian Revolution", 
  #          color = "red", size = 2.5) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
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
  # annotate("segment", x = 1923, xend = 1917, y = 28, yend = 30, 
  #          color = "red", size=0.3, arrow=arrow(length = unit(0.02, "npc"))) + 
  # annotate("text", x=1924, y=27.5, 
  #          label="Year of 1917 the Russian Revolution", 
  #          color = "red", size = 2.5) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
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

# 
# 
# #### Figure 3A (Democracy and Essential class coalition) -----------------------
# 
# Analysis_demo %>% 
#   dplyr::filter(class %in% class_labels & year > 1916) %>% 
#   ggplot(aes(x = totalunivers)) + 
#   geom_density(aes(x = totalunivers, fill = class), 
#                alpha = 0.2, size = 0.2, show.legend = F) + 
#   facet_wrap(~as.factor(class)) + 
#   geom_vline(
#     data = class_summary_demo %>% 
#       dplyr::filter(class %in% "Party elites"), 
#     aes(xintercept=mean.w1), color="black") +
#   geom_text(
#     data = class_summary_demo %>% 
#       dplyr::filter(class %in% "Party elites"),
#     map = aes(x = 10, y = 0.05), size = 3,
#     label = paste0("Mean U.I. = ", round(class_summary_demo[1, 2], 1))) +
#   geom_vline(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "The military"), 
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "The military"), 
#     map = aes(x = 10, y = 0.05), size = 3,
#     label = paste0("Mean U.I. = ", round(class_summary_demo[2, 2], 1))) + 
#   geom_vline(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "Urban Working"), 
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "Urban Working"), 
#     map = aes(x = 10, y = 0.05), size = 3,
#     label = paste0("Mean U.I. = ",round(class_summary_demo[3, 2], 1))) + 
#   geom_vline(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "Urban Middle"), 
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data= class_summary_demo %>% 
#       dplyr::filter(class %in% "Urban Middle"), 
#     map = aes(x = 10, y = 0.05), size = 3,
#     label = paste0("Mean U.I. = ", round(class_summary_demo[4, 2], 1))) + 
#   scale_fill_manual(values = futurevisions("mars")) + 
#   labs(x = "Universalism Index of SPaW",
#        y = "Density") +
#   scale_y_continuous(labels = scales::percent_format())
# 
# class_summary_merged <- 
#   bind_rows(class_summary_auto %>% mutate(regime = "Autocracies"),
#             class_summary_demo %>% mutate(regime = "Democracies"))
# 
# #### Figure 3B (Regime type and Essential class coalition) ---------------------
# 
# Analysis_allregime %>% 
#   dplyr::filter(class %in% class_labels & year > 1916) %>% 
#   drop_na(regime) %>%
#   ggplot(aes(x = totalunivers)) + 
#   geom_density(aes(x = totalunivers, fill = regime), 
#                alpha = 0.2, size = 0.2, show.legend = T) + 
#   facet_wrap(~as.factor(class), ncol = 2) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Party elites" & regime %in% "Autocracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Party elites" & regime %in% "Autocracies"),
#     map = aes(x = as.numeric(class_summary_merged[1, 2]) - 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Autocracy)\n= ", round(class_summary_merged[1, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "The military" & regime %in% "Autocracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[1]) + 
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "The military" & regime %in% "Autocracies"),
#     map = aes(x = as.numeric(class_summary_merged[2, 2]) - 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Autocracy)\n= ", round(class_summary_merged[2, 2], 1)),
#     color = futurevisions("mars")[1]) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Working" & regime %in% "Autocracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[1]) + 
#   geom_text(
#     data= class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Working"), 
#     map = aes(x = as.numeric(class_summary_merged[3, 2]) - 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Autocracy)\n= ",round(class_summary_merged[3, 2], 1)),
#     color = futurevisions("mars")[1]) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Middle" & regime %in% "Autocracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[1]) + 
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Middle" & regime %in% "Autocracies"),
#     map = aes(x = as.numeric(class_summary_merged[4, 2] - 9), y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Autocracy)\n= ", round(class_summary_merged[4, 2], 1)),
#     color = futurevisions("mars")[1]) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Party elites" & regime %in% "Democracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Party elites" & regime %in% "Democracies"),
#     map = aes(x = as.numeric(class_summary_merged[5, 2]) + 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Democracy)\n= ", round(class_summary_merged[5, 2], 1)),
#     color=futurevisions("mars")[3]) +
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "The military" & regime %in% "Democracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[3]) + 
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "The military" & regime %in% "Democracies"),
#     map = aes(x = as.numeric(class_summary_merged[6, 2]) + 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. = (Democracy)\n", round(class_summary_merged[6, 2], 1)),
#     color=futurevisions("mars")[3]) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Working" & regime %in% "Democracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[3]) + 
#   geom_text(
#     data= class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Working"), 
#     map = aes(x = as.numeric(class_summary_merged[7, 2]) + 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. = (Democracy)\n",round(class_summary_merged[7, 2], 1)),
#     color=futurevisions("mars")[3]) + 
#   geom_vline(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Middle" & regime %in% "Democracies"),
#     aes(xintercept=mean.w1), color=futurevisions("mars")[3]) + 
#   geom_text(
#     data = class_summary_merged %>% 
#       dplyr::filter(class %in% "Urban Middle" & regime %in% "Democracies"),
#     map = aes(x = as.numeric(class_summary_merged[8, 2]) + 9, y = 0.05), size = 4,
#     label = paste0("Mean U.I. = (Democracy)\n", round(class_summary_merged[8, 2], 1)),
#     color=futurevisions("mars")[3]) + 
#   scale_fill_manual(values = c(futurevisions("mars")[1],
#                                futurevisions("mars")[3])) + 
#   labs(x = "Universalism Index of SPaW",
#        y = NULL,
#        caption = "Note: The mean values for the SPaW's Universalalism Index are rounded to two decimal places.") +
#   scale_y_continuous(labels = scales::percent_format()) + 
#   theme(legend.title = element_blank(),
#         legend.position = "bottom")
# 
# #### Figure 3C (Autocracy and Other class coalition) ------------------------
# 
# Analysis_auto %>% 
#   dplyr::filter(class %in% others_labels & year > 1916) %>% 
#   ggplot(aes(x = totalunivers)) + 
#   geom_density(aes(x = totalunivers, fill = class), 
#                alpha = 0.2, size = 0.2, show.legend = F) + 
#   facet_wrap(~as.factor(class)) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "The ethnic/racial/religous groups"), 
#     aes(xintercept = mean.w1), color="black") +
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "The ethnic/racial/religous groups"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[1, 2], 2))) +
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Agrarian/local elites"), 
#              aes(xintercept = mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Agrarian/local elites"), 
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[2, 2], 2))) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Business elites/civil servants"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Business elites/civil servants"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[3, 2], 2))) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Rural Middle"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Rural Middle"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[4, 2], 2))) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Rural Working"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "Rural Working"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[5, 2], 2))) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "The aristocracy"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "The aristocracy"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[6, 2], 2))) + 
#   geom_vline(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "A foreign govt. or colonial power"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_auto %>% 
#       dplyr::filter(class %in% "A foreign govt. or colonial power"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_auto[7, 2], 2))) + 
#   viridis::scale_fill_viridis(option = "Magma", discrete = T,
#                               begin = 0.2, end = 0.9) + 
#   labs(x = "Universalism Index of SPaW",
#        y = NULL) +
#   scale_y_continuous(labels = scales::percent_format())
# 
# ###### Save Figure 3C. The Distribution of Universalism index by ---------------
# ####                   Other Class Coalitions in Autocracies
# ggsave("Documents/2_Manuscript/2_Figures/Appendix/Appendix_Figure3C.pdf", 
#        width = 8.5, height = 7, dpi = "retina")



#### Figure 3D (Democracy and Essential class coalition) -----------------------
# 
# Analysis_demo %>% 
#   dplyr::filter(class %in% others_labels & year > 1916) %>% 
#   ggplot(aes(x = totalunivers)) + 
#   geom_density(aes(x = totalunivers, fill = class), 
#                alpha = 0.2, size = 0.2, show.legend = F) + 
#   facet_wrap(~as.factor(class)) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "The ethnic/racial/religous groups"), 
#     aes(xintercept = mean.w1), color="black") +
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "The ethnic/racial/religous groups"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[1, 2], 2))) +
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Agrarian/local elites"), 
#     aes(xintercept = mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Agrarian/local elites"), 
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[2, 2], 2))) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Business elites/civil servants"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Business elites/civil servants"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[3, 2], 2))) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Rural Middle"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Rural Middle"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[4, 2], 2))) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Rural Working"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "Rural Working"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[5, 2], 2))) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "The aristocracy"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "The aristocracy"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[6, 2], 2))) + 
#   geom_vline(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "A foreign govt. or colonial power"),
#     aes(xintercept=mean.w1), color="black") + 
#   geom_text(
#     data = other_summary_demo %>% 
#       dplyr::filter(class %in% "A foreign govt. or colonial power"),
#     map = aes(x = 25, y = 0.075), size = 3,
#     label = paste0("Mean U.I. = ", round(other_summary_demo[7, 2], 2))) + 
#   viridis::scale_fill_viridis(option = "Magma", discrete = T,
#                               begin = 0.2, end = 0.9) + 
#   labs(x = "Universalism Index of SPaW",
#        y = NULL) +
#   scale_y_continuous(labels = scales::percent_format())

###### Save Figure 3D. The Distribution of Universalism index by ---------------
####                   Other Class Coalitions in democracies
# ggsave("Documents/2_Manuscript/2_Figures/Appendix/Appendix_Figure3D.pdf", 
#        width = 8.5, height = 7, dpi = "retina")

#### Figure 3E (Regime type and Essential class coalition) ---------------------
# 
# other_summary_merged <- 
#   bind_rows(other_summary_auto %>% mutate(regime = "Autocracies"),
#             other_summary_demo %>% mutate(regime = "Democracies"))
# other_summary_merged$class[1]
# 
# Analysis_allregime %>% 
#   dplyr::filter(class %in% others_labels & year > 1916) %>% 
#   drop_na(regime) %>%
#   ggplot(aes(x = totalunivers)) + 
#   geom_density(aes(x = totalunivers, fill = regime), 
#                alpha = 0.1, size = 0.1, show.legend = T) + 
#   facet_wrap(~as.factor(class), scales = "free_y", ncol = 2) + 
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[1] & 
#                       regime %in% other_summary_merged$regime[1]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[1] & 
#                       regime %in% other_summary_merged$regime[1]),
#     map = aes(x = 25, y = 0.02), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[1, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[2] & 
#                       regime %in% other_summary_merged$regime[2]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[2] & 
#                       regime %in% other_summary_merged$regime[2]),
#     map = aes(x = 25, y = 0.02), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[2, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[3] & 
#                       regime %in% other_summary_merged$regime[3]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[3] & 
#                       regime %in% other_summary_merged$regime[3]),
#     map = aes(x = 25, y = 0.02), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[3, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[4] & 
#                       regime %in% other_summary_merged$regime[4]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[4] & 
#                       regime %in% other_summary_merged$regime[4]),
#     map = aes(x = 25, y = 0.02), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[4, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[5] & 
#                       regime %in% other_summary_merged$regime[5]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[5] & 
#                       regime %in% other_summary_merged$regime[5]),
#     map = aes(x = 25, y = 0.05), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[5, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[6] & 
#                       regime %in% other_summary_merged$regime[6]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[6] & 
#                       regime %in% other_summary_merged$regime[6]),
#     map = aes(x = 25, y = 0.1), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[6, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[7] & 
#                       regime %in% other_summary_merged$regime[7]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[1]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[7] & 
#                       regime %in% other_summary_merged$regime[7]),
#     map = aes(x = 30, y = 0.45), size = 4,
#     label = paste0("Mean U.I. (Autocracy) = ", round(other_summary_merged[7, 2], 1)),
#     color = futurevisions("mars")[1]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[1] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[1] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.03), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[8, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[2] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[2] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.03), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[9, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[3] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[3] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.03), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[10, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[4] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[4] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.03), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[11, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[5] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[5] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.07), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[12, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[6] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[6] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(x = 25, y = 0.15), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[13, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   geom_vline(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[7] & 
#                       regime %in% other_summary_merged$regime[8]),
#     aes(xintercept = mean.w1), color=futurevisions("mars")[3]) +
#   geom_text(
#     data = other_summary_merged %>% 
#       dplyr::filter(class %in% other_summary_merged$class[7] & 
#                       regime %in% other_summary_merged$regime[8]),
#     map = aes(30, y = 0.6), size = 4,
#     label = paste0("Mean U.I. (Democracy) = ", round(other_summary_merged[14, 2], 1)),
#     color = futurevisions("mars")[3]) +
#   scale_fill_manual(values = c(futurevisions("mars")[1],
#                                futurevisions("mars")[3])) + 
#   scale_x_continuous(limits = c(0, 50)) + 
#   labs(x = "Universalism Index of SPaW",
#        y = NULL,
#        caption = "Note: The mean values for the SPaW's Universalalism Index are rounded to two decimal places.") +
#   scale_y_continuous(labels = scales::percent_format()) + 
#   theme(legend.title = element_blank(),
#         legend.position = "bottom")
# 
# ###### Save Figure 3E. The Distribution of Universalism index by ---------------
# ####                   Regime type and other Class Coalitions
# ggsave("Documents/2_Manuscript/2_Figures/Appendix/Appendix_Figure3E.pdf", 
#        width = 8.5, height = 7, dpi = "retina")
# 
# foreign::write.dta(Analysis_allregime, "Analysis_Data/Analysis_allregime.dta")
 