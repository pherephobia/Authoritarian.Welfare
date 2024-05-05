############################################################
################### 1/2/2019 Dataset for ###################
####### Why do authoritarian regimes redistribute? #########
######### Sanghoon Park, Univ. of South Carolina ###########
############################################################
rm(list=ls())

#Import Packages to use
library(ggplot2)
library(grid)
library(pBrackets) 
library(ezpickr)
library(countrycode)
library(reshape2)
library(stargazer)
library(summarytools)
library(stringr)
library(here)
library(tidyr)
library(purrr)
library(stargazer)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(patchwork)
library(plm)
library(wesanderson)
library(panelView)
library(RColorBrewer) 
library(ggrepel)
library(tidyverse)
here::here() %>% setwd()
getwd()

#############################################################
# Import dataset to use
## Dictatorship dataset & GDP & 
#############################################################
Analysis_allregime <- ezpickr::pick("Analysis_Data/Analysis_allregime.dta")


Analysis_allregime |>  as_tibble() -> Analysis_allregime

Analysis_allregime <- Analysis_allregime %>%
  mutate(
    ccode = countrycode::countrycode(country_name, "country.name", "cown")
  ) %>% drop_na(ccode)
Analysis_allregime$COWcode <- NULL

#############################################################
##### Regime type and Party Institutionalization Index  #####
#############################################################


criteria <- mdata %>% dplyr::filter(regime=="Democracies") %>% 
  select(v2x_polyarchy, v2xps_party) %>%
  dplyr::summarize(q.v2x_polyarchy = 
                     quantile(v2x_polyarchy, probs = c(0.5), na.rm = T),
                   q.v2xps_party = 
                     quantile(v2xps_party, probs = c(0.5), na.rm = T))

mdata %>% drop_na(regime) %>% group_by(regime) %>% count()
mdata %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Democracies" & v2x_polyarchy < 0.5) %>% 
  select(v2x_polyarchy) %>% count()
mdata %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Democracies") %>% 
  select(v2x_polyarchy) %>% count()
round(638/4773 * 100, 2)
mdata %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Autocracies" & v2x_polyarchy > 0.5) %>% 
  select(v2x_polyarchy) %>% count()
mdata %>% drop_na(regime) %>% 
  dplyr::filter(regime=="Autocracies") %>% 
  select(v2x_polyarchy) %>% count()
round(477/13108 * 100, 2)



Plot1 <- mdata %>% drop_na(regime) %>%
  ggplot(aes(y = v2x_polyarchy, x = regime, color = regime)) + 
  geom_jitter(alpha=0.2, position=position_jitter(0.2), show.legend = FALSE) +   
  geom_boxplot(alpha=0.2, 
               position=position_dodge(0.8), width = 0.4, color = "black") +
  stat_summary(fun.y=mean, geom="point", shape=16,
               size=3, color="red") + 
  geom_hline(yintercept = 0.5, color = "red") + 
  labs(x = "", y = "Electoral Democracy Index") + 
  scale_x_discrete(labels=c("Autocracies" = "Autocracies (n=13,108)", 
                            "Democracies" = "Democracies (n=4,773)")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  annotate(geom = "rect", xmin = 0.75, xmax = 1.25,
           ymin = 0.5, ymax = 0.85, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 1, y = 0.9, label = "3.64% (477) of Autocracies "
  ) +
  annotate(geom = "rect", xmin = 1.75, xmax = 2.25,
           ymin = 0.1, ymax = 0.5, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 2, y = 0.05, label = "13.37% (638) of Democracies"
  ) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 3))
print(Plot1)
#ggsave("Figures/Plot1.pdf", width = 8.5, height = 4)
ggsave("Figures/Plot1.png", width = 8.5, height = 4)




######################################################
##### Supporting groups in authoritarian regimes #####
######################################################




### support group size

autocracies %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle") & year > 1916) %>% 
  ggplot(aes(x = v2regsupgroupssize)) + 
  geom_density(aes(x = v2regsupgroupssize, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(class)) + 
  geom_vline(data=filter(summary.w, class=="Party elites"), 
             aes(xintercept=mean.size), colour="black") +
  geom_text(
    data    = filter(summary.w, class=="Party elites"),
    map = aes(x = mean.size + 0.5, y = 0.05), size = 3,
    label = "Mean size. = 0.276") +
  geom_vline(data=filter(summary.w, class=="The military"), 
             aes(xintercept=mean.size), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="The military"),
    map = aes(x = mean.size + 0.5, y = 0.05), size = 3,
    label = "Mean size = -0.02") + 
  geom_vline(data=filter(summary.w, class=="Urban Working"), 
             aes(xintercept=mean.size), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Urban Working"),
    map = aes(x = mean.size + 0.5, y = 0.05), size = 3,
    label = "Mean size = 0.919") + 
  geom_vline(data=filter(summary.w, class=="Urban Middle"), 
             aes(xintercept=mean.size), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Urban Middle"),
    map = aes(x = mean.size, y = 0.05), size = 3,
    label = "Mean size = 1.47") + 
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Universalism Index of SPaW",
       y = "Density") + 
  facet_wrap(~class, ncol = 2) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    axis.title.x = element_text(
      margin = margin(t = 25, b = 10)),
    axis.title.y = element_text(
      margin = margin(r = 5, l = 5))
  )

### Yearly frequencies
class.year <- autocracies %>% select(class, year, ccode, country_name) %>%
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle")) %>% 
  group_by(class, year) %>% summarize(n())
v2regendtype
label <- c("Party elites","The military", "The ethnic/racial/religous groups",
           "Agrarian/local elites","Business elites/civil servants",
           "Urban Working","Rural Working", 
           "Urban Middle","Rural Middle","The aristocracy",
           "A foreign govt. or colonial power")
Plot3 <-
  class.year %>% mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path() +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
#  facet_wrap(~class) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  annotate("segment", x = 1910, xend = 1917, y = 28, yend = 30, 
           colour = "red", size=0.5, arrow=arrow(length = unit(0.03, "npc"))) + 
  annotate("text", x=1900, y=28, 
           label="Year of 1917\nthe Russian Revolution", 
           color = "red", size = 2.5) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))
print(Plot3)
ggsave("Figures/Plot3.pdf", width = 8.5, height = 5)

class.year <- autocracies %>% select(class, year, ccode, country_name) %>%
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle")) %>% 
  group_by(year) %>% summarize(n())

class.year2 <- class.year %>% tidyr::spread(., class, `n()`) %>% 
  rowwise() %>% 
  mutate(Sum = sum(`Party elites`, 
                   `The military`,
                   `Urban Working`,
                   `Urban Middle`, na.rm = TRUE),
         Party_elites = (`Party elites`/Sum),
         Military = (`The military`/Sum),
         Urban_Working = (`Urban Working`/Sum),
         Urban_Middle = (`Urban Middle`/Sum)) %>% ungroup()

class.year2 %>% 
  gather(Group, Value, Party_elites:Urban_Middle) %>% 
  ggplot(aes(x=year, y=Value, fill=Group)) + 
  geom_area()
table(autocracies$class)

type.class <- autocracies %>% select(class, year, ccode, v2csantimv, country_name) %>%
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle")) %>% 
  group_by(class, v2csantimv) %>% summarize(n())

type.class <- autocracies %>% select(class, year, ccode, v2csantimv, country_name) %>%
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle")) %>%
  group_by(class) %>% summarise(across(is.numeric, mean))



type <- type.class %>% select(class, mean) %>% unique()

type %>% ggplot() + 
  geom_col(aes(x = class, y = mean, fill = class)) + 
  scale_fill_manual(values = wes_palette("GrandBudapest2")) + 
  geom_text(aes(x = class, y = mean, label = round(mean, 2)), vjust = -0.2) + 
  labs(x = "", y = "") + 
  theme_bw() + theme(
    legend.position = "bottom"
  )
ggsave("PRplot2.pdf", width = 8, height = 4)
long.type.class <- type.class %>% 
  tidyr::pivot_wider(names_from = v2regendtype, values_from = `n()`)

type.class %>% ggplot(aes(fill=class, y=`n()`, x=v2regendtype)) + 
  geom_bar(position="fill", stat="identity")


###################################################################
### All regimes
###################################################################

class.year.dic <- allregime %>% select(class, year, ccode, country_name, regime) %>%
  dplyr::filter(regime == "Autocracies") %>% 
  group_by(class, year) %>% summarize(n()) %>% mutate(
    regime = "Autocracies"
  )

class.year.dem <- allregime %>% select(class, year, ccode, country_name, regime) %>%
  dplyr::filter(regime == "Democracies") %>% 
  group_by(class, year) %>% summarize(n()) %>% mutate(
    regime = "Democracies"
  )

class.year.all <- bind_rows(class.year.dic, class.year.dem) %>% 
  mutate(
    regime = regime %>% 
    parse_factor(., levels = c("Autocracies", "Democracies"),
                 ordered = F, include_na = F)
)
  
  
class.year.all.core <- class.year.all %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Urban Working", "Urban Middle"))

label <- c("Party elites","The military", "The ethnic/racial/religous groups",
           "Agrarian/local elites","Business elites/civil servants",
           "Urban Working","Rural Working", 
           "Urban Middle","Rural Middle","The aristocracy",
           "A foreign govt. or colonial power")
Plot3_dic <-
  class.year.dic %>% mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path() +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
  #  facet_wrap(~class) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  annotate("segment", x = 1910, xend = 1917, y = 28, yend = 30, 
           colour = "red", size=0.5, arrow=arrow(length = unit(0.03, "npc"))) + 
  annotate("text", x=1900, y=28, 
           label="Year of 1917\nthe Russian Revolution", 
           color = "red", size = 2.5) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))
print(Plot3_dic)


Plot3_dem <-
  class.year.dem %>% mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path() +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
  #  facet_wrap(~class) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "", x = "") + 
  annotate("segment", x = 1910, xend = 1917, y = 28, yend = 30, 
           colour = "red", size=0.5, arrow=arrow(length = unit(0.03, "npc"))) + 
  annotate("text", x=1900, y=28, 
           label="Year of 1917\nthe Russian Revolution", 
           color = "red", size = 2.5) + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))

table(unique(class.year.all$class))
Plot3_dic <- class.year.all %>% 
  dplyr::filter(class %in% c("Party elites","The military",
                "The ethnic/racial/religous groups",
                "Agrarian/local elites","Business elites/civil servants",
                "Urban Working","Rural Working", 
                "Urban Middle","Rural Middle") & regime == "Autocracies") %>%
  mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path(show.legend = F) +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
#  facet_wrap(~regime) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))

Plot3_all <- class.year.all %>% 
  dplyr::filter(class %in% c("Party elites","The military",
                             "Business elites/civil servants",
                             "Urban Working", 
                             "Urban Middle")) %>%
  mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path(show.legend = F) +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
  #  facet_wrap(~regime) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))


Plot3_dem <- class.year.all %>% 
  dplyr::filter(class %in% c("Party elites","The military",
                             "Business elites/civil servants",
                             "Urban Working", 
                             "Urban Middle") & regime == "Democracies") %>%
  mutate(
    label = if_else(year == max(year), as.character(class), NA_character_)) %>%
  ggplot(aes(x = year, y = `n()`, color = class)) + 
  geom_path(show.legend = F) +
  scale_x_continuous(breaks = seq(1800, 2010, 30)) + 
#  facet_wrap(~regime) + 
  geom_label_repel(aes(label = label), size = 2.5,
                   nudge_x = 1, na.rm = TRUE, show.legend = F) + 
  scale_color_discrete(guide = FALSE) + 
  geom_vline(xintercept = 1917, color = "red", linetype = "dashed") + 
  scale_color_hue(l=40) + 
  labs(y = "Frequency", x = "Year") + 
  #scale_color_manual(values = wes_palette("BottleRocket1")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=9))


Plot3_dem
ggsave("Figures/Appendix/Appendix3_trenddem.pdf", width = 8.5, height = 5)




#############################################################
##### Democratization  #####
#############################################################
allregime <- allregime %>% mutate(
  Demo = case_when(
    regime == "Democracies" ~ "1",
    regime == "Autocracies" ~ "0",
    T ~ NA_character_),
  Auto = case_when(
    regime == "Democracies" ~ "0",
    regime == "Autocracies" ~ "1",
    T ~ NA_character_)
  )
allregime <- allregime %>% mutate(
  combination = paste0(Demo, Auto)
)

allregime <- allregime %>% mutate(
  democratization = case_when(
    combination == "10" ~ "Democratization",
    combination == "01" ~ "Autocratization",
    T ~ NA_character_
  ) %>% parse_factor(., levels(c("Democratization,", "Autocratization")),
                     ordered = T, include_na = F),
  democratization.nu = case_when(
    democratization == "Democratization" ~ 1L,
    democratization == "Autocratization" ~ 0L,
    T ~ NA_integer_
  ))

democ <- allregime %>% 
  select(class_raw, year, ccode, country_name) %>%
  group_by(class_raw, year) %>% summarize(n())

democ %>%  ggplot(aes(x=year, y = `n()`)) + 
  geom_bar(position="stack", stat="identity") + facet_wrap(.~class) + 
  theme_bw()
#############################################################
##### Data Exports  #####
#############################################################


sample <- Analysis_allregime |> 
  dplyr::select(ccode, year, country_name, 
                regime_boix, lexical_demo, edi, freedom,
                universality, inst_mass, repression,
                contains("party_"), alternative,
                gdp, gdppc, pop, civilwar, resource_income, contains("govt_"))

state_capacity <- ezpickr::pick("Original_data/StateCapacityDataset_v1.dta/StateCapacityDataset_v1.dta")

library(ezpickr)

# qog <- ezpickr::pick(
#   "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan22.dta"
#   )

qog |> dplyr::select(ccodecow, year, wdi_internet) -> qog_sub

capacity_sample <- state_capacity |> dplyr::select(ccode, year, Capacity)

sample |> left_join(capacity_sample, by = c("ccode", "year")) -> sample

sample |> left_join(qog_sub, by = c("ccode" = "ccodecow", "year")) -> sample

sample |> group_by(ccode) |> 
  mutate(
    inst_mass = factor(inst_mass, 
                       levels = c(2, 1),
                       labels = c("Institution-based", "Mass-based")),
    surveillance = 1 - alternative,
    internet_close = 100 - wdi_internet,
    freedom_re = 1-freedom,
    l_freedom = dplyr::lag(freedom_re, order_by = year, n = 1),
    l_internet = dplyr::lag(internet_close, order_by = year, n = 1),
    l_regime_boix = dplyr::lag(regime_boix, order_by = year, n = 1),
    l_lexical_demo = dplyr::lag(lexical_demo, order_by = year, n = 1),
    l_universality = dplyr::lag(universality, order_by = year, n = 1),
    l_inst_mass = dplyr::lag(inst_mass, order_by = year, n = 1),
    l_party_inst = dplyr::lag(party_inst, order_by = year, n = 1),
    l_party_org = dplyr::lag(party_org, order_by = year, n = 1),
    l_party_brch = dplyr::lag(party_brch, order_by = year, n = 1),
    l_party_link = dplyr::lag(party_link, order_by = year, n = 1),
    l_repression = dplyr::lag(repression, order_by = year, n = 1),
    l_party_platform = dplyr::lag(party_platform, order_by = year, n = 1),
    l_party_cohesv = dplyr::lag(party_cohesv, order_by = year, n = 1),
    l_alt = dplyr::lag(surveillance, order_by = year, n = 1),
    l_govt_alt = dplyr::lag(govt_alternative, order_by = year, n = 1),
    l_govt_monitor = dplyr::lag(govt_monitoring, order_by = year, n = 1),
    l_govt_censor = dplyr::lag(govt_censor, order_by = year, n = 1),
    l_gdp = dplyr::lag(gdp, order_by = year, n = 1),
    l_gdppc = dplyr::lag(gdppc, order_by = year, n = 1),
    l_pop = dplyr::lag(pop, order_by = year, n = 1),
    l_civilwar = dplyr::lag(civilwar, order_by = year, n = 1),
    l_resource = dplyr::lag(resource_income, order_by = year, n = 1),
    resource_dep = resource_income/gdppc,
    l_resource_dep = dplyr::lag(resource_dep, order_by = year, n = 1),
    l_statecapa = dplyr::lag(Capacity, order_by = year, n = 1),
    gdpgrth = gdp - l_gdp/gdp,
    l_gdpgrth = dplyr::lag(gdpgrth, order_by = year, n = 1),
    coldwar = if_else(year >= 1947 & year < 1992, 1L, 0L),
    l_coldwar = dplyr::lag(coldwar, order_by = year, n = 1),
  ) -> sample

sample |> dplyr::filter(regime_boix %in% 0L) |> 
  dplyr::select(
    ccode, year,
    universality, l_inst_mass, l_party_inst, l_statecapa, l_freedom,
    l_gdp, l_gdppc, l_gdpgrth, l_pop, l_civilwar, l_resource_dep) |> drop_na() |> 
  pull(year) |> unique() |> table()

library(estimatr)

lm(universality ~ 
            l_inst_mass
          + as.factor(ccode),
          #+ as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model1

lm(universality ~ 
            l_party_inst +
          + as.factor(ccode),
          #+ as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model2

lm(universality ~ 
     l_freedom +
            + as.factor(ccode),
          #+ as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model3
texreg::screenreg(model3, omit.coef = "as.factor")



lm(universality ~ 
            l_inst_mass +
          + l_party_inst
          + as.factor(ccode),
          #+ as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model4

lm(universality ~ 
            l_inst_mass
          + l_freedom
          + as.factor(ccode),
#          + as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model5

lm(universality ~ 
            l_inst_mass
          + l_party_inst
          + l_freedom
          + as.factor(ccode),
          #+ as.factor(year),
          #clusters = ccode, se_type = "stata",
          data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model6

lm(universality ~ 
     l_inst_mass
   + l_party_inst 
   + l_freedom
   + l_statecapa
   #+ I(log(l_gdp + 1))
   + I(log(l_gdppc + 1)) 
   #+ I(log(l_pop + 1)) 
   + l_gdpgrth
   + l_civilwar 
   + l_resource_dep 
   #+ coldwar
   + as.factor(ccode),
  # + as.factor(year),
   #clusters = ccode, se_type = "stata",
   data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model7
#  texreg::screenreg(omit.coef = "as.factor", include.ci = FALSE)

lm(universality ~ 
     l_inst_mass*l_party_inst
   + l_freedom
   + l_statecapa
   #+ l_repression
   #+ I(log(l_gdp + 1))
   + I(log(l_gdppc + 1)) 
   #+ I(log(l_pop + 1)) 
   + l_gdpgrth
   + l_civilwar 
   + l_resource_dep 
   #+ coldwar
   + as.factor(ccode),
   #+ as.factor(year),
   #clusters = ccode, se_type = "stata"
   data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model8 
  #texreg::screenreg(omit.coef = "as.factor") 
  #marginaleffects::plot_slopes(variables = "l_inst_mass", condition = list("l_party_inst"))

texreg::screenreg(model9, omit.coef = "as.factor")

marginaleffects::plot_slopes(model8, variables = "l_inst_mass", condition = list("l_party_inst")) +
  labs(x = "\nParty institutionalzation", y = "Marginal effect of mass-based ruling coalition on universal welfare provision\n") ->
  plot1
library(latex2exp)
marginaleffects::plot_slopes(model8, variables = "l_inst_mass", condition = list("l_party_inst")) +
  labs(x = "\nSurveillance",
       y =  expression(frac(partialdiff*paste(" (Universal welfare provision)"),
                            partialdiff*paste(" (Mass-based ruling coalition)"))))

marginaleffects::plot_predictions(model8, condition = list("l_inst_mass", "l_party_inst" = "minmax"))

ggsave("Documents/2_Manuscript/2_Figures/MPSA23_marginalplot.pdf", width = 6, height = 4, dpi = "retina")

lm(universality ~ 
     l_inst_mass*l_freedom
   + l_party_inst
   + l_statecapa
   #+ l_repression
   #+ I(log(l_gdp + 1))
   + I(log(l_gdppc + 1)) 
   #+ I(log(l_pop + 1)) 
   + l_gdpgrth
   + l_civilwar 
   + l_resource_dep 
   #+ coldwar
   + as.factor(ccode),
#   + as.factor(year),
   #clusters = ccode, se_type = "CR2",
   data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model9 
texreg::screenreg(model9, omit.coef = "as.factor")  
marginaleffects::plot_slopes(model9, variables = "l_inst_mass", condition = list("l_freedom"))

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

DAAG::vif(model7)

texreg::texreg(
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
           "129", "142", "142", "128", "129", "128", "100", "100", "100")),
  file =  "Documents/2_Manuscript/1_Tables/table1.tex")
getwd()  


lm(universality ~ 
     l_inst_mass* I(l_freedom^2)
   + l_party_inst
   + l_statecapa
   #+ l_repression
   #+ I(log(l_gdp + 1))
   + I(log(l_gdppc + 1)) 
   #+ I(log(l_pop + 1)) 
   + l_gdpgrth
   + l_civilwar 
   + l_resource_dep 
   #+ coldwar
   + as.factor(ccode),
   #+ as.factor(year),
   #clusters = ccode, se_type = "stata"
   data = sample |> dplyr::filter(regime_boix %in% 0L)) -> model_test
  texreg::screenreg(model_test, omit.coef = "as.factor") 
  
  marginaleffects::plot_slopes(model_test, variables = "l_inst_mass", condition = list("l_freedom"))
  
#############################################################
##### Coefplot for Table 1
#############################################################
library(ezpickr)
table1 <- pick("Analysis_Data/table1.dta")
library(tidyverse)

table1 <- table1 %>% dplyr::filter(!str_detect(var, ".ccode"))
table1 <- table1 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  rowid_to_column() %>%
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 1, 0),
    model = case_when(
      rowid > 28 & rowid < 35 ~ "T1M1",
      rowid > 24 & rowid < 29 ~ "T1M3",
      rowid > 17 & rowid < 25 ~ "T1M5",
      rowid > 11 & rowid < 18 ~ "T1M2",
      rowid > 7 & rowid < 12 ~ "T1M4",
      rowid > 0 & rowid < 8 ~ "T1M6"))
  


lm(universality ~ 
     l_inst_mass
   + l_freedom
   + l_party_inst
   + l_repression
   + l_statecapa
   #+ I(log(l_gdp + 1))
   + I(log(l_gdppc + 1))
   #+ I(log(l_pop + 1)) 
   + l_gdpgrth
   + l_civilwar 
   + l_resource_dep 
   #+ coldwar
   + as.factor(ccode),
   #+ as.factor(year),
   #clusters = ccode, se_type = "CR2",
   data = sample |> dplyr::filter(regime_boix %in% 0L)) |> 
  texreg::screenreg(omit.coef = "as.factor")


table1_spaw <- table1 %>% 
  dplyr::filter(var %in% c("party", "military", "urban_working", "lag3pi") &
                  model %in% c("T1M1", "T1M3", "T1M5")) 

table1_vdem <- table1 %>% 
  dplyr::filter(var %in% c("party", "military", "urban_working", "lag3pi") &
                  model %in% c("T1M2", "T1M4", "T1M6")) 

table1_spaw <- table1_spaw %>% 
  mutate(
    model = model %>% 
      parse_factor(., levels = c("T1M1", "T1M3", "T1M5"),
                   ordered = T, include_na = F),
    var = var %>% parse_factor(., c("urban_working", "party", "military", "lag3pi"),
                               include_na = F, ordered = T),
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
    )
table(table1_spaw$var)
labels.spaw <- c(T1M1 = "Model 1", T1M5 = "Model 3", T1M3 = "Model 2")
spaw <- table1_spaw %>%
  ggplot(aes(x = var, y = coef, color=as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9), show.legend = F) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9),
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_x_discrete(label = c("Urban\nWorking", "Party\nElites", 
                              "Military", "Party\nInst.")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  labs(x = "", 
       y = "Estimates",
       title = "") + 
  facet_wrap(~model, ncol = 3, labeller=labeller(model = labels.spaw)) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.title = element_text(size = 10))
print(spaw)
ggsave("PRplot1.pdf", width = 8, height = 4)

labels.vdem <- c(T1M4 = "Model 4", T1M2 = "Model 5", T1M6 = "Model 6")

table1_vdem <- table1_vdem %>% 
  mutate(
    model = model %>% 
      parse_factor(., levels = c("T1M2", "T1M4", "T1M6"),
                   ordered = T, include_na = F),
    var = var %>% parse_factor(., c("urban_working", "party", "military", "lag3pi"),
                               include_na = F, ordered = T),
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )

vdem <- table1_vdem %>%
  ggplot(aes(x = var, y = coef, color=as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9), show.legend = F) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9),
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_x_discrete(label = c("Urban\nWorking", "Party\nElites", 
                             "Military", "Party\nInst.")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  labs(x = "", 
       y = "",
       title = "Universal welfare (delivery)") + 
  facet_wrap(~model, ncol = 3, labeller=labeller(model = labels.vdem)) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.title = element_text(size = 10))

Figure.table1 <- spaw + vdem + plot_layout(ncol = 2)
print(spaw)
ggsave("Figures/Appendix/Appendix6.pdf", width = 8.5, height = 5)
#ggsave("Figures/Table1.png", width = 8.5, height = 5)

#############################################################
##### Coefplot for Table 2
#############################################################
t2m1 <- pick("Data/table2m1.dta");t2m2 <- pick("Data/table2m2.dta");
t2m3 <- pick("Data/table2m3.dta");t2m4 <- pick("Data/table2m4.dta");
t2m5 <- pick("Data/table2m5.dta");t2m6 <- pick("Data/table2m6.dta")

t2m1 <- t2m1 %>% mutate(model = "Branch")
t2m2 <- t2m2 %>% mutate(model = "Linkage")
t2m3 <- t2m3 %>% mutate(model = "Organizations")
t2m4 <- t2m4 %>% mutate(model = "Platforms")
t2m5 <- t2m5 %>% mutate(model = "Cohension")
t2m6 <- t2m6 %>% mutate(model = "Disaggregation")
table2 <- bind_rows(t2m1, t2m2, t2m3, t2m4, t2m5, t2m6)
table2 <- table2 %>% dplyr::filter(!str_detect(var, ".ccode"))
table2 <- table2 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )
table(table2$insig)
table(table2$model)
table2 <- table2 %>% mutate(
  model = model %>%
    parse_factor(., levels = c("Branch", "Linkage",
                               "Organizations", "Cohension", 
                               "Platforms", "Disaggregation"),
                 ordered = T, include_na = F)
)
table(table2$var)
test <- table2 %>%
  dplyr::filter(var %in% c("lag3pbranch", "lag3plink",
                           "lag3porgs", "lag3pcohesv", 
                           "lag3pplats"))
table(test$var)



t2m <- table2 %>%
  dplyr::filter(var %in% c("lag3pbranch", "lag3plink",
                           "lag3porgs", "lag3pcohesv", 
                           "lag3pplats")) %>%
  mutate(
    var = case_when(
      var == "lag3pbranch" ~ "Branch",
      var == "lag3pcohesv" ~ "Cohension",
      var == "lag3plink" ~ "Linkage",
      var == "lag3porgs" ~ "Organizations",
      var == "lag3pplats" ~ "Platforms",
      T ~ NA_character_)) %>%
  ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "",
       title = "Disaggregated Party Institutionalizations and\nUniversal Welfare Programs (V-Dem)",
       caption = "Note: Class coalitions and other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none", 
                     plot.title = element_text(size= 9))

ggsave("Figures/Plot4.pdf", width = 8.5, height = 5)


t1m1 <- pick("Data/table1m1.dta");t1m2 <- pick("Data/table1m2.dta");
t1m3 <- pick("Data/table1m3.dta");t1m4 <- pick("Data/table1m4.dta");
t1m5 <- pick("Data/table1m5.dta");t1m6 <- pick("Data/table1m6.dta")

t1m1 <- t1m1 %>% mutate(model = "Branch")
t1m2 <- t1m2 %>% mutate(model = "Linkage")
t1m3 <- t1m3 %>% mutate(model = "Organizations")
t1m4 <- t1m4 %>% mutate(model = "Platforms")
t1m5 <- t1m5 %>% mutate(model = "Cohension")
t1m6 <- t1m6 %>% mutate(model = "Disaggregation")
table1 <- bind_rows(t1m1, t1m2, t1m3, t1m4, t1m5, t1m6)
table1 <- table1 %>% dplyr::filter(!str_detect(var, ".ccode"))
table1 <- table1 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )
table(table1$insig)
table(table1$model)
table1 <- table1 %>% mutate(
  model = model %>%
    parse_factor(., levels = c("Branch", "Linkage",
                               "Organizations", "Cohension", 
                               "Platforms", "Disaggregation"),
                 ordered = T, include_na = F)
)
table(table1$var)
test <- table1 %>%
  dplyr::filter(var %in% c("lag3pbranch", "lag3plink",
                           "lag3porgs", "lag3pcohesv", 
                           "lag3pplats"))
table(test$var)
t1m <- table1 %>%
  dplyr::filter(var %in% c("lag3pbranch", "lag3plink",
                           "lag3porgs", "lag3pcohesv", 
                           "lag3pplats")) %>%
  mutate(
    var = case_when(
      var == "lag3pbranch" ~ "Branch",
      var == "lag3pcohesv" ~ "Cohension",
      var == "lag3plink" ~ "Linkage",
      var == "lag3porgs" ~ "Organizations",
      var == "lag3pplats" ~ "Platforms",
      T ~ NA_character_)) %>%
  ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "",
       title = "Disaggregated Party Institutionalizations and\nUniversal Welfare Programs (SPaW)") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none",
                     plot.title = element_text(size= 9))

t1m + t2m + patchwork::plot_layout(ncol = 1)
ggsave("Figures/Appendix/Appendix7.pdf", width = 8.5, height = 6)

#############################################################
##### Coefplot for interaction
#############################################################
int1 <- pick("Data/int1.dta");int2 <- pick("Data/int2.dta")
int1 <- int1 %>% mutate(model = "SPaW")
int2 <- int2 %>% mutate(model = "V-Dem")
table3 <- bind_rows(int1, int2)
table3 <- table3 %>% dplyr::filter(!str_detect(var, ".ccode"))
table3 <- table3 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )
table(table3$var)
pi_grid <- seq(0, 1, by=0.1)

betas <- rbind(table3$coef[2], table3$coef[4], table3$coef[6],
               table3$coef[7], table3$coef[9], table3$coef[11],
               table3$coef[13], 
               table3$coef[14], table3$coef[15], table3$coef[16])
party    <- cbind(1, 0, 0, pi_grid, pi_grid, 0, 0, 0, 0, 0)
military <- cbind(0, 1, 0, pi_grid, 0, pi_grid, 0, 0, 0, 0)
working  <- cbind(0, 0, 1, pi_grid, 0, 0, pi_grid, 0, 0, 0)



re.sample <- pick("revison_sample.dta")
test1 <- re.sample %>% drop_na(totalunivers,ccode, year, party,
                               military, working, urban_middle,
                               lag3gdp, lag3pop, lag3resdep)
re.sample <- re.sample %>% dplyr::filter(class %in% c(1,2,6,7) & year > 1916)
re.sample$year <- as.integer(re.sample$year)
re.sample <- re.sample %>% drop_na(year, ccode) %>% mutate(
  timeVar = year
)
install.packages("pcse2")
spaw.lm <- lm(data = re.sample,
              totalunivers ~ 
                party + military + working + 
                lag3pi + 
                I(party*lag3pi) + I(party*lag3pi) + I(working*lag3pi) + 
                lag3gdp + lag3pop + lag3resdep)
length(re.sample$ccode)
length(re.sample$year)
test <- pcse(spaw.lm, groupN = re.sample$ccode, groupT = re.sample$year, pairwise = T)


table(unique(re.sample$year))


party_int <- party %*% betas

ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "",
       caption = "Note: Class coalitions and other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none")

#############################################################
##### Coefplot for separate programs
#############################################################


t3m1 <- pick("t3m1.dta");t3m2 <- pick("t3m2.dta");t3m3 <- pick("t3m3.dta");
t3m4 <- pick("t3m4.dta");t3m5 <- pick("t3m5.dta");t3m6 <- pick("t3m6.dta")

t3m1 <- t3m1 %>% mutate(model = "Old-age")
t3m2 <- t3m2 %>% mutate(model = "Mater")
t3m3 <- t3m3 %>% mutate(model = "Sick")
t3m4 <- t3m4 %>% mutate(model = "Working")
t3m5 <- t3m5 %>% mutate(model = "Unempl.")
t3m6 <- t3m6 %>% mutate(model = "Family")

table4 <- bind_rows(t3m1, t3m2, t3m3, t3m4, t3m5, t3m6)
table4 <- table4 %>% dplyr::filter(!str_detect(var, ".ccode"))
table4 <- table4 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1),
    model = model %>% 
      parse_factor(., levels = c("Old-age", "Sick", "Unempl.", 
                                 "Mater", "Working", "Family"), 
                   include_na = F, ordered = T),
    benefits = case_when(
      model == "Old-age" ~ "Cash",
      model == "Sick" ~ "Cash",
      model == "Unempl." ~ "Cash",
      model == "Mater" ~ "Noncash",
      model == "Working" ~ "Noncash",
      model == "Family" ~ "Noncash",
      T ~ NA_character_) %>% 
      parse_factor(., levels = c("Cash", "Noncash"),
                   include_na = F, ordered = T),
    work = ifelse(var=="working", 1, 0)
  )
table4 %>% dplyr::filter(var %in% c("working", "urban_middle", "party")) %>%
  ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "Estimates",
       caption = "Note: Other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none")

t2pi1 <- pick("t2pi1.dta")
t2pi2 <- pick("t2pi2.dta")
t2pi1 <- t2pi1 %>% mutate(model = "SPaW")
t2pi2 <- t2pi2 %>% mutate(model = "V-Dem")

table5 <- bind_rows(t2pi1, t2pi2)
table5 <- table5 %>% dplyr::filter(!str_detect(var, ".ccode"))
table5 <- table5 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1),
    model = model %>% 
      parse_factor(., levels = c("SPaW", "V-Dem"),
                   include_na = F, ordered = T))
table(table5$var)
table5 %>% dplyr::filter(var %in% c("working", "urban_middle", "party")) %>%
  ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "Estimates",
       caption = "Note: Other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 1) + 
  theme_bw() + theme(legend.position = "none")

#############################################################
## Interaction class and pi
#############################################################

interaction <- pick("Analysis_Data/intclasspi.dta")
interaction$var
table6 <- interaction %>% dplyr::filter(!str_detect(var, ".ccode"))
interaction <- interaction %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )
install.packages("panelAR")
install.packages("rlang")
re.sample <- pick("Analysis_Data/urban_working.dta")
data <- re.sample %>% 
  select(totalunivers, ccode, year, ww1, ww2, coldwar, party, military,
         urban_working, urban_middle, 
         lag3pi, lag3gdp, lag3pop, lag3resdep, dum_3, dum_4,
         dum_5, dum_7, dum_9, dum_10, dum_11) %>%
  dplyr::filter((dum_3==0 | dum_4==0 | dum_5==0 | 
                  dum_7==0 | dum_9==0 | dum_10==0 | dum_11 == 0) & 
                  (year > 1915)) %>% data.frame()
drop_na(data)
data <- drop_na(data)
library(panelAR)
results <- panelAR(formula = totalunivers ~ as.factor(ccode) + 
          ww1 + ww2 + coldwar + party + military + urban_working + lag3pi + 
          I(urban_working*lag3pi) + lag3gdp + lag3pop + lag3resdep,
        data = data, panelVar = "ccode", timeVar = "year",
        panelCorrMethod = "pcse", autoCorr = "psar1", complete.case = F,
        rho.na.rm = T, singular.ok = T)
summary(results)
results$terms
results$coefficients

se <- sqrt(diag(results$vcov))

1- pnorm(3.00292408/1.449234296)

summary.panelAR(results)
help(print.summary.panelAR)

broom::tidy(results)
a <- vcov(results)
results$coefficients
results



beta_draws <- rmvnorm(n=100000, mean = coef(results), sigma=vcov(results)) #Parametric Bootstrapping
library(mvtnorm)
names(beta_draws)
beta_draw_s
beta_draw_s <- beta_draws[, -c(2:82)]
beta_draw_s[1,]
pi_grid <- seq(0, 1, by=0.01)
rownames(beta_draws)
v <- colnames(beta_draws)
dim(beta_draws)
dim(beta_draws)

noworking <- cbind(1, 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                   .0205034, .0249208, .2872979, .1951177, .2465468, 0, pi_grid, 0, 7.906028, 8.534317, 4.589921)

working <- cbind(1, 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,
                 0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632,0.01052632
                 
                 , .0205034, .0249208, .2872979, .1951177, .2465468, 1, pi_grid, pi_grid, 7.906028, 8.534317, 4.589921)


dim(beta_draws)
dim(noworking)
dim(working)

NW <- t(noworking %*% t(beta_draws))
NW.m <- apply(NW, 2, mean)
NW.se <- apply(NW, 2, quantile, c(0.025, 0.975))
W <- t(working %*% t(beta_draws))
W.m <- apply(W, 2, mean)
W.se <- apply(W, 2, quantile, c(0.025, 0.975))
NW.data <- data.frame(ID = "No-Urban working",
                     Class = "No-Urban working",
                     PI=pi_grid,
                     Mean=NW.m,
                     Lower=NW.se[1,],
                     Upper=NW.se[2,])
W.data <- data.frame(ID = "Urban working",
                      Class = "Urban working",
                      PI=pi_grid,
                      Mean=W.m,
                      Lower=W.se[1,],
                      Upper=W.se[2,])
Data <- rbind(NW.data, W.data) %>% as_tibble()

library(wesanderson)
library(tidyverse)
Data %>% 
  ggplot(aes(x=PI, y=Mean, color=ID)) + 
  geom_ribbon(aes(y = Mean, ymin = Lower, ymax = Upper, fill= ID), 
              show.legend = F, alpha = 0.2) + 
  geom_line() + 
  #geom_point(position = position_dodge(0.1)) +
  #geom_pointrange(aes(y = Mean, ymin = Lower, ymax = Upper),
  #                position = position_dodge(0.1))+ 
#facet_wrap(~ID) +
  labs(y="Predicted Universal Welfare", x="Party Institutionaliztion") + 
  scale_x_continuous(breaks = c(seq(from = 0.0, to = 1.0, by = 0.1))) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = wes_palette(n = 2, name = "BottleRocket1")) + 
  scale_fill_manual(values = wes_palette(n = 2, name = "BottleRocket1")) + 
#  scale_color_viridis(discrete=TRUE, option="plasma") + 
#  scale_fill_viridis(discrete=TRUE, option="plasma") + 
#  scale_color_brewer(palette = "PuOr") + 
  #scale_fill_brewer(palette = "PuOr") +
  guides(col = guide_legend(nrow = 1))
?scale_color_paletteer_d
ggsave("Figures/Plot5.pdf", width = 8.5, height = 4)
library(viridis)
install.packages("ggthemes")
library(ggthemes)
??wesanderson
summary(results)
data <- data.frame(data)
table2 %>%
  dplyr::filter(var %in% c("lag3pbranch", "lag3plink",
                           "lag3porgs", "lag3pcohesv", 
                           "lag3pplats")) %>%
  mutate(
    var = case_when(
      var == "lag3pbranch" ~ "Branch",
      var == "lag3pcohesv" ~ "Cohension",
      var == "lag3plink" ~ "Linkage",
      var == "lag3porgs" ~ "Organizations",
      var == "lag3pplats" ~ "Platforms",
      T ~ NA_character_)) %>%
  ggplot(aes(x = var, y = coef, color = as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  coord_flip() + 
  labs(x = "", 
       y = "",
       caption = "Note: Other Class coalitions and covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave("Figures/Plot4.pdf", width = 8.5, height = 5)

########################################################################
## Descriptive Stats.

names(data)
descriptive <- data %>% drop_na() %>%
  select(ccode, year, 
         #regime,
         totalunivers, 
         #v2dlunivl,
         party, military, urban_working, urban_middle,
         lag3pi, 
         #lag3pbranch, lag3plink, lag3porgs, lag3pplats, lag3pcohesv,
         lag3pop, lag3gdp, lag3resdep,
         ww1, ww2, coldwar)

table(descriptive$party)
table(descriptive$military)
table(descriptive$urban_working)
table(descriptive$urban_middle)

desc.stats <- psych::describe(descriptive[, -c(1:2)])[, c(2, 3, 4, 8, 9)] %>%
  rownames_to_column() %>% as_tibble()
names(desc.stats) <- c("Variables", "Obs.", "Mean", "Std.", "Min.", "Max.")  
desc.stats$Variables
desc.stats <- desc.stats %>% mutate(
  Variables = case_when(
    Variables == "totalunivers" ~ "UI_SPaW",
#    Variables == "v2dlunivl" ~ "UI_VDem",
    Variables == "party" ~ "Party Elites",
    Variables == "military" ~ "Military",
    Variables == "urban_working" ~ "Urban Working",
    Variables == "urban_middle" ~ "Urban Middle",
    Variables == "lag3pi" ~ "Party Inst.",
#    Variables == "lag3pbranch" ~ "Branch",
#    Variables == "lag3plink" ~ "Linkage",
#    Variables == "lag3porgs" ~ "Organization",
#    Variables == "lag3pplats" ~ "Platform",
#    Variables == "lag3pcohesv" ~ "Cohension",
    Variables == "lag3pop" ~ "Ln.Pop.",
    Variables == "lag3gdp" ~ "Ln.GDPpc.",
    Variables == "lag3resdep" ~ "Res.Dep.",
    Variables == "ww1" ~ "WWI",
    Variables == "ww2" ~ "WWII",
    Variables == "coldwar" ~ "Cold War",
    T ~ NA_character_ ))


xtable::xtable(desc.stats)


#############################################################
##### Graphs
#############################################################
## v2psplats: distinct party platforms

q.plats <- quantile(autocracies$v2psplats, 
                    probs = c(0.25, 0.5, 0.75, 1), na.rm = T)
sd.plats <- sd(autocracies$v2psplats, na.rm = T)
autocracies <- autocracies %>% mutate(
  q.plats = case_when(
    v2psplats <= q.plats[1] ~ "1st Quantile",
    v2psplats > q.plats[1] & v2psplats <= q.plats[2] ~ "2nd Qunatile",
    v2psplats > q.plats[2] & v2psplats <= q.plats[3] ~ "3rd Qunatile",
    v2psplats > q.plats[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                    "2nd Qunatile",
                                    "3rd Qunatile", 
                                    "4th Quantile"),
                      include_na = F))

Figure_1 <- autocracies %>% drop_na(q.plats) %>% 
  ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.plats, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme_bw()
print(Figure_1)


Figure_2 <- autocracies %>% drop_na(q.plats) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  
  facet_wrap(~ q.plats, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       y = "",
       title = "Histograms on Universalism") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme_bw()
print(Figure_2)

Plot1 <- Figure_1 + Figure_2 + plot_layout(ncol = 2)

ggsave("Figures/Plot1.pdf", width = 8.5, height = 4)
## v2psprlnks: party linkages

q.link <- quantile(autocracies$v2psprlnks, 
                     probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.link = case_when(
    v2psprlnks <= q.link[1] ~ "1st Quantile",
    v2psprlnks > q.link[1] & v2psprlnks <= q.link[2] ~ "2nd Qunatile",
    v2psprlnks > q.link[2] & v2psprlnks <= q.link[3] ~ "3rd Qunatile",
    v2psprlnks > q.link[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_3 <- autocracies %>% drop_na(q.link) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.link, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_4 <- autocracies %>% drop_na(q.link) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.link, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot2 <- Figure_3 + Figure_4 + plot_layout(ncol = 2)
print(Plot2)
ggsave("Figures/Plot2.pdf", width = 8.5, height = 4)

## v2psorgs: party organization
q.orgs <- quantile(autocracies$v2psorgs, 
                   probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.orgs = case_when(
    v2psorgs <= q.orgs[1] ~ "1st Quantile",
    v2psorgs > q.orgs[1] & v2psorgs <= q.orgs[2] ~ "2nd Qunatile",
    v2psorgs > q.orgs[2] & v2psorgs <= q.orgs[3] ~ "3rd Qunatile",
    v2psorgs > q.orgs[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_5 <- autocracies %>% drop_na(q.orgs) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.orgs, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_6 <- autocracies %>% drop_na(q.orgs) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.orgs, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot3 <- Figure_5 + Figure_6 + plot_layout(ncol = 2)
ggsave("Figures/Plot3.pdf", width = 8.5, height = 4)

## v2psprbrch: party branches
q.branch <- quantile(autocracies$v2psprbrch, 
                   probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.branch = case_when(
    v2psprbrch <= q.branch[1] ~ "1st Quantile",
    v2psprbrch > q.branch[1] & v2psprbrch <= q.branch[2] ~ "2nd Qunatile",
    v2psprbrch > q.branch[2] & v2psprbrch <= q.branch[3] ~ "3rd Qunatile",
    v2psprbrch > q.branch[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_7 <- autocracies %>% drop_na(q.branch) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.branch, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_8 <- autocracies %>% drop_na(q.branch) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.branch, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot4 <- Figure_7 + Figure_8 + plot_layout(ncol = 2)
print(Plot4)
ggsave("Figures/Plot4.pdf", width = 8.5, height = 4)

## v2pscohesv: legislative party cohesion
q.cohesv <- quantile(autocracies$v2pscohesv, 
                     probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.cohesv = case_when(
    v2pscohesv <= q.cohesv[1] ~ "1st Quantile",
    v2pscohesv > q.cohesv[1] & v2pscohesv <= q.cohesv[2] ~ "2nd Qunatile",
    v2pscohesv > q.cohesv[2] & v2pscohesv <= q.cohesv[3] ~ "3rd Qunatile",
    v2pscohesv > q.cohesv[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_9 <- autocracies %>% drop_na(q.cohesv) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.cohesv, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_10 <- autocracies %>% drop_na(q.cohesv) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.cohesv, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot5 <- Figure_9 + Figure_10 + plot_layout(ncol = 2)
print(Plot5)
ggsave("Figures/Plot5.pdf", width = 8.5, height = 4)

## v2xps_party: Party institutionalization index
q.pi <- quantile(autocracies$v2xps_party, 
                     probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.pi = case_when(
    v2xps_party <= q.pi[1] ~ "1st Quantile",
    v2xps_party > q.pi[1] & v2xps_party <= q.pi[2] ~ "2nd Qunatile",
    v2xps_party > q.pi[2] & v2xps_party <= q.pi[3] ~ "3rd Qunatile",
    v2xps_party > q.pi[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_11 <- autocracies %>% drop_na(q.pi) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.pi, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_12 <- autocracies %>% drop_na(q.pi) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.pi, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot6 <- Figure_11 + Figure_12 + plot_layout(ncol = 2)
print(Plot6)
ggsave("Figures/Plot6.pdf", width = 8.5, height = 4)

## v2dlencmps: Particularistic or public goods

q.good <- quantile(autocracies$v2dlencmps, 
                   probs = c(0.25, 0.5, 0.75, 1), na.rm = T)

autocracies <- autocracies %>% mutate(
  q.good = case_when(
    v2dlencmps <= q.good[1] ~ "1st Quantile",
    v2dlencmps > q.good[1] & v2dlencmps <= q.good[2] ~ "2nd Qunatile",
    v2dlencmps > q.good[2] & v2dlencmps <= q.good[3] ~ "3rd Qunatile",
    v2dlencmps > q.good[3] ~ "4th Quantile",
    TRUE ~ NA_character_
  ) %>% parse_factor(., levels = c("1st Quantile",
                                   "2nd Qunatile",
                                   "3rd Qunatile", 
                                   "4th Quantile"),
                     include_na = F))

Figure_13 <- autocracies %>% drop_na(q.good) %>% ggplot(aes(totalencomp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.good, nrow = 2) + 
  labs(x = "Encompassingness of Welfare Program",
       y = "Percent (%)", 
       title = "Histograms on Encompassingness") + 
  theme_bw()

Figure_14 <- autocracies %>% drop_na(q.good) %>% 
  dplyr::filter(totalunivers!=0) %>% ggplot(aes(totalunivers)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  facet_wrap(~ q.good, nrow = 2) + 
  labs(x = "Univeralsim of Welfare Program",
       title = "Histograms on Universalism") + 
  theme_bw()

Plot7 <- Figure_13 + Figure_14 + plot_layout(ncol = 2)
print(Plot7)
ggsave("Figures/Plot7.pdf", width = 8.5, height = 4)


table(autocracies$class)

################################################################################
##### mean of party institutionalization by classes in authoritarian regime ####
################################################################################

summary.pi <- autocracies %>%
  group_by(class) %>% 
  summarise(pi = mean(v2xps_party, na.rm = T))
summary.pi  

autocracies %>% 
    dplyr::filter(v2regimpgroup == c(10, 12)) %>% ggplot(aes(x = v2xps_party)) + 
  geom_density(aes(x = v2xps_party, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(v2regimpgroup))

autocracies %>% group_by(v2regimpgroup) %>%
  dplyr::filter(v2regimpgroup == c(10, 12)) %>%
  summarize(mean = mean(v2xps_party, na.rm = T),
            median = median(v2xps_party, na.rm = T),
            sd = sd(v2xps_party, na.rm = T))

autocracies %>% 
  dplyr::filter(class %in% 
                  c("Party elites","The military",
                    "Working classes", "Urban Middle")) %>%
  ggplot(aes(x = v2xps_party)) + 
  geom_density(aes(x = v2xps_party, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) +
  geom_vline(data=filter(summary.pi, class=="Party elites"), 
             aes(xintercept=pi), colour="black") +
  geom_text(
    data    = filter(summary.pi, class=="Party elites"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.515") + 
  geom_vline(data=filter(summary.pi, class=="The military"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="The military"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.364") + 
  geom_vline(data=filter(summary.pi, class=="Working classes"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="Working classes"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.661") + 
  geom_vline(data=filter(summary.pi, class=="Urban Middle"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="Urban Middle"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.590") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  labs(x = "Party Institutionalization",
       y = "Density") + 
  facet_wrap(.~class, ncol = 2) + 
  theme_bw() + 
  theme(legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(
      margin = margin(t = 25, b = 10)),
    axis.title.y = element_text(
      margin = margin(r = 25, l = 10)))


## Appendix
summary.pi
autocracies %>% 
  dplyr::filter(class %in% 
                  c("The ethnic/racial/religous groups",
                    "Agrarian/local elites",
                    "Business elites/civil servants",
                    "Rural Middle",
                    "The aristocracy",
                    "A foreign govt. or colonial power")) %>%
  ggplot(aes(x = v2xps_party)) + 
  #  geom_histogram(aes(y = ..density.., fill = regime)) + 
  geom_density(aes(x = v2xps_party, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) +
  geom_vline(data=filter(summary.pi, class=="The ethnic/racial/religous groups"), 
             aes(xintercept=pi), colour="black") +
  geom_text(
    data    = filter(summary.pi, class=="The ethnic/racial/religous groups"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.386") + 
  geom_vline(data=filter(summary.pi, class=="Agrarian/local elites"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="Agrarian/local elites"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.392") + 
  geom_vline(data=filter(summary.pi, class=="Business elites/civil servants"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="Business elites/civil servants"),
    mapping = aes(x = pi - 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.397") + 
  geom_vline(data=filter(summary.pi, class=="Rural Middle"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="Rural Middle"),
    mapping = aes(x = pi + 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.261") + 
  geom_vline(data=filter(summary.pi, class=="The aristocracy"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="The aristocracy"),
    mapping = aes(x = pi + 0.2, y = 2.2), size = 3,
    label = "Mean P.I. = 0.248") + 
  geom_vline(data=filter(summary.pi, class=="A foreign govt. or colonial power"), 
             aes(xintercept=pi), colour="black") + 
  geom_text(
    data    = filter(summary.pi, class=="A foreign govt. or colonial power"),
    mapping = aes(x = pi - 0.2, y = 2.4), size = 3,
    label = "Mean P.I. = 0.431") + 
  
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Party Institutionalization",
       y = "Density") + 
  facet_wrap(~class, ncol = 2) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(
      margin = margin(t = 25, b = 10)),
    axis.title.y = element_text(
      margin = margin(r = 25, l = 10))
  )


#########################################################
##### Lagging the variables: 1-year, 3-year, 5-year #####
#########################################################

names(autocracies)
autocracies <- autocracies %>% mutate(
  lag1v2psorgs = lag(v2psorgs, 1),
  lag1v2psprbrch = lag(v2psprbrch, 1),
  lag1v2psplats = lag(v2psplats, 1),
  lag1v2psprlnks = lag(v2psprlnks, 1),
  lag1v2pscohesv = lag(v2pscohesv, 1),
  lag1pi = lag(v2xps_party, 1),
  lag1v2dlencmps = lag(v2dlencmps, 1),
  lag1class = lag(class, 1),
  lag1class_nu = lag(class.nu, 1),
  lag1conflict = lag(e_miinteco, 1),
  lag1exports = lag(e_cow_imports, 1),
  lag1growth = lag(e_migdpgro, 1),
  lag1gdppc = lag(e_migdppc, 1),
  lag1milsize = lag(cow_milsize, 1),
  lag1milex = lag(wdi_expmil, 1),
  lag1gdppc = lag(log(rgdpnapc), 1),
  lag1resdep = lag(resdep2, 1),
  lag1urban = lag(urban, 1),
  lag1trade = lag(wdi_trade, 1),
  lag1pop14 = lag(wdi_pop14, 1),
  lag1pop65 = lag(wdi_pop65, 1),
  lag3v2psorgs = lag(v2psorgs, 3),
  lag3v2psprbrch = lag(v2psprbrch, 3),
  lag3v2psplats = lag(v2psplats, 3),
  lag3v2psprlnks = lag(v2psprlnks, 3),
  lag3v2pscohesv = lag(v2pscohesv, 3),
  lag3pi = lag(v2xps_party, 3),
  lag3v2dlencmps = lag(v2dlencmps, 3),
  lag3class = lag(class, 3),
  lag3class_nu = lag(class.nu, 3),
  lag3conflict = lag(e_miinteco, 3),
  lag3exports = lag(e_cow_imports, 3),
  lag3growth = lag(e_migdpgro, 3),
  lag3gdppc = lag(e_migdppc, 3),
  lag3milsize = lag(cow_milsize, 3),
  lag3milex = lag(wdi_expmil, 3),
  lag3gdppc = lag(log(rgdpnapc), 3),
  lag3resdep = lag(resdep2, 3),
  lag3urban = lag(urban, 3),
  lag3trade = lag(wdi_trade, 3),
  lag3pop14 = lag(wdi_pop14, 3),
  lag3pop65 = lag(wdi_pop65, 3),
  lag5v2psorgs = lag(v2psorgs, 5),
  lag5v2psprbrch = lag(v2psprbrch, 5),
  lag5v2psplats = lag(v2psplats, 5),
  lag5v2psprlnks = lag(v2psprlnks, 5),
  lag5v2pscohesv = lag(v2pscohesv, 5),
  lag5pi = lag(v2xps_party, 5),
  lag5v2dlencmps = lag(v2dlencmps, 5),
  lag5class = lag(class, 5),
  lag5class_nu = lag(class.nu, 5),
  lag5conflict = lag(e_miinteco, 5),
  lag5exports = lag(e_cow_imports, 5),
  lag5growth = lag(e_migdpgro, 5),
  lag5gdppc = lag(e_migdppc, 5),
  lag5milsize = lag(cow_milsize, 5),
  lag5milex = lag(wdi_expmil, 5),
  lag5gdppc = lag(log(rgdpnapc), 5),
  lag5resdep = lag(resdep2, 5),
  lag5urban = lag(urban, 5),
  lag5trade = lag(wdi_trade, 5),
  lag5pop14 = lag(wdi_pop14, 5),
  lag5pop65 = lag(wdi_pop65, 5)
)
autocracies <- autocracies[!duplicated(autocracies[c("Ccodecow", "year")]),]
names(autocracies)
###############################################################
##### Export the data to STATA for xtpcse 
###############################################################
table(autocracies$class)
base_sample <- autocracies %>% mutate(
  country_abb = countrycode(autocracies$ccode, 
                             "cown", "p4c"))

panel.base <- base_sample %>% 
  select(country_name, year, 
         totalunivers, class, class.nu, country_abb, e_migdppc, e_mipopula, resdep2)
table(panel.base$class)
panel.base <- panel.base %>% mutate(
  class_core = case_when(
    class == "Party elites" ~ "Party elites",
    class == "The military" ~ "The military",
    class == "Urban Working" ~ "Urban Working",
    class == "Urban Middle" ~ "Urban Middle",
    class == NA ~ NA_character_,
    T ~ "Others") %>% 
    parse_factor(., levels = c("Party elites",
                               "The military",
                               "Urban Working",
                               "Urban Middle",
                               "Others"), include_na = F, ordered = T),
  core_nu = as.numeric(class_core)
)
table(panel.base$class_core)
mycol<-brewer.pal(5,"Set2")[c(2,1,3,4,5)]
pal <- wes_palette(4, name = "Royal2", type = "continuous")
t <- panel.base %>% drop_na() %>% dplyr::filter(year > 1916 & year < 2001)
table(t$class_core)
table(t$class)
table(panel.base$class)
panel.base %>% drop_na() %>% dplyr::filter(year > 1916 & year < 2001) %>%
  panelView(totalunivers ~ core_nu + e_migdppc + e_mipopula + resdep2,
          index = c("country_abb", "year"),
          xlab = "Year", ylab = "State",
          main = "", gridOff = FALSE,
          color = mycol,
          id = unique(panel.base$country_abb), axis.lab.gap = c(5,0),
          background = "white", by.timing = TRUE,
          legend.labs = c("Party elites",
                          "The military",
                          "Urban Working",
                          "Urban Middle",
                          "Others"))
ggsave("Figures/Appendix/Appendix1_sample.pdf", width = 10.0, height = 11.0)
round(700/(700 + 837 + 133 + 61 + 975), 3)*100
round(837/(700 + 837 + 133 + 61 + 975), 3)*100
round(133/(700 + 837 + 133 + 61 + 975), 3)*100
round(61/(700 + 837 + 133 + 61 + 975), 3)*100
round(975/(700 + 837 + 133 + 61 + 975), 3)*100

panel.cov <- base_sample %>% 
  select(country_name, year, 
         totalunivers, class, class.nu, country_abb)



table_grouped <- autocracies %>% 
  group_by(class) %>% 
  drop_na() %>%
  count(Ccodecow) %>% mutate(
    total_count = sum(n)
  )
table_aggregated <- table_grouped %>%
  select(class, total_count) %>% unique()
View(table_aggregated)

sum <- autocracies %>% group_by(class, year) %>% 
  drop_na(lag3v2psorgs, lag3v2psprbrch, lag3v2psplats, lag3v2psprlnks,
          lag3v2dlencmps,lag3pi, univers_oldageprog, univers_mater_prog, 
          univers_sick_prog, univers_working_prog, univers_unemp_prog,
          univers_familiy_prog) %>%
  summarize(
    org = mean(lag3v2psorgs, na.rm = T),
    bran = mean(lag3v2psprbrch, na.rm = T),
    plat = mean(lag3v2psplats, na.rm = T),
    link = mean(lag3v2psprlnks, na.rm = T),
    coh = mean(lag3v2pscohesv, na.rm = T),
    pi = mean(lag3pi, na.rm = T),
    type = mean(lag3v2dlencmps, na.rm = T),
    old = mean(univers_oldageprog, na.rm = T),
    mater = mean(univers_mater_prog, na.rm = T),
    sick = mean(univers_sick_prog, na.rm = T),
    work = mean(univers_working_prog, na.rm = T),
    unemp = mean(univers_unemp_prog, na.rm = T),
    fam = mean(univers_familiy_prog, na.rm = T)
  )
grouped <- sum %>% 
  tidyr::gather(key = "variable", value = "value", -class, -year)

piclass <- grouped %>% dplyr::filter(variable == "pi" & class!="Other groups") %>%
  drop_na(class) %>% 
  ggplot(aes(x = year, y = value)) + geom_jitter() +  geom_line() + 
  geom_hline(yintercept = mean(grouped$value), color = "red") + 
  facet_wrap(~class, ncol = 2) + theme_bw()
print(piclass)
appendix2 <- grouped %>% dplyr::filter(variable %in% 
                            c("org", "bran", "plat", "link", "coh", "type")) %>%
  drop_na(class) %>% 
  ggplot(aes(x = year, y = value, color = variable)) + geom_jitter() +  geom_line() + 
  facet_wrap(~class, ncol = 2) + theme_bw() + theme(
    legend.position = "bottom"
  )



grouped %>% dplyr::filter(class == "Agrarian Local elites") %>% summary()



platform_m <- pick("platform.csv")

platform_m <- platform_m %>% select(-c(X6, X7)) %>% drop_na() %>%
  mutate(
    Regime = Regime %>%
      parse_factor(., 
                   levels = c("Monarchy", 
                              "Military", 
                              "Dominant Party", 
                              "Personalist"), 
                   include_na = F, ordered = TRUE))

p1 <- platform_m %>% 
  ggplot(aes(x = Platform, y = margins, color = Regime)) + 
  geom_point(show.legend = F) + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), show.legend = F) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Distinctive Platform", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p1
Linkages_m <- pick("Linkages.csv")

Linkages_m <- Linkages_m %>% 
  drop_na() %>% 
  mutate(
    Regime = Regime %>%
  parse_factor(., 
               levels = c("Monarchy", 
                          "Military", 
                          "Dominant Party", 
                          "Personalist"), 
               include_na = F, ordered = TRUE))
p2 <- Linkages_m %>% 
  ggplot(aes(x = `Constitency Linkage`, y = margins, color = Regime)) + 
  geom_point() + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Constitency Linkage", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p2

Goods_m <- pick("goods.csv")

Goods_m <- Goods_m %>% 
  drop_na() %>% 
  mutate(
    Regime = Regime %>%
      parse_factor(., 
                   levels = c("Monarchy", 
                              "Military", 
                              "Dominant Party", 
                              "Personalist"), 
                   include_na = F, ordered = TRUE))

p3 <- Goods_m %>% 
  ggplot(aes(x = Goods, y = margins, color = Regime)) + 
  geom_point(show.legend = F) + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), show.legend = F) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Type of Goods", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p3
p1 + p2 + p3 + plot_layout(ncol = 3)


nonplatform_m <- pick("non_platform.csv")

nonplatform_m <- nonplatform_m %>%  drop_na() %>%
  mutate(
    Regime = Regime %>%
      parse_factor(., 
                   levels = c("Monarchy", 
                              "Military", 
                              "Dominant Party", 
                              "Personalist"), 
                   include_na = F, ordered = TRUE))

p4 <- nonplatform_m %>% 
  ggplot(aes(x = Platform, y = margins, color = Regime)) + 
  geom_point(show.legend = F) + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), show.legend = F) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Distinctive Platform", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p4
nonlink_m <- pick("non_link.csv")

nonlink_m <- nonlink_m %>% 
  drop_na() %>% 
  mutate(
    Regime = Regime %>%
      parse_factor(., 
                   levels = c("Monarchy", 
                              "Military", 
                              "Dominant Party", 
                              "Personalist"), 
                   include_na = F, ordered = TRUE))
p5 <- nonlink_m %>% 
  ggplot(aes(x = linkage, y = margins, color = Regime)) + 
  geom_point() + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Constitency Linkage", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p5

nongoods_m <- pick("non_goods.csv")

nongoods_m <- nongoods_m %>% 
  drop_na() %>% 
  mutate(
    Regime = Regime %>%
      parse_factor(., 
                   levels = c("Monarchy", 
                              "Military", 
                              "Dominant Party", 
                              "Personalist"), 
                   include_na = F, ordered = TRUE))

p6 <- nongoods_m %>% 
  ggplot(aes(x = Goods, y = margins, color = Regime)) + 
  geom_point(show.legend = F) + geom_line(show.legend = F) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), show.legend = F) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Levels of Type of Goods", 
       y = "Predictive Margins of Regime Type with 95% CI") + 
  theme_bw() + theme(legend.position = "bottom")
p6
p4 + p5 + p6 + plot_layout(ncol = 3)



# For model 1

mdata <- mdata[!duplicated(mdata[c("Ccodecow", "year")]),]



## make it as panel data
model1_p <- model1
model1_p <- model1_p[!duplicated(model1_p[c("Ccodecow", "year")]),]
model1_p <- model1_p %>% drop_na(Ccodecow, year)
model1_p <- pdata.frame(model1_p, index=c("Ccodecow","year"))
model1_lag <- model1_p %>% mutate(
  lagv2psplats = lag(v2psplats, 3),
  lagv2psprlnks = lag(v2psprlnks, 3),
  lagv2dlencmps = lag(v2dlencmps, 3),
  lagmilsize = lag(cow_milsize, 3),
  laggdppc = lag(log(rgdpnapc), 3),
  lagresdep = lag(resdep2, 3),
  lagurban = lag(urban, 3),
  lagtrade = lag(wdi_trade, 3),
  lagpop = lag(log(wdi_pop), 3),
  lagpop14 = lag(wdi_pop14, 3),
  lagpop65 = lag(wdi_pop65, 3)
)
model1_lag <- as_tibble(model1_lag)
model1_lag <- model1_lag %>% mutate(
  year = as.numeric(as.character(year))
)
glimpse(model1_lag)
model1_lag <- model1_lag %>% drop_na(Ccodecow, year)
library(foreign)
write.dta(model1_lag, "model1.dta")
#model1_pna <- model1_p %>% drop_na()
#pdim(model_1pna)
table(is.na(model1_lag$year))
library(panelAR)
model1.tot.encom <- panelAR(totalencomp ~ lagv2psplats + lagv2psprlnks + 
                         lagv2dlencmps + lagmilsize + 
                         laggdppc + lagresdep + lagurban + 
                         lagtrade + lagpop + lagpop14 + lagpop65, 
                       data = model1_lag, 
                       panelVar="Ccodecow", 
                       timeVar="year", 
                       autoCorr="psar1",
                       panelCorrMethod="pcse", 
                       complete.case=FALSE)

tidy_model1_t <- broom::tidy(summary(model1.tot.encom)$coef)


model1.tot.univ <- panelAR(totalunivers ~ lag(v2psplats, 3) + lag(v2psprlnks, 3) + 
                              lag(v2dlencmps, 3) +
                              as.factor(year) + as.factor(Ccodecow), 
                            data = model1_p, 
                            panelVar="Ccodecow", 
                            timeVar="year", 
                            autoCorr="psar1",
                            panelCorrMethod="pcse", 
                           complete.case=FALSE,
                           rho.na.rm = TRUE)
summary(model1.tot.univ)
summary(model1.tot.univ)
tidy_model1_tuniv <- broom::tidy(summary(model1.tot.univ)$coef)

model2 <- mdata
## make it as panel data
model2_p <- model2
model2_p <- model2_p[!duplicated(model2_p[c("Ccodecow", "year")]),]
model2_p <- model2_p %>% drop_na(Ccodecow, year)
model2_p <- pdata.frame(model2_p, index=c("Ccodecow","year"))
model2_lag <- model2_p %>% mutate(
  lagregime = lag(gwf_regime, 3),
  lagv2psplats = lag(v2psplats, 3),
  lagv2psprlnks = lag(v2psprlnks, 3),
  lagv2dlencmps = lag(v2dlencmps, 3),
  lagmilsize = lag(cow_milsize, 3),
  laggdppc = lag(log(rgdpnapc), 3),
  lagresdep = lag(resdep2, 3),
  lagurban = lag(urban, 3),
  lagtrade = lag(wdi_trade, 3),
  lagpop = lag(log(wdi_pop), 3),
  lagpop14 = lag(wdi_pop14, 3),
  lagpop65 = lag(wdi_pop65, 3)
)
model2_lag <- as_tibble(model2_lag)
model2_lag <- model2_lag %>% mutate(
  year = as.numeric(as.character(year))
)
glimpse(model1_lag)
model1_lag <- model1_lag %>% drop_na(Ccodecow, year)
library(foreign)
write.dta(model2_lag, "model2.dta")








