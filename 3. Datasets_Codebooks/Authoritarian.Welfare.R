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

Vdem <- readRDS("Country_Year_V-Dem_Full+others_R_v9/Country_Year_V-Dem_Full+others_R_v9/V-Dem-CY-Full+Others-v9.rds")
Vdem.s <- 
  Vdem %>% 
  select(country_name, COWcode, year, e_lexical_index, v2x_polyarchy,
         v2xps_party, v2dlunivl,
         v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv, 
         v2dlencmps, v2regimpgroup, v2regsupgroupssize,
         e_cow_exports, e_cow_imports, e_migdpgro, e_migdppc, e_mipopula,
         e_miurbpop, e_miinteco, e_civil_war, v2x_corr, v2clrspct,
         v2x_genpp, v2x_cspart)
rm(Vdem)
#############################################################
### Welfare coverage and law
#############################################################
SPaW <- pick("SPaW_ver2.dta")

#############################################################
###  and Population
#############################################################

#############################################################
### Resources and Military size
#############################################################

Miller <- pick("Miller2015.dta")
Miller <- Miller %>% 
  select(ccode, year, resdep2, urban, cow_milsize)

#############################################################
### Control Vaiables
#############################################################
#QOG <-  pick("qog_std_ts_jan19.csv")
#QOG.ctrl <- QOG %>% 
#  select(ccodecow, year, 
#         wdi_trade, wdi_pop14, wdi_pop65, wdi_expmil)

#############################################################
### Make a Base_line dataset
#############################################################

mdata <- merge(x = Vdem.s, y=SPaW, by.x=c("COWcode", "year"),
               by.y = c("Ccodecow", "year"), all.x = TRUE)
mdata <- merge(x = mdata, y=Miller, by.x=c("COWcode", "year"),
               by.y = c("ccode", "year"), all.x = TRUE)
#mdata <- merge(x = mdata, y=QOG.ctrl, by.x = c("COWcode", "year"),
#               by.y = c("ccodecow", "year"), all.x = TRUE)
foreign::write.dta(mdata, "mdata.dta")

names(mdata)



mdata <- mdata %>% select(
  everything(), e_cow_exports, e_cow_imports, v2x_corr, v2clrspct,
  v2x_genpp, v2x_cspart
)
names(mdata)
#############################################################
### Aggregate welfare variables
#############################################################
mdata <- mdata %>% mutate(
  totalunivers = univers_oldageprog + 
    univers_familiy_prog + univers_mater_prog + 
    univers_oldageprog + univers_sick_prog + 
    univers_unemp_prog + univers_working_prog)

#normalize <- function(x) {
# return ((x - min(x)) / (max(x) - min(x)))
# }

#############################################################
##### Subset the sample with only authoritarian regimes #####
#############################################################
mdata <- mdata %>% mutate(
  regime = case_when(
    e_lexical_index < 6 ~ "Autocracies",
    e_lexical_index ==6 ~ "Democracies"
  ) %>% parse_factor(., levels = c("Autocracies", "Democracies"),
                     include_na = F, ordered = T)
)
table(mdata$regime)
autocracies <- mdata %>% dplyr::filter(regime == "Autocracies")

autocracies <- autocracies %>%
  mutate(
    ccode = countrycode(country_name, "country.name", "cown")
  ) %>% drop_na(ccode)
autocracies$COWcode <- NULL
### Sample describe
autocracies <- autocracies %>% drop_na(ccode, year)
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
  scale_x_discrete(labels=c("Autocracies" = "Autocracies (n=13108)", 
                            "Democracies" = "Democracies (n=4773)")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  annotate(geom = "rect", xmin = 0.75, xmax = 1.25,
           ymin = 0.5, ymax = 0.85, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 1, y = 0.9, label = "3.64% of Autocracies"
  ) +
  annotate(geom = "rect", xmin = 1.75, xmax = 2.25,
           ymin = 0.1, ymax = 0.5, fill = "blue", alpha = 0.1) + 
  annotate(
    "text", x = 2, y = 0.05, label = "13.37% of Democracies"
  ) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 3))
print(Plot1)
ggsave("Figures/Plot1.pdf", width = 8.5, height = 4)




######################################################
##### Supporting groups in authoritarian regimes #####
######################################################

autocracies <- autocracies %>% mutate(
  class_raw = case_when(
    v2regimpgroup == 2L ~ "Party elites",
    v2regimpgroup == 5L ~ "The military",
    v2regimpgroup == 6L ~ "The ethnic/racial groups",
    v2regimpgroup == 7L ~ "The religous groups",
    v2regimpgroup == 1L ~ "Agrarian elites",
    v2regimpgroup == 8L ~ "Local elites",
    v2regimpgroup == 3L ~ "Business elites",
    v2regimpgroup == 4L ~ "Civil servants",
    v2regimpgroup == 9L ~ "Urban Working",
    v2regimpgroup == 11L ~ "Rural Working",
    v2regimpgroup == 10L ~ "Urban Middle",
    v2regimpgroup == 12L ~ "Rural Middle",
    v2regimpgroup == 0L ~ "The aristocracy",
    v2regimpgroup == 13L ~ "A foreign govt or colonial",
    v2regimpgroup == NA ~ NA_character_,
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
table(autocracies$class_raw)

autocracies <- autocracies %>% mutate(
  class = case_when(
    v2regimpgroup == 2L ~ "Party elites",
    v2regimpgroup == 5L ~ "The military",
    v2regimpgroup == 6L ~ "The ethnic/racial/religous groups",
    v2regimpgroup == 7L ~ "The ethnic/racial/religous groups",
    v2regimpgroup == 1L ~ "Agrarian/local elites",
    v2regimpgroup == 8L ~ "Agrarian/local elites",
    v2regimpgroup == 3L ~ "Business elites/civil servants",
    v2regimpgroup == 4L ~ "Business elites/civil servants",
    v2regimpgroup == 9L ~ "Working classes",
    v2regimpgroup == 11L ~ "Working classes",
    v2regimpgroup == 10L ~ "Urban Middle",
    v2regimpgroup == 12L ~ "Rural Middle",
    v2regimpgroup == 0L ~ "The aristocracy",
    v2regimpgroup == 13L ~ "A foreign govt. or colonial power",
    T ~ NA_character_
  ) %>% 
    parse_factor(., levels = c("Party elites",
                               "The military",
                               "The ethnic/racial/religous groups",
                               "Agrarian/local elites",
                               "Business elites/civil servants",
                               "Working classes",
                               "Urban Middle",
                               "Rural Middle",
                               "The aristocracy",
                               "A foreign govt. or colonial power"), 
                 include_na = F, ordered = T),
  class.nu = as.numeric(class)
) %>% drop_na(class)

names(autocracies)

autocracies <- autocracies %>% 
  select(
    country_name, ccode, year, e_lexical_index, v2x_polyarchy, regime,
    v2xps_party, v2dlunivl, v2psprbrch, v2psprlnks, v2psorgs,
    v2psplats, v2pscohesv, v2dlencmps, v2regimpgroup, v2regsupgroupssize,
    class_raw, class_raw.nu,
    class, class.nu, totalunivers, univers_oldageprog, univers_mater_prog,
    univers_sick_prog, univers_working_prog, univers_unemp_prog, 
    univers_familiy_prog, resdep2, urban, cow_milsize, e_migdpgro, e_mipopula,
    e_migdppc, e_miurbpop, e_miinteco, e_civil_war, everything())
    
#############################################################
### Bivariate relationship between Welfare and Classes
#############################################################

class <-  c("Party elites", "The military", "Agrarian/local elites",
            "Working classes", "Urban Middle")

names(autocracies)
class_summary <- autocracies %>% 
  select(class, totalunivers, v2regsupgroupssize, v2dlunivl, year) %>%
  dplyr::filter(class %in% c("Party elites", "The military", 
                           "Working classes", "Urban Middle") & 
                  year > 1916) %>%
  group_by(class) %>% 
  summarise(mean.w1 = mean(totalunivers, na.rm = T),
            sd.w1 = sd(totalunivers, na.rm = T),
            lower.w1 = mean(totalunivers, na.rm = T) - sd(totalunivers, na.rm = T),
            upper.w1 = mean(totalunivers, na.rm = T) + sd(totalunivers, na.rm = T),
            mean.w2 = mean(v2dlunivl, na.rm = T),
            sd.w2 = sd(v2dlunivl, na.rm = T),
            lower.w2 = mean(v2dlunivl, na.rm = T) - sd(v2dlunivl, na.rm = T),
            upper.w2 = mean(v2dlunivl, na.rm = T) + sd(v2dlunivl, na.rm = T))

summary.w <- autocracies %>% dplyr::filter(year > 1916) %>%
  group_by(class) %>% 
  summarise(mean.w1 = mean(totalunivers, na.rm = T),
            mean.w2 = mean(v2dlunivl, na.rm = T),
            mean.size = mean(v2regsupgroupssize, na.rm = T))

Figure2_1 <- autocracies %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                           "Working classes", "Urban Middle") & year > 1916) %>% 
  ggplot(aes(x = totalunivers)) + 
  geom_density(aes(x = totalunivers, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(class)) + 
  geom_vline(data=filter(summary.w, class=="Party elites"), 
             aes(xintercept=mean.w1), colour="black") +
  geom_text(
    data    = filter(summary.w, class=="Party elites"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 17.5") +
  geom_vline(data=filter(summary.w, class=="The military"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="The military"),
    map = aes(x = mean.w1 + 5, y = 0.05), size = 3,
    label = "Mean U.I. = 14.7") + 
  geom_vline(data=filter(summary.w, class=="Working classes"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Working classes"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 16.5") + 
  geom_vline(data=filter(summary.w, class=="Urban Middle"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Urban Middle"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 23.5") + 
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
print(Figure2_1)


Figure2_2 <- autocracies %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Working classes", "Urban Middle") & year > 1916) %>% 
  ggplot(aes(x = v2dlunivl)) + 
  geom_density(aes(x = v2dlunivl, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(class)) + 
  geom_vline(data=filter(summary.w, class=="Party elites"), 
             aes(xintercept=mean.w2), colour="black") +
  geom_text(
    data    = filter(summary.w, class=="Party elites"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = 0.395") + 
  geom_vline(data=filter(summary.w, class=="The military"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="The military"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.316") + 
  geom_vline(data=filter(summary.w, class=="Working classes"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Working classes"),
    map = aes(x = mean.w2 + 0.2, y = 0.5), size = 3,
    label = "Mean U.I. = 1.26") + 
  geom_vline(data=filter(summary.w, class=="Urban Middle"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Urban Middle"),
    map = aes(x = mean.w2 + 0.2, y = 0.5), size = 3,
    label = "Mean U.I. = 0.118") + 
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Universalism Index of VDem",
       y = "") + 
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
print(Figure2_2)
Plot2 <- Figure2_1 + Figure2_2 + plot_layout(ncol = 2)
print(Plot2)
ggsave("Figures/Plot2.pdf", width = 8.5, height = 5)

### Other groups
autocracies %>% dplyr::filter(class=="Rural Middle") %>% 
  ggplot(aes(x = totalunivers))
  geom_density(aes(x = totalunivers, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F)
  
summary.w
Figure2_3 <- autocracies %>% 
  dplyr::filter(class %in% c("The ethnic/racial/religous groups",
                             "Agrarian/local elites",
                             "Business elites/civil servants", 
 #                            "Rural Middle",
                             "The aristocracy",
                             "A foreign govt. or colonial power") & 
                  year > 1916) %>% 
  ggplot(aes(x = totalunivers)) + 
  geom_density(aes(x = totalunivers, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(class)) + 
  geom_vline(data=filter(summary.w, class=="The ethnic/racial/religous groups"), 
             aes(xintercept=mean.w1), colour="black") +
  geom_text(
    data    = filter(summary.w, class=="The ethnic/racial/religous groups"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 11.6") +
  geom_vline(data=filter(summary.w, class=="Agrarian/local elites"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Agrarian/local elites"),
    map = aes(x = mean.w1 + 5, y = 0.05), size = 3,
    label = "Mean U.I. = 8.48") + 
  geom_vline(data=filter(summary.w, class=="Business elites/civil servants"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Business elites/civil servants"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 12.5") + 
#  geom_vline(data=filter(summary.w, class=="Rural Middle"), 
#             aes(xintercept=mean.w1), colour="black") + 
#  geom_text(
 #   data    = filter(summary.w, class=="Rural Middle"),
#    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
#    label = "Mean U.I. = 18") + 
  geom_vline(data=filter(summary.w, class=="The aristocracy"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="The aristocracy"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 5.14") + 
  geom_vline(data=filter(summary.w, class=="A foreign govt. or colonial power"), 
             aes(xintercept=mean.w1), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="A foreign govt. or colonial power"),
    map = aes(x = mean.w1 + 2, y = 0.05), size = 3,
    label = "Mean U.I. = 13.9") + 
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
print(Figure2_3)


Figure2_4 <- autocracies %>% 
  dplyr::filter(class %in% c("The ethnic/racial/religous groups",
                             "Agrarian/local elites",
                             "Business elites/civil servants", 
                             "Rural Middle",
                             "The aristocracy",
                             "A foreign govt. or colonial power") & 
                  year > 1916) %>% 
  ggplot(aes(x = v2dlunivl)) + 
  geom_density(aes(x = v2dlunivl, fill = class), 
               alpha = 0.2, size = 0.2, show.legend = F) + 
  facet_wrap(~as.factor(class)) + 
  geom_vline(data=filter(summary.w, class=="The ethnic/racial/religous groups"), 
             aes(xintercept=mean.w2), colour="black") +
  geom_text(
    data    = filter(summary.w, class=="The ethnic/racial/religous groups"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.09") +
  geom_vline(data=filter(summary.w, class=="Agrarian/local elites"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Agrarian/local elites"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.9") + 
  geom_vline(data=filter(summary.w, class=="Business elites/civil servants"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Business elites/civil servants"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = 0.27") + 
  geom_vline(data=filter(summary.w, class=="Rural Middle"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Rural Middle"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.72") + 
  geom_vline(data=filter(summary.w, class=="The aristocracy"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="The aristocracy"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.28") + 
  geom_vline(data=filter(summary.w, class=="A foreign govt. or colonial power"), 
             aes(xintercept=mean.w2), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="A foreign govt. or colonial power"),
    map = aes(x = mean.w2 + 0.2, y = 0.4), size = 3,
    label = "Mean U.I. = -0.07") + 
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  labs(x = "Universalism Index of VDem",
       y = "") + 
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
print(Figure2_2)
Appendix2 <- Figure2_3 + Figure2_4 + plot_layout(ncol = 2)
print(Appendix2)
ggsave("Figures/Plot2.pdf", width = 8.5, height = 5)


### support group size

autocracies %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Working classes", "Urban Middle") & year > 1916) %>% 
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
  geom_vline(data=filter(summary.w, class=="Working classes"), 
             aes(xintercept=mean.size), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Working classes"),
    map = aes(x = mean.size + 0.5, y = 0.05), size = 3,
    label = "Mean size = 0.719") + 
  geom_vline(data=filter(summary.w, class=="Urban Middle"), 
             aes(xintercept=mean.size), colour="black") + 
  geom_text(
    data    = filter(summary.w, class=="Urban Middle"),
    map = aes(x = mean.size + 0.5, y = 0.05), size = 3,
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
                             "Working classes", "Urban Middle")) %>% 
  group_by(class, year) %>% summarize(n())

label <- c("Party elites","The military", "The ethnic/racial/religous groups",
           "Agrarian/local elites","Business elites/civil servants",
           "Working classes","Urban Middle","Rural Middle","The aristocracy",
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


#############################################################
##### Data Exports  #####
#############################################################

sample <- autocracies %>% 
  dplyr::filter(class %in% c("Party elites", "The military",
                             "Working classes", "Urban Middle"))
  
foreign::write.dta(autocracies, "sample.dta")

#############################################################
##### Coefplot for Table 1
#############################################################
table1 <- pick("table1.dta")
head(table1)
table1 <- table1 %>% dplyr::filter(!str_detect(var, ".ccode"))
table1 <- table1 %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 1, 0)
  )
  

table1_spaw <- table1 %>% 
  dplyr::filter(var %in% c("party", "military", "working", "lag3pi") &
                  model %in% c("T1M1", "T1M3", "T1M5")) 

table1_vdem <- table1 %>% 
  dplyr::filter(var %in% c("party", "military", "working", "lag3pi") &
                  model %in% c("T1M2", "T1M4", "T1M6")) 

table1_spaw <- table1_spaw %>% 
  mutate(
    model = model %>% 
      parse_factor(., levels = c("T1M1", "T1M5", "T1M3"),
                   ordered = T, include_na = F),
    var = var %>% parse_factor(., c("working", "party", "military", "lag3pi"),
                               include_na = F, ordered = T),
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
    )
table(table1_spaw$var)
labels.spaw <- c(T1M1 = "Model 1", T1M5 = "Model 2", T1M3 = "Model 3")
spaw <- table1_spaw %>%
  ggplot(aes(x = var, y = coef, color=as.factor(insig))) + 
  geom_point(position = position_dodge(width = 0.9), show.legend = F) + 
  geom_line(show.legend = F, position = position_dodge(width = 0.9)) + 
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.9),
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_x_discrete(label = c("Working\nClass", "Party\nElites", 
                              "Military", "Party\nInst.")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  labs(x = "", 
       y = "",
       title = "Universal welfare (coverage)") + 
  facet_wrap(~model, ncol = 1, labeller=labeller(model = labels.spaw)) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.title = element_text(size = 10))
labels.vdem <- c(T1M4 = "Model 4", T1M2 = "Model 5", T1M6 = "Model 6")
table1_vdem <- table1_vdem %>% 
  mutate(
    model = model %>% 
      parse_factor(., levels = c("T1M4", "T1M2", "T1M6"),
                   ordered = T, include_na = F),
    var = var %>% parse_factor(., c("working", "party", "military", "lag3pi"),
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
  scale_x_discrete(label = c("Working\nClass", "Party\nElites", 
                             "Military", "Party\nInst.")) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  labs(x = "", 
       y = "",
       title = "Universal welfare (delivery)") + 
  facet_wrap(~model, ncol = 1, labeller=labeller(model = labels.vdem)) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.title = element_text(size = 10))

Figure.table1 <- spaw + vdem + plot_layout(ncol = 2)
ggsave("Figures/Table1.pdf", width = 8.5, height = 5)
#############################################################
##### Coefplot for Table 2
#############################################################
t2m1 <- pick("table2m1.dta");t2m2 <- pick("table2m2.dta");
t2m3 <- pick("table2m3.dta");t2m4 <- pick("table2m4.dta");
t2m5 <- pick("table2m5.dta");t2m6 <- pick("table2m6.dta")

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
       caption = "Note: Class coalitions and other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave("Figures/Plot4.pdf", width = 8.5, height = 5)

#############################################################
##### Coefplot for interaction
#############################################################
int1 <- pick("int1.dta");int2 <- pick("int2.dta")
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

interaction <- pick("intclasspi.dta")
interaction$var
table6 <- interaction %>% dplyr::filter(!str_detect(var, ".ccode"))
interaction <- interaction %>% 
  dplyr::filter(!var %in% c("ww1", "ww2", "coldwar", "_cons")) %>% 
  mutate(
    insig = ifelse(ci_lower < 0 & ci_upper > 0, 0, 1)
  )
install.packages("panelAR")
install.packages("rlang")
data <- re.sample %>% 
  dplyr::filter(dum_3==0 | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0) %>%
  dplyr::filter(year > 1915) %>% data.frame()
drop_na(data)
data <- data
results <- panelAR(formula = totalunivers ~ as.factor(ccode) + 
          ww1 + ww2 + coldwar + party + military + working + lag3pi + 
          I(working*lag3pi) + lag3gdp + lag3pop + lag3resdep,
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
pi_grid <- seq(0, 1, by=0.2)
rownames(beta_draws)
v <- colnames(beta_draws)
dim(beta_draws)
dim(beta_draws)
noworking <- cbind(1, 0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                .0205034, .0249208, .2872979, .1951177, .2465468, 0, pi_grid, 0, 7.906028, 8.534317, 4.589921)

working <- cbind(1, 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0, .0205034, .0249208, .2872979, .1951177, .2465468, 1, pi_grid, pi_grid, 7.906028, 8.534317, 4.589921)

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


noworking <- cbind(1,.0205034, .0249208, .2872979, .1951177, .2465468, 0, pi_grid, 0, 7.906028, 8.534317, 4.589921)
working <- cbind(1, .0205034, .0249208, .2872979, .1951177, .2465468, 1, pi_grid, pi_grid, 7.906028, 8.534317, 4.589921)
dim(beta_draws)
dim(noworking)
dim(working)

NW <- t(noworking %*% t(beta_draws))
NW.m <- apply(NW, 2, mean)
NW.se <- apply(NW, 2, quantile, c(0.025, 0.975))
W <- t(working %*% t(beta_draws))
W.m <- apply(W, 2, mean)
W.se <- apply(W, 2, quantile, c(0.025, 0.975))
NW.data <- data.frame(ID = "No-Working Class",
                     Class = "No-Working Class",
                     PI=pi_grid,
                     Mean=NW.m,
                     Lower=NW.se[1,],
                     Upper=NW.se[2,])
W.data <- data.frame(ID = "Working Class",
                      Class = "Working Class",
                      PI=pi_grid,
                      Mean=W.m,
                      Lower=W.se[1,],
                      Upper=W.se[2,])
Data <- rbind(NW.data, W.data) %>% as_tibble()


Data %>% 
  ggplot(aes(x=PI, y=Mean, color=ID)) + 
  geom_point() +
  geom_pointrange(aes(y = Mean, ymin = Lower, ymax = Upper)) + 
  facet_wrap(~ID) +
  labs(y="Predicted Universal Welfare", x="Party Institutionaliztion") + 
  scale_x_continuous(breaks = c(seq(from = 0.0, to = 1.0, by = 0.2))) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = wes_palette("Royal1")) + 
  guides(col = guide_legend(nrow = 1))
ggsave("Figures/Plot5.pdf", width = 8.5, height = 4.5)

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
       caption = "Note: Class coalitions and other covariates are not displayed.") + 
  facet_wrap(~model, ncol = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave("Figures/Plot4.pdf", width = 8.5, height = 5)

########################################################################
## Descriptive Stats.

names(data)
descriptive <- data %>% 
  select(country_name, ccode, year, regime,
         totalunivers, v2dlunivl,
         party, military, working, urban_middle,
         lag3pi, lag3pbranch, lag3plink, lag3porgs, lag3pplats,
         lag3pcohesv,
         lag3pop, lag3gdp, lag3resdep,
         ww1, ww2, coldwar)



desc.stats <- psych::describe(descriptive[, -c(1:4)])[, c(2, 3, 4, 8, 9)] %>%
  rownames_to_column() %>% as_tibble()
names(desc.stats) <- c("Variables", "Obs.", "Mean", "Std.", "Min.", "Max.")  
desc.stats$Variables
desc.stats <- desc.stats %>% mutate(
  Variables = case_when(
    Variables == "totalunivers" ~ "UI_SPaW",
    Variables == "v2dlunivl" ~ "UI_VDem",
    Variables == "party" ~ "Party Elites",
    Variables == "military" ~ "Military",
    Variables == "working" ~ "Working Class",
    Variables == "urban_middle" ~ "Urban Middle",
    Variables == "lag3pi" ~ "Party Inst.",
    Variables == "lag3pbranch" ~ "Branch",
    Variables == "lag3plink" ~ "Linkage",
    Variables == "lag3porgs" ~ "Organization",
    Variables == "lag3pplats" ~ "Platform",
    Variables == "lag3pcohesv" ~ "Cohension",
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

base_sample <- autocracies %>% mutate(
  country_abb = countrycode(autocracies$ccode, 
                             "cown", "p4c"))

panel.base <- base_sample %>% 
  select(country_name, year, 
         totalunivers, class, class.nu, country_abb, e_migdppc, e_mipopula, resdep2)

panel.base <- panel.base %>% mutate(
  class_core = case_when(
    class == "Party elites" ~ "Party elites",
    class == "The military" ~ "The military",
    class == "Working classes" ~ "Working classes",
    class == "Urban Middle" ~ "Urban Middle",
    class == NA ~ NA_character_,
    T ~ NA_character_) %>% 
    parse_factor(., levels = c("Party elites",
                               "The military",
                               "Working classes",
                               "Urban Middle"), include_na = F, ordered = T),
  core_nu = as.numeric(class_core)
)

mycol<-brewer.pal(4,"Set2")[c(2,1,3,4)]
pal <- wes_palette(4, name = "Royal2", type = "continuous")

panel.base %>% drop_na() %>% dplyr::filter(year > 1916) %>%
  panelView(totalunivers ~ core_nu,
          index = c("country_abb", "year"),
          xlab = "Year", ylab = "State",
          main = "", gridOff = FALSE,
          color = mycol,
          id = unique(panel.base$country_abb), axis.lab.gap = c(5,0),
          background = "white", by.timing = TRUE,
          legend.labs = c("Party elites",
                          "The military",
                          "Working classes",
                          "Urban Middle"))

panel.base %>% drop_na() %>% dplyr::filter(year > 1916) %>%
  panelView(totalunivers ~ core_nu + e_migdppc + e_mipopula + resdep2,
            index = c("country_abb", "year"),
            xlab = "Year", ylab = "State",
            main = "", gridOff = FALSE,
            color = mycol,
            id = unique(panel.base$country_abb), axis.lab.gap = c(1,0),
            background = "white", by.timing = TRUE,
            legend.labs = c("Party elites",
                            "The military",
                            "Working classes",
                            "Urban Middle"))


ggsave("Figure/Appendix1.pdf", width = 8.5, height = 11.0)

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








mdata_p <- mdata
mdata_p <- mdata_p[!duplicated(mdata_p[c("Ccodecow", "year")]),]
mdata_p <- mdata_p %>% drop_na(Ccodecow, year, v2psplats, v2psprlnks, v2dlencmps)
mdata_p <- pdata.frame(mdata_p, index=c("Ccodecow","year"))
table(mdata_p$year)

pdim(mdata_pna)
(table(mdata_p$Ccodecow, mdata_p$year)>1)
table(SPaW$year)
length(table(index(mdata_p), useNA = "ifany"))
summary(mdata_pna$year)
names(mdata)
mdata.small <- subset(mdata, 
                      select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                               univers_sick_prog, univers_working_prog,
                               univers_unemp_prog, univers_familiy_prog,
                               univers, univers.std, gwf_regime, 
                               v2xlg_legcon, wdi_oilrent, wdi_pop,
                               wdi_gdpcapcon2010, wdi_gdpcapgr, wdi_fdiin,
                               wdi_trade, wdi_agedr, NetODAin, csh_i), year > 1945 & gwf_regime>0)
mdata.small$log.gdppc <- log(mdata.small$wdi_gdpcapcon2010 + 1)
mdata.small$log.pop <- log(mdata.small$wdi_pop)

mdata.1<- subset(mdata.small, 
                 select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                          univers_sick_prog, univers_working_prog,
                          univers_unemp_prog, univers_familiy_prog,
                          univers.std, gwf_regime, 
                          wdi_oilrent,
                          log.gdppc, wdi_gdpcapgr, csh_i,
                          wdi_trade, wdi_agedr, NetODAin), is.na(v2xlg_legcon)==TRUE)

mdata.2<- subset(mdata.small, 
          select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                   univers_sick_prog, univers_working_prog,
                   univers_unemp_prog, univers_familiy_prog,
                   univers.std, gwf_regime, 
                   v2xlg_legcon, wdi_oilrent,
                   log.gdppc, wdi_gdpcapgr, csh_i,
                   wdi_trade, wdi_agedr, NetODAin))
mdata.small$v2xlg_legcon[is.na(mdata.small$v2xlg_legcon)==TRUE] <- -99

mdata.0<- subset(mdata.small, 
                 select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                          univers_sick_prog, univers_working_prog,
                          univers_unemp_prog, univers_familiy_prog,
                          univers.std, gwf_regime, 
                          v2xlg_legcon, wdi_oilrent,
                          log.gdppc, wdi_gdpcapgr, csh_i,
                          wdi_trade, wdi_agedr, NetODAin))

mdata.0$v2xlg_legcon[!is.na(mdata.0$v2xlg_legcon)] <- -99
summary(mdata.small$v2xlg_legcon)
mdata.1.clean <- mdata.1[complete.cases(mdata.1),]
mdata.2.clean <- mdata.2[complete.cases(mdata.2),]
mdata.0.clean <- mdata.0[complete.cases(mdata.0),]
mdata.0.clean$v2xlg_legcon[mdata.0.clean$v2xlg_legcon==-99] <- NA
mdata.monarchy <- subset(mdata.m, select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                         univers_sick_prog, univers_working_prog,
                         univers_unemp_prog, univers_familiy_prog,
                         univers.std, 
                         v2xlg_legcon,
                         log.gdppc, wdi_gdpcapgr, csh_i,
                         wdi_trade, wdi_agedr, NetODAin, wdi_oilrent), gwf_regime==1)
mdata.military <- subset(mdata.m, select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                                           univers_sick_prog, univers_working_prog,
                                           univers_unemp_prog, univers_familiy_prog,
                                           univers.std, 
                                           v2xlg_legcon,
                                           log.gdppc, wdi_gdpcapgr, csh_i,
                                           wdi_trade, wdi_agedr, NetODAin, wdi_oilrent), gwf_regime==2)
mdata.party <- subset(mdata.m, select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                                           univers_sick_prog, univers_working_prog,
                                           univers_unemp_prog, univers_familiy_prog,
                                           univers.std, 
                                           v2xlg_legcon,
                                           log.gdppc, wdi_gdpcapgr, csh_i,
                                           wdi_trade, wdi_agedr, NetODAin, wdi_oilrent), gwf_regime==3)
mdata.personal <- subset(mdata.m, select=c(ccode, year, univers_oldageprog, univers_mater_prog, 
                                           univers_sick_prog, univers_working_prog,
                                           univers_unemp_prog, univers_familiy_prog,
                                           univers.std, 
                                           v2xlg_legcon,
                                           log.gdppc, wdi_gdpcapgr, csh_i,
                                           wdi_trade, wdi_agedr, NetODAin, wdi_oilrent), gwf_regime==4)

stargazer(subset(mdata.0.clean[c("gwf_regime", "v2xlg_legcon",
                                "univers_oldageprog", "univers_mater_prog",
                                "univers_sick_prog", "univers_working_prog", 
                                "univers_unemp_prog", "univers_familiy_prog", 
                                "univers.std", "log.gdppc", "wdi_gdpcapgr",
                                "csh_i", "wdi_trade","wdi_oilrent", 
                                "NetODAin","wdi_agedr")], 
                 title="Descriptive Statistics", digits=2,
                 summary.stat = c("n", "mean", "sd")))
summary(unique(mdata.clean$year))
summary(unique(mdata.clean$year[is.na(mdata.clean$wdi_agedr)==FALSE]))

length(unique(mdata.m$ccode[mdata.m$gwf_regime==1]))
length(unique(mdata.m$ccode[mdata.m$gwf_regime==2]))
length(unique(mdata.m$ccode[mdata.m$gwf_regime==3]))
length(unique(mdata.m$ccode[mdata.m$gwf_regime==4]))
mean(mdata.m$v2xlg_legcon[mdata.m$gwf_regime==2], na.rm=TRUE)
mean(mdata.m$v2xlg_legcon[mdata.m$gwf_regime==3], na.rm=TRUE)
mean(mdata.m$v2xlg_legcon[mdata.m$gwf_regime==4], na.rm=TRUE)
stargazer(subset(mdata.monarchy[c("v2xlg_legcon",
                           "univers_oldageprog", "univers_mater_prog",
                           "univers_sick_prog", "univers_working_prog", 
                           "univers_unemp_prog", "univers_familiy_prog", 
                           "univers.std", "log.gdppc", "wdi_gdpcapgr",
                           "csh_i", "wdi_trade","wdi_oilrent", 
                           "NetODAin","wdi_agedr")], 
                 title="Descriptive Statistics", digits=2, summary=FALSE,
                 summary.stat = c("n", "mean")))

library(dplyr); library(purrr)

mdata.m %>% split(. $gwf_regime) %>% walk(~ stargazer(., type = "text"))


require(reporttools)
vars <- mdata.m[,c("v2xlg_legcon")]
group <- mdata.m[,c('gwf_regime')]

## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, group = group, 
                prec = 2, cap = "Descriptive Statistics Table by Regime", 
                lab = "tab:descr", stats = c("mean", "s"))
c?tableContinuous

mdata.m$countryn <- countrycode(mdata.m$ccode, 
                                "cown", "country.name")

table(unique(mdata.clean$countryn))

model0.univ <- ddply(mdata.0.clean, .(gwf_regime), summarise, 
                     Old = mean(univers_oldageprog), 
                     Mater = mean(univers_mater_prog),
                     Sick = mean(univers_sick_prog), 
                     Working = mean(univers_working_prog),
                     Unemp = mean(univers_unemp_prog), 
                     Family = mean(univers_familiy_prog))
model0.univ.long <- melt(model0.univ, id.vars=c("gwf_regime"))
names(model0.univ.long)[2:3] <- c("Type", "value")
model0.univ.long$gwf_regime <- factor(model0.univ.long$gwf_regime,
                                      labels = c("Monarchy","Military",
                                                 "Dominant party","Personlist"))
model0.univ.sd <- ddply(mdata.0.clean, .(gwf_regime), summarise, 
                     Old = sd(univers_oldageprog), 
                     Mater = sd(univers_mater_prog),
                     Sick = sd(univers_sick_prog), 
                     Working = sd(univers_working_prog),
                     Unemp = sd(univers_unemp_prog), 
                     Family = sd(univers_familiy_prog))
model0.univ.long.sd <- melt(model0.univ.sd, id.vars=c("gwf_regime"))
names(model0.univ.long.sd)[2:3] <- c("Type", "sd")
model0.univ.m<- cbind(model0.univ.long, model0.univ.long.sd)
model0.univ.m[4:5] <- NULL


pdf("Figures/plot1.pdf")
plot1 <- ggplot(data=model0.univ.long, aes(x=as.factor(Type), y=value)) + 
  geom_bar(stat="identity") + 
  facet_wrap(gwf_regime~., labeller = label_value) +
#  geom_errorbar(aes(ymin=value-sd,
#                    ymax=value+sd), width=.2,
#              position=position_dodge(.9)) +
  theme_bw() + 
  labs(x="Programs for Risks", y="Averages of Program Coverage")
print(plot1)
dev.off()

model0.univ.line <- ddply(mdata.small, .(gwf_regime, year), summarise, 
                     Welfare = mean(univers.std, na.rm=TRUE))
model0.univ.line.long <- melt(model0.univ.line, id.vars=c("gwf_regime", "year"))
model0.univ.line.long[3] <- NULL
model0.univ.line.long$gwf_regime <- factor(model0.univ.line.long$gwf_regime,
                                      labels = c("Monarchy","Military",
                                                 "Dominant party","Personlist"))
ggplot(data=model0.univ.line.long[model0.univ.line.long$year <2006, ], aes(x=year, y=value)) + 
  geom_point() + geom_line() + facet_wrap(.~gwf_regime) + 
  scale_x_continuous(breaks = seq(1945, 2005, by=5))

mdata.0.panel<-pdata.frame(mdata.0.clean, index=c("ccode","year"))
pdim(mdata.0.panel)
library(lmtest)
names(mdata.0.clean)
lm.model1 <- lm(univers.std ~ gwf_regime.fa +
                  log.gdppc + wdi_gdpcapgr + wdi_trade + 
                  wdi_oilrent + csh_i + NetODAin + wdi_agedr, data=mdata.0.clean)
bgtest(lm.model1)
bptest(lm.model1)
summary(lm.model1)
###autocorrelation
library(plm)
lm.model1.lag <- plm(univers.std ~ lag(gwf_regime.fa) +
                  lag(log.gdppc) + lag(wdi_gdpcapgr) + lag(wdi_trade) + 
                  lag(wdi_oilrent) + lag(csh_i) + lag(NetODAin) + lag(wdi_agedr), 
                  data=mdata.0.clean)
summary(lm.model1.lag)
mdata.0.clean$gwf_regime.fa <- as.factor(mdata.0.clean$gwf_regime)
mdata.0.clean$gwf_regime.fa <- relevel(mdata.0.clean$gwf_regime.fa, ref="3")
table(mdata.0.panel$gwf_regime.fa)
lm.model1.lag.fix <- plm(univers.std ~ lag(gwf_regime.fa, 1) +
                           lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                           lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + 
                           lag(wdi_agedr, 1), 
                         data=mdata.0.clean, 
                 model="within", index = c("ccode", "year"))
summary(lm.model1.lag.fix) # Results are observed for within-unit variation only

par.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon, 1) +
                           lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                           lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + 
                           lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==3, ], 
                         model="within", index = c("ccode", "year"))
table(mdata.0.clean$gwf_regime)

par.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                           lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                           lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                         data=mdata.0.clean[mdata.0.clean$gwf_regime==3, ], 
                         model="within", index = c("ccode", "year"))
summary(par.fix.inter) # Results are observed for within-unit variation only

dother.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime!=3, ], 
                     model="within", index = c("ccode", "year"))
summary(mil.fix.inter) # Results are observed for within-unit variation only

par.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==3, ], 
                     model="within", index = c("ccode", "year"))
summary(par.fix.inter) # Results are observed for within-unit variation only

per.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==4, ], 
                     model="within", index = c("ccode", "year"))
summary(per.fix.inter) # Results are observed for within-unit variation only


lm.model1.lag.random <- plm(univers.std ~ lag(gwf_regime.fa, 1) +
                              lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                              lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                            data=mdata.0.clean, 
                     model="random", index = c("ccode", "year"))
summary(lm.model1.lag.random)

lm.model1.lag.pooling <- plm(univers.std ~ lag(gwf_regime.fa, 1) +
                              lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                              lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                            data=mdata.0.clean, 
                            model="pooling")
pFtest(lm.model1.lag.fix, lm.model1.lag.pooling) ##Fixed is better than pooled
plmtest(lm.model1.lag.pooling, effects="individual") ##Random is better than pooled
phtest(lm.model1.lag.fix, lm.model1.lag.random) ## rejected: correlated heterogeneity, fe better

library(panelAR)
lm.model1.lag.pcse<- panelAR(univers.std ~ lag(gwf_regime.fa, 1) +
                               lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + 
                               lag(wdi_trade, 1) + lag(wdi_oilrent, 1) + 
                               lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1),
                             data = mdata.0.clean, 
                             panelVar = 'ccode', timeVar = 'year',
        panelCorrMethod = "pcse", autoCorr = "psar1", complete.case = F, rho.na.rm = TRUE)
summary(lm.model1.lag.pcse)

inter.lag.pcse<- panelAR(univers.std ~ lag(gwf_regime.fa, 1)*lag(v2xlg_legcon) +
                               lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + 
                               lag(wdi_trade, 1) + lag(wdi_oilrent, 1) + 
                               lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1),
                             data = mdata.0.clean, 
                             panelVar = 'ccode', timeVar = 'year',
                             panelCorrMethod = "pcse", autoCorr = "psar1", complete.case = F, rho.na.rm = TRUE)
summary(inter.lag.pcse)


mon.pcse.inter <- panelAR(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==1, ], 
                     panelVar = 'ccode', timeVar = 'year',
                     panelCorrMethod = "pcse", autoCorr = "psar1", 
                     complete.case = F, rho.na.rm = TRUE)
summary(mon.pcse.inter) # Results are observed for within-unit variation only

mil.pcse.inter <- panelAR(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==2, ], 
                     panelVar = 'ccode', timeVar = 'year',
                     panelCorrMethod = "pcse", autoCorr = "psar1", 
                     complete.case = F, rho.na.rm = TRUE)
summary(mil.pcse.inter) # Results are observed for within-unit variation only

par.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==3, ], 
                     model="within", index = c("ccode", "year"))
summary(par.fix.inter) # Results are observed for within-unit variation only

per.fix.inter <- plm(univers.std ~ lag(v2xlg_legcon) +
                       lag(log.gdppc, 1) + lag(wdi_gdpcapgr, 1) + lag(wdi_trade, 1) + 
                       lag(wdi_oilrent, 1) + lag(csh_i, 1) + lag(NetODAin, 1) + lag(wdi_agedr, 1), 
                     data=mdata.0.clean[mdata.0.clean$gwf_regime==4, ], 
                     model="within", index = c("ccode", "year"))
summary(per.fix.inter) # Results are observed for within-unit variation only





coef.table <- stargazer(lm.model1.lag.fix, par.fix.inter,
                          other.fix.inter,
                         column.labels = c("Model 1", 
                                           "Model 2: Dominant Party",
                                           "Model 3: Others"),
                         model.names=TRUE, style="aer",
                         ci=TRUE, p.auto=TRUE,
                         dep.var.labels.include=TRUE,
                         table.placement="htbp",
                         font.size="scriptsize",
                         model.numbers=FALSE, #float.env="sidewaystable",
                         notes.label=" ",
                         digits=2, df=FALSE, omit.stat=c("rsq"),
                         order = c(1, 2),
                         no.space=TRUE,
                         title = "Panel Regression with fixed effects",
                         label = "tab:coefficients",
                         notes.align = "r",
                         notes = c("*** p less than 0.001, ** p less than  0.01, * p less than  0.05"),
                         star.cutoffs = c(0.05, 0.01, 0.001),
                         notes.append = FALSE)


prop.table(table(mdata.0.clean$gwf_regime))



summary <- model1.clean %>% split(.$gwf_regime) %>% map(summary)
summary[[1]]
summary[[2]]
table()
table(model1.clean$gwf_regime, model1.clean$year)
mean(na.omit(mdata.small$v2xlg_legcon[mdata.small$gwf_regime==1]))
mean(na.omit(mdata.small$v2xlg_legcon[mdata.small$gwf_regime==2]))
mean(na.omit(mdata.small$v2xlg_legcon[mdata.small$gwf_regime==3]))
mean(na.omit(mdata.small$v2xlg_legcon[mdata.small$gwf_regime==4]))
getwd()
str(summary)
summary(summary)
table(summary)
class(mdata$gwf_regime)




summary <- mdata %>%
  select(wdi_fdiin, wdi_oilrent, `Foreign Aid (% GNI)`, wdi_pop,
         oldage_yearlaw, mater_yearlaw, sick_yearlaw, unemp_yearlaw, working_yearlaw,
         familiy_yearlaw, univers_oldageprog, univers_mater_prog, univers_sick_prog,
         univers_unemp_prog, univers_working_prog, univers_familiy_prog) %>%
  group_by(mdata$gwf_regime) %>%
  summarise(FDI.inflow = mean(mdata$wdi_fdiin, na.rm = TRUE),
            Oil.rent = mean(mdata$wdi_oilrent, na.rm = TRUE),
            Foregin.aid = mean(mdata$`Foreign Aid (% GNI)`, na.rm = TRUE),
            Population = mean(mdata$wdi_pop, na.rm = TRUE),
            Oldage.law = mean(mdata$oldage_yearlaw, na.rm = TRUE),
            Mater.law = mean(mdata$mater_yearlaw, na.rm = TRUE),
            Sick.law = mean(mdata$sick_yearlaw, na.rm = TRUE),
            Unemp.law = mean(mdata$unemp_yearlaw, na.rm = TRUE),
            Working.law = mean(mdata$working_yearlaw, na.rm = TRUE),
            Family.law = mean(mdata$familiy_yearlaw, na.rm = TRUE),
            Oldage.univ = mean(mdata$univers_oldageprog, na.rm = TRUE),
            Mater.univ = mean(mdata$univers_mater_prog, na.rm = TRUE),
            Sick.univ = mean(mdata$univers_sick_prog, na.rm = TRUE),
            Unemp.univ = mean(mdata$univers_unemp_prog, na.rm = TRUE),
            Working.univ = mean(mdata$univers_working_prog, na.rm = TRUE),
            Family.univ = mean(mdata$univers_familiy_prog, na.rm = TRUE))

library(stargazer)
stargazer(data.frame(gwf2), type="text", out="descriptive.txt")
?stargazer


regime <- wth2

regime <- merge(x = regime, y =gwf2, by.x = c("cowcode", "year"),
                by.y = c("ccode", "year"), all=TRUE)
regime <- merge(x = regime, y=cgv2, by = c("cowcode", "year"), all=TRUE)

regime$gwf <- ifelse(regime$gwf_monarchy==1, 1, 
                     ifelse(regime$gwf_military==1,2,
                            ifelse(regime$gwf_party==1,3,
                                   ifelse(regime$gwf_personal==1,4, NA))))
regime$cgv <- ifelse(regime$regime==5, 1, 
                     ifelse(regime$regime==4, 2,
                            ifelse(regime$regime==3, 3, NA)))
table(regime$cgv)
regime$wth <- ifelse(regime$mon==1, 1, 
                     ifelse(regime$mil==1, 2,
                            ifelse(regime$mul==1, 3,
                                   ifelse(regime$onep==1, 4,
                                          ifelse(regime$nop==1,5, NA)))))
regime2 <- subset(regime, select=c("gwf", "cgv", "wth"))

regime2 <- apply_labels(regime2,
                        cgv = c("Monarchy" = 1,
                                "Military" = 2,
                                "Civilian" = 3),
                        gwf = c("Monarchy" = 1,
                                "Military" = 2,
                                "Party" = 3,
                                "Personal" = 4),
                        wth = c("Monarchy" = 1,
                                "Military" = 2,
                                "Multi-party" = 3,
                                "One-party" = 4,
                                "No-party" = 5))

stargazer(data.frame(regime2), type="text", out="descriptive.txt")
table(regime2$cgv)
a <- cro(regime2$cgv, regime2$gwf)
cro(regime2$cgv, regime2$gwf)
cro(regime2$cgv, regime2$wth)
cro(regime2$wth, regime2$gwf)
library(xtable)
install.packages("gmodels")
library(gmodels)
install.packages("descr")
library(descr)
a<-CrossTable(regime2$cgv, regime2$gwf, dnn = c("CGV", "GWF"),
              prop.chisq = FALSE, prop.c = FALSE,prop.t = FALSE,prop.r = FALSE)
b <- CrossTable(regime2$cgv, regime2$wth, dnn = c("CGV", "WTH"),
                prop.chisq = FALSE, prop.c = FALSE,prop.t = FALSE,prop.r = FALSE)
??CrossTable
print(xtable(a,digits=0))
print(xtable(b,digits=0))
a
print(xtable(a), include.rownames = TRUE, include.colnames = TRUE, 
      sanitize.text.function = I)
stargazer(format(a, quote=FALSE, justify="right"), type="html")
summary(lm.model1.lag.fix)

model.graph1 <- plot_model(lm.model1.lag.fix, 
           show.values = TRUE, value.offset = .3, 
           order.terms = c(2, 3, 1, 4, 5, 8, 6, 7, 9, 10),
           axis.labels = c("Age Dependency", "Oil Rent", "Foreign Aid",
                           "Trade Openness", "Domestic Investment", 
                           "GDPpc Growth", "GDPpc(logged)", 
                           "Personalist", "Military", "Monarchy"))
summary(par.fix.inter)
model.graph2 <- plot_model(par.fix.inter, 
                           show.values = TRUE, value.offset = .3, 
                           order.terms = c(1, 2, 3, 6, 4, 7, 5, 8),
                           axis.labels = c("Age Dependency", "Oil Rent", "Foreign Aid",
                                           "Trade Openness", "Domestic Investment", 
                                           "GDPpc Growth", "GDPpc(logged)", 
                                           "Legislature Binding"))
model.graph3 <- plot_model(dother.fix.inter, 
                           show.values = TRUE, value.offset = .3, 
                           order.terms = c(1, 2, 3, 6, 4, 7, 5, 8),
                           axis.labels = c("Age Dependency", "Oil Rent", "Foreign Aid",
                                           "Trade Openness", "Domestic Investment", 
                                           "GDPpc Growth", "GDPpc(logged)", 
                                           "Legislature Binding"))
model.graph3
library(ggpubr)
pdf("Figures/plot7.pdf")

plot7<- ggarrange(model.graph1, model.graph2, model.graph3 + rremove("x.text"), 
          labels = c("Model1", "Model2", "Model3"),
          ncol = 3, nrow = 1, heights = 3)
print(plot7)
dev.off()
