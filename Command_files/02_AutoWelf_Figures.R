## Project: Why do authoritarian regimes provide welfare programs? ------------------
##          2. Descriptive Figures  -----------------------------------
## Author:
##   - SangHoon Park (UofSC)
## Date:

## Import Packages to use ------------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")
pacman::p_load(ggplot2, grid, pBrackets, countrycode, reshape2, 
               tidyr, patchwork, plm, futurevisions, panelView, RColorBrewer,
               ggrepel, extrafont, tidyverse)
sysfonts::font_add_google("Barlow Semi Condensed")

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

## Import Data to use ------------------------------------------------------------------------------
baseline <- readRDS("Analysis_data/baseline.RDS")
robust_alt <- readRDS("Analysis_data/robust_alt.RDS")
load("Analysis_data/main_bench.RData")
load("Analysis_data/main_full.RData")
load("Analysis_data/jackknife_full4.RData")

#### Figure 1 Distribution of the universalism index of V-Dem by mass-based coalition and ----------
####          elite-based coalition, 1960-2020

baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) |> 
  drop_na(year, mass_coalition) |>  ungroup() |> 
  mutate(
    mass_coalition = factor(mass_coalition,
                            levels = c("Institution-based",
                                       "Mass-based"),
                            labels = c("Elite-based coalition",
                                       "Mass-based coalition"))) -> figure1

summary_fig1 <- figure1 |> group_by(mass_coalition) |> 
  summarize(mean = mean(universality, na.rm = T))


figure1 |> drop_na(mass_coalition) |> 
  ggplot(aes(x = universality, color = mass_coalition)) +
  geom_density(aes(fill = mass_coalition), show.legend = F, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(
    data = summary_fig1,
    aes(xintercept = mean,
        color = mass_coalition), show.legend = F) +
  ggrepel::geom_label_repel(
    data = summary_fig1,
    aes(x = mean,
        color = mass_coalition,
        label = round(mean, 2)), fill = "white",
    y = 0.5,
    size = 5,
    nudge_y = 0.5,
    nudge_x = 0.5,
    show.legend = F) +
  facet_wrap(~mass_coalition) +
  scale_color_manual(values = c(futurevisions::futurevisions("mars")[1],
                                futurevisions::futurevisions("mars")[3])) +
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[1],
                               futurevisions::futurevisions("mars")[3])) +
  scale_y_continuous() +
  labs(y = NULL, x = "\nUniversalism index of V-Dem\n",
       caption = str_wrap("\nNote: The figure presents two panels comparing the distribution of welfare universalism in elite-based (left) and mass-based (right) coalitions with their averages of welfare universalism. The value of zero indicates the point where welfare policies are equally divided between means-tested and universalistic approaches.",
                          100, exdent = 10)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=9),
        strip.text = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

ggsave("Documents/2_Manuscript/2_Figures/fig1.pdf", 
       width = 7, height = 5.5, dpi = "retina")


#### Figure 2 Marginal Effect of Mass Coalition by Mass Party Organization Level -------------------
# jk_full4_brm <- data.frame()
# mpo_int <- c(seq(-5, 10, 0.01))
# coef_f4main_brm <- MASS::mvrnorm(n = 4000, mu = coef(brm_full_tfixed4), Sigma = vcov(brm_full_tfixed4))
# me_f4main_brm <- coef_f4main_brm[, c("l_mass_coalition", "l_mass_coalition:l_massparty")] %*% rbind(1, mpo_int) 
# f4main_brm_df <- data.frame(
#   MPO = mpo_int,
#   Mean = apply(me_f4main_brm, 2, mean, na.rm = T),
#   lower = apply(me_f4main_brm, 2, quantile, probs = 0.025, na.rm = T),
#   upper = apply(me_f4main_brm, 2, quantile, probs = 0.975, na.rm = T)
# ) |> mutate(Exclude = paste0("No County excluded"),
#             id = paste0("0"))
# 
# for (i in 1:length(unique_COWfull_brm)) {
#   tryCatch({
#     model <- glm(
#       universality ~ l_mass_coalition*l_massparty + log(l_gdppc+1) + l_gdpgrth + 
#         l_lnresource + l_civilwar + l_repression + l_CSOconsult + l_CSOpart +
#         l_hereditaryindex + l_militaryindex + l_partyindex + l_personindex +
#         as.factor(COWcode) + as.factor(year),
#       data = baseline |> 
#         dplyr::filter(brm_regime %in% "Autocracies (BRM)" & !COWcode %in% unique_COWfull_brm[i]))
#     mpo_int <- c(seq(-5, 10, 0.01))
#     coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
#     me <- coef[, c("l_mass_coalitionMass-based", "l_mass_coalitionMass-based:l_massparty")] %*% rbind(1, mpo_int) 
#     temp_df <- data.frame(
#       MPO = mpo_int,
#       Mean = apply(me, 2, mean, na.rm = T),
#       lower = apply(me, 2, quantile, probs = 0.025, na.rm = T),
#       upper = apply(me, 2, quantile, probs = 0.975, na.rm = T)
#     ) |> 
#       mutate(Exclude = paste0("County ", unique_COWfull_brm[i], " excluded"),
#              id = paste0(unique_COWfull_brm[i]))
#     jk_full4_brm <- jk_full4_brm |> bind_rows(temp_df)
#   }, error = function(e) {
#     message(paste("Error in iteration", i, ":", e$message))
#     # Optionally, you can log or handle the error here
#   })
# }
# 
# jk_full4_brm |> bind_rows(f4main_brm_df) ->
#   jk_full4_brm_final
# 
# baseline |> 
#   dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
#   dplyr::select(COWcode, universality, l_mass_coalition, l_massparty,
#                 l_gdppc, l_gdpgrth, l_lnresource, l_civilwar, l_repression, 
#                 l_CSOconsult, l_CSOpart, l_hereditaryindex, l_militaryindex, 
#                 l_partyindex, l_personindex) |> drop_na() |> 
#   summarize(
#     massparty = quantile(l_massparty, probs = c(0.1, 0.9), na.rm = T),
#     meanlngdppc = mean(log(l_gdppc + 1), na.rm = T),
#     meangdpgrth = mean(l_gdpgrth, na.rm = T),
#     meanresource = mean(l_lnresource, na.rm = T),
#     meancivilwar = median(l_civilwar, na.rm = T),
#     meanrepression = mean(l_repression, na.rm = T),
#     meancsocon = mean(l_CSOconsult, na.rm = T),
#     meancsopart = mean(l_CSOpart, na.rm = T),
#     meanhereditary = mean(l_hereditaryindex, na.rm = T),
#     meanmilitary = mean(l_militaryindex, na.rm = T),
#     meanparty = mean(l_partyindex, na.rm = T),
#     meanpersonal = mean(l_personindex, na.rm = T)
#   ) -> check_brm
# 
# coef(brm_full_tfixed4)[1]*1 + coef(brm_full_tfixed4)[14]*as.numeric(check_brm[1, 1])  -> low
# 
# coef(brm_full_tfixed4)[1]*1 + coef(brm_full_tfixed4)[14]*check_brm[2, 1] -> high
# 
# point <- data.frame(MPO = c(as.numeric(check_brm[1, 1]), as.numeric(check_brm[2, 1])),
#                     Mean =c(as.numeric(low), as.numeric(high)) ,
#                     id = c("0", "0"))

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

ggsave("Documents/2_Manuscript/2_Figures/Fig2.pdf",
       width = 9, height = 7, dpi = 1600)




