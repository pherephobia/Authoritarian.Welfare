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

#### Figure 1: Distribution of coalition
#### 2.2 %
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

ggsave("Documents/2_Manuscript/2_Figures/fig1_brm.pdf",
       width = 8.5, height = 5, dpi = "retina")

#### Figure 1A Alternative Coalition
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
  #ggsci::scale_color_nejm() +
  #ggsci::scale_fill_nejm() +
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

ggsave("Documents/2_Manuscript/2_Figures/fig1A_robust_brm.pdf",
       width = 8.5, height = 5, dpi = "retina")

#### Figure2 ####

baseline |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) |> 
  drop_na(year, mass_coalition) |>  ungroup() |> 
  mutate(
    mass_coalition = factor(mass_coalition,
                            levels = c("Institution-based",
                                       "Mass-based"),
                            labels = c("Elite-based coalition",
                                       "Mass-based coalition"))) -> figure2

summary_fig2A <- figure2 |> group_by(mass_coalition) |> 
  summarize(mean = mean(universality, na.rm = T))

robust_alt |> 
  dplyr::filter(brm_regime %in% "Autocracies (BRM)") |> 
  dplyr::filter(year > 1959 & year < 2021) -> figure2B

summary_fig2B <- figure2B |> group_by(mass_inclusive_T050) |> 
  summarize(mean = mean(universality, na.rm = T)) |> drop_na()

bind_rows(summary_fig2A |> mutate(group = c(1, 2)) |> 
            rename(coalition = mass_coalition),
          summary_fig2B |> mutate(group = c(1, 2),
                                  mass_inclusive_T050 = factor(
                                    mass_inclusive_T050, levels = c(0, 1),
                                    labels = c("Non-mass coalition", "Mass-inclusive coalition"))) |> 
            rename(coalition = mass_inclusive_T050)) ->
  summary_fig2_comb

bind_rows(figure2 |> dplyr::select(universality, coalition = mass_coalition) |> 
            mutate(group = case_when(
              coalition %in% "Elite-based coalition" ~ 1L,
              coalition %in% "Mass-based coalition" ~ 2L)),
          figure2 |> dplyr::select(universality, coalition = coalition_alt) |> 
            mutate(group = case_when(
              coalition %in% "Non-mass coalition" ~ 1L,
              coalition %in% "Mass-inclusive coalition" ~ 2L))) ->
  figure2_comb

figure2_comb |> drop_na(group) |> 
  mutate(group = factor(group,
                        levels = c(1, 2),
                        labels = c("Elite-based coalition", "Mass-based coalition"))) |>
  ggplot(aes(x = universality, color = coalition)) +
  geom_density(aes(fill = coalition), show.legend = F, alpha = 0.6) +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(
    data = summary_fig2_comb |> 
      mutate(group = factor(group,
                            levels = c(1, 2),
                            labels = c("Elite-based coalition", "Mass-based coalition"))),
    aes(xintercept = mean,
        color = coalition), show.legend = F) +
  # ggrepel::geom_label_repel(
  #   data = summary_fig2_comb |>
  #     mutate(group = factor(group,
  #                           levels = c(1, 2),
  #                           labels = c("Elite-based coalition", "Mass-based coalition"))),
  #   aes(x = mean,
  #       color = coalition,
  #       label = paste0("Mean of U.I. for\n", coalition, "\n", as.character(round(mean, 2)))),
  #   y = 0.5,
  #   size = 5,
  #   nudge_y = 0.5,
  #   nudge_x = 0.5,
  #   show.legend = F) +
  geom_text(
  data = summary_fig2_comb |>
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
  #gghighlight::gghighlight(coalition %in% c("Elite-based coalition", "Mass-based coalition")) +
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

ggsave("Documents/2_Manuscript/2_Figures/fig2_brm.pdf", 
       width = 7, height = 5.5, dpi = "retina")
