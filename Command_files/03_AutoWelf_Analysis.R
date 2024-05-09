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
load("Analysis_data/main_full.RData")

#### Table 1 Ruling Coalitions, Mass Party Organizations, and Universal Welfare Provisions --------
####         in Autocracies

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

texreg::htmlreg(list(brm_full_tfixed1, brm_full_tfixed2, brm_full_tfixed3, brm_full_tfixed4),
               coef_omit = "as.factor",
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
               single.row = T,
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