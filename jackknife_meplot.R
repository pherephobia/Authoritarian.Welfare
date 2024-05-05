
vdemdata::vdem -> vdem
library(tidyverse)
vdem |> dplyr::filter(year > 1959) |> 
  dplyr::select(COWcode, year, e_civil_war, v2x_polyarchy, v2stfisccap) ->
  subset

main_model <- glm(e_civil_war ~ v2x_polyarchy*v2stfisccap,
                  family = binomial(link = "logit"), data = subset)

### condition의 구간을 설정해준다.
summary(subset$v2stfisccap)

state_fiscal_cap_int <- seq(-3, 3, 0.05)

### Do Parametric Bootstrapping Here
### First, if our estimates are BLUE, the coefficients will be similar with the 
### true values (parameters) in population.
### Second, then we can generate multivariate random normal distributions for 
### each coefficient estimates, treating the coefficients as mean and 
### variance of covariance as standard deviations.
### Use MASS package.

coef_main <- MASS::mvrnorm(n = 4000, mu = coef(main_model), Sigma = vcov(main_model))

### coef_main 을 확인해보면 각 계수값들이 4000개 씩 시뮬레이션된 것을 확인할 수 있다.

### Marginal Effects
### Conditioning variable을 기준으로 우리가 생각할 수 있는 한계효과는
### \parital{e_civil_war}/\partial{demregion} =  b2 + b3*v2x_polyarchy
### 이후 행렬 곱셈으로 계산. b2 + b3*v2x_polyarchy %*% (1, conditioning intervals)
me_main <- coef_main[, c("v2stfisccap", "v2x_polyarchy:v2stfisccap")] %*% rbind(1, state_fiscal_cap_int) 

main_df <- data.frame(
  statecap = state_fiscal_cap_int,
  Mean = apply(me_main, 2, mean, na.rm = T),
  lower = apply(me_main, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(me_main, 2, quantile, probs = 0.975, na.rm = T)
) |> mutate(Exclude = paste0("No County excluded"),
            id = paste0("0"))

### Jackknife regression
### 표본에서 국가들을 하나씩 제거해 가면서 추정해보자: 특정 국가가 결과를 견인하나?

### 고유한 국가 값을 확인

subset |> rowid_to_column() |> 
  mutate(rowid = as.character(rowid)) |> 
  dplyr::filter(rowid %in% broom::augment(main_model)$.rownames) ->
  subset_complete

sort(unique(subset_complete$COWcode)) -> unique_COW

### 시뮬레이션 결과를 담을 빈 깡통을 만든다

jackknife_df <- data.frame()

for (i in 1:length(unique_COW)) {
  tryCatch({
    model <- glm(e_civil_war ~ v2x_polyarchy + v2stfisccap + v2x_polyarchy*v2stfisccap,
                 family = binomial(link = "logit"), 
                 data = subset |> dplyr::filter(!COWcode %in% unique_COW[i]))
    state_fiscal_cap_int <- seq(-3, 3, 0.05)
    coef <- MASS::mvrnorm(n = 4000, mu = coef(model), Sigma = vcov(model))
    me <- coef[,  c("v2stfisccap", "v2x_polyarchy:v2stfisccap")] %*% rbind(1, state_fiscal_cap_int) 
    temp_df <- data.frame(
      statecap = state_fiscal_cap_int,
      Mean = apply(me, 2, mean, na.rm = T),
      lower = apply(me, 2, quantile, probs = 0.025, na.rm = T),
      upper = apply(me, 2, quantile, probs = 0.975, na.rm = T)
    ) |> 
      mutate(Exclude = paste0("County ", unique_COW[i], " excluded"),
             id = paste0(unique_COW[i]))
    jackknife_df <- jackknife_df |> bind_rows(temp_df)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    # Optionally, you can log or handle the error here
  })
}

jackknife_df |> bind_rows(main_df) ->
  jk_full_final

subset |> 
  dplyr::select(COWcode, year, e_civil_war, v2x_polyarchy, v2stfisccap) |> drop_na() |> 
  summarize(
    v2x_polyarchy = mean(v2x_polyarchy, na.rm = T),
    v2stfisccap = quantile(v2stfisccap, probs = c(0.1, 0.9), na.rm = T)) -> check

coef(main_model)[3]*1 + coef(main_model)[4]*check[1, 2]  -> low

coef(main_model)[3]*1 + coef(main_model)[4]*check[2, 2]  -> high

point <- data.frame(statecap = c(as.numeric(check[1, 2]), as.numeric(check[2, 2])),
                    Mean =c(as.numeric(low), as.numeric(high)) ,
                    id = c("0", "0"))

theme_set(theme_bw())

jk_full_final |> 
  ggplot(aes(x = statecap, y = Mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  geom_line(aes(color = id), linetype = "dashed", show.legend = F) +
  geom_line(aes(y = lower, color = id), show.legend = F) +
  geom_line(aes(y = upper, color = id), show.legend = F) +
  geom_point(data = point, aes(statecap, Mean, fill = id),
             shape = 21, size = 3, show.legend = F) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(seq(-5, 10, 1)), expand = c(.05,.05)) +
  scale_y_continuous(breaks = c(seq(0, 7, 1))) +
  labs(x = NULL, y = "Estimated marginal effects") +
  gghighlight::gghighlight(id %in% "0", use_direct_label = FALSE) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) -> ME_sim


subset |> 
  ggplot(aes(x = v2stfisccap, y = ..density..)) + 
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(breaks = c(seq(-3, 3, 1)), lim = c(-3, 3), 
                     expand = c(.05,.05)) +
  labs(x = "State Fiscal Capacity)", y = "Density",
       caption = str_wrap("\nNote: The figure depicts the impact of X on Y, with the main result from the full model highlighted in color. Each line plot is shown with 95% confidence intervals. The colored points represent the estimated marginal effects at the bottom and top ten percentiles of Z. Grey lines represent the marginal effects of models using the jackknife method, with each model excluding one country from the analysis.", 130, exdent = 10)) +
  theme(plot.caption = element_text(hjust = 0)) -> MPO_hist

library(patchwork)
library(ggExtra)

ME_sim + MPO_hist +
  patchwork::plot_layout(
    ncol = 1, 
    nrow = 2, 
    widths = 4,
    heights = c(6.5, 1.5)
  )