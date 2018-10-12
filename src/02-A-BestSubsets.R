library("ProjectTemplate")
load.project()

glimpse(df_train)

plot_bsr <- function(model, summary) {
  plot(model, scale = "adjr2")
  xyplot(
    summary$adjr2 ~ 1:length(summary$adjr2),
    type = "b",
    lattice.options = ggplot2like.opts(),
    main = paste0("Max Adj R2: ", round(max(summary$adjr2), 3))
  ) %>% plot()
  xyplot(
    summary$bic ~ 1:length(summary$adjr2),
    type = "b",
    lattice.options = ggplot2like.opts(),
    main = paste0("BIC: ", round(min(summary$bic), 3))
  ) %>% plot()
}
print_metrics <- function(summary){
    message('BIC: ',round(min(summary$bic),3))
    message('Max Adj R2: ',round(max(summary$adjr2),3))
}

df_bsr_1 <- df_train[c(response_sqrt, predictors)]

# Randomize rows
df_bsr_1 <- df_bsr_1[sample(1:nrow(df_bsr_1), nrow(df_bsr_1)), ]

df_bsr_1 <- as_tibble(model.matrix(~., df_bsr_1)[, -1]) %>%
  janitor::clean_names()
glimpse(df_bsr_1)

(lin_combos_to_remove <- caret::findLinearCombos(df_bsr_1)$remove)

df_bsr_1 <- df_bsr_1[-lin_combos_to_remove]

(predictors_lm_1 <- names(df_bsr_1[-1]))

fit_bsr_1 <- regsubsets(price_sqrt ~ ., df_bsr_1)
fit_bsr_1_summary <- summary(fit_bsr_1)
plot_bsr(fit_bsr_1, fit_bsr_1_summary)
print_metrics(fit_bsr_1_summary)

fit_bsr_2 <- regsubsets(price_sqrt ~ (carat + color + clarity + cut_not_ideal + channel_internet + channel_mall)^2, df_bsr_1, nvmax = 20, nbest = 1)
fit_bsr_2_summary <- summary(fit_bsr_2)
plot_bsr(fit_bsr_2, fit_bsr_2_summary)
print_metrics(fit_bsr_2_summary)

fit_bsr_3 <- regsubsets(price_sqrt ~ (carat + color + clarity + cut_not_ideal + channel_internet + channel_mall)^2+store_ausmans+store_blue_nile+store_chalmers+store_danford+store_fred_meyer+store_goodmans+store_kay+store_r_holland+store_riddles+store_university, df_bsr_1, nvmax = 20, nbest = 1)
fit_bsr_3_summary <- summary(fit_bsr_3)
plot_bsr(fit_bsr_3, fit_bsr_3_summary)
print_metrics(fit_bsr_3_summary)

## Forward Selection ----
null <- lm(price_sqrt~1, df_bsr_1)
full <- lm(price_sqrt~(.)^2, df_bsr_1)
fit_fwd <- step(null, scope=list(lower=null, upper=full), direction="forward")
summary(fit_fwd)
BIC(fit_fwd)

## Backward Selection ----
fit_bkwd <- step(full, direction="backward")
summary(fit_bkwd)
BIC(fit_bkwd)
