library("ProjectTemplate")
load.project()

glimpse(df_train)

plot_bsr <- function(model,summary){
    plot(model, scale='adjr2')
    xyplot(summary$adjr2~1:length(summary$adjr2),type='b', lattice.options = ggplot2like.opts(), main=paste0('Max Adj R2: ', round(max(summary$adjr2),2))) %>% plot()
    xyplot(summary$bic~1:length(summary$adjr2),type='b', lattice.options = ggplot2like.opts(), main=paste0('BIC: ', round(max(summary$bic),2))) %>% plot()
}

df_bsr_1 <- df_train[c(response_sqrt, predictors)]

# Randomize rows
df_bsr_1 <- df_bsr_1[sample(1:nrow(df_bsr_1), nrow(df_bsr_1)),]

df_bsr_1 <- as_tibble(model.matrix(~., df_bsr_1)[,-1]) %>%
    janitor::clean_names()
glimpse(df_bsr_1)

(lin_combos_to_remove <- caret::findLinearCombos(df_bsr_1)$remove)

df_bsr_1 <- df_bsr_1[-lin_combos_to_remove]

(predictors_lm_1 <- names(df_bsr_1[-1]))

fit_bsr_1 <- regsubsets(price_sqrt~., df_bsr_1)
fit_bsr_1_summary <- summary(fit_bsr_1)
plot_bsr(fit_bsr_1, fit_bsr_1_summary)

fit_bsr_1 <- regsubsets(price_sqrt~(.)^2, df_bsr_1, nvmax = 20, nbest = 1)

xyplot(fit_bsr_1_summary$adjr2~1:length(fit_bsr_1_summary$adjr2),type='b',
       lattice.options = theEconomist.opts())
