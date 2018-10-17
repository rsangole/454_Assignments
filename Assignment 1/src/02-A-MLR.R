library("ProjectTemplate")
load.project()

glimpse(df_train)
make_model_formula <- function(response, predictors){
    paste0(eval(response),'~',paste0(eval(predictors),collapse = "+"))
}
build_model_and_explore <- function(formula, dataset, modelname, wt){
    lm_model <- lm(formula, dataset)
    print(summary(lm_model))
    lm_audit <- audit(lm_model, label = modelname)
    # wt = (1/fitted(lm(fit_lm_1$residuals~df_lm_1$store)))^2
    print(plot(lm_audit))
    print(plot(lm_audit, type="Autocorrelation"))
    print(plot(lm_audit, type="ScaleLocation"))
    print(plotHalfNormal(lm_audit))
    print(plotPrediction(lm_audit))
    print(plotResidualDensity(lm_audit))
    print(plot(lm_audit, type="CooksDistance"))
    list(lm_model, lm_audit)
}


df_lm_1 <- df_train[c(response, predictors)]

# Randomize rows
# df_lm_1 <- df_lm_1[sample(1:nrow(df_lm_1), nrow(df_lm_1)),]

df_lm_1 <- as_tibble(model.matrix(~., df_lm_1)[,-1]) %>%
    janitor::clean_names()
glimpse(df_lm_1)

(lin_combos_to_remove <- caret::findLinearCombos(df_lm_1)$remove)

df_lm_1 <- df_lm_1[-lin_combos_to_remove]

(predictors_lm_1 <- names(df_lm_1[-1]))

result <- build_model_and_explore(formula = make_model_formula(response,predictors_lm_1),
                        dataset = df_lm_1,
                        modelname = 'Lin Model',
                        wt = wt)

fit_lm_1 <- result[[1]]

xyplot(fit_lm_1$residuals~fit_lm_1$fitted.values, xlab='Fitted',ylab='Residuals',panel = function(...){panel.abline(h = 0,col = 'gray');panel.xyplot(...,type = c('smooth','p'),col.line = 'red',lwd = 1)})

# explainer_glm2 <- DALEX::explain(lm_model2, data=df_02a_pl, y=df_02a_pl$price_log)
#
# modelDown::modelDown(explainer_glm,explainer_glm2)
