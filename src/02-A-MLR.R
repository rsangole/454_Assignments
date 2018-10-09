library("ProjectTemplate")
load.project()

glimpse(df)
make_model_formula <- function(response, predictors){
    paste0(eval(response),'~',paste0(eval(predictors),collapse = "+"))
}
build_model_and_explore <- function(formula, dataset, modelname, wt){
    lm_model <- lm(formula, dataset, weights = wt)
    print(summary(lm_model))
    lm_audit <- audit(lm_model, label = modelname)
    print(plot(lm_audit))
    print(plot(lm_audit, type="Autocorrelation"))
    print(plot(lm_audit, type="ScaleLocation"))
    print(plotHalfNormal(lm_audit))
    print(plotPrediction(lm_audit))
    print(plotResidualDensity(lm_audit))
    print(plot(lm_audit, type="CooksDistance"))
    list(lm_model, lm_audit)
}


df_sqrt <- df[c(response_sqrt, predictors)]
glimpse(df_sqrt)

# wt = (1/fitted(lm(model_lm$residuals~df_sqrt$store)))^2

result <- build_model_and_explore(formula = make_model_formula(response_sqrt,predictors),
                        dataset = df_sqrt,
                        modelname = 'Sqrt Model',
                        wt = wt)

model_lm <- result[[1]]

xyplot(model_lm$residuals~model_lm$fitted.values)
xyplot(model_lm$residuals~model_lm$fitted.values|df_sqrt$store)
xyplot(model_lm$residuals~model_lm$fitted.values,groups=df_sqrt$channel,pch='O')

explainer_glm2 <- DALEX::explain(lm_model2, data=df_02a_pl, y=df_02a_pl$price_log)

modelDown::modelDown(explainer_glm,explainer_glm2)
