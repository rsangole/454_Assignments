library("ProjectTemplate")
load.project()

glimpse(df)

df_02a_p <- df[c(response, predictors)]
glimpse(df_02a_p)

lm_model <- lm(price~., df_02a_p)
summary(lm_model)

lm_audit <- audit(lm_model, label = 'lm_model', data = df_02a_p, y = df_02a_p$price)
plot(lm_audit)
plot(lm_audit, type="Autocorrelation")
plot(lm_audit, type="ACF")
plot(lm_audit, type="ScaleLocation")
plotHalfNormal(lm_audit)
plotPrediction(lm_audit)
plotResidualDensity(lm_audit)
plot(lm_audit, type="CooksDistance")

explainer_glm <- DALEX::explain(lm_model, data=df_02a_p, y=df_02a_p$price)
modelDown::modelDown(explainer_glm)


df_02a_pl <- df[c(response_log, predictors)]
glimpse(df_02a_pl)

lm_model2 <- lm(price_log~., df_02a_pl)
summary(lm_model2)

lm_audit2 <- audit(lm_model2, label = 'lm_model2', data = df_02a_pl, y = df_02a_pl$price_log)
plot(lm_audit2)
plot(lm_audit2, type="Autocorrelation")
plot(lm_audit2, type="ACF")
plot(lm_audit2, type="ScaleLocation")
plotHalfNormal(lm_audit2)
plotPrediction(lm_audit2)
plotResidualDensity(lm_audit2)
plot(lm_audit2, type="CooksDistance")

explainer_glm2 <- DALEX::explain(lm_model2, data=df_02a_pl, y=df_02a_pl$price_log)

modelDown::modelDown(explainer_glm,explainer_glm2)

lm(price_log~store+cut+channel+carat+as.factor(color)+as.factor(clarity), df_02a_pl) %>% summary()

df_02a_pl %>%
    mutate(color=as.factor(color),
           clarity=as.factor(clarity))-> df_02a_mm
# %>% model.matrix(price_log~.-1,.) %>% as_tibble()

df_02a_mm %>% caret::findLinearCombos() -> lincomb

df_02a_mm <- df_02a_mm[,-lincomb$remove]

df_02a_mm$price_log = df_02a_pl$price_log

lm_model2 <- lm(price_log~., df_02a_mm)
lm_audit2 <- audit(lm_model2, label = 'lm_model2')
plot(lm_audit2, type='Residual', variable='store')
plot(lm_audit2, type='Residual')
plotResidualDensity(lm_audit2, variable = 'store')
plotResidualDensity(lm_audit2, variable = 'color')
plotResidualDensity(lm_audit2, variable = 'channel')
plot(lm_audit2, type="Autocorrelation")
plot(lm_audit2, type="ACF")
plot(lm_audit2, type="ScaleLocation")
plotHalfNormal(lm_audit2)
plotPrediction(lm_audit2)
plotResidualDensity(lm_audit2)
plot(lm_audit2, type="CooksDistance", n=10)

explainer_glm2 <- DALEX::explain(lm_model2, data=df_02a_pl, y=df_02a_pl$price_log)

modelDown::modelDown(explainer_glm,explainer_glm2)
