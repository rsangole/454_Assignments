library("ProjectTemplate")
load.project()

glimpse(df_train)

df_rf_1 <- df_train[c(response_sqrt, predictors)]

# RF ----

fit_rf_caret <- train(
  x = df_rf_1[, -1],
  y = df_rf_1$price_sqrt,
  method = "rf", ,
  trControl = trainControl(method = "cv", number = 5)
)
fit_rf_caret
fit_rf_caret$finalModel
plot(fit_rf_caret)
caret::RMSE(predict(fit_rf_caret), df_rf_1$price_sqrt)
caret::MAE(predict(fit_rf_caret), df_rf_1$price_sqrt)

# Converting some vars to factors
df_rf_2 <- df_rf_1 %>%
    mutate(color = as.factor(color),
           clarity = as.factor(clarity))
fit_rf_caret_2 <- train(x = df_rf_2[,-1],
                           y = df_rf_2$price_sqrt,
                           method = 'rf',
                           trControl = trainControl(method = 'cv', number = 5))
fit_rf_caret_2
fit_rf_caret_2$finalModel
plot(fit_rf_caret_2)
caret::RMSE(predict(fit_rf_caret_2), df_rf_2$price_sqrt)
caret::MAE(predict(fit_rf_caret_2), df_rf_2$price_sqrt)

xyplot((df_rf_2$price_sqrt-predict(fit_rf_caret))~(df_rf_2$price_sqrt-predict(fit_rf_caret_2)),asp=1,panel=function(...){panel.abline(a = c(0,1));panel.xyplot(...)}, )
