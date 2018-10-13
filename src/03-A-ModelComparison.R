
df_test_fac <- df_test %>%
    mutate(color = as.factor(color),
           clarity = as.factor(clarity))

df_test_mm_2 <- model.matrix(price_sqrt~(.)^2, df_test)[,-1]

pred_rpart_1 = predict(fit_rpart_1 , df_test)
pred_rpart_caret_1 = predict(fit_rpart_caret , df_test)
pred_rpart_caret_2 = predict(fit_rpart_caret_2 , df_test_fac)
pred_ctree_1 = predict(fit_ctree_1 , df_test)
pred_ctree_caret = predict(fit_ctree_caret , df_test)
pred_ctree_caret_2 = predict(fit_ctree_caret_2 , df_test_fac)
preds_glmnet_cv_2 <- predict(fit_glmnet_cv_2, s = 'lambda.1se', newx = as.matrix(df_test_mm_2))
pred_ = predict(fit_ , df_test)
pred_ = predict(fit_ , df_test)

tibble(pred_rpart_1,pred_rpart_caret_1,pred_rpart_caret_2,pred_ctree_1,
       pred_ctree_caret) %>%
    map_dbl(~caret::RMSE(.x, df_test$price_sqrt)) %>%
    dotplot()
