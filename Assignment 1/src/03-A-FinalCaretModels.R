library("ProjectTemplate")
load.project()
ls()
## Data Prep ----
df_train_caret <- df_train[c(predictors)] %>%
    mutate(carat_is_2 = ifelse(carat==2,1,0),
           carat_gt_2 = ifelse(carat>2,1,0))
df_test_caret <- df_test[c(predictors)] %>%
    mutate(carat_is_2 = ifelse(carat==2,1,0),
           carat_gt_2 = ifelse(carat>2,1,0))
train_y <- df_train$price
train_y_sqrt <- df_train$price_sqrt
test_y <- df_test$price
test_y_sqrt <- df_test$price_sqrt

make_modelmatrix_no_interactions <- function(df){
    as_tibble(model.matrix(~., df)[, -1]) %>%
        janitor::clean_names() %>%
        as.matrix()
}
make_modelmatrix_2way_interactions <- function(df){
    as_tibble(model.matrix(~(.)^2, df)[, -1]) %>%
        janitor::clean_names() %>%
        as.matrix()
}

df_train_mm <- make_modelmatrix_no_interactions(df_train_caret)
df_train_mm_2way <-make_modelmatrix_2way_interactions(df_train_caret)
df_test_mm <- make_modelmatrix_no_interactions(df_test_caret)
df_test_mm_2way <-make_modelmatrix_2way_interactions(df_test_caret)

## Models ----

trCtrl <- trainControl(method = 'cv',
                       number = 10,
                       returnData = T)

### Basic LM

fit_lm <- train(x = df_train_mm,
                y = train_y,
                method = 'lm',
                trControl = trCtrl,
                metric = 'RMSE')
fit_lm_sqrt <- train(x = df_train_mm,
                     y = train_y_sqrt,
                     method = 'lm',
                     trControl = trCtrl,
                     metric = 'RMSE')
# fit_lm_2w <- train(x = df_train_mm_2way,
#                 y = train_y,
#                 method = 'lm',
#                 trControl = trCtrl,
#                 metric = 'RMSE')
# fit_lm_2w_sqrt <- train(x = df_train_mm_2way,
#                      y = train_y_sqrt,
#                      method = 'lm',
#                      trControl = trCtrl,
#                      metric = 'RMSE')

### BSR, Fwd, Bckward

(fit_fwd <- train(x = df_train_mm,
                  y = train_y_sqrt,
                  method = 'leapForward',
                  trControl = trCtrl,
                  metric = 'RMSE',
                  tuneGrid = data.frame(nvmax=c(10,12,14,17))))
plot(fit_fwd)

(fit_bwd <- train(x = df_train_mm,
                  y = train_y_sqrt,
                  method = 'leapBackward',
                  trControl = trCtrl,
                  metric = 'RMSE',
                  tuneGrid = data.frame(nvmax=c(2,4,6,8,10,12,14))))
plot(fit_bwd)

(fit_fwd_2w <- train(x = df_train_mm_2way,
                     y = train_y_sqrt,
                     method = 'leapForward',
                     trControl = trCtrl,
                     metric = 'RMSE',
                     tuneGrid = data.frame(nvmax=seq(4,60,by = 2))))
plot(fit_fwd_2w)

(fit_bwd_2w <- train(x = df_train_mm_2way,
                     y = train_y_sqrt,
                     method = 'leapBackward',
                     trControl = trCtrl,
                     metric = 'RMSE',
                     tuneGrid = data.frame(nvmax=seq(4,60,by = 2))))
plot(fit_bwd_2w)


# (fit_step_2w <- train(x = df_train_mm_2way,
#                       y = train_y_sqrt,
#                       method = 'leapSeq',
#                       trControl = trCtrl,
#                       metric = 'RMSE',
#                       tuneGrid = data.frame(nvmax=c(10,20,30))))
# plot(fit_step_2w)

### Shrinkage

(fit_lasso <- train(x = df_train_mm_2way[,1:30],
                    y = train_y_sqrt,
                    method = 'lasso',
                    trControl = trCtrl,
                    metric = 'RMSE',
                    tuneGrid =  data.frame(fraction=seq(from = 0.1,to = 1,by = 0.05))))
plot(fit_lasso)

(fit_ridge <- train(x = df_train_mm_2way[,1:40],
                    y = train_y_sqrt,
                    method = 'ridge',
                    trControl = trCtrl,
                    metric = 'RMSE',
                    tuneGrid = data.frame(lambda=seq(0,0.02,0.001))))
plot(fit_ridge)

### Tree

(fit_tree <- train(x = df_train_mm_2way,
                 y = train_y_sqrt,
                 method = 'ctree',
                 trControl = trCtrl,
                 metric = 'RMSE',
                 tuneLength = 10))
plot(fit_tree)
plot(fit_tree$finalModel)

### RF

(fit_rf <- train(x = df_train_mm,
                    y = train_y_sqrt,
                    method = 'rf',
                    trControl = trCtrl,
                    metric = 'RMSE',
                    tuneGrid = data.frame(mtry=seq(2,17,2))))
(fit_rf_2w <- train(x = df_train_mm_2way,
                 y = train_y_sqrt,
                 method = 'rf',
                 trControl = trCtrl,
                 metric = 'RMSE',
                 tuneGrid = data.frame(mtry=seq(15,70,5))))
plot(fit_rf)
plot(fit_rf$finalModel)
plot(fit_rf_2w)
plot(fit_rf_2w$finalModel)

## Train Set Predictions ----

pred_train_bwd <- predict(fit_bwd)
pred_train_bwd_2w <- predict(fit_bwd_2w)
pred_train_fwd <- predict(fit_fwd)
pred_train_fwd_2w <- predict(fit_fwd_2w)
pred_train_lasso <- predict(fit_lasso)
pred_train_ridge <- predict(fit_ridge)
pred_train_lm_sqrt <- predict(fit_lm_sqrt)
pred_train_rf <- predict(fit_rf)
pred_train_rf_2w <- predict(fit_rf_2w)
pred_train_tree <- predict(fit_tree)

## Test Set Predictions ----
pred_test_bwd <- predict(fit_bwd, newdata = df_test_mm)
pred_test_bwd_2w <- predict(fit_bwd_2w, newdata = df_test_mm_2way)
pred_test_fwd <- predict(fit_fwd, newdata = df_test_mm)
pred_test_fwd_2w <- predict(fit_fwd_2w, newdata = df_test_mm_2way)
pred_test_lasso <- predict(fit_lasso, newdata = df_test_mm_2way)
pred_test_ridge <- predict(fit_ridge, newdata = df_test_mm_2way)
pred_test_lm_sqrt <- predict(fit_lm_sqrt, newdata = df_test_mm)
pred_test_rf <- predict(fit_rf, newdata = df_test_mm)
pred_test_rf_2w <- predict(fit_rf_2w, newdata = df_test_mm_2way)
pred_test_tree <- predict(fit_tree, newdata = df_test_mm_2way)

## Model Comparisons ----

ls(pattern = 'fit') %>% cat(sep = '\n')

resamps <- resamples(
    list(
        fit_bwd,
        fit_bwd_2w,
        fit_fwd,
        fit_fwd_2w,
        fit_lasso,
        fit_ridge,
        fit_lm_sqrt,
        fit_rf,
        fit_rf_2w,
        fit_tree
    ),
    modelNames = c(
        'Backward',
        'Backward 2-way',
        'Forward',
        'Forward 2-way',
        'Lasso 2-way',
        'Ridge 2-way',
        'Linear Reg',
        'RanForest',
        'RanForest 2-way',
        'Cond Tree 2-way'
    )
)
summary(resamps)

(resamps_plot_1 <- bwplot(resamps, metric = c('RMSE','MAE')))
(resamps_plot_2 <- bwplot(resamps, metric = c('Rsquared')))
(resamps_plot_3 <- dotplot(resamps, metric = c('Rsquared')))

# tibble(pred_rpart_1,pred_rpart_caret_1,pred_rpart_caret_2,pred_ctree_1,
#        pred_ctree_caret, preds_glmnet_caret) %>%
#     map_dbl(~caret::RMSE(.x, df_test$price_sqrt)) %>%
#     dotplot()

### RMSE Calculations

results_df <- tibble(
    modelNames = c(
        'Backward',
        'Backward 2-way',
        'Forward',
        'Forward 2-way',
        'Lasso 2-way',
        'Ridge 2-way',
        'Linear Reg',
        'RanForest',
        'RanForest 2-way',
        'Cond Tree 2-way'
    ),
    yhat_train = list(
        pred_train_bwd,
        pred_train_bwd_2w,
        pred_train_fwd,
        pred_train_fwd_2w,
        pred_train_lasso,
        pred_train_ridge,
        pred_train_lm_sqrt,
        pred_train_rf,
        pred_train_rf_2w,
        pred_train_tree
    ),
    yhat_test = list(
        pred_test_bwd,
        pred_test_bwd_2w,
        pred_test_fwd,
        pred_test_fwd_2w,
        pred_test_lasso,
        pred_test_ridge,
        pred_test_lm_sqrt,
        pred_test_rf,
        pred_test_rf_2w,
        pred_test_tree
    )
) %>%
    mutate(
        train_residuals = purrr::map(yhat_train, ~ (.x - train_y_sqrt)),
        test_residuals =  purrr::map(yhat_test, ~ (.x - test_y_sqrt)),
        R2_train = purrr::map_dbl(yhat_train, ~ caret::R2(.x, train_y_sqrt)),
        R2_test = purrr::map_dbl(yhat_test, ~ caret::R2(.x, test_y_sqrt)),
        RMSE_train = purrr::map_dbl(yhat_train, ~ caret::RMSE(.x, train_y_sqrt)),
        RMSE_test = purrr::map_dbl(yhat_test, ~ caret::RMSE(.x, test_y_sqrt)),
        MAE_train = purrr::map_dbl(yhat_train, ~ caret::MAE(.x, train_y_sqrt)),
        MAE_test = purrr::map_dbl(yhat_test, ~ caret::MAE(.x, test_y_sqrt))
    )
results_df
p1 <- results_df %>%
    arrange(RMSE_test) %>%
    mutate(modelNames = factor(modelNames,levels = modelNames)) %>%
    dotplot(modelNames~RMSE_train+RMSE_test,.,auto.key=T, type='b', xlab='RMSE')
p2 <- results_df %>%
    arrange(MAE_test) %>%
    mutate(modelNames = factor(modelNames,levels = modelNames)) %>%
    dotplot(modelNames~MAE_train+MAE_test,.,auto.key=T, type='b',xlab='MAE')
p3 <- results_df %>%
    arrange(-R2_test) %>%
    mutate(modelNames = factor(modelNames,levels = modelNames)) %>%
    dotplot(modelNames~R2_train+R2_test,.,auto.key=T, type='b',xlab='R Sq')
(final_result_dotplot <- gridExtra::grid.arrange(p1,p2,p3,ncol=3))


ls(pattern = 'df') %>%
    purrr::walk(.f = ~cache(.x))
ls(pattern = 'train_') %>%
    purrr::walk(.f = ~cache(.x))
ls(pattern = 'test_') %>%
    purrr::walk(.f = ~cache(.x))
ls(pattern = 'pred_') %>%
    purrr::walk(.f = ~cache(.x))
ls(pattern = 'fit_') %>%
    purrr::walk(.f = ~cache(.x))
ls(pattern = 'resamps_plot') %>%
    purrr::walk(.f = ~cache(.x))
cache('final_result_dotplot')