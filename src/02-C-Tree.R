library("ProjectTemplate")
load.project()

glimpse(df_train)

df_tree_1 <- df_train[c(response_sqrt, predictors)]


## rpart trees ----

# Randomize rows
df_tree_1 <- df_tree_1[sample(1:nrow(df_tree_1), nrow(df_tree_1)),]

library(rpart)
library(rpart.plot)

(fit_rpart_1 <- rpart::rpart(price_sqrt~., df_tree_1, control = rpart::rpart.control(xval = 5))
)
rpart.rules(fit_rpart_1)
rpart.plot(fit_rpart_1)
cache('fit_rpart_1')
plotcp(fit_rpart_1)
rpart::rsq.rpart(fit_rpart_1)
pred_rpart <- predict(fit_rpart_1)

caret::RMSE(pred_rpart, df_tree_1$price_sqrt)
caret::MAE(pred_rpart, df_tree_1$price_sqrt)

fit_rpart_caret <- train(x = df_tree_1[,-1],
      y = df_tree_1$price_sqrt,
      method = 'rpart',
      trControl = trainControl(method = 'cv', number = 5))
fit_rpart_caret
fit_rpart_caret$finalModel
plot(fit_rpart_caret)
caret::RMSE(predict(fit_rpart_caret), df_tree_1$price_sqrt)
caret::MAE(predict(fit_rpart_caret), df_tree_1$price_sqrt)


# Converting some vars to factors
df_tree_2 <- df_tree_1 %>%
    mutate(color = as.factor(color),
           clarity = as.factor(clarity))
fit_rpart_caret_2 <- train(x = df_tree_2[,-1],
                         y = df_tree_2$price_sqrt,
                         method = 'rpart',
                         trControl = trainControl(method = 'cv', number = 5))
fit_rpart_caret_2
fit_rpart_caret_2$finalModel
plot(fit_rpart_caret_2$finalModel)
caret::RMSE(predict(fit_rpart_caret_2), df_tree_2$price_sqrt)
caret::MAE(predict(fit_rpart_caret_2), df_tree_2$price_sqrt)


## ctree ----

library(partykit)

(fit_ctree_1 <- ctree(price_sqrt~., df_tree_1))
plot(fit_ctree_1)
pred_ctree <- predict(fit_ctree_1)

caret::RMSE(pred_ctree, df_tree_1$price_sqrt)
caret::MAE(pred_ctree, df_tree_1$price_sqrt)

fit_ctree_caret <- train(x = df_tree_1[,-1],
      y = df_tree_1$price_sqrt,
      method = 'ctree',      ,
      trControl = trainControl(method = 'cv', number = 5))

fit_ctree_caret
plot(fit_ctree_caret)
print(fit_ctree_caret$finalModel)
caret::RMSE(predict(fit_ctree_caret), df_tree_1$price_sqrt)
caret::MAE(predict(fit_ctree_caret), df_tree_1$price_sqrt)

fit_ctree_caret_2 <- train(x = df_tree_2[,-1],
                         y = df_tree_2$price_sqrt,
                         method = 'ctree',      ,
                         trControl = trainControl(method = 'cv', number = 5))
fit_ctree_caret_2
plot(fit_ctree_caret_2)
caret::RMSE(predict(fit_ctree_caret_2), df_tree_1$price_sqrt)
caret::MAE(predict(fit_ctree_caret_2), df_tree_1$price_sqrt)

