set.seed(10)

train_index <- caret::createDataPartition(df$price_sqrt, times = 1, p = 0.7, list = F)

df_train <- df[train_index,]
df_test <- df[-train_index,]

# ecdf(df$price_sqrt) %>% plot()
# ecdf(df_train$price_sqrt) %>% plot()
# ecdf(df_test$price_sqrt) %>% plot()
# densityplot(~df_train$price_sqrt+df_test$price_sqrt)

cache('df_train')
cache('df_test')