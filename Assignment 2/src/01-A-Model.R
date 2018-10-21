glimpse(df_raw)

# train_index <- caret::createDataPartition(df_raw$strength, p = 0.8, list = F)
df_train <- df_raw
# df_test <- df_raw[-train_index,]

df_train <- df_train %>%
  group_by(mac,pos_x,pos_y,degree) %>%
  summarise(mean_str = mean(strength)) %>%
  dcast(pos_x+pos_y+degree~mac) %>%
  as_tibble() %>%
  mutate(degree=as.numeric(degree))
df_test <- df_test_raw %>%
  group_by(mac,pos_x,pos_y,degree) %>%
  summarise(mean_str = mean(strength)) %>%
  dcast(pos_x+pos_y+degree~mac) %>%
  as_tibble() %>%
  mutate(degree=as.numeric(degree))

find_knn <- function(df_train, test_row, k){
  train_dist_mat <- as.matrix(df_train[,-1:-2])
  test_strengths <- as.matrix(test_row[,-1:-2])
  euc_dist = numeric()
  for (i in 1:nrow(train_dist_mat)) {
    euc_dist=c(euc_dist,sqrt(sum((train_dist_mat[i,] - test_strengths)^2)))
  }
  df_train %>%
    dplyr::mutate(euc_dist = euc_dist) %>%
    dplyr::select(pos_x,pos_y,degree,euc_dist) %>%
    dplyr::arrange(euc_dist) %>%
    head(k)
}
get_xy_hats <- function(df_train, test_row, k){
  nearest_neighbours <- find_knn(df_train, test_row, k)
  x_hat <- mean(nearest_neighbours$pos_x)
  y_hat <- mean(nearest_neighbours$pos_y)
  paste(x_hat,y_hat,sep = '_')
}
get_errors <- function(df_train, df_test, k, type){
  euc_dist <- character()
  for (i in 1:nrow(df_test)) {
    euc_dist <- c(euc_dist,get_xy_hats(df_train, df_test[i,], k))
  }
  df_test$euc_dist <- euc_dist
  df_test <- df_test %>% separate(euc_dist, sep = "_", into = c("x_hat","y_hat"))
  df_test <- df_test %>%
    select(matches("[xy]")) %>%
    map_df(~as.numeric(.x)) %>%
    mutate(err = sqrt((x_hat-pos_x)^2+(y_hat-pos_y)^2))
  if(type=='mean')
    return(mean(df_test$err))
  if(type=='median')
    return(median(df_test$err))
  }
get_predictions <- function(df_train, df_test, k){
  euc_dist <- character()
  for (i in 1:nrow(df_test)) {
    euc_dist <- c(euc_dist,get_xy_hats(df_train, df_test[i,], k))
  }
  df_test$euc_dist <- euc_dist
  df_test <- df_test %>% separate(euc_dist, sep = "_", into = c("x_hat","y_hat"))
  df_test %>%
    select(matches("[xy]")) %>%
    map_df(~as.numeric(.x)) %>%
    mutate(err = (sqrt((x_hat-pos_x)^2)+sqrt((y_hat-pos_y)^2))/2)
    # mutate(err = sqrt((x_hat-pos_x)^2+(y_hat-pos_y)^2))
}

k_search_median <- 1:15 %>% map_dbl(~get_errors(df_train, df_test, .x, 'median'))
xyplot(k_search_median~1:15,type="o")

k_search_mean <- 1:15 %>% map_dbl(~get_errors(df_train, df_test, .x, 'mean'))
xyplot(k_search_mean~1:15,type="o")

kFit <- get_predictions(df_train,df_test,6)

kFit %>%
  ggplot()+
  geom_point(aes(pos_x,pos_y),col='darkgray',size=1)+
  geom_segment(aes(x = pos_x,y = pos_y,xend=x_hat,yend=y_hat),arrow = arrow(ends = 'last',length = unit(3,'mm')),col='darkgray')+
  geom_point(aes(x_hat,y_hat),col='red',size=1)
