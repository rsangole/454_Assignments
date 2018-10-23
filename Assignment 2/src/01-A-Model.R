library(ggthemr)
ggthemr("flat dark")
glimpse(df_raw)

# train_index <- caret::createDataPartition(df_raw$strength, p = 0.8, list = F)
df_train <- df_raw
# df_test <- df_raw[-train_index,]

df_train <- df_train %>%
  group_by(mac, pos_x, pos_y, degree) %>%
  summarise(mean_str = mean(strength)) %>%
  dcast(pos_x + pos_y + degree ~ mac) %>%
  as_tibble() %>%
  mutate(degree = as.numeric(degree))
df_test <- df_test_raw %>%
  group_by(mac, pos_x, pos_y, degree) %>%
  summarise(mean_str = mean(strength)) %>%
  dcast(pos_x + pos_y + degree ~ mac) %>%
  as_tibble() %>%
  mutate(degree = as.numeric(degree))

assertive::assert_all_are_true(names(df_train) == names(df_test))
find_knn_wted <- function(df_train, test_row, k, d_type) {
  train_dist_mat <- as.matrix(df_train[, -1:-2])
  test_strengths <- as.matrix(test_row[, -1:-2])
  distance <- numeric()
  switch(d_type,
    euc = {
      for (i in 1:nrow(train_dist_mat)) {
        distance <- c(distance, sqrt(sum((train_dist_mat[i, ] - test_strengths)^2)))
      }
    },
    l1 = {
      for (i in 1:nrow(train_dist_mat)) {
        distance <- c(distance, (sum(abs(train_dist_mat[i, ] - test_strengths))))
      }
    }
  )

  df_train <- df_train %>%
    dplyr::mutate(distance = distance) %>%
    dplyr::select(pos_x, pos_y, degree, distance) %>%
    dplyr::arrange(distance)

  sum_wt <- sum(1 / df_train$distance)
  weights <- (1 / df_train$distance) / sum_wt
  df_train$distance <- df_train$distance * weights

  df_train %>%
    dplyr::arrange(distance) %>%
    head(k)
}
find_knn <- function(df_train, test_row, k, d_type) {
  train_dist_mat <- as.matrix(df_train[, -1:-2])
  test_strengths <- as.matrix(test_row[, -1:-2])
  distance <- numeric()
  switch(d_type,
    euc = {
      for (i in 1:nrow(train_dist_mat)) {
        distance <- c(distance, sqrt(sum((train_dist_mat[i, ] - test_strengths)^2)))
      }
    },
    l1 = {
      for (i in 1:nrow(train_dist_mat)) {
        distance <- c(distance, (sum(abs(train_dist_mat[i, ] - test_strengths))))
      }
    }
  )
  df_train %>%
    dplyr::mutate(distance = distance) %>%
    dplyr::select(pos_x, pos_y, degree, distance) %>%
    dplyr::arrange(distance) %>%
    head(k)
}
get_xy_hats <- function(df_train, test_row, k, d_type, wted) {
  if (wted) {
    nearest_neighbours <- find_knn_wted(df_train, test_row, k, d_type)
  } else {
    nearest_neighbours <- find_knn(df_train, test_row, k, d_type)
  }

  x_hat <- mean(nearest_neighbours$pos_x)
  y_hat <- mean(nearest_neighbours$pos_y)
  paste(x_hat, y_hat, sep = "_")
}
get_errors <- function(df_train, df_test, k, type, d_type, wted) {
  euc_dist <- character()
  for (i in 1:nrow(df_test)) {
    euc_dist <- c(euc_dist, get_xy_hats(df_train, df_test[i, ], k, d_type, wted))
  }
  df_test$euc_dist <- euc_dist
  df_test <- df_test %>% separate(euc_dist, sep = "_", into = c("x_hat", "y_hat"))
  df_test <- df_test %>%
    select(matches("[xy]")) %>%
    map_df(~as.numeric(.x)) %>%
    mutate(err = sqrt((x_hat - pos_x)^2 + (y_hat - pos_y)^2))
  if (type == "mean") {
    return(mean(df_test$err))
  }
  if (type == "median") {
    return(median(df_test$err))
  }
}
get_predictions <- function(df_train, df_test, k, d_type, wted) {
  distance <- character()
  for (i in 1:nrow(df_test)) {
    distance <- c(distance, get_xy_hats(df_train, df_test[i, ], k, d_type, wted))
  }
  df_test$distance <- distance
  df_test <- df_test %>%
    separate(distance, sep = "_", into = c("x_hat", "y_hat"))
  df_test %>%
    select(matches("[xy]")) %>%
    map_df(~as.numeric(.x)) %>%
    mutate(err = (sqrt((x_hat - pos_x)^2) + sqrt((y_hat - pos_y)^2)) / 2)
}
plot_search <- function(fit, k_space, model_name) {
  tibble(fit = fit, k_space = k_space) %>%
    ggplot() +
    geom_line(aes(x = k_space, y = fit)) +
    geom_point(aes(x = k_space, y = fit)) +
    geom_point(aes(
      x = k_space[which(fit == min(fit))],
      y = min(fit)
    ), col = "yellow", size = 2) +
    labs(title = paste0("Model name: ", model_name, "       Min error: ", round(min(fit), 3)))
}

k_search_mean_l1 <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "mean", "l1", FALSE))
k_search_mean_euc <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "mean", "euc", FALSE))
k_search_median_l1 <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "median", "l1", FALSE))
k_search_median_euc <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "median", "euc", FALSE))
k_search_median_l1_wted <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "median", "l1", TRUE))
k_search_median_euc_wted <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "median", "euc", TRUE))
k_search_mean_euc_wted <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "mean", "euc", TRUE))
k_search_mean_l1_wted <- 1:30 %>% map_dbl(~get_errors(df_train, df_test, .x, "mean", "l1", TRUE))


result_summary <- tibble(
  model_names = c("mean_l1",
                  "mean_euc",
                  "median_l1",
                  "median_euc",
                  "mean_euc_wted",
                  "mean_l1_wted",
                  "median_euc_wted",
                  "median_l1_wted"),
  avg_error = c(list(k_search_mean_l1),
                list(k_search_mean_euc),
                list(k_search_median_l1),
                list(k_search_median_euc),
                list(k_search_mean_euc_wted),
                list(k_search_mean_l1_wted),
                list(k_search_median_euc_wted),
                list(k_search_median_l1_wted))
) %>%
  mutate(min_error = map_dbl(avg_error, ~min(.x)),
         k_at_min = map_dbl(avg_error, ~which(.x == min(.x)))) %>%
  arrange(min_error)

result_summary %>%
  ggplot(aes(forcats::fct_reorder(model_names, min_error,.desc = T), min_error))+
  geom_col()+
  coord_flip()+
  labs(x="")

plot_search(k_search_mean_l1, 1:30, "mean_l1")
plot_search(k_search_mean_euc, 1:30, "mean_euc")
plot_search(k_search_median_l1, 1:30, "median_l1")
plot_search(k_search_median_euc, 1:30, "median_euc")
plot_search(k_search_mean_euc_wted, 1:30, "mean_euc_wted")
plot_search(k_search_mean_l1_wted, 1:30, "mean_l1_wted")
plot_search(k_search_median_euc_wted, 1:30, "median_euc_wted")
plot_search(k_search_median_l1_wted, 1:30, "median_l1_wted")

kFit_l1 <- get_predictions(df_train, df_test, 15, "l1", FALSE)

kFit_l1 %>%
  ggplot()+
  geom_point(aes(pos_x,x_hat),col='yellow')+
  geom_point(aes(pos_y,y_hat))+
  geom_abline(slope = 1, intercept = 0, col='gray')+
  labs(x='x  or  y', y='x_hat  or  y_hat',caption="x values in yellow")


g1 <- kFit_l1 %>% ggplot(aes(pos_x,x_hat))+
  geom_abline(slope = 1, intercept = 0, col='gray') +
  geom_point()+
  coord_equal()
g2 <- kFit_l1 %>% ggplot(aes(pos_y,y_hat))+
  geom_abline(slope = 1, intercept = 0, col='gray') +
  geom_point()+
  coord_equal()
gridExtra::grid.arrange(g1,g2,ncol=2)

kFit_l1$type <- "l1"

# kFit_euc <- get_predictions(df_train, df_test, 6, "euc", TRUE)
kFit_euc$type <- "euc"

bind_rows(kFit_l1, kFit_euc) %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(y = x_hat, x = pos_x, color = type))

bind_rows(kFit_l1, kFit_euc) %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(y = y_hat, x = pos_y, color = type))

bind_rows(kFit_l1, kFit_euc) %>%
  ggplot() +
  geom_density(aes(err, fill = type), alpha = .4)


cache("k_search_mean_l1")
cache("k_search_mean_euc")
cache("k_search_median_l1")
cache("k_search_median_euc")
cache("k_search_mean_euc_wted")
cache("k_search_mean_l1_wted")
cache("k_search_median_euc_wted")
cache("k_search_median_l1_wted")
cache("result_summary")
cache("kFit_l1")
