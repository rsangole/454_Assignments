# Read raw data
raw_data <- two.months.salary
rm("two.months.salary")

skimr::skim(raw_data)

processing_code <- function(df) {
  # TODO:
  # For non-tree models:
  # channel, store, cut -> dummy variables
  # color, clarity may or may not be dummy vars
  # log(price)
  # pca?
  df %>%
        mutate(price_log = log(price))

}

df <- processing_code(raw_data)

cache("df")

df

response <- "price"
response_log <- "price_log"
predictors <- names(df)[!(names(df) %in% c(response, response_log))]
