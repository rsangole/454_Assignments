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
    df <- df[!duplicated(df), ]
    df %>%
        mutate(price_log = log(price),
               price_sqrt = sqrt(price))

}

df <- processing_code(raw_data)

cache("df")

df

response <- "price"
response_log <- "price_log"
response_sqrt <- "price_sqrt"
predictors <-
    names(df)[!(names(df) %in% c(response, response_log, response_sqrt))]
