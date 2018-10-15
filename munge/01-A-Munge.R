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
        mutate(price_sqrt = (price)^0.2) # using lamda = 0.2, still calling it price_sqrt since I realised 0.2 much later after writing a lot of code. Not bothering to change the var name across the board now.

    }

caret::BoxCoxTrans(raw_data$price)

df <- processing_code(raw_data)

cache("df")

df

response <- "price"
response_log <- "price_log"
response_sqrt <- "price_sqrt"
predictors <-
    names(df)[!(names(df) %in% c(response, response_log, response_sqrt))]
