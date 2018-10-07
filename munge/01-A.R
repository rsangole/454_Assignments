# Read raw data
raw_data <- two.months.salary
rm('two.months.salary')

skimr::skim(raw_data)

processing_code <- function(df){
  df
}

df <- processing_code(raw_data)

cache('df')

df

response <- 'price'
predictors <- head(names(df),-1)