library(foreach)
library(tidyverse)
library(ggthemr)
ggthemr("fresh")
df <- list.files(pattern = "(csv)$") %>%
        map_df(~read_csv(.x,
                         col_types = cols(
                                 .default = col_integer(),
                                 UniqueCarrier = col_character(),
                                 TailNum = col_character(),
                                 AirTime = col_character(),
                                 Origin = col_character(),
                                 Dest = col_character(),
                                 TaxiIn = col_character(),
                                 TaxiOut = col_character(),
                                 CancellationCode = col_character(),
                                 CarrierDelay = col_character(),
                                 WeatherDelay = col_character(),
                                 NASDelay = col_character(),
                                 SecurityDelay = col_character(),
                                 LateAircraftDelay = col_character()
                         ),
                         col_names = T,
                         progress = T))

drv <- dbDriver("PostgreSQL")
con <- dbConnect(
    drv,
    dbname = "postgres",
    host = "localhost",
    port = 5433,
    user = "postgres",
    password = "ishaRA"
)

if (!dbExistsTable(con, "airlines")) {
    # specifies the details of the table
    sql_command <-
        "CREATE TABLE
    airlines(
    Year integer,
Month integer,
    DayofMonth integer,
    DayOfWeek integer,
    DepTime integer,
    CRSDepTime integer,
    ArrTime integer,
    CRSArrTime integer,
    UniqueCarrier integer,
    FlightNum integer,
    TailNum integer,
    ActualElapsedTime integer,
    CRSElapsedTime integer,
    AirTime integer,
    ArrDelay integer,
    DepDelay integer,
    Origin integer,
    Dest integer,
    Distance integer,
    TaxiIn integer,
    TaxiOut integer,
    Cancelled integer,
    CancellationCode integer,
    Diverted integer,
    CarrierDelay integer,
    WeatherDelay integer,
    NASDelay integer,
    SecurityDelay integer,
    LateAircraftDelay integer
    )
    WITH (OIDS=FALSE)
    TABLESPACE pg_default;
    ALTER TABLE aviation_safety_training OWNER TO postgres;"

    dbGetQuery(con, sql_command)

    dbWriteTable(
        conn = con,
        name = 'airlines',
        value = df,
        append = TRUE,
        row.names = FALSE
    )

}

library(dbplyr)


df$depHours <- floor(df$CRSDepTime/100)
df$depHours[df$depHours==24] <- 0
splits <- split(1:nrow(df),df$depHours)

myProbs <- c(0.50,0.75,0.90)

delayQuantiles <- foreach(hour = splits, .combine = cbind) %do% {
        quantile(df[hour,"DepDelay"][[1]], myProbs, na.rm=T)
}

colnames(delayQuantiles) <- names(splits)
delayQuantiles
melted <- melt(delayQuantiles)
names(melted) <- c("percentage","hour","delay")
melted %>%
        ggplot()+
        geom_line(aes(y=delay,x=hour,color=percentage))



splits <- split(1:nrow(df),df$DayOfWeek)
myProbs <- c(0.50,0.75,0.90)
delayQuantiles <- foreach(day = splits, .combine = cbind) %do% {
        quantile(df[day,"DepDelay"][[1]], myProbs, na.rm=T)
}
colnames(delayQuantiles) <- names(splits)
delayQuantiles
melted <- melt(delayQuantiles)
names(melted) <- c("percentage","day","delay")
melted %>%
        ggplot(aes(y=delay,x=day,color=percentage))+
        geom_line()+
        geom_point()



splits <- split(1:nrow(df),df$Month)
myProbs <- c(0.50,0.75,0.90)
delayQuantiles <- foreach(month = splits, .combine = cbind) %do% {
        quantile(df[month,"DepDelay"][[1]], myProbs, na.rm=T)
}
colnames(delayQuantiles) <- names(splits)
delayQuantiles
melted <- melt(delayQuantiles)
names(melted) <- c("percentage","month","delay")
melted %>%
        ggplot(aes(y=delay,x=month,color=percentage))+
        geom_line()+
        geom_point()

# close the connection
dbDisconnect(con)