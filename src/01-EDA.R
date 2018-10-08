library("ProjectTemplate")
load.project()

# Overall df and quality checks
df
skimr::skim(df)
str(df)

# Response
summary(df[response])
histogram(~price, df, breaks = 20)
bwplot(~price, df)

# Predictors
splom(df[predictors] %>% sample_n(100))

parallelplot(df, horizontal.axis = F)

df %>%
  plot_ly(
    type = "parcoords",
    line = list(
      color = ~log(price),
      colorscale = "Jet",
      showscale = TRUE
    ),
    dimensions = list(
      list(values = ~as.numeric(carat), label = "carat"),
      list(values = ~as.numeric(cut), label = "cut", ticktext = levels(df$cut), tickvals = 1:length(levels(df$cut))),
      list(values = ~as.numeric(clarity), label = "clarity"),
      list(values = ~as.numeric(color), label = "color"),
      list(values = ~as.numeric(channel), label = "channel", ticktext = levels(df$channel), tickvals = 1:length(levels(df$channel))),
      list(values = ~as.numeric(store), label = "store", ticktext = levels(df$store), tickvals = 1:length(levels(df$store))),
      list(values = ~as.numeric(price), label = "price")
    )
  )

axis <- function(title) {
  list(
    title = title,
    titlefont = list(size = 15),
    ticksuffix = "%",
    tickfont = list(size = 15),
    tickcolor = "rgba(0,0,0,0)",
    ticklen = 5
  )
}
df %>%
  plot_ly(
    type = "scatterternary",
    a = ~carat,
    b = ~color,
    c = ~clarity,
    text = ~channel,
    marker = list(color = ~price, symbol = 100, size = 9, line = list(width = 2))
  ) %>%
  layout(ternary = list(
    sum = 100,
    aaxis = axis("a: carat"),
    baxis = axis("b: color"),
    caxis = axis("c: clarity")
  ))

# Observations:
# very high price (20k+) diamonds are almost all from store 3 - Blue Nile - Internet sales
# 20k+ diamonts are all 2 carat, but 15k-20k diamonds have 2.0+ carat... so nonlinearity
# 17k+, only players are ashbord and blue nile via internet channels
# 11k - 20k, kay comes into play
# cheapest <2k sold by univ, riddles, blue nile
# largest sales from internet channels
# Ashford has amazing groupings of prices! (report...) while Blue Nile doesn't have these groupings....
# price~carat are mostly parallel lines, but there are intersections of lines indicating interaction terms
# ternary plots show very interesting patterns!

is_alluvia_form(df)

ggplot(aes(axis5 = cut, axis2 = color, axis4 = clarity, axis3 = channel, axis1 = store), data = df) +
  geom_alluvium(aes(fill = log(price)), width = 1 / 12) +
  geom_stratum(width = 1 / 15, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("store", "color", "channel", "clarity", "cut"), expand = c(.05, .05))


densityplot(~price, groups = cut, df, auto.key = T)
densityplot(~price, groups = color, df, auto.key = T)
densityplot(~price, groups = clarity, df, auto.key = T)
densityplot(~price, groups = channel, df, auto.key = T)
densityplot(~price, groups = store, df, auto.key = T)

table(complete.cases(df))

df %>%
    tabyl(color) %>%
    adorn_pct_formatting()
df %>%
    tabyl(cut) %>%
    adorn_pct_formatting()
df %>%
    tabyl(clarity) %>%
    adorn_pct_formatting()
df %>%
    tabyl(channel) %>%
    adorn_pct_formatting()
df %>%
    tabyl(store) %>%
    adorn_pct_formatting()

bwplot(price~cut, df, xlab = 'cut')
bwplot(price~channel, df, xlab = 'channel')
bwplot(price~store, df, xlab = 'store')
bwplot(price~as.factor(color), df, xlab = 'color')
bwplot(price~as.factor(clarity), df, xlab = 'clarity')
xyplot(price~carat,df,type=c('smooth','p'))
xyplot(price~color,df,type=c('smooth','p'))
xyplot(price~clarity,df,type=c('smooth','p'))

