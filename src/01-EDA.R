library("ProjectTemplate")
load.project()

##---- Overall df_train and quality checks -----
df_train
skimr::skim(df_train)
str(df_train)

##---- Response ----
summary(df_train[response])
histogram(~price, df_train, breaks = 20)
bwplot(~price, df_train)

##----  Predictors ----
splom(df_train[predictors] %>% sample_n(100))

parallelplot(df_train, horizontal.axis = F)

df_train %>%
  plot_ly(
    type = "parcoords",
    line = list(
      color = ~price_sqrt,
      colorscale = "Jet",
      showscale = TRUE
    ),
    dimensions = list(
      list(values = ~as.numeric(carat), label = "carat"),
      list(values = ~as.numeric(cut), label = "cut", ticktext = levels(df_train$cut), tickvals = 1:length(levels(df_train$cut))),
      list(values = ~as.numeric(clarity), label = "clarity"),
      list(values = ~as.numeric(color), label = "color"),
      list(values = ~as.numeric(channel), label = "channel", ticktext = levels(df_train$channel), tickvals = 1:length(levels(df_train$channel))),
      list(values = ~as.numeric(store), label = "store", ticktext = levels(df_train$store), tickvals = 1:length(levels(df_train$store))),
      list(values = ~round(as.numeric(price_sqrt),digits = 2), label = "price")
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
df_train %>%
  plot_ly(
    type = "scatterternary",
    a = ~carat,
    b = ~color,
    c = ~clarity,
    text = ~channel,
    marker = list(color = ~price, symbol = 100, size = 9, line = list(width = 2)),
    height = 400
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

is_alluvia_form(df_train)

ggplot(aes(axis5 = cut, axis2 = color, axis4 = clarity, axis3 = channel, axis1 = store), data = df_train) +
  geom_alluvium(aes(fill = log(price)), width = 1 / 12) +
  geom_stratum(width = 1 / 15, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("store", "color", "channel", "clarity", "cut"), expand = c(.05, .05))


densityplot(~price, groups = cut, df_train, auto.key = T)
densityplot(~price, groups = color, df_train, auto.key = T)
densityplot(~price, groups = clarity, df_train, auto.key = T)
densityplot(~price, groups = channel, df_train, auto.key = T)
densityplot(~price, groups = store, df_train, auto.key = T)

table(complete.cases(df_train))

df_train %>%
    tabyl(color) %>%
    adorn_pct_formatting()
df_train %>%
    tabyl(cut) %>%
    adorn_pct_formatting()
df_train %>%
    tabyl(clarity) %>%
    adorn_pct_formatting()
df_train %>%
    tabyl(channel) %>%
    adorn_pct_formatting()
df_train %>%
    tabyl(store) %>%
    adorn_pct_formatting()

bwplot(price_sqrt~cut, df_train, xlab = 'cut')
bwplot(price_sqrt~channel, df_train, xlab = 'channel')
bwplot(price_sqrt~store, df_train, xlab = 'store')
bwplot(price_sqrt~store,df_train, xlab = 'store')
bwplot(price_sqrt~as.factor(color), df_train, xlab = 'color')
bwplot(price_sqrt~as.factor(clarity), df_train, xlab = 'clarity')
xyplot(price_sqrt~carat,df_train,type=c('smooth','p'))
xyplot(price_sqrt~color,df_train,type=c('smooth','p'))
xyplot(price_sqrt~clarity,df_train,type=c('smooth','p'))

#---- Correlation Plots ----
cormat <- cor(df_train[,c('clarity','color','carat','price_sqrt')])
corrplot::corrplot(cormat, method = 'number', type = 'lower', diag = F, order = 'hclust', number.cex = 2, tl.col = 'black')


# For reporting
lvlplot <- levelplot(price_sqrt~store+carat,df_train,pretty = T,scales = list(x=list(rot=45)),cuts = 14)
cache('lvlplot')

p1 <- xyplot(price~carat,df_train,type=c('smooth','p'),col.line='red',lwd=2)
p2 <- xyplot(price_sqrt~carat,df_train,type=c('smooth','p'),col.line='red',lwd=2)
carat_price_xy <- gridExtra::grid.arrange(p1,p2,ncol=2)
cache('carat_price_xy')

p1 <- xyplot(price_sqrt~carat,df_train,type=c('smooth','p'),col.line='red',lwd=2)
p2 <- xyplot(price_sqrt~color,df_train,type=c('smooth','p'),col.line='red',lwd=2)
p3 <- xyplot(price_sqrt~clarity,df_train,type=c('smooth','p'),col.line='red',lwd=2)
p4 <- xyplot(price_sqrt~cut,df_train,type=c('smooth','p'),col.line='red',lwd=2)
xy_3way <- gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)
cache('xy_3way')

