opar <- trellis.par.get()
trellis.par.set(ggplot2like.opts())
oopt <- lattice.options(ggplot2like.opts())

# Univariate ----

skim(df_raw)

df_raw %>% histogram(~strength,.)
BoxCoxTrans(abs(df_raw$strength))

df_raw %>% histogram(~pos_x,.)
df_raw %>% histogram(~pos_y,.)
df_raw %>% histogram(~degree,.)
df_raw %>% densityplot(~degree,.)
plot(ecdf(df_raw$degree),pch='.',ylab='ECDF',xlab='degree',main='')
plot(ecdf(df_raw$strength),pch='.',ylab='ECDF',xlab='strength',main='')
plot(ecdf(df_raw$pos_x),pch='.',ylab='ECDF',xlab='pos_x',main='')
plot(ecdf(df_raw$pos_y),pch='.',ylab='ECDF',xlab='pos_y',main='')

df_raw %>% tabyl(mac) %>% adorn_pct_formatting()
df_raw %>% tabyl(t) %>% adorn_pct_formatting()

# Multivariate ----
bwplot(strength~mac, df_raw, scales = list(x=list(rot=45)))
xyplot(jitter(pos_y)~jitter(pos_x), df_raw)
ecdfplot(~strength|degree,df_raw)
ecdfplot(~strength|mac,df_raw)
densityplot(~strength|mac,df_raw, plot.points=F)
marginal.plot(~df_raw[,c(1,2,4,5)], df_raw, plot.points = F,groups = df_raw$degree)

bwplot(strength~mac|degree, df_raw, scales = list(x=list(rot=45)))
bwplot(strength~degree|mac, df_raw, scales = list(x=list(rot=45)))

df_raw %>%
  group_by(mac,degree) %>%
  summarise(mean_str = mean(strength),
            sd_str = sd(strength)) %>%
  ungroup() %>%
  dotplot(mean_str~mac,groups=degree,.,auto.key=list(columns=9), scales = list(x=list(rot=45)))
df_raw %>%
  group_by(mac,degree) %>%
  summarise(mean_str = mean(strength),
            sd_str = sd(strength)) %>%
  ungroup() %>%
  dotplot(sd_str~mac,groups=degree,.,auto.key=list(columns=9), scales = list(x=list(rot=45)))

df_raw %>%
  group_by(mac,degree,pos_xy) %>%
  summarise(mean_str = mean(strength),
            sd_str = sd(strength)) %>%
  bwplot(sd_str~cut(mean_str,breaks = seq(-90,-30,5)),.)

mac_addresses <- unique(df_raw$mac)
for (i in 1:6) {
  df_raw %>%
    filter(mac == mac_addresses[i]) %>%
    group_by(mac_x, mac_y, pos_x, pos_y) %>%
    summarise(mean_str = mean(strength),
              sd_str = sd(strength)) %>%
    ggplot(aes(x = pos_x, y = pos_y, z = mean_str, fill = mean_str))+
    geom_raster(interpolate = T)+
    scale_fill_gradient2(low = 'black',high='red',midpoint = -60,mid = 'yellow')+
    geom_point()+
    geom_point(aes(x = mac_x, y = mac_y),col='blue',pch=2, size = 4)+
    labs(title=paste(i," -- ",mac_addresses[i]))+
    theme_light() -> p
  plot(p)
}


# Distance

xyplot(strength~dist|mac+degree, df_raw, alpha=0.2)
xyplot(strength~dist|degree, df_raw, alpha=0.2)
