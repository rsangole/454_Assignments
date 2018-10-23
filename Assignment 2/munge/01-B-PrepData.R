df_raw <- df_raw %>%
  dplyr::filter(mode=="3") %>%
  dplyr::select(-mode, -row_id, -freq, -pos_z, -id) %>%
  dplyr::mutate(strength = as.numeric(strength))

cache('df_raw')

# Make the angles common and factorize

df_raw <- df_raw %>%
  mutate(
    degree_corrected = case_when(
      between(degree, -20,20) ~ 0,
      between(degree, 35,55) ~ 45,
      between(degree, 80,100) ~ 90,
      between(degree, 125,145) ~ 135,
      between(degree, 170,190) ~ 180,
      between(degree, 210,230) ~ 225,
      between(degree, 260, 280) ~ 270,
      between(degree, 305, 325) ~ 315,
      between(degree, 350, 370) ~ 0,
      TRUE ~ degree
    ),
    degree = as.factor(degree_corrected),
    degree_corrected = NULL
  )

# Remove mac addresses

# mac_to_keep <- df_raw %>% dplyr::count(mac) %>% arrange(n) %>% tail(7) %>% pull(mac)
mac_to_keep <- c("00:0f:a3:39:e1:c0","00:14:bf:3b:c7:c6","00:14:bf:b1:97:81","00:14:bf:b1:97:8a","00:14:bf:b1:97:8d","00:14:bf:b1:97:90")

df_raw <- df_raw %>%
  dplyr::filter(mac %in% mac_to_keep)

# Removing 39:dd:cd after some EDA wrk
df_raw <- df_raw %>% filter(mac != "00:0f:a3:39:dd:cd")

# Make x-y pairs

df_raw <- df_raw %>% mutate(pos_xy = paste(pos_x,pos_y,sep = '_'))


# MAC address locations

mac_locations <- tibble(
  mac = c("00:14:bf:b1:97:8a",
          "00:14:bf:b1:97:90",
          "00:0f:a3:39:e1:c0",
          "00:14:bf:b1:97:8d",
          "00:14:bf:b1:97:81",
          "00:14:bf:3b:c7:c6"),
  mac_x = c(2.5, 1, 7.5, 33.5, 33.5, 12.8),
  mac_y = c(-0.8, 14, 6.3, 9.3, 2.8, -2.8)
)

df_raw <- df_raw %>%
  left_join(mac_locations) %>%
  mutate(dist = sqrt((mac_x-pos_x)^2+(mac_y-pos_y)^2))

# df_raw %>%
#   left_join(mac_locations) %>%
#   filter(mac == mac_addresses[6]) %>%
#   group_by(pos_x, pos_y,mac_x,mac_y) %>%
#   summarise(mean_str = mean(strength),
#             sd_str = sd(strength)) %>%
#   ggplot()+
#   geom_raster(aes(x = pos_x, y = pos_y, z = mean_str, fill = mean_str),interpolate = T)+
#   scale_fill_gradient2(low = 'black',high='red',midpoint = -60,mid = 'yellow')+
#   geom_point(aes(x = pos_x, y = pos_y))+
#   geom_point(aes(x = mac_x, y = mac_y),col='red',pch=2, size = 4)+
#   labs(title=paste(i," -- ",mac_addresses[i]))+
#   theme_light()

cache('df_raw')
cache('mac_locations')


# -- Test ----

df_test_raw <- df_test_raw %>%
  dplyr::filter(mode=="3") %>%
  dplyr::select(-mode, -row_id, -freq, -pos_z, -id) %>%
  dplyr::mutate(strength = as.numeric(strength))%>%
  mutate(
    degree_corrected = case_when(
      between(degree, -20,20) ~ 0,
      between(degree, 35,55) ~ 45,
      between(degree, 80,100) ~ 90,
      between(degree, 125,145) ~ 135,
      between(degree, 170,190) ~ 180,
      between(degree, 210,230) ~ 225,
      between(degree, 260, 280) ~ 270,
      between(degree, 305, 325) ~ 315,
      between(degree, 350, 370) ~ 0,
      TRUE ~ degree
    ),
    degree = as.factor(degree_corrected),
    degree_corrected = NULL
  )
df_test_raw <- df_test_raw %>% filter(mac %in% mac_to_keep)

cache('df_test_raw')
