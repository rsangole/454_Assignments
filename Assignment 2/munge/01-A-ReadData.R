# Eliminate any lines which start with "#"
raw_lines <- read_lines("data/offline.final.trace.txt")
raw_lines <- raw_lines[!stringr::str_detect(raw_lines, pattern = "^#")]
raw_lines_list <- map(raw_lines, ~stringr::str_split(.x,pattern = ';',simplify = T))
mask <- map_int(raw_lines_list, ~length(.x)) < 5
raw_lines_list <- raw_lines_list[!mask]
df <- tibble(
  t=map_chr(raw_lines_list,~.x[[1,1]]),
  id=map_chr(raw_lines_list,~.x[[1,2]]),
  pos=map_chr(raw_lines_list,~.x[[1,3]]),
  degree=map_chr(raw_lines_list,~.x[[1,4]])) %>%
  separate(col = 'pos', sep = ",", into = c("pos_x","pos_y","pos_z")) %>%
  map_df(~stringr::str_remove_all(.x, pattern = "[a-z]*=")) %>%
  mutate(t=as.POSIXct(x = as.numeric(t)/1000,origin = '1970-01-01 UTC'),
         pos_x = as.numeric(pos_x),
         pos_y = as.numeric(pos_y),
         pos_z = as.numeric(pos_z),
         degree = as.numeric(degree))

df <- df %>% rownames_to_column('row_id')

names(raw_lines_list) <- df$row_id
measurements <- map(raw_lines_list, ~.x[-1:-4])

make_df <- function(i,x){
  tibble(row_id = i,
         measurements = unlist(x)) %>%
    separate(measurements, sep = ",", into = c("mac", "freq", "mode")) %>%
    separate(col = 'mac', sep="=", into= c("mac","strength"))
}

df_measurements <- seq_along(measurements) %>% map_df(~make_df(.x, measurements[.x]))

df_measurements

df_raw <- df_measurements %>% left_join(df)

df_raw

skimr::skim(df_raw)

cache('df_measurements')
cache('df_raw')
# map2(raw_lines_list, df$row_id, ~)
#
#          strength = as.numeric(strength),
#          mode = as.numeric(mode))
# ,
# measurements =map_chr(raw_lines_list,~.x[[1,5]]
#                       %>%
#                         separate(measurements, sep = ",", into = c("mac", "freq", "mode")) %>%
#                         %>%
#                         separate(col = 'mac', sep="=", into= c("mac","strength")) %>%
# df$mac



# Eliminate any lines which start with "#"
raw_lines <- read_lines("data/online.final.trace.txt")
raw_lines <- raw_lines[!stringr::str_detect(raw_lines, pattern = "^#")]
raw_lines_list <- map(raw_lines, ~stringr::str_split(.x,pattern = ';',simplify = T))
mask <- map_int(raw_lines_list, ~length(.x)) < 5
raw_lines_list <- raw_lines_list[!mask]
df_test_raw <- tibble(
  t=map_chr(raw_lines_list,~.x[[1,1]]),
  id=map_chr(raw_lines_list,~.x[[1,2]]),
  pos=map_chr(raw_lines_list,~.x[[1,3]]),
  degree=map_chr(raw_lines_list,~.x[[1,4]])) %>%
  separate(col = 'pos', sep = ",", into = c("pos_x","pos_y","pos_z")) %>%
  map_df(~stringr::str_remove_all(.x, pattern = "[a-z]*=")) %>%
  rownames_to_column('row_id') %>%
  mutate(t=as.POSIXct(x = as.numeric(t)/1000,origin = '1970-01-01 UTC'),
         pos_x = as.numeric(pos_x),
         pos_y = as.numeric(pos_y),
         pos_z = as.numeric(pos_z),
         degree = as.numeric(degree),
         row_id = as.numeric(row_id))

names(raw_lines_list) <- df_test$row_id
measurements <- map(raw_lines_list, ~.x[-1:-4])

df_measurements <- seq_along(measurements) %>% map_df(~make_df(.x, measurements[.x]))

df_measurements

df_test_raw <- df_measurements %>% left_join(df_test_raw)

df_test_raw

skimr::skim(df_test_raw)

cache('df_test_raw')
