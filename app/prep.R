library(tidyverse)
library(lubridate)

## function to combine contributing countries in summarize()
comb.cc <- function(cc) {
  
  str_c(sort(unique(unlist(str_split(cc, '\\|')))), collapse = '|')
  
}

## load data
radpko_m <- data.table::fread('radpko_bases_cc_m.csv') %>% 
  as_tibble() %>% 
  mutate(year = year(date)) %>% 
  select(mission:date, year, everything(), -gid)


radpko_m %>%
  group_by(year, mission, country, base) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date), mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(all_of(names(radpko_m))) %>% 
  write_csv('radpko_bases_cc_y.csv')
