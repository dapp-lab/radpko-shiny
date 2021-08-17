library(tidyverse)
library(lubridate)
library(sf)
library(rmapshaper)

## function to combine contributing countries in summarize()
comb.cc <- function(cc) {
  
  str_replace(str_c(sort(unique(unlist(str_split(cc, '\\|')))), collapse = '|'),
              '0\\|', '')
  
}

## read in bas data
radpko_m <- data.table::fread('radpko_bases_cc.csv') %>% 
  as_tibble() %>% 
  mutate(year = year(date), date = ymd(date)) %>% 
  select(mission:date, year, everything())

radpko_cols <- names(radpko_m)
radpko_cols <- radpko_cols[!radpko_cols %in% c('base', 'longitude', 'latitude')]
radpko_cols <- c('id', radpko_cols)


ssa_gadm <- c('Chad', 'Central African Republic', 'Mali',
              'Democratic Republic of the Congo', 'Burundi', 'Sudan',
              'South Sudan', 'Sierra Leone', 'Liberia', "Côte d''Ivoire")

gadm_0 <- st_read('~/Dropbox/Datasets/GADM/gadm34_levels_gpkg/gadm34_levels.gpkg',
                  layer = 'level0',
                  query = str_c('SELECT NAME_0, geom FROM level0 WHERE NAME_0 IN (',
                                str_c("'", ssa_gadm, "'", collapse = ', '), ')'))

gadm_2 <- st_read('~/Dropbox/Datasets/GADM/gadm34_levels_gpkg/gadm34_levels.gpkg',
                  layer = 'level2',
                  query = str_c('SELECT NAME_2, geom FROM level2 WHERE NAME_0 IN (',
                                str_c("'", ssa_gadm, "'", collapse = ', '), ')'))

prio <- st_read("~/Dropbox/WashU/Projects/Will's Book/Data/priogrid_cellshp/priogrid_cell.shp",
                query = 'SELECT gid, "_ogr_geometry_" FROM priogrid_cell',
                wkt_filter = gadm_0 %>% st_geometry() %>% st_union() %>% st_as_text())

## simplify spatial objects
gadm_0 <- ms_simplify(gadm_0, keep = .05, keep_shapes = T)
gadm_2 <- ms_simplify(gadm_2, keep = .05, keep_shapes = T)

## create sf objects
radpko_m_bases_sf <- radpko_m %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>% 
  st_set_crs(st_crs(4326))

## create ADM0 spatial object
radpko_m_adm0_sf <- st_join(gadm_0, radpko_m_bases_sf, left = F) %>% 
  select(-base, -gid) %>% 
  group_by(NAME_0, date, year) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -country, -mission,
                     -geom),
                   sum, na.rm = T),
            across(c(matches('^cc|_cc$'), mission, country), comb.cc),
            .groups = 'drop') %>% 
  as.data.frame() %>% # convert from sf, tbl_df, tbl, data.frame to sf, data.frame
  st_as_sf(sf_column_name = 'geom')

## create ADM2 spatial object
radpko_m_adm2_sf <- st_join(gadm_2, radpko_m_bases_sf, left = F) %>% 
  select(-base, -gid)

## create PRIO GRID spatial object
radpko_m_grid_sf <- st_join(prio, radpko_m_bases_sf %>% select(-gid), left = F) %>%
  st_drop_geometry() %>% 
  group_by(gid, date, year) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'),
                     -mission, -country, -base),
                   sum, na.rm = T),
            across(c(matches('^cc|_cc$'), mission, country), comb.cc),
            .groups = 'drop') %>% 
  inner_join(prio, ., by = 'gid')

## save monthly data
radpko_m_bases_sf %>% 
  select(id = base, any_of(radpko_cols), -gid) %>% 
  saveRDS('app/radpko_bases_m.rds')
radpko_m_adm0_sf %>% 
  select(id = NAME_0, any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm0_m.rds')
radpko_m_adm2_sf %>% 
  select(id = NAME_2, any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm2_m.rds')
radpko_m_grid_sf %>% 
  select(id = gid, any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_grid_m.rds')

## create yearly base data
radpko_m_bases_sf %>%
  group_by(id = base, year, mission, country) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date, -gid),
                   mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_bases_y.rds')

## create yearly adm0 data
radpko_m_adm0_sf %>%
  group_by(year, id = NAME_0) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date, -country,
                     -mission),
                   mean, na.rm = T),
            across(c(matches('^cc|_cc$'), country, mission), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm0_y.rds')

## create yearly adm2 data
radpko_m_adm2_sf %>%
  group_by(year, mission, country, id = NAME_2) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date),
                   mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm2_y.rds')

## create yearly grid data
radpko_m_grid_sf %>%
  group_by(year, mission, country, id = gid) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date, -gid),
                   mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_grid_y.rds')






