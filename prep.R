library(tidyverse)
library(lubridate)
library(sf)

## function to combine contributing countries in summarize()
comb.cc <- function(cc) {
  
  str_c(sort(unique(unlist(str_split(cc, '\\|')))), collapse = '|')
  
}

## read in bas data
radpko_m <- data.table::fread('radpko_bases_cc.csv') %>% 
  as_tibble() %>% 
  mutate(year = year(date)) %>% 
  select(mission:date, year, everything())

radpko_cols <- names(radpko_m)
radpko_cols[radpko_cols == 'base'] <- 'id'

cshapes <- st_read('~/Dropbox/Datasets/cshapes/cshapes_0.6/cshapes.shp')

ssa_cshapes <- c('Mauritania', 'Mali', 'Niger', 'Chad', 'Sudan', 'Eritrea', 'Senegal',
                 'The Gambia', 'Guinea-Bissau', 'Guinea', 'Sierra Leone', 'Liberia',
                 "Cote d'Ivoire", 'Burkina Faso', 'Ghana', 'Togo', 'Benin', 'Nigeria',
                 'Cameroon', 'Central African Republic', 'South Sudan', 'Ethiopia',
                 'Somalia', 'Equatorial Guinea', 'Gabon', 'Congo',
                 'Congo, DRC', 'Uganda', 'Kenya', 'Angola',
                 'Zambia', 'Rwanda', 'Burundi', 'Tanzania')



ssa_cshapes[!ssa_cshapes %in% cshapes$CNTRY_NAME]

cshapes %>%
  filter(CNTRY_NAME %in% ssa_cshapes,
         GWEYEAR >= min(radpko_m$year),
         !(CNTRY_NAME == 'Sudan' & GWSYEAR <= min(radpko_m$year))) %>% 
  saveRDS('app/ssa.Rds')


ssa_filter <- cshapes %>%
  filter(CNTRY_NAME %in% ssa_cshapes,
         GWEYEAR >= min(radpko_m$year),
         !(CNTRY_NAME == 'Sudan' & GWSYEAR <= min(radpko_m$year))) %>% 
  st_geometry() %>% 
  st_union() %>% 
  st_as_text()


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
                query = 'SELECT gid, geometry FROM priogrid_cell',
                wkt_filter = gadm_0 %>% st_geometry() %>% st_union() %>% st_as_text())

## create sf objects
radpko_m_bases_sf <- radpko_m %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>% 
  st_set_crs(st_crs(4326))

## create ADM0 spatial object
radpko_m_adm0_sf <- st_join(gadm_0, radpko_m_bases_sf, left = F) %>% 
  select(-base, -gid)

## create ADM2 spatial object
radpko_m_adm2_sf <- st_join(gadm_2, radpko_m_bases_sf, left = F) %>% 
  select(-base, -gid)

## create PRIO GRID spatial object
radpko_m_grid_sf <- st_join(prio, radpko_m_bases_sf %>% select(-gid), left = F) %>% 
  select(-base)

test <- radpko_m_grid_sf %>% 
  group_by(date, mission, country, gid) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$')), mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            .groups = 'drop')

## save monthly data
radpko_m_bases_sf %>% 
  select(id = base, any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_bases_m.rds')
radpko_m_adm2_sf %>% 
  select(id = NAME_2, any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm2_m.rds')

## create yearly base data
radpko_m_bases_sf %>%
  group_by(year, mission, country, id = base) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date), mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_bases_y.rds')

## create yearly adm2 data
radpko_m_adm2_sf %>%
  group_by(year, mission, country, id = NAME_2) %>% 
  summarize(across(c(everything(), -matches('^cc|_cc$'), -date), mean, na.rm = T),
            across(matches('^cc|_cc$'), comb.cc),
            date = min(date),
            .groups = 'drop') %>% 
  select(any_of(radpko_cols)) %>% 
  saveRDS('app/radpko_adm2_y.rds')

## create yearly grid data
radpko_m_grid_sf

radpko_m_grid_sf[duplicated(str_c(radpko_m_grid_sf$gid, radpko_m_grid_sf$date)), ] %>% 
  st_drop_geometry() %>% View()




radpko_y_sf <- radpko_y %>% 
  select(mission:longitude) %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>% 
  st_set_crs(st_crs(4326))






