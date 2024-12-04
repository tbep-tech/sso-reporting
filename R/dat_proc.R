
# data-raw/export.xlsx downloaded from https://prodenv.dep.state.fl.us/DepPNP/reports/viewIncidentDetails on Dec. 4, 2024  
# workflow adapted from https://github.com/tbep-tech/piney-point-analysis/blob/main/spills.Rmd

library(tidyverse)
library(here)
library(sf)

data(tbshed, package = 'tbeptools')
prj <- 4326

fl <- readxl::read_excel(here('data-raw/export.xls'))

# format as spatial object, clip by watershed
# format date objects, 
# select columns of interest, filter relevant counties
# some duplicate entires were removed
rprt <- fl |> 
  mutate(
    Longitude = as.numeric(Longitude), 
    Latitude = as.numeric(Latitude),
    Longitude = ifelse(sign(Longitude) == 1, -1 * Longitude, Longitude)
  ) |> 
  filter(!is.na(Longitude) & !is.na(Latitude)) |> 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) |> 
  st_filter(tbshed) |> 
  mutate(
    date = mdy_hm(`Report Date/Time`, tz = 'EST'),
    date = as.Date(date),
    yr = year(date), 
    mo = month(date)
  ) |> 
  select(
    date, 
    yr, 
    mo, 
    id = `SWO Incident Number`, 
    descrip = `Incident Report`, 
    facility = `Facility Name`, 
    county = `Affected Counties`
  ) |> 
  filter(county %in% c('Hillsborough', 'Pinellas', 'Manatee', 'Pasco', 'Polk')) |>
  distinct()

save(rprt, file = here("data/rprt.RData"))
