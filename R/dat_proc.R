# View raw data from https://prodenv.dep.state.fl.us/DepPNP/reports/viewIncidentDetails
# workflow adapted from https://github.com/tbep-tech/piney-point-analysis/blob/main/spills.Rmd
# web services https://ca.dep.state.fl.us/arcgis/rest/services/External_Services/PNP/FeatureServer/1

library(tidyverse)
library(here)
library(httr)
library(sf)

source(here('R/funcs.R'))

prj <- 4326

# download raw data from web services ---------------------------------------------------------

# retrieved 2025-04-02

base_url <- "https://ca.dep.state.fl.us/arcgis/rest/services/External_Services/PNP/FeatureServer/1/query"

get_dat <- function(offset = 0, max_rec) {
  query_params <- list(
    where = "1=1",
    outFields = "*",
    resultOffset = offset,
    resultRecordCount = max_rec,
    f = "geojson"
  )

  req <- GET(base_url, query = query_params)
  dat <- st_read(rawToChar(req$content), quiet = TRUE)
  
  out <- list(
    dat = dat,
    n_rec = nrow(dat)
  )
  
  return(out)
  
}

# Retrieve data dynamically
all_dat <- NULL
offset <- 0
max_rec <- 10000

repeat {
  
  cat('offset', offset, '\n')
  
  result <- get_dat(offset, max_rec)
  all_dat <- rbind(all_dat, result$dat)
  
  if (result$n_rec < max_rec){
    cat('done!\n')
    break
  }
  
  offset <- offset + max_rec
  
}

rawdat <- all_dat
save(rawdat, file = here('data-raw/rawdat.RData'))

# format raw data -----------------------------------------------------------------------------

load(file = here('data-raw/rawdat.RData'))
     
# format as spatial object, clip by watershed
# format date objects, 
# select columns of interest, filter relevant counties
# some duplicate entires were removed
rprt <- rawdat |> 
  select(
    date = DATE_TIME_OF_REPORT,
    id = INCIDENT_NUMBER,
    nm = INCIDENT_NAME,
    descrip = INCIDENT_REPORT,
    facility = FACILITY_NAME,
    county = AFFECTED_COUNTIES, 
    Longitude = LONGITUDE,
    Latitude = LATITUDE
  ) |> 
  mutate(
    Longitude = as.numeric(Longitude), 
    Latitude = as.numeric(Latitude),
    Longitude = ifelse(sign(Longitude) == 1, -1 * Longitude, Longitude)
  ) |> 
  filter(!is.na(Longitude) & !is.na(Latitude)) |> 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) |> 
  st_filter(tbeptools::tbshed) |> 
  mutate(
    date = as.POSIXct(date/1000, origin = "1970-01-01", tz = "UTC"),
    date = as.Date(date),
    yr = year(date), 
    mo = month(date)
  ) |> 
  filter(county %in% c('Hillsborough', 'Pinellas', 'Manatee', 'Pasco', 'Polk')) |>
  filter(yr < 2025) |> 
  distinct()

save(rprt, file = here("data/rprt.RData"))

# extract volumes -----------------------------------------------------------------------------

load(file = here('data/rprt.RData'))

# extract volumes from descrip field
vols <- rprt |>
  filter(grepl('[0-9]', descrip)) |>
  mutate(
    descrip = gsub('\\,', '', descrip),
    descrip = tolower(descrip),
    hasgal = grepl('gallon', descrip),
    hasvol = grepl('spill\\svolume:\\s', descrip),
    hasmlg = grepl('^.*\\d+\\smg\\.|^.*\\d+\\smg\\s', descrip),
    galcnt = str_count(descrip, 'gallon')
  ) |>
  filter(hasvol | hasgal | hasmlg) |>
  mutate(
    idx = row_number()
  ) |>
  mutate(
    volest = extract_fun(descrip, hasgal, hasvol, hasmlg),
    .by = idx
  ) |>
  filter(!is.na(volest)) |>
  mutate(
    modt = floor_date(date, unit = 'months')
  ) |> 
  st_intersection(tbeptools::tbsegshed) |> 
  select(-hasgal, -hasvol, -hasmlg, -galcnt, -idx, -long_name) |> 
  filter(yr < 2025)

save(vols, file = here('data/vols.RData'))
