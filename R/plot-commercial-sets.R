#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot locations of commercial length samples
# Robyn Forrest. July 6 2022.
# For Pacific cod TWG
library(tidyverse)
library(gfdata)
library(gfplot)
library(here)

## # Load the raw data
dat.file <- here("data/pcod-cache/pacific-cod.rds")

if(!file.exists(dat.file)){
  gfdata::cache_pbs_data(species = "pacific cod",
                         path = here("data/pcod-cache"),
                         survey_sets = TRUE,
                         unsorted_only = FALSE,
                         return_all_lengths = TRUE)

}
dat <- readRDS(dat.file)

minyear <- 2007

# First, get a summary of commercial length sampling by year
# Just look at WCVI for now
d_samples1 <- dat$commercial_samples %>%
  dplyr::filter(major_stat_area_name %in%
                  c("3C: S.W. VANCOUVER ISLAND","3D: N.W. VANCOUVER ISLAND"),
                year>minyear)

summary_by_year <- d_samples1 %>%
  select(year,sample_id,gear_desc,length) %>%
  group_by(year,gear_desc) %>%
  summarize(nsamples=n_distinct(sample_id),
            nlengths=sum(!is.na(length)),
            meanlength=round(mean(length),2),
            sdlength=round(sd(length),2),
            selength=round(sdlength/sqrt(nlengths),2))

colnames(summary_by_year) <- c("Year", "Gear","Num samples", "Num lengths","Raw mean length", "SD length", "SE length")

summary_by_year

# OK so there are commercial samples for every year since 2010, except 2018

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now try to plot them on a map
# Get length samples with locations
# dat$commercial_samples does not include lats and longs,
#  so join with dat$catch
# try joining on fishing_event_id
areas <- c("5B: NORTHERN Q.C. SOUND",
           "5C: SOUTHERN HECATE STRAIT" ,
           "5D: NORTHERN HECATE STRAIT",
           "5A: SOUTHERN Q.C. SOUND",
           "3C: S.W. VANCOUVER ISLAND",
           "3D: N.W. VANCOUVER ISLAND")

d_samples2 <- dat$commercial_samples %>%
  dplyr::filter(major_stat_area_name %in% areas,
                year>minyear,
                gear_desc %in% c("GROUNDFISH TRAWL", "UNKNOWN TRAWL"))

# fishing_event_id and trip_id are character vectors in this table (why?)
d_catch <- dat$catch %>%
  dplyr::filter(year>=minyear,
                major_stat_area_name %in% areas,
                gear %in% c("GROUNDFISH TRAWL", "UNKNOWN TRAWL")) %>%
  mutate(fishing_event_id = as.double(fishing_event_id),
         trip_id = as.double(trip_id))

# left joining the above two tables doesn't work because no
#   common fishing_event_ids. All fields from d_samples2 are NA
d_samps_loc <- d_catch %>%
  left_join(d_samples2)%>%
  select(year, best_date,trip_id, fishing_event_id, vessel_name,
         lat, lon, sample_id,length) %>%
  filter(!is.na(lat),!is.na(lon))

# all sample ids and lengths are NA
View(d_samps_loc)

# there are no common fishing_event_ids or trip ids
fevents <- unique(d_samples2$fishing_event_id)
tripids <- unique(d_samples2$trip_id)

fe_test <- d_catch %>%
  filter(fishing_event_id %in% fevents) %>%
  nrow()

trip_test <- d_catch %>%
  filter(trip_id %in% tripids) %>%
  nrow()

