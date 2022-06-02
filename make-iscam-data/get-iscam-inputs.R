# Run this file to create the inputs needed for the iscam model
# All generated outputs are put in the data/generated folder
# Author: Robyn Forrest (RF), March-May 2022.
# Code taken from elsewhere in the repo, originally written by Sean Anderson,
# Chris Grandin and RF

# This script can stand alone

# TODO: There's a bug in cache_pbs_data since new return_all_lengths argument.
# Pulling the data from GFBio won't currently work

library(tidyverse)
library(gfdata)
library(gfplot)
library(here)

rootd <- here::here()
rootd.R <- file.path(rootd, "R")
rootd.data <- file.path(rootd, "data")

# load necessary functions
source(here('R/get-data-functions.R'))
source(here('R/cpue-functions.R'))
french <- FALSE

# 1. Get the data and save into pcod-cache folder
# Note that return_all_lengths must be set to TRUE because some lengths since
# 2016 were recorded as TOTAL_LENGTH instead of FORK_LENGTH.
# Maria confirmed this would have been a recording error rather than measurement error (May 2022)
dat.file <- here("data/pcod-cache/pacific-cod.rds")

if(!file.exists(dat.file)){
  gfdata::cache_pbs_data(species = "pacific cod",
                         path = file.path(rootd.data,
                                          "pcod-cache"),
                         survey_sets = TRUE,
                         unsorted_only = FALSE,
                         return_all_lengths = TRUE)
}
dat <- readRDS(dat.file) #query done June 2,2022

#test
unique(dat$commercial_samples$length_type)

# Need to update the commercial samples to use the return_all_lengths argument,
# which is currently only implemented in get_commercial_samples()
# Need it so that it doesn't just use fork length

# April 22, 2022. Philina updated gfdata to include length type
file.name <- here::here("data/generated/comm-samples-with-length-type.rds")
 if(!file.exists(file.name)){
   comsamp_newgfdata <- gfdata::get_commercial_samples("pacific cod",
                                                        unsorted_only = FALSE,
                                                        return_all_lengths = TRUE)
   saveRDS(comsamp_newgfdata,file.name)
 }else{
   comsamp_newgfdata <- readRDS(file.name)
 }
#
# # Replace commercial_samples
 dat$commercial_samples <- comsamp_newgfdata


#=================================================================================
# 2.Survey indices
surv_index <- dat$survey_index %>%
  dplyr::filter(survey_abbrev %in%
                  c("SYN QCS", "SYN WCVI", "SYN HS", "SYN WCHG", "OTHER HS MSA")) %>%
  dplyr::select(survey_abbrev, year, biomass, re, lowerci, upperci, num_sets, num_pos_sets) %>%
  dplyr::mutate(lowerci = round(lowerci/1000, 1), upperci = round(upperci/1000, 1),
                biomass = round(biomass/1000, 1), re = round(re, 2)) %>%
  dplyr::rename(`Survey abbrev.` = survey_abbrev, Year = year, Biomass = biomass,
                CV = re, `Lower CI` = lowerci, `Upper CI` = upperci, Sets = num_sets, `Positive sets` = num_pos_sets) %>%
  dplyr::arrange(`Survey abbrev.`, Year)

write_csv(surv_index, here::here("data/generated/all_surveys.csv"))

#=================================================================================
# 2. CPUE indices: these take a long time
params <- list()
params$species_proper <- "Pacific Cod"
params$april1_year <- TRUE
params$area <- c("5[ABCD]+", "3[CD]+")
params$area_name <- c("5ABCD", "3CD")
params$skip_single_variable_models <- FALSE

params$era <- "historic"
source(here::here("R/cpue.R"))
dfleet_hist <- dfleet
gg_cpue_hist <- gg_cpue
cpue_pred_hist <- predictions
arith_cpue_hist <- arith_cpue
m_historic <- readRDS(here::here("data/generated/cpue-models-pcod-historic.rds"))

params$era <- "modern"
source(here::here("R/cpue.R"))
dfleet_modern <- dfleet
gg_cpue_modern <- gg_cpue
cpue_pred_modern <- predictions
arith_cpue_modern <- arith_cpue
m_modern <- readRDS(here::here("data/generated/cpue-models-pcod-modern.rds"))

readr::write_csv(cpue_pred_modern, here::here("data/generated/cpue-predictions-modern.csv"))
readr::write_csv(cpue_pred_hist, here::here("data/generated/cpue-predictions-historical.csv"))

#=================================================================================
# 3. Catch data
## Example of how to view by year for 3CD:
## c3cd <- catch.3 %>%
##   group_by(year) %>%
##     summarize(canada = sum(canada_catch),
##     usa = sum(usa_catch),
##     total_catch = sum(total_catch))
catch.3 <- total.catch.yr.qtr(dat$catch,
                              areas = "3[CD]+",
                              include.usa = TRUE)

catch.5 <- total.catch.yr.qtr(dat$catch,
                              areas = "5[ABCD]+",
                              include.usa = TRUE)
c3cd <- catch.3 %>%
        group_by(year) %>%
        summarize(canada = sum(canada_catch),
                  usa = sum(usa_catch),
                  total_catch = sum(total_catch)) %>%
        mutate(area="3CD") %>%
        filter(year>1955)

c5abcd <- catch.5 %>%
  group_by(year) %>%
  summarize(canada = sum(canada_catch),
            usa = sum(usa_catch),
            total_catch = sum(total_catch)) %>%
  mutate(area="5ABCD") %>%
  filter(year>1955)

allcatch <- rbind(c3cd,c5abcd)

readr::write_csv(allcatch, here("data/generated/all-commercial-catch.csv"))

#=================================================================================
# 4. Commercial mean weights (copied from R/get-mean-weight.R)
d <- dat$commercial_samples
include.usa = TRUE
#prevMeanWeight <- read.csv(file.path(rootd.data, "MeanWeights_previous.csv"))

## 3CD
w3cd <- get.mean.weight(d,
                         dat$catch,
                         areas = "3[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA3,
                         b = .BETA3) %>%
        mutate(area="3CD")

## 5ABCD
w5abcd <- get.mean.weight(d,
                           dat$catch,
                           areas = c("5[AB]+", "5[CD]+"),
                           include.usa = include.usa,
                           a = .ALPHA5,
                           b = .BETA5)%>%
          mutate(area="5ABCD")

allmeanweight <- rbind(w3cd,w5abcd)

readr::write_csv(allmeanweight, here("data/generated/all-commercial-mean-weight.csv"))



