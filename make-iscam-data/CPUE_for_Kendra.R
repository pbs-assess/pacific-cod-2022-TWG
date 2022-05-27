# Run this file to create the inputs needed for the iscam model
# All generated outputs are put in the data/generated folder
# Author: Robyn Forrest (RF), March 2022.
# Code taken from elsewhere in the repo, originally written by Sean Anderson,
#     Chris Grandin and RF

library(tidyverse)
library(gfdata)
library(gfplot)
library(here)
# load necessary functions
source(here('cpue-functions.R'))
french <- FALSE

# 1. Get the data (dat) and save into rds file
dat.file <- here("pacific-cod.rds")

if(!file.exists(dat.file)){
   gfdata::cache_pbs_data(species = "pacific cod",
                  path = here(),
                  survey_sets = TRUE,
                  unsorted_only = FALSE)
 }
dat <- readRDS(dat.file)

#=================================================================================
# 2. CPUE indices - UPDATE SPECIES AND AREA NAMES IN THIS FILE AND CPUE.R
params <- list()
params$species_proper <- "Pacific Cod"
params$april1_year <- TRUE
params$area <- c("5[ABCD]+", "3[CD]+")
params$area_name <- c("5ABCD", "3CD")
params$skip_single_variable_models <- FALSE

params$era <- "historic"
source(here::here("cpue.R"))
dfleet_hist <- dfleet
gg_cpue_hist <- gg_cpue
cpue_pred_hist <- predictions
arith_cpue_hist <- arith_cpue
m_historic <- readRDS(here("cpue-models-historic.rds"))

params$era <- "modern"
source(here::here("R/cpue.R"))
dfleet_modern <- dfleet
gg_cpue_modern <- gg_cpue
cpue_pred_modern <- predictions
arith_cpue_modern <- arith_cpue
m_modern <- readRDS(here::here("cpue-models-modern.rds"))

readr::write_csv(cpue_pred_modern, here::here("cpue-predictions-modern.csv"))
readr::write_csv(cpue_pred_hist, here::here("cpue-predictions-historical.csv"))

#=================================================================================
# 3.Survey indices
surv_index <- dat$survey_index %>%
  dplyr::filter(survey_abbrev %in%
                  c("SYN QCS", "SYN WCVI", "SYN HS", "SYN WCHG", "OTHER HS MSA")) %>%
  dplyr::select(survey_abbrev, year, biomass, re, lowerci, upperci, num_sets, num_pos_sets) %>%
  dplyr::mutate(lowerci = round(lowerci/1000, 1), upperci = round(upperci/1000, 1),
                biomass = round(biomass/1000, 1), re = round(re, 2)) %>%
  dplyr::rename(`Survey abbrev.` = survey_abbrev, Year = year, Biomass = biomass,
                CV = re, `Lower CI` = lowerci, `Upper CI` = upperci, Sets = num_sets, `Positive sets` = num_pos_sets) %>%
  dplyr::arrange(`Survey abbrev.`, Year)

write_csv(surv_index, here("all_surveys.csv"))

