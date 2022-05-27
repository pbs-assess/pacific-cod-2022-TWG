# all.r
# This is the master file - it loads all packages and sources all
#  other R source code files.
#
# To debug in an R session, run these 3 commands first:
# source(here::here("R/all.r"));load.models.into.parent.env();source(here::here("R/custom-knitr-variables.r"))

library(lubridate)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gfplot)
library(purrr)
library(scales)
library(cowplot)
library(coda)
library(knitr)
library(PBSmodelling)
library(xtable)
library(tidyverse)
library(RColorBrewer)
library(kableExtra)
library(png)
library(leaflet)
library(svglite)
library(rosettafish)

rootd <- here::here()
rootd.R <- file.path(rootd, "R")
rootd.data <- file.path(rootd, "data")
rootd.index <- file.path(rootd, "index")
rootd.models <- file.path(rootd, "models")
rootd.pres <- file.path(rootd, "presentations")

source(file.path(rootd.R, "utilities.R"))
source(file.path(rootd.R, "verify.R"))
source(file.path(rootd.R, "model-setup.R"))
source(file.path(rootd.R, "load-models.R"))
source(file.path(rootd.R, "mcmc-diagnostics.R"))
source(file.path(rootd.R, "figures-biomass.R"))
source(file.path(rootd.R, "figures-recruitment.R"))
source(file.path(rootd.R, "figures-catch.R"))
source(file.path(rootd.R, "figures-indices.R"))
source(file.path(rootd.R, "figures-mean-weight.R"))
source(file.path(rootd.R, "figures-fishing-mortality.R"))
source(file.path(rootd.R, "figures-mcmc-diagnostics.R"))
source(file.path(rootd.R, "tables-catch.R"))
source(file.path(rootd.R, "tables-parameters.R"))
source(file.path(rootd.R, "tables-priors.R"))
source(file.path(rootd.R, "tables-decisions.R"))
source(file.path(rootd.R, "plot-survey-sets.R"))
source(file.path(rootd.R, "get-alk.R"))
source(file.path(rootd.R, "get-age-sample.R"))
source(file.path(rootd.data, "get-data.R"))

## # Code to setup forecast model runs
## # source("forecast-catch-levels.r")
## # Code to setup retro model runs.
## # source("retrospective-setup.r")
##
## # Set up variables for data tables from csv files
## # source("data-tables.r")
##
##
## # Load the raw data
dat.file <- file.path(rootd.data,
                      "pcod-cache",
                      "pacific-cod.rds")

if(!file.exists(dat.file)){
  gfdata::cache_pbs_data(species = "pacific cod",
                 path = file.path(rootd.data,
                                  "pcod-cache"),
                 survey_sets = TRUE,
                 unsorted_only = FALSE)
}
dat <- readRDS(dat.file)

tac.file <- file.path(rootd.data,
                      "pcod-tac-1996-2021.csv")
tac <- read.csv(tac.file, header = TRUE)

## ggplot globals for project
ggplot2::theme_set(gfplot::theme_pbs())
scale_colour_continuous <- scale_colour_viridis_c
scale_fill_continuous <- scale_fill_viridis_c

sensitivity_colors <- c("#000000", RColorBrewer::brewer.pal(8L, "Dark2"))
scale_colour_discrete <- function(...) scale_colour_manual(..., values = sensitivity_colors)
scale_fill_discrete <- function(...) scale_fill_manual(... , values = sensitivity_colors)

options(dplyr.summarise.inform = FALSE)
