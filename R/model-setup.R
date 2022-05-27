## -----------------------------------------------------------------------------
## Set verbosity for this project (R code)
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.025, 0.975)

## -----------------------------------------------------------------------------
## iscam files with names that don't change depending on model
rep.file <- "iscam.rep"
par.file <- "iscam.par"
mcmc.file <- "iscam_mcmc.csv"
mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
mcmc.recr.file <- "iscam_rt_mcmc.csv"
mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
mcmc.natural.mort.file <- "iscam_m_mcmc.csv"
mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
mpd.proj.file <- "iscammpd_proj_Gear1.csv"
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- as.numeric(substr(Sys.Date(), 1, 4))
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- 2018
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## rootd.models all.r
## -----------------------------------------------------------------------------
model.dir <- rootd.models
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
exe.file.name <- "iscam.exe"
if(verbose) cat0("iscam executable file: \n  ", exe.file.name)
starter.file.name <- "iscam.dat"
if(verbose) cat0("iscam starter file: \n  ", starter.file.name)

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Start year for the models
start.yr <- 1956
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}

## Start year for the fishery age comps
## start.yr.age.comps <- 1951
## if(verbose){
##   cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
## }

## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2020
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2020
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
base.model.3cd.name <- ifelse(french, "Sc 1a. Ref", "1a) 2020 Reference model 3CD")
base.model.3cd.dir.name <- file.path(model.dir,
                                     "1_1a_3CD_BASE_2020")

base.model.5abcd.name <- ifelse(french, "Sc 1a. Ref", "1a) 2020 Reference model 5ABCD")
base.model.5abcd.dir.name <- file.path(model.dir,
                                       "0_1a_5ABCD_BASE_2020")

if(verbose){
  cat0("Base model directory name for reference model 5abcd:\n", base.model.5abcd.dir.name)
  cat0("Base model pretty name for reference model 5abcd:\n", base.model.5abcd.name)
  cat0("Base model directory name for reference model 3cd:\n", base.model.3cd.dir.name)
  cat0("Base model pretty name for reference model 3cd:\n", base.model.3cd.name)
}

# ## -----------------------------------------------------------------------------
# ## Decision table models to average (5ABCD)
# ## -----------------------------------------------------------------------------
# desc.models.5abcd.dir.name <- c(base.model.5abcd.dir.name,
#                                 file.path(model.dir,
#                                           "0_2d_5ABCD_rsoleq_1_1"),
#                                 file.path(model.dir,
#                                           "0_2e_5ABCD_q_cv06"),
#                                 file.path(model.dir,
#                                           "0_3a_5ABCD_Mprior_mean04_sd01"),
#                                 file.path(model.dir,
#                                           "0_5a_5ABCD_kage_3"),
#                                 file.path(model.dir,
#                                           "0_6b_5ABCD_sig015"),
#                                 file.path(model.dir,
#                                           "0_7b_5ABCD_sigW_015"))
# desc.models.5abcd.name <- c(base.model.5abcd.name,
#                             ifelse(french, "Sc 2d.", "2d) HSSS ln(q) prior mean = ln(1.0 * 0.35), QCSSS = ln(1.0 * 0.65)"),
#                             ifelse(french, "Sc 2e.", "2e) HSSS and QCSS ln(q) prior SD = 0.6"),
#                             ifelse(french, "Sc 3a.", "3a) M prior mean = 0.4, SD = 0.1"),
#                             ifelse(french, "Sc 5a.", "5a) kage = 3y and update FW parameters"),
#                             ifelse(french, "Sc 6b.", "6b) Fix sigma O = 0.15"),
#                             ifelse(french, "Sc 7b.", "7b) Fix sigma W = 0.15"))
#

## MOST OF THESE SENSITIVITY MODELS ARE NOT USED -- DELETE FOR NOW
## -----------------------------------------------------------------------------
## Sensitivity models group 0 (5ABCD)
## -----------------------------------------------------------------------------
sens.models.dir.name.0 <- c(file.path(model.dir,
                                     "0_1c_5ABCD_BASE_2020_no_wt_since_2016"),
                            file.path(model.dir,
                                      "0_1b_5ABCD_BASE_2020_2016_wt"))

sens.models.name.0 <- c("No mean weight since 2016",
                        "Use 2016 weight from 2017 to 2020")

## -----------------------------------------------------------------------------
## Decision table models to average (3CD)
## -----------------------------------------------------------------------------
# desc.models.3cd.dir.name <- c(base.model.3cd.dir.name,
#                               file.path(model.dir,
#                                         "1_2d_3CD_q_1"),
#                               file.path(model.dir,
#                                         "1_2e_3CD_q_cv06"),
#                               file.path(model.dir,
#                                         "1_3a_3CD_Mprior_mean04_sd01"),
#                               file.path(model.dir,
#                                         "1_5a_3CD_kage3"),
#                               file.path(model.dir,
#                                         "1_6b_3CD_sig015"),
#                               file.path(model.dir,
#                                         "1_7b_3CD_sigW015"))
#
# desc.models.3cd.name <- c(base.model.3cd.name,
#                           ifelse(french, "Sc 2d.", "2d) WCVISS ln(q) prior mean = ln(1.0)"),
#                           ifelse(french, "Sc 2e.", "2e) WCVISS ln(q) prior SD = 0.6"),
#                           ifelse(french, "Sc 3a.", "3a) M prior mean = 0.4, SD = 0.1"),
#                           ifelse(french, "Sc 5a.", "5a) kage = 3y and update FW parameters"),
#                           ifelse(french, "Sc 6b.", "6b) Fix sigma O = 0.15"),
#                           ifelse(french, "Sc 7b.", "7b) Fix sigma W = 0.15"))

## -----------------------------------------------------------------------------
## Sensitivity models group 00 (3CD)
## -----------------------------------------------------------------------------
sens.models.dir.name.00 <- c(file.path(model.dir,
                                      "1_1c_3CD_BASE_2020_no_wt_since_2016"),
                            file.path(model.dir,
                                      "1_1b_3CD_BASE_2020_2016_wt"))

sens.models.name.00 <- c("No mean weight since 2016",
                         "Use 2016 weight from 2017 to 2020")


## -----------------------------------------------------------------------------
## Retrospectives
## -----------------------------------------------------------------------------
retro.dir.names.3cd <- c(file.path(base.model.3cd.dir.name,
                                   "Retrospective01"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective02"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective03"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective04"))
retro.dir.names.5abcd <- c(file.path(base.model.5abcd.dir.name,
                                   "Retrospective01"),
                           file.path(base.model.5abcd.dir.name,
                                     "Retrospective02"),
                           file.path(base.model.5abcd.dir.name,
                                     "Retrospective03"),
                           file.path(base.model.5abcd.dir.name,
                                     "Retrospective04"))
retro.dir.names <- c(retro.dir.names.3cd,
                     retro.dir.names.5abcd)
retro.names <- c("- 1 year",
                 "- 2 years",
                 "- 3 years",
                 "- 4 years")

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model.5abcd <<- load.models(base.model.5abcd.dir.name)
  #desc.models.5abcd <<- load.models(desc.models.5abcd.dir.name)
  #avg.model.5abcd <<- avg.models(desc.models.5abcd)
  sens.models.0 <<- load.models(sens.models.dir.name.0)
  # sens.models.1.sub <<- load.models(sens.models.dir.name.1.sub)
  # sens.models.1.sub2 <<- load.models(sens.models.dir.name.1.sub2)
  # sens.models.1 <<- load.models(sens.models.dir.name.1)
  # sens.models.2 <<- load.models(sens.models.dir.name.2)
  # sens.models.2.sub <<- load.models(sens.models.dir.name.2.sub)
  # sens.models.3 <<- load.models(sens.models.dir.name.3)
  # sens.models.4 <<- load.models(sens.models.dir.name.4)
  # sens.models.5 <<- load.models(sens.models.dir.name.5)
  # sens.models.6 <<- load.models(sens.models.dir.name.6)
  #sens.models.6.sub <<- load.models(sens.models.dir.name.6.sub)
  # sens.models.7 <<- load.models(sens.models.dir.name.7)
  # sens.models.108 <<- load.models(sens.models.dir.name.108)

  base.model.3cd <<- load.models(base.model.3cd.dir.name)
  #desc.models.3cd <<- load.models(desc.models.3cd.dir.name)
  #avg.model.3cd <<- avg.models(desc.models.3cd)
  sens.models.00 <<- load.models(sens.models.dir.name.00)
  # sens.models.8.sub <<- load.models(sens.models.dir.name.8.sub)
  # sens.models.8.sub2 <<- load.models(sens.models.dir.name.8.sub2)
  # sens.models.8 <<- load.models(sens.models.dir.name.8)
  # sens.models.9 <<- load.models(sens.models.dir.name.9)
  # sens.models.9.sub <<- load.models(sens.models.dir.name.9.sub)
  # sens.models.10 <<- load.models(sens.models.dir.name.10)
  # sens.models.11 <<- load.models(sens.models.dir.name.11)
  # sens.models.12 <<- load.models(sens.models.dir.name.12)
  # sens.models.13 <<- load.models(sens.models.dir.name.13)
  #sens.models.13.sub <<- load.models(sens.models.dir.name.13.sub)
  # sens.models.14 <<- load.models(sens.models.dir.name.14)
  # sens.models.15 <<- load.models(sens.models.dir.name.15)


  # base.retro.models.5abcd <<- load.models(retro.dir.names.5abcd)
  # base.retro.models.3cd <<- load.models(retro.dir.names.3cd)
}

build <- function(ovwrt.base = FALSE,
                  ovwrt.sens = FALSE,
                  ovwrt.retro = FALSE,
                  burnin = 1000,
                  thin = 1){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.
  ##
  ## ovwrt.base - overwrite the RData file for the base model?
  ## ovwrt.sens - overwrite the RData files for the sensitivity models?
  ## ovwrt.retro - overwrite the RData files for the retrospective models?

  ## Base models
  create.rdata.file(base.model.5abcd.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    burnin = burnin,
                    thin = thin,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)
  create.rdata.file(base.model.3cd.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    burnin = burnin,
                    thin = thin,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the for loop below to work right
  sens.models.names.list <- c(#unlist(desc.models.5abcd.dir.name),
                              #unlist(desc.models.3cd.dir.name)#,
                              unlist(sens.models.dir.name.0),
                              # unlist(sens.models.dir.name.1),
                              # unlist(sens.models.dir.name.2),
                              # unlist(sens.models.dir.name.3),
                              # unlist(sens.models.dir.name.4),
                              # unlist(sens.models.dir.name.5),
                              # unlist(sens.models.dir.name.6),
                              # unlist(sens.models.dir.name.7),
                              # unlist(sens.models.dir.name.108),
                              unlist(sens.models.dir.name.00)#,
                              # unlist(sens.models.dir.name.8),
                              # unlist(sens.models.dir.name.9),
                              # unlist(sens.models.dir.name.10),
                              # unlist(sens.models.dir.name.11),
                              # unlist(sens.models.dir.name.12),
                              # unlist(sens.models.dir.name.13),
                              # unlist(sens.models.dir.name.14),
                              # unlist(sens.models.dir.name.15)
                              )
  ## Sensitivity models
  for(model.nm in sens.models.names.list){
    create.rdata.file(model.nm,
                      ovwrt.rdata = ovwrt.sens,
                      load.proj = TRUE,
                      burnin = burnin,
                      thin = thin,
                      low = confidence.vals[1],
                      high = confidence.vals[2],
                      verbose = verbose)
  }
  #
  # ## Retrospective models
  # for(model.nm in retro.dir.names){
  #   create.rdata.file(model.nm,
  #                     ovwrt.rdata = ovwrt.retro,
  #                     load.proj = TRUE,
  #                     burnin = burnin,
  #                     thin = thin,
  #                     low = confidence.vals[1],
  #                     high = confidence.vals[2],
  #                     verbose = verbose)
  # }
}
