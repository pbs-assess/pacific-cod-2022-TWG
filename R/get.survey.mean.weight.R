# Code to get the annual mean weight in the survey. Try to mimic the commercial mean
# weight code as much as possible

# This is for the technical working group meeting to look at whether we can
# calibrate the survey mean weight to the commercial mean weight, to possibly
# use survey mean weight going forward despite different selectivities

# June 2 2022. Robyn Forrest (RF)

# TODO: Check that the catch weighting makes sense
# Well, none of it really makes sense because length sampling has been
# length-stratified for most years of the survey!!

french <- FALSE
source(here::here("R/all.R"))

# L-W parameters
#3CD
.ALPHA3 <- 7.65616e-06
.BETA3 <- 3.08

#5ABCD
.ALPHA5 <- 6.722839e-06
.BETA5 <- 3.11

dat <-  readRDS(here("data/pcod-cache/pacific-cod.rds"))

catch_weight_summary <- dat$survey_sets %>%
  select(c(year, fishing_event_id, sample_id, catch_weight)) %>%
  filter(!is.na(sample_id))

# get survey lengths (unweighted)
lengthwt_raw <- dat$survey_samples %>%
  filter(survey_abbrev %in% c("SYN WCVI"),
         usability_code %in% c(0, 1, 2, 6),
         !is.na(length)) %>%
  select(year,fishing_event_id, sample_id,length,weight) %>%
  mutate(weight_calc=.ALPHA3*length^.BETA3, weight=weight/1000) %>%
  left_join(catch_weight_summary)

# Get the annual mean weight (unweighted by catch weight)
Annual_mean_wt_raw <- lengthwt_raw %>%
  group_by(year) %>%
  summarize(mean_weight_calc=mean(weight_calc),
            mean_weight_obs = mean(weight, na.rm=TRUE))

# Now weight by catch
# get the mean weight in the samples, with the catch weight from the fishing event
Mean_wt_samples <- lengthwt_raw %>%
  group_by(year, sample_id) %>%
  summarize(mean_weight_calc=mean(weight_calc),
            mean_weight_obs = mean(weight, na.rm=TRUE),
            catch_weight=catch_weight)

Annual_mean_wt_weighted_calc <- Mean_wt_samples %>%
  group_by(year) %>%
  summarize(weighted_mean_weight_calc=sum(mean_weight_calc*catch_weight)/sum(catch_weight))

Annual_mean_wt_weighted <- Mean_wt_samples %>%
  group_by(year) %>%
  filter(!is.na(mean_weight_obs)) %>%
  summarize(weighted_mean_weight_obs=sum(mean_weight_obs*catch_weight)/sum(catch_weight)) %>%
  left_join(Annual_mean_wt_weighted_calc)


# plot measured weight against calculated weight
# raw
g <- lengthwt_raw %>%
  filter(!is.na(weight)) %>%
  ggplot() +
  geom_point(aes(x=weight, y=weight_calc), colour="darkblue") +
  gfplot::theme_pbs() +
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  labs(title = "WCVI", y = "Calculated weight from length", x = "Measured weight")
print(g)

# Plot annual mean weights
# raw
g <- Annual_mean_wt_raw %>%
  melt(id.vars="year", variable.name="measurement_type", value.name="mean_weight") %>%
  ggplot()+
  geom_line(aes(x=year, y=mean_weight, colour=measurement_type, linetype=measurement_type), size=1.5)+
  ylim(0,2.1)
g

# catch weighted
g <- Annual_mean_wt_weighted %>%
  melt(id.vars="year", variable.name="measurement_type", value.name="mean_weight") %>%
  ggplot()+
  geom_line(aes(x=year, y=mean_weight, colour=measurement_type, linetype=measurement_type), size=1.5)+
  ylim(0,2.1)
g
