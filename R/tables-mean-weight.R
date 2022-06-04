# NOT WORKING

mw.table <- function(base,
                     sens,
                     years=1956:2020){

  mpd <- base$mpd
  yrs <- base$dat$meanwtdata[,1]
  obs <- mpd$obs_annual_mean_weight

  tab <- cbind(yrs, obs) %>% as.tibble() %>%
    filter(yrs %in% years)
  names(tab) <- c("Year", "2020 Reference Model")

  for(i in 1:length(sens)){

    colname<-paste0("Sc",i)
    model <- sens[i]
    yrs <- model$dat$meanwtdata[,1]
    obs <- model$mpd$obs_annual_mean_weight
    tmp <- cbind(yrs, obs) %>% as.tibble() %>%
      filter(yrs %in% years) %>%
      rename(colname=obs)

    tab <- cbind(tab,tmp$obs)
  }

  tab

 }

