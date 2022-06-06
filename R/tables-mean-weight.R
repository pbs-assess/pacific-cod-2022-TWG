# get a list of the mean weights and add NAs for missing years
# Arguments like this:
# models <- c(base.model.3cd, sens.models.11)
# model.names <- c(base.model.3cd.name, sens.models.name.11)

# Robyn Forrest, June 6 2022

mw.table <- function(models,
                     model.names,
                     years=2010:2020){

    years <- years
    # get all the mean weights in a list format
    mean.wts.list <- lapply(models,
           function(x){
              mpd <- x$mpd
              yrs <- x$dat$meanwtdata[,1]
              obs <- mpd$obs_annual_mean_weight
              tab <- cbind(yrs, obs) %>% as_tibble() %>%
                filter(yrs %in% years)
              # now add missing years
              missing <- years[!years %in% tab$yrs]
              if(length(missing)>0){
                tmp <- cbind(missing,NA) %>%
                  as.data.frame() %>%
                  rename(yrs = names(.)[1], obs = names(.)[2]) %>%
                  rbind(tab) %>%
                  arrange(yrs)
                tab <- tmp}
              })

     # bind together in a table and add row names
     mean.wts <- mean.wts.list[[1]]
     colnames(mean.wts) <- c("Year",model.names[1])
     if(length(models)>1){
       for(i in 2:length(models)){
          mean.wt <- mean.wts.list[[i]]$obs
          mean.wts <- cbind(mean.wts,mean.wt)
          colnames(mean.wts) <- c(colnames(mean.wts[1:i]),model.names[i])
        }# end if
      }#end for
     as_tibble(mean.wts)
 }# end function

