# This script is called by all.R

#Length-weight parameters
#coastwide
.ALPHA <- 6.79e-06
.BETA <- 3.11

#3CD
.ALPHA3FEM <- 7.43e-06
.ALPHA3 <- 7.65616e-06
.BETA3FEM <- 3.09
.BETA3 <- 3.08

#5ABCD
.ALPHA5FEM <- 6.52e-06
.ALPHA5 <- 6.722839e-06
.BETA5FEM <- 3.12
.BETA5 <- 3.11

#old (2013 assessment)
.ALPHA2013 <- 7.377e-06
.BETA2013 <- 3.0963

prevMeanWeight <- read.csv(file.path(rootd.data, "MeanWeights_previous.csv"))

rootd <- here::here()
rootd.data <- file.path(rootd, "data")
resultsd <- file.path(rootd.data, "results")
dir.create(resultsd, showWarnings = FALSE)

if(FALSE){
  gfplot::cache_pbs_data(species = "pacific pod",
                 path = file.path(rootd.data, "pcod-cache"),
                 unsorted_only = FALSE)
}

dat <- load.data(cache.dir = file.path(rootd.data, "pcod-cache")) #Original line
d <- dat$commercial_samples

################################################################
## Get mean weight
include.usa <- TRUE
## 5AB
df5AB <- get.mean.weight(d,
                         dat$catch,
                         areas = "5[AB]+",
                         include.usa = include.usa,
                         a = .ALPHA5,
                         b = .BETA5)

write_csv(df5AB,file.path(resultsd,"AnnualMeanWeight_5AB.csv"))

## 5AB - OLD LW pars
# df5ABoldpars <- get.mean.weight(d,
#                                 dat$catch,
#                                 areas = "5[AB]+",
#                                 include.usa = include.usa,
#                                 a = .ALPHA2013,
#                                 b = .BETA2013)

## 5CD
df5CD <- get.mean.weight(d,
                         dat$catch,
                         areas = "5[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA5,
                         b = .BETA5)
write_csv(df5CD,file.path(resultsd,"AnnualMeanWeight_5CD.csv"))

## 5CD - OLD LW pars
# df5CDoldpars <- get.mean.weight(d,
#                                 dat$catch,
#                                 areas = "5[CD]+",
#                                 include.usa = include.usa,
#                                 a = .ALPHA2013,
#                                 b = .BETA2013)

## 3CD
df3CD <- get.mean.weight(d,
                         dat$catch,
                         areas = "3[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA3,
                         b = .BETA3)
write_csv(df3CD,file.path(resultsd,"AnnualMeanWeight_3CD.csv"))

## 5ABCD
df5ABCD <- get.mean.weight(d,
                           dat$catch,
                           areas = c("5[AB]+", "5[CD]+"),
                           include.usa = include.usa,
                           a = .ALPHA5,
                           b = .BETA5)
write_csv(df5ABCD,file.path(resultsd,"AnnualMeanWeight_5ABCD.csv"))

#################################################################
## Plot results
## 5AB
df <- df5AB
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5AB")
ggsave(file.path(resultsd,"AnnualMeanWeight_5AB.png"), width=8, height=6, units="in")

## 5CD
df <- df5CD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5CD")
ggsave(file.path(resultsd,"AnnualMeanWeight_5CD.png"), width=8, height=6, units="in")

## 3CD
df <- df3CD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 3CD")
ggsave(file.path(resultsd,"AnnualMeanWeight_3CD.png"), width=8, height=6, units="in")

## 5ABCD
df <- df5ABCD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5ABCD")
ggsave(file.path(resultsd,"AnnualMeanWeight_5ABCD.png"), width=8, height=6, units="in")

#########################################################################################
## Compare to previous analyses
## 5AB
# Analysis <- "2018 new LW pars"
# df <- df5AB
# df <- cbind(df,rep(Analysis,nrow(df)))
# colnames(df) <- c("Year","MeanWeight","Analysis")
# Analysis <- "2018 old LW pars"
# df2 <- df5ABoldpars
# df2 <- cbind(df2,rep(Analysis,nrow(df2)))
# colnames(df2) <- c("Year","MeanWeight","Analysis")
#
# dfcompare <- subset(prevMeanWeight, Area=="5AB", select=-Area)
# dfcompare <- rbind(dfcompare,df,df2)
# ggplot(data=dfcompare, aes(x=Year,y=MeanWeight, group=Analysis, colour=Analysis)) +
#   geom_line(lwd=1) +
#   ylim(0,1.1*max(dfcompare$MeanWeight)) +
#   theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold")) +
#   scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
#   labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5AB")
# ggsave(file.path(resultsd,"AnnualMeanWeightCompare_5AB.png"), width=8, height=6, units="in")
#
# ## 5CD
# Analysis <- "2018 new LW pars"
# df <- df5CD
# df <- cbind(df,rep(Analysis,nrow(df)))
# colnames(df) <- c("Year","MeanWeight","Analysis")
# Analysis <- "2018 old LW pars"
# df2 <- df5CDoldpars
# df2 <- cbind(df2,rep(Analysis,nrow(df2)))
# colnames(df2) <- c("Year","MeanWeight","Analysis")
#
# dfcompare <- subset(prevMeanWeight, Area=="5CD", select=-Area)
# dfcompare <- rbind(dfcompare,df,df2)
# ggplot(data=dfcompare, aes(x=Year,y=MeanWeight, group=Analysis, colour=Analysis)) +
#   geom_line(lwd=1) +
#   ylim(0,1.1*max(dfcompare$MeanWeight)) +
#   theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold")) +
#   scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
#   labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5CD")
# ggsave(file.path(resultsd,"AnnualMeanWeightCompare_5CD.png"), width=8, height=6, units="in")
#
# #########################################################################################
# ## Compare Female-only to both sexes growth pars
# ## 5ABCD
# df5abcd.both <- get.mean.weight(d,
#                            dat$catch,
#                            areas = "5[ABCD]+",
#                            include.usa = include.usa,
#                            a = .ALPHA5,
#                            b = .BETA5)
# write_csv(df5abcd.both, file.path(resultsd,"AnnualMeanWeightGrowthBothSexes_5ABCD.csv"))
#
# df5abcd.fem <- get.mean.weight(d,
#                           dat$catch,
#                            areas = "5[ABCD]+",
#                           include.usa = include.usa,
#                           a = .ALPHA5FEM,
#                           b = .BETA5FEM)
# Analysis <- "Both sexes Growth params"
# df <- df5abcd.both
# df <- cbind(df, rep(Analysis, nrow(df)))
# colnames(df) <- c("Year", "MeanWeight", "Analysis")
# Analysis <- "Female only Growth params"
# df2 <- df5abcd.fem
# df2 <- cbind(df2, rep(Analysis, nrow(df2)))
# colnames(df2) <- c("Year", "MeanWeight", "Analysis")
#
# dfcompare <- rbind(df,df2)
# ggplot(data=dfcompare,
#        aes(x=Year,y=MeanWeight,
#            group=Analysis,
#            colour=Analysis)) +
#   geom_line(lwd=1) +
#   ylim(0,1.1*max(dfcompare$MeanWeight)) +
#   theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold")) +
#   scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
#   labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5ABCD")
# ggsave(file.path(resultsd,"AnnualMeanWeightCompareGrowthSexes_5ABCD.png"), width=8, height=6, units="in")
#
# ## 3CD
# df3cd.both <- get.mean.weight(d,
#                            dat$catch,
#                            areas = "3[CD]+",
#                            include.usa = include.usa,
#                            a = .ALPHA5,
#                            b = .BETA5)
# write_csv(df3cd.both, file.path(resultsd,"AnnualMeanWeightGrowthBothSexes_3CD.csv"))
#
# df3cd.fem <- get.mean.weight(d,
#                           dat$catch,
#                            areas = "3[CD]+",
#                           include.usa = include.usa,
#                           a = .ALPHA5FEM,
#                           b = .BETA5FEM)
# Analysis <- "Both sexes Growth params"
# df <- df3cd.both
# df <- cbind(df, rep(Analysis, nrow(df)))
# colnames(df) <- c("Year", "MeanWeight", "Analysis")
# Analysis <- "Female only Growth params"
# df2 <- df3cd.fem
# df2 <- cbind(df2, rep(Analysis, nrow(df2)))
# colnames(df2) <- c("Year", "MeanWeight", "Analysis")
#
# dfcompare <- rbind(df,df2)
# ggplot(data=dfcompare,
#        aes(x=Year,y=MeanWeight,
#            group=Analysis,
#            colour=Analysis)) +
#   geom_line(lwd=1) +
#   ylim(0,1.1*max(dfcompare$MeanWeight)) +
#   theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold")) +
#   scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
#   labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 3CD")
# ggsave(file.path(resultsd,"AnnualMeanWeightCompareGrowthSexes_3CD.png"), width=8, height=6, units="in")
