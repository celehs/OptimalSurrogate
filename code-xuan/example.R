rm(list=ls())
timestart<-Sys.time()

load("~/Desktop/optimalsurrogate/dataexample.RData")
source('~/Desktop/optimalsurrogate/optmainfun.R')
out=pte.estimate(sob=data$sob , yob=data$yob , aob=data$aob , var = T , conf.int = T , rep=100 )

#### discrete
load("~/Desktop/optimalsurrogate/dataexample_d.RData")
source('~/Desktop/optimalsurrogate/optmainfun_d.R')
out=pte.estimate(sob=data$sob , yob=data$yob , aob=data$aob , var = T , conf.int = T , rep=100 )


timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime) 



