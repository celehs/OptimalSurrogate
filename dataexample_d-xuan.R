load("data/dataexample_d.RData")
source("code-xuan/optmainfun_d.R")

set.seed(123)
system.time(out <- pte.estimate(
  sob = data$sob, yob = data$yob, aob = data$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out

