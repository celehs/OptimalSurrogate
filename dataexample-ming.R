load("data/dataexample.RData")
source("code-ming/pte.R")

set.seed(123)
system.time(out <- pte(
  sob = data$sob, yob = data$yob, aob = data$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out
