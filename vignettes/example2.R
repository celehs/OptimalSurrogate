## ------------------------------------------------------------------------
library(OptimalSurrogate)

## ------------------------------------------------------------------------
data <- dataexample_d
head(data)
tail(data)

## ------------------------------------------------------------------------
set.seed(123)
system.time(out <- pte_d(
  sob = data$sob, yob = data$yob, aob = data$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out

