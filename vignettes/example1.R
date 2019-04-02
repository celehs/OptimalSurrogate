## ------------------------------------------------------------------------
library(OptimalSurrogate)

## ------------------------------------------------------------------------
data <- dataexample
head(data)
tail(data)

## ------------------------------------------------------------------------
set.seed(123)
system.time(out <- pte(
  sob = data$sob, yob = data$yob, aob = data$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out

