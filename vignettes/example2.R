## ------------------------------------------------------------------------
library(OptimalSurrogate)

## ------------------------------------------------------------------------
data(marker_disc)
head(marker_disc)
tail(marker_disc)

## ------------------------------------------------------------------------
set.seed(123)
system.time(out <- pte_disc(
  sob = marker_disc$sob, 
  yob = marker_disc$yob, 
  aob = marker_disc$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out

