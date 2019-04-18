## ------------------------------------------------------------------------
library(OptimalSurrogate)

## ------------------------------------------------------------------------
data(marker_cont)
head(marker_cont)
tail(marker_cont)

## ------------------------------------------------------------------------
set.seed(123)
system.time(out <- pte_cont(
  sob = marker_cont$sob, 
  yob = marker_cont$yob, 
  aob = marker_cont$aob, 
  var = TRUE, conf.int = TRUE, rep = 1000))
out[[1]]

## ------------------------------------------------------------------------
plot(as.numeric(rownames(out[[2]])), out[[2]][, "est"], type = "l", 
     las = 1, xlab = "Surrogate Marker", ylab = "Optimal Transformation")

