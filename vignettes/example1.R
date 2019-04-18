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
x <- as.numeric(rownames(out[[2]]))
plot(x, out[[2]][, "est"], ylim = range(out[[2]][, -2]), type = "l", 
     las = 1, xlab = "Surrogate Marker", ylab = "Optimal Transformation")
  lines(x, out[[2]][, "lower"], lty = 2)
  lines(x, out[[2]][, "upper"], lty = 2)

