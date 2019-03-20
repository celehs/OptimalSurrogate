# sob: DISCRETE surrogate marker
# yob: outcome of interest (continuous or binary)
# aob: treatment assignment (1: treatment; 0: control)
# var: whether variance should be calculated (TRUE/FALSE)
# conf.int: whether 95% confidence intervals should be calculated (TRUE/FALSE)
# rep: number of resampling replications (default is 500)
pte_d <- function(sob, yob, aob, var = TRUE, conf.int = TRUE, rep = 500) {
  pte.fun <- function(v, sob, yob, aob, n, kern, kern2, s) {
    v <- as.numeric(v)
    # gs estimation
    m.s.hat <- colSums(v * yob * kern) / colSums(v * kern)
    m.sob.hat <- colSums(v * yob * kern2) / colSums(v * kern2)
    c.hat <- mean(v * yob * (1 - aob)) / mean(v * (1 - aob)) -
      mean(v * m.sob.hat * (1 - aob)) / mean(v * (1 - aob))
    f0s.hat <- colMeans(v * kern * (1 - aob)) / mean(v * (1 - aob))
    fs.hat <- colMeans(v * kern)
    integrand <- f0s.hat^2 / fs.hat
    temp <- sum(integrand)
    g.s.hat <- m.s.hat + f0s.hat / fs.hat * c.hat / temp
    # pte estimation
    causal <- mean(v * yob * aob) / mean(v * aob) - 
      mean(v * yob * (1 - aob)) / mean(v * (1 - aob))
    tempind <- c(sapply(1:n, function(kk) {which.min(abs(sob[kk] - s))}))
    causals <- mean(v * g.s.hat[tempind] * aob) / mean(v * aob) -
      mean(v * g.s.hat[tempind] * (1 - aob)) / mean(v * (1 - aob))
    pte1 <- causals / causal
    mses <- 2 * mean(v * (yob - g.s.hat[tempind])^2)
    c <- mean(v * yob * (1 - aob)) / mean(v * (1 - aob))
    mse <- 2 * mean(v * (yob - c)^2) 
    pte2 <- sqrt(1 - mses / mse)
    c(causal, causals, pte1, pte2, g.s.hat)
  }
  n <- length(yob)
  s <- sort(unique(sob))
  kern <- sapply(1:length(s), function(kk) {as.numeric(sob == s[kk])})
  kern2 <- sapply(1:length(sob), function(kk) {as.numeric(sob == sob[kk])})
  out <- pte.fun(v = rep(1, n), sob, yob, aob, n, kern, kern2, s)
  ans <- list("delta" = out[1], "delta.gs" = out[2], "pte1" = out[3], "pte2" = out[4])
  # resampling
  if (var) { 
    v <- matrix(rexp(n * rep), nrow = n)
    g.s.re <- matrix(NA, length(out), rep)
    for (j in 1:rep) {
      v.ptb <- v[, j]
      g.s.re[, j] <- pte.fun(v.ptb, sob, yob, aob, n, kern, kern2, s)
    }
    ans$delta.se <- sd(g.s.re[1, ])
    ans$delta.gs.se <- sd(g.s.re[2, ])
    temp1 <- g.s.re[3, ] 
    temp2 <- g.s.re[4, ]    
    ans$pte1.se <- sd(temp1[(temp1 < 1) * (temp1 > 0) > 0])
    ans$pte2.se <- sd(temp2[(temp2 < 1) * (temp2 > 0) > 0])
    # confidence intervals
    if (conf.int) { 
      ans$conf.int.delta <- ans$delta + c(-1, 1) * 1.96 * ans$delta.se
      ans$conf.int.delta.gs <- ans$delta.gs + c(-1, 1) * 1.96 * ans$delta.gs.se
      ans$conf.int.pte1 <- ans$pte1 + c(-1, 1) * 1.96 * ans$pte1.se
      ans$conf.int.pte2 <- ans$pte2 + c(-1, 1) * 1.96 * ans$pte2.se      
    }
  }
  ans
}
