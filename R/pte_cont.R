#' @title PTE estimation with continuous surrogate marker
#' @param sob CONTINUOUS surrogate marker
#' @param yob outcome of interest (continuous or binary)
#' @param aob treatment assignment (1: treatment; 0: control)
#' @param var whether variance should be calculated (TRUE/FALSE)
#' @param conf.int whether 95\% confidence intervals should be calculated (TRUE/FALSE)
#' @param rep number of resampling replications (default is 500)
#' @export
pte_cont <- function(sob, yob, aob, var = TRUE, conf.int = TRUE, rep = 500) {
  VTM <- function(vc, dm) matrix(vc, ncol = length(vc), nrow = dm, byrow = TRUE)
  Kern.FUN <- function(zz, zi, bw) dnorm((VTM(zz, length(zi)) - zi) / bw) / bw
  pte.fun <- function(v, sob, yob, aob, n, kern, kern2, nn, s, step) {
    v <- as.numeric(v)
    # gs estimation
    m.s.hat <- colSums(v * yob * kern) / colSums(v * kern)
    m.sob.hat <- colSums(v * yob * kern2) / colSums(v * kern2)
    c.hat <- mean(v * yob * (1 - aob)) / mean(v * (1 - aob)) -
      mean(v * m.sob.hat * (1 - aob)) / mean(v * (1 - aob))
    f0s.hat <- colMeans(v * kern * (1 - aob)) / mean(v * (1 - aob))
    fs.hat <- colMeans(v * kern)
    integrand <- f0s.hat^2 / fs.hat
    temp <- (integrand[1] + integrand[nn + 1] +
               2 * sum(integrand[seq(2, nn, by = 2)]) +
               4 * sum(integrand[seq(3, nn - 1, by = 2)])) * step / 3
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
    mse <- 2 * (mean(v * (yob - c)^2))
    pte2 <- sqrt(1 - mses / mse)
    c(causal, causals, pte1, pte2, g.s.hat)
  }
  nn <- 199
  n <- length(yob)
  from <- min(sob)
  to <- max(sob)
  step <- (to - from) / nn
  s <- seq(from, to, by = step)
  bw <- 1.06 * sd(sob) * n^(-0.26) # n^(-1/5)/(n^0.06)
  kern <- Kern.FUN(zz = s, zi = sob, bw)
  kern2 <- Kern.FUN(zz = sob, zi = sob, bw)
  out <- pte.fun(v = rep(1, n), sob, yob, aob, n, kern, kern2, nn, s, step)
  ans <- list("delta" = out[1], "delta.gs" = out[2], "pte1" = out[3], "pte2" = out[4])
  ret <- out[1:4]
  names(ret) <- c("delta", "delta.gs", "pte1", "pte2")
  # resampling
  if (var) {
    v <- matrix(rexp(n * rep), nrow = n)
    g.s.re <- matrix(NA, length(out), rep)
    for (j in 1:rep) {
      v.ptb <- v[, j]
      g.s.re[, j] <- pte.fun(v.ptb, sob, yob, aob, n, kern, kern2, nn, s, step)
    }
    ans$delta.se <- sd(g.s.re[1, ])
    ans$delta.gs.se <- sd(g.s.re[2, ])
    temp1 <- g.s.re[3, ]
    temp2 <- g.s.re[4, ]
    ans$pte1.se <- sd(temp1[(temp1 < 1) * (temp1 > 0) > 0])
    ans$pte2.se <- sd(temp2[(temp2 < 1) * (temp2 > 0) > 0])
    ret <- cbind(ret, c(ans$delta.se, ans$delta.gs.se, ans$pte1.se, ans$pte2.se))
    colnames(ret) <- c("est", "se")
    # confidence intervals
    if (conf.int) {
      ans$conf.int.delta <- ans$delta + c(-1, 1) * 1.96 * ans$delta.se
      ans$conf.int.delta.gs <- ans$delta.gs + c(-1, 1) * 1.96 * ans$delta.gs.se
      ans$conf.int.pte1 <- ans$pte1 + c(-1, 1) * 1.96 * ans$pte1.se
      ans$conf.int.pte2 <- ans$pte2 + c(-1, 1) * 1.96 * ans$pte2.se
      ret <- cbind(ret, rbind(
        ans$conf.int.delta,
        ans$conf.int.delta.gs,
        ans$conf.int.pte1,
        ans$conf.int.pte2))
      colnames(ret) <- c("est", "se", "lower", "upper")
    }
  }
  ret
}
