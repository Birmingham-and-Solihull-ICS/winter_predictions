library(dlm)


buildFun <- function(x) {
  m <- dlmModPoly(1, dV = exp(x[1]))
  m$JW <- matrix(1)
  m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
  j <- which(time(Nile) == 1899)
  m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
  return(m)
}

fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun)
fit$conv

dlmNileJump <- buildFun(fit$par)
V(dlmNileJump)

dlmNileJump$X[c(1, which(time(Nile) == 1899)), 1]


nileJumpFilt <- dlmFilter(Nile, dlmNileJump)


plot(Nile, type = 'o', col = "seagreen")
lines(dropFirst(nileJumpFilt$m), type = 'o',pch = 20, col = "brown")
