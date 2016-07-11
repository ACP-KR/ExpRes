
mlr = function(y, x.raw)
{
  if (kappa(x.raw) > 999) cat(paste("Condition Number is ", kappa(x.raw), ". Consider standardization !\n", sep=""))
  if (length(y) != length(x.raw[,1])) {
    cat("Numbers of rows of x matrix and y vector are different.\n")
    return(NULL)
  }

  n = length(y)
  namelist = c("Intercept", names(x.raw))

  Intercept = 1
  x = as.matrix(cbind(Intercept, x.raw))

  b = solve(t(x) %*% x) %*% t(x) %*% y
  p = length(b)

  y.hat = x %*% b
  e     = y - y.hat
  SSE   = sum(e^2)
  MSE   = SSE/(n - p)
  b.se  = sqrt(diag(as.numeric(MSE) * solve(t(x) %*% x)))
  b.t   = b/b.se
  b.p   = pt(b.t, n - p)
  for (i in 1:p) {
    if (b.p[i] > 0.5) b.p[i] = 1 - b.p[i]
  }
  b.p = 2 * b.p

  res1 = data.frame(namelist, cbind(b, b.se, b.t, b.p))
  names(res1) = c("Variable", "Estimate", "SE", "T", "p-value")

  h    = as.matrix(diag(x %*% solve(t(x) %*% x) %*% t(x)))
  sr   = e / sqrt(MSE * (1 - h))
  MSEi = ( (n - p)*MSE - e^2/(1 - h) ) / (n - p - 1)
  sdr  = e / sqrt(MSEi * (1 - h))

  DFFITS = sqrt(h/(1 - h)) * e / sqrt(MSEi*(1 - h))

  bi = matrix(nrow=n, ncol=p)
  for (i in 1:n) {
    z = x[-i,]
    bi[i,] = solve(t(z) %*% z) %*% t(z) %*% y[-i]
  }
  bm = matrix(rep(t(b),n), byrow=T, ncol=p)
  DFBETAS = (bm - bi)/sqrt(MSEi %*% diag(solve(t(x) %*% x)))

  COVRATIO = matrix(nrow=n)
  for (i in 1:n) {
    COVRATIO[i] = det(MSEi[i] * solve(t(x[-i,]) %*% x[-i,])) / det(MSE*solve(t(x) %*% x))
  }

  D = e^2 / (1-h)^2 * h / (p * MSE)

  windows()
  par(mfrow=c(2,2))
  plot(y.hat, e, xlab="Predicted Value", ylab="Residual")
  plot(y.hat, sr, xlab="Predicted Value", ylab="Studentized Residual")
  plot(h, e*e/SSE, xlab="HAT", ylab="e^2/SSE")
  plot(COVRATIO, DFFITS, xlab="COVRATIO", ylab="DFFITS")
   
  res2 = data.frame(cbind(y.hat, e, sdr, h, D, COVRATIO, DFFITS, DFBETAS))
  names(res2) = c("Yhat", "Residual", "R-Student", "hat", "Cook's D", "COV-Ratio", "DFFITS", namelist)

  result = list(res1, res2, n, p, n-p, SSE, MSE)
  names(result) = c("Model Estimates", "Influence Diagnostics with DFBETAs", "n", "Parameter Count", "Degree of Freedom", "SSE", "MSE")

  return(result)
}
