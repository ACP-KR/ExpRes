#library(MASS)
source("c:/nma/nmlib2.r")
setwd("C:/Rt/ExpDes/")
t0203 = read.csv("T0203.CSV")
t0203$LogCL = log(t0203$X5FU_CL)
t0203$sDose = t0203$Dose / 1000
t0203$sAge = t0203$Age / 60
t0203$sBSA = t0203$BSA / 1.83

summary(lm(LogCL ~ sBSA + sDose, data=t0203))

y = as.matrix(t0203$LogCL)
n = length(y)

Intercept = 1
x = as.matrix(cbind(Intercept, t0203[,c(11,9)]))

b = solve(t(x) %*% x) %*% t(x) %*% y
p = length(b)

y.hat = x %*% b

SSE = as.numeric(t(y) %*% y - t(b) %*% t(x) %*% y)
MSE = SSE / (n - p)
b.var = diag(as.numeric(MSE) * solve(t(x) %*% x))
b.se = sqrt(diag(as.numeric(MSE) * solve(t(x) %*% x)))
residual = y - y.hat
h = as.matrix(diag(x %*% solve(t(x) %*% x) %*% t(x)))
b.t = b / b.se
b.p = pt(b.t, n-p)
for (i in 1:p) {
  if (b.p[,i] > 0.5) b.p[,i] = 1 - b.p[,i]
}
b.p = 2 * b.p

cbind(b, b.se, b.t, b.p)

e = y - y.hat

e.s = sqrt(MSE * (1-h))
sr = e / e.s
MSEi = ((n-p)*MSE - e^2 / (1 - h)) / (n-p-1)
sdr = e / sqrt(MSEi*(1-h))

DFFITS = sqrt(h/(1-h))*e/sqrt(MSEi*(1-h))

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

cbind(e, sdr, h, COVRATIO, DFFITS, DFBETAS)


###

y = as.vector(t0203$LogCL)
Intercept = 1
x = as.matrix(cbind(Intercept, t0203$BSA))

b = solve(t(x) %*% x) %*% t(x) %*% y
y.hat = x %*% b

SSE = t(y) %*% y - t(b) %*% t(x) %*% y
MSE = SSE / (length(y) - length(b))
b.var = diag(as.numeric(MSE) * solve(t(x) %*% x))
b.se = sqrt(diag(as.numeric(MSE) * solve(t(x) %*% x)))
residual = y - y.hat
H = x %*% solve(t(x) %*% x) %*% t(x)
b.t = b / b.se
2*(1 - pt(b.t, 23))
2*(pt(b.t, 23))

e = y - y.hat
h = diag(H)
e.s = sqrt(MSE * (1-h))
r = e / e.s
plot(t0203$BSA, h)










f0605 = read.csv('f0605.csv')
x.raw = f0605[,2:3]
Intercept = 1
X = as.matrix(cbind(Intercept, x.raw))
Y = f0605[,4]
b = solve(t(X) %*% X) %*% t(X) %*% Y
Y.hat = X %*% b
SSE = t(Y) %*% Y - t(b) %*% t(X) %*% Y
MSE = SSE / (length(Y) - length(b))
b.var = diag(as.numeric(MSE) * solve(t(X) %*% X))
b.se = sqrt(diag(as.numeric(MSE) * solve(t(X) %*% X)))



#######################################
condnum = function (x)
{
  sqrt(max(eigen(t(x) %*% x)[[1]]) / min(eigen(t(x) %*% x)[[1]]))
}

mlr = function(y, x.raw, standardize=0)
{
  if (kappa(x.raw) > 999 & standardize==0) cat(paste("Condition Number is ", kappa(x.raw), ". Consider standardization !\n", sep=""))
  if (length(y) != length(x.raw[,1])) {
    cat("Numbers of rows of x matrix and y vector are different.\n")
    return(NULL)
  }

  n = length(y)
  namelist = c("Intercept", names(x.raw))

  x.avg = matrix(rep(mean(x.raw, na.rm=T), n), nrow=n, byrow=T)
  if (standardize==3) {
    x.sd = matrix(rep(sd(x.raw, na.rm=T), n), nrow=n, byrow=T)
    x = (x.raw - x.avg) / x.sd
  } else if(standardize==2) {
    x = x.raw / x.avg
  } else if(standardize==1) {
    x = x.raw - x.avg
  } else {
    x = x.raw
  }

  Intercept = 1
  x = as.matrix(cbind(Intercept, x))

  b = solve(t(x) %*% x) %*% t(x) %*% y
  p = length(b)

  y.hat = x %*% b
  e = y - y.hat
  SSE = sum(e^2)
  MSE = SSE / (n - p)
  b.se = sqrt(diag(as.numeric(MSE) * solve(t(x) %*% x)))
  b.t = b / b.se
  b.p = pt(b.t, n-p)
  for (i in 1:p) {
    if (b.p[i] > 0.5) b.p[i] = 1 - b.p[i]
  }
  b.p = 2 * b.p

#  if (standardize == 2) {
#    b[-1] = b[-1] * x.avg[1,]
#    b.se[-1] = b.se[-1] * x.avg[1,1]
#  }

  res1 = data.frame(namelist, cbind(b, b.se, b.t, b.p))
  names(res1) = c("Variable", "Estimate", "SE", "T", "p-value")

  h = as.matrix(diag(x %*% solve(t(x) %*% x) %*% t(x)))
  sr = e / sqrt(MSE * (1-h))
  MSEi = ((n-p)*MSE - e^2 / (1 - h)) / (n-p-1)
  sdr = e / sqrt(MSEi*(1-h))

  DFFITS = sqrt(h/(1-h))*e/sqrt(MSEi*(1-h))

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

  res2 = data.frame(cbind(e, sdr, h, D, COVRATIO, DFFITS, DFBETAS))
  names(res2) = c("Residual", "R-Student", "hat", "Cook's D", "COV-Ratio", "DFFITS", namelist)

  windows()
  par(mfrow=c(2,2), oma=c(1,1,3,1))
  plot(D, type="n", xlab="Index", ylab="Cook's Distance")
  for(i in 1:n) {
    if(D[i] == max(D)) text(i, D[i], i)
    else points(i, D[i])
  }

  plot(y.hat, sdr, type="n", xlab="Predicted Value", ylab="Studentized deleted residuals")
  for(i in 1:n) {
    if(abs(sdr[i]) > 2) text(y.hat[i], sdr[i], i)
    else points(y.hat[i], sdr[i])
  }

  plot(h, e^2/SSE, type="n", xlab="hat", ylab="e^2/SSE")
  for(i in 1:n) {
    if(e[i]^2/SSE > 0.15) text(h[i], e[i]^2/SSE, i)
    else points(h[i], e[i]^2/SSE)
  }

  plot(COVRATIO, type="n", DFFITS, xlab="COVRATIO", ylab="DFFITS")
  for(i in 1:n) {
    if(abs(DFFITS[i]) > 1 | abs(COVRATIO[i]-1) > 3*p/n) text(COVRATIO[i], DFFITS[i], i)
    else points(COVRATIO[i], DFFITS[i])
  }

  mtext("Influence Diagnostics", outer=T, side=3)

  result = list(res1, res2)
  if (standardize == 1 | standardize == 2 | standardize == 3) {
    names(result) = c("Model Estimates with Standardization", "Influence Diagnostics with DFBETAs")
  } else {
    names(result) = c("Model Estimates", "Influence Diagnostics with DFBETAs")
  }
  result
}


#############

t0203 = read.csv('T0203.CSV')
t0203$LogCL = log(t0203$X5FU_CL)
windows()
par(mfrow=c(3,2), oma=c(1,1,3,1))
plot(t0203$Age, t0203$X5FU_CL, log="y", ylim=c(0.1,10))
abline(lm(log(t0203$X5FU_CL)~t0203$Age))

plot(t0203$BSA, t0203$X5FU_CL, log="y", ylim=c(0.1,10))
abline(lm(log(t0203$X5FU_CL)~t0203$BSA))

plot(t0203$Dose, t0203$X5FU_CL, log="y", ylim=c(0.1,10))
abline(lm(log(t0203$X5FU_CL)~t0203$Dose))

boxplot(t0203$Sex, t0203$X5FU_CL)
boxplot(t0203$MTX, t0203$X5FU_CL)

y = t0203[,"LogCL"]
x = t0203[,4:5]

summary(lm(y ~ x$BSA + x$Dose))
mlr(y, x)
mlr(y, x, standardize=2)

x$BSA = x$BSA / 1.83
x$Dose = x$Dose / 1000
mlr(y, x)

y = t0203[,"LogCL"]
x = t0203[,4:5]
n = length(x[,1])
x.avg = matrix(rep(mean(x), n), nrow=n, byrow=T)
x.sd = matrix(rep(sd(x), n), nrow=n, byrow=T)
x = (x - x.avg) / x.sd
x$BSA = x$BSA / 1.83
x$Dose = x$Dose / 1000
mlr(x, y)


f0605 = read.csv('f0605.csv')
X = f0605[,2:3]
Y = f0605[,4]
mlr(X,Y)

