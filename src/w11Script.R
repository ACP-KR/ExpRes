source("C:/Rt/ExpDes/MLR.R")
T03 = read.csv("C:/Rt/ExpDes/CH02T03.CSV")

T03[,"AGE"] = T03[,"AGE"]/50
T03[,"BSA"] = T03[,"BSA"]/1.83
T03[,"DOSE"] = T03[,"DOSE"]/1000

mlr(log(T03[,7]), T03[,2:6])

mlr(log(T03[,7]), T03[,c("BSA","DOSE")])


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
  if (b.p[i] > 0.5) b.p[i] = 1 - b.p[i]
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

