#Example in PPT file

a = 2
b = 2
y = c(34.4, 43.1, 30.6, 37.3)
n = c(32, 15, 180, 72)

X = matrix(c(1,1,1,1, 1,1,-1,-1, 1,-1,1,-1, 1,-1,-1,1), nrow=a*b)

XpX = matrix(nrow=a*b, ncol=a*b)
for (i in 1:(a*b)) for (j in 1:(a*b)) XpX[i,j] = sum(n*X[,i]*X[,j])
XpX

XpY = matrix(nrow=a*b, ncol=1)
for (i in 1:(a*b)) XpY[i] = sum(n * y * X[,i])
XpY

R.all = t(XpY) %*% solve(XpX) %*% XpY
R.all

R.mab = t(XpY[-4]) %*% solve(XpX[-4,-4]) %*% XpY[-4]
R.mab

R.all = t(XpY) %*% solve(XpX) %*% XpY
R.all

R.mab = t(XpY[-4]) %*% solve(XpX[-4,-4]) %*% XpY[-4]
R.mab

R.maab = t(XpY[-3]) %*% solve(XpX[-3,-3]) %*% XpY[-3]
R.maab

R.mbab = t(XpY[-2]) %*% solve(XpX[-2,-2]) %*% XpY[-2]
R.mbab

SSAxB = R.all - R.mab
SSA = R.all - R.mbab
SSB = R.all - R.maab

