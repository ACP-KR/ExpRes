# Two-way ANOVA with Unequal Number of Observations
source("E:/Rt/ExpDes/MakeDesign.R")

a2wvr = function(Tbl, alpha=0.05) # Tbl should not have zero count cell
{
  xijk = Tbl[,3]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  N = length(xijk)

  xi.. = vector(length=l)
  ni.  = vector(length=l)
  x.j. = vector(length=m)
  n.j  = vector(length=m)
  xij. = matrix(nrow=l, ncol=m)
  nij = matrix(nrow=l, ncol=m)
  yij. = data.frame(nrow=l*m, ncol=4)

  CurRow = 1
  SSE = 0
  for (i in 1:l) {
    for(j in 1:m) {
      xtmp = Tbl[Tbl[,1]==i & Tbl[,2]==j, 3]
      xij.[i,j] = mean(xtmp)
      nij[i,j] = length(xtmp)
      SSE = SSE + sum((xtmp - xij.[i,j])^2)
      yij.[CurRow, 1] = i
      yij.[CurRow, 2] = j
      yij.[CurRow, 3] = nij[i,j]
      yij.[CurRow, 4] = xij.[i,j]
      CurRow = CurRow + 1
    }
  }
  colnames(yij.) = c("i", "j", "n", "mean")

  ni. = rowSums(nij)
  n.j = colSums(nij)
  xi.. = rowMeans(xij.)
  x.j. = colMeans(xij.)

  x... = mean(xijk)
  SST = sum((xijk - x...)^2)

############################### Graybill p575~9. This gives Type III SS of SAS, different from aov, lm in R
  hi. = rowSums(1/nij)
  h.j = colSums(1/nij)

  YA  = sum(1/hi. * rowSums(xij.)) / sum(1/hi.)
  YB =  sum(1/h.j * colSums(xij.)) / sum(1/h.j)
  SSA = sum(1/hi. * (rowSums(xij.) - YA)^2)
  SSB = sum(1/h.j * (colSums(xij.) - YB)^2)
#  SSAxB = SST - SSA - SSB - SSE
##################################

################################### (Same with above) This gives Type III SS of SAS, different from aov, lm in R
  X = MakeDesign(l, m)
  XpX = matrix(nrow=l*m, ncol=l*m)
  for (i in 1:(l*m)) for (j in 1:(l*m)) XpX[i,j] = sum(yij.[,3] * X[,i] * X[,j])

  XpY = matrix(nrow=l*m, ncol=1)
  for (i in 1:(l*m)) XpY[i] = sum(yij.[,3] * yij.[,4] * X[,i])

  A = 2:l
  B = (l+1):(l+m-1)
  AB = (l+m):(l*m)

  R.all  = t(XpY) %*% solve(XpX) %*% XpY
  R.mab  = t(XpY[-AB]) %*% solve(XpX[-AB,-AB]) %*% XpY[-AB]
  R.maab = t(XpY[-B]) %*% solve(XpX[-B,-B]) %*% XpY[-B]
  R.mbab = t(XpY[-A]) %*% solve(XpX[-A,-A]) %*% XpY[-A]

  SSAxB = R.all - R.mab
#  SSA = R.all - R.mbab
#  SSB = R.all - R.maab
####################################

############################  # See Neter p978-9, Do Not USE
#  SSA = sum(ni. * (xi.. - x...)^2)
#  SSB = sum(n.j * (x.j. - x...)^2)
#  SSAB = sum(yij.[,3] * (yij.[,4] - x...)^2)
#  SSAxB = 0
#  for (i in 1:l) for (j in 1:m) SSAxB = SSAxB + nij[i,j]*(xij.[i,j] - xi..[i] - x.j.[j] + x...)^2
# or
#  SSAxB = SSAB - SSA - SSB 
#############################

############################# Do Not USE
#  xi..bar = mean(xi..)
#  x.j.bar = mean(x.j.)
#  x...bar = mean(xi..bar)
#  xij.bar = yij.[,4]
#  SSA = sum(ni.*(xi.. - xi..bar)^2)
#  SSB = sum(n.j*(x.j. - x.j.bar)^2)
#  SSAB = sum(yij.[,3]*(yij.[,4] - x...bar)^2)
#  SSAxB = SSAB - SSA - SSB
############################

#  SSE = SST - SSA - SSB - SSAxB

  df.A = l - 1
  df.B = m - 1
  df.AxB = (l - 1)*(m - 1)
  df.T = N - 1
#  df.E = df.T - df.A - df.B - df.AxB
  df.E = N - l*m

  SS = c(SSA, SSB, SSAxB, SSE, SST)
  phi = c(df.A, df.B, df.AxB, df.E, df.T)
  MS = SS / phi
  MSE = MS[4]

  Res1 = cbind(SS, phi, MS)
  rownames(Res1) = c("Factor A", "Factor B", "Interation (AxB)", "Error", "Total")

  F.val = MS[1:3]/MSE
  F.crt = qf(1 - alpha, phi[1:3], df.E)
  p.val = 1 - pf(F.val, phi[1:3], df.E)

  Res2 = cbind(F.val, F.crt, p.val)

  t.crt = qt(1 - alpha/2, df.E)
  SE = sqrt(MSE/yij.[,3])
  LL = yij.[,4] - t.crt * SE
  UL = yij.[,4] + t.crt * SE
  Res3 = cbind(yij.[,1], yij.[,2], yij.[,3], LL, yij.[,4], UL)
  colnames(Res3) = c("i", "j", "n", "LL", "PE", "UL")

  Res = list(Res1, Res2, Res3)
  names(Res) = c("Sum of Squares", "F test for Fixed Factors", "Cell Means")
  return(Res)
}
