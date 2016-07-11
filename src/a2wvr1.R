# Two-way ANOVA with Unequal Number of Observations
source("C:/Rt/ExpDes/MakeDesign.R")

a2wvr1 = function(Tbl, alpha=0.05) # Tbl should not have zero count cell
{
  xijk = Tbl[,"Y"]
  A.lvl = unique(Tbl[,"A"])
  B.lvl = unique(Tbl[,"B"])
  l = length(A.lvl)
  m = length(B.lvl)
  N = length(xijk)

  xij. = matrix(nrow=l, ncol=m)
  nij = matrix(nrow=l, ncol=m)
  yij. = data.frame(nrow=l*m, ncol=4)

  CurRow = 1
  SSE = 0
  for (i in 1:l) {
    for(j in 1:m) {
      xtmp = Tbl[Tbl[,"A"]==A.lvl[i] & Tbl[,"B"]==B.lvl[j], "Y"]
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

  x... = mean(xijk)
  SST = sum((xijk - x...)^2)

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

  SSA = R.all - R.mbab
  SSB = R.all - R.maab
  SSAxB = R.all - R.mab

  df.A = l - 1
  df.B = m - 1
  df.AxB = (l - 1)*(m - 1)
  df.T = N - 1
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
