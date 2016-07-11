# Youden.R

Youden = function(Tbl, alpha=0.05)
{
  xijk = Tbl[,4]
  N   = length(xijk)
  l   = length(unique(Tbl[,1])) # fxied factor
  m   = length(unique(Tbl[,2])) # block
  n   = length(unique(Tbl[,3])) # treatment
  p   = N/m                     # block size
  r   = N/n                     # replicate per treatment
  lambda = r*(p - 1)/(n - 1)
  f.adj = (n - 1)/n * p/(p - 1)
  xi.. = vector(length=l)
  x.j. = vector(length=m)
  x..k = vector(length=n)
  xa.k = vector(length=n)

  for (i in 1:l) xi..[i] = mean(Tbl[Tbl[,1]==i,4])
  for (j in 1:m) x.j.[j] = mean(Tbl[Tbl[,2]==j,4])
  for (k in 1:n) x..k[k] = mean(Tbl[Tbl[,3]==k,4])
  for (k in 1:n) {
    tmp = vector()
    for (j in Tbl[Tbl[,3]==k,2]) {
      tmp = c(tmp, Tbl[Tbl[,2]==j,4])
    }
    xa.k[k] = mean(tmp)
  }
  x... = mean(xijk)
  SST = sum((xijk - x...)^2)
  SSA = m*sum((xi.. - x...)^2)
  SSB = p*sum((x.j. - x...)^2)
  SSC = r*f.adj*sum((x..k - xa.k)^2)
  SSE = SST - SSA - SSB - SSC
  SS = c(SSA, SSB, SSC, SSE, SST)

  df.A = l - 1
  df.B = m - 1
  df.C = n - 1
  df.E = N - l - m - n + 2
  df.T = N - 1
  phi = c(df.A, df.B, df.C, df.E, df.T)
  MS = SS / phi
  Res1 = cbind(SS, phi, MS)
  RowNameList = c("A", "B", "C", "E", "T")
  rownames(Res1) = RowNameList

  MSE = MS[4]
  F.val = MS[1:3]/MSE
  F.crt = qf(1 - alpha, phi[1:3], df.E)
  p.val = 1 - pf(F.val, phi[1:3], df.E)
  Res2 = cbind(F.val, F.crt, p.val)
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:3]

  y..k = x... + f.adj*(x..k - xa.k)
  LL = y..k - qt(1 - alpha/2, df.E)*sqrt(f.adj*MSE/r)
  UL = y..k + qt(1 - alpha/2, df.E)*sqrt(f.adj*MSE/r)
  Res3 = cbind(LL, y..k, UL)

  t.crt <<- qt(1 - alpha/2, df.E)
  t.tukey = qtukey(1 - alpha, l, df.E)/sqrt(2)
  PDIFF = matrix(ncol=9, nrow=n*(n - 1)/2)
  SE = sqrt(f.adj*MSE/r*2)
  CurRow = 1
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      PDIFF[CurRow, 1] = i
      PDIFF[CurRow, 2] = j
      PDIFF[CurRow, 3] = y..k[i] - y..k[j]
      PDIFF[CurRow, 4] = y..k[i] - y..k[j] - t.crt*SE
      PDIFF[CurRow, 5] = y..k[i] - y..k[j] + t.crt*SE
      if (PDIFF[CurRow, 4]*PDIFF[CurRow, 5] > 0) PDIFF[CurRow, 6] = 1
      else PDIFF[CurRow, 6] = 0
      PDIFF[CurRow, 7] = y..k[i] - y..k[j] - t.tukey*SE
      PDIFF[CurRow, 8] = y..k[i] - y..k[j] + t.tukey*SE
      if (PDIFF[CurRow, 7]*PDIFF[CurRow, 8] > 0) PDIFF[CurRow, 9] = 1
      else PDIFF[CurRow, 9] = 0
      CurRow = CurRow + 1
    }
  }
  colnames(PDIFF) = c("i", "j", "Difference", "LL", "UL", "Significance", "TukeyHSD LL", "TukeyHSD UL", "TukeyHSD Significance")
  
  Res5 = c(SSE/qchisq(1 - alpha/2, df.E), MSE, SSE/qchisq(alpha/2, df.E))
  names(Res5) = c("LL", "PE", "UL")
  Res = list(Res1, Res2, Res3, PDIFF, Res5)
  names(Res) = c("Sum of Squares", "F test", "Level Means of Factor C", "Pairwise Difference Between Levels of Factor C", "Variance (s2e) Estimation")
  return(Res)
}
