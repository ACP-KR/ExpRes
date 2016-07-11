# BIBD.R

BIBD = function(Tbl, alpha=0.05)
{
  xij = Tbl[,3]
  N   = length(xij)
  l   = length(unique(Tbl[,1])) # treatment
  m   = length(unique(Tbl[,2])) # block
  p   = N/m                     # block size
  r   = m*p/l                   # replicate per treatment
  f.adj = (l - 1)/l*p/(p - 1)
  xi. = vector(length=l)
  x.j = vector(length=m)
  xai = vector(length=l)

  for (i in 1:l) xi.[i] = mean(Tbl[Tbl[,1]==i,3])
  for (j in 1:m) x.j[j] = mean(Tbl[Tbl[,2]==j,3])
  for (i in 1:l) {
    tmp = vector()
    for (j in Tbl[Tbl[,1]==i,2]) {
      tmp = c(tmp, Tbl[Tbl[,2]==j,3])
    }
    xai[i] = mean(tmp)
  }
  x.. = mean(xij)
  SST = sum((xij - x..)^2)
  SSA = p*sum((x.j - x..)^2)
  SSB = r*f.adj*sum((xi. - xai)^2)
  SSE = SST - SSA - SSB
  SS = c(SSA, SSB, SSE, SST)

  df.A = m - 1
  df.B = l - 1
  df.E = N - l - m + 1
  df.T = N - 1
  phi = c(df.A, df.B, df.E, df.T)
  MS = SS / phi
  Res1 = cbind(SS, phi, MS)
  RowNameList = c("A", "B", "E", "T")
  rownames(Res1) = RowNameList

  MSE = MS[3]
  F.val = MS[1:2]/MSE
  F.crt = qf(1 - alpha, phi[1:2], df.E)
  p.val = 1 - pf(F.val, phi[1:2], df.E)
  Res2 = cbind(F.val, F.crt, p.val)
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:2]

  yi. = x.. + f.adj * (xi. - xai)
  LL = yi. - qt(1 - alpha/2, df.E)*sqrt(f.adj*MSE/r)
  UL = yi. + qt(1 - alpha/2, df.E)*sqrt(f.adj*MSE/r)
  Res3 = cbind(LL, yi., UL)

  t.crt <<- qt(1 - alpha/2, df.E)
  t.tukey = qtukey(1 - alpha, l, df.E)/sqrt(2)
  PDIFF = matrix(ncol=9, nrow=l*(l - 1)/2)
  SE = sqrt(f.adj*MSE/r*2)
  CurRow = 1
  for(i in 1:(l-1)){
    for(j in (i+1):l) {
      PDIFF[CurRow, 1] = i
      PDIFF[CurRow, 2] = j
      PDIFF[CurRow, 3] = yi.[i] - yi.[j]
      PDIFF[CurRow, 4] = yi.[i] - yi.[j] - t.crt*SE
      PDIFF[CurRow, 5] = yi.[i] - yi.[j] + t.crt*SE
      if (PDIFF[CurRow, 4]*PDIFF[CurRow, 5] > 0) PDIFF[CurRow, 6] = 1
      else PDIFF[CurRow, 6] = 0
      PDIFF[CurRow, 7] = yi.[i] - yi.[j] - t.tukey*SE
      PDIFF[CurRow, 8] = yi.[i] - yi.[j] + t.tukey*SE
      if (PDIFF[CurRow, 7]*PDIFF[CurRow, 8] > 0) PDIFF[CurRow, 9] = 1
      else PDIFF[CurRow, 9] = 0
      CurRow = CurRow + 1
    }
  }
  colnames(PDIFF) = c("i", "j", "Difference", "LL", "UL", "Significance", "TukeyHSD LL", "TukeyHSD UL", "TukeyHSD Significance")
  Res = list(Res1, Res2, Res3, PDIFF)
  names(Res) = c("Sum of Squares", "F test", "Adjusted Level Means and Intervals", "Pairwise Difference")

  return(Res)
}
