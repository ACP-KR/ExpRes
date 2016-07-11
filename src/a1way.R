# One-way ANOVA. This can handle different data counts among levels and random factor.

a1way = function(Tbl, alpha=0.05) 
{
  xij = Tbl[,2]
  N   = length(xij)
  l   = length(unique(Tbl[,1]))
  mi  = vector(length=l)
  xi.  = vector(length=l)
  for (i in 1:l) {
    xi = xij[Tbl[,1]==i]
    mi[i] = length(xi)
    xi.[i] = mean(xi)
  }
  x.. = mean(xij)
  SST = sum((xij - x..)^2)
  SSA = sum(mi*(xi. - x..)^2)
  SSE = SST - SSA
  SS = c(SSA, SSE, SST)

  df.A = l - 1
  df.E = N - l
  df.T = N - 1
  phi = c(df.A, df.E, df.T)

  MS = SS / phi
  MSA = MS[1]
  MSE = MS[2]

  F.val = MSA/MSE
  F.crt = qf(1 - alpha, df.A, df.E)
  p.val = 1 - pf(F.val, df.A, df.E)

  t.crt <<- qt(1 - alpha/2, df.E)
  LL = xi. - t.crt*sqrt(MSE/mi)
  UL = xi. + t.crt*sqrt(MSE/mi)

  t.tukey = qtukey(1 - alpha, l, df.E)/sqrt(2)
  PDIFF = matrix(ncol=9, nrow=l*(l - 1)/2)
  CurRow = 1
  for(i in 1:(l-1)){
    for(j in (i+1):l) {
      SE = sqrt(MSE*(1/mi[i] + 1/mi[j]))
      PDIFF[CurRow, 1] = i
      PDIFF[CurRow, 2] = j
      PDIFF[CurRow, 3] = xi.[i] - xi.[j]
      PDIFF[CurRow, 4] = xi.[i] - xi.[j] - t.crt*SE
      PDIFF[CurRow, 5] = xi.[i] - xi.[j] + t.crt*SE
      if (PDIFF[CurRow, 4]*PDIFF[CurRow, 5] > 0) PDIFF[CurRow, 6] = 1 
      else PDIFF[CurRow, 6] = 0
      PDIFF[CurRow, 7] = xi.[i] - xi.[j] - t.tukey*SE
      PDIFF[CurRow, 8] = xi.[i] - xi.[j] + t.tukey*SE
      if (PDIFF[CurRow, 7]*PDIFF[CurRow, 8] > 0) PDIFF[CurRow, 9] = 1 
      else PDIFF[CurRow, 9] = 0
      CurRow = CurRow + 1
    }  
  }

  Res1 = cbind(SS, phi, MS)
  rownames(Res1) = c("Factor A", "Error", "Total")
  Res2 = c(F.val, F.crt, p.val)
  names(Res2) = c("F", "F(0.05)", "p-value")
  Res3 = cbind(LL, xi., UL)
  Res4 = PDIFF
  colnames(Res4) = c("Ai", "Aj", "Difference", "LL", "UL", "Signficance", "TukeyHSD LL", "TukeyHSD UL", "Tukey Signficance")
  Res5 = c(SSE/qchisq(1 - alpha/2, df.E), MSE, SSE/qchisq(alpha/2, df.E))
  names(Res5) = c("LL", "PE", "UL")
  s2a = (MSA - MSE)/((N*N - sum(mi^2))/(N*(l-1))) 
  Res6 = c(s2a, s2a/(s2a + MSE))
  names(Res6) = c("s2a", "s2a/(s2a + MSE)")
  Res = list(Res1, Res2, Res3, Res4, Res5, Res6)
  names(Res) = c("Sum of Squares", "F test for Factor A as Fixed Factor", "Level Means and Boundary of Factor A as Fixed Factor", "Pairwise Difference of Factor A as Fixed Factor", "MSE for s2e", "s2a for Factor A as Random Factor")
  return(Res)
}
