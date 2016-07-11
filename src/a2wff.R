# Two-way ANOVA with two fixed factors and same replications
# used for Table 5.6 p135

a2wff = function(Tbl, alpha=0.05)
{
  xijk = Tbl[,3]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  r = length(xijk) / (l*m)
  x... = mean(xijk)

  xi.. = vector(length=l)
  x.j. = vector(length=m)
  xij. = matrix(nrow=l, ncol=m)
  for (i in 1:l) xi..[i] = mean(Tbl[Tbl[,1]==i, 3])
  for (j in 1:m) x.j.[j] = mean(Tbl[Tbl[,2]==j, 3])
  for (i in 1:l) for(j in 1:m) xij.[i,j] = mean(Tbl[Tbl[,1]==i & Tbl[,2]==j, 3])

  SST = sum((xijk - x...)^2)
  SSA = m*r*sum((xi.. - x...)^2)
  SSB = l*r*sum((x.j. - x...)^2)
  SSAB = r*sum((xij. - x...)^2)
  SSAxB = SSAB - SSA - SSB
  SSE = SST - SSAB

  df.A = l - 1
  df.B = m - 1
  df.AxB = (l - 1)*(m - 1)
  df.E = l*m*(r - 1)
  df.T = l*m*r - 1

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
  SE = sqrt(MSE/r)
  LL = xij. - t.crt * SE
  UL = xij. + t.crt * SE

  Res = list(Res1, Res2, xij., LL, UL)
  names(Res) = c("Sum of Squares", "F test for Fixed Factors", "Cell Means", "Lower Limits", "Upper Limits")
  return(Res)
}

