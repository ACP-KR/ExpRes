# Chapter 7 Split-Plot design see p213

p213 = function(Tbl, alpha=0.05)
{
  xijk = Tbl[,4]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  r = length(unique(Tbl[,3]))
  x... = mean(xijk)

  xi.. = vector(length=l)
  x.j. = vector(length=m)
  xij. = matrix(nrow=l, ncol=m)

  for (i in 1:l) xi..[i] = mean(Tbl[Tbl[,1]==i, 4])
  for (j in 1:m) x.j.[j] = mean(Tbl[Tbl[,2]==j, 4])
  for (i in 1:l) for(j in 1:m) xij.[i,j] = mean(Tbl[Tbl[,1]==i & Tbl[,2]==j,4])

  SST = sum((xijk - x...)^2)
  SSA = m*r*sum((xi.. - x...)^2)
  SSB = l*r*sum((x.j. - x...)^2)
  SSAB = r*sum((xij. - x...)^2)
  SSE1 = SSAxB = SSAB - SSA - SSB
  SSE2 = SST - SSAB

  df.A = l - 1
  df.B = m - 1
  df.E1 = (l - 1)*(m - 1)
  df.E2 = l*m*(r - 1)
  df.T = l*m*r - 1

  SS = c(SSA, SSB, SSE1, SSE2, SST)
  phi = c(df.A, df.B, df.E1, df.E2, df.T)
  MS = SS / phi
  MSE1 = MS[3]
  MSE2 = MS[4]

  F.val1 = MS[1:2]/MSE1
  F.val2 = MSE1/MSE2
  F.crt1 = qf(1 - alpha, phi[1:2], df.E1)
  F.crt2 = qf(1 - alpha, df.E1, df.E2)
  p.val1 = 1 - pf(F.val1, phi[1:2], df.E1)
  p.val2 = 1 - pf(F.val2, df.E1, df.E2)

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("Factor A", "Factor B", "Error(1)", "Error(2)", "Total")
  rownames(Res1) = RowNameList
  Res2 = cbind(c(F.val1, F.val2), c(F.crt1, F.crt2), c(p.val1, p.val2))
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:3]
  Res = list(Res1, Res2)
  names(Res) = c("Sum of Squares", "F test")
  return(Res)
}
