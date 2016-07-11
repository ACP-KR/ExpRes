# Function for E7.1

FxE7.1 = function(Tbl, alpha=0.05)
{
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  n = length(unique(Tbl[,3]))
  r = length(unique(Tbl[,4]))

  xijkp = Tbl[,5]
  x.... = mean(xijkp)

  xi... = vector(length=l)
  xij.. = matrix(nrow=l, ncol=m)
  xijk. = matrix(nrow=l*m*n, ncol=4)

  for(i in 1:l) xi...[i] = mean(Tbl[Tbl[,1]==i,5])
  for(i in 1:l) for(j in 1:m) xij..[i,j] = mean(Tbl[Tbl[,1]==i & Tbl[,2]==j, 5])

  CurRow = 1
  for(i in 1:l)
    for(j in 1:m)
      for(k in 1:n) {
        xijk.[CurRow, 1] = i
        xijk.[CurRow, 2] = j
        xijk.[CurRow, 3] = k
        xijk.[CurRow, 4] = mean(Tbl[Tbl[,1]==i & Tbl[,2]==j & Tbl[,3]==k, 5])
        CurRow = CurRow + 1
      }

  SST = sum((xijkp - x....)^2)
  SSA = m*n*r*sum((xi... - x....)^2)
  SSAB = n*r*sum((xij.. - x....)^2)
  SSABC = r*sum((xijk.[,4] - x....)^2)

  SSB.A = SSAB - SSA
  SSC.AB = SSABC - SSAB
  SSE = SST - SSABC

  df.A = l - 1
  df.B.A = l*(m - 1)
  df.C.AB = l*m*(n - 1)
  df.E = l*m*n*(r - 1)
  df.T = l*m*n*r - 1

  SS = c(SSA, SSB.A, SSC.AB, SSE, SST)
  phi = c(df.A, df.B.A, df.C.AB, df.E, df.T)
  MS = SS / phi

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("A", "B(A)", "C(AB)", "E", "T")
  rownames(Res1) = RowNameList

  F.val = MS[1:3]/MS[2:4]
  F.crt = qf(1 - alpha, phi[1:3], phi[2:4])
  p.val = 1 - pf(F.val, phi[1:3], phi[2:4])

  Res2 = cbind(F.val, F.crt, p.val)
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:3]
  
  MSE = MS[4]
  S2C.AB = (MS[3] - MS[4])/r
  S2B.A = (MS[2] - MS[3])/(n*r)
  S2A = (MS[1] - MS[2])/(m*n*r)
  
  Res3 = c(MSE, S2C.AB, S2B.A, S2A)
  names(Res3) = c("MSE", "s2c(ab)", "s2b(a)", "s2a")
  Res = list(Res1, Res2, Res3)
  names(Res) = c("Sum of Squares", "F test", "Variance Estimation")
  return(Res)
}
