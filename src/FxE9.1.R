# Function for Example 9.1

FxE9.1 = function(Tbl, alpha=0.05)
{
  xijl = Tbl[,4]
  k = length(unique(Tbl[,1]))

  xi.. = vector(length=k)
  x.j. = vector(length=k)
  x..l = vector(length=k)

  for (i in 1:k) xi..[i] = mean(Tbl[Tbl[,1]==i,4])
  for (j in 1:k) x.j.[j] = mean(Tbl[Tbl[,2]==j,4])
  for (l in 1:k) x..l[l] = mean(Tbl[Tbl[,3]==l,4])

  x... = mean(xijl)
  SST = sum((xijl - x...)^2)
  SSA = k*sum((xi.. - x...)^2)
  SSB = k*sum((x.j. - x...)^2)
  SSC = k*sum((x..l - x...)^2)
  SSE = SST - SSA - SSB - SSC

  df.A = k - 1
  df.B = k - 1
  df.C = k - 1
  df.E   = (k - 1)*(k - 2)
  df.T   = k*k - 1

  SS = c(SSA, SSB, SSC, SSE, SST)
  phi = c(df.A, df.B, df.C, df.E, df.T)
  MS = SS/phi
  MSE = MS[4]

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("A", "B", "C", "E", "T")
  rownames(Res1) = RowNameList

  F.val = MS[1:3]/MSE
  F.crt = qf(1 - alpha, phi[1:3], df.E)
  p.val = 1 - pf(F.val, phi[1:3], df.E)

  Res2 = cbind(F.val, F.crt, p.val)
  rownames(Res2) = RowNameList[1:3]

  xijl.hat = matrix(nrow=k*k*k, ncol=4)
  CurRow = 1
  for(i in 1:k) {
    for(j in 1:k) {
      for(l in 1:k) {
        xijl.hat[CurRow, 1] = i
        xijl.hat[CurRow, 2] = j
        xijl.hat[CurRow, 3] = l
        xijl.hat[CurRow, 4] = xi..[i] + x.j.[j] + x..l[l] - 2*x...
        CurRow = CurRow + 1
      }
    }
  }
  colnames(xijl.hat) = c("A", "B", "C", "Expection")

  SDiff1 = qt(1 - alpha/2, df.E)*sqrt(MSE/k)
  SDiff2 = qt(1 - alpha/2, df.E)*sqrt(MSE*(3*k-2)/(k*k))

  Res = list(Res1, Res2, xi.., x.j., x..l, SDiff1, SDiff2, xijl.hat)
  names(Res) = c("Sume of Squares", "F test", "Means of Factor A", "Means of Factor B", "Means of Factor C", "SE for Means", "SE for Difference", "Expections of Means")
  return(Res)
}
