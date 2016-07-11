# Three way ANOVA without replicaion

a3w = function(Tbl, alpha=0.05)
{
  xijk = Tbl[,4]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  n = length(unique(Tbl[,3]))

  xi.. = vector(length=l)
  x.j. = vector(length=m)
  x..k = vector(length=n)
  xij. = matrix(nrow=l, ncol=m)
  xi.k = matrix(nrow=l, ncol=n)
  x.jk = matrix(nrow=m, ncol=n)

  for (i in 1:l) xi..[i] = mean(Tbl[Tbl[,1]==i,4])
  for (j in 1:m) x.j.[j] = mean(Tbl[Tbl[,2]==j,4])
  for (k in 1:n) x..k[k] = mean(Tbl[Tbl[,3]==k,4])
  for (i in 1:l) for (j in 1:m) xij.[i,j] = mean(Tbl[Tbl[,1]==i & Tbl[,2]==j,4])
  for (i in 1:l) for (k in 1:n) xi.k[i,k] = mean(Tbl[Tbl[,1]==i & Tbl[,3]==k,4])
  for (j in 1:m) for (k in 1:n) x.jk[j,k] = mean(Tbl[Tbl[,2]==j & Tbl[,3]==k,4])

  x... = mean(xijk)
  SST = sum((xijk - x...)^2)
  SSA = m*n*sum((xi.. - x...)^2)
  SSB = l*n*sum((x.j. - x...)^2)
  SSC = l*m*sum((x..k - x...)^2)
  SSAB = n*sum((xij. - x...)^2)
  SSAC = m*sum((xi.k - x...)^2)
  SSBC = l*sum((x.jk - x...)^2)
  SSAxB = SSAB - SSA - SSB
  SSAxC = SSAC - SSA - SSC
  SSBxC = SSBC - SSB - SSC
  SSAxBxC = SST - (SSA + SSB + SSC + SSAxB + SSAxC + SSBxC)
  SSE = SSAxBxC

  df.A = l - 1
  df.B = m - 1
  df.C = n - 1
  df.AxB = (l - 1)*(m - 1)
  df.AxC = (l - 1)*(n - 1)
  df.BxC = (m - 1)*(n - 1)
  df.E   = (l - 1)*(m - 1)*(n - 1)
  df.T   = l*m*n - 1

  SS = c(SSA, SSB, SSC, SSAxB, SSAxC, SSBxC, SSE, SST)
  phi = c(df.A, df.B, df.C, df.AxB, df.AxC, df.BxC, df.E, df.T)
  MS = SS/phi
  MSE = MS[7]

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("Factor A", "Factor B", "Factor C", "A x B", "A x C", "B x C", "Error", "Total")
  rownames(Res1) = RowNameList
  F.val = MS[1:6]/MSE
  F.crt = qf(1 - alpha, phi[1:6], df.E)
  p.val = 1 - pf(F.val, phi[1:6], df.E)

  Res2 = cbind(F.val, F.crt, p.val)
  rownames(Res2) = RowNameList[1:6]

  xijk.hat = matrix(nrow=l*m*n, ncol=7)
  alpha = 0.05
  t.cut = qt(1 - alpha/2, df.E)
  CurRow = 1
  for(i in 1:l) {
    for(j in 1:m) {
      for(k in 1:n) {
        xijk.hat[CurRow, 1] = i
        xijk.hat[CurRow, 2] = j
        xijk.hat[CurRow, 3] = k
        xijk.hat[CurRow, 4] = xij.[i,j] + xi.k[i,k] + x.jk[j,k] - xi..[i] - x.j.[j] - x..k[k] + x...
        xijk.hat[CurRow, 5] = 1/(1/n + 1/l - 1/(l*n))
        xijk.hat[CurRow, 6] = xijk.hat[CurRow, 4] - t.cut*sqrt(MSE/xijk.hat[CurRow, 5]) 
        xijk.hat[CurRow, 7] = xijk.hat[CurRow, 4] + t.cut*sqrt(MSE/xijk.hat[CurRow, 5])
        CurRow = CurRow + 1
      }
    }
  }
  colnames(xijk.hat) = c("A", "B", "C", "Expection", "Effective Replication (ne)", "LL", "UL")
  Res = list(Res1, Res2, xij., xi.k, x.jk, xijk.hat)
  names(Res) = c("Sum of Squares", "F Test for Fixed Factors", "Means for (Ai,Bj)", "Means for (Ai,Ck)", "Means for (Bj,Ck)", "Expectations for (Ai,Bj,Ck)")
  return(Res)
}
