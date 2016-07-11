# Function for Table 7.x

T7.x = function(Tbl, alpha=0.05)
{
  l = length(unique(Tbl[,2]))
  m = length(unique(Tbl[,3]))
  n = length(unique(Tbl[,4]))
  r = length(unique(Tbl[,1]))

  xijkp = Tbl[,5]
  x.... = mean(xijkp)

  xi... = vector(length=l)
  x.j.. = vector(length=m)
  x..k. = vector(length=n)
  x...p = vector(length=r)

  xij.. = matrix(nrow=l, ncol=m)
  xi.k. = matrix(nrow=l, ncol=n)
  x.jk. = matrix(nrow=m, ncol=n)

  xijk. = matrix(nrow=l*m*n, ncol=4)
  xij.p = matrix(nrow=l*m*r, ncol=4)

  for(i in 1:l) xi...[i] = mean(Tbl[Tbl[,2]==i,5])
  for(j in 1:m) x.j..[j] = mean(Tbl[Tbl[,3]==j,5])
  for(k in 1:n) x..k.[k] = mean(Tbl[Tbl[,4]==k,5])
  for(p in 1:r) x...p[p] = mean(Tbl[Tbl[,1]==p,5])

  for(i in 1:l) for(j in 1:m) xij..[i,j] = mean(Tbl[Tbl[,2]==i & Tbl[,3]==j, 5])
  for(i in 1:l) for(k in 1:n) xi.k.[i,k] = mean(Tbl[Tbl[,2]==i & Tbl[,4]==k, 5])
  for(j in 1:m) for(k in 1:n) x.jk.[j,k] = mean(Tbl[Tbl[,3]==j & Tbl[,4]==k, 5])

  CurRow = 1
  for(i in 1:l)
    for(j in 1:m)
      for(k in 1:n) {
        xijk.[CurRow, 1] = i
        xijk.[CurRow, 2] = j
        xijk.[CurRow, 3] = k
        xijk.[CurRow, 4] = mean(Tbl[Tbl[,2]==i & Tbl[,3]==j & Tbl[,4]==k, 5])
        CurRow = CurRow + 1
      }

  CurRow = 1
  for(i in 1:l)
    for(j in 1:m)
      for(p in 1:r) {
        xij.p[CurRow, 1] = i
        xij.p[CurRow, 2] = j
        xij.p[CurRow, 3] = p
        xij.p[CurRow, 4] = mean(Tbl[Tbl[,2]==i & Tbl[,3]==j & Tbl[,1]==p, 5])
        CurRow = CurRow + 1
      }

  SST = sum((xijkp - x....)^2)
  SSA = m*n*r*sum((xi... - x....)^2)
  SSB = l*n*r*sum((x.j.. - x....)^2)
  SSC = l*m*r*sum((x..k. - x....)^2)
  SSR = l*m*n*sum((x...p - x....)^2)
  SSAB = n*r*sum((xij.. - x....)^2)
  SSAC = m*r*sum((xi.k. - x....)^2)
  SSBC = l*r*sum((x.jk. - x....)^2)
  SSABR = n*sum((xij.p[,4] - x....)^2)
  SSABC = r*sum((xijk.[,4] - x....)^2)

  SSAxB = SSAB - SSA - SSB
  SSAxC = SSAC - SSA - SSC
  SSBxC = SSBC - SSB - SSC
  SSE1 = SSAxBxR = SSABR - SSR - SSAB
  SSAxBxC = SSABC - SSA - SSB - SSC - SSAxB - SSAxC - SSBxC
  SSE2 = SST - SSABC - SSR - SSE1

  df.R = r - 1
  df.A = l - 1
  df.B = m - 1
  df.AxB = df.A * df.B
  df.E1 = (l*m - 1)*(r - 1)
  df.C = n - 1
  df.AxC = df.A * df.C
  df.BxC = df.B * df.C
  df.AxBxC = df.A * df.B * df.C
  df.E2 = l*m*(n - 1)*(r - 1)
  df.T = l*m*n*r - 1

  SS = c(SSR, SSA, SSB, SSAxB, SSE1, SSC, SSAxC, SSBxC, SSAxBxC, SSE2, SST)
  phi = c(df.R, df.A, df.B, df.AxB, df.E1, df.C, df.AxC, df.BxC, df.AxBxC, df.E2, df.T)
  MS = SS / phi

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("R", "A", "B", "AxB", "E1", "C", "AxC", "BxC", "AxBxC", "E2", "T")
  rownames(Res1) = RowNameList
  MSE1 = MS[5]
  MSE2 = MS[10]
  F.val1 = MS[1:4]/MSE1
  F.val2 = MS[5:9]/MSE2
  F.crt1 = qf(1 - alpha, phi[1:4], df.E1)
  F.crt2 = qf(1 - alpha, phi[5:9], df.E2)
  p.val1 = 1 - pf(F.val1, phi[1:4], df.E1)
  p.val2 = 1 - pf(F.val2, phi[5:9], df.E2)

  Res2 = cbind(c(F.val1, F.val2), c(F.crt1, F.crt2), c(p.val1, p.val2))
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:9]
  Res = list(Res1, Res2)
  names(Res) = c("Sum of Squares", "F test")
  return(Res)
}
