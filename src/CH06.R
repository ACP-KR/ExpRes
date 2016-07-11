# Chapter 6

# Shortest solution first

# Randomization 반복이 없는 3원배치
l = 3
m = 3
n = 3

vRan = sample(l*m*n)
vFac1 = sort(rep(1:l, m*n))
vFac2 = rep(sort(rep(1:l, m)), n)
vFac3 = rep(1:l, m*n)
RanTbl = cbind(vFac1, vFac2, vFac3, vRan)
RanTbl


# Example 6.1 p170

vData = c(74, 86, 76, 72, 91, 87, 48, 65, 56,
           61, 78, 71, 62, 81, 77, 55, 72, 63,
           50, 70, 60, 49, 68, 64, 52, 69, 60)


Tbl = cbind(vFac1, vFac2, vFac3, vData)
alpha = 0.05

a3w = function(Tbl, alpha=0.05)
{
  xijk = Tbl[,4]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  n = length(unique(Tbl[,3]))

  xi.. = vector(length=l)
  x.j. = vector(length=m)
  x..k = vector(length=m)
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

  F.val = MS[1:6]/MSE
  F.crt = qf(1 - alpha, phi[1:6], df.E)
  p.val = 1 - pf(F.val, phi[1:6], df.E)

  Res2 = cbind(F.val, F.crt, p.val)

  xijk.hat = matrix(nrow=l*m*n, ncol=5)
  CurRow = 1
  for(i in 1:l) {
    for(j in 1:m) {
      for(k in 1:n) {
        xijk.hat[CurRow, 1] = i
        xijk.hat[CurRow, 2] = j
        xijk.hat[CurRow, 3] = k
        xijk.hat[CurRow, 4] = xij.[i,j] + xi.k[i,k] + x.jk[j,k] - xi..[i] - x.j.[j] - x..k[k] + x...
        xijk.hat[CurRow, 5] = l*m*n/(l*m + l*n + m*n - l - m - n + 1)
        CurRow = CurRow + 1
      }
    }
  }

  Res = list(Res1, Res2, xij., xi.k, x.jk, xijk.hat)

  return(Res)
}

a3w(Tbl)


# Table 6.14 계수형자료
l = 4
m = 2
r = 120

vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m, r)), l)
vFac3 = rep(1:r, l*m)
vData = c(rep(0,115), rep(1,5), rep(0,110), rep(1, 10),
           rep(0,108), rep(1,12), rep(0,100), rep(1, 20),
           rep(0,117), rep(1,3), rep(0,112), rep(1, 8),
           rep(0,100), rep(1,20), rep(0,98), rep(1, 22))

Tbl = cbind(vFac1, vFac2, vFac3, vData)
alpha = 0.05

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

  Res1 = cbind(SS, phi, MS)

  F.val1 = MS[1:2]/MSE1
  F.val2 = MSE1/MSE2
  F.crt1 = qf(1 - alpha, phi[1:2], df.E1)
  F.crt2 = qf(1 - alpha, df.E1, df.E2)
  p.val1 = 1 - pf(F.val1, phi[1:2], df.E1)
  p.val2 = 1 - pf(F.val2, df.E1, df.E2)


  Res2 = cbind(c(F.val1, F.val2), c(F.crt1, F.crt2), c(p.val1, p.val2))

  Res = list(Res1, Res2)

  return(Res)
}

p213(Tbl)

# Compare with Chapter 5 a2wff(Tbl) result

a2wff(Tbl[,-3])

