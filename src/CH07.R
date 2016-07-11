#Chapter 7

# Shortest solution first

# Randomization Table 7.3

set.seed(100)
l = 3
m = 2
n = 3
r = 2

vFct1 = sort(rep(1:r, l*m))
vFct2 = rep(rep(1:l,m), r)
vFct3 = rep(rep(1:m,l), r)

vRan1 = vector()
for (p in 1:r) {
  vRan1 = c(vRan1, sample(l*m))
}

RanTbl1 = cbind(rep(vFct1, n), rep(vFct2, n), rep(vFct3, n), rep(vRan1, n))
RanTbl1 = RanTbl1[order(RanTbl1[,1],RanTbl1[,4]),]

vRan2 = vector()
for (i in 1:(l*m)) {
  vRan2 = c(vRan2, sample(1:n))
}

RanTbl2 = cbind(RanTbl1, vRan2)
colnames(RanTbl2) = c("R","A","B","Ran1","C")
RanTbl2[,-4]


# Table 7.4
l = 3
m = 2
n = 3
r = 2

vData = c(88, 90, 89, 91, 92, 91, 84, 85, 91, 90, 91, 92, 79, 82, 86, 87, 89, 92,
           86, 89, 91, 90, 91, 90, 87, 86, 92, 90, 89, 90, 81, 80, 87, 87, 88, 89)

vFac1 = sort(rep(1:r,(l*m*n)))
vFac2 = rep(sort(rep(1:l,(m*n))), r)
vFac3 = rep(rep(sort(rep(1:m,l)),n),r)
vFac4 = rep(rep(1:n),l*m*r)

Tbl0 = cbind(vFac1, vFac2, vFac3, vFac4)
Tbl0 = Tbl0[order(Tbl0[,1],Tbl0[,4],Tbl0[,2],Tbl0[,3]),]

Tbl = cbind(Tbl0, vData)
alpha = 0.05

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

  MSE1 = MS[5]
  MSE2 = MS[10]
  F.val1 = MS[1:4]/MSE1
  F.val2 = MS[5:9]/MSE2
  F.crt1 = qf(1 - alpha, phi[1:4], df.E1)
  F.crt2 = qf(1 - alpha, phi[5:9], df.E2)
  p.val1 = 1 - pf(F.val1, phi[1:4], df.E1)
  p.val2 = 1 - pf(F.val2, phi[5:9], df.E2)

  Res2 = cbind(c(F.val1, F.val2), c(F.crt1, F.crt2), c(p.val1, p.val2))

  Res = list(Res1, Res2)

  return(Res)
}

T7.x(Tbl)

vData2 = c(1, -3, 18, 21, 20, 21, 3, 4, 21, 16, 18, 22, -1, 1, 19, 19, 23, 20,
          2, -2, 17, 18, 20, 16, 0, 3, 20, 21, 20, 19, 1, 0, 19, 16, 21, 20)

Tbl2 = cbind(Tbl0, vData2)

T7.x(Tbl2)

Res3 = aov(vData2 ~ as.factor(Tbl0[,2]) + as.factor(Tbl0[,3]) + as.factor(Tbl0[,4]))
Res3
summary(Res3)



# 7.6 인자가 분할이 안되는 경우 p213
#

vData3 = c(61.0, 60.2, 63.3, 62.7, 61.3, 61.9,
          64.1, 63.2, 66.2, 65.4, 63.2, 64.2,
          65.2, 66.1, 66.6, 67.2, 66.0, 66.4)

l = 3
m = 3
r = 2

vFac1 = sort(rep(1:l,m*r))
vFac2 = rep(sort(rep(1:m,r)),l)
vFac3 = rep(1:r,l*m)
Tbl0 = cbind(vFac1, vFac2, vFac3)

Tbl1 = cbind(Tbl0,vData3)
Tbl = Tbl1

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

p213(Tbl1)



# Example 7.1 p220

vData4 = c(55.30, 55.33, 55.53, 55.55, 55.04, 55.05, 55.22, 55.20,
            55.89, 55.82, 56.14, 56.12, 55.56, 55.54, 55.76, 55.84,
            55.35, 55.39, 55.59, 55.53, 55.10, 55.06, 55.29, 55.34,
            55.30, 55.38, 55.44, 55.45, 55.03, 54.94, 55.12, 55.15)

l = 4
m = 2
n = 2
r = 2

vFac1 = sort(rep(1:l, m*n*r))
vFac2 = rep(sort(rep(1:m, n*r)), l)
vFac3 = rep(sort(rep(1:n, r)), l*m)
vFac4 = rep(1:r, l*m*n)

Tbl = cbind(vFac1, vFac2, vFac3, vFac4, vData4)


e7.1 = function(Tbl, alpha=0.05)
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

  F.val = MS[1:3]/MS[2:4]
  F.crt = qf(1 - alpha, phi[1:3], phi[2:4])
  p.val = 1 - pf(F.val, phi[1:3], phi[2:4])

  Res2 = cbind(F.val, F.crt, p.val)
  
  MSE = MS[4]
  S2C.AB = (MS[3] - MS[4])/r
  S2B.A = (MS[2] - MS[3])/(n*r)
  S2A = (MS[1] - MS[2])/(m*n*r)
  
  Res3 = c(MSE, S2C.AB, S2B.A, S2A)

  Res = list(Res1, Res2, Res3)

  return(Res)
}

e7.1(Tbl)



