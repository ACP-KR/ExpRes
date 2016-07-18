# Two-Way ANOVA with no replication
# This can be used for one-random and one fixed factor ANOVA (randomized block design)

a2wnorpl = function(Tbl, alpha=0.05)
{
  xij = Tbl[,3]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  xi. = vector(length=l)
  x.j = vector(length=m)
  for (i in 1:l) xi.[i] = mean(Tbl[Tbl[,1]==i,3])
  for (j in 1:m) x.j[j] = mean(Tbl[Tbl[,2]==j,3])

  x.. = mean(xij)
  SST = sum((xij - x..)^2)
  SSA = m*sum((xi. - x..)^2)
  SSB = l*sum((x.j - x..)^2)
  SSE = SST - SSA - SSB

  df.T = l*m - 1
  df.A = l - 1
  df.B = m - 1
  df.E = (l - 1)*(m - 1)
  
  SS = c(SSA, SSB, SSE, SST)
  phi = c(df.A, df.B, df.E, df.T)
  MS = SS / phi

  Res1 = cbind(SS, phi, MS)

  MSE = MS[3]
  F.val = MS[1:2] / MSE
  F.crt = qf(1 - alpha, phi[1:2], df.E)
  p.val = 1 - pf(F.val, phi[1:2], df.E)

  Res2 = cbind(F.val, F.crt, p.val)
  colnames(Res2) = c("F", "F(0.05)", "p-value")
       
  t.crt <<- qt(1 - alpha/2, df.E)
  LL.a = xi. - t.crt*sqrt(MSE/m)
  UL.a = xi. + t.crt*sqrt(MSE/m)
  LL.b = x.j - t.crt*sqrt(MSE/l)
  UL.b = x.j + t.crt*sqrt(MSE/l)

  Res3 = cbind(LL.a, xi., UL.a)
  Res4 = cbind(LL.b, x.j, UL.b)
  
  Res5 = matrix(nrow=l*m, ncol=5)
  colnames(Res5) = c("A", "B", "PE", "LL", "UL")
  CurRow = 1 
  ne = 1/(1/m + 1/l - 1/(l*m))
  SE = sqrt(MSE/ne)
  
  for (i in 1:l) {
    for (j in 1:m) {
      Res5[CurRow, 1] = i
      Res5[CurRow, 2] = j
      Res5[CurRow, 3] = xi.[i] + x.j[j] - x..
      Res5[CurRow, 4] = Res5[CurRow, 3] - t.crt * SE
      Res5[CurRow, 5] = Res5[CurRow, 3] + t.crt * SE
      CurRow = CurRow + 1
    }
  }

  t.tukey = qtukey(1 - alpha, l, df.E)/sqrt(2)
  
  PDIFF.a = matrix(ncol=9, nrow=l*(l - 1)/2)
  colnames(PDIFF.a) = c("Ai","Aj","PE","LL","UL", "Zero", "Tukey LL", "Tukey UL", "Zero2")
  SE = sqrt(MSE*(2/m))
  CurRow = 1
  for(i in 1:(l-1)){
    for(j in (i+1):l) {
      PDIFF.a[CurRow, 1] = i
      PDIFF.a[CurRow, 2] = j
      PDIFF.a[CurRow, 3] = xi.[i] - xi.[j]
      PDIFF.a[CurRow, 4] = xi.[i] - xi.[j] - t.crt*SE
      PDIFF.a[CurRow, 5] = xi.[i] - xi.[j] + t.crt*SE
      if (PDIFF.a[CurRow, 4]*PDIFF.a[CurRow, 5] > 0) PDIFF.a[CurRow, 6] = 1 
      else PDIFF.a[CurRow, 6] = 0
      PDIFF.a[CurRow, 7] = xi.[i] - xi.[j] - t.tukey*SE
      PDIFF.a[CurRow, 8] = xi.[i] - xi.[j] + t.tukey*SE
      if (PDIFF.a[CurRow, 7]*PDIFF.a[CurRow, 8] > 0) PDIFF.a[CurRow, 9] = 1 
      else PDIFF.a[CurRow, 9] = 0
      CurRow = CurRow + 1
    }  
  }

  PDIFF.b = matrix(ncol=9, nrow=m*(m - 1)/2)
  colnames(PDIFF.b) = c("Bi", "Bj", "PE", "LL", "UL", "Zero", "Tukey LL", "Tukey UL", "Zero2")
  SE = sqrt(MSE*(2/l))
  CurRow = 1
  for(i in 1:(m-1)){
    for(j in (i+1):m) {
      PDIFF.b[CurRow, 1] = i
      PDIFF.b[CurRow, 2] = j
      PDIFF.b[CurRow, 3] = x.j[i] - x.j[j]
      PDIFF.b[CurRow, 4] = x.j[i] - x.j[j] - t.crt*SE
      PDIFF.b[CurRow, 5] = x.j[i] - x.j[j] + t.crt*SE
      if (PDIFF.b[CurRow, 4]*PDIFF.b[CurRow, 5] > 0) PDIFF.b[CurRow, 6] = 1 
      else PDIFF.b[CurRow, 6] = 0
      PDIFF.b[CurRow, 7] = x.j[i] - x.j[j] - t.tukey*SE
      PDIFF.b[CurRow, 8] = x.j[i] - x.j[j] + t.tukey*SE
      if (PDIFF.b[CurRow, 7]*PDIFF.b[CurRow, 8] > 0) PDIFF.b[CurRow, 9] = 1 
      else PDIFF.b[CurRow, 9] = 0
      CurRow = CurRow + 1
    }  
  }

  Res6 = PDIFF.a
  Res7 = PDIFF.b
  
  v2.a = (MS[2] + (l - 1)*MSE)/(l*m)
  v2.b = (MS[1] + (m - 1)*MSE)/(l*m)
  df.as = (MS[2] + (l - 1)*MSE)^2 / (MS[2]^2/df.B + ((l - 1)*MSE)^2/df.E)   # Satterthwaite
  df.bs = (MS[1] + (m - 1)*MSE)^2 / (MS[1]^2/df.B + ((m - 1)*MSE)^2/df.E) 
  
  t.crt.a = qt(1 - alpha/2, df.as)  # Factor B as Random Factor
  LL.a2 = xi. - t.crt.a * sqrt(v2.a)
  UL.a2 = xi. + t.crt.a * sqrt(v2.a)

  t.crt.b = qt(1 - alpha/2, df.bs)  # Factor A as Random Factor
  LL.b2 = x.j - t.crt.b * sqrt(v2.b)
  UL.b2 = x.j + t.crt.b * sqrt(v2.b)

  Res8 = cbind(c(v2.a, v2.b), c(df.as, df.bs))
  colnames(Res8) = c("s2", "degree of freedom")
  rownames(Res8) = c("A", "B")
  Res9 = cbind(LL.a2, xi., UL.a2)
  Res10 = cbind(LL.b2, x.j, UL.b2)
  
  Res = list(Res1, Res2, Res3, Res4, Res5, Res6, Res7, Res8, Res9, Res10)
  names(Res) = c("SS", "F", "Means: A", "Means: B", "(Ai, Bj)", "Pairwise Difference of A", "Pairwise Difference of B", "Estimation for S2A, S2B", "In case of B as Random Factor", "In case of A as random factor")
  
  return(Res)
}
