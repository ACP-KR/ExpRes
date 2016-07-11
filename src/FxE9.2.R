# Function for Example 9.2

FxE9.2 = function(Tbl, alpha=0.05)
{
  xijlm = Tbl[,5]
  k = length(unique(Tbl[,1]))

  xi... = vector(length=k)
  x.j.. = vector(length=k)
  x..l. = vector(length=k)
  x...m = vector(length=k)

  for(i in 1:k) xi...[i] = mean(Tbl[Tbl[,1]==i,5])
  for(j in 1:k) x.j..[j] = mean(Tbl[Tbl[,2]==j,5])
  for(l in 1:k) x..l.[l] = mean(Tbl[Tbl[,3]==l,5])
  for(m in 1:k) x...m[m] = mean(Tbl[Tbl[,4]==m,5])

  x.... = mean(xijlm)
  SST = sum((xijlm - x....)^2)
  SSA = k*sum((xi... - x....)^2)
  SSB = k*sum((x.j.. - x....)^2)
  SSC = k*sum((x..l. - x....)^2)
  SSD = k*sum((x...m - x....)^2)
  SSE = SST - SSA - SSB - SSC - SSD

  df.A = k - 1
  df.B = k - 1
  df.C = k - 1
  df.D = k - 1
  df.E = (k - 1)*(k - 3)
  df.T = k*k - 1

  SS = c(SSA, SSB, SSC, SSD, SSE, SST)
  phi = c(df.A, df.B, df.C, df.D, df.E, df.T)
  MS = SS/phi

  Res1 = cbind(SS, phi, MS)
  RowNameList = c("A", "B", "C", "D", "E", "T")
  rownames(Res1) = RowNameList

  MSE = MS[5]
  F.val = MS[1:4]/MSE
  F.crt = qf(1 - alpha, phi[1:4], df.E)
  p.val = 1 - pf(F.val, phi[1:4], df.E)

  Res2 = cbind(F.val, F.crt, p.val)
  colnames(Res2) = c("F", "F(0.05)", "p-value")
  rownames(Res2) = RowNameList[1:4]
  
  SDiff1 = qt(1 - alpha/2, df.E) * sqrt(MSE/k)
  SDiff2 = qt(1 - alpha/2, df.E) * sqrt(MSE*(2*k - 1)/(k*k))
  SDiff3 = qt(1 - alpha/2, df.E) * sqrt(MSE*(3*k - 2)/(k*k))
  SDiff4 = qt(1 - alpha/2, df.E) * sqrt(MSE*(4*k - 3)/(k*k))

  Res = list(Res1, Res2, xi..., x.j.., x..l., x...m, SDiff1, SDiff2, SDiff3, SDiff4)
  names(Res) = c("Sum of Squares", "F test", "Level Means of Factor A", "Level Means of Factor B", "Level Means of Factor C", "Level Means of Factor D", "SE for One Factor", "SE for Two Factors", "SE for Three Factors", "SE for Four Factors")
  return(Res)
}
