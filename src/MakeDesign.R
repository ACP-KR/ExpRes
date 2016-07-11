
MakeDesign = function(l, m)
{
  Cells = l*m
  
  vA = sort(rep(1:l,m))
  vB = rep(1:m,l)
  rM = rep(1,Cells)
  rA = matrix(rep(0,Cells*(l-1)), nrow=Cells, ncol=(l-1))
  rB = matrix(rep(0,Cells*(m-1)), nrow=Cells, ncol=(m-1))
  rAB = matrix(nrow=Cells, ncol=(l-1)*(m-1))

  for (i in 1:(l-1)) rA[vA == i,i] = 1
  rA[vA == l,] = -1

  for (j in 1:(m-1)) rB[vB == j,j] = 1
  rB[vB == m,] = -1
  
  for (i in 1:(l-1)) for (j in 1:(m-1)) rAB[,(i-1)*(m-1)+j] = rA[,i]*rB[,j]

  return(cbind(rM, rA, rB, rAB))
}
