# RD4 : Same variance test for Two-way ANOVA See ¹Ú¼ÍÇö Ã¥ p127

RD4 = function(Tbl)
{
  xijk = Tbl[,3]
  l = length(unique(Tbl[,1]))
  m = length(unique(Tbl[,2]))
  r = length(xijk) / (l*m)

  D4tab = cbind(2:8, c(3.267, 2.575, 2.282, 2.115, 2.004, 1.924, 1.864))
  D4 = D4tab[r - 1, 2]

  Rij  = matrix(nrow=l*m, ncol=3)
  CurRow = 1
  for (i in 1:l) {
    for(j in 1:m) {
      xij = Tbl[Tbl[,1]==i & Tbl[,2]==j,3]
      Rij[CurRow, 1] = i
      Rij[CurRow, 2] = j
      Rij[CurRow, 3] = max(xij) - min(xij)
      CurRow = CurRow + 1
    }
  }

  Rmax = max(Rij[,3])
  Rbar = mean(Rij[,3])
  colnames(Rij) = c("Row", "Column", "Range")
  Res = list(Rij, Rmax, Rbar, D4, Rbar*D4, Rmax < Rbar*D4)
  names(Res) = c("Range Table(Rij)", "Rmax", "Rbar", paste("D4 for Replicate =", r), "RBar*D4", "Controlled: Is Rmax is less than Rbar*D4?")
  return(Res)
}
