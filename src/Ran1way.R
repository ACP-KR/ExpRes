# Randomization Table 3.1 for One-way ANOVA (parallel group design)

Ran1way = function(nLvl, nRpl, LvlPrefix="A", RplPrefix="R")
{
  RawArr = sample(nLvl*nRpl)
  RawCol = cbind(rep(1:nLvl,nRpl), RawArr)
  RawCol1 = RawCol[order(RawCol[,2]),]
  RawCol2 = RawCol[order(RawCol[,1],RawCol[,2]),]

  RowNames = vector(length=nLvl*nRpl)
  for (i in 1:(nLvl*nRpl)) {
    RowNames[i] = paste(LvlPrefix, RawCol1[i,1], sep="")
  }
 
  colnames(RawCol1) = c("Level","Experiment Order")
  rownames(RawCol1) = RowNames
    
  RawMat = matrix(nrow=nRpl, ncol=nLvl, RawCol2[,2])

  LevelNames = vector()
  for (i in 1:nLvl) {
    LevelNames = c(LevelNames, paste(LvlPrefix, i, sep=""))
  }
  
  RowNames = vector()
  for (i in 1:nRpl) {
    RowNames = c(RowNames, paste(RplPrefix, i, sep=""))
  }
  colnames(RawMat) = LevelNames
  rownames(RawMat) = RowNames

  Res = list(RawCol1[,2], RawMat)
  names(Res) = c("Experiment Order Vector", "Exmperiment Order Matrix")
  return(Res)
}
