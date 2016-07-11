#Chapter 3 ?Ͽ???ġ

# Table 3.1 p64
source("Ran1way.R")
Ran1way(nLvl=4, nRpl=5)

##########
# Example 3.1
nLvl = 4
nRpl = 3

LvlPrefix = "A"
RplPrefix = "R"
LevelNames = vector()
for (i in 1:nLvl) LevelNames = c(LevelNames, paste(LvlPrefix, i, sep=""))

RowNames = vector()
for (i in 1:nRpl) RowNames = c(RowNames, paste(RplPrefix, i, sep=""))

vData = c(8.44, 8.36, 8.28, 8.59, 8.91, 8.60, 9.34, 9.41, 9.69, 8.92, 8.92, 8.74)

Tbl = T3.5 = matrix(nrow=nRpl, ncol=nLvl, vData)
Tbl
colnames(Tbl) = LevelNames
rownames(Tbl) = RowNames

l = nLvl = dim(Tbl)[2]
m = nRpl = dim(Tbl)[1]

GTot = sum(Tbl)
GMean = mean(Tbl)

Tot = colSums(Tbl)
Bar = colMeans(Tbl)

SST = sum((Tbl - GMean)^2)
SSA = nRpl*sum((Bar - GMean)^2)
SSE = SST - SSA
SS = c(SSA, SSE, SST)

df.A = l - 1
df.E = l*(m - 1)
df.T = l*m - 1

dfs = c(df.A, df.E, df.T)

MS = SS/dfs
MS

F.val = MS[1]/MS[2]
p.val = 1 - pf(F.val, df.A, df.E)
F.val
p.val
alpha = 0.05
F.crt = qf(1 - alpha, df.A, df.E)
F.crt

MSE = MS[2]

LL = Bar - qt(1 - alpha/2, df.E)*sqrt(MSE/m)
UL = Bar + qt(1 - alpha/2, df.E)*sqrt(MSE/m)
cbind(LL, Bar, UL)


library(gplots)
plotCI(Bar, uiw=sqrt(MSE/m), type="b", ylab="Strength", xlab="Temperature", xaxt="n", ylim=c(8,10))
axis(side=1, at=1:4, labels=colnames(Tbl), cex=0.7)


V.diff = 2*MSE/m
t.crt = qt(1 - alpha/2, df.E)
LSD = t.crt * sqrt(V.diff)

PDIFF = matrix(ncol=6,nrow=(nLvl*(nLvl-1)/2))
CurRow = 1
for(i in 1:nLvl){
  for(j in (i+1):nLvl) {
    PDIFF[CurRow, 1] = i
    PDIFF[CurRow, 2] = j
    PDIFF[CurRow, 3] = Bar[i] - Bar[j]
    PDIFF[CurRow, 4] = Bar[i] - Bar[j] - LSD
    PDIFF[CurRow, 5] = Bar[i] - Bar[j] + LSD
    if (PDIFF[CurRow, 4]*PDIFF[CurRow, 5] > 0) PDIFF[CurRow, 6] = 1 
    else PDIFF[CurRow, 6] = 0
    CurRow = CurRow + 1
  }  
}

PDIFF


## Using R lm

cLvl = sort(rep(LevelNames, nRpl))

T3.5L = data.frame(cLvl, vData)
T3.5L

Res1 = lm(vData ~ cLvl, T3.5L)
Res2 = lm(vData ~ cLvl - 1, T3.5L)
Res3 = aov(vData ~ cLvl, T3.5L)
anova(Res1)
summary(Res2)
TukeyHSD(Res3)


# Improved coding

vData = c(8.44, 8.36, 8.28, 8.59, 8.91, 8.60, 9.34, 9.41, 9.69, 8.92, 8.92, 8.74)
vLevel = sort(rep(1:4,3))
source("a1way.R")
a1way(cbind(vLevel, vData))
TukeyHSD(aov(vData ~ as.factor(vLevel)))

vData2 = c(49, 73, 58, 38, 42, 31, 40, 43, 44, 34, 20, 46, 41, 58, 31, 65, 45, 73, 76)
vLevel2 = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4)

source("a1way.R")
Res = a1way(cbind(vLevel2, vData2))
Res

library(gplots)
plotCI(Res[[3]][,2], uiw=(Res[[3]][,2]-Res[[3]][,1])/t.crt, type="b", xaxt="n", ylim=c(20,80))
axis(side=1, at=1:l, cex=0.7)

