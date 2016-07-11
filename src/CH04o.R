#Chapter 4 반복이 없는 이원배치와 난괴법

# Example 4.1
vData = c(97.6, 97.3, 96.7, 98.6, 98.2, 96.9, 99.0, 98.0, 97.9, 98.0, 97.7, 96.5)
vFac1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
vFac2 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
Tbl = cbind(vFac1, vFac2, vData)

source("E:/Rt/ExpDes/a2wnorpl.R")
a2wnorpl(Tbl)

# Table 4.7 p114 난괴법 (One Way ANOVA and Block Randomization)
vData = c(13.1, 12.9, 13.4, 12.4, 12.7, 12.5, 12.3, 12.0, 12.2)
vFac1 = sort(rep(1:3,3))
vFac2 = rep(1:3,3)
a2wnorpl(cbind(vFac1, vFac2, vData))

# Randomize 반복이 없는 이원배치
set.seed(100)
l = 4
m = 3
rRaw = sample(l*m)
RanTbl = cbind(sort(rep(1:l,m)), rep(1:l,m), rRaw)
RanTbl[order(RanTbl[,3]),]

RanTbl2 = matrix(nrow=m, ncol=l, rRaw)
RanTbl2

# Randomize 난괴법
set.seed(100)
l = 4  # Fixed Factor
m = 3  # Random factor
rRaw = vector(length=l*m)
for (i in 1:m) {
  rRaw[((i-1)*l+1):(i*l)] = sample(((i-1)*l+1):(i*l))
}
rRaw
RanTbl = cbind(sort(rep(1:m,l)), rep(1:l,m), rRaw)
RanTbl[order(RanTbl[,3]),]
RanTbl2 = matrix(nrow=l, ncol=m, rRaw)
RanTbl2


