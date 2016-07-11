# Chapter 9
# Randomization Table 9.4 p281
set.seed(1)
k = 3
vFac1 = sort(rep(1:k, k*k))
vFac2 = rep(sort(rep(1:k, k)), k)
vFac3 = rep(1:k, k*k)
vRan = sample(k*k*k)

RanTbl = cbind(vFac1, vFac2, vFac3, vRan)
RanTbl
RanTbl[order(RanTbl[,4]),]

# Example 9.1 (p288), Table 9.6 (p289)
T9.6 = read.csv("E:/Rt/ExpDes/T9.6.CSV")

source("E:/Rt/ExpDes/FxE9.1.R")
FxE9.1(T9.6)

# Example 9.2 (p296), Table 9.11 (p297) 
T9.11 = read.csv("E:/Rt/ExpDes/T9.11.CSV")

source("E:/Rt/ExpDes/FxE9.2.R")
FxE9.2(T9.11)




