#Chapter 7
# Randomization Table 7.3 p203
set.seed(100)
l = 3
m = 2
n = 3
r = 2

vFct1 = sort(rep(1:r, l*m))
vFct2 = rep(rep(1:l,m), r)
vFct3 = rep(rep(1:m,l), r)

vRan1 = vector()
for (p in 1:r) vRan1 = c(vRan1, sample(l*m))

RanTbl1 = cbind(rep(vFct1, n), rep(vFct2, n), rep(vFct3, n), rep(vRan1, n))
RanTbl1 = RanTbl1[order(RanTbl1[,1],RanTbl1[,4]),]

vRan2 = vector()
for (i in 1:(l*m)) vRan2 = c(vRan2, sample(1:n))

RanTbl2 = cbind(RanTbl1, vRan2)
colnames(RanTbl2) = c("R","A","B","Ran1","C")
RanTbl2[,-4]

# Table 7.4 p203
T7.4 = read.csv("E:/Rt/ExpDes/T7.4.CSV")

source("E:/Rt/ExpDes/T7.x.R")
T7.x(T7.4)
summary(aov(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + Error(as.factor(R):as.factor(A):as.factor(B)), data=T7.4))
summary(aov(Y ~ as.factor(A)*as.factor(C) + as.factor(B), data=T7.4))

# Table 7.5 p204
T7.5 = read.csv("E:/Rt/ExpDes/T7.5.CSV")
T7.x(T7.5)
summary(aov(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + Error(as.factor(R):as.factor(A):as.factor(B)), data=T7.5))
summary(aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C), data=T7.5))


# 7.6 인자가 분할이 안되는 경우 p213
TP213 = read.csv("E:/Rt/ExpDes/TP213.CSV")

source("E:/Rt/ExpDes/p213.R")
p213(TP213)
summary(aov(Y ~ as.factor(A) + as.factor(B) + Error(as.factor(A):as.factor(B)), data=TP213))


# Example 7.1 p220
E7.1 = read.csv("E:/Rt/ExpDes/E7.1.CSV")

source("E:/Rt/ExpDes/FxE7.1.R")
FxE7.1(E7.1)



