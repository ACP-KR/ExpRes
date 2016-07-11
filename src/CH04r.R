#Chapter 4 반복이 없는 이원배치와 난괴법


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


# Example 4.1 반복이 없는 이원배치
T4.3 = read.csv("E:/Rt/ExpDes/T4.3.CSV") #p103
T4.3

res = lm(Y ~ as.factor(A) + as.factor(B), T4.3)
res
summary(res)
anova(res)

res2 = aov(Y ~ as.factor(A) + as.factor(B), T4.3)
res2
summary(res2)
model.tables(res2, type="means", se=TRUE)
model.tables(res2, type="effects", se=TRUE)
TukeyHSD(res2)

source("E:/Rt/ExpDes/a2wnorpl.R")
a2wnorpl(T4.3)

# Table 4.7 p114 난괴법 (One Way ANOVA and Block Randomization)
T4.7 = read.csv("E:/Rt/ExpDes/T4.7.CSV")
T4.7

res3 = aov(Y ~ as.factor(A) + Error(as.factor(B)), data=T4.7)
res3
summary(res3)

a2wnorpl(T4.7)

