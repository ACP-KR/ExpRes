# Chapter 5
# Table 5.5 p135 Randomization
l = 3
m = 6
r = 2

vRaw = sample(1:(l*m*r))
vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m, r)),l)
RanTbl = cbind(vFac1, vFac2, vRaw)

RanTbl
RanTbl[order(vFac1, vFac2, vRaw),]
RanTbl[order(vRaw),]

# Table 5.6 p135
T5.6 = read.csv("E:/Rt/ExpDes/T5.6.CSV")
T5.6
attach(T5.6)

windows()
interaction.plot(A, B, Y)
windows()
interaction.plot(B, A, Y)

source("E:/Rt/ExpDes/RD4.R")
RD4(T5.6)

source("E:/Rt/ExpDes/a2wff.R")
a2wff(T5.6)

summary(aov(Y ~ as.factor(A) * as.factor(B), data=T5.6))

source("E:/Rt/ExpDes/a2wvr.R")
a2wvr(T5.6)


## Example 5.1 p150 반복수가 다른 일원배치
E5.1 = read.csv("E:/Rt/ExpDes/E5.1.CSV")
source("E:/Rt/ExpDes/a1way.R")
a1way(E5.1)


# Testing for Unequal Count
T5.6b = T5.6[-1,]
summary(aov(Y ~ as.factor(B) * as.factor(A), data=T5.6b)) # THis gives Type I SS in SAS PROC GLM
summary(aov(Y ~ as.factor(A) * as.factor(B), data=T5.6b)) # THis gives Type I SS in SAS PROC GLM
source("E:/Rt/ExpDes/a2wvr.R")                           # This gives Type III SS in SAS PROC GLM
a2wvr(T5.6b) 

# Do not use the following
res2 = lm(Y ~ as.factor(A)*as.factor(B), data=T5.6b)
anova(res2)
library(car)
Anova(res2, type=3)



#################################
attach(T5.6b)
T5.6f = data.frame(A = factor(A), B = factor(B), Y=Y)
detach(T5.6b)
T5.6f
replications(T5.6f)
summary(res1 = aov(Y~A*B, data=T5.6f))

#library(nlme)
#summary(lme(Y ~ A + B + A:B, data=T5.6f))

# Example in Neter book

NT23.1 = read.csv("E:/Rt/ExpDes/NT23.1.CSV")
a2wvr(NT23.1) 

summary(aov(Y ~ as.factor(A) * as.factor(B), data=NT23.1))

#library(nlme)
#summary(lme(Y ~ as.factor(A) * as.factor(B), data=NT23.1))


