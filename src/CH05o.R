# Chapter 5

# Shortest solution first

# Table 5.5 Randomization
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

# Table 5.6
vData = c(305, 302, 335, 337, 366, 364, 372, 374, 376, 373, 348, 350,
           322, 325, 350, 348, 326, 324, 330, 330, 327, 330, 310, 308,
           320, 322, 342, 344, 338, 336, 348, 348, 350, 350, 330, 328)

T5.6 = cbind(vFac1, vFac2, vData)
T5.6

interaction.plot(vFac1, vFac2, vData)
interaction.plot(vFac2, vFac1, vData)

source("E:/Rt/ExpDes/RD4.R")
RD4(T5.6)

source("E:/Rt/ExpDes/a2wff.R")
a2wff(T5.6)

## Example 5.1 p150 반복수가 다른 일원배치

a1 = c(20, 18, 19, 17, 17, 22, 18, 13, 16, 15)
a2 = c(25, 23, 28, 26, 19, 26)
a3 = c(24, 25, 18, 22, 27, 24)
a4 = c(14, 12)
l = 4
m = c(length(a1), length(a2), length(a3), length(a4))

vData = c(a1, a2, a3, a4)
vFact = vector()
for (i in 1:l) {
  vFact = c(vFact, rep(i, m[i]))
}
vFact
E5.1 = cbind(vFact, vData)

source("E:/Rt/ExpDes/a1way.R")
a1way(E5.1)
