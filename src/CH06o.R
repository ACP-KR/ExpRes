# Chapter 6
# Randomization 반복이 없는 3원배치
l = 3
m = 3
n = 3

vRan = sample(l*m*n)
vFac1 = sort(rep(1:l, m*n))
vFac2 = rep(sort(rep(1:l, m)), n)
vFac3 = rep(1:l, m*n)
RanTbl = cbind(vFac1, vFac2, vFac3, vRan)
RanTbl

# Example 6.1 p170
vData = c(74, 86, 76, 72, 91, 87, 48, 65, 56,
           61, 78, 71, 62, 81, 77, 55, 72, 63,
           50, 70, 60, 49, 68, 64, 52, 69, 60)

E6.1 = cbind(vFac1, vFac2, vFac3, vData)

source("E:/Rt/ExpDes/a3w.R")
a3w(E6.1)

# Table 6.14 계수형자료
l = 4
m = 2
r = 120

vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m, r)), l)
vFac3 = rep(1:r, l*m)
vData = c(rep(0,115), rep(1,5), rep(0,110), rep(1, 10),
           rep(0,108), rep(1,12), rep(0,100), rep(1, 20),
           rep(0,117), rep(1,3), rep(0,112), rep(1, 8),
           rep(0,100), rep(1,20), rep(0,98), rep(1, 22))

T6.14 = cbind(vFac1, vFac2, vFac3, vData)

source("E:/Rt/ExpDes/p213.R")
p213(T6.14)

# Compare with Chapter 5 a2wff(Tbl) result
source("E:/Rt/ExpDes/a2wff.R")
a2wff(T6.14[,-3])

