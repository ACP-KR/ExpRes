#Chapter 7
# Randomization Table 7.3
set.seed(100)
l = 3
m = 2
n = 3
r = 2

vFct1 = sort(rep(1:r, l*m))
vFct2 = rep(rep(1:l,m), r)
vFct3 = rep(rep(1:m,l), r)

vRan1 = vector()
for (p in 1:r) {
  vRan1 = c(vRan1, sample(l*m))
}

RanTbl1 = cbind(rep(vFct1, n), rep(vFct2, n), rep(vFct3, n), rep(vRan1, n))
RanTbl1 = RanTbl1[order(RanTbl1[,1],RanTbl1[,4]),]

vRan2 = vector()
for (i in 1:(l*m)) {
  vRan2 = c(vRan2, sample(1:n))
}

RanTbl2 = cbind(RanTbl1, vRan2)
colnames(RanTbl2) = c("R","A","B","Ran1","C")
RanTbl2[,-4]


# Table 7.4
l = 3
m = 2
n = 3
r = 2

vData = c(88, 90, 89, 91, 92, 91, 84, 85, 91, 90, 91, 92, 79, 82, 86, 87, 89, 92,
           86, 89, 91, 90, 91, 90, 87, 86, 92, 90, 89, 90, 81, 80, 87, 87, 88, 89)

vFac1 = sort(rep(1:r,(l*m*n)))
vFac2 = rep(sort(rep(1:l,(m*n))), r)
vFac3 = rep(rep(sort(rep(1:m,l)),n),r)
vFac4 = rep(rep(1:n),l*m*r)

Tbl0 = cbind(vFac1, vFac2, vFac3, vFac4)
Tbl0 = Tbl0[order(Tbl0[,1],Tbl0[,4],Tbl0[,2],Tbl0[,3]),]

Tbl = cbind(Tbl0, vData)
alpha = 0.05

source("E:/Rt/ExpDes/T7.x.R")
T7.x(Tbl)

vData2 = c(1, -3, 18, 21, 20, 21, 3, 4, 21, 16, 18, 22, -1, 1, 19, 19, 23, 20,
          2, -2, 17, 18, 20, 16, 0, 3, 20, 21, 20, 19, 1, 0, 19, 16, 21, 20)

Tbl2 = cbind(Tbl0, vData2)

T7.x(Tbl2)

Res3 = aov(vData2 ~ as.factor(Tbl0[,2]) + as.factor(Tbl0[,3]) + as.factor(Tbl0[,4]))
Res3
summary(Res3)



# 7.6 인자가 분할이 안되는 경우 p213

vData3 = c(61.0, 60.2, 63.3, 62.7, 61.3, 61.9,
          64.1, 63.2, 66.2, 65.4, 63.2, 64.2,
          65.2, 66.1, 66.6, 67.2, 66.0, 66.4)

l = 3
m = 3
r = 2

vFac1 = sort(rep(1:l,m*r))
vFac2 = rep(sort(rep(1:m,r)),l)
vFac3 = rep(1:r,l*m)
Tbl0 = cbind(vFac1, vFac2, vFac3)

Tbl1 = cbind(Tbl0,vData3)
Tbl = Tbl1

source("E:/Rt/ExpDes/p213.R")
p213(Tbl1)


# Example 7.1 p220

vData4 = c(55.30, 55.33, 55.53, 55.55, 55.04, 55.05, 55.22, 55.20,
            55.89, 55.82, 56.14, 56.12, 55.56, 55.54, 55.76, 55.84,
            55.35, 55.39, 55.59, 55.53, 55.10, 55.06, 55.29, 55.34,
            55.30, 55.38, 55.44, 55.45, 55.03, 54.94, 55.12, 55.15)

l = 4
m = 2
n = 2
r = 2

vFac1 = sort(rep(1:l, m*n*r))
vFac2 = rep(sort(rep(1:m, n*r)), l)
vFac3 = rep(sort(rep(1:n, r)), l*m)
vFac4 = rep(1:r, l*m*n)

Tbl = cbind(vFac1, vFac2, vFac3, vFac4, vData4)


source("E:/Rt/ExpDes/FxE7.1.R")
FxE7.1(Tbl)



