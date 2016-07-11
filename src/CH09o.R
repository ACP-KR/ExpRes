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


# Example 9.1
vData = c(68, 64, 71, 71, 72, 74, 70, 80, 74, 80, 63, 65, 70, 69, 68,
           64, 58, 69, 66, 65, 70, 72, 76, 70, 78)
vFac3 = c(1,2,3,4,5, 2,3,4,5,1, 3,4,5,1,2, 4,5,1,2,3, 5,1,2,3,4)

k = 5
vFac1 = sort(rep(1:k, k))
vFac2 = rep(1:k, k)
T9.6 = cbind(vFac1, vFac2, vFac3, vData)

source("E:/Rt/ExpDes/FxE9.1.R")
FxE9.1(T9.6)


# Example 9.2
vData = c(1,1,2,3,15,
  1,2,4,1,5,
  1,3,1,4,15,
  1,4,3,2,19,
  2,1,1,1,4,
  2,2,3,3,19,
  2,3,2,2,16,
  2,4,4,4,26,
  3,1,3,4,8,
  3,2,1,2,9,
  3,3,4,3,19,
  3,4,2,1,14,
  4,1,4,2,19,
  4,2,2,4,16,
  4,3,3,1,17,
  4,4,1,3,34)

T9.11 = matrix(vData, nrow=16, ncol=5, byrow=TRUE)
T9.11

source("E:/Rt/ExpDes/FxE9.2.R")
FxE9.2(T9.11)




