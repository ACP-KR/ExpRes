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

# Example 6.1 Table 6.5 p170
vData = c(74, 86, 76, 72, 91, 87, 48, 65, 56,
           61, 78, 71, 62, 81, 77, 55, 72, 63,
           50, 70, 60, 49, 68, 64, 52, 69, 60)

E6.1 = cbind(vFac1, vFac2, vFac3, vData)
colnames(E6.1) = c("A", "B", "C", "Y")
write.csv(E6.1, "E:/Rt/ExpDes/E6.1.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E6.1.CSV")

# Table 6.13 계수형 자료, One-Way
l = 4
r = 200
vFac1 = sort(rep(1:l, r))
vFac2 = rep(1:r, l)
vData = c(rep(0, 190), rep(1, 10), rep(0, 178), rep(1, 22), rep(0, 194), rep(1, 6), rep(0, 170), rep(1, 30))
T6.13 = cbind(vFac1, vFac2, vData)
colnames(T6.13) = c("A", "R", "Y")
write.csv(T6.13, "E:/Rt/ExpDes/T6.13.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T6.13.CSV")

# Table 6.14 계수형자료, Two-Way
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
colnames(T6.14) = c("A", "B", "R", "Y")
write.csv(T6.14, "E:/Rt/ExpDes/T6.14.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T6.14.CSV")



