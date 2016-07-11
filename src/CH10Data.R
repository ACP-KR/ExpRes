# Chapter 10

# Example 10.1 (p313)
vData = c(31, 45, 46, 43, 22, 21, 18, 23, 82, 110, 88, 72, 30, 37, 38, 29)

l = 2
m = 2
r = 4
vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m,r)),l)
E10.1 = cbind(vFac1, vFac2, vData)
E10.1
colnames(E10.1) = c("A", "B", "Y")
write.csv(E10.1, "E:/Rt/ExpDes/E10.1.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E10.1.CSV")

# Example 10.2 (p319)
vData = c(72, 65, 85, 83, 58, 53, 68, 63)
l = 2
m = 2
n = 2

vFac1 = sort(rep(1:l, m*n))
vFac2 = rep(sort(rep(1:l, m)), n)
vFac3 = rep(1:l, m*n)

E10.2 = cbind(vFac1, vFac2, vFac3, vData)
colnames(E10.2) = c("A", "B", "C", "Y")
write.csv(E10.2, "E:/Rt/ExpDes/E10.2.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E10.2.CSV")

## Example 10.3 (p325), Table 10.9 (p326)
k = 2
r = 4

vFac1 = sort(rep(0:1, k*k*k))
vFac2 = rep(sort(rep(0:1, k*k)), k)
vFac3 = rep(sort(rep(0:1, k)), k*k)
vFac4 = rep(0:1, k*k*k)
vFac5 = sort(rep(1:r, k*k*k*k))

vRpl1 = c(32, 35, 29, 40, 26, 80, 36, 105, 47, 63, 51, 64, 61, 100, 76, 90)
vRpl2 = c(43, 42, 39, 44, 36, 68, 31, 99, 41, 41, 34, 39, 76, 68, 65, 82)
vRpl3 = c(27, 56, 27, 53, 24, 75, 32, 74, 48, 60, 40, 75, 56, 87, 70, 89)
vRpl4 = c(19, 35, 28, 36, 18, 67, 30, 73, 45, 53, 48, 72, 64, 66, 63, 101)

T10.9 = data.frame(rep(vFac1, r), rep(vFac2, r), rep(vFac3, r), rep(vFac4, r), vFac5, c(vRpl1, vRpl2, vRpl3, vRpl4))
colnames(T10.9) = c("A", "B", "C", "D", "R", "Y")
write.csv(T10.9, "E:/Rt/ExpDes/T10.9.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T10.9.CSV")
