#Chapter 7
# Table 7.4 p203
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

T7.4 = cbind(Tbl0, vData)
colnames(T7.4) = c("R", "A", "B", "C", "Y")
write.csv(T7.4, "E:/Rt/ExpDes/T7.4.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T7.4.CSV")

# Table 7.5 p204
vData2 = c(1, -3, 18, 21, 20, 21, 3, 4, 21, 16, 18, 22, -1, 1, 19, 19, 23, 20,
          2, -2, 17, 18, 20, 16, 0, 3, 20, 21, 20, 19, 1, 0, 19, 16, 21, 20)

T7.5 = cbind(Tbl0, vData2)
colnames(T7.5) = c("R", "A", "B", "C", "Y")
write.csv(T7.5, "E:/Rt/ExpDes/T7.5.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T7.5.CSV")


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

TP213 = cbind(Tbl0,vData3)
colnames(TP213) = c("A", "B", "R", "Y")
write.csv(TP213, "E:/Rt/ExpDes/TP213.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/TP213.CSV")


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

E7.1 = cbind(vFac1, vFac2, vFac3, vFac4, vData4)
colnames(E7.1) = c("A", "B", "C", "R", "Y")
write.csv(E7.1, "E:/Rt/ExpDes/E7.1.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E7.1.CSV")




