#Chapter 4 반복이 없는 이원배치와 난괴법

# Example 4.1, Table 4.3 p 103
vData = c(97.6, 97.3, 96.7, 98.6, 98.2, 96.9, 99.0, 98.0, 97.9, 98.0, 97.7, 96.5)
vFac1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
vFac2 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)

T4.3 = cbind(vFac1, vFac2, vData)
colnames(T4.3) = c("A", "B", "Y")
write.csv(T4.3, "E:/Rt/ExpDes/T4.3.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T4.3.CSV")


#
vData = c(13.1, 12.9, 13.4, 12.4, 12.7, 12.5, 12.3, 12.0, 12.2)
vFac1 = sort(rep(1:3,3))
vFac2 = rep(1:3,3)

T4.7 = cbind(vFac1, vFac2, vData)
colnames(T4.7) = c("A", "B", "Y")
write.csv(T4.7, "E:/Rt/ExpDes/T4.7.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T4.7.CSV")




