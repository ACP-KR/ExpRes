# Chapter 12

# Table 12.4 p382
vData = c(0,0,0,0,0,9,
           0,0,0,1,1,12,
           0,1,1,0,0,8,
           0,1,1,1,1,15,
           1,0,1,0,1,16,
           1,0,1,1,0,20,
           1,1,0,0,1,13,
           1,1,0,1,0,13)
T12.4 = matrix(vData, nrow=8, ncol=6, byrow=TRUE)
colnames(T12.4) = c("A", "B", "C", "D", "E", "Y")
write.csv(T12.4, "E:/Rt/ExpDes/T12.4.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T12.4.CSV")

