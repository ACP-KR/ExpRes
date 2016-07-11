# Chapter 11 2n형 교락법
# Example 11.1 p348
vData = c(0,0,0,0,1,82,
           0,1,1,0,1,55,
           1,1,0,1,1,88,
           1,0,1,1,1,81,
           0,0,0,1,2,80,
           1,1,0,0,2,85,
           1,0,1,0,2,84,
           0,1,1,1,2,84,
           1,0,0,0,3,76,
           0,1,0,1,3,73,
           0,0,1,1,3,72,
           1,1,1,0,3,74,
           0,0,1,0,4,71,
           0,1,0,0,4,79,
           1,0,0,1,4,79,
           1,1,1,1,4,89)

k = 4
b = 4
r = 1

E11.1 = matrix(vData, nrow=(2^k), ncol=(k+2), byrow=TRUE)
colnames(E11.1) = c("A","B","C","D","BL","Y")
write.csv(E11.1, "E:/Rt/ExpDes/E11.1.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E11.1.CSV")


# Example 11.2 p353, Table 11.6 p353
vData2 = c(1,0,0,1,1,50,
           0,1,0,1,1,67,
           1,1,1,1,1,82,
           0,0,1,1,1,62,
           1,1,0,2,1,78,
           1,0,1,2,1,82,
           0,1,1,2,1,70,
           0,0,0,2,1,38,

           0,0,0,3,2,40,
           0,1,0,3,2,51,
           1,0,1,3,2,74,
           1,1,1,3,2,61,
           1,0,0,4,2,69,
           0,0,1,4,2,72,
           0,1,1,4,2,91,
           1,1,0,4,2,59,

           0,1,1,5,3,82,
           1,0,0,5,3,74,
           0,0,0,5,3,41,
           1,1,1,5,3,67,
           1,1,0,6,3,40,
           0,0,1,6,3,55,
           1,0,1,6,3,79,
           0,1,0,6,3,60)

k = 3
b = 2
r = 3

T11.6 = matrix(vData2, nrow=r*2^k, ncol=(k+3), byrow=TRUE)
colnames(T11.6) = c("A","B","C","BL","R","Y")
write.csv(T11.6, "E:/Rt/ExpDes/T11.6.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T11.6.CSV")


# Example 11.3 p360
vData3 = c(0,0,1,1,
           1,1,1,4,
           2,2,1,2,
           1,0,2,-2,
           2,1,2,1,
           0,2,2,2,
           2,0,3,3,
           0,1,3,0,
           1,2,3,-1)

E11.3 = matrix(vData3, nrow=9, ncol=4, byrow=TRUE)
colnames(E11.3) = c("A","B","BL","Y")
write.csv(E11.3, "E:/Rt/ExpDes/E11.3.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E11.3.CSV")


