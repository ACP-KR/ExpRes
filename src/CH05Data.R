# Chapter 5 Data Preparation

# Table 5.6 p135
vData = c(305, 302, 335, 337, 366, 364, 372, 374, 376, 373, 348, 350,
           322, 325, 350, 348, 326, 324, 330, 330, 327, 330, 310, 308,
           320, 322, 342, 344, 338, 336, 348, 348, 350, 350, 330, 328)

l = 3
m = 6
r = 2

vRaw = sample(1:(l*m*r))
vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m, r)),l)

T5.6 = cbind(vFac1, vFac2, vData)
colnames(T5.6) = c("A", "B", "Y")
write.csv(T5.6, "E:/Rt/ExpDes/T5.6.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/T5.6.CSV")


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
colnames(E5.1) = c("A", "Y")
write.csv(E5.1, "E:/Rt/ExpDes/E5.1.CSV", row.names=FALSE, quote=FALSE)
read.csv("E:/Rt/ExpDes/E5.1.CSV")



