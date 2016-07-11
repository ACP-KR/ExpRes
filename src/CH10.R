# Chapter 10

# Example 10.1
vData = c(31, 45, 46, 43, 22, 21, 18, 23, 82, 110, 88, 72, 30, 37, 38, 29)

l = 2
m = 2
r = 4
vFac1 = sort(rep(1:l, m*r))
vFac2 = rep(sort(rep(1:m,r)),l)
Tbl = cbind(vFac1, vFac2, vData)
Tbl

# See Chapter 5 반복이 동일한 이원배치

windows()
interaction.plot(vFac1, vFac2, vData)
windows()
interaction.plot(vFac2, vFac1, vData)

RD4(Tbl)
a2wff(Tbl)


# Example 10.2
vData = c(72, 65, 85, 83, 58, 53, 68, 63)
l = 2
m = 2
n = 2

vFac1 = sort(rep(1:l, m*n))
vFac2 = rep(sort(rep(1:l, m)), n)
vFac3 = rep(1:l, m*n)

Tbl = cbind(vFac1, vFac2, vFac3, vData)

# Chapter 6 반복이 없는 3원배치법
a3w(Tbl)


## Example 10.3
k = 2
r = 4

vFac1 = sort(rep(0:1, k*k*k))
vFac2 = rep(sort(rep(0:1, k*k)), k)
vFac3 = rep(sort(rep(0:1, k)), k*k)
vFac4 = rep(0:1, k*k*k)
vFac5 = sort(rep(1:r, k*k*k*k))

#vData = c(121, 168, 123, 173, 104, 290, 129, 351, 181, 217, 173, 250, 257, 321, 274, 362)
#Tbl = cbind(vFac1, vFac2, vFac3, vFac4, vData)

vRpl1 = c(32, 35, 29, 40, 26, 80, 36, 105, 47, 63, 51, 64, 61, 100, 76, 90)
vRpl2 = c(43, 42, 39, 44, 36, 68, 31, 99, 41, 41, 34, 39, 76, 68, 65, 82)
vRpl3 = c(27, 56, 27, 53, 24, 75, 32, 74, 48, 60, 40, 75, 56, 87, 70, 89)
vRpl4 = c(19, 35, 28, 36, 18, 67, 30, 73, 45, 53, 48, 72, 64, 66, 63, 101)

Tbl = data.frame(rep(vFac1, r), rep(vFac2, r), rep(vFac3, r), rep(vFac4, r), vFac5, c(vRpl1, vRpl2, vRpl3, vRpl4))
colnames(Tbl) = c("A", "B", "C", "D", "R", "Y")
Res1 = lm(Y ~ as.factor(A)+ as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
               , Tbl)
Res2 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D) - 1
               , Tbl)
Res3 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
               , Tbl)
anova(Res1)
summary(Res2)
TukeyHSD(Res3)


Res4 = lm(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), Tbl)
anova(Res4)



