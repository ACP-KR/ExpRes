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
T12.4 = data.frame(T12.4)

Res1 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D) + as.factor(E), T12.4)
Res3 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D) + as.factor(E), T12.4)

anova(Res1)
summary(Res1)
TukeyHSD(Res3)

Res4 = lm(Y ~ as.factor(A) + as.factor(C) + as.factor(D), T12.4)
Res6 = aov(Y ~ as.factor(A) + as.factor(C) + as.factor(D), T12.4)

anova(Res4)
summary(Res4)
TukeyHSD(Res6)

# Example 12.1 p387
T12.8 = read.csv("E:/Rt/ExpDes/T12.8.csv")

Res7 = lm(Y ~ as.factor(A)*as.factor(B) + as.factor(C)*as.factor(D) + as.factor(F) + as.factor(H), T12.8)
Res9 = aov(Y ~ as.factor(A)*as.factor(B) + as.factor(C)*as.factor(D) + as.factor(F) + as.factor(H), T12.8)

anova(Res7)
summary(Res7)
TukeyHSD(Res9)

# Example 12.2 p394
library(FrF2)
FrF2(nruns=16, nfactors=7, factor.names=c("A","B","C","D","F","G","H"), estimable=formula("~A+B+C+D+F+G+H+A:(B+C+D)+G:H"), clear=FALSE)

# Example 12.3 p396
FrF2(nruns=8, nfactors=4, estimable=formula("~A+B+C+D+A:B+C:D"), clear=FALSE) # Not successful
FrF2(nruns=16, nfactors=4, estimable=formula("~A+B+C+D+A:B+C:D"), clear=FALSE)

# Example 12.4 p397
FrF2(nruns=16, nfactors=5, factor.names=c("A","B","C","D","F"), estimable=formula("~A+B+C+D+F+B:(A+C+D)"), clear=FALSE)

# Table 12.14 p400
T12.14 = read.csv("E:/Rt/ExpDes/T12.14.csv")

Res10 = lm(Y ~ as.factor(A)*as.factor(B) + as.factor(C) + as.factor(D) + as.factor(F) + as.factor(G), T12.14)
Res12 = aov(Y ~ as.factor(A)*as.factor(B) + as.factor(C) + as.factor(D) + as.factor(F) + as.factor(G), T12.14)

anova(Res10)
summary(Res10)
TukeyHSD(Res12)


