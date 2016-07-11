# Chapter 11

# Example 11.1 p348
E11.1 = read.csv("E:/Rt/ExpDes/E11.1.CSV")

# Wrong Code
#Res1 = lm(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), E11.1)
#Res2 = aov(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), E11.1)

#anova(Res1)
#TukeyHSD(Res3)

# Correct Code
Res1 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
             + as.factor(BL)
             , E11.1)

Res2 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
             + as.factor(BL)
            , E11.1)

anova(Res1)
TukeyHSD(Res2)

# Example 11.2 p353
T11.6 = read.csv("E:/Rt/ExpDes/T11.6.CSV")

Res4 = lm(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C), T11.6)
Res5 = lm(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C) - 1, T11.6)
Res6 = aov(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C), T11.6)

anova(Res4)
summary(Res5)
TukeyHSD(Res6)

# Example 11.3 p360
E11.3 = read.csv("E:/Rt/ExpDes/E11.3.CSV")

Res7 = lm(Y ~ as.factor(A)*as.factor(B), E11.3)
Res8 = lm(Y ~ as.factor(A)*as.factor(B) - 1, E11.3)
Res9 = aov(Y ~ as.factor(A)*as.factor(B), E11.3)

anova(Res7)
summary(Res8)
TukeyHSD(Res9)

Res10 = lm(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B), E11.3)
Res11 = lm(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B)- 1, E11.3)
Res12 = aov(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B), E11.3)

anova(Res10)
summary(Res11)
TukeyHSD(Res12)

# Example 11.4 p367
library(FrF2)
FrF2(8, 4)
FrF2(16, 4)
FrF2(nruns=8, nfactors=3, blocks=2) # Table 11.9 p365
FrF2(8, nfactors=4, factor.names=c("A","B","C","D"), estimable = formula("~A+B+C+D+A:(B+C+D)"), clear=FALSE, alias.block.2fis=TRUE)

