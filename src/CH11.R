# Chapter 11

# Shortest solution first

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

Tbl = matrix(vData, nrow=(2^k), ncol=(k+2), byrow=TRUE)
colnames(Tbl) = c("A","B","C","D","BL","Y")
Tbl = data.frame(Tbl)

# Wrong Code
Res1 = lm(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), Tbl)
Res2 = lm(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D) - 1, Tbl)
Res3 = aov(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), Tbl)

anova(Res1)
summary(Res2)
TukeyHSD(Res3)

# Correct Code
Res1 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
             + as.factor(BL)
          , Tbl)

Res2 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
             + as.factor(BL)
             - 1, Tbl)

Res3 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
             + as.factor(BL)
            , Tbl)

anova(Res1)
summary(Res2)
TukeyHSD(Res3)


# Example 11.2 p353

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

Tbl2 = matrix(vData2, nrow=r*2^k, ncol=(k+3), byrow=TRUE)
colnames(Tbl2) = c("A","B","C","BL","R","Y")
Tbl2 = data.frame(Tbl2)

Res4 = lm(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C), Tbl2)
Res5 = lm(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C) - 1, Tbl2)
Res6 = aov(Y ~ as.factor(BL) + as.factor(A)*as.factor(B)*as.factor(C), Tbl2)

anova(Res4)
summary(Res5)
TukeyHSD(Res6)

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

Tbl3 = matrix(vData3, nrow=9, ncol=4, byrow=TRUE)
colnames(Tbl3) = c("A","B","BL","Y")
Tbl3 = data.frame(Tbl3)

Res7 = lm(Y ~ as.factor(A)*as.factor(B), Tbl3)
Res8 = lm(Y ~ as.factor(A)*as.factor(B) - 1, Tbl3)
Res9 = aov(Y ~ as.factor(A)*as.factor(B), Tbl3)

anova(Res7)
summary(Res8)
TukeyHSD(Res9)

Res10 = lm(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B), Tbl3)
Res11 = lm(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B)- 1, Tbl3)
Res12 = aov(Y ~ as.factor(BL) + as.factor(A) + as.factor(B) + as.factor(A*B), Tbl3)

anova(Res10)
summary(Res11)
TukeyHSD(Res12)


# Example 11.4 p367
library(FrF2)
FrF2(8, 4)
FrF2(16, 4)
FrF2(nruns=8, nfactors=3, blocks=2) # Table 11.9 p365
FrF2(8, nfactors=4, factor.names=c("A","B","C","D"), estimable = formula("~A+B+C+D+A:(B+C+D)"), clear=FALSE, alias.block.2fis=TRUE)

