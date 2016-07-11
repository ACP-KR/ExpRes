# Chapter 10
# Example 10.1 (p313) # See Chapter 5 반복이 동일한 이원배치
E10.1 = read.csv("E:/Rt/ExpDes/E10.1.CSV")
attach(E10.1)
windows()
interaction.plot(A, B, Y)
windows()
interaction.plot(B, A, Y)
detach(E10.1)

source("E:/Rt/ExpDes/RD4.R")
RD4(E10.1)
source("E:/Rt/ExpDes/a2wff.R")
a2wff(E10.1)

# Example 10.2 (p319)
E10.2 = read.csv("E:/Rt/ExpDes/E10.2.CSV")
source("E:/Rt/ExpDes/a3w.R") # Chapter 6 반복이 없는 3원배치법
a3w(E10.2)

## Example 10.3 (p325), Table 10.9 (p326)
T10.9 = read.csv("E:/Rt/ExpDes/T10.9.CSV")

Res1 = lm(Y ~ as.factor(A)+ as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
               , T10.9)
Res2 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D) - 1
               , T10.9)
Res3 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(D)
             + as.factor(A):as.factor(B)
             + as.factor(A):as.factor(C)
             + as.factor(A):as.factor(D)
             + as.factor(B):as.factor(C)
             + as.factor(B):as.factor(D)
             + as.factor(C):as.factor(D)
               , T10.9)
anova(Res1)
summary(Res2)
TukeyHSD(Res3)


Res4 = lm(Y ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D), T10.9)
anova(Res4)



