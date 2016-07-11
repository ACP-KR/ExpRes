# Chapter 14

# Example 14.2 p443
T14.4 = read.csv("E:/Rt/ExpDes/T14.4.csv")
T14.4

Res1 = lm(Y ~ as.factor(Blk) + as.factor(Trt), T14.4)
Res2 = aov(Y ~ as.factor(Blk) + as.factor(Trt), T14.4)

anova(Res1)  # See the difference with the book
summary(Res1) # See the difference with the book
summary(Res2) # See the difference with the book
model.tables(Res2, type="means", se=TRUE) # R 2.13.1 이상 쓸 것, Different Result
model.tables(Res2, type="effects", se=TRUE)

TukeyHSD(Res2) # See the difference with the book

## Detailed BIBD of T14.4
source("E:/Rt/ExpDes/BIBD.R")
BIBD(T14.4)

# Table 14.8 Youden Square p 
T14.8 = read.csv("E:/Rt/ExpDes/T14.8.csv")
T14.8

Res3 = lm(Y ~ as.factor(Blk) + as.factor(A) + as.factor(C), T14.8)
Res4 = aov(Y ~ as.factor(Blk) + as.factor(A) + as.factor(C), T14.8)

anova(Res3) # See the difference with the book
summary(Res3) # See the difference with the book
model.tables(Res4, type="means", se=TRUE) # R 2.13.1 이상 쓸 것, Different
model.tables(Res4, type="effects", se=TRUE)
TukeyHSD(Res4) # See the difference with the book

# Detailed Youden Square
source("E:/Rt/ExpDes/Youden.R")
Youden(T14.8)
