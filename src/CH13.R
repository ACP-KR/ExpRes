# Chapter 13

# Table 13.4 p417
T13.4 = read.csv("E:/Rt/ExpDes/T13.4.csv")

Res1 = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C), T13.4)
Res2 = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C), T13.4)

anova(Res1)
summary(Res1)
TukeyHSD(Res1)

# Example 13.1 p421

# Example 13.2 p423
E13.2 = read.csv("E:/Rt/ExpDes/E13.2.csv")

summary(lm(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D) + as.factor(F) + as.factor(G) - 1, E13.2))
Res3 = lm(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D) + as.factor(F) + as.factor(G), E13.2)
Res4 = aov(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D) + as.factor(F) + as.factor(G), E13.2)

anova(Res3)
summary(Res3)
TukeyHSD(Res4)

lmr = lm(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D) - 1, E13.2)
smr = summary(lmr)
smr$sigma
smr$df
df.residual(lmr)


Res5 = lm(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D), E13.2)
anova(Res5)
summary(Res5)

Res6 = aov(Y ~ as.factor(A) + as.factor(B)*as.factor(C) + as.factor(D), E13.2)
TukeyHSD(Res6)
model.tables(Res6, type="means", se=TRUE) # R 2.13.1 이상 쓸 것, Different Result
model.tables(Res6, type="effects", se=TRUE)


