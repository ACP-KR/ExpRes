# Quiz 2-4
WF13.2 = read.csv("E:/Rt/ExpDes/WF13.2.csv")
WF13.2

source("E:/Rt/ExpDes/a2wnorpl.R")
a2wnorpl(WF13.2)

# Quiz 5-7
KNOUP278 = read.csv("E:/Rt/ExpDes/KNOUP278.csv")
KNOUP278

# 6
summary(aov(Y ~ as.factor(R) + as.factor(A) + Error(as.factor(R):as.factor(A)) + as.factor(B) + as.factor(R):as.factor(B) + as.factor(A):as.factor(B), data=KNOUP278), digits=22)
1 - pf(14.086, 3, 6)

# or
anova(lm(Y ~ as.factor(R) + as.factor(A) + as.factor(R):as.factor(A) + as.factor(B) + as.factor(R):as.factor(B) + as.factor(A):as.factor(B), data=KNOUP278))
1 - pf(4164.8/295.7, 3, 6)

# 7
summary(aov(Y ~ as.factor(R) + as.factor(A) + as.factor(B) + as.factor(R):as.factor(B), data=KNOUP278))

# Quiz 8
KNOUP279 = read.csv("E:/Rt/ExpDes/KNOUP279.csv")
KNOUP279

anova(lm(Y ~ as.factor(A)/as.factor(B), data=KNOUP279))
1 - pf(7.5278/7.7685, 2, 9)

