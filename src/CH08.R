# Chapter 8

# Example 8.1
x = c(2, 3, 4, 5, 6)
y = c(4, 7, 6, 8, 10)

cor(x,y)
lm.r = lm(y ~ x)
summary(lm.r)
anova(lm.r)

# Table 8.4
x = sort(rep(c(100, 120, 140, 160, 180), 3))
y = c(2.9, 2.1, 3.1, 3.5, 3.1, 3.8, 5.2, 4.2, 4.6, 5.9, 6.2, 5.6, 6.4, 6.5, 7.3)

lm.r = lm(y ~ x)
summary(lm.r)
anova(lm.r)

aov.r = aov(y ~ as.factor(x))
summary(aov.r)
