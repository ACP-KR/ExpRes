#Chapter 3 ?Ͽ???ġ

source("Ran1way.R")
Ran1way(nLvl=4, nRpl=5)

# Table 3.5 p65
T3.5 = read.csv("T3.5.CSV")
T3.5
plot(Y ~ as.factor(A), T3.5)
lmr1 = lm(Y ~ as.factor(A), T3.5)
lmr1
smr1 = summary(lmr1)
smr1
smr1$sigma
smr1$df
df.residual(lmr1)
anr1 = anova(lmr1)
ResSE = sqrt(anr1["Residuals","Mean Sq"]/anr1["as.factor(A)","Df"])
t.cut = qt(1 - 0.05/2, anr1["Residuals","Df"])
LSD95 = t.cut*ResSE
LSD95


aovr1 = aov(Y ~ as.factor(A), T3.5)
summary(aovr1)
model.tables(aovr1, "means")
model.tables(aovr1, "means")$"tables"$"as.factor(A)"
model.tables(aovr1, "means")$"tables"$"as.factor(A)" - LSD95
model.tables(aovr1, "means")$"tables"$"as.factor(A)" + LSD95

res1b = lm(Y ~ as.factor(A) - 1, T3.5)
smr1b = summary(res1b)
smr1b$sigma
smr1b$df
df.residual(res1b)

res2 = aov(Y ~ as.factor(A), T3.5)
summary(res2)
t1 = model.tables(res2, type="means", se=TRUE)
t1
t2 = model.tables(res2, type="effects", se=TRUE)
t2
TukeyHSD(res2)

library(gplots)
plotCI(t1$tables[[2]], uiw=t2$se[[1]], type="b", ylab="Strength", xlab="Temperature", xaxt="n", ylim=c(8,10))
axis(side=1, at=1:4, labels=c("A1","A2","A3","A4"), cex=0.7)

source("a1way.R")
a1way(T3.5)

##########
# Example 3.1, Table 3.12 p85
T3.13 = read.csv("T3.13.CSV")
source("a1way.R")
a1way(T3.13)

res = lm(Y ~ as.factor(A), T3.13)
res
summary(res)
anova(res)

res2 = aov(Y ~ as.factor(A), T3.13)
summary(res2)
#se.contrast(res2, contrasts(as.factor(T3.13[,"A"])), c(-1,1, 0))

t1 = model.tables(res2, type="means", se=TRUE)
t1
t2 = model.tables(res2, type="effects", se=TRUE)
t2
TukeyHSD(res2)

library(gplots)
plotCI(t1$tables[[2]], uiw=t2$se[[1]], type="b", ylab="??????", xlab="?????? ????", xaxt="n", ylim=c(0,100))
axis(side=1, at=1:4, labels=c("A1","A2","A3","A4"), cex=0.7)
