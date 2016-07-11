# CH03 - CH07.R
# T3.5 p65
T3.5 = read.csv("E:/Rt/ExpDes/T3.5.CSV")
T3.5
plot(Y ~ as.factor(A), T3.5)

r.aov = aov(Y ~ as.factor(A), T3.5)
s.aov = summary(r.aov)
r.aov
s.aov

r.lm = lm(Y ~ as.factor(A), T3.5)
s.lm = summary(r.lm)
a.lm = anova(r.lm)
r.lm
s.lm
a.lm

MSE = a.lm["Residuals","Mean Sq"]
df.E = a.lm["Residuals","Df"]
m = 3
SE = sqrt(MSE/m)
alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)
LSD95 = t.cut*SE
LSD95

model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - LSD95
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + LSD95

TukeyHSD(r.aov)

DiffSE = sqrt(2*MSE/m)

# T3.13 p85
T3.13 = read.csv("E:/Rt/ExpDes/T3.13.CSV")
T3.13
plot(Y ~ as.factor(A), T3.5)

r.aov = aov(Y ~ as.factor(A), T3.13)
s.aov = summary(r.aov)

r.lm = lm(Y ~ as.factor(A), T3.13)
s.lm = summary(r.lm)
a.lm = anova(r.lm)

mi = c(5, 6, 5, 3)
SE = sqrt(a.lm["Residuals","Mean Sq"]/mi)
alpha = 0.05
t.cut = qt(1 - alpha/2, a.lm["Residuals","Df"])
LSD95 = t.cut*SE
LSD95

model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - LSD95
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + LSD95

TukeyHSD(r.aov)

# P3.4 p90
P3.4 = read.csv("E:/Rt/ExpDes/P3.4.CSV")
P3.4

r.aov = aov(Y ~ as.factor(A), data=P3.4)
s.aov = summary(r.aov)
r.aov
s.aov

attributes(r.aov)
attributes(s.aov)

r.lm = lm(Y ~ as.factor(A), data=P3.4)
s.lm = summary(r.lm)
a.lm = anova(r.lm)
r.lm
s.lm
a.lm

attributes(r.lm)
attributes(s.lm)
attributes(a.lm)

MSA = a.lm["as.factor(A)","Mean Sq"]
SSE = a.lm["Residuals","Sum Sq"]
df.E = a.lm["Residuals","Df"]
MSE = SSE / df.E
m = 7

SSA = (MSA - MSE) / m
alpha = 0.05
c(SSE/qchisq(1 - alpha/2, df.E), MSE, SSE/qchisq(alpha/2, df.E))

source("E:/Rt/ExpDes/a1way.R")
a1way(P3.4)

# T4.3 p103
T4.3 = read.csv("E:/Rt/ExpDes/T4.3.CSV")
T4.3

r.aov = aov(Y ~ as.factor(A) + as.factor(B), data=T4.3)
s.aov = summary(r.aov)
r.aov
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B), data=T4.3)
s.lm = summary(r.lm)
a.lm = anova(r.lm)
r.lm
s.lm
a.lm

MSA = a.lm["as.factor(A)","Mean Sq"]
MSB = a.lm["as.factor(B)","Mean Sq"]
SSE = a.lm["Residuals","Sum Sq"]
df.E = a.lm["Residuals","Df"]
MSE = SSE / df.E

m = 3
l = 4
SE.A = sqrt(MSE/m) 
SE.B = sqrt(MSE/l)

alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)

model.tables(r.aov, type="means", SE=TRUE)
model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - t.cut*SE.A
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + t.cut*SE.A

model.tables(r.aov, "means")$"tables"$"as.factor(B)"
model.tables(r.aov, "means")$"tables"$"as.factor(B)" - t.cut*SE.B
model.tables(r.aov, "means")$"tables"$"as.factor(B)" + t.cut*SE.B

TukeyHSD(r.aov)

source("E:/Rt/ExpDes/a2wnorpl.R")
a2wnorpl(T4.3)


# T4.7 p114
T4.7 = read.csv("E:/Rt/ExpDes/T4.7.CSV")
T4.7

r.aov = aov(Y ~ as.factor(A) + as.factor(B), data=T4.7)
s.aov = summary(r.aov)
r.aov
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B), data=T4.7)
s.lm = summary(r.lm)
a.lm = anova(r.lm)
r.lm
s.lm
a.lm

model.tables(r.aov, type="means", SE=TRUE)

MSA = a.lm["as.factor(A)","Mean Sq"]
MSB = a.lm["as.factor(B)","Mean Sq"]
SSE = a.lm["Residuals","Sum Sq"]
df.E = a.lm["Residuals","Df"]
MSE = SSE / df.E

m = 3
l = 3
SE.A = sqrt(MSE/m) 
SE.B = sqrt(MSE/l)

alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)

model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - t.cut*SE.A
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + t.cut*SE.A

model.tables(r.aov, "means")$"tables"$"as.factor(B)"
model.tables(r.aov, "means")$"tables"$"as.factor(B)" - t.cut*SE.B
model.tables(r.aov, "means")$"tables"$"as.factor(B)" + t.cut*SE.B

TukeyHSD(r.aov)

source("E:/Rt/ExpDes/a2wnorpl.R")
a2wnorpl(T4.7)


# T5.6 p135
T5.6 = read.csv("E:/Rt/ExpDes/T5.6.CSV")
T5.6

r.aov = aov(Y ~ as.factor(A) * as.factor(B), data=T5.6)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) * as.factor(B), data=T5.6)
a.lm = anova(r.lm)
a.lm

model.tables(r.aov, type="means", SE=TRUE)

r = 2
MSE = a.lm["Residuals","Mean Sq"]
SE = sqrt(MSE/r)
df.E = a.lm["Residuals","Df"]
alpha = 0.05
t.crt = qt(1 - alpha/2, df.E)
t.crt * SE

model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(B)"
model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(B)" - t.crt*SE
model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(B)" + t.crt*SE

TukeyHSD(r.aov)

source("E:/Rt/ExpDes/a2wff.R")
a2wff(T5.6)


# NT23.1 Kutner & Neter Table 23.1 p954
NT23.1 = read.csv("E:/Rt/ExpDes/NT23.1.CSV")
NT23.1

source("E:/Rt/ExpDes/a2wvr.R")
a2wvr(NT23.1)

# T6.5.CSV p170
T6.5 = read.csv("E:/Rt/ExpDes/T6.5.CSV")
T6.5

r.aov = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(B) + as.factor(A):as.factor(C) + as.factor(B):as.factor(C), data=T6.5)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(B) + as.factor(A):as.factor(C) + as.factor(B):as.factor(C), data=T6.5)
a.lm = anova(r.lm)
a.lm


r.aov = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(B) + as.factor(B):as.factor(C), data=T6.5)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(B) + as.factor(B):as.factor(C), data=T6.5)
a.lm = anova(r.lm)
a.lm

model.tables(r.aov, type="means", SE=TRUE)

source("E:/Rt/ExpDes/a3w.R")
a3w(T6.5)


# T6.13 p181
T6.13 = read.csv("E:/Rt/ExpDes/T6.13.CSV")
T6.13
head(T6.13)
r.aov = aov(Y ~ as.factor(A), data=T6.13)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A), data=T6.13)
a.lm = anova(r.lm)
a.lm


# T6.14 p183
T6.14 = read.csv("E:/Rt/ExpDes/T6.14.CSV")
head(T6.14)
r.aov = aov(Y ~ as.factor(A) + as.factor(B) + Error(as.factor(A):as.factor(B)), data=T6.14)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) * as.factor(B), data=T6.14)
a.lm = anova(r.lm)
a.lm

r.aov = aov(Y ~ as.factor(A) + as.factor(B), data=T6.14)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B), data=T6.14)
a.lm = anova(r.lm)
a.lm

MSA = a.lm["as.factor(A)","Mean Sq"]
MSB = a.lm["as.factor(B)","Mean Sq"]
SSE = a.lm["Residuals","Sum Sq"]
df.E = a.lm["Residuals","Df"]
MSE = SSE / df.E

l = 4
m = 2
r = 120
SE.A = sqrt(MSE/(m*r))
SE.B = sqrt(MSE/(l*r))

alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)

model.tables(r.aov, type="means", SE=TRUE)
model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - t.cut*SE.A
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + t.cut*SE.A

model.tables(r.aov, "means")$"tables"$"as.factor(B)"
model.tables(r.aov, "means")$"tables"$"as.factor(B)" - t.cut*SE.B
model.tables(r.aov, "means")$"tables"$"as.factor(B)" + t.cut*SE.B


source("E:/Rt/ExpDes/p213.R")
p213(T6.14)


# T7.4 p203
T7.4 = read.csv("E:/Rt/ExpDes/T7.4.CSV")
T7.4

r.aov = aov(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + Error(as.factor(R):as.factor(A):as.factor(B)), data=T7.4)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + as.factor(R):as.factor(A):as.factor(B), data=T7.4)
a.lm = anova(r.lm)
a.lm

r.aov = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(C), data=T7.4)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C) + as.factor(A):as.factor(C), data=T7.4)
a.lm = anova(r.lm)
a.lm

model.tables(r.aov, type="means", SE=TRUE)

MSE = a.lm["Residuals","Mean Sq"]
df.E = a.lm["Residuals","Df"]

l = 3
m = 2
n = 3
r = 2
SE.AC = sqrt(MSE/(m*r))

alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)

model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(C)"
model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(C)" - t.cut*SE.AC
model.tables(r.aov, "means")$"tables"$"as.factor(A):as.factor(C)" + t.cut*SE.AC

source("E:/Rt/ExpDes/T7.x.R")
T7.x(T7.4)

# T7.5 p204
T7.5 = read.csv("E:/Rt/ExpDes/T7.5.CSV")
T7.5

r.aov = aov(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + Error(as.factor(R):as.factor(A):as.factor(B)), data=T7.5)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(R) + as.factor(A)*as.factor(B)*as.factor(C) + as.factor(R):as.factor(A):as.factor(B), data=T7.5)
a.lm = anova(r.lm)
a.lm

r.aov = aov(Y ~ as.factor(A) + as.factor(B) + as.factor(C), data=T7.5)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B) + as.factor(C), data=T7.5)
a.lm = anova(r.lm)
a.lm

model.tables(r.aov, type="means", SE=TRUE)

MSE = a.lm["Residuals","Mean Sq"]
df.E = a.lm["Residuals","Df"]

l = 3
m = 2
n = 3
r = 2
SE.A = sqrt(MSE/(m*n*r))

alpha = 0.05
t.cut = qt(1 - alpha/2, df.E)

model.tables(r.aov, "means")$"tables"$"as.factor(A)"
model.tables(r.aov, "means")$"tables"$"as.factor(A)" - t.cut*SE.A
model.tables(r.aov, "means")$"tables"$"as.factor(A)" + t.cut*SE.A

source("E:/Rt/ExpDes/T7.x.R")
T7.x(T7.5)

# TP213 p213
TP213 = read.csv("E:/Rt/ExpDes/TP213.CSV")
TP213

r.aov = aov(Y ~ as.factor(A) + as.factor(B) + Error(as.factor(A):as.factor(B)), data=TP213)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) * as.factor(B), data=TP213)
a.lm = anova(r.lm)
a.lm

r.aov = aov(Y ~ as.factor(A) + as.factor(B), data=TP213)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A) + as.factor(B), data=TP213)
a.lm = anova(r.lm)
a.lm

model.tables(r.aov, type="means", SE=TRUE)


# T7.14 p220
T7.14 = read.csv("E:/Rt/ExpDes/T7.14.CSV")
T7.14

r.aov = aov(Y ~ as.factor(A)/as.factor(B)/as.factor(C), data=T7.14)
s.aov = summary(r.aov)
s.aov

r.lm = lm(Y ~ as.factor(A)/as.factor(B)/as.factor(C), data=T7.14)
a.lm = anova(r.lm)
a.lm

