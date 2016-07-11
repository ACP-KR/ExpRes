# 1 분산분석을 하라
# 2 각 수준에서 수율의 90% 신뢰구간 구하고 이를 도시
# 3 현재 사용되고 있는 촉매의 첨가량이 A1(1%), 촉매의 첨가량을 1% 증가시켜 사용하려면, 수율이 최소 4% 증가해야 함. 이 경우 촉매 첨가량 증가시키는 것이 바람직한가? 그 이유를 설명

WD = c("D:\\project\\ExperimentDesign\\src", "~/GIT/ExperimentDesign/src")
setwd(WD[2])

library(gplots) # install.packages("gplots")
source("a1way.R")

vData = c(84.3, 83.9, 84.2, 87.3, 86.8, 87.2, 89.5, 89.8, 90.1, 92.0, 93.1, 92.8)
vLevel = sort(rep(1:4,3))

cbind(vLevel, vData)

a1way(cbind(vLevel, vData))
TukeyHSD(aov(vData ~ as.factor(vLevel)))
Res = a1way(cbind(vLevel, vData))
Res

# gplot2
plotCI(Res[[3]][,2], uiw=(Res[[3]][,2]-Res[[3]][,1])/t.crt, type="b", xaxt="n", ylim=c(70,100))
axis(side=1, at=1:4, cex=0.7)

# qplot
factor(vLevel)
Table = data.frame(factor(vLevel), vData)
Table
library(ggplot2)
qplot(vLevel, vData, data = Table, geom=c("boxplot", "jitter"))+ labs(list(x = "Dose (mg)", y = "" )) + theme_bw()

