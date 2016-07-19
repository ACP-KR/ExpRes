#05.03
WD = c("D:\\project\\ExperimentDesign\\src", # 회의실 컴퓨터
       "~/GIT/ExperimentDesign/src",
       "D:\\GIT\\ExperimentDesign\\src")
setwd(WD[3])
##
# 기계 소음 작게 하기 위하여 모터의 베어링
# 조립 후의 볼베어링 유격을 3수준,
# 조립 후 진동을 3수준으로 바꾸어
# 3회 반복 -> 3x3x3 = 27회의 랜덤 실험, 소음계로 소음 측정
# A1 = 0, A2 = 5, A3 = 10u, 조립후의 진동 B1 = 40u, B2 = 110u, B3 = 180u

# (1) 등분산의 가정을 검토
# (2) 분산분석표 작성한 후 소음감소의 최적조건이 무엇인지 찾아라. 소음의 모평균을 신뢰율 95%로 구간 추정

Ex5.3 = read.csv("shan/Ex5.3.csv")
anova(lm(Y ~ as.factor(A) + as.factor(B) + as.factor(A)*as.factor(B), data=Ex5.3))

#Analysis of Variance Table

#Response: Y
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
#  as.factor(A)               2 27.185  13.593  9.4103 0.001595 ** 
#  as.factor(B)               2 79.407  39.704 27.4872 3.38e-06 ***
#  as.factor(A):as.factor(B)  4 22.815   5.704  3.9487 0.017961 *  
#  Residuals                 18 26.000   1.444                     

source("a2wff.R")
a2wff(Ex5.3)
#
#$`Sum of Squares`
#                        SS phi        MS
#Factor A          27.18519   2 13.592593
#Factor B          79.40741   2 39.703704
#Interation (AxB)  22.81481   4  5.703704
#Error             26.00000  18  1.444444
#Total            155.40741  26  5.977208
#
#$`F test for Fixed Factors`
#         F.val    F.crt        p.val
#[1,]  9.410256 3.554557 1.594572e-03
#[2,] 27.487179 3.554557 3.380025e-06
#[3,]  3.948718 2.927744 1.796100e-02
#
#$`Cell Means`
#         [,1]     [,2]     [,3]
#[1,] 76.33333 79.66667 82.33333
#[2,] 75.00000 79.00000 77.00000
#[3,] 76.00000 78.00000 80.00000
#
#$`Lower Limits`
#         [,1]     [,2]     [,3]
#[1,] 74.87553 78.20886 80.87553
#[2,] 73.54219 77.54219 75.54219
#[3,] 74.54219 76.54219 78.54219
#
#$`Upper Limits`
#         [,1]     [,2]     [,3]
#[1,] 77.79114 81.12447 83.79114
#[2,] 76.45781 80.45781 78.45781
#[3,] 77.45781 79.45781 81.45781

#교호작용이 유의하다
