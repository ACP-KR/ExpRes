# Chapter 6
# Randomization 반복이 없는 3원배치
source("E:/Rt/ExpDes/R3w.R")
R3w(l=3, m=3, n=3)

# Example 6.1 p170
T6.5 = read.csv("E:/Rt/ExpDes/T6.5.CSV")
source("E:/Rt/ExpDes/a3w.R")
a3w(T6.5)

# Table 6.14 계수형자료
T6.14 = read.csv("E:/Rt/ExpDes/T6.14.CSV")
source("E:/Rt/ExpDes/p213.R")
p213(T6.14)

# Compare with Chapter 5 a2wff(Tbl) result
source("E:/Rt/ExpDes/a2wff.R")
a2wff(T6.14)

