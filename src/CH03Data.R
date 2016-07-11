# Chapter 3 Data Prep

# Example 3.1 Table 3.5 p65
nLvl = 4
nRpl = 3

vData = c(8.44, 8.36, 8.28, 8.59, 8.91, 8.60, 9.34, 9.41, 9.69, 8.92, 8.92, 8.74)
vLevel = sort(rep(1:nLvl,nRpl))
T3.5 = cbind(vLevel, vData)
colnames(T3.5) = c("A","Y")
write.csv(T3.5, "T3.5.CSV", row.names=FALSE, quote=FALSE)
read.csv("T3.5.CSV")


# Table 3.13 p85
vData2 = c(49, 73, 58, 38, 42, 31, 40, 43, 44, 34, 20, 46, 41, 58, 31, 65, 45, 73, 76)
vLevel2 = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4)
T3.13 = cbind(vLevel2, vData2)
colnames(T3.13) = c("A","Y")
write.csv(T3.13, "T3.13.CSV", row.names=FALSE, quote=FALSE)
read.csv("T3.13.CSV")


# ???��?�� 3.4
vData3 = c(15.4, 15.2, 15.0, 15.3, 15.2, 14.9, 15.1,
            14.8, 14.9, 14.7, 15.0, 14.8, 14.9, 15.0,
            15.5, 15.4, 15.3, 15.2, 15.4, 15.5, 15.0)
vLevel3 = sort(rep(1:3, 7))
P3.4 = cbind(vLevel3, vData3)
colnames(P3.4) = c("A","Y")
write.csv(P3.4, "P3.4.CSV", row.names=FALSE, quote=FALSE)
read.csv("P3.4.CSV")



