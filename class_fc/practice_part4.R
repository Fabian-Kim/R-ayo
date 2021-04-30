#Part4. 데이

#load source file
rawN3 <- read.csv(file="C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest01.csv", header = T)
rawN3

#devide into two groups by 'group'
groupA <- rawN3[rawN3$group=='A',1:2]
groupB <- rawN3[rawN3$group=='B',1:2]

#mean of each group
mean(groupA[,2]) # <- 2
mean(groupB[,2])

#Normallity test - shapiro
shapiro.test(groupA[,2])
## p-value > 0.05
shapiro.test(groupB[,2])

#Normality test - qqplot
qqnorm(groupA[,2])
qqline(groupA[,2])
qqnorm(groupB[,2])
qqline(groupB[,2])

#Variances
var.test(groupA[,2], groupB[,2])

#t-test
t.test(groupA[,2], groupB[,2],alternative = "less", var.equal = T)
## alternative less : 왼쪽 

