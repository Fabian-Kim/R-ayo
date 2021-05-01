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


####
#laod file
rawN10 <- read.csv(file = "C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest02.csv", header = T)
rawN10

#grouping
groupA2 <- rawN10[rawN10$group=='A',1:2]
groupB2 <- rawN10[rawN10$group=='B', 1:2]

#mean
mean(groupA2[,2])
mean(groupB2[,2])

#Normality
shapiro.test(groupA2[,2]) #p-value = 0.2826 > 0.05
shapiro.test(groupB2[,2]) #p-value = 0.9108 > 0.05

#Variance
var.test(groupA2[,2],groupB2[,2])

#ttest
t.test(groupA2[,2],groupB2[,2], alternative = "less", var.equal = F)
