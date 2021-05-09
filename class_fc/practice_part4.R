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

### 대응표본t검정 <- 차이고려
#file load
raw_d <- read.csv(file = "C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest02d.csv", header = T)
raw_d
groupAd <- raw_d[,1]
groupBd <- raw_d[,2]

#대응표본 티는 분산동질성 검증이 없음
t.test(groupAd, groupBd, alternatve = "less", paired = T) # <- paired가 대응표본임을 나타냄

#데이터 30이상일 경우(대표본) z검증
rawN30 <- read.csv(file = "C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest03.csv", header = T)
groupA3 <- rawN30[rawN30$group == 'A', 1:2]
groupB3 <- rawN30[rawN30$group == 'B', 1:2]
var.test(groupA3[,2], groupB3[,2], alternative = "less", var.equal = F)
z.test


#한글타자에러해
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")
wget http://ikuya.info/tmp/fcitx-qt5-rstudio.tar.gz
Sys.setenv(LANG = "en_US.UTF-8")
getwd() # <- "C:/Users/Fabian/Documents/RStudio/R-ayo"
options(encoding="utf-8")
Sys.setlocale("LC_ALL","English")
Sys.setlocale("LC_ALL", "korean")

#ztest 함수 assign
z.test <- function(x1, x2){
  n_x1 = length(x1)
  n_x2 = length(x2)
  mean_x1 = mean(x1)
  mean_x2 = mean(x2)
  cat("\n")
  cat("\tTwo Sample z-test\n")
  cat("\n")
  cat("mean of x1:", mean_x1, "\n")
  cat("mean of x2:", mean_x2, "\n")
  var_x1 = var(x1)
  var_x2 = var(x2)
  z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
  abs_z = abs(z)
  cat("z =", abs_z, "\n")
  p_value = 1-pnorm(abs_z)
  cat("p-value =", p_value)
}

#z테스트
z.test(groupA3[,2], groupB3[,2])

# (CAUTION!) ztest를 쓸 상황에 ttest를 사용하게 되면 결과가 달라짐


#########################################3
#여러 집단의 평균 차이 검정 <- ANOVA 검정

# 총오차 = 집단 간 오차 + 집단 내 오차
# 집단 내 오차 = 집단 내 데이터들의 오차 제곱의 합 = 각 데이터 값 - 해당집단 평균
# 집단 간 오차 = 집단 간 데이터평균 오차제곱의 합
# 집단간오차 > 집단내오차 <- 집단간 평균차이가 있다
# F 통계량 = {(집단간오차/집단개수-1)}/({집단내오차)/(전체데이터-집단개수)}
# F분포는 무조건 양수, 1 이상인 것을 p-value로 함

# 여러 집단 평균 차이 검정
raw_anova <- read.csv(file = "C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest04.csv", header = T)
groupA4 <- raw_anova[raw_anova$group == 'A', 1:2]
groupB4 <- raw_anova[raw_anova$group == 'B', 1:2]
groupC4 <- raw_anova[raw_anova$group == 'C', 1:2]
mean(groupA4[,2])
mean(groupB4[,2])
mean(groupC4[,2])
#anova 테스트는 양측 검정을 함

#정규성 검정
shapiro.test(groupA4[,2])
qqnorm(groupA4[,2])
qqline(groupA4[,2])

shapiro.test(groupB4[,2])
qqnorm(groupB4[,2])
qqline(groupB4[,2])

shapiro.test(groupC4[,2])
qqnorm(groupC4[,2])
qqline(groupC4[,2])

#분산 동질성 검정 <- levene테스트, bartlett테스트
install.packages("lawstat")
library(lawstat)
levene.test(raw_anova$heigh, raw_anova$group)

bartlett.test(height~group, data = raw_anova)

#anova 테스트
rawAnova <- aov(height~group, data = raw_anova)
summary(rawAnova)

#분할표를 사용한 카이제곱 검정
#카이제곱 통계량 = (관측값-기대값)^ / 기대값 들의 합
#관측값 <- 분할표 내의 각 값
#기대값 <- 행의합, 열의합, 총합을 토대로 계산한 값
#카이제곱 분포 <- 0이상이며, p-value>0.05 이면 귀무가설 채택

#카이제곱 실습
raw_chisq <- read.csv(file = "C:/Users/Fabian/Documents/RStudio/R-ayo/class_fc/sourceData/htest05.csv", header = T)
rawTable <- table(raw_chisq)
rawTable

chisq.test(rawTable, correct = F)
#셀 기대도수 > 5 <- correct = F
#셀 기대도수 < 5 <- correct = T

