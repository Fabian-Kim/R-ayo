#A practice note for Part 3

#변수간 관계를 나타내는 산점도 그리기 (Scater Plot)
library(ggplot2)
ggplot(data = mpg, aes(x = displ,
                       y = hwy)) # <- 배경
ggplot(data = mpg, aes(x = displ,
                       y = hwy)) +
  geom_point() # <- 점 추가
ggplot(data = mpg, aes(x = displ,
                       y = hwy)) + 
  geom_point()+ 
  xlim(3,6) # <- 축 범위 제한
qplot() # <- 단순 데이터 확인용
ggplot() # <- 보고용, 세부 조작용

ggplot(data = mpg, aes(x = cty,
                       y = hwy)) +
  geom_point()
ggplot(data = midwest, aes(x = poptotal,
                           y = popasian))+
  geom_point()+
  xlim(0,500000) +
  ylim(0,10000)

#평균막대그래프
library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy= mean(hwy)) # <- 평균 변수를 추가후 그래프 생성
df_mpg
ggplot(data = df_mpg, aes(x = drv,
                          y = mean_hwy))+
  geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy),
                          y = mean_hwy))+
  geom_col()

#빈도막대그래프
library(ggplot2)
library(dplyr)
ggplot(data=mpg, aes(x=drv))+
  geom_bar()

ggplot(data=mpg, aes(x=hwy))+
  geom_bar()

pr_mpg <- mpg %>% 
  filter(class =="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
ggplot(data = pr_mpg, aes(x = reorder(manufacturer,-mean_cty),
                          y = mean_cty)) + geom_col()

ggplot(data = mpg, aes(x = class))+
  geom_bar()
  
#선그래프 <- 주로 시계열 데이터에 사용
ggplot(data = economics,aes(x = date,
                            y = unemploy))+
  geom_line()

ggplot(data = economics, aes(x = date,
                             y = psavert))+
  geom_line()

#박스플로 <- 집단비교
ggplot(data = mpg, aes(x = drv,
                       y = hwy))+
  geom_boxplot()
## 점 <- 1.5iqr 이 넘을 경우 (iqr = 상위 1Q~하위 1Q 거리)
## 중간선 <- 중간값(mean)
## 상자 상하 <- 상위 1분위, 하위 1분위
## 선 끝 <- 최소값, 최대값
## boxplot 쓸 때 groupby 하면 안됨! 데이터가 잘릴 수 있음

#Boxplot Practice
mpg_cl <- mpg %>% 
  filter(class %in% c("compact", "subcompact","suv"))
ggplot(data = mpg_cl, aes(x = class,
                       y = cty))+
  geom_boxplot()


#결측치(missing value) 정제하기
##정제할 대상 <- 빠진 데이터, 이상한 데이터

df <- data.frame(sex = c("m","f",NA,'m',"f"),
                 score = c(5,4,3,4,NA))
df

is.na(df) # <- 결측치 확인
table(is.na(df)) # <- 결측치 개수 확인

library(dplyr)
df %>% 
  filter(is.na(score))
df_nomiss <- df %>% 
  filter(!is.na(score)&!is.na(sex))
df_nomiss

df_nomiss2 <- na.omit(df) # <- 모든 변수에 결측치 없는 것 추출, 실제로 많이 쓰지는 않음

mean(df$score, na.rm = T) # <- 결측치 제외하고 평균
sum(df$score, na.rm = T) # <- 결측치 제외한 합계

exam <- read.csv("csv_exam.csv")
head(exam,5)

exam %>% 
  summarise(mean_math = mean(math, na.rm = T),
            sum_english = sum(english, na.rm = T),
            median_science = median(science, na.re = T))


#결측치 대체하기 (Imputation)
#- 대표값으로 일괄 대체
#- 예측값을 추정해서 대체
#- 다른 변수들을 토대로 머신러닝 (최신 추세)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

#결측치 practice
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212), "hwy"] <- NA

table(is.na(mpg$drv))
table(is.na(mpg$hwy))
mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

#이상치 정제하기 (Outlier)
#- 존재할 수 없는 값, 극단적인 값

#존재할 수 없는 값 걸러내기
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
table(outlier$sex)
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex) # <- 이상치를 NA로 변경
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

#정상범위 외의 값 걸러내기
mpg <- as.data.frame((ggplot2::mpg))
boxplot(mpg$hwy)$stats # <- 상자그림의 수치 출력

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

#이상치 정제 practice
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93), "drv"] <- "k"
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42)

##이상치 확인
table(mpg$drv)
##drv가 4,f,r 인 경우 제외하고 NA로 할당
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
##이상치 재확인
table(mpg$drv)

boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

#한국복지패널데이터
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare <- read.spss("C:/Users/Fabian/Documents/RStudio/R-ayo/source/data_spss_Koweps2014.sav", to.data.frame = T)
# <- 자주 불러오는 것보다 assign 해두고 복사하는 것이 효율성이 좋음
welfare <- raw_welfare

dim(welfare)
str(welfare)
head(welfare)

welfare <- rename(welfare,
                  sex = h0901_4, #성별
                  birth = h0901_5, #출생연도
                  income = h09_din) #소득

class(welfare$sex)
summary(welfare$sex)
table(welfare$sex) # <- 무응답 9가 없음
welfare$sex <- ifelse(welfare$sex == 1, "male","female")
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex,
                              y = mean_income))+
  geom_col()

#생년변수 확인
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#나이변수 생성
welfare$age <- 2014-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

#나이별 소득 평균표
age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
age_income
ggplot(data = age_income, aes(x = age,
                              y = mean_income))+
  geom_point()
 
#연령대 변수 생성
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                        ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

#연령대별 소득 평균
welfare_income <- welfare %>% 
  filter(ageg !="young") %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
welfare_income

#연령대/성별에 따른 소득
sex_income <- welfare %>% 
  filter(ageg != "young") %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
ggplot(data = sex_income, aes(x = ageg,
                              y = mean_income,
                              fill = sex))+
  geom_col(position = "dodge")
