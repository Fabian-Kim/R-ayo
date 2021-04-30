#데이터 프레임
history <- c(90,80,60,70)
history
math <- c(50,60,100,20)

df_midterm <- data.frame(history,math)
class <- c(1,1,2,2)

df_midterm <- data.frame(history,math,class)

mean(df_midterm$history)


#외부데이터 R로 가져오기
install.packages("readxl")
library(readxl)
df_finalexam <- read_excel("kc_house_data.xlsx",sheet=1, col_names=T)
#파일 위치가 working directory 내에 있어야 함
df_finalexam
mean(df_finalexam$price)

#csv 불러오기
csv_exam <- read.csv("csv_exam.csv",header=T)

#csv로 저장하기
write.csv(df_finalexam, file='output_newdata.csv') 

#데이터 앞부분
exam <- read.csv("csv_exam.csv")
head(exam)
tail(exam,10)
View(exam)
dim(exam)
str(exzm)
summary(exam)

#데이터를 프레임으로 가져오기
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
#:: (더블콜론) package 안에 있는 특정한 것을 지칭함

mpg
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)

#변수명 바꾸기
#- 바꿀 이름이 먼저, 반드시 assign
library(dplyr)
df_raw <- data.frame(var1=c(1,2,1), var2=c(2,3,2))
df_raw
df_new <- df_raw
df_new
df_new <- rename(df_new,v2=var2)
df_new

#변수명 바꾸기 practice more
library(ggplot2)
librayr(dplyr)
dim(mpg)
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_edit <- mpg_raw
mpg_edit <- rename(mpg_edit,city=cty)
mpg_edit <- rename(mpg_edit,highway=hwy)
head(mpg_edit)

#파생변수 추가하기
#- 프레임명 뒤에 $
df <- data.frame(var1=c(4,3,8),
                 var2=c(2,6,1))
df
df$var_sum <- df$var1+df$var2
df
df$var_mean <- (df$var1+df$var2)/2
f

#파생변수 추가하기 practice more
mpg$total <- (mpg$cty+mpg$hwy)/2
mpg$totla <- mpg$total
summary(mpg$total)
hist(mpg$total)

#조건문 변수
mpg$test <- ifelse(mpg$totla >=20, "Pass","Fail")
head(mpg,20)
table(mpg$test)
qplot(mpg$test)

#중첩조건문
mpg_grade <- ifelse(mpg$total >=30, "A",
                    ifelse(mpg$total >=20, "B","C"))
head(mpg,20)
table(mpg_grade)

#practice using ggplot2::midwest
midwest_raw <- as.data.frame(ggplot2::midwest)
dim(midwest_raw)
head(midwest_raw)
tail(midwest_raw)
summary(midwest_raw)
hist(midwest_raw)

library(dplyr)
midwest_raw <- rename(midwest_raw, total=poptotal)
midwest_raw <- rename(midwest_raw, asian=popasian)
midwest_raw$asian_per <- midwest_raw$asian/midwest_raw$total*100
midwest_raw$asian_per
hist(midwest_raw$asian_per)
mean(midwest_raw$asian_per)


## 데이터 전처리
#filter, select, arrange, ...

#조건에 맞는 데이터(행) 추출하기 <- filter
library(dplyr)
exam <- read.csv(("csv_exam.csv"))
exam %>%
  filter(class == 1)
exam %>% 
  filter(class == 2)
exam %>% 
  filter(class!=1) #!는 NOT
exam %>% 
  filter(math>50)
exam %>% 
  filter(math>=90|english>=90) #"|"는 OR
exam %>% 
  filter(class %in% c(1,3,5)) # %in% : match operator

#mpg 사용해서 데이터 분석
View(mpg)
mpg <- as.data.frame(ggplot2::mpg)
mpg_a <- mpg %>% filter(displ<=4)
mpg_b <- mpg %>% filter(displ>=5) 
mean(mpg_a$hwy)
mean(mpg_b$hwy)

mpg_audi <- mpg %>% filter(manufacturer=="audi")
mpg_toyota <- mpg %>% filter(manufacturer=="toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

mpg_cmp <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(mpg_cmp$hwy)

#필요한 변수(열)를 추출하기 <- select
exam %>% select(math)
exam %>% select(-math)
exam %>% 
  filter(class==1) %>% 
  select(english)

#mpg로 변수 추출 실습
mpg_k <- mpg %>% select(class, cty)
head(mpg_k,10)

mpg_suv <- mpg_k %>% filter(class=="suv")
mpg_compact <-  mpg_k %>% filter(class=="compact")
mean(mpg_suv$cty)
mean(mpg_compact$cty)

#데이터 정렬하기
library(dplyr)
exam
exam %>% arrange (class,math)
exam %>% arrange(desc(class))

mpg <- as.data.frame((ggplot2::mpg))
mpg %>% 
  filter(manufacturer == "audi")
mpg %>% 
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

#파생변수 추가하기
exam %>% 
  mutate(total = math + english + science) %>%  
  head
# dplyr는 앞에 데이터프레임 이름을 적지 않아도 됨
exam %>% 
  mutate(test=ifelse(science >=60, "pass","fail")) %>% 
  head
exam %>% 
  mutate(total = math + english) %>% 
  arrange(total) %>% 
  head
mpg_c <- as.data.frame(ggplot2::mpg)
mpg_c
mpg_c %>% 
  mutate (hwycty = hwy + cty,
          mean_hwycty = hwycty / 2) %>% 
  arrange(desc(mean_hwycty)) %>% 
  head(3)

#집단별로 데이터 요약 <- 많이 씀!
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n()) # <- 행의 개수(빈도)
mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)

#데이터 합치기, 열 결합
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
test2 <- data.frame(id = c(1:5),
                    final = c(70,83,65,95,80))
total <- left_join(test1,test2,by = "id")
total
name <- data.frame(class = c(1:5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
exam_new <- left_join(exam,name,by="class")
exam_new

#데이터 합치기, 행 결합
group_a <- data.frame(id = c(1:5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(1:5),
                      test = c(70,83,65,95,80))
group_all <- bind_rows(group_a,group_b)
group_all

fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel
fuel <- left_join(mpg,fuel, by = "fl")
fuel

#데이터 합치기 practice
library(ggplot2)
library(dplyr)
midwest
midwest %>% 
  select(popadults, poptotal) %>% 
  head(10)
midwest$adole_per <- (midwest$poptotal-midwest$popadults)/midwest$popadults*100
midwest %>% 
  select(county,popadults,poptotal,adole_per) %>% 
  arrange(desc(adole_per)) %>% 
  head(5)
