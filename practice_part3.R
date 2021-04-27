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

