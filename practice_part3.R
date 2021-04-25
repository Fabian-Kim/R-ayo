#A practice note for Part 3

#변수간 관계를 나타내는 산점도 그리기 (Scater Plot)
library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy)) # <- 배경
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() # <- 점 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point()+ 
  xlim(3,6) # <- 축 범위 제한
qplot() # <- 단순 데이터 확인용
ggplot() # <- 보고용, 세부 조작용

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggplot(data = midwest, aes(x = poptotal, y = popasian))+
  geom_point()+
  xlim(0,500000) +
  ylim(0,10000)

#평균막대그래프
library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy= mean(hwy))
df_mpg
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy))+
  geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy))+
  geom_col()

