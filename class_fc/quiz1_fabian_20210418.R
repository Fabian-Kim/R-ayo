#Week1 Qhiz 제출합니다. - Fabian Kim

library(dplyr)
library(ggplot2)

#1
head(mpg,10)

#2
num <- seq(1:50)
num1 <- num[num%%5==0]
num1

#3
year <- "2021"
month <- "05"
day <-"06"
paste(year,"-",month,"-",day)

#4
sq * 3

#5
dim(diamonds)

#6
str(diamonds)

#7
summary(diamonds)

#8
install.packages("readxl")
library(readxl)
kchous <- read_excel("C:/Users/Fabian/Desktop/rstudy/Study Note/Week1/kc_house_data.xlsx")

#9
kchous %>%
select(id,date,price,bedrooms)
