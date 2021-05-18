---
title: "k-nearest practice"
author: "Fabian"
date: "18/05/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## install caret package
```{install caret}
install.packages("caret", dependencies = T)
library(caret)
```
* caret 패키지는 다른 패키지를 참조하므로, depenedencies 설정 필요

## Caret 함수
### trainControl
trainControl(
  method = "repeatedcv",
  number = 10,
  repeats =5
)

### expand.grid  
expand.grid(k = 1:10)

### train
train(
  class~.,
  data = train,
  method = "knn", 
  trControl = trainControl(),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k = 1:10),
  metric = "Accuracy"
)

