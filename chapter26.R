library(tidyverse)

# 26.1 Caret Basics
# 26.2 Caret Options

# 예측변수 응답변수

## 26.2.1 caret Training Controls

library(caret)

ctrl <- trainControl(method = "repeatedcv",
                     repeats=3,
                     number=5,
                     summaryFunction=defaultSummary,
                     allowParallel=TRUE)

## 26.2.2 caret Search Grid

gamGrid <- tibble(select=c(TRUE, TRUE, FALSE, FALSE),
                  method=c('GCV.Cp', 'REML', 'GCV.Cp', 'REML'))
gamGrid

# 26.3 Tuning a Boosted Tree
acs <- as_tibble(
  read.table("http://jaredlander.com/data/acs_ny.csv",
             sep=",", header=TRUE, stringsAsFactors = FALSE)
)
acs

library(plyr)
library(dplyr)

acs <- acs %>% 
  mutate(Income=factor(FamilyIncome >= 150000,
                       levels=c(FALSE, TRUE),
                       labels=c("Below", "Above")))

acsFormula <- Income ~ NumChildren + NumRooms + NumVehicles + NumWorkers + OwnRent + ElectricBill + FoodStamp + HeatingFuel

ctrl <- trainControl(method = "repeatedcv",
                     repeats=2,
                     number=5,
                     summaryFunction=twoClassSummary,
                     classProbs=TRUE,
                     allowParallel=FALSE)

boostGrid <- expand.grid(nrounds=100,
                         max_depth=c(2, 6, 10),
                         eta=c(0.01, .1),
                         gamma=c(0),
                         colsample_bytree=1,
                         min_child_weight=1,
                         subsample=.7)

set.seed(73615)
boostTuned <- train(acsFormula, data=acs,
                    method="xgbTree",
                    metric="ROC",
                    trControl=ctrl,
                    tuneGrid=boostGrid, nthread=4)

boostTuned$results %>% 
  arrange(ROC)

plot(boostTuned)

library(xgboost)

xgb.plot.multi.trees(boostTuned$finalModel,
                     feature_names=boostTuned$coefnames)

acsNew <- read_csv("http://www.jaredlander.com/data/acsNew.csv")

predict(boostTuned, newdata=acsNew, type="raw") %>% 
  head

predict(boostTuned, newdata=acsNew, type="prob") %>% 
  head

# 26.4 Conclusion
