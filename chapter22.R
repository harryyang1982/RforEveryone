library(tidyverse)

# 22.1 Elastic Net

acs <- read_csv("http://jaredlander.com/data/acs_ny.csv")

# build a data.frame where ten first three columns are numeric

testFrame <- data.frame(First=sample(1:10, 20, replace=T),
                        Second=sample(1:20, 20, replace=T),
                        Third=sample(1:10, 20, replace=T),
                        Fourth=factor(rep(c("Alice", "Bob", "Charlie", "David"), 5)),
                        Fifth=ordered(rep(c("Edward", "Frank", "Georgia", "Hank", "Issac"), 4)),
                        Sixth=rep(c("a", "b"), 10), stringsAsFactor=F)
testFrame

head(model.matrix(First ~ Second + Fourth + Fifth, testFrame))

library(useful)

# always use all levels
head(build.x(First ~ Second + Fourth + Fifth, testFrame,
             contrasts = FALSE))

# just use all levels for Fourth
head(build.x(First ~ Second + Fourth + Fifth, testFrame, 
             contrasts=c(Fourth=FALSE, Fifth=TRUE)))

# make a binary Income variable for building a logistic regression
acs <- acs %>% 
  mutate(FamilyIncome = parse_number(FamilyIncome),
         Insurance = parse_number(Insurance),
         ElectricBill = parse_number(ElectricBill),
         NumBedrooms = parse_number(NumBedrooms),
         NumChildren = parse_number(NumChildren),
         NumPeople = parse_number(NumPeople),
         NumRooms = parse_number(NumRooms),
         Income = FamilyIncome >= 150000)

# build predictor matrix
# do not include the intercept as glmnet will add that automatically
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople +
                  NumRooms + NumUnits + NumVehicles + NumWorkers + 
                  OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + 
                  Insurance + Language - 1,
                data=acs, contrasts = FALSE)
acsX

class(acsX)
dim(acsX)
topleft(acsX, c=6)
topright(acsX, c=6)

# build response predictor
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople +
                  NumRooms + NumUnits + NumVehicles + NumWorkers + 
                  OwnRent + YearBuilt + ElectricBill + FoodStamp +
                  HeatingFuel + Insurance + Language - 1, data=acs)
head(acsY)
tail(acsY)

library(glmnet)
set.seed(1863561)

acsCV1 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5)

acsCV1$lambda.min

acsCV1$lambda.1se

plot(acsCV1)
coef(acsCV1, s="lambda.1se")

