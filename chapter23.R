# 23.1 Nonlinear Least Squares

load("data/wifi.rdata")
wifi

head(wifi)

library(tidyverse)

ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() +
  scale_color_gradient2(low="blue", mid="white", high="red",
                        midpoint = mean(wifi$Distance))

# specify the square root model
# starting values are at the center of the grid
wifiMod1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2),
                data=wifi, start=list(betaX=50, betaY=50))
summary(wifiMod1)

ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() +
  scale_color_gradient2(low="blue", mid="white", high="red",
                        midpoint= mean(wifi$Distance)) +
  geom_point(data=as.data.frame(t(coef(wifiMod1))),
             aes(x=betaX, y=betaY), size=5, color="green")

# 23.2 Splines

data("diamonds")
# fit with a few different degrees of freedom
# the degrees of freedom must be greater than 1
# but less than the number of unique x values in the data
diaSpline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price)
diaSpline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price,
                            df=2)
diaSpline3 <- smooth.spline(x=diamonds$carat, y=diamonds$price,
                            df=10)
diaSpline4 <- smooth.spline(x=diamonds$carat, y=diamonds$price,
                            df=20)
diaSpline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price,
                            df=50)
diaSpline6 <- smooth.spline(x=diamonds$carat, y=diamonds$price,
                            df=100)

get.spline.info <- function(object) {
  data.frame(x=object$x, y=object$y, df=object$df)
}

splineDF <- map_df(list(diaSpline1, diaSpline2, diaSpline3, diaSpline4,
                        diaSpline5, diaSpline6), get.spline.info)
head(splineDF)

g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
g + geom_line(data=splineDF,
              aes(x=x, y=y,color=factor(round(df, 0)), group=df)) +
  scale_color_discrete("Degrees of \nFreedom")

library(splines)
head(ns(diamonds$carat, df=1))
head(ns(diamonds$carat, df=2))
head(ns(diamonds$carat, df=3))
head(ns(diamonds$carat, df=4))

g + stat_smooth(method="lm", formula=y ~ ns(x, 6), color="blue")
g + stat_smooth(method="lm", formula=y ~ ns(x, 3), color="red")

g + stat_smooth()

# 23.3 Generalized Additive Models

creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose",
                 "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits",
                 "Job", "NumLiable", "Phone", "Foreign", "Credit")

# use read.table to read the file
# specify that headers are not included
# the col.names are from creditNames
credit <- read_csv("http://home.cse.ust.hk/~qyang/221/Assignments/German/GermanData.csv")
colnames(credit) <- creditNames
credit

head(credit %>% 
       select(CreditHistory, Purpose, Employment, Credit))

creditHistory <- c(A30="All Paid", A31="All Paid This Bank",
                   A32="Up To Date", A33="Late Payment",
                   A34="Critical Account")

purpose <- c(A40="car (new)", A41="car (used)",
             A42="furniture/equipment", A43="radio/television",
             A44="domestic appliances", A45="repairs", A46="education",
             A47="(vacation - does not exist?)", A48="retraining",
             A49="business", A410="others")

employment <- c(A71="unemployed", A72="< 1 year", A73="1 - 4 years",
                A74="4 - 7 years", A75=">= 7 years")

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]

# code credit as good/bad
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
credit$Credit <- factor(credit$Credit, levels=c("Good", "Bad"))

# after
head(credit %>% select(CreditHistory, Purpose, Employment, Credit))

library(useful)
ggplot(credit, aes(x=CreditAmount, y=Credit)) +
  geom_jitter(position = position_jitter(height = .2)) +
  facet_grid(CreditHistory ~ Employment) +
  xlab("Credit Amount") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(labels=multiple)

ggplot(credit, aes(x=CreditAmount, y=Age)) +
  geom_point(aes(color=Credit)) +
  facet_grid(CreditHistory ~ Employment) +
  xlab("Credit Amount") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(labels=multiple)

library(mgcv)

# fit a logistic GAM
# apply a tensor product on CreditAmount and a spline on Age
creditGam <- gam(Credit ~ te(CreditAmount) + s(Age) + CreditHistory + Employment,
                 data=credit, family=binomial(link="logit"))

summary(creditGam)

plot(creditGam, select=1, se=TRUE, shade=TRUE)
plot(creditGam, select=2, se=TRUE, shade=TRUE)

# 23.4 Decision Trees
library(rpart)

creditTree <- rpart(Credit ~ CreditAmount + Age + 
                      CreditHistory + Employment, data=credit)

creditTree

library(rpart.plot)
rpart.plot(creditTree, extra=4)

# 23.5 Boosted Trees
library(useful)

# we do not need an intercept since it is a tree
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount - 1

# we use all levels of the categorical variables since it is a tree
creditX <- build.x(creditFormula, data=credit, contrasts=FALSE)
creditY <- build.y(creditFormula, data=credit)

# convert the logical vector to [0, 1]
creditY <- as.integer(relevel(creditY, ref='Bad')) - 1

library(xgboost)
creditBoost <- xgboost(data=creditX, label=creditY, max.depth=3,
                       eta=.3, nthread=4, nrounds=3,
                       objective="binary:logistic")

creditBoost20 <- xgboost(data=creditX, label=creditY, max.depth=3,
                         eta=.3, nthread=4, nrounds=20,
                         objective="binary:logistic")

install.packages("DiagrammeR")

xgb.plot.multi.trees(creditBoost, feature_names=colnames(creditX))
xgb.ggplot.importance(xgb.importance(creditBoost,
                                     feature_names=colnames(creditX)))
# xgb.plot.importance(xgb.importance(creditBoost,
#                                      feature_names=colnames(creditX)))

# 23.6 Random Forests
library(randomForest)
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + 
  Duration + Age + CreditAmount - 1
# we use all levels of the categorical variables since it is a tree
creditX <- build.x(creditFormula, data=credit, contrasts=FALSE)
creditY <- build.y(creditFormula, data=credit)

# fit the random forest
creditForest <- randomForest(x=creditX, y=creditY)
creditForest

# build the response matrix
creditY2 <- as.integer(relevel(creditY, ref="Bad")) - 1
# Fit the random forest
boostedForest <- xgboost(data=creditX, label=creditY2, max_depth=4,
                         num_parallel_tree=1000,
                         subsample=0.5, colsample_bytree=0.5,
                         nrounds=3, objective="binary:logistic")
xgb.plot.multi.trees(boostedForest, feature_names=colnames(creditX))
?xgb.plot.multi.trees
