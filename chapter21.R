# 21.1 Residuals

library(tidyverse)

housing <- read_csv("data/housing.csv")

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt",
                    "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt", 
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")

housing <- filter(housing, Units < 1000)
head(housing)

# fit a model
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

# visualize the model
library(coefplot)
coefplot(house1)

head(fortify(house1))

# save a plot to an object
# notice we are using the created columns for the x- and y-axes
# they are .fitted and .resid

h1 <- ggplot(aes(x=.fitted, y=.resid), data = house1) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Values", y = "Residuals")

# print the plot
h1

h1 + geom_point(aes(color=Boro))

# basic plot
plot(house1, which=1)
plot(house1, which=1, col=as.numeric(factor(house1$model$Boro)))

legend("topright", legend=levels(factor(house1$model$Boro)), pch=1,
       col=as.numeric(factor(levels(factor(house1$model$Boro)))),
       text.col = as.numeric(factor(levels(factor(house1$model$Boro)))),
       title = "Boro")

plot(house1, which=2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()

ggplot(house1, aes(x=.resid)) + geom_histogram()
# Histogram of residuals from house1. This does not look normally distributed, meaning our model is incomplete.

# 21.2 Comparing Models

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data=housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class, data=housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data=housing)

multiplot(house1, house2, house3, house4, house5, pointSize = 2)

anova(house1, house2, house3, house4, house5)

AIC(house1, house2, house3, house4, house5)

BIC(house1, house2, house3, house4, house5)

# create the binary variable based on whether ValuePerSqFt is above 150
housing$HighValue <- housing$ValuePerSqFt >= 150

high1 <- glm(HighValue ~ Units + SqFt + Boro,
             data=housing, family=binomial(link="logit"))
high2 <- glm(HighValue ~ Units * SqFt + Boro,
             data=housing, family=binomial(link="logit"))
high3 <- glm(HighValue ~ Units + SqFt * Boro + Class,
             data=housing, family=binomial(link="logit"))
high4 <- glm(HighValue ~ Units + SqFt * Boro + SqFt * Class,
             data=housing, family=binomial(link="logit"))
high5 <- glm(HighValue ~ Boro + Class,
             data=housing, family=binomial(link="logit"))

anova(high1, high2, high3, high4, high5)
AIC(high1, high2, high3, high4, high5)
BIC(high1, high2, high3, high4, high5)

# 21.3 Cross-Validation

library(boot)
# refit house1 using glm instead of lm
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro,
               data=housing, family=gaussian(link="identity"))
identical(coef(house1), coef(houseG1))

# run the cross-validation with 5 folds
houseCV1 <- cv.glm(housing, houseG1, K=5)
houseCV1$delta

houseG2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
houseG3 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data=housing)
houseG4 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class, data=housing)
houseG5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing)

houseCV2 <- cv.glm(housing, houseG2, K=5)
houseCV3 <- cv.glm(housing, houseG3, K=5)
houseCV4 <- cv.glm(housing, houseG4, K=5)
houseCV5 <- cv.glm(housing, houseG5, K=5)

cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta,
                                 houseCV3$delta, houseCV4$delta,
                                 houseCV5$delta))
# do some cleaning up to make the results more presentable give better column names
names(cvResults) <- c("Error", "Adjusted.Error")

cvResults$Model <- sprintf("houseG%s", 1:5)
cvResults

# visualize the results
# test with ANOVA
cvANOVA <- anova(houseG1, houseG2, houseG3, houseG4, houseG5)
cvResults$ANOVA <- cvANOVA$`Resid. Dev`

cvResults$AIC <- AIC(houseG1, houseG2, houseG3, houseG4, houseG5)$AIC

# make the data.frame suitable for plotting

cvMelt <- cvResults %>% 
  select(Model, everything()) %>% 
  gather(Measure, Value, Error:AIC)

ggplot(cvMelt, aes(x=Model, y=Value)) +
  geom_line(aes(group=Measure, color=Measure)) + 
  facet_wrap(~Measure, scales="free_y") +
  theme(axis.text.x=element_text(angle=90, vjust=.5)) +
  guides(color=FALSE)

cv.work <- function(fun, k = 5, data,
                    cost = function(y, yhat) mean((y - yhat)^2),
                    response="y", ...) {
  # generate folds
  folds <- tibble(Fold=sample(rep(x=1:k, length.out=nrow(data))),
                  Row=1:nrow(data))
  
  # start the error at 0
  error <- 0
  
  ## loop through each of the folds
  ## for each fold:
  ## fit the model on the training data
  ## predict on the test data
  ## compute the error and accumulate it
  for (f in 1:max(folds$Fold)) {
    # rows that are in test set
    theRows <- folds$Row[folds$Fold == f]
    
    ## call fun on data[-theRows, ]
    ## predict on data[theRows, ]
    mod <- fun(data=data[-theRows, ], ...)
    pred <- predict(mod, data[theRows, ])
    
    # add new error weighted by the number of rows in this fold
    error <- error +
      cost(data[theRows, response], pred) * (length(theRows) / nrow(data))
  }
  return(error)
}

cv1 <- cv.work(fun = lm, k = 5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt + Boro)
cv2 <- cv.work(fun = lm, k = 5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units * SqFt + Boro)
cv3 <- cv.work(fun = lm, k = 5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + Class)
cv4 <- cv.work(fun = lm, k = 5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class)
cv5 <- cv.work(fun = lm, k = 5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Boro + Class)

cvResults <- data.frame(Model=sprintf("house%s", 1:5),
                        Error=c(cv1, cv2, cv3, cv4, cv5))

# 21.4 Bootstrap

data(baseball, package="plyr")
baseball <- filter(baseball, year >= 1990)

baseball

bat.avg <- function(data, indices=1:NROW(data), hits="h", at.bats="ab") {
  sum(data[indices, hits], na.rm=TRUE) / sum(data[indices, at.bats], na.rm=TRUE)
}

bat.avg(baseball)

avgBoot <- boot(data=baseball, statistic = bat.avg, R=1200, stype="i")

# print original measure and estimates of bias and standard error
avgBoot

# print the confidence interval
boot.ci(avgBoot, conf=.95, type="norm")

ggplot() +
  geom_histogram(aes(x=avgBoot$t), fill="grey", color="grey") +
  geom_vline(xintercept = avgBoot$t0 + c(-1, 1) * 2 * sqrt(var(avgBoot$t)),
             linetype=2)

# 21.5 Stepwise Variable Selection

nullModel <- lm(ValuePerSqFt ~ 1, data=housing)
fullModel <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Boro*Class, data=housing)

# try different modesl
# start with nullModel
# do not go above fullModel
# work in both direction
houseStep <- step(nullModel,
                  scope=list(lower=nullModel, upper=fullModel),
                  direction="both")
houseStep
