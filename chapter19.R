library(tidyverse)

# 19.1 Simple Linear Regression

data(father.son, package='UsingR')

head(father.son)

ggplot(father.son, aes(x=fheight, y=sheight)) + 
  geom_point() +
  geom_smooth(method = "lm") + labs(x="Fathers", y="Sons")

heightsLM <- lm(sheight ~ fheight, data=father.son)
heightsLM

summary(heightsLM)

## 19.1.1 ANOVA Alternative

data(tips, package="reshape2")
head(tips)

tipsAnova <- aov(tip ~ day - 1, data=tips)

tipsLM <- lm(tip ~ day - 1, data=tips)

summary(tipsAnova)
summary(tipsLM)

tipsByDay <- tips %>% 
  group_by(day) %>% 
  summarize(tip.mean = mean(tip), 
            tip.sd = sd(tip),
            Length=NROW(tip),
            tfrac=qt(p=.90, df=Length-1),
            Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
            Upper=tip.mean + tfrac*tip.sd/sqrt(Length))

tipsByDay

tipsInfo <- summary(tipsLM)
# tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2])
# tipsCoef <- within(tipsCoef, {
#   Lower <- Estimate - qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
#   Upper <- Estimate + qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
#   day <- rownames(tipsCoef)
# })
# tipsCoef

tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2]) %>% 
  rownames_to_column() %>% 
  rename(day=rowname) %>% 
  mutate(Lower = Estimate - qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`,
         Upper = Estimate + qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`)

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
  ggtitle("Tips by day calculated manually")

ggplot(tipsCoef, aes(x=Estimate, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
  ggtitle("Tips by day calculated from regression model")

# 19.2 Multiple Regression

housing <- read_csv("http://www.jaredlander.com/data/housing.csv")

names(housing) <- c("Neightborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFT", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value", 
                    "ValuePerSqFt", "Boro")
housing

ggplot(housing, aes(x=ValuePerSqFt)) +
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot")

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot")

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
  geom_histogram(binwidth = 10) + labs(x="Value per Square Foot") +
  facet_wrap(~Boro)

ggplot(housing, aes(x=SqFt)) + geom_histogram()
ggplot(housing, aes(x=Units)) + geom_histogram()
ggplot(housing %>% filter(Units < 1000), aes(x=SqFt)) +
  geom_histogram()
ggplot(housing %>% filter(Units < 1000), aes(x=Units)) +
  geom_histogram()

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing %>% filter(Units < 1000), aes(x=SqFt, y=ValuePerSqFt)) +
  geom_point()
ggplot(housing %>% filter(Units < 1000), aes(x=Units, y=ValuePerSqFt)) +
  geom_point()

sum(housing$Units >= 1000)

# remove them

housing <- housing %>% filter(Units < 1000)

# plot ValuePerSqFt against SqFt
ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=SqFt, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=log(ValuePerSqFt))) + geom_point()

# plot ValuePerSqFt aginast Units
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(Units), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(Units), y=ValuePerSqFt)) + geom_point()

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

# three things below are the same
house1$coefficients
coef(house1)
coefficients(house1)

library(coefplot)
coefplot(house1)

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data=housing)

coef(house2)
coef(house3)

coefplot(house2)
coefplot(house3)

house4 <- lm(ValuePerSqFt ~ SqFt * Units * Income, data=housing)
coef(house4)

house5 <- lm(ValuePerSqFt ~ Class*Boro, data=housing)
house5$coefficients

coefplot(house1, sort = "mag") + scale_x_continuous(limits=c(-.25, .1))
coefplot(house1, sort = "mag") + scale_x_continuous(limits=c(-.0005, .0005))

house1.b <- lm(ValuePerSqFt ~ scale(Units) + scale(SqFt) + Boro,
               data=housing)
coefplot(house1.b, sort="mag")

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, data=housing)
coef(house6)

house7 <- lm(ValuePerSqFt ~ (Units + SqFt)^2, housing)
house7$coefficients

house8 <- lm(ValuePerSqFt ~ Units * SqFt, housing)
coef(house8)

identical(house7$coefficients, house8$coefficients)

house9 <- lm(ValuePerSqFt ~ I(Units + SqFt)^2, housing)
house9$coefficients

# as from the coefplot package
multiplot(house1, house2, house3)

housingNew <- read_csv("http://www.jaredlander.com/data/housingNew.csv")

housePredict <- predict(house1, newdata=housingNew, se.fit = TRUE,
                        interval = "prediction", level = .95)

head(housePredict$fit)
head(housePredict$se.fit)
