# 20.1 Logistic Regression

library(tidyverse)

acs <- read_csv("http://jaredlander.com/data/acs_ny.csv")

acs <- acs %>% 
  mutate(FamilyIncome = parse_number(FamilyIncome),
         NumBedrooms = parse_number(NumBedrooms),
         NumChildren = parse_number(NumChildren),
    Income = FamilyIncome >= 150000)

library(useful)

ggplot(acs, aes(x=FamilyIncome)) +
  geom_density(fill="grey", color="grey") +
  geom_vline(xintercept = 150000) +
  scale_x_continuous(labels = multiple.dollar, limits=c(0, 1000000))

head(acs)

income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType,
               data=acs, family=binomial(link = "logit"))
summary(income1)

library(coefplot)
coefplot(income1)

invlogit(income1$coefficients)

# 20.2 Poisson Regression

ggplot(acs, aes(x=NumChildren)) + geom_histogram(binwidth = 1)

children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data=acs, family=poisson(link="log"))
summary(children1)

coefplot(children1)


# the standarized residuals
z <- (acs$NumChildren - children1$fitted.values) /
  sqrt(children1$fitted.values)
# Overdispersion Factor
sum(z^2) / children1$df.residual

# Overdispersion p-value
pchisq(sum(z^2), children1$df.residual)

children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data=acs, family=quasipoisson(link = "log"))
multiplot(children1, children2)

# 20.3 Other Generalized Linear Models

# 20.4 Survival Analysis

library(survival)
head(bladder)

bladder[100:105,]

# now look at the response variable built by build.y
survObject <- with(bladder[100:105, ], Surv(stop, event))

# Surv(stop, event, data = bladder %>% slice(100:105))

survObject
survObject[, 1:2]

cox1 <- coxph(Surv(stop, event) ~ rx + number + size + enum,
              data=bladder)

summary(cox1)
plot(survfit(cox1), xlab = "Days", ylab = "Survival Rate",
     conf.int = TRUE)

cox2 <- coxph(Surv(stop, event) ~ strata(rx) + number +
                size + enum, data=bladder)
summary(cox2)

plot(survfit(cox1), xlab = "Days", ylab = "Survival Rate",
     conf.int = TRUE, col = 1:2)
legend("bottomleft", legend=c(1, 2), lty=1, col=1:2,
       text.col=1:2, title="rx")

cox.zph(cox1)

cox.zph(cox2)

head(bladder2)

ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size + enum + 
               cluster(id), data=bladder2)
ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum + cluster(id), data = bladder2)

plot(survfit(ag1), conf.int = TRUE)
plot(survfit(ag2), conf.int = TRUE, col = 1:2)
legend("topright", legend=c(1, 2), lty = 1, col = 1:2,
       text.col = 1:2, title = "rx")
