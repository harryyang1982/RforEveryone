# 18.1 Summary Statistics

x <- sample(x=1:100, size=100, replace=TRUE)

x %>% 
  mean()

y <- x

y[sample(x=1:100, size=20, replace=FALSE)] <- NA
y

y %>% 
  mean()

y %>% 
  mean(na.rm=TRUE)

grades <- c(95, 72, 87, 66)
weights <- c(1/2, 1/4, 1/8, 1/8)

mean(grades)
weighted.mean(x=grades, w=weights)

var(x)

sum((x - mean(x)) ^ 2) / (length(x) - 1)

sqrt(var(x))

sd(x)

sd(y)
sd(y, na.rm = TRUE)

min(x)
max(x)
median(x)
min(y)
min(y, na.rm = TRUE)

summary(x)
summary(y)

quantile(x, probs=c(.25, .75))

quantile(y, probs=c(.25, .75))

quantile(y, probs=c(.25, .75), na.rm = TRUE)

quantile(x, probs=c(.1, .25, .5, .75, .99))

# 18.2 Correlation and Covariance

library(tidyverse)

head(economics)

cor(economics$pce, economics$psavert)
economics

cor(economics[, c(2, 4:6)])

cor(economics$pce, economics$psavert)

xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)

nMinusOne <- nrow(economics) - 1
xSD <- sqrt(sum(xPart ^2) / nMinusOne)
ySD <- sqrt(sum(yPart ^2) / nMinusOne)

sum(xPart * yPart) / (nMinusOne * xSD * ySD)


library(GGally)

ggpairs(economics[, c(2, 4:6)])

econCor <- cor(economics[, c(2, 4:6)])
econMelt <- econCor %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(x = rowname) %>% 
  gather(y, Correlation, pce:unemploy)

ggplot(econMelt, aes(x=x, y=y)) +
  geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low = scales::muted("red"), mid="white",
                       high="steelblue",
                       guide=guide_colorbar(ticks = FALSE, barheight = 10),
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(x=NULL, y=NULL)

theMat <- tibble(m = c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4),
                 n = c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7),
                 p = c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9),
                 q = c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2),
                 r = c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10))

cor(theMat, use="everything")

cor(theMat, use="all.obs")
cor(theMat, use="complete.obs")
cor(theMat, use="na.or.complete")

identical(cor(theMat, use="complete.obs"),
          cor(theMat, use="na.or.complete")
)

cor(theMat[c(1, 4, 7, 9, 10), ])

cor(theMat, use = "pairwise.complete.obs")
cor(theMat[, c("m", "n")], use="complete.obs")

cor(theMat[, c("m", "p")], use="complete.obs")

data(tips, package="reshape2")
head(tips)

ggpairs(tips)

# install.packages("RXKCD")
library(RXKCD)

getXKCD(which="552")

cov(economics$pce, economics$psavert)
cov(economics[, c(2, 4:6)])

# sum(xPart * yPart) / nMinusOne

identical(cov(economics$pce, economics$psavert),
          cor(economics$pce, economics$psavert) *
            sd(economics$pce) * sd(economics$psavert))

# 18.3 T-Tests

head(tips)

unique(tips$sex)

unique(tips$day)

## 18.3.1 One-Sample T-Test

t.test(tips$tip, alternative = "two.sided", mu=2.50)

randT <- rt(30000, df=NROW(tips)-1)
tipTTest <- t.test(tips$tip, alternative="two.sided", mu=2.50)

ggplot(tibble(x=randT)) +
  geom_density(aes(x=x), fill = "grey", color="grey") +
  geom_vline(xintercept = tipTTest$statistic) +
  geom_vline(xintercept = mean(randT) + c(-2, 2) * sd(randT), linetype = 2)

t.test(tips$tip, alternative="greater", mu=2.5)

## 18.3.2 Two-Sample T-Test

aggregate(tip ~ sex, data=tips, var)

tips %>% 
  group_by(sex) %>% 
  summarize(tip = var(tip))

# test for normality of tip distribution
shapiro.test(tips$tip)

shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips %>% filter(sex == "Female") %>% .[["tip"]])
shapiro.test(tips %>% filter(sex == "Male") %>% .[["tip"]])

# all the test fail so inspect visually

ggplot(tips, aes(x=tip, fill=sex)) +
  geom_histogram(binwidth=.5, alpha=1/2)

# use Ansari-Bradley test to examine the equality of variances

ansari.test(tip ~ sex, tips)

# setting var.equal=TRUE runs a standard two sample t-test
# var.equal=FALSE (the default) would run the Welch test
t.test(tip ~ sex, data=tips, var.equal=TRUE)

tipSummary <- tips %>% 
  group_by(sex) %>% 
  summarize(tip.mean = mean(tip),
            tip.sd = sd(tip),
            Lower = tip.mean - 2 * tip.sd/sqrt(NROW(tip)),
            Upper = tip.mean + 2 * tip.sd/sqrt(NROW(tip)))
tipSummary

ggplot(tipSummary, aes(x = tip.mean, y = sex)) + geom_point() +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = .2) 

# 18.3.3 Paired Two-Sample T-Test

data(father.son, package = "UsingR")

library(tidyverse)

head(father.son)

t.test(father.son$fheight, father.son$sheight, paired = TRUE)

heightDiff <- father.son$fheight - father.son$sheight

ggplot(father.son, aes(x = fheight - sheight)) +
  geom_density() +
  geom_vline(xintercept = mean(heightDiff)) +
  geom_vline(xintercept = mean(heightDiff) + 
               2*c(-1, 1)*sd(heightDiff) / sqrt(nrow(father.son)), linetype = 2) 

# 18.4 ANOVA

data(tips, package="reshape2")

tipAnova <- aov(tip ~ day - 1, tips)
tipIntercept <- aov(tip ~ day, tips)
tipAnova$coefficients

tipIntercept$coefficients

summary(tipAnova)

tipsByDay <- tips %>% 
  group_by(day) %>% 
  summarize(tip.mean = mean(tip),
            tip.sd = sd(tip),
            Length=NROW(tip),
            tfrac=qt(p=.90, df=Length-1),
            Lower=tip.mean - tfrac * tip.sd / sqrt(Length),
            Upper=tip.mean + tfrac * tip.sd / sqrt(Length))

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height=.3)

nrow(tips)
NROW(tips)

nrow(tips$tip)
NROW(tips$tip)

