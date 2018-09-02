library(tidyverse)

rnorm(n=10)
rnorm(n=10, mean=100, sd=20)

randNorm10 <- rnorm(10)
dnorm(randNorm10)

dnorm(c(-1, 0, 1))

randNorm <- rnorm(3000)
randDensity <- dnorm(randNorm)

randTable <- tibble(randNorm, randDensity)

ggplot(randTable, aes(x = randNorm, y = randDensity)) +
  geom_point() + labs(x = "Random Normal Variables", 
                      y = "Density")

pnorm(randNorm10)
pnorm(c(-3, 0, 3))
pnorm(-1)

pnorm(1) - pnorm(0)
pnorm(1) - pnorm(-1)

p <- ggplot(randTable) + aes(x = randNorm, y = randDensity) +
  geom_line() + labs(x = "x", y = "Density")

neg1Seq <- seq(from=min(randNorm), to=-1, by=.1)

lessThanNeg1 <- tibble(x=neg1Seq, y=dnorm(neg1Seq))

head(lessThanNeg1)

lessThanNeg1 <- rbind(c(min(randNorm), 0),
                          lessThanNeg1,
                          c(max(lessThanNeg1$x), 0))

p + geom_polygon(data=lessThanNeg1, aes(x = x, y = y))

# create a similar sequence going from -1 to 1
neg1Pos1Seq <- seq(from = -1, to = 1, by = .1)

neg1To1 <- tibble(x=neg1Pos1Seq, y = dnorm(neg1Pos1Seq))

head(neg1To1)

neg1To1 <- rbind(c(min(neg1To1$x), 0),
                 neg1To1,
                 c(max(neg1To1$x), 0))

p + geom_polygon(data=neg1To1, aes(x=x, y=y))

randProb <- pnorm(randNorm)
ggplot(tibble(x=randNorm, y=randProb)) + aes(x=x, y=y) +
  geom_point() + labs(x="Random Normal Variables", y="Probability")

randNorm10
qnorm(pnorm(randNorm10))

all.equal(randNorm10, qnorm(pnorm(randNorm10)))

# 17.2 Binomial Distribution

rbinom(n = 1, size = 10, prob = .4)

rbinom(n = 1, size = 10, prob = .4)

rbinom(n = 5, size = 10, prob = .4)

rbinom(n = 10, size = 10, prob = .4)

rbinom(n=1, size=1, prob=.4)
rbinom(n=5, size=1, prob=.4)
rbinom(n=10, size=1, prob=.4)

binomData <- tibble(Successes=rbinom(n=10000, size=10, prob=.3))

ggplot(binomData, aes(x=Successes)) +
  geom_histogram(binwidth = 1)

binom5 <- tibble(Successes=rbinom(n=10000, size=5, prob=.3), Size=5)
dim(binom5)

binom10 <- tibble(Successes=rbinom(n=10000, size=10, prob=.3), Size=10)

dim(binom10)

head(binom10)

binom100 <- tibble(Successes=rbinom(n=10000, size=100, prob=.3), Size=100)
binom1000 <- tibble(Successes=rbinom(n=10000, size=1000, prob=.3), Size=1000)

binomAll <- bind_rows(binom5, binom10, binom100, binom1000)

dim(binomAll)

head(binomAll)
tail(binomAll)

ggplot(binomAll, aes(x=Successes)) + geom_histogram() +
  facet_wrap(~Size, scales = "free")

dbinom(x=3, size=10, prob=.3)
pbinom(q=3, size=10, prob=.3)
dbinom(x=1:10, size=10, prob=.3)
pbinom(q=1:10, size=10, prob=.3)

qbinom(p=.3, size=10, prob=.3)
qbinom(p=c(.3, .35, .4, .5, .6), size=10, prob=.3)

# 17.3 Poisson Distribution

library(tidyverse)

lambda <- list(1, 2, 5, 10, 20)
n <- as.list(rep(10000, 5))
args <- list(n, lambda)

pois_df <- args %>% 
  pmap_dfc(rpois) %>% 
  rename(lambda1 = V1,
         lambda2 = V2,
         lambda5 = V3,
         lambda10 = V4,
         lambda20 = V5) %>% 
  gather(lambda, x, lambda1:lambda20) %>% 
  mutate(lambda = as.factor(as.numeric(str_extract(string=lambda,
                              pattern="\\d+"))))

ggplot(pois_df, aes(x=x)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ lambda) + ggtitle("Probability Mass Function")

ggplot(pois_df, aes(x=x)) +
  geom_density(aes(group=lambda, color=lambda, fill=lambda), 
               adjust = 4, alpha = 1/2) +
  scale_color_discrete() + scale_fill_discrete() +
  ggtitle("Probability Mass Function")

