library(tidyverse)

# 22.1 Elastic Net

acs <- read_csv("data/acs.csv")

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
  mutate(Income = FamilyIncome >= 150000)

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

# plot the path
plot(acsCV1$glmnet.fit, xvar="lambda")
# add in vertical lines for the optimal values of lambda
abline(v=log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty=2)

# fit the ridge model
set.seed(71623)
acsCV2 <- cv.glmnet(x=acsX, y=acsY, family="binomial",
                    nfolds = 5, alpha = 0)

# look at the lambda values
acsCV2$lambda.min
acsCV2$lambda.1se

# look at the coefficients
coef(acsCV2, s="lambda.1se")

# plot the cross-validation error path
plot(acsCV2)

# plot the coefficient path
plot(acsCV2$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty=2)

library(parallel)
library(doParallel)

# set the seed for repeatability of random results
set.seed(2834673)

# create folds
# we want observations to be in the same fold each time it is run
theFolds <- sample(rep(x=1:5, length.out=nrow(acsX)))

# make sequence of alpha values
alphas <- seq(from=.5, to=1, by=.05)

# set the seed for repeatability of random results
set.seed(5127151)

# start a cluster with two workers
cl <- makeCluster(2)
# register the workers
registerDoParallel(cl)

# keep track of timing
before <- Sys.time()

# build foreach loop to run in parallel
## several arguments
acsDouble <- foreach(i=1:length(alphas), .errorhandling = "remove",
                     .inorder = FALSE, .multicombine = TRUE,
                     .export = c("acsX", "acsY", "alphas", "theFolds"),
                     .packages = "glmnet") %dopar%
                     {
                       print(alphas[i])
                       cv.glmnet(x=acsX, y=acsY, family="binomial", nfolds=5,
                                 foldid=theFolds, alpha=alphas[i])
                     }

# stop timing
after <- Sys.time()

# make sure to stop the cluster when done
stopCluster(cl)

# time difference
# this will depend on speed, memory & number of cores of the machine
after - before

sapply(acsDouble, class)

# function for extracting info from cv.glmnet object

extractGlmnetInfo <- function(object) {
  # find lambdas
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  # figure out where those lambdas fall in the path
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  # build a one line data.frame with each of the selected lambdas and
  # its corresponding error figures
  data.frame(lambda.min=lambdaMin, error.min=object$cvm[whichMin],
             lambda.1se=lambda1se, error.1se=object$cvm[which1se])
}

# apply that function to each element of the list
# combine it all into a data.frame
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))

# could also be done with ldply from plyr
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical(alphaInfo, alphaInfo2)

# make a column listing the alphas
alphaInfo$Alpha <- alphas
alphaInfo

alphaMelt <- alphaInfo %>% 
  select(Alpha, everything()) %>% 
  gather(Measure, Value, lambda.min:error.1se)

alphaMelt$Type <- str_extract(string=alphaMelt$Measure,
                              pattern="(min)|(1se)")
alphaMelt

# some housekeeping
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure,
                                 pattern="\\.(min|1se)",
                                 replacement="")
# alphaCast <- reshape2::dcast(alphaMelt, Alpha + Type ~ Measure,
#                    value.var="Value")
alphaCast <- alphaMelt %>% 
  spread(Measure, Value)
alphaCast

ggplot(alphaCast, aes(x=Alpha, y=error)) +
  geom_line(aes(group=Type)) +
  facet_wrap(~Type, scales="free_y", ncol=1) +
  geom_point(aes(size=lambda))

set.seed(5127151)
acsCV3 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5,
                    alpha=alphaInfo$Alpha[which.min(alphaInfo$error.1se)])

plot(acsCV3)
plot(acsCV3$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV3$lambda.min, acsCV3$lambda.1se)), lty = 2)

theCoef <- as.matrix(coef(acsCV3, s="lambda.1se"))
coefDF <- data.frame(Value=theCoef, Coefficients=rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV3, s="lambda.1se")), ]
ggplot(coefDF, aes(x=X1, y=reorder(Coefficients, X1))) +
  geom_vline(xintercept=0, color="grey", linetype=2) +
  geom_point(color="blue") +
  labs(x="Value", y="Coefficient", title="Coefficient Plot")

# 22.2 Bayesian Shrinkage

download.file("http://jaredlander.com/data/ideo.rdata",
              "data/ideo.rdata")

load("data/ideo.rdata")

head(ideo)
results <- ideo %>% 
  group_by(Year) %>% 
  do(Model=glm(Vote ~ Race + Income + Gender + Education,
               data=.,
               family=binomial(link="logit")))

results

library(coefplot)

voteInfo <- multiplot(results$Model,
                      coefficients="Raceblack", plot=FALSE)

names(results$Model) <- as.character(results$Year)

head(voteInfo)

# plot it restricting the window to (-20, 10)
multiplot(results$Model,
          coefficients = "Raceblack", secret.weapon = TRUE) +
  coord_flip(xlim=c(-20, 10))


resultsB <- ideo %>% 
  group_by(Year) %>% 
  do(Model=arm::bayesglm(Vote ~ Race + Income + Gender + Education,
                         data=.,
                         family=binomial(link="logit"),
                         prior.scale=2.5, prior.df=1))

names(resultsB$Model) <- as.character(results$Year)

# build the coefficient plot
multiplot(resultsB$Model, coefficients="Raceblack", secret.weapon = TRUE)
