# 11.1 Apply Family
## 11.1.1 Apply

theMatrix <- matrix(1:9, nrow = 3)
theMatrix

apply(theMatrix, 1, sum)
apply(theMatrix, 2, sum)

rowSums(theMatrix)
colSums(theMatrix)

theMatrix[2, 1] <- NA
apply(theMatrix, 1, sum)
apply(theMatrix, 1, sum, na.rm = TRUE)
rowSums(theMatrix)
rowSums(theMatrix, na.rm = TRUE)

## 11.1.2 lapply and sapply

theList <- list(A=matrix(1:9, 3), B=1:5, C=matrix(1:4, 2), D=2)
theList

lapply(theList, sum)

sapply(theList, mean)
sapply(theList, sum)

theNames <- c("Jared", "Deb", "Paul")
lapply(theNames, nchar)
sapply(theNames, nchar)

## 11.1.3 mapply

firstList <- list(A=matrix(1:16, 4), B=matrix(1:16, 2), C=1:5)
secondList <- list(A=matrix(1:16, 4), B=matrix(1:16, 8), C=15:1)

mapply(identical, firstList, secondList)

simpleFunc <- function(x, y) {
  NROW(x) + NROW(y)
}

mapply(simpleFunc, firstList, secondList)

# 11.2 aggregate

data(diamonds, package = "ggplot2")
head(diamonds)

aggregate(price ~ cut, diamonds, mean)

aggregate(price ~ cut + color, diamonds, mean)

aggregate(cbind(price, carat) ~ cut, diamonds, mean)

aggregate(cbind(price, carat) ~ cut + color, diamonds, mean)

# 11.3 plyr

## 11.3.1 ddply

library(plyr)

head(baseball)

# subsetting with [ is faster than using ifelse]
baseball$sf[baseball$year < 1954] <- 0

any(is.na(baseball$sf))

# set NA hbp's to 0
baseball$hbp[is.na(baseball$hbp)] <- 0

# check that it worked
any(is.na(baseball$hbp))

# only keep players with at least 50 at bats in a season
baseball <- baseball[baseball$ab >= 50, ]

baseball$OBP <- with(baseball, (h + bb + hbp) / (ab + bb + hbp + sf))
tail(baseball)

obp <- function(data) {
  c(OBP=with(data, sum(h + bb + hbp) / sum(ab + bb + hbp + sf)))
}

careerOBP <- ddply(baseball, .variables = "id", .fun = obp)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE), ]

head(careerOBP, 10)

## 11.3.2 llply

theList <- list(A=matrix(1:9, 3), B=1:5, C=matrix(1:4, 2), D=2)
lapply(theList, sum)

llply(theList, sum)

identical(lapply(theList, sum), llply(theList, sum))

sapply(theList, sum)
laply(theList, sum)

## 11.3.3 plyr Helper Functions

data(diamonds, package = "ggplot2")

aggregate(price ~ cut, diamonds, each(mean, median))

system.time(dlply(baseball, "id", nrow))

iBaseball <- idata.frame(baseball)
system.time(dlply(iBaseball, "id", nrow))

## 11.3.4 Speed versus Convenience

# 11.4 data.table

library(data.table)

theDF <- data.frame(A=1:10,
                    B=letters[1:10],
                    C=LETTERS[11:20],
                    D=rep(c("One", "Two", "Three"), length.out = 10))
theDT <- data.table(A=1:10,
                    B=letters[1:10],
                    C=LETTERS[11:20],
                    D=rep(c("One", "Two", "Three"), length.out = 10))

theDF
theDT

class(theDF$B)
class(theDT$B)

diamondsDT <- data.table(diamonds)
diamondsDT

theDT[1:2,]
theDT[theDT$A >= 7,]
theDT[A >=7,]

theDT[, list(A, C)]
theDT[, B]

theDT[, list(B)]
theDT[, "B", with=FALSE]

theDT[, c("A", "C"), with=FALSE]
theCols <- c("A", "C")
theDT[, theCols, with=FALSE]

## 11.4.1 Keys

tables()

setkey(theDT, D)
theDT

key(theDT)
tables()

theDT["One",]

theDT[c("One", "Two"), ]

setkey(diamondsDT, cut, color)

diamondsDT[J("Ideal", "E"), ]

diamondsDT[J("Ideal", c("E", "D")), ]

## 11.4.2 data.table Aggregation

aggregate(price ~ cut, diamonds, mean)

diamondsDT[, mean(price), by=cut]

diamondsDT[, list(price=mean(price)), by = cut]

diamondsDT[, list(price=mean(price)), by=list(cut, color)]

diamondsDT[, list(price=mean(price), carat=mean(carat)), by=cut]
diamondsDT[, list(price=mean(price), carat=mean(carat),
                  caratSum=sum(carat)), by = cut]

diamondsDT[, list(price=mean(price), carat=mean(carat)), 
           by=list(cut, color)]
