q <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")
nchar(q)
y <- -4:5
y
nchar(y)
c(One="a", Two="y", Last="r")

w <- 1:3
names(w) <- c("a", "b", "c")
w

## 4.4.2 Factor Vectors

q2 <- c(q, "Hockey", "Lacrosse", "Hockey", "Water Polo",
        "Hockey", "Lacrosse")

q2Factor <- as.factor(q2)
q2Factor

as.numeric(q2Factor)

factor(x=c("High School", "College", "Masters", "Doctorate"),
       levels = c("High School", "College", "Masters", "Doctorate"),
       ordered = TRUE)

# 4.5 Calling Functions

# 4.6 Function Documentation

apropos("mea")

# 4.7 Missing Data

## 4.7.1 NA
z <- c(1, 2, NA, 8, 3, NA, 3)
z

is.na(z)

zChar <- c("Hockey", NA, "Lacrosse")
zChar

is.na(zChar)

mean(z)
mean(z, na.rm = TRUE)

## 4.7.2 NULL

z <- c(1, NULL, 3)
z
d <- NULL
is.null(d)

is.null(7)

# 4.8 Pipes

library(tidyverse)

x <- 1:10
mean(x)

x %>% mean
z <- c(1, 2, NA, 8, 3, NA, 3)

sum(is.na(z))
z %>% is.na %>% sum
z %>% sum(na.rm = T)

z %>% mean(na.rm = T)

