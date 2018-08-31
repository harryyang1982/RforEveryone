# 13.1 map

theList <- list(A=matrix(1:9, nrow = 3), B = 1:5, C=matrix(1:4, nrow = 2), D = 2)
theList

lapply(theList, sum)

library(tidyverse)

theList %>% 
  map(sum)

identical(lapply(theList, sum), theList %>% map(sum))

theList2 <- theList
theList2[[1]][2, 1] <- NA
theList2[[2]][4] <- NA

theList2 %>% map(sum)

theList2 %>% map(function(x) sum(x, na.rm = TRUE))
theList2 %>% map(sum, na.rm=TRUE)

identical(theList2 %>% map(function(x) sum(x, na.rm = TRUE)),
          theList2 %>% map(sum, na.rm=TRUE))

# 13.2 map with Specified Types

## 13.2.1 map_int

theList %>% map_int(NROW)

theList %>% map_int(mean)

## 13.2.2 map_dbl

theList %>% map_dbl(mean)

## 13.2.3 map_chr

theList %>% map_chr(class)

theList3 <- theList

theList3[['E']] <- factor(c("A", "B", "C"), ordered = TRUE)

class(theList3$E)

theList3 %>%  map_chr(class)

theList3 %>% map(class)

## 13.2.4 map_lgl

theList %>% map_lgl(function(x) NROW(x) < 3)
# theList %>% map_int(NROW)

## 13.2.5 map_df

buildDF <- function(x) {
  data.frame(A=1:x, B=x:1)
}

listOfLengths <- list(3, 4, 1, 5)

listOfLengths %>% map(buildDF)

listOfLengths %>% map_df(buildDF)

## 13.2.6 map_if

theList %>% map_if(is.matrix, function(x) x*2)

# theList %>% map(function(x) x*2)

theList %>% map_if(is.matrix, ~ .x*2)

# 13.3 Iterating over a data.frame

data(diamonds)

diamonds %>% map_if(is.numeric, mean)
diamonds %>% map_dbl(mean)

diamonds %>% 
  summarize_each(funs(mean))

diamonds %>% 
  summarize_all(mean)

# 13.4 map with Multiple Inputs

firstList <- list(A=matrix(1:16, 4), B=matrix(1:16, 2), C=1:5)
secondList <- list(A=matrix(1:16, 4), B=matrix(1:16, 8), C=15:1)

simpleFunc <- function(x, y) {
  NROW(x) + NROW(y)
}

map2(firstList, secondList, simpleFunc)
map2_int(firstList, secondList, simpleFunc)

pmap(list(firstList, secondList), simpleFunc)
pmap_int(list(firstList, secondList), simpleFunc)

