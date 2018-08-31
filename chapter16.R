paste(c("Hello", "Hey", "Howdy"), c("Jared", "Bob", "David"))

paste("Hello", c("Jared", "Bob", "David"))

paste("Hello", c("Jared", "Bob", "David"), c("Goodbye", "Seeya"))

vectorOfText <- c("Hello", "Everyone", "out there", ".")
paste(vectorOfText, collapse = " ")
paste(vectorOfText)

paste(vectorOfText, collapse = "*")

# 16.2 sprintf

person <- "Jared"
partySize <- "eight"
waitTime <- 25

paste("Hello ", person, ", your party of ", partySize,
      " will be seated in ", waitTime, " minutes.", sep="")

sprintf("Hello %s, your party of %s will be seated in %s minutes", person, partySize, waitTime)
sprintf("Hello %s, your party of %s will be seated in %s minutes", c("Jared", "Bob"), c("eight", 16, "four", 10), waitTime)

# 16.3 Extracting Text

library(XML)

load("data/presidents.rdata")

theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
presidents <- readHTMLTable(theURL, which = 3, as.data.frame = TRUE,
                            skip.rows = 1, header = TRUE,
                            stringsAsFactors = FALSE)

library(tidyverse)

head(presidents)
presidents <- presidents %>% 
  slice(1:64)

yearList <- str_split(presidents$YEAR, pattern = "-")
yearList

yearMatrix <- data.frame(Reduce(rbind, yearList))

names(yearMatrix) <- c("Start", "Stop")

presidents <- bind_cols(presidents, yearMatrix)

presidents <- presidents %>% 
  mutate(Start = as.numeric(as.character(Start)),
         Stop = as.numeric(as.character(Stop))) 

str(presidents)

presidents %>% 
  mutate(PRESIDENT = str_sub(PRESIDENT, start=1L, end=3L))

presidents %>% 
  mutate(PRESIDENT = str_sub(PRESIDENT, start=4L, end=8L))

presidents %>% 
  filter(str_sub(Start, start=4, end=4) == 1) %>% 
  select(YEAR, PRESIDENT, Start, Stop)

# 16.4 Regular Expressions

johnPos <- str_detect(presidents$PRESIDENT, pattern = "John")

presidents[johnPos, c("YEAR", "PRESIDENT", "Start", "Stop")]

presidents %>% 
  filter(str_detect(PRESIDENT, pattern = "John")) %>% 
  select(YEAR, PRESIDENT, Start, Stop)
  
badSearch <- str_detect(presidents$PRESIDENT, "john")
goodSearch <- str_detect(presidents$PRESIDENT, regex("john", ignore_case = TRUE))

sum(badSearch)
sum(goodSearch)

con <- url("http://www.jaredlander.com/data/warTimes.rdata")
load(con)
close(con)

head(warTimes, 10)

warTimes[str_detect(warTimes, pattern = "-")]

theTimes <- str_split(string=warTimes, pattern="(ACAEA)|-", n = 2)
theTimes

which(str_detect(string=warTimes, pattern="-"))

theTimes[[147]]
theTimes[[150]]

# theStart <- sapply(theTimes, FUN=function(x) x[1])
theStart <- theTimes %>% 
  map_chr(function(x) x[1])

head(theStart)
theStart <- str_trim(theStart)
head(theStart)

str_extract(string=theStart, pattern="January")

theStart[str_detect(theStart, pattern = "January")]

head(str_extract(theStart, "[0-9][0-9][0-9][0-9]"), 20)
head(str_extract(theStart, "\\d{4}"), 20)

str_extract(theStart, "\\d{1,3}")

head(str_extract(theStart, pattern="^\\d{4}"), 30)
head(str_extract(theStart, pattern="\\d{4}$"), 30)
head(str_extract(theStart, pattern="^\\d{4}$"), 30)

head(str_replace(string=theStart, pattern = "\\d", replacement="x"), 30)
head(str_replace_all(string=theStart, pattern = "\\d", replacement="x"), 30)

head(str_replace_all(string=theStart, pattern = "\\d{1,4}", replacement="x"), 30)

commands <- c("<a href=index.html>The Link is here</a>",
              "<b>This is bold text</b>")

# get the text between the HTML tags
# the content in (.+?) is substituted using \\1

str_replace(string=commands, pattern="<.+?>(.+?)<.+>",
            replacement="\\1")
?regex
