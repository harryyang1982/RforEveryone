# 14.1 cbind and rbind

# 14.2 Joins

# download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
#               destfile = "data/ForeignAid.zip")
# unzip("data/ForeignAid.zip", exdir="data")

library(tidyverse)
library(stringr)

theFiles <- dir("data/", pattern = "\\.csv")

for(a in theFiles) {
  nameToUse <- str_sub(string=a, start=12, end=18)
  temp <- read.table(file=file.path("data", a),
                     header=TRUE, sep=",", stringsAsFactors = FALSE)
  assign(x=nameToUse, value=temp)
}

## 14.2.1 merge

Aid90s00s <- merge(x=Aid_90s, y=Aid_00s,
                   by.x = c("Country.Name", "Program.Name"),
                   by.y = c("Country.Name", "Program.Name"))
head(Aid90s00s)

## 14.2.2 plyr join

library(plyr)
Aid90s00sJoin <- join(x=Aid_90s, y=Aid_00s,
                      by=c("Country.Name", "Program.Name"))
head(Aid90s00sJoin)

# first figure out the names of the data.frames

frameNames <- str_sub(string=theFiles, start=12, end=18)

# build an empty list
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames

for(a in frameNames) {
  frameList[[a]] <- eval(parse(text=a))
}

head(frameList[[1]])
head(frameList[["Aid_00s"]])
head(frameList[[5]])
head(frameList[["Aid_60s"]])

allAid <- Reduce(function(...) {
  join(..., by=c("Country.Name", "Program.Name"))},
  frameList)

library(useful)
corner(allAid, c=15)
bottomleft(allAid, c=15)

## 14.2.3 data.table merge

library(data.table)
dt90 <- data.table(Aid_90s, key=c("Country.Name", "Program.Name"))
dt00 <- data.table(Aid_00s, key=c("Country.Name", "Program.Name"))

dt0090 <- dt90[dt00]

# 14.3 reshape2

## 14.3.1 melt

head(Aid_00s)

library(reshape2)

melt00 <- melt(Aid_00s, id.vars = c("Country.Name", "Program.Name"),
               variable.name = "Year", value.name = "Dollars")
melt00

tail(melt00, 10)

Aid_00s
allAid

tidy_all <- tibble::as_tibble(allAid)

melt_all <- tidy_all %>% 
  tidyr::gather("year", "dollars", `FY2000`:`FY1999`)

melt_all %>% 
  tail(10)

library(scales)

melt00$Year <- as.numeric(str_sub(melt00$Year, start=3, 6))
meltAgg <- aggregate(Dollars ~ Program.Name + Year, data=melt00,
                     sum, na.rm = TRUE)

meltAgg$Program.Name <- str_sub(meltAgg$Program.Name, start=1, end=10)

ggplot(meltAgg, aes(x=Year, y=Dollars)) +
  geom_line(aes(group=Program.Name)) +
  facet_wrap(~ Program.Name) +
  scale_x_continuous(breaks=seq(from=2000, to=2009, by=2)) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=0)) +
  scale_y_continuous(labels = multiple_format(extra=dollar,
                                              multiple="B"))

## 14.3.2 dcast

cast00 <- dcast(melt00, Country.Name + Program.Name ~ Year,
                value.var = "Dollars")
head(cast00)
