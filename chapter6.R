# 6.1 reading CSVs

theUrl <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato <- read.table(file=theUrl, header=TRUE, sep=",")

head(tomato)

x <- 10:1
y <- -4:5
q <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")

theDF <- data.frame(First=x, Second=y, Sport=q,
                    stringsAsFactors = FALSE)
theDF$Sport

## 6.1.1 read_delim

library(tidyverse)
theUrl <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato2 <- read_delim(theUrl, delim=',')

tomato2

## 6.1.2 fread

library(data.table)
theUrl <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato3 <- fread(input=theUrl, sep=',', header=TRUE)

head(tomato3)
tomato3
str(tomato3)

# 6.2 Excel Data

download.file(url = "http://www.jaredlander.com/data/ExcelExample.xlsx",
              destfile='data/ExcelExample.xlsx')

library(readxl)
excel_sheets("data/ExcelExample.xlsx")

tomatoXL <- read_excel("data/ExcelExample.xlsx")
tomatoXL

wineXL1 <- read_excel("data/ExcelExample.xlsx", sheet=2)
wineXL1

wineXL2 <- read_excel("data/ExcelExample.xlsx", sheet="Wine")
head(wineXL2)

# tomatoXL2 <- read_excel("http://www.jaredlander.com/data/ExcelExample.xlsx")
## excel file is not directly downloaded from web

# 6.3 Reading from Databases
download.file("http://www.jaredlander.com/data/diamonds.db",
              destfile="data/diamonds.db", mode="wb")

library(RSQLite)
drv <- dbDriver("SQLite")
class(drv)

con <- dbConnect(drv, 'data/diamonds.db')
class(con)

dbListTables(con)
dbListFields(con, name='diamonds')
dbListFields(con, name='DiamondColors')

# simple SELECT * query from one table

diamondsTable <- dbGetQuery(con,
                            "SELECT * FROM diamonds",
                            stringsAsFactors=FALSE)

# simple SELECT * query from one table

colorTable <- dbGetQuery(con,
                         "SELECT * FROM DiamondColors",
                         stringsAsFactors=FALSE)

# do a join between the two tables
longQuery <- "SELECT * FROM diamonds, DiamondColors
                       WHERE
                       diamonds.color = DiamondColors.Color"

diamondsJoin <- dbGetQuery(con, longQuery,
                           stringsAsFactors=FALSE)

head(diamondsTable)
head(colorTable)
head(diamondsJoin)

# 6.4 Data from Other Statistical Tools
# 6.5 R Binary Files

save(tomato, file="data/tomato.rdata")
rm(tomato)
head(tomato)

load("data/tomato.rdata")
head(tomato)

n <- 20
r <- 1:10
w <- data.frame(n, r)

n
r
w

save(n, r, w, file="data/multiple.rdata")
rm(n, r, w)

n
r
w

load("data/multiple.rdata")
n
r
w

smallVector <- c(1, 5, 4)
smallVector

saveRDS(smallVector, file='thisObject.rds')
thatVect <- readRDS("thisobject.rds")
thatVect

identical(smallVector, thatVect)

# 6.6 Data Included with R

library(tidyverse)

data(diamonds, package='ggplot2')
head(diamonds)

# 6.7 Extract Data from Web Sites

## 6.7.1 Simple HTML Tables

library(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE,
                          stringsAsFactors=FALSE)

## 6.7.2 Scraping Web Data

library(rvest)
ribalta <- read_html("http://www.jaredlander.com/data/ribalta.html")
ribalta
class(ribalta)

ribalta %>% html_nodes("ul") %>% html_nodes("span")

ribalta %>% html_nodes(".street")

ribalta %>% html_nodes(".street") %>% html_text()
ribalta %>% html_nodes("#longitude") %>% html_attr("value")

ribalta %>% 
  html_nodes("table.food-items") %>% 
  magrittr::extract2(5) %>% 
  html_table()

# 6.8 Reading JSON Data

library(jsonlite)
pizza <- fromJSON("http://www.jaredlander.com/data/PizzaFavorites.json")
pizza

class(pizza)
class(pizza$Name)
class(pizza$Details)

class(pizza$Details[[1]])

pizza$Details[[1]]
