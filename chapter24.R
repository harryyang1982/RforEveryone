# 24.1 Autoregressive Moving Average

# load the World Bank API package

library(WDI)

# pull the data
gdp <- WDI(country=c("US", "CA", "GB", "DE", "CN",  "JP", "SG", "IL"),
           indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start=1960, end=2011)

# give it good names
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
head(gdp)

library(tidyverse)
library(scales)

ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) +
  geom_line() + scale_y_continuous(label=dollars)

library(useful)
# absolute GDP
ggplot(gdp, aes(Year, GDP, color=Country, linetype=Country)) +
  geom_line() +
  scale_y_continuous(labels=multiple_format(extra=dollar,
                                            multiple="M"))

# get US data
us <- gdp$PerCapGDP[gdp$Country == "United States"]

# convert it to a time series
us <- ts(us, start=min(gdp$Year), end=max(gdp$Year))
us

plot(us, ylab="Per Capita GDP", xlab="Year")

acf(us)
pacf(us)

x <- c(1, 4, 8, 2, 6, 6, 5, 3)
# one diff
diff(x, differences = 1)
# two iterative diffs
diff(x, differences = 2)
# equivalent to one diff
diff(x, lag=1)
# diff elements that are two indices apart
diff(x, lag=2)

library(forecast)
ndiffs(x=us)
plot(diff(us, 2))

usBest <- auto.arima(x=us)
usBest

acf(usBest$residuals)
pacf(usBest$residuals)

coef(usBest)

# predict 5 years into the future and include the standard error
predict(usBest, n.ahead=5, se.fit=TRUE)

# make a prediction for 5 years out
theForecast <- forecast(object=usBest, h=5)
# plot it
plot(theForecast)

# 24.2 VAR

gdpCast <- gdp %>% 
  select(Year, Country, PerCapGDP) %>% 
  spread(Country, PerCapGDP)

# remove first 10 rows since Germany did not have

# convert to time series
gdpTS <- ts(data=gdpCast[, -1], start=min(gdpCast$Year),
            end=max(gdpCast$Year))

# build a plot and legend using base graphics
plot(gdpTS, plot.type="single", col=1:8)
legend("topleft", legend=colnames(gdpTS), ncol=2, lty=1,
       col=1:8, cex=.9)

gdpTS <- gdpTS[, which(colnames(gdpTS) != "Germany")]
gdpTS

numDiffs <- ndiffs(gdpTS)
numDiffs

gdpDiffed <- diff(gdpTS, differences=numDiffs)
plot(gdpDiffed, plot.type="single", col=1:7)
legend("bottomleft", legend=colnames(gdpDiffed), ncol=2, lty=1,
       col=1:7, cex=.9)

library(vars)
# fit the model
gdpVar <- VAR(gdpDiffed, lag.max=12)
# chosen order
gdpVar$p

# names of each of the models
names(gdpVar$varresult)

# each model is actually an lm object
class(gdpVar$varresult$Canada)
class(gdpVar$varresult$Japan)

# each model has its own coefficients
head(coef(gdpVar$varresult$Canada))
head(coef(gdpVar$varresult$Japan))

library(coefplot)
coefplot(gdpVar$varresult$Canada)
coefplot(gdpVar$varresult$Japan)

predict(gdpVar, n.ahead=5)

# 24.3 GARCH

library(quantmod)
att <- getSymbols("T", auto.assign=FALSE)

library(xts)
# show data
head(att)

plot(att)

chartSeries(att)
addBBands()
addMACD(32, 50, 12)

attClose <- att$T.Close
class(attClose)

head(attClose)

library(rugarch)
attSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1, 1)),
                      mean.model=list(armaOrder=c(1, 1)),
                                      distribution.model="std")
attSpec

attGarch <- ugarchfit(spec=attSpec, data=attClose)
attGarch

# attGarch is an S4 object so its slots are accessed by @
# the slot fit is a list, its elements are accessed by the dollar sign
plot(attGarch@fit$residuals, type="l")
plot(attGarch, which=10)

# ARMA(1, 1)
attSpec1 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(1, 1)),
                       distribution.model="std")
# ARMA(0, 0)
attSpec2 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(0, 0)),
                       distribution.model="std")
# ARMA(0, 2)
attSpec3 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(0, 2)),
                       distribution.model="std")
# ARMA(1, 2)
attSpec4 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(1, 2)),
                       distribution.model="std")

attGarch1 <- ugarchfit(spec=attSpec1, data=attClose)
attGarch2 <- ugarchfit(spec=attSpec2, data=attClose)
attGarch3 <- ugarchfit(spec=attSpec3, data=attClose)
attGarch4 <- ugarchfit(spec=attSpec4, data=attClose)

infocriteria(attGarch1)
infocriteria(attGarch2)
infocriteria(attGarch3)
infocriteria(attGarch4)

attPred <- ugarchboot(attGarch, n.ahead=50,
                      method=c("Partial", "Full")[1])
plot(attPred, which=2)

# diff the logs, drop the first one which is now NA
attLog <- diff(log(attClose))[-1]
# build the specification
attLogSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                             garchOrder=c(1, 1)),
                         mean.model=list(armaOrder=c(1, 1)),
                         distribution.model="std")
# fit the model
attLogGarch <- ugarchfit(spec=attLogSpec, data=attLog)
infocriteria(attLogGarch)
