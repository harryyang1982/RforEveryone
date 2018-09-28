# 25.1 K-means

wineUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine <- read.table(wineUrl, header=FALSE, sep=',',
                   stringsAsFactors = FALSE,
                   col.names=c("Cultivar", "Alcohol", "Malic.acid",
                               "Ash", "Alcalinity.of.ash",
                               "Magnesium", "Total.phenols",
                               "Flavanoids", "Nonflavnoid.phenols",
                               "Proanthocyanin", "Color.intensity",
                               "Hue", "OD280.OD315.of.diluted.wines",
                               "Proline"))
head(wine)

library(tidyverse)

wineTrain <- wine %>% 
  select(-Cultivar)

set.seed(278613)
wineK3 <- kmeans(x=wineTrain, centers=3)
wineK3

library(useful)
plot(wineK3, data=wineTrain)

# iris
# data(iris)
# 
# irisTrain <- iris %>% 
#   select(-Species)
# 
# set.seed(20180928)
# irisK3 <- kmeans(x=irisTrain, centers=4)
# plot(irisK3, data=irisTrain)

set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers=3, nstart=25)
# see the cluster sizes with 1 start
wineK3$size

# see the cluster sizes with 25 starts
wineK3N25$size

wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25,
                      seed=278613)
wineBest

PlotHartigan(wineBest)

table(wine$Cultivar, wineK3N25$cluster)

plot(table(wine$Cultivar, wineK3N25$cluster),
     main="Confusion Matrix for Wine Clustering",
     xlab="Cultivar", ylab="Cluster")

library(cluster)
theGap <- clusGap(wineTrain, FUNcluster=pam, K.max=20)
gapDF <- as_tibble(theGap$Tab)
gapDF

#logW curves
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=logW), color="blue") +
  geom_point(aes(y=logW), color="blue") +
  geom_line(aes(y=E.logW), color="green") +
  geom_point(aes(y=E.logW), color="green") +
  labs(x="Number of Clusters")

# gap curve
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=gap), color="red") +
  geom_point(aes(y=gap), color="red") +
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
  labs(x="Number of Clusters", y="Gap")

# 25.2 PAM

library(WDI)

indicators <- c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG",
                "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
                "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG",
                "TG.VAL.TOTL.GD.ZS")

# pull info on these indicators for all countries in our list
# not all countries have information for every indicator
# some countries do not have any data
wbInfo <- WDI(country="all", indicator=indicators, start=2011,
              end=2011, extra=TRUE)

library(tidyverse)

wbInfo <- wbInfo %>% 
  filter(region != "Aggregates")
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ]
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ]

# set rownames so we know the country without using that for clustering
rownames(wbInfo) <- wbInfo$iso2c
# refactorize region, income and lending
# this accounts for any changes in the levels
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)

# find which columns to keep
# not those in this vector
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",
                                         "capital", "iso3c"))
# fit the clustering
wbPam <- pam(x=wbInfo[, keep.cols], k=12,
             keep.diss=TRUE, keep.data=TRUE)

# show the medoid observations
wbPam$medoids

# make a silhouette plot
plot(wbPam, which.plots=2, main="")

# download.file(url="http://jaredlander.com/data/worldmap.zip",
#               destfile="data/worldmap.zip", method="curl")
# unzip(zipfile="data/worldmap.zip", exdir="data")

library(maptools)
world <- readShapeSpatial("data/world_country_admin_boundary_shapefile_with_fips_codes.shp")

library(tidyverse)

world@data$FipsCntry <- as.character(
  recode(world@data$FipsCntry,
         AU="AT", AS="AU", VM="VN", BM="MM", SP="ES",
         PO="PT", IC="IL", SF="ZA", TU="TR", IZ="IQ",
         UK="GB", EI="IE", SU="SD", MA="MG", MO="MA",
         JA="JP", SW="SE", SN="SG")
)

# make an id column using the rownames
world@data$id <- rownames(world@data)

# convert into a data.frame
library(broom)
world.df <- tidy(world, region="id")
head(world.df)

world.df <- world.df %>% 
  left_join(world@data[, c("id", "CntryName", "FipsCntry")],
            by="id")
world.df
head(world.df)

clusterMembership <- tibble(FipsCntry=names(wbPam$clustering),
                            Cluster=wbPam$clustering)

head(clusterMembership)

world.df <- left_join(world.df, clusterMembership, by="FipsCntry")
world.df$Cluster <- as.character(world.df$Cluster)
world.df$Cluster <- factor(world.df$Cluster, levels=1:12)

ggplot() +
  geom_polygon(data=world.df, aes(x=long, y=lat, group=group,
                                  fill=Cluster, color=Cluster)) +
  labs(x=NULL, y=NULL) + coord_equal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks=element_blank(), panel.background=element_blank())

wbPam$clusinfo

# 25.3 Hierarchical Clustering

wineH <- hclust(d=dist(wineTrain))
plot(wineH)

# calculate distance
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",
                                         "capital", "iso3c"))
wbDaisy <- daisy(x=wbInfo[, keep.cols])

wbH <- hclust(wbDaisy)
plot(wbH)

wineH1 <- hclust(d=dist(wineTrain), method="single")
wineH2 <- hclust(d=dist(wineTrain), method="complete")
wineH3 <- hclust(d=dist(wineTrain), method="average")
wineH4 <- hclust(d=dist(wineTrain), method="centroid")

plot(wineH1, labels=FALSE, main="Single")
plot(wineH2, labels=FALSE, main="Complete")
plot(wineH3, labels=FALSE, main="Average")
plot(wineH4, labels=FALSE, main="Centroid")

# plot the tree
plot(wineH)
# split into 3 clusters
rect.hclust(wineH, k=3, border="red")
rect.hclust(wineH, k=13, border="blue")

# plot the tree
plot(wineH)
# split into 3 clusters
rect.hclust(wineH, h=200, border="red")
# split into 13 clusters
rect.hclust(wineH, h=800, border="blue")
