### R code for final project
### Dropbox containing variable definitions: https://www.dropbox.com/scl/fo/f185czaubsejosyyfhl9e/h?rlkey=qqvg0w0pkl26bzpq86lsbox6b&dl=0


###################################################
### Loading data and setting directory
###################################################
setwd("/Users/justinpuleio/Downloads/Rutgers Files/Data Science Files")
wdidata20 <- read.csv("wdidata20.csv", stringsAsFactors = T)

###################################################
### Testing various variables for entries
###################################################
##test 1
wdidata20.test <- subset(wdidata20, SE.ADT.LITR.ZS >= 0 & SL.TLF.0714.ZS)
nrow(wdidata20.test)

##test 2
wdidata20.test2 <- subset(wdidata20, SE.ADT.LITR.ZS >= 0 & 
                              EG.ELC.ACCS.ZS >= 0 & IT.NET.USER.ZS >= 0)
nrow(wdidata20.test2)

##test 3
wdidata20.test3 <- subset(wdidata20, SE.ADT.LITR.ZS >= 0 & 
                              EG.ELC.ACCS.ZS >= 0 & IT.NET.USER.ZS >= 0 & 
                              SH.DYN.AIDS.ZS >= 0 & NY.GDP.MKTP.CD >= 0)
nrow(wdidata20.test3)

###################################################
### Creating new variables
###################################################
##subset data to only show entries with "complete" information 
#based on variables of study
wdidata20.quality <- subset(wdidata20, SE.ADT.LITR.ZS >= 0 & 
                              EG.ELC.ACCS.ZS >= 0 & IT.NET.USER.ZS >= 0 & 
                               SH.DYN.AIDS.ZS >= 0 & NY.GDP.MKTP.CD >= 0)


##creating inverse of HIV population variable
wdidata20.quality$non.HIV.population <- rep(NA, length(wdidata20.quality$X))

##taking average of four variables to establish "quality of life" model
#done for every entry via loop
wdidata20.quality$life.quality <- rep(NA, length(wdidata20.quality$X))
for (i in 1:length(wdidata20.quality$X)){
  wdidata20.quality$non.HIV.population[i] <- 
    100 - wdidata20.quality$SH.DYN.AIDS.ZS[i]
  wdidata20.quality$life.quality[i] <- (wdidata20.quality$SE.ADT.LITR.ZS[i] + 
                                          wdidata20.quality$SH.DYN.AIDS.ZS[i]
                                         + wdidata20.quality$IT.NET.USER.ZS[i]
                                         + wdidata20.quality$EG.ELC.ACCS.ZS[i]
                                         ) / 4
}

###################################################
### Calculating prediction errors
###################################################
##creating secondary data set to contain one entry per country
list <- subset(wdidata20.quality, select=c(country,region))
countries <- unique(list)

##creating new list of unique countries to ensure appropriate matchmaking 
#between data sets
countryname <- countries$country

##calculating prediction error via RMSE method
#stored in single-entry data set
countries$prediction.error <- rep(NA, length(countries))
for (i in 1:length(countries$country)){
  country <- subset(wdidata20.quality, country == countryname[i])
  fit <- lm(life.quality ~ NY.GDP.MKTP.CD, data = country)
  countries$prediction.error[i] <-sqrt(mean(fit$residuals^2))
}

##observing prediction error data
mean(countries$prediction.error)
range(countries$prediction.error)

###################################################
### Removing countries with two or less entries
###################################################
##observing number of entries per country
listfrequency <- data.frame(table(list$country))

##categorizing countries with two or less entries under "false zero" data set
falsezero <- subset(listfrequency[listfrequency$Freq <= 2,])
countries$false.zero <- rep(NA, length(countries))
countries$false.zero <- ifelse(countries$country %in% falsezero$Var1, 1, 0)

##creating new data frame without "false zero" countries
countries2 <- subset(countries, false.zero != 1)

##observing new prediction error data
mean(countries2$prediction.error)
range(countries2$prediction.error)

###################################################
### Plotting prediction error by region
###################################################
##removing unused data to clean plot
countries <- droplevels(countries)
countries2 <- droplevels(countries2)

##plotting all countries including "false zero" prediction errors
boxplot(prediction.error ~ region, data = countries,
        xlab= "",
        ylab= "Prediction Error",
        main= "Prediction Error by Region: \n With False Zeroes",
        las=2,
        names= c("E. Asia \n & Pacific","Eur. &   \nCent. Asia","Lat. Amer.
                 \nCaribbean", "MENA","S. Asia","Sub-Sah.\n Africa   "))

##plotting countries without "false zero" prediction errors
boxplot(prediction.error ~ region, data = countries2,
        xlab= "",
        ylab= "Prediction Error",
        main= "Prediction Error by Region: \n Without False Zeroes",
        las=2,
        names= c("E. Asia \n & Pacific","Eur. &   \nCent. Asia","Lat. Amer.
                 \nCaribbean", "MENA","S. Asia","Sub-Sah.\n Africa   "))

##removing additional label in title
boxplot(prediction.error ~ region, data = countries2,
        xlab= "",
        ylab= "Prediction Error",
        main= "Prediction Error by Region",
        las=2,
        names= c("E. Asia \n & Pacific","Eur. &   \nCent. Asia","Lat. Amer.
                 \nCaribbean", "MENA","S. Asia","Sub-Sah.\n Africa   "))

###################################################
### Selecting sample countries
###################################################
##observing life quality data
range(wdidata20.quality$life.quality)
mean(wdidata20.quality$life.quality)

##subset data to contain only the four selected countries
selectexamples <- subset(wdidata20.quality, country == "Brazil" | 
                          country == "Greece" | country == "Chad" | country == 
                           "Qatar")

##running linear regressions for each country
brazil <- subset(wdidata20.quality, country == "Brazil")
fit2 <- lm(life.quality ~ NY.GDP.MKTP.CD, data = brazil)
greece <- subset(wdidata20.quality, country == "Greece")
fit3 <- lm(life.quality ~ NY.GDP.MKTP.CD, data = greece)
chad <- subset(wdidata20.quality, country == "Chad")
fit4 <- lm(life.quality ~ NY.GDP.MKTP.CD, data = chad)
qatar <- subset(wdidata20.quality, country == "Qatar")
fit5 <- lm(life.quality ~ NY.GDP.MKTP.CD, data = qatar)

###################################################
### Sample graph
###################################################
##plotting sample countries
plot(selectexamples$NY.GDP.MKTP.CD, selectexamples$life.quality,
     col= ifelse(selectexamples$country == "Brazil", "forestgreen", 
                 ifelse(selectexamples$country == "Greece", "lightblue1", 
                        ifelse(selectexamples$country == "Chad", "gold", 
                               "red3"))),
     ylab= "Life Quality",
     xlab= "GDP (USD)",
     main= "Quality of Life Based on GDP",
     pch= 19,
     cex.axis= .9)

##adding regression models for each country to plot
abline(fit2, col="forestgreen")
abline(fit3, col="lightblue1")
abline(fit4, col="gold")
abline(fit5, col="red3")

##creating a legend
legend("bottomright", col= c("forestgreen", "lightblue1", "gold", "red3"), 
       c("Brazil", "Greece", "Chad", "Qatar"),
       bty= "n",
       lwd= 2,
       cex= .9)

##observing prediction error for each regression model
sqrt(mean(fit2$residuals^2))
sqrt(mean(fit3$residuals^2))
sqrt(mean(fit4$residuals^2))
sqrt(mean(fit5$residuals^2))

###################################################
### Calculating means
###################################################
##creating mean life quality and mean gdp variables per country
countries2$mean.lq <- rep(NA, length(countries2$country))
countries2$mean.gdp <- rep(NA, length(countries2$country))

##creating new list of unique countries to ensure appropriate matchmaking 
#between data sets
countryname2 <- countries2$country
countryname2 <- as.character(countryname2)

##loop which subsets and takes mean of life quality and gdp 
#based on all entries for a particular country
for (i in 1:length(countries2$country)){
  country2 <- subset(wdidata20.quality, country == countryname2[i])
  countries2$mean.lq[i] <- mean(country2$life.quality)
  countries2$mean.gdp[i] <- mean(country2$NY.GDP.MKTP.CD)
}

##observing significance of mean prediction error on mean life quality
mean(countries2$prediction.error) / mean(countries2$mean.lq)

##subset countries based on region
eap <- subset(countries2, region == "East Asia & Pacific")
eca <- subset(countries2, region == "Europe & Central Asia")
lac <- subset(countries2, region == "Latin America & Caribbean")
mena <- subset(countries2, region == "Middle East & North Africa")
sa <- subset(countries2, region == "South Asia")
ssa <- subset(countries2, region == "Sub-Saharan Africa")

##mean prediction error and life quality for East Asia/Pacific
mean(eap$prediction.error)
mean(eap$mean.lq)
mean(eap$prediction.error) / mean(eap$mean.lq)

##mean prediction error and life quality for Europe/Central Asia
mean(eca$prediction.error)
mean(eca$mean.lq)
mean(eca$prediction.error) / mean(eca$mean.lq)

##mean prediction error and life quality for Latin America/Caribbean
mean(lac$prediction.error)
mean(lac$mean.lq)
mean(lac$prediction.error) / mean(lac$mean.lq)

##mean prediction error and life quality for Middle East/North Africa
mean(mena$prediction.error)
mean(mena$mean.lq)
mean(mena$prediction.error) / mean(mena$mean.lq)

##mean prediction error and life quality for South Asia
mean(sa$prediction.error)
mean(sa$mean.lq)
mean(sa$prediction.error) / mean(sa$mean.lq)

##mean prediction error and life quality for Sub-Saharan Africa
mean(ssa$prediction.error)
mean(ssa$mean.lq)
mean(ssa$prediction.error) / mean(ssa$mean.lq)

###################################################
### Graphing means
###################################################
##creating plot of mean gdp and mean life quality
plot(countries2$mean.gdp, countries2$mean.lq,
     col= ifelse(countries2$region == "East Asia & Pacific", "purple",
                 ifelse(countries2$region == "Europe & Central Asia", 
                        "dodgerblue", ifelse(countries2$region == 
                                              "Latin America & Caribbean", 
                                            "turquoise", 
                                            ifelse(countries2$region == 
                                                     "Middle East & North 
                                                   Africa", "yellow2", 
                                                   ifelse(countries2$region == 
                                                            "South Asia", 
                                                          "red3", 
                                                          "orange"))))), 
     xlab= "Mean GDP (USD)",
     ylab= "Mean Life Quality",
     main= "Mean Life Quality Against Mean GDP",
     pch= 19)

##adding a legend
legend("bottomright", 
       col= c("purple", "dodgerblue", "turquoise", "yellow2", "red3", "orange"), 
       c("East Asia & Pacific", "Europe & Central Asia", "Latin America & 
         Caribbean", "Middle East & North Africa", "South Asia", 
         "Sub-Saharan Africa"),
       bty= "n",
       lwd= 2,
       cex= .6)

#observing correlation between variables
cor(countries2$mean.gdp, countries2$mean.lq)

###################################################
### Graphing error based on frequency
###################################################
##merging country frequency data set with "countries2" data set 
countries2$entries <- rep(NA, length(countries2$country))
for (i in 1:length(countries2$country)){
  countries2$entries[i] <- listfrequency$Freq[listfrequency$Var1 == 
                                                countryname2[i]]
}

##creating a plot
plot(countries2$entries, countries2$prediction.error,
     col= ifelse(countries2$region == "East Asia & Pacific", "purple",
                 ifelse(countries2$region == "Europe & Central Asia", 
                        "dodgerblue", ifelse(countries2$region == 
                                               "Latin America & Caribbean", 
                                             "turquoise", 
                                             ifelse(countries2$region == 
                                                      "Middle East & North 
                                                   Africa", "yellow2", 
                                                    ifelse(countries2$region == 
                                                             "South Asia", 
                                                           "red3", 
                                                           "orange"))))), 
     xlab= "Entries",
     ylab= "Prediction Error",
     main= "Prediction Error Based on No. of Entries",
     pch= 19)

##creating a legend
legend("bottomright", 
       col= c("purple", "dodgerblue", "turquoise", "yellow2", "red3", "orange"), 
       c("East Asia & Pacific", "Europe & Central Asia", "Latin America & 
         Caribbean", "Middle East & North Africa", "South Asia", 
         "Sub-Saharan Africa"),
       bty= "n",
       lwd= 2,
       cex= .4)

##observing correlation between variables
cor(countries2$entries, countries2$prediction.error)

###################################################
### Creating predictions with model
###################################################
##creating variable identifying year with most recent gdp entry
countries2$most.recent.year <- rep(NA, length(countries2$country))

##creating variable measuring years to life quality 100
countries2$years.to.100 <- rep(NA, length(countries2$country))

##creating variable identifying expected year to reach quality of life of 100
countries2$expected.100.year <- rep(NA, length(countries2$country))

##loop calculating years for countries to reach quality of life of 100 
#utilizing gdp trends from model
for (i in 1:length(countries2$country)){
  #predicting required gdp for quality of life of 100
  country2 <- subset(wdidata20.quality, country == countryname2[i])
  fit <- lm(NY.GDP.MKTP.CD ~ life.quality, data = country2)
  prediction <- predict(fit, data.frame(life.quality = 100))
  #subset to find mean gdp change over time
  country3 <- subset(wdidata20, country == countryname2[i] & 
                       NY.GDP.MKTP.CD >= 0)
  meangdpchange <- mean(country3$NY.GDP.MKTP.KD.ZG)
  #identify most recent year and its gdp
  recentyear <- subset(country3, year == max(country3$year))
  recentgdp <- recentyear$NY.GDP.MKTP.CD
  #solving for time
  equation1 <- (prediction - recentgdp) / recentgdp
  equation2 <- 1 + (meangdpchange / 100)
  countries2$years.to.100[i] <- ifelse(meangdpchange > 0, log(equation1) / 
                                         log(equation2), NA)
  #calculating expected year to reach 100
  countries2$most.recent.year[i] <- recentyear$year
  countries2$expected.100.year[i] <- ifelse(meangdpchange > 0, recentyear$year 
                                            + countries2$years.to.100[i], NA)
  countries2$expected.100.year[i] <- round(countries2$expected.100.year[i], 
                                           digits = 0)
}

###################################################
### Graphing predictions
###################################################
##loading relevant packages
library(maps)
library(ggplot2)

##adding expected year to worldmap data
worldmap <- map_data("world")
worldmap$expected.100.year <- rep(NA, length(worldmap$order))
for (i in 1:length(worldmap$order)){
  expectedyear <- subset(countries2, country == worldmap$region[i])
  worldmap$expected.100.year[i] <- ifelse(worldmap$region[i] %in% countryname2, 
                                          expectedyear$expected.100.year, NA)
}

##creating the graph
range(countries2$expected.100.year, na.rm = T)
ggplot()+
  geom_polygon(data=worldmap, mapping =aes(x=long, y=lat, group=group,
                                             fill=expected.100.year),
               colour="black")+
  scale_fill_gradient2(name="Year", low="green", mid="blue", high="red",
                       limits= c(2016,2238), midpoint= 2023)+
  ggtitle("Expected Year to Reach Maximum Life Quality")+
  coord_quickmap()+
  theme_void()