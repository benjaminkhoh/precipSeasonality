library(raster)
library(stringr)
library(rgdal)
library(sp)
library(dplyr)
library(Kendall)
###summer list
junList <- dir(path = ".", pattern = "\\06_bil.bil$")
julList <- dir(path = ".", pattern = "\\07_bil.bil$")
augList <- dir(path = ".", pattern = "\\08_bil.bil$")

summerNames <- as.character(seq(from = 1991, to = 2021))
str_sub(summerNames, start = 0, end = 0) <- "summer"
summerList <- list()
i = 0
for(i in 1:length(junList)){
  tmpJun <- raster(paste0(junList[i]))
  tmpJul <- raster(paste0(julList[i]))
  tmpAug <- raster(paste0(augList[i]))
  summerList[[i]] <-  (tmpJun + tmpJul + tmpAug)
}

for (i in 1:length(summerList)){
  assign(paste(summerNames[i]),summerList[[i]])
}
###
###winter list
decList <- dir(path = ".", pattern = "12_bil.bil$")
janList <- dir(path = ".", pattern = "01_bil.bil$")
febList <- dir(path = ".", pattern = "02_bil.bil$")

winterNames <- as.character(seq(from = 1991, to = 2021))
str_sub(winterNames, start = 0, end = 0) <- "winter"
winterList <- list()

i = 0
for(i in 1:length(decList)){
  tmpDec <- raster(paste0(decList[i]))
  tmpJan <- raster(paste0(janList[i]))
  tmpFeb <- raster(paste0(febList[i]))
  winterList[[i]] <-  (tmpDec + tmpJan + tmpFeb)
}

for (i in 1:length(winterList)){
  assign(paste(winterNames[i]),winterList[[i]])
}
###fall list
sepList <- dir(path = ".", pattern = "09_bil.bil$")
octList <- dir(path = ".", pattern = "10_bil.bil$")
novList <- dir(path = ".", pattern = "11_bil.bil$")

fallNames <- as.character(seq(from = 1991, to = 2021))
str_sub(fallNames, start = 0, end = 0) <- "fall"
fallList <- list()

i = 0
for(i in 1:length(sepList)){
  tmpSep <- raster(paste0(sepList[i]))
  tmpOct <- raster(paste0(octList[i]))
  tmpNov <- raster(paste0(novList[i]))
  fallList[[i]] <-  (tmpSep + tmpOct + tmpNov)
}

for (i in 1:length(fallList)){
  assign(paste(fallNames[i]),fallList[[i]])
}
###
###spring list
marList <- dir(path = ".", pattern = "03_bil.bil$")
aprList <- dir(path = ".", pattern = "04_bil.bil$")
mayList <- dir(path = ".", pattern = "05_bil.bil$")

springNames <- as.character(seq(from = 1991, to = 2021))
str_sub(springNames, start = 0, end = 0) <- "spring"
springList <- list()

i = 0
for(i in 1:length(marList)){
  tmpMar <- raster(paste0(marList[i]))
  tmpApr <- raster(paste0(aprList[i]))
  tmpMay <- raster(paste0(mayList[i]))
  springList[[i]] <-  (tmpMar + tmpApr + tmpMay)
}

for (i in 1:length(springList)){
  assign(paste(springNames[i]),springList[[i]])
}
###combination of years
yearList <- list()
westernUS <- readOGR("Shapefiles/WesternUS.gpkg")
westUSNAD <- spTransform(westernUS, CRS("+proj=longlat +datum=NAD83 +no_defs"))
for(i in 1:length(springList)){
  yearList[[i]] <- (winterList[[i]] + springList[[i]] + fallList[[i]] + summerList[[i]])
  yearList[[i]] <- crop(yearList[[i]], extent(westUSNAD))
  yearList[[i]] <- mask(yearList[[i]],westUSNAD)
}

yearNames <- as.character(seq(from = 1991, to = 2021))
for (i in 1:length(yearList)){
  assign(paste(yearNames[i]),yearList[[i]])
}
###summer proportion
summerPercents <- list()
for(i in 1:length(yearList)){
  summerPercents[[i]] <- (summerList[[i]]/yearList[[i]])
  summerPercents[[i]] <- crop(summerPercents[[i]], extent(westUSNAD))
  summerPercents[[i]] <- mask(summerPercents[[i]], westUSNAD)
}

summerStack <- stack(summerPercents)
plot(summerStack)

summerMean <- calc(summerStack, mean)
###winter proportion
winterPercents <- list()
for(i in 1:length(yearList)){
  winterPercents[[i]] <- (winterList[[i]]/yearList[[i]])
  winterPercents[[i]] <- crop(winterPercents[[i]], extent(westUSNAD))
  winterPercents[[i]] <- mask(winterPercents[[i]], westUSNAD)
}

for (i in 1:length(winterPercents)){
  assign(paste(winterNames[i]),winterPercents[[i]])
}

winterStack <- stack(winterPercents)
winterMean <- calc(winterStack, mean)

plot(winterMean)
###interannual differences
summerYtY <- list()
i = 0
for(i in 1:30) {
  summerYtY[[i]] <- (summerPercents[[i+1]]-summerPercents[[i]])
}
sumYtYStack <- stack(summerYtY)
summerBeginToEnd <- summerPercents[[31]] - summerPercents[[1]]

winterYtY <- list()
i = 0
for(i in 1:30) {
  winterYtY[[i]] <- (winterPercents[[i+1]]-winterPercents[[i]])
}
winYtYStack <- stack(winterYtY)
winterBeginToEnd <- winterPercents[[31]] - winterPercents[[1]]
###proportion-slope calculations
winterDFList <- list()
for (i in 1:length(winterPercents)){
  winterDFList[[i]] <- data.frame(rasterToPoints(winterPercents[[i]]))
  winterDFList[[i]]$Year <- yearNames[i]
}

winterBrick <- brick(winterStack)
yearNames <- as.numeric(yearNames)

winTrend.rast <- calc(winterBrick, trend.func)
winTrend.rast2 <- raster.kendall(winterBrick)
summerDFList <- list()
for (i in 1:length(summerPercents)){
  summerDFList[[i]] <- data.frame(rasterToPoints(summerPercents[[i]]))
  summerDFList[[i]]$Year <- yearNames[i]
}
summerBrick <- brick(summerStack)
sumTrend.rast <- calc(summerBrick, trend.func)
sumTrend.rast2 <- raster.kendall(summerBrick)

par(mfrow=c(2,2))
plot(sumTrend.rast, main = "Summer Trend, trend.func()")
plot(sumTrend.rast2, main = "Summer Trend, raster.kendall()")

plot(winTrend.rast, main = "Winter Trend, trend.func()")
plot(winTrend.rast2, main = "Winter Trend, raster.kendall()")

image(summerMean)
image(winterMean)
image(sumTrend.rast)
image(winTrend.rast)
image(winTrend.Kendall)