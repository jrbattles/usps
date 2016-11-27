library(downloader)
library(ggplot2)
library(grid)
library(dplyr)  

## Exploratory Data Analysis
download(url="https://about.usps.com/who-we-are/foia/leased-facilities/ok.csv", destfile="ok-usps.csv")

## https://about.usps.com/who-we-are/foia/readroom/ownedfacilitiesreport.htm
download(url="https://about.usps.com/who-we-are/foia/owned-facilities/ok.csv", destfile="ok-usps-owned.csv")

## collect raw data
dataRaw <- read.csv("ok-usps.csv", header = TRUE, skip = 3, as.is = TRUE)
dataRawOwn <- read.csv("ok-usps-owned.csv", header = TRUE, skip = 3, as.is = TRUE)
dataSeats <- read.csv("ok-countyseats.csv", header = TRUE, as.is = TRUE)
dataSeats$City <- toupper(dataSeats$City)
# http://okcommerce.gov/wp-content/uploads/2015/06/Population_Projections_Report-2012.pdf
dataDecrease <- read.csv("ok-countydecline.csv", header = TRUE, as.is = TRUE)
dataIncrease <- read.csv("ok-countyincrease.csv", header = TRUE, as.is = TRUE)

## modify RawData to convert to numeric and factors where needed.
dataRaw$Annual.Rent = as.numeric(gsub("[\\$,]", "", dataRaw$Annual.Rent))
dataRaw$Annual.Rent...Sq.Ft = as.numeric(gsub("[\\$,]", "", dataRaw$Annual.Rent...Sq.Ft))
dataRaw$Next.Rent = as.numeric(gsub("[\\$,]", "", dataRaw$Next.Rent))
dataRaw$Next.Rent...Sq.Ft = as.numeric(gsub("[\\$,]", "", dataRaw$Next.Rent...Sq.Ft))
dataRaw$Eff.Date <- as.Date(dataRaw$Eff.Date, "%m/%d/%Y") 
dataRaw$Exp.Date <- as.Date(dataRaw$Exp.Date, "%m/%d/%Y") 
dataRaw$Maint <- as.factor(dataRaw$Maint)
dataRaw$Unit.Name <- as.factor(dataRaw$Unit.Name)
dataRaw$Int.Sq.Ft = as.numeric(gsub("[\\,]", "", dataRaw$Int.Sq.Ft))
dataRaw$Site.Sq.Ft = as.numeric(gsub("[\\,]", "", dataRaw$Site.Sq.Ft))

#dataRaw$City %in% dataSeats$City

#targetsCountySeats <- collect(inner_join(dataRaw, dataSeats))

#targetsCountySeatsGrow <- collect(inner_join(targetsCountySeats, dataIncrease))

#indx <- targets$County %in% dataDecline$County
rentCheap <- subset(dataRaw, subset=(Annual.Rent <= 15000))
#rentCheap <- subset(rentCheap, subset=(Annual.Rent >= 15000))
rentCheapMains <- subset(rentCheap, subset=(Unit.Name == "MAIN OFFICE"))
avgRentSqFt <- mean(rentCheap$Annual.Rent...Sq.Ft)
rentCheap2 <- rentCheap %>% mutate(ChangeSqFtRate = Annual.Rent...Sq.Ft - avgRentSqFt)
rentCheaper <- subset(rentCheap2, subset=(ChangeSqFtRate < 0))

qplot(Annual.Rent, Site.Sq.Ft, data = rentCheap, colour = Maint)
qplot(Next.Rent...Sq.Ft, Next.Rent, data = dataRaw, colour = Exp.Date)

p <- ggplot(rentCheap, aes(Annual.Rent, Site.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = 0, nudge_y = 0.5)


p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(size = 2, vjust = 0, nudge_y = 0.5)

p <- ggplot(rentCheap, aes(Annual.Rent, Next.Rent, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = factor(Maint)))

p <- ggplot(rentCheap, aes(Annual.Rent, Annual.Rent...Sq.Ft - avgRentSqFt, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = Exp.Date))

p <- ggplot(rentCheaper, aes(Annual.Rent, ChangeSqFtRate, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = Exp.Date))

p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(angle = 45, aes(colour = factor(Maint)))

hist(rentCheap$Annual.Rent, breaks = 50)

## Adding Latitude & Longitude to the locations
install.packages("RDSTK")
library("RDSTK", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
fullAddr = paste(dataRaw[326,5], dataRaw[326,6], dataRaw[326,7], dataRaw[326,8], sep = ",")
coordsPO <- street2coordinates(fullAddr)

## playing with maps
library(ggmap)
qmap('Tulsa')
qmap('north miami, ok')
qmap('north miami, ok', zoom = 13)
map <- qmap('north miami, ok', zoom = 13)
Longitude <- coordsPO[5]
Latitude <- coordsPO[3]
map + geom_point(data = coordsPO, aes(x = Longitude, y = Latitude), color="red", size=3, alpha=0.5)


## target 250 main offices locations with annual rent < $15K
write.table(rentCheapMains, file = "CheapMainOffices2.csv", sep = ",")
