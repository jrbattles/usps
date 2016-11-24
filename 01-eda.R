library(downloader)
library(ggplot2)
library(grid)
library(dplyr)  

## Exploratory Data Analysis
download(url="https://about.usps.com/who-we-are/foia/leased-facilities/ok.csv", destfile="ok-usps.csv")

## collect raw data
dataRaw <- read.csv("ok-usps.csv", header = TRUE, skip = 3, as.is = TRUE)
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
dataRaw$Int.Sq.Ft = as.numeric(gsub("[\\,]", "", dataRaw$Int.Sq.Ft))
dataRaw$Site.Sq.Ft = as.numeric(gsub("[\\,]", "", dataRaw$Site.Sq.Ft))

#dataRaw$City %in% dataSeats$City

# 
targetsCountySeats <- collect(inner_join(dataRaw, dataSeats))
targetsCountySeatsGrow <- collect(inner_join(targetsCountySeats, dataIncrease))

#indx <- targets$County %in% dataDecline$County

rentCheap <- subset(targetsCountySeatsGrow, subset=(Annual.Rent <= 25000))


qplot(Annual.Rent, Site.Sq.Ft, data = rentCheap, colour = Maint)
qplot(Next.Rent...Sq.Ft, Next.Rent, data = dataRaw, colour = Exp.Date)

p <- ggplot(rentCheap, aes(Annual.Rent, Site.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = 0, nudge_y = 0.5)


p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(size = 2, vjust = 0, nudge_y = 0.5)

p <- ggplot(rentCheap, aes(Annual.Rent, Site.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(vjust = -.5, aes(colour = factor(Maint)))

p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
p + geom_point() + geom_text(angle = 45, aes(colour = factor(Maint)))
