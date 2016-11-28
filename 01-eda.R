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

# subset for rents < $15K and only main post offices.
rentCheap <- subset(dataRaw, subset=(Annual.Rent <= 15000))
rentCheapMains <- subset(rentCheap, subset=(Unit.Name == "MAIN OFFICE" | Unit.Name == "MAIN POST OFFICE" | Unit.Name == "MPO"))

# Calculate average AnnRate per Sq. Ft and add column with info
avgRentSqFt <- mean(rentCheapMains$Annual.Rent...Sq.Ft)
rentCheapMains2 <- rentCheapMains %>% mutate(ChangeSqFtRate = Annual.Rent...Sq.Ft - avgRentSqFt)

# subset for only those with rates below average market value... Growth!!!!
rentCheapMainsWillGrow <- subset(rentCheapMains2, subset=(ChangeSqFtRate < 0))

## clean up zip codes and mesh with population data 
rentCheapRegions <- rentCheapMainsWillGrow %>% mutate(region = clean.zipcodes(ZIP.Code))
rentCheapRegsPops <- merge(rentCheapRegions, df_pop_zip, by.x = "region", by.y = "region", all.x = TRUE)
names(rentCheapRegsPops)[34]<-paste("Population")
 
# Plot Annual Rent per Population by Unit Name type
p <- ggplot(rentCheapRegsPops, aes(Annual.Rent, Population, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = factor(Unit.Name)))


######  WIP below here

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
coordsPO[3]

rentCheapRegsPopsLocs <- rentCheapRegsPops %>% mutate(Full.Addr = paste(Property.Address, City, ST, ZIP.Code, sep = ","))
rentCheapRegsPopsLatLong <- merge(rentCheapRegsPopsLocs, zipcode, by.x = "region", by.y = "zip")
#rentCheapRegsPopsLatLong <- rentCheapRegsPopsLocs %>% mutate(Lat = street2coordinates(rentCheapRegsPopsLocs$Full.Addr))

#names(rentCheapRegsPopsLocs)
#street2coordinates(rentCheapRegsPopsLocs$Full.Addr)

#mapping
names(rentCheapRegsPopsLatLong)
rentCheapRegsPopsLatLongNoZ <- filter(rentCheapRegsPopsLatLong, Annual.Rent > 0)
#rentCheapRegsPopsLatLong <- filter(rentCheapRegsPopsLatLong, Next.Rent...Sq.Ft > 0)
names(rentCheapRegsPopsLatLongNoZ)[29]<-paste("Rent.Per.Sq.Ft")
names(rentCheapRegsPopsLatLongNoZ)

#theme_set(theme_bw(16))
#OklahomaMap <- qmap("el reno, ok", zoom = 6, color = "bw", extent = "normal", scale = 2)
#OklahomaMap +
#    geom_point(aes(x = longitude, y = latitude, colour = Rent.Per.Sq.Ft, size = Population, labels = PO.Name),
#               data = rentCheapRegsPopsLatLong)

##  mapping with PO Names now
#theme_set(theme_bw(16))
#OklahomaMap <- qmap("el reno, ok", zoom = 7, color = "bw", extent = "normal", scale = 2, maprange = TRUE)
#OklahomaMap +
#    geom_point(aes(x = longitude, y = latitude, colour = Rent.Per.Sq.Ft, size = Population),
#               data = rentCheapRegsPopsLatLongNoZ) + geom_text(data = rentCheapRegsPopsLatLongNoZ, aes(x = longitude, y = latitude, label = PO.Name), size = 2, vjust = 0, hjust = 0)

## mapping with PO Names and red / gren colour gradiant
theme_set(theme_bw(16))
OklahomaMap <- qmap("el reno, ok", zoom = 7, color = "bw", extent = "normal", scale = 2, maprange = TRUE)
OklahomaMap +
    geom_point(aes(x = longitude, y = latitude, colour = Rent.Per.Sq.Ft, size = Population), 
               data = rentCheapRegsPopsLatLongNoZ) + scale_color_gradient(low = "green", high = "red") +
                geom_text(data = rentCheapRegsPopsLatLongNoZ, aes(x = longitude, y = latitude, label = PO.Name), size = 2, vjust = 0, hjust = 0)


# alternate
myLocation <- c(-105, 34.5, -93, 37.5)
myMap <- get_map(location = myLocation, source = "google", color = "bw", scale = 2)
ggmap(myMap)


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


# download POSTPLAN info.
install.packages("pdftools")
library(pdftools)
download(url="http://auspl.com/wp-content/uploads/2013/08/POSTPlan-Statistics.pdf", destfile="POSTPlan-Statistics.pdf")
dfPostPlan <- pdf_info("POSTPlan-Statistics.pdf")

