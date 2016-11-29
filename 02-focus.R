## United States Post Office Rental Properties
## Exploratory Data Analysis
## Jason R. Battles, jason.r.battles@gmail.com

library(downloader)
library(ggplot2)
library(grid)
library(dplyr)  
library("zipcode", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")


## Exploratory Data Analysis
download(url="https://about.usps.com/who-we-are/foia/leased-facilities/ok.csv", destfile="ok-usps.csv")

## https://about.usps.com/who-we-are/foia/readroom/ownedfacilitiesreport.htm
download(url="https://about.usps.com/who-we-are/foia/owned-facilities/ok.csv", destfile="ok-usps-owned.csv")

## collect raw data.  #497 leased post offices. 189 USPS fully owned props.
poLease <- read.csv("ok-usps.csv", header = TRUE, skip = 3, as.is = TRUE)   
#dataRawOwn <- read.csv("ok-usps-owned.csv", header = TRUE, skip = 3, as.is = TRUE)
#dataSeats <- read.csv("ok-countyseats.csv", header = TRUE, as.is = TRUE)

# load data on Oklahoma county seats and whether county population is growing / shrinking
#dataSeats$City <- toupper(dataSeats$City)
# http://okcommerce.gov/wp-content/uploads/2015/06/Population_Projections_Report-2012.pdf
#dataDecrease <- read.csv("ok-countydecline.csv", header = TRUE, as.is = TRUE)
#dataIncrease <- read.csv("ok-countyincrease.csv", header = TRUE, as.is = TRUE)

## modify RawData to convert to numeric and factors where needed.
poLease$Annual.Rent = as.numeric(gsub("[\\$,]", "", poLease$Annual.Rent))
poLease$Annual.Rent...Sq.Ft = as.numeric(gsub("[\\$,]", "", poLease$Annual.Rent...Sq.Ft))
poLease$Next.Rent = as.numeric(gsub("[\\$,]", "", poLease$Next.Rent))
poLease$Next.Rent...Sq.Ft = as.numeric(gsub("[\\$,]", "", poLease$Next.Rent...Sq.Ft))
poLease$Eff.Date <- as.Date(poLease$Eff.Date, "%m/%d/%Y") 
poLease$Exp.Date <- as.Date(poLease$Exp.Date, "%m/%d/%Y") 
poLease$Maint <- as.factor(poLease$Maint)
poLease$Unit.Name <- as.factor(poLease$Unit.Name)
poLease$Int.Sq.Ft = as.numeric(gsub("[\\,]", "", poLease$Int.Sq.Ft))
poLease$Site.Sq.Ft = as.numeric(gsub("[\\,]", "", poLease$Site.Sq.Ft))

#dataRaw$City %in% dataSeats$City

#targetsCountySeats <- collect(inner_join(dataRaw, dataSeats))

#targetsCountySeatsGrow <- collect(inner_join(targetsCountySeats, dataIncrease))

#indx <- targets$County %in% dataDecline$County

# subset for rents < $15K and only main post offices.
poLeaseLow <- subset(poLease, subset=(Annual.Rent <= 20000))
poLeaseLowMain <- subset(poLeaseLow, subset=(Unit.Name == "MAIN OFFICE" | Unit.Name == "MAIN POST OFFICE" | Unit.Name == "MPO"))

# Calculate average AnnRate per Sq. Ft and add column with info
avgRentSqFt <- mean(poLeaseLowMain$Annual.Rent...Sq.Ft)
#poLeaseLowMain <- poLeaseLowMain %>% mutate(LeaseMktComparisonToAvg = Annual.Rent...Sq.Ft - avgRentSqFt)

# subset for only those with rates below average market value... Growth!!!!
#rentCheapMainsWillGrow <- subset(rentCheapMains2, subset=(ChangeSqFtRate < 0))

## clean up zip codes and mesh with population data 
library("zipcode", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
data(df_pop_zip)

poLeaseLowMainLocs <- poLeaseLowMain %>% mutate(region = clean.zipcodes(ZIP.Code))
poLeaseLowMainLocs <- merge(poLeaseLowMainLocs, df_pop_zip, by.x = "region", by.y = "region", all.x = TRUE)
names(poLeaseLowMainLocs)
names(poLeaseLowMainLocs)[33]<-paste("Population")
 
# Plot Annual Rent per Population by Unit Name type
p <- ggplot(rentCheapRegsPops, aes(Annual.Rent, Population, label = PO.Name))
p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = factor(Unit.Name)))


######  WIP below here

#qplot(Annual.Rent, Site.Sq.Ft, data = rentCheap, colour = Maint)
#qplot(Next.Rent...Sq.Ft, Next.Rent, data = dataRaw, colour = Exp.Date)

#p <- ggplot(rentCheap, aes(Annual.Rent, Site.Sq.Ft, label = PO.Name))
#p + geom_point() + geom_text(size = 3, vjust = 0, nudge_y = 0.5)

#p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
#p + geom_point() + geom_text(size = 2, vjust = 0, nudge_y = 0.5)

#p <- ggplot(rentCheap, aes(Annual.Rent, Next.Rent, label = PO.Name))
#p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = factor(Maint)))

#p <- ggplot(rentCheap, aes(Annual.Rent, Annual.Rent...Sq.Ft - avgRentSqFt, label = PO.Name))
#p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = Exp.Date))

#p <- ggplot(rentCheaper, aes(Annual.Rent, ChangeSqFtRate, label = PO.Name))
#p + geom_point() + geom_text(size = 3, vjust = -.5, aes(colour = Exp.Date))

#p <- ggplot(rentCheap, aes(Annual.Rent, Int.Sq.Ft, label = PO.Name))
#p + geom_point() + geom_text(angle = 45, aes(colour = factor(Maint)))

hist(poLeaseLowMainLocs$Annual.Rent, breaks = 50)

## Adding Latitude & Longitude to the locations
#install.packages("RDSTK")
#library("RDSTK", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
#fullAddr = paste(dataRaw[326,5], dataRaw[326,6], dataRaw[326,7], dataRaw[326,8], sep = ",")
#coordsPO <- street2coordinates(fullAddr)
#coordsPO[3]

data("zipcode")
#rentCheapRegsPopsLocs <- rentCheapRegsPops %>% mutate(Full.Addr = paste(Property.Address, City, ST, ZIP.Code, sep = ","))
poLeaseLowMainLocs <- merge(poLeaseLowMainLocs, zipcode, by.x = "region", by.y = "zip")
#rentCheapRegsPopsLatLong <- rentCheapRegsPopsLocs %>% mutate(Lat = street2coordinates(rentCheapRegsPopsLocs$Full.Addr))

#names(rentCheapRegsPopsLocs)
#street2coordinates(rentCheapRegsPopsLocs$Full.Addr)

#mapping
names(poLeaseLowMainLocs)
#rentCheapRegsPopsLatLongNoZ <- filter(rentCheapRegsPopsLatLong, Annual.Rent > 0)
#rentCheapRegsPopsLatLong <- filter(rentCheapRegsPopsLatLong, Next.Rent...Sq.Ft > 0)
names(poLeaseLowMainLocs)[30]<-paste("Rent.Sq.Ft")
names(poLeaseLowMainLocs)
poLeaseLowMainLocs <- poLeaseLowMainLocs %>% mutate(Full.Addr = paste(Property.Address, City, ST, ZIP.Code, sep = ","))
poLeaseLowMainLocs <- poLeaseLowMainLocs %>% mutate(LeaseMktComparisonToAvg = Rent.Sq.Ft - avgRentSqFt)

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
library("ggmap", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
theme_set(theme_bw(16))
OklahomaMap <- qmap("el reno, ok", zoom = 7, color = "bw", extent = "normal", scale = 2, maprange = TRUE)
OklahomaMap +
    geom_point(aes(x = longitude, y = latitude, colour = Rent.Sq.Ft, size = Population), 
               data = poLeaseLowMainLocs) + scale_color_gradient(low = "green", high = "red") +
                geom_text(data = poLeaseLowMainLocs, aes(x = longitude, y = latitude, label = PO.Name), size = 2, vjust = 0, hjust = 0)


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

poLeaseLowMainLocs <- filter(poLeaseLowMainLocs, Annual.Rent > 0)
qplot(poLeaseLowMainLocs$Annual.Rent, 
      geom="histogram",
      binwidth = 1000,
      xlab = "Annual Rent",
      fill = I("blue"),
      col=I("yellow")) 

qplot(poLeaseLowMainLocs$Population, 
      geom="histogram",
      binwidth = 250,
      xlab = "Population Served",
      fill = I("brown"),
      col=I("yellow")) 


poLeaseLowMainLocs250up <- filter(poLeaseLowMainLocs, Population > 250)

qplot(poLeaseLowMainLocs250up$Annual.Rent, 
      geom="histogram",
      binwidth = 1000,
      xlab = "Annual Rent",
      fill = I("blue"),
      col=I("yellow")) 

qplot(poLeaseLowMainLocs250up$Population, 
      geom="histogram",
      binwidth = 250,
      xlab = "Population Served",
      fill = I("brown"),
      col=I("yellow")) 
                   
## target 311 main offices locations with annual rent < $20K
write.table(poLeaseLowMainLocs250up, file = "poLeaseLowMainLocs250up.csv", sep = ",")

poLeaseLowMainLocs20 <- filter(poLeaseLowMainLocs, Annual.Rent > 0)

theme_set(theme_bw(16))
OklahomaMap <- qmap("el reno, ok", zoom = 7, color = "bw", extent = "normal", scale = 2, maprange = TRUE)
OklahomaMap +
    geom_point(aes(x = longitude, y = latitude, colour = Rent.Sq.Ft, size = Population), 
               data = poLeaseLowMainLocs250up) + scale_color_gradient(low = "green", high = "red") +
    geom_text(data = poLeaseLowMainLocs250up, aes(x = longitude, y = latitude, label = PO.Name), size = 2, vjust = 0, hjust = 0)


# download POSTPLAN info.
#install.packages("pdftools")
#library(pdftools)
#download(url="http://auspl.com/wp-content/uploads/2013/08/POSTPlan-Statistics.pdf", destfile="POSTPlan-Statistics.pdf")
#dfPostPlan <- pdf_info("POSTPlan-Statistics.pdf")

