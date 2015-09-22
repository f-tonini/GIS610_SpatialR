# GIS610: Spatial Data Visualization R
Francesco Tonini  
October 15, 2015  



This lecture will walk you through some more advanced spatial data manipulation and visualization in R. __*We acknowledge that the materials provided here do not by any means intend to be comprehensive nor attempt to cover all concepts that would otherwise require a semester-long course*__. The sections and code chunks found here are derived from a variety of internet resources as well as past `R` workshops. Because this repository is open-source, please feel free to use it as you deem appropriate and I would greatly appreciate any feedback to improve and build upon it. If you are familiar with version control concepts, such as Git and Github, you are more than welcome to 'fork' this repository on your own account and send a 'pull request' to me if you wish to add/edit any of the scripts.

###Set Your Working Directory
Here, we'll use the `setwd` function to tell R where the module 1 folder is located on our machine. This will depend on the location of your downloaded course folder, Intro-Spatial-R.


```r
# modify the filepath based on your user specific folder location
# Single or double quotes work for text strings. Must use forward slashes.

#Some common examples:
#setwd('C:/Users/username/Documents/GIS610_SpatialR/Spatial Data Visualization in R')   # for Windows users
#setwd("~/Documents/GIS610_SpatialR/Spatial Data Visualization in R")                   # ~ for Mac users
```





#Spatial Data in R: Using R as a GIS

In the previous lecture, we saw examples on how to produce basic maps within `R` as well as adding some more advanced descriptive information to them. In this section, we keep showing alternative ways in which `R` can be used as a GIS, i.e. for basic-to-intermediate mapping tasks. Let us start with some examples on how Google Maps basemaps can be pulled into `R` and basic maps be overlaid onto these "canvases". Two very useful `R` packages are `RgoogleMaps` and `plotGoogleMaps`. 

###The `RgoogleMaps` package

The three core functions within `RgoogleMap` are:  

* __GetMap()__ fetches a map tile from the Google API. Returns a list containing the image and all parameters needed to properly scale coordinates  
* __PlotOnStaticMap()__ adds point type data to an existing map  
* __PlotPolysOnStaticMap()__ adds polygon type data stored in a data structure defined by the package `PBSmapping` to an existing map in form.


```r
library(RgoogleMaps)

#load dataset of interest with crime data in the SF area for 2013
load("./data/incidents2013.rda")
#list what dataset(s) has been loaded
ls()
```

```
## [1] "incidents"
```

```r
#let's have a quick exploration of the crime dataset
dim(incidents)
```

```
## [1] 126991     17
```

```r
head(incidents)
```

```
##   IncidntNum          Category                        Descript DayOfWeek
## 1  121047551 RECOVERED VEHICLE        VEHICLE, RECOVERED, AUTO  Saturday
## 2   13670901     LARCENY/THEFT         PETTY THEFT OF PROPERTY    Monday
## 3   60847080          WARRANTS                  WARRANT ARREST    Sunday
## 4   71276391          WARRANTS ENROUTE TO OUTSIDE JURISDICTION Wednesday
## 5   71276391          WARRANTS                  WARRANT ARREST Wednesday
## 6   90818299       WEAPON LAWS          POSS OF LOADED FIREARM Wednesday
##         Date  Time PdDistrict     Resolution                    Location
## 1 2013-01-05 12:18    BAYVIEW ARREST, BOOKED 900 Block of CONNECTICUT ST
## 2 2013-04-15 16:15  INGLESIDE           NONE      SILVER AV / MISSION ST
## 3 2013-03-24 17:33   SOUTHERN ARREST, BOOKED      800 Block of BRYANT ST
## 4 2013-02-13 22:41   NORTHERN ARREST, BOOKED   CHESTNUT ST / FILLMORE ST
## 5 2013-02-13 22:41   NORTHERN ARREST, BOOKED   CHESTNUT ST / FILLMORE ST
## 6 2013-02-20 16:00   SOUTHERN ARREST, BOOKED      800 Block of BRYANT ST
##           X        Y violent HrOfDay TimeOfDay HourOfWeek PhaseOfWeek
## 1 -122.3978 37.75397   FALSE      12  12.30000    36.3000    Saturday
## 2 -122.4313 37.72873   FALSE      16  16.25000    88.2500     Mon-Thu
## 3 -122.4037 37.77518   FALSE      17  17.55000    65.5500      Sunday
## 4 -122.4363 37.80081   FALSE      22  22.68333   142.6833     Mon-Thu
## 5 -122.4363 37.80081   FALSE      22  22.68333   142.6833     Mon-Thu
## 6 -122.4037 37.77518   FALSE      16  16.00000   136.0000     Mon-Thu
##   CensusBlock
## 1 06075061400
## 2 06075025500
## 3 06075018000
## 4 06075012800
## 5 06075012800
## 6 06075018000
```

```r
#let's fetch the map tile at the desired resolution and centered on a lat/lon point
SFzoom13 <- GetMap(center = c(lat = 37.77173, lon = -122.4306), 
                   destfile = "./data/figures/SanFrancisco.z13.png", 
                   GRAYSCALE = FALSE, zoom = 13, SCALE = 2, maptype = "terrain")
#let's randomly sample a number of crimes for visualization purposes
Ncrimes <- 10000
ranPts <- sample(nrow(incidents), Ncrimes)
crimes <- incidents[ranPts, ]
#We are mostly interested in looking at "violent" crimes.
#let's start by converting the variable from factor to logical
crimes$violent <- as.logical(crimes$violent)
#We want to pick two different colors for violent and non violent crimes.
#in order to overlay these colors with some transparency, we need to define the transparency "alpha" level
#inside the rgb() function. In this case we pick the colors 'red' and 'cyan' defined by
#their corresponding R-G-B values
colViolent <- rgb(1,0,0,0.3)
colNonViolent <- rgb(0,0,1,0.3)
cols <- c(colViolent, colNonViolent)
#now, we are ready to plot on our static Google Maps tile
#png("./data/figures/SanFrancisco.z13.png", width=1280, height=1280) #use if you want to save higher quality image
tmp <- PlotOnStaticMap(SFzoom13, lat = crimes$Y, lon = crimes$X, cex=0.9, 
                       pch=20, col=cols[as.numeric(crimes$violent)+1])
legend("topright", col = c(colNonViolent,colViolent), pch=20, 
       legend = c("N","Y"), title="violent", cex = 1)
```

![](./data/figures/googlemap-1.png) 

```r
#dev.off() #if you use png() remember to close the graphical device
```
