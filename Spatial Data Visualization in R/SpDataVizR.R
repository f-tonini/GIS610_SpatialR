# ---	
# title: "GIS610: Spatial Data Visualization in R"	
# author: "Francesco Tonini"	
# date: "October 15, 2015"	
# output:	
#   html_document:	
#     keep_md: yes	
#     toc: yes	
#   pdf_document:	
#     toc: yes	
# ---	
# 	
# 	
library(knitr)	
opts_chunk$set(cache=TRUE, fig.path='./data/figures/')	
# 	
# 	
# This lecture will walk you through some more advanced spatial data manipulation and visualization in R. __*We acknowledge that the materials provided here do not by any means intend to be comprehensive nor attempt to cover all concepts that would otherwise require a semester-long course*__. The sections and code chunks found here are derived from a variety of internet resources as well as past `R` workshops. Because this repository is open-source, please feel free to use it as you deem appropriate and I would greatly appreciate any feedback to improve and build upon it. If you are familiar with version control concepts, such as Git and Github, you are more than welcome to 'fork' this repository on your own account and send a 'pull request' to me if you wish to add/edit any of the scripts.	
# 	
# ###Set Your Working Directory	
# Here, we'll use the `setwd` function to tell R where the module 1 folder is located on our machine. This will depend on the location of your downloaded course folder, Intro-Spatial-R.	
# 	
# 	
# modify the filepath based on your user specific folder location	
# Single or double quotes work for text strings. Must use forward slashes.	
	
#Some common examples:	
#setwd('C:/Users/username/Documents/GIS610_SpatialR/Spatial Data Visualization in R')   # for Windows users	
#setwd("~/Documents/GIS610_SpatialR/Spatial Data Visualization in R")                   # ~ for Mac users	
# 	
# 	
# 	
# 	
if (!file.exists("./data/figures")) dir.create(file.path(getwd(),"./data/figures"))	
# 	
# 	
# ## R Packages Needed	
# 	
# Make sure you have these packages installed (use install.packages() function in R) or some scripts will not run. In order to use some of the useful functionalities that come with each package, we need to call them into our current R session using the library() or require() command.	
# 	
# 	
library(rgdal)   #for reading/writing geo files	
library(rgeos)   #for simplification	
library(plyr)	
library(leafletR)	
library(RColorBrewer)	
library(Quandl)	
library(reshape2)	
library(RgoogleMaps)	
library(plotGoogleMaps)	
library(loa)	
library(latticeExtra)	
library(ggmap)	
library(maptools)	
library(rjson)	
# 	
# 	
# This lecture will also touch on topics such as interactive web maps from R. This topic is fairly recent in the R community and package development is ongoing. In fact, you will not be able to find and install the following packages from the common CRAN repository. In order to download and install R packages that are under development on the author's Github repository, we need to install an R library called `devtools`, which allows to use the command install_github() for the desired packages. 	
# 	
# 	
library(devtools)	
##The R packages rCharts and rMaps are not available on CRAN yet.	
##Run the following code to install from Github repo prior to loading the libraries:	
##install_github('ramnathv/rCharts')	
##install_github('ramnathv/rMaps')	
library(rCharts)	
library(rMaps)	
# 	
# 	
# #Spatial Data in R: Using R as a GIS	
# 	
# In the previous lecture, we saw examples on how to produce basic maps within `R` as well as adding some more advanced descriptive information to them. In this section, we keep showing alternative ways in which `R` can be used as a GIS, i.e. for basic-to-intermediate mapping tasks. Let us start with some examples on how Google Maps basemaps can be pulled into `R` and basic maps be overlaid onto these "canvases". Two very useful `R` packages are `RgoogleMaps` and `loa`.  	
# 	
# ###The `RgoogleMaps` and `loa` packages	
# 	
# The three core functions within `RgoogleMap` are:  	
# 	
# * __GetMap()__ fetches a map tile from the Google API. Returns a list containing the image and all parameters needed to properly scale coordinates  	
# * __PlotOnStaticMap()__ adds point type data to an existing map  	
# * __PlotPolysOnStaticMap()__ adds polygon type data stored in a data structure defined by the package `PBSmapping` to an existing map in form.	
# 	
# 	
#load dataset of interest with crime data in the SF area for 2013	
load("./data/incidents2013.rda")	
#list what dataset(s) has been loaded	
ls()	
#let's have a quick exploration of the crime dataset	
dim(incidents)	
head(incidents)	
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
#dev.off() #if you use png() remember to close the graphical device	
# 	
# 	
# The `loa` package contains various plots and functions that make use of the lattice/trellis plotting framework. The plots (which include loaPlot, GoogleMap and trianglePlot) use panelPal, a function that extends `lattice` and `hexbin` package methods to automate plot subscripting and panel-to-panel and panel-to key synchronization/management. See __?loa__ for further details. Here, we will use GoogleMap(), which is basically a wrapper for `RgoogleMaps` function GetMap() and loa function loaPlot()	
# 	
# 	
#let's save the time of the day in a separate variable and look at it	
temp <- incidents$TimeOfDay	
head(temp)	
#we not want to generate a character vector with labels indicating the approx. time of the day	
#We can start by replicating the label "About Midnight" for a number of times equal to the length	
#of the temp vector	
tod <- rep("About Midnight", length(temp))	
	
#now that we have prepared this new character vector variable, we need to substitute	
#the correct labels to the appropriate time windows in a day. We can do it in the following way	
tod <- ifelse(as.numeric(temp) > 2 & as.numeric(temp) < 9, "About Dawn", tod)	
tod <- ifelse(as.numeric(temp) > 8 & as.numeric(temp) < 14, "About Midday", tod)	
tod <- ifelse(as.numeric(temp) > 13 & as.numeric(temp) < 20, "About Dusk", tod)	
tod <- factor(tod, levels=c("About Dawn", "About Midday", "About Dusk", "About Midnight"))	
	
#we want to also have a character vector with a label indicating whether a crime is violent or not	
v <- ifelse(incidents$violent==TRUE, "Violent", "Non-violent")	
	
#now we are redy to use the GoogleMap() function to plot a series of temporal plots	
#of crimes by time of the day and type of crime (violent/non-violent)	
GoogleMap(~Y*X|tod+v, #conditioning	
          data=incidents, col="darkred", 	
          cex=0.1, alpha=0.1, pch = '.',	
          scales=list(draw=FALSE), xlab="", ylab="")            #to hide axes	
	
#png("SFtest.png", 1690, 717, bg="transparent", res=118, type="cairo-png")	
useOuterStrips(trellis.last.object())	
#dev.off()	
# 	
# 	
# ###The `ggmap` package	
# 	
# The `ggmap` package enables visualization by combining the spatial information of static maps from Google Maps, OpenStreetMap, Stamen Maps or CloudMade Maps with the layered grammar of graphics implementation of `ggplot2`. More information on this R package and tutorials from which the following section is extracted can be found [__HERE__](http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf).	
# 	
# 	
#let's start simple by using the ggmap() function to simply pull down a map from the web	
#centered on a city of interest	
raleigh <- get_map(location = "raleigh")	
ggmap(raleigh, extent = "normal")	
	
#The `ggmap` package comes with a "crime" dataset. We will use it here for some plotting	
#exercise. Let's only subset violent crimes	
violent_crimes <- crime[crime$offense != "auto theft" & crime$offense != "theft" & crime$offense != "burglary", ]	
	
#let's order the violent crimes	
violent_crimes$offense <- factor(violent_crimes$offense,	
                              levels = c("robbery", "aggravated assault", "rape", "murder"))	
	
#we can now subset the violent crimes happening within the bounding box seen above	
violent_crimes <- violent_crimes[-95.39681 <= violent_crimes$lon  & violent_crimes$lon <= -95.34188 & 	
                                  29.73631 <= violent_crimes$lat & violent_crimes$lat <= 29.78400, ]	
	
#one last step before plotting. We may want to choose a background theme 	
#among the ones offered by the `ggplot2` package	
theme_set(theme_bw(16))	
	
#Finally, using a syntax equal to the `ggplot2` package, let's plot our crimes on top 	
#of our tile	
HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")	
HoustonMap +	
        geom_point(aes(x = lon, y = lat, colour = offense, size = offense),	
                   data = violent_crimes)	
# 	
# 	
# #Spatial Data in R: Using R for Interactive GIS Maps	
# 	
# The maps we have created so far are useful to visualize spatial information on top of static basemaps (similar to traditional old-school paper maps). However, because nowadays we are connected to the web all the times, we should consider the added power of designing interactive maps for the web. These typically access the most up-to-date information and can have specialized tools for retrieving information. Moreover, web maps are much more easy to read by the general public that is used to seeing them all the time. A good example of these sort of interactive maps is Google Maps. The main problem most R Users face is that they work on it all the time but do not nenecessarily know how to code in javascript (which would be needed for most web map applications). Luckily, R now offers the opportunity to create nice interactive maps for the web without any knowledge of javascript and HTML! Here we will exlore two main packages: `plotGoogleMaps`and `googleVis`.	
# 	
# ###The `plotGoogleMaps`and `googleVis` packages	
# 	
# The `plotGoogleMaps` package uses the power of Google's APIs to create intuitive and fully interactive web maps. We can use this package to create stunning HTML pages and later upload them to our websites or share with colleagues. More tutorials and examples similar to these ones can be found [__HERE__](http://r-video-tutorial.blogspot.com/2015/05/interactive-maps-for-web-in-r.html) and [__HERE__](http://www2.uaem.mx/r-mirror/web/packages/plotGoogleMaps/vignettes/plotGoogleMaps-intro.pdf).	
# 	
# We can start by looking at how to plot spatial point data. __NOTE__: the HTML files generated here, should open automatically in your default browser and saved to a local working directory folder.	
# 	
# 	
#Load point data from a dataset included in the package	
data(meuse)	
#convert the data frame to a spatial data frame	
coordinates(meuse)<-~x+y 	
#adding Coordinate Referent Sys	
proj4string(meuse) <- CRS('+init=epsg:28992')	
#Create web map of Point data	
m <- plotGoogleMaps(meuse, filename='myMap1.htm') 	
### THE HTML FILE WILL OPEN IN YOUR BROWSER AUTOMATICALLY ####	
	
#or you can plot a bubble graph with graduated symbols by zinc values	
m <- bubbleGoogleMaps(meuse, zcol='zinc', max.radius = 80, filename='myMap2.htm') 	
### THE HTML FILE WILL OPEN IN YOUR BROWSER AUTOMATICALLY ####	
# 	
# 	
# We may be interested in plotting spatial polygon data (choropleth maps). We need to use the R package `RColorBrewer` to deal with color coding.	
# 	
# 	
#inside the maptools package we can find a shapefile with North Carolina counties 	
nc <- readShapeSpatial( system.file("shapes/sids.shp",	
                        package="maptools")[1], proj4string=CRS("+proj=longlat	
                        +datum=NAD27"))	
	
#let's make a choropleth map 	
m <- plotGoogleMaps(nc, zcol="NWBIR74", filename='MyMap3.htm', mapTypeId='TERRAIN',	
                    colPalette= brewer.pal(7,"Reds"),	
                    strokeColor="white")	
### THE HTML FILE WILL OPEN IN YOUR BROWSER AUTOMATICALLY ####	
# 	
# 	
# Another very useful package to create interactive visualizations, in our case maps, is `googleVis`. This package lets you can create rich interactive graphics that you can play with locally in Rstudio or in your browser. For more information and examples, please check [__HERE__](http://github.com/mages/googleVis). The example shown here, is adjusted from the [R tutorial for spatial statistics](http://r-video-tutorial.blogspot.com/2015/05/interactive-maps-for-web-in-r.html) blog. In our example, we are going to import a shapefile with the borders of all the countries in the world found on [thematicmapping.org](thematicmapping.org).	
# 	
# 	
library(googleVis)	
library(rgdal)	
	
#let's start by reading in the shapefile	
polygons <- readOGR("data", layer="TM_WORLD_BORDERS_SIMPL-0.3")	
class(polygons)	
#assign this class back to a simple dataframe object	
data.poly <- as.data.frame(polygons)	
head(data.poly)	
#subset only country name and population fields	
data.poly <- data.poly[, c("NAME", "POP2005")]	
data.poly$POP2005 <- data.poly$POP2005 / 1000000 #divide population by 1 million 	
names(data.poly) <- c("Country Name","Population 2005 (millions)")	
#now we can use the gvisGeoMap() function to create the desired interactive map 	
map <- gvisGeoMap(data=data.poly, locationvar = "Country Name", 	
                  numvar='Population 2005 (millions)', 	
                  options=list(width='800px',heigth='500px',colors="['0x0000ff', '0xff0000']"))	
#save map to file using print()	
print(map, file="MyMap4.html")	
### THE HTML FILE IS SAVED ON YOUR LOCAL FOLDER ####	
# 	
# 	
# ###Create choropleth web maps using the `leafletR` package	
# 	
# In this section, we will show how to use power of the `leafletR` package to create very nice interactive online maps, without the need of writing a single line of Javascript or HTML5. This package uses the open-source JavaScript library called `leaflet`. In this sections, you will also start working with GeoJSON files. JSON (JavaScript Object Notation) files are lightweight data-interchange formats, easy for humans to read and write and easy for machines to parse and generateare. They are very commonly used to exchange and save data from web applications. GeoJSON adds encoding features and support for a variety of geographic data structures (i.e. geometries). You can read more about it [__HERE__](http://geojson.org/geojson-spec.html).	
# 	
# Here we import a polygon boundary of the state of Utah and a set of polygons representing game management units within the state.	
# 	
# 	
deer_unit <- readOGR(dsn = "data", layer = "deerrange_UT") # Game management units	
	
#The attribute data is stored in a slot called `@data` so if we just want to look at the data stored in the object:	
summary(deer_unit@data)	
names(deer_unit) # The attribute data names	
class(deer_unit)	
	
#Calculate a new field for vector data, such as the total number of adult deer and elk in each management unit:	
deer_unit$tot_deer_elk <- deer_unit$DEER + deer_unit$ELK	
summary(deer_unit@data)	
# 	
# 	
# ####Simplify your shapefile (if necessary)	
# 	
# Many shapefiles have significant detail that results in very large GeoJSON (JavaScript Object Notation) files. If you do not simplify the topology, a file can end up being several MB. Simplifying a topology represents an important GIS technique to reduce the vertex count in features that were captured in too much detail. More detail can be found [__HERE__](http://resources.arcgis.com/EN/HELP/MAIN/10.1/index.html#//01mm00000008000000). Here, we will use the `rgeos` package to simplify topology. __NOTE__: if preserving topology is crucial, consider using tools in QGIS or GRASS GIS. For this demonstration we will restrict ourselves to R	
# 	
# 	
library(rgeos)	
	
#save the data slot	
deer_unit_sub <- deer_unit@data[, c("UName", "UNum", "AREA_km2", "tot_deer_elk")]	
#simplification yields a SpatialPolygons class	
deer_unit <- gSimplify(deer_unit, tol=0.01, topologyPreserve=TRUE)	
class(deer_unit)	
#to write to geojson we need a SpatialPolygonsDataFrame	
deer_unit <- SpatialPolygonsDataFrame(deer_unit, data=deer_unit_sub)	
class(deer_unit)	
head(deer_unit@data)	
# 	
# 	
# We are now ready to create the GeoJSON file. We will also create the variable intervals (cuts) as we want in order to map it.	
# 	
# 	
#write data to GeoJSON	
writeOGR(deer_unit, dsn="./data/DeerElkGeoJson", layer="DeerElkGeoJson", driver="GeoJSON")	
#a GeoJSON datasource is translated to single OGRLayer object with pre-defined name OGRGeoJSON"	
ogrInfo("./data/DeerElkGeoJson", "OGRGeoJSON")	
#Divide the variable of interest into bins using custom cuts (in this case quintiles)	
cuts <- round(quantile(deer_unit$tot_deer_elk, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)	
# 	
# 	
# We are almost ready to create the final interactive web map. First, we want to choose a number of fields that we want displayed in a pop-up balloon inside our map. We may also want to define a graduated color style to plot our attribute of interest. For that, we will use the styleGrad() function of the `leafletR` package.	
# 	
# 	
#Choose fields to include in the popup	
popup <- c("UName", "UNum", "AREA_km2", "tot_deer_elk")	
#Graduated style symbology based on an attribute	
sty <- styleGrad(prop="tot_deer_elk", breaks=cuts, closure="left", style.par="col", 	
                 style.val=rev(heat.colors(5)), leg="Deer & Elk Population", lwd=1)	
#Create the map and load into browser	
map <- leaflet(data="./data/DeerElkGeoJson", style=sty, title="DeerElk", 	
               base.map="osm", incl.data=TRUE,  popup=popup)	
	
#To look at the map you can either find the .html file in your local folder, or call it using	
#browseURL(map)	
# 	
# 	
# ###Create Leaflet web heat maps	
# 	
# In this module, we will work on a dataset containing abundance records of California bay laurel (UMCA) trees in the Sonoma county, California. After reading in and cleaning the dataset, we will create a leaflet heat map using a JavaScript plugin. You can find plenty of more examples and documentation [__HERE__](https://rstudio.github.io/leaflet/).	
# 	
# 	
#read in data table 	
tree_dat <- read.csv("./data/plots_umca_infection.csv", header=T)	
#read plot locations shapefile	
plots <- readOGR(dsn='./data', layer='plot_202_latlon')	
#let's subset data for year 2012 only and 3 variables of interest.	
tree_dat_2012 <- tree_dat[tree_dat$year == 2012, c("plot", "year", "tot_bay")] 	
head(tree_dat_2012)	
# 	
# 	
# We now need to add the geographic coordinates of each plot. We have 202 plot locations in the shapefile but our dataset only has 179. We can use the R function merge() to keep only the matching ones.	
# 	
# 	
tree_dat_2012 <- merge(tree_dat_2012, plots@data, by.x="plot", by.y="PLOT_ID")	
head(tree_dat_2012)	
#remove unnecessary variables	
tree_dat_2012 <- tree_dat_2012[ , c("POINT_Y","POINT_X","tot_bay")]	
#rename the coordinates fields to lat and lon	
names(tree_dat_2012)[1:2] <- c("lat", "lon") 	
head(tree_dat_2012)	
# 	
# 	
# Now, we can create a new Leaflet map instance and set the plot extent based on our coordinates centroid.	
# 	
# 	
#create a new leaflet map instance	
Lmap <- Leaflet$new()	
#set the view and zoom to the desired study area. Let's center it on our mean lat-lon coordinates	
Lmap$setView(c(mean(tree_dat_2012$lat), mean(tree_dat_2012$lon)), zoom=10)	
#add a terrain basemap	
Lmap$tileLayer(provider = "MapQuestOpen.OSM")	
# 	
# 	
# Before generating the interactive web map, we need to convert our dataset to JSON format using the toJSONArray2() function.	
# 	
# 	
tree_dat <- toJSONArray2(na.omit(tree_dat_2012), json = F, names = F)	
#let's print out the first two elements of the JSON file	
cat(toJSON(tree_dat[1:2]), '\n')	
# 	
# 	
# The heat map is created thank to a leaflet-heat plugin, wrote by Vladimir Agafonkin. We only need to point to the plugin web address to use it with our map.	
# 	
# 	
#add leaflet-heat plugin. Thanks to Vladimir Agafonkin	
Lmap$addAssets(jshead = c(	
  "http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js"	
))	
	
#add javascript to modify underlying chart	
Lmap$setTemplate(afterScript = sprintf("	
<script>	
  var addressPoints = %s	
  var heat = L.heatLayer(addressPoints).addTo(map)           	
</script>	
", rjson::toJSON(tree_dat)	
))	
Lmap	
### CHECK YOUR BROWSER INSIDE THE figures FOLDER TO SEE THE MAP ####	
# 	
# 	
# ###Create animated choropleths	
# 	
# The following tutorial is based on the [__rMaps__](http://rmaps.github.io/blog/posts/animated-choropleths/index.html) online tutorial used to create animated choropleths. The data selected are different from the original. The first step to creating any visualization is getting the data. Let us fetch time-series data on burglaries in the US, from [Quandl] (https://www.quandl.com/). 	
# 	
# 	
#let's download the table referring to burglaries	
rbData = Quandl("FBI_UCR/USCRIME_TYPE_BURGLARIES")	
rbData[1:10, 1:5]	
# 	
# 	
# The dataset is in the wide-form. The first step is to convert it into the long-form (more convenient for visualization purposes). Moreover, we remove data for the US as a whole, as well as for DC, so that the crime rates across entities (states) are comparable	
# 	
# 	
#let's use the melt() function to restructure the data in wide form	
datm <- melt(rbData, 'Year', 	
  variable.name = 'State',	
  value.name = 'Crime'	
)	
datm <- subset(na.omit(datm), 	
  !(State %in% c("United States", "District of Columbia"))	
)	
head(datm)	
# 	
# 	
# Crime rates need to be categorized (discrete classes). One way to do this is to divide them into quartiles.	
# 	
# 	
#let's use the function transform() to change our data frame	
datm2 <- transform(datm,	
  State = state.abb[match(as.character(State), state.name)],	
  fillKey = cut(Crime, quantile(Crime, seq(0, 1, 1/4)), labels = LETTERS[1:4]),	
  Year = as.numeric(substr(Year, 1, 4))	
)	
# 	
# 	
# Each quartile needs to be associated with a fill color chosen from a palette. We can use the `RColorBrewer` package to deal with custom color fill.	
# 	
# 	
fills = setNames(	
  c(brewer.pal(4, 'OrRd'), 'white'),	
  c(LETTERS[1:4], 'defaultFill')	
)	
# 	
# 	
# We need to convert the data frame into a list of lists. We will use Hadley's `plyr` package to simplify the code.	
# 	
# 	
dat2 <- dlply(na.omit(datm2), "Year", function(x){	
  y = toJSONArray(x, json = F)	
  names(y) = lapply(y, '[[', 'State')	
  return(y)	
})	
names(dat2)	
#dat2[["1960"]] to inspect a list element	
# 	
# 	
# We can now create a simple choropleth map of crime rates for a given year. The Datamaps reference class gives us simple bindings to the DataMaps library	
# 	
# 	
options(rcharts.cdn = TRUE)	
map <- Datamaps$new()	
map$set(	
  dom = 'chart_1',	
  scope = 'usa',	
  fills = fills,	
  data = dat2[["1980"]],	
  legend = TRUE,	
  labels = TRUE	
)	
map	
# 	
# 	
# Use a customized wrapper function that absorbs js code to produce a dynamic choropeth map.	
# 	
# 	
source('ichoropleth.R')	
map2 <- ichoropleth(Crime ~ State,	
  data = datm2[,1:3],	
  pal = 'OrRd', #color ramp	
  ncuts = 4,  #quartiles	
  animate = 'Year'	
)	
map2	
# 	
# 	
# #### END OF SPATIAL DATA VISUALIZATION IN R####	
# 	
# 	
# 	
# convert .Rmd script to .R file for instruction	
# note .R file subsequently cleaned up for code margin of ~80 char	
	
rmd2rscript <- function(infile,outfile){	
  # read the file	
  flIn <- readLines(infile)	
  # identify the start of code blocks	
# 	
  # identify the end of code blocks	
  cdEnd <- sapply(cdStrt, function(x){	
# 	
#     return(preidx + x)	
#   })	
#   # define an expansion function	
#   # strip code block indacators	
#   flIn[c(cdStrt, cdEnd)] <- ""	
#   expFun <- function(strt, End){	
#     strt <- strt+1	
#     End <- End-1	
#     return(strt:End)	
#   }	
#   idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd, 	
#                 SIMPLIFY = FALSE))	
#   # add comments to all lines except code blocks	
#   comIdx <- 1:length(flIn)	
#   comIdx <- comIdx[-idx]	
#   for(i in comIdx){	
#     flIn[i] <- paste("# ", flIn[i], sep = "")	
#   }	
#   # create an output file	
#   #nm <- strsplit(infile, split = "\\.")[[1]][1]	
#   flOut <- file(outfile, "w")	
#   for(i in 1:length(flIn)){	
#     cat(flIn[i], "\n", file = flOut, sep = "\t")	
#   }	
#   close(flOut)	
# }	
# 	
# infile <- 'SpDataVizR.Rmd'	
# outfile <- 'SpDataVizR.R'	
# 	
# rmd2rscript(infile, outfile)	
# ```	
