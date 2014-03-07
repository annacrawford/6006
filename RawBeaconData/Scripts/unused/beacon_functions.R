### Functions used by beacon_processing.r

# Started by Anna Crawford - Feb 6/2013
# Last updated by Anna Crawford - Feb 7/2013

###############################################################################
# reset all vars
rm(list=ls(all=TRUE))

# Parameters for this script
progdir = 'C://Users/acrawford/Dropbox/RProj/beacons/PolluxWork/working_scripts'
wrkdir = "C://Users/acrawford/Dropbox/RProj/beacons/beacon_data/ProcessedData"

#progdir = 'c:/users/anna/Dropbox/beacons/script'  #-# Dir where r files are located
#wrkdir = 'c:/users/anna/Dropbox/beacon/Markham'   #-# Dir where data files are located
fname = "031950_2011.csv"  		#-# name file with ext

fname = choose.files(paste(wrkdir, fname, sep="/"), caption = "Select an beacon track file", multi = FALSE)
imgtype = "png"   	#-# use png for regular use, eps for publishing
##############################
# source new libraries # to install type: install.packages(pacakage, install.dependencies=TRUE)
library(sp)
library(rgdal)
library(GEOmap)
library(chron)
#library(robfilter)
setwd(progdir)
source("polarplot.r")
setwd(wrkdir)

######################################
# read in the data...
drift = read.table(fname, header=TRUE, sep=",", 
			dec=".", na.strings="NULL", strip.white=FALSE)

# the following will filter these with a 7 obs moving window more at help(robust.filter)
##drift$airTemp = robust.filter(drift$airTemp, width=7)$level                  
#drift$atmPres = robust.filter(drift$atmPres, width=7)$level
#drift$batt = robust.filter(drift$batt, width=7)$level         

# get a filename for shapefile here
fout = as.character(drift$beacon[1])
fout = substr(fout,nchar(fout)-5,nchar(fout)) #the name is last 6 digits of imei
fout = paste(fout, "_", as.character(drift$year[1]), sep="")

obs = seq(1,length(drift$year))
drift$beacon = as.character(drift$beacon)

# get the time difference between positions     #need to work on for sailwx (not on the hour...)
dt = as.numeric(difftime(drift$gps_time[2:length(drift$gps_time)],
        drift$gps_time[1:length(drift$gps_time)-1], units="hours"))

                                                          
# get the distance travelled between positions 
dist = vector(mode = 'numeric', length=0)
az = vector(mode = 'numeric', length=0)
err = vector(mode = 'numeric', length=0)

for (i in 1:(length(drift$lat)-1) ) {
	vect = distaz(drift$lat[i], drift$lon[i],drift$lat[i+1], drift$lon[i+1])      
	dist = append(dist, vect$dist)
	az = append(az, vect$az)
	err = append(err, vect$err)
	}
        # code likely based on spherical earth - some inaccuracy over large distance
        # suggest Kalman Filter before doing this?
dst = data.frame(dist, az, err)

cummuDst = rep(0,length(dst$dist))
for (i in 1:length(cummuDst)) {
  cummuDst[i] = sum(dst$dist[1:i])
  }

#calc the speed    #This is in beacon_converter script
speed = dst$dist/dt # in kph

# azimuth, dist, speed, etc have shorter vectors than other data
# first datapoints are technically NaN, but I will put 0 for compatability with ARC
tmp = 0
az = append(tmp, dst$az)
err = append(tmp, dst$err)
dist = append(tmp, dst$dist)
cummuDst = append(tmp, cummuDst)
speed = append(tmp, speed)
dst = data.frame(dist, az, err, cummuDst, speed)
#####################
# function to look for a file in the working directory and delete if if it exists
	# gets around gdal issue of not being able to overwrite. 
	# filename is a single string and ext can be a vector of strings
delfile = function(filename, ext) {
	for (i in 1:length(ext)) {
		if ( file.exists( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") ) ) {
			file.remove( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") )
			}
		if ( file.exists( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") ) ) {
			file.remove( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") )
			}
	} #end for
	} #end function

######################
# function to write dataset to a point and line shapefile (gpx and kml), given a dataframe
  # and output filename.  The dataframe must have columns lat + lon in wgs84
  # also only one beacon (line) per dataframe.  

shpout = function(dataset, filename) {         
 
  # generate a field that is a unique identifier for each point (name of point) 
  name = as.character(dataset$beacon)
	name = substr(name,nchar(name)-5,nchar(name)) #the name is last 6 digits of imei
	obs = seq(1,length(name))
	name = paste(name, formatC(obs, flag="0", width=5), sep="_")
	dataset = data.frame(name, dataset)

  #get matrix of coordinates - make line
  coords = cbind(dataset$lon, dataset$lat)
  ln = Line(coords)  # make line, assume only one beacon - one line
  lns = Lines(list(ln), ID=filename)  # make this the only line in lines
  lnssp = SpatialLines(list(lns))  # turn into a spatial class
  proj4string(lnssp) <- CRS(c("+proj=longlat +ellps=WGS84"))  # add the CRS
  
  lndf = data.frame(filename)    #make a dataframe to go with the line...
  names(lndf) = 'beacon'      
  # rownames have to match!
  row.names(lndf) = sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
  # link spatial lines with dataframe
  lnsspdf = SpatialLinesDataFrame(lnssp, data = lndf)
  # get coords, make points
  coordinates(dataset) = c("lon", "lat")
  #define the projection - assume WGS84 lat lon
  proj4string(dataset) <- CRS(c("+proj=longlat +ellps=WGS84"))

  # overwrite all files - backup first if you want to save the files!
  delfile(filename, c("shp","dbf","prj","shx","kml","gpx"))

	#write files:
# shp
  writeOGR(dataset, getwd(), paste(filename, "_pt", sep=""),
     	driver='ESRI Shapefile')
  writeOGR(lnsspdf, getwd(), paste(filename, "_ln", sep=""), 
     	driver='ESRI Shapefile')

#KML
  #Can't overwrite KMLs
   writeOGR(dataset, paste(getwd(),paste(filename, "_pt", ".kml", sep=""), sep="/"), filename, "KML")
   writeOGR(lnsspdf, paste(getwd(),paste(filename, "_ln", ".kml", sep=""), sep="/"), filename, "KML")

#GPX  Doesn't work properly for tracks...!!!

  df = dataset[,c(1,3)]
  names(df) = c('name','time')
  writeOGR(df, paste(getwd(),paste(filename, "_pt", ".gpx", sep=""), sep="/"), 'waypoints', "GPX")
  #writeOGR(df, paste(getwd(),paste(filename, "_ln", ".gpx", sep=""), sep="/"), 'tracks', "GPX")

  }
########################             

# look for daily changes in distance (hourly is too noisy?)
# if doy has changed then a new day has dawned...
doy = as.numeric(strftime(drift$gps_time, "%j"))
differentdate = doy[1:length(doy)-1]-doy[2:length(doy)]
index = which(differentdate != 0)+1  

#copy data and subset on index (not a 24 hr sampling but the day changed)
daydatetime = drift$gps_time[index]
daylat = drift$lat[index]
daylon = drift$lon[index]
daydt = as.numeric(difftime(daydatetime[2:length(daydatetime)],
        daydatetime[1:length(daydatetime)-1], units="hours"))
 
# get the distance travelled between positions 
dist = vector(mode = 'numeric', length=0)
az = vector(mode = 'numeric', length=0)
err = vector(mode = 'numeric', length=0)

for (i in 1:(length(daylat)-1) ) {
	vect = distaz(daylat[i], daylon[i],daylat[i+1], daylon[i+1])      
	dist = append(dist, vect$dist)
	az = append(az, vect$az)
	err = append(err, vect$err)
	}
        # code likely based on spherical earth - some inaccuracy over large distance
        # suggest Kalman Filter before doing this?
daydist = data.frame(dist, az, err)


#calc the speed
dayspeed = daydist$dist/daydt # in kph
dayspeed = dayspeed*24 #in km per day
dayaz = daydist$az
# get rid of the negs
for (i in 1:length(dayaz)) {
  if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
  }
dayaz = dayaz*pi/180


#https://stat.ethz.ch/pipermail/r-help/2002-October/025964.html     
polarplot(dayspeed, dayaz, theta.zero = pi/2, theta.clw = TRUE, method = 1,
    rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
    grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
    lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
    polygon.bottom = TRUE, overlay = NULL, pi2.lab = FALSE, text.lab = c("N","E","S","W"),
    num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
    rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
    tlabel.col = "black", main = "Ice island daily velocity (km/d)", sub = fout)

savePlot(filename = paste(fout, "_polarplot", sep=""), 
		type = imgtype, device = dev.cur(), restoreConsole = TRUE)

plot(daydatetime[1:length(daydatetime)-1], dayspeed, ylab = "Speed (km/d)", xlab = "Date", 
    main = "Ice island daily speed", sub = fout)

savePlot(filename = paste(fout, "_speedplot", sep=""), 
		type = imgtype, device = dev.cur(), restoreConsole = TRUE)

# Doesn't work!!!
#plot((ecdf(dayspeed)), ylab = "Probability", xlab = "Speed [km/day]",
#    main = "Cummulative density function")


                
############################## OUTPUT DATA
# this indicates a good positional accuracy - see argos2shp - the actual accuracy is better than this (but unknown)
locQual = rep(3, length(drift$lat))
                                                                                                                   
# setup the data
drift = data.frame(drift$beacon, as.character(drift$gps_time), drift$lon, drift$lat, 
          drift$airTemp, drift$atmPres, drift$batt, locQual, dst$dist, dst$az, dst$cummuDst, dst$speed)
vars = c('beacon', 'gps_time', 'lon', 'lat', 'airTemp', 'atmPes', 'batt', 'locQual',
        'distance','bearing','cummulat','speed')
names(drift) = vars

# write the data to csv file
setwd("C://Users/acrawford/Dropbox/RProj/beacons/beacon_data/ProcessedData/shp")

write.table(drift, paste(fout, "_R.csv", sep=""), sep = ",", col.names=TRUE, row.names=FALSE)


shpout(drift,fout)