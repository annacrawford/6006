### Script to take sbd beacon csv files (created after running python script through 
# gmail account), sort by date (Y,M,D,H,Min), remove non-sense rows (impossible dates), 
# remove double entries, remove any suspicious jumps by removing large speed jumps

# Started by Anna Crawford - Feb 6/2013
# Last updated by Anna Crawford - Feb 20/2014

########################################################################################
library(sp)
library(rgdal)
library(GEOmap)
library(chron)

#Remove previous stuff from R
rm(list=ls())

#set working directory
#change to where beacon files are kept
setwd("/tank/HOME/acrawford/6006/RawBeaconData")

#original data
fname1 = "463170_2012.csv"
beacon <- read.csv(fname1, header=T)

# The columns in the Iridium beacon data (used for standard format) are:
#IMEI Year Month Day Hour Minute Latitude Longitude Temperature Voltage Battery AtmPress FormatID
vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
names(beacon) = vars

########removing impossible data
#remove all months >12
x<-13
beacon<-beacon[beacon[,3]<x,]
#remove all days >31
y<-32
beacon<-beacon[beacon[,4]<y,]
#remove all hours > 24
z<-25
beacon<-beacon[beacon[,5]<z,]

#sort data (rows) based on month, day, hour, minute
beacon<-beacon[order(beacon$year, beacon$month, beacon$day, beacon$hour, beacon$minute), ]

#remove duplicates
beacon<-beacon[!duplicated(beacon[,2:6]), ]

#remove impossible battery voltage (is this ok? - there are some (6 in total - one hour of 
#entries) that I know are wrong but cannot have removed any other way it seems). 
voltage<-15
beacon<-beacon[beacon[,10]<voltage,]

#remove impossible locations 
#remove latitudes >90 degrees
latmax<-91
latmin<-30
beacon<-beacon[beacon[,7]<latmax,]
beacon<-beacon[beacon[,7]>latmin,]


#To remove impossible jumps in distance
#Calculate distance traveled, calculate speed, determine a threshold speed, remove entries
#with speeds above this. Still have to do in a loop format (incase 5 entries in a row are all erroneous)

# following is D.Mueller's work from Iridium2Shp.r script                                                          
# get the distance travelled between positions 

# translate between date time formats
bdate = paste(beacon$year, 
    formatC(beacon$month, flag="0", width=2), 
		formatC(beacon$day, flag="0", width=2), 
		sep="-")
strTime = paste(formatC(beacon$hour, flag="0", width=2),  
	      formatC(beacon$minute, flag="0", width=2),
		sep=":")
strDateTime = paste(bdate, strTime, sep=" ")
datetime = strftime(strDateTime, "%Y-%m-%d %H:%M:%S")
datetime = as.POSIXct(datetime, tz="UTC")
gps_time = datetime

obs = seq(1,length(beacon$year))
beacon$beacon = substring(fname1, 1, 6)

#put the new vectors into the dataframe to keep together
beacon = data.frame(beacon, obs, gps_time)


for (i in 1:5) { #can decide how long to run, possibly for(i in 1:length(beacon$speed)), just very slow
    dist = vector(mode = 'numeric', length=0)
    az = vector(mode = 'numeric', length=0)
    err = vector(mode = 'numeric', length=0)
    
#for (i in 1:(length(beacon$lat)-1) ) {
 # vect = distaz(beacon$lat[i], beacon$lon[i],beacon$lat[i+1], beacon$lon[i+1])      
#	dist = append(dist, vect$dist)
#	az = append(az, vect$az)
#	err = append(err, vect$err)

      
    dst = data.frame(dist, az, err)


    # get the time difference between positions
    dt = as.numeric(difftime(beacon$gps_time[2:length(beacon$gps_time)],
        beacon$gps_time[1:length(beacon$gps_time)-1], units="hours"))

    #calc the speed
    speed = dist/dt # in kph

    #add speed column to original data frame
    beacon$speed<-c(0, speed)

####See Derek's email about re-doing this loop - how to remove/recalc speeds based without 
#looping through entire vector 

    #speed threshold: 25 kph
    #remove speeds >25 kph
    sp<-25 #try and find highest reported speed... sp in kph 
    beacon<-beacon[beacon[,15]<sp,]
    }

beacon<-beacon[complete.cases(beacon[,1:15]),]


# get a filename for csv here
fout = fname1 # as.character(beacon$beacon[1])

# set directory for output csv file
setwd("/tank/HOME/acrawford/6006/ProcessedBeaconData")
#write csv file
write.table(beacon, paste(fout), sep = ",", col.names=TRUE, row.names=FALSE)
