### Script to take CIS/SailWX text dump to csv standard format

# go to sailwx.info --> ship tracker --> search by  WMO call sign
# 'dump ship history' and bottom of recent coordinate transmissions
# select all - copy to in Excel work file  - delete first 4 rows - save as "47###.csv'

#make sure to subset data frame based on start date (line 50-54)
#####################################################################

sailwx2csv <-
  function(drift)
{
#read in the data...  - there may be a comma at the end of the header line, just get rid of this by hand. 
#drift = read.table(fname1, header=T, sep=",",
 # 		dec=".", na.strings="NULL", strip.white=FALSE)

# sail wx column headers are: #X.UTC.date.time, Unix.UTC.timestamp, lat, lon, callsign, wind.from, knots, gust, barometer
#air.temp, dew.point, water.temp

#standard format is: 
#vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
beacon<-drift[,5] 
lat<-drift[,3]
lon<-drift[,4]
atmPres<-drift[,9]
airTemp<-drift[,10]
batt<-length(drift$beacon)
FormatID<-length(drift$FormatID)
date<-strptime(drift[,1], format="%Y-%b-%d %H%M")
#drift[date >= as.POSIXlt("2010-10-10 0000")]
year<-as.numeric(substr(date, 1,4))
month<-as.numeric(substr(date, 6,7))
day<-as.numeric(substr(date, 9,10))
hour<-as.numeric(substr(date, 12,13))
minute<-as.numeric(substr(date, 15, 16))

beacon<-data.frame(beacon,year, month, day, hour, minute, lat, lon, airTemp, batt, atmPres, FormatID)

#substitute in the starting date info below to get row that where info interested in starts 
#only if needed - if your text dump includes beacon placements you are not interested in (re-use of WMO#)
#which(drift$year==2012 & drift$month==10 & drift$day==10 & drift$hour==14) #& drift$minute==00) # For some reason can't get minute to work

#subset based on start date of beacon (output of line abo e)
#again, only if needed
#drift<-data.frame(drift[1:14428,])

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
beacon$beacon = substring(fname1, 1, 5)

#put the new vectors into the dataframe to keep together
beacon = data.frame(beacon, obs, gps_time)

#source('~/6006/Scipts/distaz/distaz.R')
for (i in 1:2) { #can decide how long to run, possibly for(i in 1:length(beacon$speed)), just very slow
  dist = vector(mode = 'numeric', length=0)
  az = vector(mode = 'numeric', length=0)
  err = vector(mode = 'numeric', length=0)
  
  for (i in 1:(length(beacon$lat)-1) ) {
    vect = distaz(beacon$lat[i], beacon$lon[i],beacon$lat[i+1], beacon$lon[i+1])      
    dist = append(dist, vect$dist)
    az = append(az, vect$az)
    err = append(err, vect$err)
  }
  
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
fout = as.character(beacon$beacon[1])

# set directory for output csv file
setwd("/tank/HOME/acrawford/6006/ProcessedBeaconData")
write.table(beacon, paste(fout, ".csv", sep=""), sep = ",", col.names=TRUE, row.names=FALSE)
}

