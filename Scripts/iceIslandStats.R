#Function for ice island stats

iceIslandStats<-
  function(beacon)
  {
    doy = as.numeric(strftime(beacon$gps_time, "%j"))
    differentdate = doy[1:length(doy)-1]-doy[2:length(doy)]
    index = which(differentdate != 0)+1  
    
    #copy data and subset on index (not a 24 hr sampling but the day changed)
    daydatetime = beacon$gps_time[index]
    daylat = beacon$lat[index]
    daylon = beacon$lon[index]
    daydt = as.numeric(difftime(daydatetime[2:length(daydatetime)],
                                daydatetime[1:length(daydatetime)-1], units="hours"))
    
    # get the distance travelled between positions 
    dist = vector(mode = 'numeric', length=0)
    err = vector(mode = 'numeric', length=0)
    
    for (i in 1:(length(daylat)-1) ) {
      vect = distaz(daylat[i], daylon[i],daylat[i+1], daylon[i+1])      
      dist = append(dist, vect$dist)
      err = append(err, vect$err)
    }
    # code likely based on spherical earth - some inaccuracy over large distance
    # suggest Kalman Filter before doing this?
    daydist = data.frame(dist, err, daydt, daydatetime[1:i])
    
    #calc the speed
    dayspeed = daydist$dist/daydist$daydt # in kph
    dayspeed = dayspeed*24 #in km per day
 
###Stats to put into a text file
#Start, end and total days tracked
startDate<-daydatetime[1]
endDate<-tail(daydatetime, n=1)
totalDays<-tail(cumsum(abs(differentdate)))[1]
    
#Average speed, temperature, battery
aveSpd<-mean(dayspeed) #kph    
aveTmp<-mean(beacon$airTemp)  
aveAtmPress<-mean(beacon$atmPres)    
aveBatt<-mean(beacon$batt)
    
#Location & distance
startLat<-daylat[1]
startLon<-daylon[1]
endLat<-tail(daylat, n=1)
endLon<-tail(daylon,n=1)
totalDist<-1
direction<-0
      
# overwrite all files - backup first if you want to save the files!
deleteFile(paste("summery_", substring(fname1,1,5), sep=""), c("txt"))
    
#Write text file to output directory with above summary information
summary<- as.character(paste("summary_", substring(fname1,1,5), ".txt", sep=""))
writeLines(c(paste("Summary information for beacon", substring(fname1,1,5), sep=" "), " ",
             paste("Start Date", startDate, sep=" "), 
             paste("End Date", endDate, sep=" "),
             paste("Total Days Tracked", totalDays, sep=" "), " ",
             paste("Average Speed (kph)", aveSpd, sep= " "), 
             paste("Average Temperature (C)", aveTmp, sep=" "), 
             paste("Average Batter (V)", aveBatt, sep=" "), 
             paste("Average Atmospheric Pressure (hPa)", aveAtmPress, sep=" "), " ",
             paste("Starting Latitude", startLat, "Starting Longitude", startLon, sep=" "), 
             paste("Ending Latitude", endLat, "Ending Longitude", endLon, sep=" "), 
             paste("Total Distance (km)", totalDist), 
             paste("Direction", direction)), summary)
  }    
   