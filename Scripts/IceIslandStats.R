#########################################################################################
######################Quality added output files######################################### 
# Programmer: Anna Crawford - PhD Candidate DGES - Carleton University 
# Created: 28 February 2014
# Last Modified: 6 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert processed/standardized csv beacon data to a summarized
# text file. Called on by BeaconProcessing.R  
#
# Necessary file: Standardized beacon csv. This should be kept in an 
# 'output' directory with other processed data files. Writte there with beacon_processing.R 
#########################################################################################

IceIslandStats <-
  function(Beacon)
  {
    # Finding day of year and number of differet days in beacon record
    doy = as.numeric(strftime(Beacon$gps_time, "%j"))
    differentDate = doy[1:length(doy)-1] - doy[2:length(doy)]
    index = which(differentDate != 0)+1  
    
    # Copy data and subset on index (not a 24 hr sampling but the day changed)
    dayDateTime = Beacon$gps_time[index]
    dayLat = Beacon$lat[index]
    dayLon = Beacon$lon[index]
    daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                                dayDateTime[1:length(dayDateTime)-1], units="hours"))
    
    # Get the distance travelled between positions 
    dist = vector(mode = 'numeric', length=0)
    err = vector(mode = 'numeric', length=0)
    
    for (i in 1:(length(dayLat) - 1) ) {
      vect = Distaz(dayLat[i], dayLon[i],dayLat[i + 1], dayLon[i + 1])      
      dist = append(dist, vect$dist)
      err = append(err, vect$err)
    }    # End function to find day distance
    
    DayDist = data.frame(dist, err, daydt, dayDateTime[1:i])
    
    # Calculate the speed (distance traveled per day)
    daySpeed = DayDist$dist / DayDist$daydt    # kph
    daySpeed = daySpeed * 24    # km per day
 
    ### Statistics to put into the text file
    # Start, end and total days tracked
    endDate <- tail(dayDateTime, n = 1)
    startDate <- dayDateTime[1]
    totalDays <- tail(cumsum(abs(differentDate)))[1]
    
    # Calculatig and declarig average speed, temperature, pressure, battery
    aveAtmPress <- mean(Beacon$atmPres)    #Pa    
    aveBatt <- mean(Beacon$batt)    # voltage
    aveSpd <- mean(daySpeed)    # kph    
    aveTmp <- mean(Beacon$airTemp)    # C  
        
    #Location & distance
    endLat <- tail(dayLat, n=1)
    endLon <- tail(dayLon,n=1)
    startLat <- dayLat[1]
    startLon <- dayLon[1]
    totalDist <- 1 #####FINISH
    direction <- 0 #####FINISH
      
    # Overwrite past summary files - backup first if you want to save the files!
    DeleteFile(paste("summary_", fname1, sep = ""), c("txt"))
    
    # Write text file to output directory with above summary information
    summary <- as.character(paste("summary_", fname1, ".txt", sep = ""))
       writeLines(c(paste("Summary information for beacon", fname1, sep = " "), " ",
                    paste("Start Date", startDate, sep = " "), 
                    paste("End Date", endDate, sep = " "),
                    paste("Total Days Tracked", totalDays, sep =" "), " ",
                    paste("Average Speed (kph)", aveSpd, sep = " "), 
                    paste("Average Temperature (C)", aveTmp, sep = " "), 
                    paste("Average Batter (V)", aveBatt, sep = " "), 
                    paste("Average Atmospheric Pressure (hPa)", aveAtmPress, sep = " "), " ",
                    paste("Starting Latitude", startLat, "Starting Longitude", startLon, sep = " "), 
                    paste("Ending Latitude", endLat, "Ending Longitude", endLon, sep = " "), 
                    paste("Total Distance (km)", totalDist), 
                    paste("Direction", direction)), summary)
    }    # End function     
   