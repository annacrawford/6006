########################################################################################
######################Processing of raw beacon data###################################### 
# Programmer: Derek Mueller/Anna Crawford - Carleton University 
# Created: 5 March 2014
# Last Modified: 5 March 2014
# Project: Ice island Drift, deterioration and detection 
# Descrition: Function to convert raw Oceanetics beacon data to a standardied csv before
# further processing to 'quality added' files. Called on by beacon_processing.R  
#
# Necessary file: Oceanetic raw beacon data. This should be kept in an 
# 'input' directory with other raw data. 
# 
# Note: Check longitude at line 44. Changes if > or < 100 degrees
#########################################################################################

Oceanetics2csv <- 
  function(Drift)
  {
    ### The following information is kept in but noted out incase the script ever needs to 
    # be run in stand-alone format
    # Set working directory
    # Change to where raw Beacon files are kept
    # setwd("/tank/HOME/acrawford/6006/RawBeaconData")
    
    # Change to specific Oceanetics beacon - without file extension and read in data
    # fname1  =  "300034013460170.csv"        
    # Drift  =  read.csv(fname1, header = TRUE)
    
    # To columns in the Oceanetics csv are: ID,  yyyy,	mm,	dd,	hh,	lat,	long,                         
    # Standard format is: 
    # vars = c('beacon','year','month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
    
    ### Declare variables  
    beacon <- Drift$ID    # beacon name
    year <- Drift$yyyy    # dates - year
    month <- Drift$mm    # dates - month
    day <- Drift$dd    # dates - day
    hour <- Drift$hh    # dates - hour
    minute = 0    # no minute data
    lat <- Drift$lat    # latitude  
    lon <- Drift$long    # longitude 
    
    #if >100 degrees
    #lon_deg <- as.numeric(substr(Drift[,5],1,4))
    #lon_dec_min <- (as.numeric(substr(Drift[,5],6,14))/60)
    #lon <- lon_deg+lon_dec_min
    
    # Declaring of extra information - may or may not be included in raw data. 
    # Here is the likely available data. -99 is assigned to variables included in the standardized
    # csv but are not provided by this source type. 
    airTemp <- -99
    batt <- -99
    atmPres <--99
    FormatID <- -99    
    
    # Put the declared variables into one data frame and assign the column headings
    Beacon <- data.frame(beacon,year, month, day, hour, minute, lat, lon, airTemp, batt, atmPres, FormatID)
    vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
    names(Beacon) = vars
    
    ### Clean the raw data by removing impossible data
    # Remove all months (x) > 12
    x <- 13
    Beacon <- Beacon[Beacon[,3] < x,]
    #remove all days (y) > 31
    y <- 32
    Beacon <- Beacon[Beacon[,4] < y,]
    #remove all hours (z) > 24
    z <- 25
    Beacon <- Beacon[Beacon[,5] < z,]
    
    # Sort data (rows) based on year, month, day, hour, minute (ascending)
    Beacon <- Beacon[order(Beacon$month, Beacon$day, Beacon$hour, Beacon$minute), ]
    
    # Remove duplicates rows
    Beacon <- Beacon[!duplicated(Beacon[,2:6]), ]
    
    # Remove impossible battery voltage 
    voltage <- 15
    Beacon <- Beacon[Beacon[,10] < voltage,]
    
    # Remove impossible locations 
    # Remove latitudes >90 degrees
    latmax <- 91
    latmin <- 30
    Beacon <- Beacon[Beacon[,7] < latmax,]
    Beacon <- Beacon[Beacon[,7] > latmin,]
    
    # To remove impossible jumps in distance
    # calculate distance traveled, calculate speed, determine a threshold speed, remove entries
    # with speeds above this. Still have to do in a loop format (incase 5 entries in a row are all erroneous)
    
    # Following is D.Mueller's work from the Iridium2Shp.r script                                                          
    # First, get the distance travelled between positions    
    # Translate between date time formats
    bdate = paste(Beacon$year, 
                  formatC(Beacon$month, flag = "0", width = 2), 
                  formatC(Beacon$day, flag = "0", width = 2), 
                  sep = "-")
    strTime = paste(formatC(Beacon$hour, flag = "0", width = 2),  
                    formatC(Beacon$minute, flag = "0", width = 2),
                    sep = ":")
    strDateTime = paste(bdate, strTime, sep = " ")
    datetime = strftime(strDateTime, "%Y-%m-%d %H:%M:%S")    # Formatting
    datetime = as.POSIXct(datetime, tz = "UTC")    # Time format
    gps_time = datetime    # Name for dataframe
    
    obs = seq(1,length(Beacon$year))    # Makes a list of observation numbers 
    Beacon$beacon = substring(fname1, 10, 15)    # Adds a beacon name column
    
    # Put the new vectors into the dataframe to keep together
    Beacon = data.frame(Beacon, obs, gps_time)
    
    
    for (i in 1:2) { 
      dist = vector(mode = 'numeric', length = 0)
      az = vector(mode = 'numeric', length = 0)
      err = vector(mode = 'numeric', length = 0)
      
    for (i in 1:(length(Beacon$lat)-1)) {
      vect = Distaz(Beacon$lat[i], Beacon$lon[i],Beacon$lat[i+1], Beacon$lon[i+1])      
      dist = append(dist, vect$dist)
      az = append(az, vect$az)
      err = append(err, vect$err)
    }    # End distance loop
      
      dst = data.frame(dist, az, err)
      
      # Get the time difference between positions
      dt = as.numeric(difftime(Beacon$gps_time[2:length(Beacon$gps_time)],
                               Beacon$gps_time[1:length(Beacon$gps_time)-1], units = "hours"))
      
      # Calculate the speed
      speed = dist/dt # in kph
      
      # Add speed column to original data frame
      Beacon$speed <- c(0,speed)
      
      # Remove impossible speeds
      sp <- 25    # Speeds greater than 25 kp
      Beacon <- Beacon[Beacon[,15]<sp,]
    }  # End speed cleaning loop
    
    # Remove incomplete lines from beacon dataframe 
    Beacon <- Beacon[complete.cases(Beacon[,1:15]),]
        
    ### Write csv
    # Choose output directory 
    setwd("/tank/HOME/acrawford/6006/ProcessedBeaconData")
    write.csv(Beacon, paste(fname1,'.csv', sep=""),  row.names=FALSE)
    }    # End function