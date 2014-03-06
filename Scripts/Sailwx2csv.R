#########################################################################################
######################Processing of raw beacon data###################################### 
# Programmer: Anna Crawford/Derek Mueller- Carleton University 
# Created: 2011
# Last Modified: 5 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert beacon data collected by sailwx.ino to a standardied csv 
# before further processing to 'quality added' files. Called on by beacon_processing.R  
#
# Necessary file: sailwx raw beacon data in csv format This should be kept in an 
# 'input' directory with other raw data. 

# To access sailwx data: go to sailwx.info --> ship tracker --> search by  WMO call sign
# 'dump ship history' and bottom of recent coordinate transmissions
# Select all - copy to in Excel work file  - delete first 4 rows - save as "47###.csv'
# Make sure that there is no comma at the end of the header row

# Make sure to subset data frame based on start date (line 50-54)
# Note: Check longitude at line 44. Changes if > or < 100 degrees
##########################################################################################

Sailwx2csv <-
  function(Drift)
  {
    ### The following information is kept in but noted out incase the script ever needs to 
    # be run in stand-alone format
    ### Set working directory (where raw beacon files are kept)
    # setwd("/tank/HOME/acrawford/6006/RawBeaconData")

    # Change to specific Sailwx beacon file - without file extension
    # fname1 = "47551"
    
    # Read in the data
    # Drift = read.table(fname1, header=T, sep=",", dec=".", na.strings="NULL", strip.white=FALSE)

    # Sailwx column headers are: #X.UTC.date.time, Unix.UTC.timestamp, lat, lon, callsign, wind.from, knots, gust, barometer
    # air.temp, dew.point, water.temp

    # Standard format is: 
    # vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
    
    ### Declare variables
    beacon <- Drift[,5]   # beacon name
    date <- strptime(Drift[,1], format="%Y-%b-%d %H%M")
    year <- as.numeric(substr(date, 1,4)) # date - year
    month <- as.numeric(substr(date, 6,7))    # date = month
    day <- as.numeric(substr(date, 9,10))    # date = day
    hour <- as.numeric(substr(date, 12,13))    # time = hour
    minute <- as.numeric(substr(date, 15, 16))    # time = minute
    lat <- Drift[,3]    # latitude
    lon <- Drift[,4]    # longitude
    
    # Declaring of extra information - may or may not be included in raw data. 
    atmPres <- Drift[,9]    # atmospheric pressure
    airTemp <- Drift[,10]    # air temperature
    batt <- length(Drift$beacon)    # battery voltage
    formatID <- length(Drift$FormatID)    # Format ID

    # Put the declared variables into one data frame and assign the column headings
    Beacon <- data.frame(beacon,year, month, day, hour, minute, lat, lon, airTemp, batt, atmPres, formatID)
    vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'formatID')
    names(Beacon) = vars

    ### Clean the raw data by removing impossible data
    # Remove all months (x) >12
    x <- 13
    Beacon <- Beacon[Beacon[,3] < x,]
    # Remove all days (y) > 31
    y <- 32
    Beacon <- Beacon[Beacon[,4] < y,]
    # Remove all hours (z) > 24
    z <- 25
    Beacon <- Beacon[Beacon[,5] < z,]

    # Sort data (rows) based on year, month, day, hour, minute (ascending)
    Beacon <- Beacon[order(Beacon$year, Beacon$month, Beacon$day, Beacon$hour, Beacon$minute), ]

    # Remove duplicate rows
    Beacon <- Beacon[!duplicated(Beacon[,2:6]), ]

    # Remove impossible battery voltage
    voltage <- 15
    Beacon <- Beacon[Beacon[,10] < voltage,]

    # Remove impossible locations 
    # Remove latitudes >90 degrees
    latMax <- 91
    latMin <- 30
    Beacon <- Beacon[Beacon[,7] < latMax,]
    Beacon <- Beacon[Beacon[,7] > latMin,]

    # To remove impossible jumps in distance
    # calculate distance traveled, calculate speed, determine a threshold speed, remove entries
    # with speeds above this. Loop format 
    
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
    dateTime = strftime(strDateTime, "%Y-%m-%d %H:%M:%S")    # Formatting
    dateTime = as.POSIXct(dateTime, tz = "UTC")    # Time format
    gps_time = dateTime    # Name for dataframe

    obs = seq(1,length(Beacon$year))    # Makes a list of observation numbers
    Beacon$beacon = substring(fname1, 1, 5)    # Adds a beacon name column

    # Put the new vectors into the dataframe to keep together
    Beacon = data.frame(Beacon, obs, gps_time)

    # Calculating distance
    for (i in 1:2) { 
      dist = vector(mode = 'numeric', length = 0)
      az = vector(mode = 'numeric', length = 0)
      err = vector(mode = 'numeric', length = 0)
  
    for (i in 1:(length(Beacon$lat)-1) ) {
      vect = Distaz(Beacon$lat[i], Beacon$lon[i],Beacon$lat[i+1], Beacon$lon[i+1])      
      dist = append(dist, vect$dist)
      az = append(az, vect$az)
      err = append(err, vect$err)
    }
  
    # dst = data.frame(dist, az, err) # Need? Remove
  
    # Get the time difference between positions
    dt = as.numeric(difftime(Beacon$gps_time[2:length(Beacon$gps_time)],
                           Beacon$gps_time[1:length(Beacon$gps_time)-1], units = "hours"))
  
    # Calculate the speed
    speed = dist/dt    # in kph
  
    # Add speed column to original data frame
    Beacon$speed <- c(0, speed)
  
    # Remove impossible speeds
    sp <- 25    # Speeds greater than 25 kph
    Beacon <- Beacon[Beacon[,15] < sp,]
    } # End speed cleaning loop
    
    # Remove incomplete lines from beacon dataframe 
    Beacon <- Beacon[complete.cases(Beacon[,1:15]),]
 
    ### Write csv
    # Get a filename for csv here
    fout = as.character(Beacon$beacon[1])

    # Set directory for output csv file
    setwd("/tank/HOME/acrawford/6006/ProcessedBeaconData")
    write.table(Beacon, paste(fout, ".csv", sep=""), sep = ",", col.names=TRUE, row.names=FALSE)
  }  # End function

