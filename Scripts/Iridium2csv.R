#########################################################################################
######################Processing of raw beacon data###################################### 
# Programmer: Derek Mueller/Anna Crawford - Carleton University 
# Created: 2011
# Last Modified: 5 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert raw Iridum beacon data to a standardied csv before
# further processing to 'quality added' files. Called on by beacon_processing.R  
#
# Necessary file: Iridium raw beacon data This should be kept in an 
# 'input' directory with other raw data. This is downloaded from a gmail account
# using a Python script created by C. Lopes

# Note: Check longitude at line 44. Changes if > or < 100 degrees
#########################################################################################

Iridium2csv <- 
    function(Drift)
    {
      ### The following information is kept in but noted out incase the script ever needs to 
      # be run in stand-alone format
      # Set working directory (where raw beacon files are kept)
      # setwd("/tank/HOME/acrawford/6006/RawBeaconData")
      
      # Change to specific Iridum beacon file - without file extension and read in data
      # fname1 = "463170_2012.csv"
      # Drift <- read.csv(fname1, header=T)
      
      # The columns in the Iridium beacon data (used for standard format) are:
      # IMEI Year Month Day Hour Minute Latitude Longitude Temperature Voltage Battery AtmPress FormatID
      # Standard format is:
      vars = c('beacon','year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'airTemp', 'batt', 'atmPres', 'FormatID')
      names(Drift) = vars
      Beacon <- Drift

      # Clean the raw data by removing impossible data
      # Remove all months (x) > 12
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
      Beacon <- Beacon[Beacon[,10]<voltage,]

      # Remove impossible locations (latitudes >90 degrees)
      latmax <- 91
      latmin <- 30
      Beacon <- Beacon[Beacon[,7]<latmax,]
      Beacon <- Beacon[Beacon[,7]>latmin,]

      # Remove impossible longitudes (longitudes west of 180 degree)s
      lonmin <-  -120
      Beacon <- Beacon[Beacon[,8]>lonmin,]

      # To remove impossible jumps in distance
      # Calculate distance traveled, calculate speed, determine a threshold speed, remove entries
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
      datetime = strftime(strDateTime, "%Y-%m-%d %H:%M:%S")    # Formatting
      datetime = as.POSIXct(datetime, tz = "UTC")    # Time format
      gps_time = datetime    # Name for dataframe  

      obs = seq(1,length(Beacon$year))    # Makes a list of observation numbers 
      Beacon$beacon = substring(fname1, 1, 6)    # Adds a beacon name column

      #Put the new vectors into the dataframe to keep together
      Beacon = data.frame(Beacon, obs, gps_time)

      # Calculating distance
      for (i in 1:2) { #can decide how long to run, possibly for(i in 1:length(Beacon$speed)), just very slow
       dist = vector(mode = 'numeric', length = 0)
       az = vector(mode = 'numeric', length = 0)
       err = vector(mode = 'numeric', length = 0)
  
      for (i in 1:(length(Beacon$lat)-1)) {
       vect = Distaz(Beacon$lat[i], Beacon$lon[i],Beacon$lat[i+1], Beacon$lon[i+1])      
  	   dist = append(dist, vect$dist)
       az = append(az, vect$az)
       err = append(err, vect$err)
      }
  
      dst = data.frame(dist, az, err)
  
      # Get the time difference between positions
      dt = as.numeric(difftime(Beacon$gps_time[2:length(Beacon$gps_time)],
                      Beacon$gps_time[1:length(Beacon$gps_time)-1], units = "hours"))

      # Calculate the speed
      speed = dist/dt # in kph

      # Add speed column to original data frame
      Beacon$speed <- c(0, speed)

     # Remove imossible speeds 
     sp <- 25    # Speeds greater than 25 kph
     Beacon <- Beacon[Beacon[,15]<sp,]
   }    # End speed cleaning loop

     # Remove incomplete lines from beacon dataframe
     Beacon <- Beacon[complete.cases(Beacon[,1:15]),]


    ### Write csv
    # Choose file name  
    fout = fname1 

    # Choose output directory
    setwd("/tank/HOME/acrawford/6006/ProcessedBeaconData")
    #write csv file
    write.table(Beacon, paste(fout, '.csv', sep= " "), sep = ",", col.names = TRUE, row.names = FALSE)
  }    # End function