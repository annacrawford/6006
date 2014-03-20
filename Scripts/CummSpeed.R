#########################################################################################
######################Quality added beacon files ######################################## 
# Programmer: Derek Mueller/Anna Crawford - Carleton University 
# Created: 2011
# Last Modified: 6 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert standardized beacon csv to cummulative speed plot  
# Called on by BeaconProcessing.R  
#
# Necessary file: Processed/standardized beacon csv file 
#############################################################################################
CummSpeed <-  
  function(Beacon)
  {
    # Calculating independent number of days
    doy  =  as.numeric(strftime(Beacon$gps_time, "%j"))
    differentDate = doy[1:length(doy)-1]-doy[2:length(doy)]
    index = which(differentDate != 0)+1  
    
    # Copy data and subset on index (not a 24 hr sampling but the day changed)
    dayDateTime = Beacon$gps_time[index]
    dayLat = Beacon$lat[index]
    dayLon = Beacon$lon[index]
    daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                                dayDateTime[1:length(dayDateTime)-1], units = "hours"))
    
    # Get the distance travelled between positions 
    dist = vector(mode = 'numeric', length = 0)
    az = vector(mode = 'numeric', length = 0)
    err = vector(mode  =  'numeric', length = 0)
    
    for (i in 1:(length(dayLat)-1) ) {
      vect = Distaz(dayLat[i], dayLon[i], dayLat[i+1], dayLon[i+1])      
      dist = append(dist, vect$dist)
      az = append(az, vect$az)
      err = append(err, vect$err)
    }    # End distance function
    
    DayDist = data.frame(dist, az, err, daydt, dayDateTime[1:i])
    # Get rid of rows with NAs
    DayDist = DayDist[complete.cases(DayDist),]
    
    # Calculate the speed
    daySpeed = DayDist$dist/DayDist$daydt # in kph
    daySpeed = daySpeed*24 #in km per day
    dayaz = DayDist$az
    
    # Get rid of the negatives
    for (i in 1:length(dayaz)) {
      if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
    }    # End negative function
    
    dayaz = dayaz*pi/180
    
    # Plot cummulative density function for speed vs probability
    plot((ecdf(daySpeed)), ylab = "Probability", xlab = "Speed [km/day]",
     main = "Cummulative density function")
    
    # Delete existing files
    DeleteFile(paste("cummSpeed", substring(fname1,1,5), sep = ""), c("png"))
        
    # Save to output directory
    setwd(output)
    dev.copy(png, paste("cummSpeed_", substring(fname1,1,5), ".png", sep = ""))
    dev.off()
    
  }    # End function