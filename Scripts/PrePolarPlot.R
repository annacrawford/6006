#########################################################################################
######################Quality added beacon files ######################################## 
# Programmer: Derek Mueller/Anna Crawford - Carleton University 
# Created: 2011
# Last Modified: 6 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert standardized beacon csv to polarplot. 
# Called on by BeaconProcessing.R  
#
# Necessary file: Processed/standardized beaconcsv file. 

# Realize that there is an added line to remove rows with NAs when calculating azimuth. 
# Is there another way to fix this?
############################################################################################
PrePolarPlot <- 
  function(Beacon)
  {
    # Finding independent number of days 
    doy = as.numeric(strftime(Beacon$gps_time, "%j"))
    differentDate = doy[1:length(doy) - 1]-doy[2:length(doy)]
    index = which(differentDate != 0) + 1  
    
    # Copy data and subset on index (not a 24 hr sampling but the day changed)
    dayDateTime = Beacon$gps_time[index]
    dayLat = Beacon$lat[index]
    dayLon = Beacon$lon[index]
    daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                                dayDateTime[1:length(dayDateTime) - 1], units = "hours"))
    
    # Get the distance travelled between positions 
    dist = vector(mode = 'numeric', length = 0)
    az = vector(mode = 'numeric', length = 0)
    err = vector(mode = 'numeric', length = 0)
    
    for (i in 1:(length(dayLat) - 1) ) {
      vect = Distaz(dayLat[i], dayLon[i],dayLat[i + 1], dayLon[i + 1])      
      dist = append(dist, vect$dist)
      az = append(az, vect$az)
      err = append(err, vect$err)
    }    # end distace function
    
    DayDist = data.frame(dist, az, err, daydt, dayDateTime[1:i])
    
    # Get rid of rows with NAs
    DayDist = DayDist[complete.cases(DayDist),]
    
    # Calculate the speed
    daySpeed = DayDist$dist / DayDist$daydt    # kph
    daySpeed = daySpeed*24    # km per day
    dayaz = DayDist$az
    
    # Get rid of the negatives
    for (i in 1:length(dayaz)) {
      if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
    }    # End azimuth function
    
    dayaz = dayaz*pi/180
    
    # Plot
    # Source help: https://stat.ethz.ch/pipermail/r-help/2002-October/025964.html     
    PolarPlot(daySpeed, dayaz, theta.zero = pi/2, theta.clw = TRUE, method = 1,
              rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
              grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
              lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
              polygon.bottom = TRUE, overlay = NULL, pi2.lab = FALSE, text.lab = c("N","E","S","W"),
             num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
              rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
              tlabel.col = "black", main = "Ice island daily velocity (km/d)", sub = fname1)

    # Delete existing files
    DeleteFile(paste("polarplot_", fname1, sep = ""), c("png"))
    
    # Save polarplot to output file  
    dev.copy(png, paste("polarplot_", fname1, ".png", sep = ""))
    dev.off()
    
     }    # End function