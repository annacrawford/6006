
#Run before polarpolot, speedplot or cummDist
prePlots<-
  function(beacon)
  {
    # look for daily changes in distance (hourly is too noisy?)
    # if doy has changed then a new day has dawned...
    
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
    daydist = data.frame(dist, az, err, daydt, daydatetime[1:i])
    # Get rid of rows with NAs
    daydist = daydist[complete.cases(daydist),]
    
    #calc the speed
    dayspeed = daydist$dist/daydist$daydt # in kph
    dayspeed = dayspeed*24 #in km per day
    dayaz = daydist$az
    
    # get rid of the negatives
    for (i in 1:length(dayaz)) {
      if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
    }
    dayaz = dayaz*pi/180
  }    