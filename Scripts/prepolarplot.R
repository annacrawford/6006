prepolarplot<-
  function(beacon)
  {
# look for daily changes in distance (hourly is too noisy?)
# if doy has changed then a new day has dawned...
imgtype = "png"  
    
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


#https://stat.ethz.ch/pipermail/r-help/2002-October/025964.html     
polarplot(dayspeed, dayaz, theta.zero = pi/2, theta.clw = TRUE, method = 1,
          rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
          grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
          lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
          polygon.bottom = TRUE, overlay = NULL, pi2.lab = FALSE, text.lab = c("N","E","S","W"),
          num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
          rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
          tlabel.col = "black", main = "Ice island daily velocity (km/d)", sub = substring(fname1,1,6))

savePlot(filename = paste("polarplot_", substring(fname1,1,5), sep=""), type = "png",
          device = dev.cur())

plot(daydist[1:length(daydist$dayspeed)-1], daydist$dayspeed, ylab = "Speed (km/d)", xlab = "Date", 
     main = "Ice island daily speed", sub = substring(fname1,1,6))

savePlot(filename = paste(fout, "_speedplot", sep=""), 
         type = imgtype, device = dev.cur(), restoreConsole = TRUE)
}

# Doesn't work!!!
#plot((ecdf(dayspeed)), ylab = "Probability", xlab = "Speed [km/day]",
#    main = "Cummulative density function")