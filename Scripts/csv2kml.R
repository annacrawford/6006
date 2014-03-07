#########################################################################################
######################Quality added beacon files ######################################## 
# Programmer: Derek Mueller - Carleton University 
# Created: 2011
# Last Modified: 6 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: function to write dataset to a point and line kml file, given a dataframe
# and output filename. The dataframe must have columns lat + lon in wgs84
# also only one beacon (line) per dataframe.  
# Called on by BeaconProcessing.R  
#
# Necessary file: Processed/standardized beacon csv file 
##########################################################################################

csv2kml <-
  function(Dataset, filename) 
    {         
      # Generate a field that is a unique identifier for each point (name of point) 
      name <- as.character(Dataset$beacon)
      name <- substr(name,nchar(name)-5,nchar(name)) #the name is last 6 digits of imei
      obs <- seq(1,length(name))
      name <- paste(name, formatC(obs, flag = "0", width = 5), sep = "_")
      Dataset <- data.frame(name, Dataset)
      
      # Get matrix of coordinates - make line
      coords <- cbind(Dataset$lon, Dataset$lat)
      ln <- Line(coords)     # Make line, assume only one beacon - one line
      lns <- Lines(list(ln), ID = filename)    # Make this the only line in lines
      lnssp <- SpatialLines(list(lns))     # Turn into a spatial class
      proj4string(lnssp) <- CRS(c("+proj=longlat +ellps=WGS84"))     # Add the CRS
      
      lndf <- data.frame(filename)      # Make a dataframe to go with the line.
      names(lndf) <- 'beacon'      
      # Rownames have to match!
      row.names(lndf) <- sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
      # Link spatial lines with dataframe
      lnsspdf <- SpatialLinesDataFrame(lnssp, data = lndf)
      # Get coords, make points
      coordinates(Dataset) <- c("lon", "lat")
      # Define the projection - assume WGS84 lat lon
      proj4string(Dataset) <- CRS(c("+proj=longlat +ellps=WGS84"))
      
      ### Write files
        
      # KML
      writeOGR(Dataset, paste(getwd(),paste(filename, "_pt", ".kml", sep = ""), sep = "/"), filename, "KML")
      writeOGR(lnsspdf, paste(getwd(),paste(filename, "_ln", ".kml", sep = ""), sep = "/"), filename, "KML")
    }    # End function