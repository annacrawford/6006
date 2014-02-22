######################
# function to write dataset to a point and line shapefile, given a dataframe
# and output filename.  The dataframe must have columns lat + lon in wgs84
# also only one beacon (line) per dataframe.  

csv2shp = function(dataset, filename) {         
  
  # generate a field that is a unique identifier for each point (name of point) 
  name = as.character(dataset$beacon)
  name = substr(name,nchar(name)-5,nchar(name)) #the name is last 6 digits of imei
  obs = seq(1,length(name))
  name = paste(name, formatC(obs, flag="0", width=5), sep="_")
  dataset = data.frame(name, dataset)
  
  #get matrix of coordinates - make line
  coords = cbind(dataset$lon, dataset$lat)
  ln = Line(coords)  # make line, assume only one beacon - one line
  lns = Lines(list(ln), ID=filename)  # make this the only line in lines
  lnssp = SpatialLines(list(lns))  # turn into a spatial class
  proj4string(lnssp) <- CRS(c("+proj=longlat +ellps=WGS84"))  # add the CRS
  
  lndf = data.frame(filename)    #make a dataframe to go with the line...
  names(lndf) = 'beacon'      
  # rownames have to match!
  row.names(lndf) = sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
  # link spatial lines with dataframe
  lnsspdf = SpatialLinesDataFrame(lnssp, data = lndf)
  # get coords, make points
  coordinates(dataset) = c("lon", "lat")
  #define the projection - assume WGS84 lat lon
  proj4string(dataset) <- CRS(c("+proj=longlat +ellps=WGS84"))
  
  # overwrite all files - backup first if you want to save the files!
  deleteFile(filename, c("shp","dbf","prj","shx"))
  
  #write files:
  # shp
  
  writeOGR(dataset, getwd(), paste(filename, "_pt", sep=""),
           driver='ESRI Shapefile')
  writeOGR(lnsspdf, getwd(), paste(filename, "_ln", sep=""), 
           driver='ESRI Shapefile')

}