csv2shp_pt <-
  function(beacon) 
{
id = seq(1,length(beacon$lon))
drift = data.frame(id, beacon)
dd = data.frame(id, beacon$lon, beacon$lat)
ddShapefile <- convert.to.shapefile(dd, drift, "id", 1)
write.shapefile(ddShapefile, substring(fname1,1,6), arcgis=T)
}