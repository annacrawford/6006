QualityAdded <-
  function(beacon)
  {
    #Converting from standardized csv to point shapefile. 
    csv2shp(Beacon, fname1) 
    
    # Note if using Arc:
    # Before importing to ArcInfo make a text file called *.prj with the following line:
    # That will tell ArcInfo that it is not projected - WGS84 coords.
    # copy the file:  WGS1984.prj, which contains the following:
    # GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],
    # PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]
    # file.copy("WGS1984.prj", paste(buoy, ".prj", sep=""), overwrite = TRUE)
    
    # Converting from standardized csv to kml
    # Note: kml files cannot be overwritten. Move these to a different directory if same file name 
    # exists in output directory already
    csv2kml(Beacon, fname1) 
    
    # Converting from standardized csv to gpx 
    csv2gpx(Beacon, fname1)
    
    # Converting csv to polarPlot - polarplot called within prepolarplot function
    PrePolarPlot(Beacon) 
    
    # Converting csv to speedPlot
    SpeedPlot(Beacon)
    
    # Converting csv to cummSpeed plot
    CummSpeed(Beacon)
    
    # Text file of summary stats
    IceIslandStats(Beacon)
  }    # End function