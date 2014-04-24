#########################################################################################
######################Processing of raw beacon data###################################### 
# Programmer: Anna Crawford - Carleton University 
# Created: 7 March 2014
# Last Modified: 7 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to convert raw  beacon data to a standardied csv before
# further processing to 'quality added' files. Called on by BeaconProcessing.R  
#
# Necessary file: Raw beacon data This should be kept in an 
# 'input' directory with other raw data. 
#########################################################################################
raw2csv <-
  function(beaconType, Drift) 
    { 
    if (beaconType == "Sailwx") {
      Sailwx2csv(Drift)
    } else if (beaconType == "Iridium") {
      Iridium2csv(Drift)
    } else if (beaconType == "Oceanetics") {
      Oceanetics2csv(Drift)
    } else if (beaconType == "Joubeh") {
      Joubeh2csv(Drift)
    } else {
      Canatec2csv(Drift)
    }    # End statement 
  
    #Test: Check that csv was written
   if (!file_test("-f", paste(fname1,".csv",sep=''))) {stop("csv file not written")}
    # End function
  }