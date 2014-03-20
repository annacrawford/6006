#########################################################################################
######################Deleting replicated files######################################### 
# Programmer: Derek Mueller - Carleton University 
# Created: 2011
# Last Modified: 7 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Function to delete files already existing in processed beacon data directory
# which the script is attempting to write. Called on by BeaconProcessing.R  
#########################################################################################


DeleteFile = function(filename, ext) {
  for (i in 1:length(ext)) {
    if ( file.exists( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") )
    }
    if ( file.exists( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") )
    }
    if ( file.exists( paste(getwd(),paste(filename, ext[i], sep="."), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, ext[i], sep="."), sep="/") )
    }
  } #end for
} #end function