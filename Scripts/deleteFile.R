deleteFile = function(filename, ext) {
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