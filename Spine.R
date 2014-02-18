#Main beacon script
#Trying to see differeces
#More differneces, now see it on GitHub

# reset all vars
#install.packages(c("tcltk2", "GEOmap", "chron", "shapefiles", "sp", "rgdal"))

rm(list=ls(all=TRUE))
library(sp)
library(rgdal)
#library(shapefiles)
library(GEOmap)
library(chron)
library(tcltk2)

# Parameters for this script
progdir = '/home/acrawford/wrkdir/anna/beacons/PolluxWork/original_scripts'  #-# Dir where r files are located
wrkdir = '/home/acrawford/wrkdir/anna/beacons' 	#-# Dir where data files are located
fname = "031950_2011.csv"  		#-# name file with ext

fname = tk_choose.files(paste(wrkdir, fname, sep="/"), caption = "Select an iridium beacon track file", multi = FALSE)
imgtype = "png" 		#-# use png for regular use, eps for publishing
skip_lines = 1
##############################