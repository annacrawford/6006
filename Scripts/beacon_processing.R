#############################################################################
######################Processing of raw beacon data########################## 
# Takes in raw data after download from appropriate source 
# Creates a  standardized csv file with individual functions from raw data
# for each beacon type/source, respectively
#
# Calls on individual functions for various other output file types (shapefiles, 
# polarplot, etc.)

# Created by Anna Crawford - WIRL - PhD Candidate DGES - Carleton University - 2014
# distaz, polarplot functions written by Derek Mueller, WIRL - Assistant Professor DGES - Carleton University 
# Last updated by Anna Crawford - Feb 20/2014
##############################################################################
#Remove previous workspace
rm(list=ls(all=TRUE))

#Load Necessary librariers
library(sp)
library(rgdal)
library(GEOmap)
library(chron)
library(RSEIS)
library(RPMG)
library(sp)
library(shapefiles)
library(rgdal)
library(grDevices)


#name input directory 
input = "/tank/HOME/acrawford/6006/RawBeaconData" # Where raw data files kept
#name output directory 
output = "/tank/HOME/acrawford/6006/ProcessedBeaconData" # Where output files (csv, shp, polarplot, etc...) will be put

#Read in raw data
setwd(input)
fname1 = "47551.csv" #fname1 is the file name of the raw data that you will process
drift = read.table(fname1, header=T, sep=",", dec=".", na.strings="NULL", strip.white=FALSE)

###Source functions
#Set program directory 
prgdir = "/tank/HOME/acrawford/6006/Scipts" #Where scripts are kept
setwd(prgdir)

#Functions to convert raw data to standardized csv
source("distaz.R")
source("sailwx2csv.R")
source("joubeh2csv.R")
source("iridium2csv.R")

#Functions to convert standardized csv to quality added file types
source("prepolarplot.R")
source("polarplot.r")
source("csv2shp.R")
source("csv2kml.R")
source("csv2gpx.R")
#Functions to # overwrite all files - backup first if you want to save the files!
source("deleteFile.R")

#Convert raw data to standardized csv format. 
sailwx2csv(drift)
joubeh2csv(drift)
iridium2csv(drift)

#################################################################################
### Convert standardized csv files to quality added files 

#Need to read in new csv data located in output directory
setwd(output)
beacon<-read.table(fname1, header=T, sep=",", dec=".", na.strings="NULL", strip.white=FALSE)

#Converting from standardized csv to point shapefile. Output will be in output directory
csv2shp(beacon, substring(fname1,1,5)) # Beacon is the new name after raw data has been processed to standardized form 

# Before importing to ArcInfo make a text file called *.prj with the following line:
# That will tell ArcInfo that it is not projected - WGS84 coords.
# copy the file:  WGS1984.prj, which contains the following:
# GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]
file.copy("WGS1984.prj", paste(buoy, ".prj", sep=""), overwrite = TRUE)

#Converting from standardized csv to kml. Output will be in output directory 
csv2kml(beacon, substring(fname1,1,5))

#Converting from standardized csv to gpx. Output will be in output directory 
csv2gpx(beacon, substring(fname1,1,5))

#Converting csv to polar plot
prepolarplot(beacon) #polarplot called within prepolarplot function