#########################################################################################
######################Processing of raw beacon data###################################### 
# Programmer: Anna Crawford - PhD Candidate DGES - Carleton University 
# Created: 10 February 2014
# Last Modified: 5 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Takes in raw data after download from appropriate source 
# Creates a  standardized csv file with individual functions for each beacon type/source
#
# Calls on individual functions for various other output file types 
#
# Necessary file: beacon data downloaded/converted to csv format. This should be kept in an 
# 'input' directory with other raw data
#
# Command line directions: XXXXXX

# Distaz, polarplot functions written by Derek Mueller,  DGES - Carleton University 
##########################################################################################

### Remove previous workspace
rm(list=ls(all=TRUE))

### Load Necessary librariers
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

### Directories
#name input directory, where raw data files are kept
input = "/tank/HOME/acrawford/6006/RawBeaconData"  
#name output directory, where the output/standardized csv file will be written
output = "/tank/HOME/acrawford/6006/ProcessedBeaconData"    
# (csv, shp, polarplot, etc...) will be put

### Read in raw data
# fname1 is the file name of the raw data that you will process
setwd(input)
fname1 = "463170_2012"    
Drift = read.csv(paste(fname1, '.csv', sep=""), header = T, sep = ",", dec = ".", 
                 na.strings = "NULL", strip.white = FALSE)

###Source functions
# Set program directory, where scripts are kept
prgdir = "/tank/HOME/acrawford/6006/Scripts" 
setwd(prgdir)

# Functions to convert raw data to standardized csv
source("Distaz.R")
source("Sailwx2csv.R")
source("Joubeh2csv.R")
source("Iridium2csv.R")
source("Oceanetics2csv.R")
source("Canatec2csv.R")

# Functions to convert standardized csv to quality added file types
source("csv2shp.R")
source("csv2kml.R")
source("csv2gpx.R")

# Functions to convert standardized csv to R plots
source("PrePolarPlot.R")
source("PolarPlot.R")
source("SpeedPlot.R")
source("CummSpeed.R")

# Function to output a summary stats file
source("IceIslandStats.R")

# Function to overwrite all files - backup first if you want to save the files!
source("deleteFile.R")

### Processing raw data
# Run function relevant to the beacon's source/type
# Converts raw data to standardized csv format 
Sailwx2csv(Drift) 
Joubeh2csv(Drift) 
Iridium2csv(Drift)
Oceanetics2csv(Drift) 
Canatec2csv(Drift) 

#########################################################################################
### Convert standardized csv files to quality added files 

# Need to read in new csv data located in output directory. All following files will be 
# written to this output directory. 
setwd(output)
Beacon <- read.table(paste(fname1,'.csv', sep = ""), header = T, sep = ",", dec = ".", 
                   na.strings = "NULL", strip.white = FALSE)

# Converting from standardized csv to point shapefile. 
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