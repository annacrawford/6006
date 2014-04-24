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
# Command line directions: input directory, output directory, program directory, filename, beacon type
# Write in command line without commas

# Distaz function is available with the GEOmap library
# PolarPlot functions found through  the heR.Misc library 
##########################################################################################

### Remove previous workspace
rm(list=ls(all=TRUE))

### Load Necessary librariers
library(sp)
library(rgdal)
library(chron)
library(RSEIS)
library(RPMG)
library(shapefiles)
library(grDevices)
library(assertthat)
library(rPython)
library(date)

### Define system arguments
args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
prgdir <- args[3]
fname1 <- args[4]
beaconType <- args[5]

###Source functions
#prgdir ='/tank/HOME/acrawford/6007/Scripts'
setwd(prgdir)

# Functions to convert raw data to standardized csv
source("raw2csv.R")
source("Distaz.R")
source("Sailwx2csv.R")
source("Joubeh2csv.R")
source("Iridium2csv.R")
source("Oceanetics2csv.R")
source("Canatec2csv.R")

source("Validation.R")    # Assertion function to check standardization procedure

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
source("DeleteFile.r")

# Capture messages and errors to a log file in output directory
setwd(output)
BLP <- file("BeaconProcessingLog.txt", open="at")

# Output start time, beaconType and filename (fname1) to above log
sink(BLP, type="output", append=TRUE, split = FALSE) 
print(Sys.time())
print(beaconType)
print(fname1)
sink(BLP, type="message", append=TRUE, split = FALSE) 

### Unit test # 1
# Checking correct number of arguments stated at command line 
if(length(args) != 5) 
{
  stop("check number of input arguments  ")  
}    # end check

# Checking that beaconType argument is one of possible options
valid = c("Joubeh", "Iridium", "Sailwx", "Oceanetics", "Canatec")
if(!(beaconType %in% valid))
{
  stop("beaconType is not a possible option  ")
}    # end check

### Read in raw data
setwd(input)
Drift <- read.csv(paste(fname1, '.csv', sep=""), header = T, sep = ",", dec = ".", 
       na.strings = "NULL", strip.white = FALSE)

### Unit test 2
# checking that beacon data is read in 
if (class(Drift) != "data.frame")
{
  stop("error reading in beacon data  ") 
}    # end check

### Processing raw data
# Run function relevant to the beacon's source/type
# Converts raw data to standardized csv format 
raw2csv(beaconType, Drift) 

#########################################################################################
### Convert standardized csv files to quality added files 

# Need to read in new csv data located in output directory. All following files will be 
# written to this output directory. 
setwd(output) # Maybe this isn't needed because x2csv scripts already are in this directory 
Beacon <- read.table(paste(fname1,'.csv', sep = ""), header = TRUE , sep = ",", dec = ".", 
                   na.strings = "NULL", strip.white = FALSE)

### Unit test 3
# Assertion to check if standardized data is in correct standardized format
# Returns 'True' or 'False'
Validation(Beacon)

# Test to check if time is in correct format
# Will output an error message to log, but will continue to run program
date <- try(as.Date(Beacon[2:length(Beacon$gps_time),14], format = "%Y-%m-%d %H:%M:%S"))
  if( class( date ) == "try-error" || is.na ( date ) ) print ( " gps_time is not in correct format")

# Assertion to check that data is ordered chronologically
# Outputs a warning to log file - program continues to run
# Get the time difference between positions & make sure all are positive
chronDate <- as.numeric(difftime(Beacon$gps_time[2:length(Beacon$gps_time)],
  Beacon$gps_time[1:length(Beacon$gps_time)-1], units = "hours")) 

if (any(chronDate<0)=='TRUE')
{
  print("standardized csv is not ordered chronologically by gps_time ") 
}    # end check

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
# exists in output directory already.
# This error will cause the program to halt and an error will be reported in the file log
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

# Unit test 4
# Check that Rplots.pdf (contains polarplot, speed plot and cummSpeed plot) were created and saved
if (!file_test("-f", paste("Rplots", ".pdf",sep=''))) 
{
  stop("plot pdf not written   ")
}    # end check

#Disconnect sink
unlink("BeaconProcessingLog.txt")