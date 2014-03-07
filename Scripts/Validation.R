#########################################################################################
######################Validation of standardized csv format############################## 
# Programmer: Anna Crawford - Carleton University 
# Created: 6 March 2014
# Last Modified: 6 March 2014
# Project: Ice island drift, deterioration and detection 
# Descrition: Assertion function to make sure that the processed csv data is standardized
# correctly before feeding through quality added functions

# Necessary file: Processed csv beacon data which was written to 'output' directory with
# beacon_processing.R
#########################################################################################

Validation <-
  function(Beacon)
  {
    # Validating that all columns are filled with appropriate data type
    validate_that(Beacon$beacon[1] == fname1)
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),2]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),3]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),4]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),5]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),6]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),7]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),8]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),9]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),10]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),11]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),12]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),13]))
    validate_that(is.factor(Beacon[2:length(Beacon$gps_time),14]))
    validate_that(is.numeric(Beacon[2:length(Beacon$gps_time),15]))
  }     # End function