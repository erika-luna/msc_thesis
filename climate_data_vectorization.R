##################################################
##################################################
########## CLIMATE DATA DAYMET DOWNLOAD ##########
##################################################
##################################################

# 2020.05.26 Creating script to download and rasterize Daymet data

###############
### SUMMARY ###
###############

# The Alberta Innovates project seeks to understand how climate change is influencing crop yields in Canada
# And which best management practices can help mitigate these climate impacts

# In order to build the statistical models, we need climate data at the ecodistrict and CCS/SAR level
# We have Daymet data which is gridded
# We need to vectorize the Daymet climate data


##############
### SET UP ###
##############

# INSTALL PACKAGES
#install.packages("daymetr")
#install.packages("here")

# LOAD PACKAGES
library(daymetr)
library(here)

# DEFINE VARIABLES
years <- 1980:2018

#################
### LOAD DATA ###
#################

# this downloads daily tmin for prairie provinces as netcdf file
# straight into GIS/ClimateRasters/ folder

#prairiescoords <- c(60,-120, 49,-89) #Canada
prairiescoords <- c(32,-117, 15,-87) #Mexico


download_daymet_ncss(location = prairiescoords, 
                     start = years[1],
                     end = years[length(years)],
                     param = "tmin")
                     #path = here("GIS/ClimateRasters"))

download_daymet_ncss(location = prairiescoords,
                     start = years[1],
                     end = years[length(years)],
                     param = "tmax",
                     path = here("GIS/ClimateRasters"))


download_daymet_ncss(location = prairiescoords,
                     start = years[1],
                     end = years[length(years)],
                     param = "prcp",
                     path = here("GIS/ClimateRasters"))

nc2tif(here("GIS/ClimateRasters")) # convert all loaded netcdf files to tif

## print converted files
print(list.files(tempdir(), "*.tif"))




