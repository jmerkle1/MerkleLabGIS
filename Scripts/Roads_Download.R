

#############################################################################################################
##                                    Script Name: TIGER Roads download                                    ##
##  Purpose:  Download national roads database, convert roads to raster and calculate euclidean distances  ##
##                                      Jerod Merkle & Jessie Shapiro                                      ##
##                                            Updated: July 2023                                           ##
#############################################################################################################





## ---------------------------

## Step 1
## First download the first downloaded the national roads database here:
## https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.2016.html

## Notes: The downloaded geodatabase is very large and recommended to open this in ArcGIS Pro.
## In ArcGIS Pro, crop to the bounding box (Butte, MT to Yuma, CO) and export as a shapefile called Roads_TIGER2016_WyomingPlus




## ---------------------------

## Step 2 
## Select Primary and Secondary roads. This step can also be done in ArcGIS Pro

## Notes: In ArcGIS, use the feature to raster (to rasterize) tool. Put the MTFCC as the column info to transfer
## then, run euclidian distance (on the rasterized road layer) in ArcGIS. 
## Each of the rasters were written into tif files using ArcGIS. 


#Read libraries
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(gdalUtilities)


#Read in Wyoming roads shapefile
rds <- st_read("C:/Users/cowboy/Documents/gis-viewer/data", "roads_wyoming")

head(rds)
st_crs(rds)$proj4string
st_bbox(rds)


# reproject into AEA projection
rds <- st_transform(rds, 5072)

table(rds$MTFCC)

# this is for just the primary and secondary roads
rds_sub <- rds %>% 
  filter(MTFCC %in% c("S1100","S1200"))

st_write(rds_sub, "C:/Users/cowboy/Documents/gis-viewer/data", "Roads_TIGER2022_PriSec_AEA",
         driver="ESRI Shapefile")

st_write(rds, "C:/Users/cowboy/Documents/gis-viewer/data", "Roads_TIGER2022_All_AEA",
         driver="ESRI Shapefile")



## ---------------------------

## Step 3

## Rewrite them in a better memory format (lops off the decimal, but that's ok)
## and renames them a bit more logically.

r <- rast("C:/Users/cowboy/Documents/gis-viewer/data/roads/TIGER22_Dist2_PriSec_30m.tif")
ncell(r)
crs(r)
extent(r)
datatype(r)

writeRaster(r, "C:/Users/cowboy/Documents/gis-viewer/data/roads/TIGER22_Dist2_PriSec_30m_Clean.tif",
            datatype="INT4U", overwrite=TRUE)


r <- rast("C:/Users/jmerkle/Desktop/roads/dist2rd_all1.tif")
ncell(r)
crs(r)
extent(r)
datatype(r)

writeRaster(r, "C:/Users/jmerkle/Desktop/roads/Dist2Rds_TIGER16_All_30m.tif",
            datatype="INT4U", overwrite=TRUE)



## ---------------------------

## Step 4
## Convert the final tif files into a Cloud Optimized Geotiff for storage in Pathfinder

# Specify the paths to the input TIF file and the output COG file
input_tif <- "C:/Users/cowboy/Documents/gis-viewer/data/roads/TIGER22_Dist2_PriSec_30m_Clean.tif"
output_cog <- "C:/Users/cowboy/Documents/gis-viewer/data/roads/COG.tif"

# Convert TIF to COG using gdal_translate
gdal_translate(input_tif, output_cog, of = "COG")

command <- c("gdal_translate", input_tif, output_cog, "-co", "TILED=YES", "-co", "COPY_SRC_OVERVIEWS=YES", "-co", "COMPRESS=DEFLATE")
system2(command)


# Lastly, remove the intermediate/working files 
