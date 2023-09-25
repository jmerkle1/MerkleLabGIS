#########################################################################
##                      Script Name: RAP download                      ##
##  Purpose: Download RAP data and extract cover and biomass layer     ##
##                    Jerod Merkle & Jessie Shapiro                    ##
##                          Updated: September 2023                    ##
#########################################################################


rm(list = ls())


#Read libraries
library(sf)
library(tidyverse)
library(httr)
library(lubridate)
library(jsonlite)
library(terra)
library(raster)
library(gdalUtilities)



#Vector of the years for which to download the data
ys <- c(2022)

#Get bounding box coordinates
xmin <- -112.33799283518043
xmax <- -103.1677412167879
ymin <- 40.17161259361823
ymax <- 46.03112042220768

setwd("G:/Shared drives/wy-coop-shapiro/gis-organization/data/test/")

## ---------------------------

## Step 1
## Download any new data directly from the RAP site for vegetation cover

rasters_list <- list()

for (i in 1:length(ys)) {
  cat(paste0("Downloading ", ys[i], " data ... \n"))
  
  # Construct the URL
  url <- paste0("/vsicurl/http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v3/vegetation-cover-v3-", ys[i], ".tif")
  
  r <- rast(url)
  
  bbox_extent <- extent(xmin, xmax, ymin, ymax)
  
  bbox_poly <- as(bbox_extent, "SpatialPolygons")
  
  r <- crop(r, bbox_poly)
  
  # Add the raster to the list
  rasters_list[[i]] <- r
}



## ---------------------------

## Step 2
## Extract bands from the rap-vegetation-cover


# Create a list to hold the labeled rasters
labeled_rasters_list <- list()

# Stack the rasters 
for(i in 1:length(rasters_list)) {
  s <- rasters_list[[i]]
  # Stack raster from the list
  s <- stack(rasters_list[[i]])
  
  # Create an empty list to hold the layers of the current stack
  current_layers <- list()
  
  # Add each layer of the stack to the list with a label
  current_layers[["AnnualForbsGrasses"]] <- s[[1]]
  current_layers[["BareGround"]] <- s[[2]]
  current_layers[["Litter"]] <- s[[3]]
  current_layers[["PerennialForbsGrasses"]] <- s[[4]]
  current_layers[["Shrubs"]] <- s[[5]]
  current_layers[["Trees"]] <- s[[6]]
  
  # Add the labeled layers to the main list, with the year as a name
  labeled_rasters_list[[paste0(ys[i])]] <- current_layers
}


## ---------------------------

## Step 3
## Convert to vegetation cover layers to COGS

for (year in names(labeled_rasters_list)) {
  
  # Access the labeled rasters for the current year
  rasters_for_year <- labeled_rasters_list[[year]]
  
  for (label in names(rasters_for_year)) {
    current_raster <- rasters_for_year[[label]]

    # Save raster to a temporary file
    temp_tif <- tempfile(fileext = ".tif")
    writeRaster(current_raster, filename = temp_tif, format = "GTiff", overwrite=TRUE)
    
    output_cog <- paste0("G:/Shared drives/wy-coop-shapiro/gis-organization/data/COG/test/", "RAP", "_",year,"_","cover","_", label, ".tif")
    
    # Convert to COG using gdal_translate
    gdal_translate(temp_tif, output_cog, of = "COG")
    
    # Delete the temporary file after conversion
    file.remove(temp_tif)
  }
}


## ---------------------------

## Step 4
## Download any new data directly from the RAP site for biomass layers

rasters_list <- list()

for (i in 1:length(ys)) {
  cat(paste0("Downloading ", ys[i], " data ... \n"))
  
  # Construct the URL
  url <- paste0("/vsicurl/http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-biomass/v3/vegetation-biomass-v3-", ys[i], ".tif")
  
  r <- rast(url)
  
  bbox_extent <- extent(xmin, xmax, ymin, ymax)
  
  bbox_poly <- as(bbox_extent, "SpatialPolygons")
  
  r <- crop(r, bbox_poly)
  
  # Add the raster to the list
  rasters_list[[i]] <- r
}


## ---------------------------

## Step 5
## Extract bands from the rap-vegetation-cover

# Create a list to hold the labeled rasters
labeled_rasters_list <- list()

# Stack the rasters 
for(i in 1:length(rasters_list)) {
  s <- rasters_list[[i]]
  # Stack raster from the list
  s <- stack(rasters_list[[i]])
  
  # Create an empty list to hold the layers of the current stack
  current_layers <- list()
  
  # Add each layer of the stack to the list with a label
  current_layers[["Biomass_AnnualForbsGrasses"]] <- s[[1]]
  current_layers[["Biomass_PerennialForbsGrasses"]] <- s[[2]]
  
  # Add the labeled layers to the main list, with the year as a name
  labeled_rasters_list[[paste0(ys[i])]] <- current_layers
}

## ---------------------------

## Step 6
## Convert biomass layers to COGS

for (year in names(labeled_rasters_list)) {
  
  # Access the labeled rasters for the current year
  rasters_for_year <- labeled_rasters_list[[year]]
  
  for (label in names(rasters_for_year)) {
    current_raster <- rasters_for_year[[label]]
    
    # Save raster to a temporary file
    temp_tif <- tempfile(fileext = ".tif")
    writeRaster(current_raster, filename = temp_tif, format = "GTiff", overwrite=TRUE)
    
    # Define the output path
    output_cog <- paste0("G:/Shared drives/wy-coop-shapiro/gis-organization/data/COG/test/", "RAP","_",year,"_", label, ".tif")
    
    # Convert to COG using gdal_translate
    gdal_translate(temp_tif, output_cog, of = "COG")
    
    #Delete the temporary file after conversion
    file.remove(temp_tif)
  }
}



