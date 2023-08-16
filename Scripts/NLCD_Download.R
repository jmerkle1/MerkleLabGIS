

#########################################################################
##                      Script Name: NLCD download                     ##
##  Purpose: Download NLCD data and extract tree canopy and landcover  ##
##                    Jerod Merkle & Jessie Shapiro                    ##
##                          Updated: July 2023                         ##
#########################################################################



## ---------------------------

## Step 1
## Download any new data directly from the NLCD site and unzip folder content

rm(list=ls())


#Read libraries
library(terra)
library(sf)
library(mapview)
library(stringr)
library(gdalUtilities)


## Set working directory to where you downloaded the recent data
setwd("C:/Users/cowboy/Downloads/NLCD_landcover_2019_release_all_files_20210604")

fls <- list.files(getwd(),".img$", recursive = TRUE)
fls

## There might be a new Tree Canopy Cover data, in which case, just download the newest data here
## https://www.mrlc.gov/data?f%5B0%5D=category%3ATree%20Canopy

Tree2021<- rast("C:/Users/cowboy/Downloads/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_CONUS_2021_v2021-4.tif")

dim(Tree2021)

#Plot it
plot(Tree2021)



#Bring in bounding box

# Create temp files
temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp' 
URL <- "https://pathfinder.arcc.uwyo.edu/devise/MerkleLabGIS/Boundaries/Wyoming_Bounding_Box.zip"
download.file(URL, temp)

# Unzip the contents of the temp and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)

# Read the shapefile
wy<- sf::read_sf(temp2)

#Fix or check projection
st_crs(wy)$proj4string
wy <- st_transform(wy, crs=5072)
st_crs(wy)$proj4string
st_bbox(wy)

#Add a buffer
wy <- st_buffer(wy, dist=200000)  # make a large buffer

#View on map
mapview(wy)




## ---------------------------

## Step 2
## Crop data to bounding box

# need to make a names vector for writing out later
nms <- str_split_fixed(fls, "/", 2)[,2]


for(i in 1:length(fls)){
  r <- rast(fls[i])
  print(dim(r))
  print(datatype(r))
  # plot(r)
  
  r <- crop(r, st_transform(wy, crs=st_crs(r)))
  
  writeRaster(r, filename = paste0("Z:/Landcover_NLCD/",
                                   sub(".img", ".tif", nms[i])),
              filetype="GTiff", 
              datatype="INT1U")
}


## Crop for just the tree cover data

r <- crop(Tree2021, st_transform(wy, crs=st_crs(Tree2021)))
datatype(r)
plot(r)
hist(values(r))

writeRaster(r, filename = "C:/Users/cowboy/Downloads/nlcd_tcc_CONUS_2020_v2021-4/nlcd_2021_treecanopy.tif",
            filetype="GTiff", 
            datatype="INT1U")

## ---------------------------

## Step 4
## Convert the final tif files into a Cloud Optimized Geotiff for storage in Pathfinder


# Specify the paths to the input TIF file and the output COG file

input_tif <- "C:/Users/cowboy/Downloads/nlcd_tcc_CONUS_2020_v2021-4/nlcd_2021_treecanopy.tif"
output_cog <- "C:/Users/cowboy/Documents/gis-viewer/data/NLCD_2021_TreeCanopy.tif"

gdal_translate(input_tif, output_cog, of = "COG")


#Another way to write it out
gdalUtilities::gdal_translate(src_dataset = input_tif, 
                              dst_dataset = output_cog,
                              co = matrix(c("TILED=YES", 
                                            "COPY_SRC_OVERVIEWS=YES", 
                                            "COMPRESS=DEFLATE"), 
                                          ncol = 1))



