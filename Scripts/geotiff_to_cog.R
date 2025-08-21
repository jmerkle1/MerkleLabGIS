#########################################################################
##                      Script Name: Convert to COG                    ##
##        Purpose: Convert standard GeoTIFF to COG format              ##
##                          Updated: August 19 2025                    ##
#########################################################################


# Load standard packages
require(gdalUtilities)

# Set working directory
setwd("C:/Users/cowboy/data")

# Define input and output paths of the standard GeoTIFF and the location of the COG format
input_tif <- "CDL_2023.tif"
output_cog <- "CDL_2023_cog.tif"


# Convert to COG with LZW compression
#
#   Data type should match your source data:
#   - Options include:"INT1U" (1-byte unsigned int, values 0–255), "Int16", "UInt16", "Float32", etc.
#
#   Compression:
#   - "LZW" is lossless and widely compatible or "DEFLATE" (slightly smaller files, slower)
#
#   Internal tiling block size in pixels:
#   - 128 is reasonable for small/moderate rasters
#   - Larger blocks (e.g., 256 or 512) can improve read performance on large rasters
#

gdal_translate(
  src_dataset = input_tif,
  dst_dataset = output_cog,
  of = "COG",
  datatype = "INT1U", 
  co = c(
    "COMPRESS=LZW",
    "BLOCKSIZE=128",
    "BIGTIFF=YES"
  )
)

