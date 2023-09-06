#' Crop Rasters
#'
#' CropRasters is an R function to crop a set of cloud-optimized geotiffs using a polygon. After cropping, the rasters are saved to a specificed output 
#' directory where the directory structure reflects the orginal COG URLs. 
#'
#' @param cog_urls A vector of raster URLs. These URLS should point directly to the geotiff files that you intend to crop from the Merkle Database
#' @param polygon_sf An sf polygon object
#' @param output_folder Output directory path of the cropped rasters
#'
#' @return
#' @import httr
#' @import jsonlite
#' @import raster
#' @import terra
#' @export
#'
#' @examples




CropRasters <- function(cog_urls, polygon_sf, output_folder) {
  require(raster)
  require(sf)
  
  # Initialize an empty list to store cropped rasters
  
  cropped_rasters <- list()
  
  # Iterate over the list of COG URLs
  for (cog_url in cog_urls) {
    # Prepend '/vsicurl/' to the COG URL
    cog_path <- paste0("/vsicurl/", cog_url)
    
    # Load the COG as a raster
    cog_raster <- raster(cog_path)
    
    # Ensure the polygon_sf is of class 'sf'
    if (!inherits(polygon_sf, "sf")) {
      stop("The provided spatial object is not an sf object.")
    }
    
    projected_polygon_sf <- st_transform(polygon_sf, crs = projection(cog_raster))
    
      
    # Crop the raster using the projected sf polygon
    cropped_raster <- crop(cog_raster, as(projected_polygon_sf, "Spatial"))
    plot(cropped_raster)
    
    # Extract the directory name from the COG URL for separate folder
    folder_name <- basename(dirname(cog_url))
    
    # Check if directory exists, if not, create it
    dir_path <- file.path(output_folder, folder_name)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }
    
    # Extract the file name from the COG URL
    file_name <- basename(cog_url)
    
    # Construct the output file path
    output_path <- file.path(dir_path, file_name)
    
    # Write the cropped raster to the specified output path
    writeRaster(cropped_raster, output_path)
    
    # Add the cropped raster to the list
    cropped_rasters <- append(cropped_rasters, cropped_raster)
  }
  
  # Return the list of cropped rasters
  return(cropped_rasters)
}

