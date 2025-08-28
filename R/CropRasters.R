#' Crop Rasters
#'
#' CropRasters is an R function to crop a set of cloud-optimized geotiffs using a polygon. After cropping, the rasters are saved to a specificed output 
#' directory where the directory structure reflects the orginal COG URLs. 
#'
#' @param cog_urls A vector of raster URLs. These URLS should point directly to the geotiff files that you intend to crop from the Merkle Database
#' @param polygon_sf An sf polygon object
#' @param output_folder Output directory path of the cropped rasters
#' @param writeData If true, save the cropped rasters to the output folder. If FALSE (default), rasters are returned in memory only
#'
#' @return
#' @import httr
#' @import jsonlite
#' @import terra
#' @import sf
#' @export
#'
#' @examples


CropRasters <- function(cog_urls, polygon_sf, output_folder, writeData = FALSE) {
  
  # Check polygon validity
  if (!inherits(polygon_sf, "sf")) {
    stop("The provided polygon must be an sf object.")
  }
  
  cropped_rasters <- list()
  
  for (cog_url in cog_urls) {
    cog_path <- paste0("/vsicurl/", cog_url)
    
    message("--------------------------------------------------")
    message("Processing: ", basename(cog_url))
    
    
    # Read raster as SpatRaster
    cog_raster <- rast(cog_path)
    
    #If name is missing grab it from filename
    if (all(names(cog_raster) == "layer")) {
      base_name <- tools::file_path_sans_ext(basename(cog_url))

      if (nlyr(cog_raster) == 1) {
        names(cog_raster) <- base_name
      } else {
        names(cog_raster) <- paste0(base_name, "_band", seq_len(nlyr(cog_raster)))
      }
    }
    
    message("Reprojecting polygon to match raster CRS...")
    
    # Reproject polygon to match raster CRS
    polygon_projected <- st_transform(polygon_sf, crs(cog_raster))
    
    # Crop and mask
    message("Cropping raster...")
    
    cropped <- crop(cog_raster, vect(polygon_projected))
    masked <- mask(cropped, vect(polygon_projected))
    
    # Write output if requested
    if (writeData) {
      folder_name <- basename(dirname(cog_url))
      dir_path <- file.path(output_folder, folder_name)
      if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
      
      file_name <- basename(cog_url)
      output_path <- file.path(dir_path, file_name)
      
      message("Writing raster to: ", output_path)
      writeRaster(masked, output_path, overwrite = TRUE)
    } else {
      message("Raster kept in memory (not written to disk).")
    }
    
    cropped_rasters[[length(cropped_rasters) + 1]] <- masked
  }
  message("Finished processing ", length(cropped_rasters), " rasters.")
  message("--------------------------------------------------")
  
  return(cropped_rasters)
}


