#' Title
#'
#' @return
#' @import httr
#' @import jsonlite
#' @import raster
#' @import terra
#' @export
#'
#' @examples
#' bucket()

crop_rasters <- function(cog_urls, shapefile_path, output_folder) {
  require(raster)

  # Load the shapefile as a spatial object
  shapefile <- shapefile(shapefile_path)

  # Initialize an empty list to store cropped rasters
  cropped_rasters <- list()

  # Iterate over the list of COG URLs
  for (cog_url in cog_urls) {
    # Prepend '/vsicurl/' to the COG URL
    cog_path <- paste0("/vsicurl/", cog_url)
    print(cog_path)

    # Load the COG as a raster
    cog_raster <- raster(cog_path)
    print(cog_raster)

    projected_shapefile <- spTransform(shapefile, projection(cog_raster))
    projection(projected_shapefile)

    # Crop the projected COG to the projected shapefile extent
    cropped_raster <- crop(cog_raster, projected_shapefile)
    plot(cropped_raster)
    # Extract the file name from the COG URL
    file_name <- basename(cog_url)

    # Construct the output file path
    output_path <- file.path(output_folder, file_name)
    print(output_path)
    # Write the cropped raster to the specified output path
    writeRaster(cropped_raster, output_path)

    # Add the cropped raster to the list
    cropped_rasters <- append(cropped_rasters, cropped_raster)

  }

  # Return the list of cropped rasters
  return(cropped_rasters)
}
