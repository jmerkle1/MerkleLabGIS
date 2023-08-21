#' RAP Download Function
#' 
#' The DownloadRAP function allows users to download specific metrics from the Rangeland Analysis Platform (RAP) for a given year.
#' 
#' @param RAPmetric character vector of RAP metrics to use (e.g. Biomass_AnnualForbsGrasses, Cover_Bareground)
#' @param years numeric vector of years for which you want to download the data
#' @param outDir character string representing the path to where you want to download the rasters
#' 
#' @import terra
#' @import MerkleLabGIS
#' @export
#' 
#' 
DownloadRAP <- function(RAPmetric = c("Biomass_AnnualForbsGrasses", "Cover_BareGround"),
                        years = 2022,
                        outDir) {
  # Check RAP Metric
  if(any(RAPmetric %in% c("Biomass_AnnualForbsGrasses","Biomass_PerennialForbsGrasses", 
                          "Cover_AnnualForbsGrasses","Cover_BareGround","Cover_Litter","Cover_PerennialForbsGrasses",
                          "Cover_Shrubs","Cover_Trees")==FALSE)){
    stop("The RAPmetric must only be Biomass_AnnualForbsGrasses,Biomass_PerennialForbsGrasses, 
                    Cover_AnnualForbsGrasses,Cover_BareGround,Cover_Litter,Cover_PerennialForbsGrasses,
                       Cover_Shrubs,Cover_Trees.")
  }
  
  # Check output directory
  if(!is.null(outDir)){
    if(!is.character(outDir)) stop("outDir must be a character string. Preferably of a directory that exists within your file system.")
    if(!dir.exists(outDir)) stop("You have specified a directory that does not currently exist within your file system!")
  }
  
  # Fetch RAP data from pathfinder
  dt <- bucket()
  RAP <- dt[dt$Category == "Landcover_RAP",]
  
  if(all(c("terra") %in% installed.packages()[,1])==FALSE)
    stop("You must install the terra package")
  require("terra")
  
  for(year in years) {
    for(metric in RAPmetric) {

      filename <- paste0("RAP_", year, "_", metric, ".tif")
      
      file_url <- RAP$url[RAP$filename == filename]
      
      if(length(file_url) == 0) {
        message(paste0("RAP file for year ", year, " and metric ", metric, " not found in the list."))
        next
      }
      
      r <- terra::rast(file_url)
      
      # Write the raster to the destination folder
      dest_path <- file.path(outDir, filename)
      terra::writeRaster(r, dest_path, overwrite=TRUE)
      message(paste0("Downloaded ", filename, " to ", dest_path))
    }
  }
}
