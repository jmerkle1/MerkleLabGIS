#' Download DAYMET Daily Datasets From DEVISE Repository
#'
#' Downloads daily subsets of masked Snow Pack Temperature (Celcius) provided by the Snow Data Assimilation System (PRISM), see: \url{ftp://prism.nacse.org/daily/}.
#'
#' @param  params A character vector containing a set of possible DAYMET datasets for download. prcp = Daily Precipitation in mm; swe = Snow Water Equivalent; tmax = Daily Maximum Temperature in Celsius; tmin = Daily Minimum Temperature in Celsius. Default is params = c("prcp", "swe", "tmax", "tmin")
#' @param startDate A character with the starting date of interest. ISO date format is required (e.g. YYYY-mm-dd)
#' @param endDate A character with the ending date of interest. ISO date format is required (e.g. YYYY-mm-dd). If NULL, only the single start date file will be downloaded.
#' @param outDir A character specifying the root output directory for downloaded files. This should be the root directory, sub-directories will be created if missing. Assumes your current working directory by default
#'
#' @return Returns the downloaded DEVISE study area DAYMET GeoTif files. Output is projected into epsg:5072 and cropped to DEVISE Study Extent
#' @import terra
#' @import lubridate
#' @export



DownloadDAYMET<- function(params = c( "prcp"), startDate, endDate, outDir) {

  # validate params
  allowed_metrics <- c("prcp", "swe", "tmax", "tmin")
  
  params <- tolower(params)
  if (!all(params %in% allowed_metrics)) {
    stop(paste0("params must be one or more of: ", paste(allowed_metrics, collapse = ", ")))
  }
  
  # Validate date inputs
  if (!is.character(startDate) || is.na(startDate)) stop("startDate must be a character string like '2022-01-10'")
  if (!is.null(endDate) && (!is.character(endDate) || is.na(endDate))) stop("endDate must be a character string like '2022-01-10'")
  if (!is.null(outDir) && (!is.character(outDir) || !dir.exists(outDir))) stop("outDir must be a valid existing directory")
  
  # Create date range
  start <- as.Date(startDate)
  end <- as.Date(endDate)
  all_dates <- seq(start, end, by = "1 day")
  all_years <- unique(year(all_dates))
  
  for (param in params) {
    for (yr in all_years) {
      # Filter dates for this year only
      year_dates <- all_dates[year(all_dates) == yr]
      band_indices <- yday(year_dates)
      
      # Construct COG URL and /vsicurl/ path
      url <- sprintf("https://pathfinder.arcc.uwyo.edu/devise/cloudenabled/daily/cog/daymet/%s/daymet_daily_%s_%d.tif",
                     param, param, yr)
      vsicurl_path <- paste0("/vsicurl/", url)
      
      message(sprintf("Accessing remote raster stack for %s %d ...", param, yr))
      
      tryCatch({
        rast_all <- rast(vsicurl_path)
        subset_rast <- rast_all[[band_indices]]
        
        # Check: if subset is empty (e.g., failed band read), skip saving
        if (nlyr(subset_rast) == 0) {
          warning(sprintf("No valid bands found for %s %d. Skipping save.", param, yr))
          next
        }
        
        # Save output
        subDir <- file.path(outDir, param)
        if (!dir.exists(subDir)) dir.create(subDir, recursive = TRUE)
        
        output_name <- sprintf("daymet_%s_%s_to_%s.tif", param, min(year_dates), max(year_dates))
        output_path <- file.path(subDir, output_name)
        
        writeRaster(subset_rast, output_path, overwrite = TRUE)
        message(sprintf("Saved to %s", output_path))
      }, error = function(e) {
        message(sprintf("Failed to process %s for %d: %s", param, yr, e$message))
      })
    }
  }
}
