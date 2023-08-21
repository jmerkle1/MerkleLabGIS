#' Download SNODAS Daily Datasets From DEVISE Repository
#'
#' Downloads daily subsets of masked raster data provided by the Snow Data Assimilation System (SNODAS), see: \url{https://nsidc.org/data/g02158}.
#'
#' @param  params A character vector containing a set of possible SNODAS datasets for download. SWE = Snow Water Equivalent and SnowDepth Default is params = c("SWE", "SnowDepth")
#' @param startDate A character with the starting date of interest. ISO date format is required (e.g. YYYY-mm-dd)
#' @param endDate A character with the ending date of interest. ISO date format is required (e.g. YYYY-mm-dd). If NULL, only the single start date file will be downloaded.
#' @param outDir A character specifying the root output directory for downloaded files. This should be the root directory, sub-directories will be created if missing. Assumes your current working directory by default
#'
#' @return Returns the downloaded DEVISE study area SNODAS GeoTif files. Output is projected into epsg:5072 and cropped to DEVISE Study Extent
#'
#' @examples
#' # Download all SNODAS datasets for a range of dates.
#' download_SNODAS(params = c( "SWE", "SnowDepth"), startDate = "2020-09-01", endDate = "2020-09-05", outDir = "./Test")
#'
#' @export



DownloadSNODAS<- function(params = c("SWE", "SnowDepth"), startDate, endDate, outDir){

  require(foreach)

  if(!exists("startDate")){
    stop("startDate must be a charater string of a date (e.g. '2020-06-30')")
  } else{
    if(any(is.null(startDate), is.na(startDate))){
      stop("startDate must be a charater string of a date (e.g. '2020-06-30')")
    }
  }
  if(!is.character(startDate)) stop("startDate must be a charater string of a date (e.g. '2020-06-30')")
  if(!lubridate::is.Date(as.Date(startDate))) stop("startDate must be a charater string of a date (e.g. '2020-06-30')")
  
  # End date
  if(!is.null(endDate)){
    if(!is.character(endDate)) stop("endDate must be a charater string of a date (e.g. '2020-06-30')")
    if(inherits(try(lubridate::is.Date(as.Date(endDate)), silent = TRUE), "try-error")) stop("endDate must be a charater string of a date (e.g. '2020-06-30')")
  }

  # output directory
  if(!is.null(outDir)){
    if(!is.character(outDir)) stop("outDir must be a character string. Preferably of a directory that exists within your file system.")
    if(!dir.exists(outDir)) stop("You have specified a directory that does not currently exist within your file system!")
  }

  # params
  if(!all(params %in% c("SolidPrecipitation", "SWE", "SnowMeltRunoff", "SnowDepth", "LiquidPrecipitation"))) {
    stop("params must contain only the following possible input as a character vector: c(\"SnowPackSublimation\", \"SolidPrecipitation\", \"SnowPackTemperature\", \"BlowingSnowSublimation\", \"SWE\", \"SnowMeltRunoff\", \"SnowDepth\", \"LiquidPrecipitation\")")
  }


  dat<- httr::POST(
    "https://devise.uwyo.edu/umbraco/api/snodasapi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox(startDate),
           EndDate = jsonlite::unbox(endDate),
           Metrics = params),
      auto_unbox = FALSE
    )
  )

  dat<- httr::content(dat)

  # Extract the response
  dat <- do.call(rbind.data.frame, dat)
  
  # Download the files from Repository
  for(i in 1:nrow(dat)){
    # Create subdirectory for each metric if it doesn't exist
    subDir <- file.path(outDir, dat$metric[i])
    if (!dir.exists(subDir)) {
      dir.create(subDir)
    }
    
    # Set the destination file path within the metric's subdirectory
    destFilePath <- file.path(subDir, dat$filename[i])
    
    try(utils::download.file(url = dat$url[i], destfile = destFilePath, quiet = TRUE, mode = "wb"), silent = TRUE)
  }
}
