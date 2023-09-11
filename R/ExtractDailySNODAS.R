#' Daily SNODAS Extraction Function
#' 
#' The Extract Daily SNODAS function processes data from SNODAS
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired SNODAS metrics either SWE or SnowDepth, and a date range. It extracts metric values for the provided points. 
#' 
#' @param XYdata sf point object
#' @param Metrics character vector of SNODAS metrics to use (SWE or SnowDepth)
#' @param datesname name of your column representing date as POSIXct
#' @param num_cores number of cores to use for parallel processing
#' @return Returns a vector with length of XYdata with the point/date values of SNODASMetrics
#' 
#' 
#' @return
#' @import sf
#' @import terra
#' @import jsonlite
#' @import tidyverse
#' @import parallel
#' @import httr
#' @import dplyr
#' @export
#' 
#' 
#' 
#' 


ExtractDailySNODAS <- function(XYdata = data,
                               datesname = "date",
                               Metrics = c("SWE", "SnowDepth"),
                               num_cores = 4) {
  
  if (!inherits(XYdata, "sf"))
    stop("XYdata is not an sf object")
  require("sf")
  require("parallel")
  require("terra")
  
  # Create the formatted_dates column
  unique_dates <- unique(XYdata[[datesname]])
  formatted_dates <- paste0(format(unique_dates, "%Y-%m-%d"))
  
  XYdata$formatted_dates <- format(XYdata[[datesname]], "%Y-%m-%d")
  
  # Determine the start and end dates from the formatted_dates column
  start_date <- min(formatted_dates)
  end_date <- max(formatted_dates)
  
  dat <- httr::POST("https://devise.uwyo.edu/Umbraco/api/SnodasApi/GetData",
                    httr::content_type_json(),
                    body = jsonlite::toJSON(list(StartDate = jsonlite::unbox(start_date),
                                                 EndDate = jsonlite::unbox(end_date),
                                                 Metrics = Metrics),
                                            auto_unbox = FALSE)
  ) %>% httr::content()
  
  df <- data.frame(t(sapply(dat, c)))
  df$sampDate <- as.POSIXct(unlist(df$sampDate))
  dates <- as.POSIXct(df$sampDate)
  
  original_crs <- st_crs(XYdata)
  
  dates <- XYdata[[datesname]]
  
  if (!inherits(dates, "POSIXct"))
    stop("Your datesname column is not POSIXct")
  if (any(is.na(dates)))
    stop("You have NAs in your datesname column")
  
  XYdata <- st_transform(XYdata, crs = 5072)
  
  # Setup cluster
  clust <- makeCluster(num_cores)
  
  # export the objects you need for your calculations from your environment to each node's environment
  clusterExport(clust, varlist = c("XYdata", "Metrics", "df"),envir = environment() )
  
  dat_snow <- do.call(rbind, clusterApplyLB(clust, 1:nrow(XYdata), function(i){
    library(sf)
    library(terra)
    row <- XYdata[i, ]
    results <- data.frame()
    
    for (Metric in Metrics) {
      date_str <- row$formatted_dates
      url_for_date <- df$url[df$sampDate == date_str & df$metric == Metric]  # Filter by Metric
      if (length(url_for_date) == 0) {
        extracted_val <- NA  # No raster available for this date and metric
      } else {
        r <- try(terra::rast(as.character(url_for_date)), silent = TRUE)
        if (inherits(r, "SpatRaster")) {
          extracted_vals <- terra::extract(r, row)  # Extract all values from the raster
          if (length(extracted_vals) >= 2) {
            extracted_val <- extracted_vals[2]  # Extract the second value (index 2)
          } else {
            extracted_val <- NA
          }
        } else {
          extracted_val <- NA
        }
      }
      # Create a column for the metric and store the extracted value
      results[1, Metric] <- extracted_val
    }
    
    return(results)
  }))
  
  stopCluster(clust)   # Stop the parallelization framework
  
  # Bind the extracted data to the original XYdata based on the date
  XYdata <- cbind(XYdata, dat_snow)
  
  # Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  # Remove formatted date column
  XYdata <- XYdata %>%
    select(-formatted_dates)
  
  return(XYdata)
}


