#' SNODAS Extraction Function
#' 
#' The Extract SNODAS function processes data from SNODAS
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired SNODAS metrics (e.g., MaxSnowDepth, MaxSWE, MeanSnowDepth), and a date range. It extracts metric values for the provided points. 
#' 
#' @param point_data sf point object
#' @param metric_name character vector of SNODAS metrics to use (e.g. MaxSnowDepth, MeanSnowDepth)
#' @param start_date
#' @param end_date
#' @return Returns a vector with length of XYdata with the point/date values of SNODASMetrics
#' 
#' 
#' @return
#' @import sf
#' @import terra
#' @import jsonlite
#' @import tidyverse
#' @import httr
#' @export
#' 
#' 
#' 
#' 


ExtractSNODAS <- function(point_data, 
                                start_date = "2005-01-01", 
                                end_date = "2010-01-01", 
                                metric_name = NULL) {
  
  # Load necessary libraries
  require(terra)
  require(sf)
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  if(!inherits(point_data, "sf")) {
    stop("point_data must be an sf object")
  }
  # Obtain the list of all available metrics
  mySnodasMetrics <- httr::POST("https://devise.uwyo.edu/umbraco/api/snodasapi/GetDerivedAnnualMetricsList", content_type_json()) %>% content()
  mySnodasMetrics <- do.call(rbind.data.frame, mySnodasMetrics)
  
  # Select metric
  if(is.null(metric_name)) {
    metric <- mySnodasMetrics$metric[1]
  } else {
    metric <- metric_name
  }
  
  # Get the data
  dat <- httr::POST("https://devise.uwyo.edu/umbraco/api/Snodasapi/GetDerivedAnnualData", 
                    httr::content_type_json(),
                    body = jsonlite::toJSON(list(StartDate = jsonlite::unbox(start_date),
                                                 EndDate = jsonlite::unbox(end_date),
                                                 Metrics = metric),
                                            auto_unbox = FALSE)
  ) %>% content()
  dat <- do.call(rbind.data.frame, dat)
  
  # Read rasters directly from URL
  rasters <- lapply(dat$url, function(url) terra::rast(url))
  
  # Combine the rasters
  rs <- do.call(c, rasters)
  
  names(rs) <- str_split(dat$filename, "\\.", simplify = TRUE)[,1]
  
  pts <- vect(as(points %>% st_transform(crs = 5072), "Spatial"))
  
  # Extract data
  Snodas <- terra::extract(rs, pts, ID = FALSE)
  df <- as.data.frame(Snodas)
  names(df) <- names(rs)
  result <- cbind(point_data, df)
  
  return(result)
}
