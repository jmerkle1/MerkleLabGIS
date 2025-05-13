#' DAYMET Extraction Function
#' 
#' The Extract DAYMET function processes annual data from DAYMET
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired DAYMET metrics ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin"), and a date range. It extracts metric values for the provided points. 
#' 
#' @param point_data sf point object
#' @param metric_name character vector of DAYMET metrics to use ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin")
#' @param start_date
#' @param end_date
#' @return Returns a vector with length of point_data with the point/date values of DAYMET metrics
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


ExtractAnnualDAYMET <- function(XYdata, datesname = "date", Metrics = c("prcp", "swe"),
                                num_cores = NULL) {
  if (!inherits(XYdata, "sf"))
    stop("XYdata must be an sf object")
  
  allowed_metrics <- c("prcp", "swe", "tmax", "tmin")
  
  Metrics <- tolower(Metrics)
  if (!all(Metrics %in% allowed_metrics)) {
    stop(paste0("Metrics must be one or more of: ", paste(allowed_metrics, collapse = ", ")))
  }

  
  require(terra)
  require(sf)
  require(httr)
  require(jsonlite)
  require(tidyverse)
  require(lubridate)
  require(parallel)
  
  if (is.null(num_cores)) num_cores <- detectCores() - 1
  
  dates <- XYdata[[datesname]]
  if (!inherits(dates, "POSIXct")) stop("Your dates column is not POSIXct")
  if (any(is.na(dates))) stop("NA values found in date column")
  
  XYdata$year <- year(dates)
  
  original_crs <- st_crs(XYdata)
  XYdata <- st_transform(XYdata, 5072)
  
  clust <- makeCluster(num_cores)
  clusterEvalQ(clust, {
    library(terra)
    library(sf)
  })
  clusterExport(clust, varlist = c("XYdata", "Metrics"), envir = environment())
  
  result_list <- clusterApplyLB(clust, 1:nrow(XYdata), function(i) {
    row <- XYdata[i, ]
    values <- sapply(Metrics, function(metric) {
      yr <- row$year
      
      url <- sprintf("https://pathfinder.arcc.uwyo.edu/devise/cloudenabled/annual/cog/daymet/daymet_annual_%s_all-years.tif",
                     metric)
      vsicurl_path <- paste0("/vsicurl/", url)
      
      tryCatch({
        r <- terra::rast(vsicurl_path)
        band_names <- names(r)
        band_index <- which(grepl(paste0(metric, "_", yr), band_names))
        
        if (length(band_index) == 0) return(NA)
        
        v <- terra::extract(r[[band_index]], row)
        return(v[1, 2])
      }, error = function(e) {
        return(NA)
      })
    }, simplify = FALSE)
    
    return(as.data.frame(values))
  })
  
  stopCluster(clust)
  
  result_df <- do.call(rbind, result_list)
  colnames(result_df) <- Metrics
  
  XYdata <- cbind(XYdata, result_df)
  XYdata <- st_transform(XYdata, crs = original_crs)
  return(XYdata)
}
