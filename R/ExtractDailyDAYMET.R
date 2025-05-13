#' Daily DAYMET Extraction Function
#' 
#' The Extract Daily DAYMET function processes data from DAYMET
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired DAYMET metrics either "prcp", "swe", "tmax", "tmin", and a date range. It extracts metric values for the provided points. 
#' 
#' @param XYdata sf point object
#' @param Metrics character vector of DAYMET metrics to use ("prcp", "swe", "tmax", "tmin")
#' @param datesname name of your column representing date as POSIXct
#' @param num_cores number of cores to use for parallel processing
#' @return Returns a vector with length of XYdata with the point/date values of DaymetMetrics
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


ExtractDailyDAYMET <- function(XYdata, datesname = "date", Metrics = "prcp",num_cores = NULL) {
  if (!inherits(XYdata, "sf"))
    stop("XYdata must be an sf object")
  
  allowed_metrics <- c("prcp", "swe", "tmax", "tmin")
  
  Metrics <- tolower(Metrics)
  if (!all(Metrics %in% allowed_metrics)) {
    stop(paste0("Metrics must be one or more of: ", paste(allowed_metrics, collapse = ", ")))
  }
  
  
  require("sf")
  require("parallel")
  require("terra")
  require("httr")
  require("jsonlite")
  require("dplyr")
  
  #Check cores
  if(is.null(num_cores)){
    num_cores <- detectCores()-1
  }else{
    num_cores <- num_cores
  }  
  dates <- XYdata[[datesname]]
  if (!inherits(dates, "POSIXct")) stop("Your dates column is not POSIXct")
  if (any(is.na(dates))) stop("NA values found in date column")
  
  Metrics <- tolower(Metrics)
  
  XYdata$formatted_dates <- format(dates, "%Y-%m-%d")
  XYdata$year <- year(dates)
  XYdata$doy <- yday(dates)
  
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
      band <- row$doy
      
      url <- sprintf("https://pathfinder.arcc.uwyo.edu/devise/cloudenabled/daily/cog/daymet/%s/daymet_daily_%s_%d.tif",
                     metric, metric, yr)
      vsicurl_path <- paste0("/vsicurl/", url)
      
      tryCatch({
        r <- terra::rast(vsicurl_path)
        v <- terra::extract(r[[band]], row)
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

