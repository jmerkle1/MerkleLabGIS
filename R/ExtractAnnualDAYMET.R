#' DAYMET Extraction Function
#' 
#' The Extract DAYMET function processes annual data from DAYMET
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired DAYMET metrics ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin"), and a date range. It extracts metric values for the provided points. 
#' 
#' @param point_data sf point object
#' @param metric_name character vector of DAYMET metrics to use ("prcp", "swe", "tmax", "tmin")
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


ExtractAnnualDAYMET <- function(point_data, start_date, end_date, metric_name = c("prcp"), num_cores = NULL) {
  if (!inherits(point_data, "sf"))
    stop("point_data must be an sf object")
  
  allowed_metrics <- c("prcp", "swe", "tmax", "tmin")
  metric_name <- tolower(metric_name)
  
  if (!all(metric_name %in% allowed_metrics)) {
    stop(paste0("metric_name must be one or more of: ", paste(allowed_metrics, collapse = ", ")))
  }
  
  require(terra)
  require(sf)
  require(parallel)
  require(lubridate)
  
  if (is.null(num_cores)) num_cores <- detectCores() - 1
  
  years <- seq(year(start_date), year(end_date))
  original_crs <- st_crs(point_data)
  point_data <- st_transform(point_data, 5072)
  
  clust <- makeCluster(num_cores)
  clusterEvalQ(clust, {
    library(terra)
    library(sf)
  })
  clusterExport(clust, varlist = c("point_data", "metric_name", "years"), envir = environment())
  
  result_list <- clusterApplyLB(clust, 1:nrow(point_data), function(i) {
    row <- point_data[i, ]
    result_vector <- list()
    
    for (metric in metric_name) {
      url <- sprintf("https://pathfinder.arcc.uwyo.edu/devise/cloudenabled/annual/cog/daymet/daymet_annual_%s_all-years.tif", metric)
      vsicurl_path <- paste0("/vsicurl/", url)
      
      tryCatch({
        r <- terra::rast(vsicurl_path)
        
        for (yr in years) {
          band_index <- which(grepl(paste0(metric, "_", yr), names(r)))
          value <- NA
          if (length(band_index) > 0) {
            v <- terra::extract(r[[band_index]], row)
            if (!is.null(v) && ncol(v) > 1) value <- v[1, 2]
          }
          result_vector[[paste0(metric, "_", yr)]] <- value
        }
      }, error = function(e) {
        for (yr in years) {
          result_vector[[paste0(metric, "_", yr)]] <- NA
        }
      })
    }
    
    return(as.data.frame(result_vector))
  })
  
  stopCluster(clust)
  
  final_df <- do.call(rbind, result_list)
  output <- cbind(point_data, final_df)
  output <- st_transform(output, crs = original_crs)
  return(output)
}
