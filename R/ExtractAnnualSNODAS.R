#' Annual SNODAS Extraction Function
#' 
#' The Extract Annual SNODAS function processes data from SNODAS
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
#' @import parallel
#' @import tidyverse
#' @import lubridate
#' @import httr
#' @export
#' 
#' 
#' 
#' 


ExtractAnnualSNODAS <- function(point_data, start_date, end_date,
                                metric_name = c("snowdepth"), num_cores = NULL) {
  
  message("--------------------------------------------------")
  message("Starting SNODAS extraction...")
  
  if (!inherits(point_data, "sf"))
    stop("point_data must be an sf object")
  
  allowed_metrics <- c("snowdepth", "snowdepth-accum", "swe", "snowdays", "snowmelt")
  metric_name <- tolower(metric_name)
  if (!all(metric_name %in% allowed_metrics)) {
    stop(paste0("metric_name must be one or more of: ",
                paste(allowed_metrics, collapse = ", ")))
  }
  
  if (is.null(num_cores)) num_cores <- detectCores() - 1
  
  years <- seq(year(start_date), year(end_date))
  original_crs <- st_crs(point_data)
  point_data <- st_transform(point_data, 5072)
  
  clust <- makeCluster(num_cores)
  clusterEvalQ(clust, { library(terra); library(sf) })
  clusterExport(clust, c("point_data", "years"), envir = environment())
  
  message("Extracting SNODAS values from remote COGs...")
  
  # parallelize over metrics
  result_list <- clusterApplyLB(clust, metric_name, function(metric) {
    url <- sprintf("https://devise.s3.arcc.uwyo.edu/cloudenabled/annual/cog/snodas/snodas_annual_%s_all-years.tif",
                   metric)
    vsicurl_path <- paste0("/vsicurl/", url)
    
    tryCatch({
      r <- terra::rast(vsicurl_path)
      
      # preallocate
      res_metric <- matrix(NA, nrow = nrow(point_data), ncol = length(years))
      colnames(res_metric) <- paste0(metric, "_", years)
      
      for (k in seq_along(years)) {
        yr <- years[k]
        band_index <- which(grepl(paste0(metric, "_", yr), names(r)))
        if (length(band_index) > 0) {
          v <- terra::extract(r[[band_index]], point_data)
          if (!is.null(v) && ncol(v) > 1) {
            res_metric[, k] <- v[, 2]
          }
        }
      }
      as.data.frame(res_metric)
    }, error = function(e) {
      df_na <- as.data.frame(matrix(NA, nrow = nrow(point_data), ncol = length(years)))
      colnames(df_na) <- paste0(metric, "_", years)
      df_na
    })
  })
  
  stopCluster(clust)
  
  # combine metrics side by side
  final_df <- do.call(cbind, result_list)
  output <- cbind(point_data, final_df)
  output <- st_transform(output, crs = original_crs)
  
  message("SNODAS extraction complete.")
  message("--------------------------------------------------")
  return(output)
}