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


ExtractDailyDAYMET <- function(XYdata, datesname = "date", Metrics = "prcp", num_cores = NULL) {
  message("--------------------------------------------------")
  message("Starting DAYMET extraction...")
  
  if (!inherits(XYdata, "sf")) stop("XYdata must be an sf object")
  
  allowed_metrics <- c("prcp", "swe", "tmax", "tmin")
  Metrics <- tolower(Metrics)
  if (!all(Metrics %in% allowed_metrics)) {
    stop(paste0("Metrics must be one or more of: ", paste(allowed_metrics, collapse = ", ")))
  }
  
  if (is.null(num_cores)) num_cores <- detectCores() - 1
  
  dates <- XYdata[[datesname]]
  if (!inherits(dates, "POSIXct")) stop("Your dates column is not POSIXct")
  if (any(is.na(dates))) stop("NA values found in date column")
  
  XYdata$year <- year(dates)
  XYdata$doy  <- yday(dates)
  
  original_crs <- st_crs(XYdata)
  XYdata <- st_transform(XYdata, 5072)
  
  # Tasks: year × metric
  tasks <- expand.grid(metric = Metrics,
                       year   = unique(XYdata$year),
                       stringsAsFactors = FALSE)
  
  clust <- makeCluster(num_cores)
  clusterEvalQ(clust, { library(terra); library(sf) })
  clusterExport(clust, "XYdata", envir = environment())
  
  message("Extracting DAYMET values from remote COGs...")
  
  result_list <- clusterApplyLB(clust, seq_len(nrow(tasks)), function(j) {
    metric <- tasks$metric[j]
    yr     <- tasks$year[j]
    
    url <- sprintf("https://devise.s3.arcc.uwyo.edu/cloudenabled/daily/cog/daymet/%s/daymet_daily_%s_%d.tif",
                   metric, metric, yr)
    vsicurl_path <- paste0("/vsicurl/", url)
    
    tryCatch({
      r <- terra::rast(vsicurl_path)
      sub <- XYdata[XYdata$year == yr, ]
      if (nrow(sub) == 0) return(NULL)
      
      vals <- numeric(nrow(sub))
      for (d in unique(sub$doy)) {
        if (d <= nlyr(r)) {
          rows <- which(sub$doy == d)
          v <- terra::extract(r[[d]], sub[rows, ])
          vals[rows] <- v[,2]
        }
      }
      data.frame(idx = as.integer(rownames(sub)),
                 metric = metric,
                 val = vals)
    }, error = function(e) {
      NULL
    })
  })
  
  stopCluster(clust)
  message("DAYMET extraction complete. Combining results...")
  
  
  results <- do.call(rbind, result_list)
  result_df <- data.frame(idx = seq_len(nrow(XYdata)))
  for (m in Metrics) {
    result_df[[m]] <- NA
    sel <- results$metric == m
    result_df[[m]][results$idx[sel]] <- results$val[sel]
  }
  
  XYdata <- cbind(XYdata, result_df[, Metrics, drop = FALSE])
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  message("Extraction finished. Returning data.")
  message("--------------------------------------------------")
  return(XYdata)
}

