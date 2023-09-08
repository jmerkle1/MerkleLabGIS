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
  
  # Parallel processing setup
  cl <- makeCluster(num_cores)
  
  clusterEvalQ(cl, {
    library(terra)
    library(sf)
    
  })
  
  # Parallel function for extraction
  parallel_extract <- function(date_index, XYdata, Metrics, formatted_dates, unique_dates, df) {
    date <- formatted_dates[date_index]
    date_str <- format(unique_dates[date_index], "%Y-%m-%d")
    url_for_date <- df$url[df$sampDate == date_str & df$metric == Metrics]  # Filter by Metric
    if (length(url_for_date) == 0) {
      return(NA)  # No raster available for this date and metric
    }
    r <- try(terra::rast(as.character(url_for_date)), silent = TRUE)
    if (inherits(r, "try-error")) {
      print(paste0("Warning: Error fetching raster for ", date_str, " and metric ", Metrics))
      return(NA)
    } else if (!inherits(r, "SpatRaster")) {
      print(paste0("Warning: Fetched object is not a SpatRaster for ", date_str, " and metric ", Metrics))
      return(NA)
    } else {
      extracted_vals <- terra::extract(r, XYdata[dates == unique_dates[date_index], , drop = FALSE])
      return(extracted_vals[[2]])  # Return the extracted value
    }
  }
  
  extracted_vals_list <- clusterApply(cl, seq_along(unique_dates), function(i) {
    parallel_extract(i, XYdata, Metrics, formatted_dates, unique_dates, df)
  })
  
  stopCluster(cl)
  
  for (i in seq_along(unique_dates)) {
    col_name <- paste0("Snodas_", Metrics)
    XYdata[dates == unique_dates[i], col_name] <- extracted_vals_list[[i]]
  }
  
  # Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  # Remove formatted date column
  XYdata <- XYdata %>%
    select(-formatted_dates)
  
  return(XYdata)
}


