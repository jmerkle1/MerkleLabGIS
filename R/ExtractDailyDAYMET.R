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


ExtractDailyDAYMET <- function(XYdata = data,
                               datesname = "date",
                               Metrics = c("prcp", "swe", "tmax", "tmin"),
                               num_cores = NULL) {
  
  if (!inherits(XYdata, "sf"))
    stop("XYdata is not an sf object")
  
  require("sf")
  require("parallel")
  require("terra")
  
  #Check cores
  if(is.null(num_cores)){
    num_cores <- detectCores()-1
  }else{
    num_cores <- num_cores
  }
  
  #Check date column
  dates <- XYdata[[datesname]]
  
  if (!inherits(dates, "POSIXct"))
    stop("Your datesname column is not POSIXct")
  if (any(is.na(dates)))
    stop("You have NAs in your datesname column")
  
  rm(dates)
  
  XYdata$formatted_dates <- format(XYdata[[datesname]], "%Y-%m-%d")
  
  
  # Determine the start and end dates for the SNODAS api
  start_date <- min(XYdata$formatted_dates)
  end_date <- max(XYdata$formatted_dates)
  
  dat <- httr::POST("https://devise.uwyo.edu/Umbraco/api/DaymetApi/GetData",
                    httr::content_type_json(),
                    body = jsonlite::toJSON(list(StartDate = jsonlite::unbox(start_date),
                                                 EndDate = jsonlite::unbox(end_date),
                                                 Metrics = Metrics),
                                            auto_unbox = FALSE)
  ) %>% httr::content()
  
  df <- data.frame(t(sapply(dat, c)))
  df$sampDate <- as.POSIXct(unlist(df$sampDate))
  dates <- as.POSIXct(df$sampDate)
  
  #Save original crs
  original_crs <- st_crs(XYdata)
  
  #Transform for extraction
  XYdata <- st_transform(XYdata, crs = 5072)
  
  XYdata <- XYdata[order(XYdata$date), ]
  
  XYtemp <- XYdata[,c("formatted_dates","geometry")]
  
  unique_dates <- unique(XYtemp$formatted_dates)
  
  # Initialize an empty list
  dat_daymet_list <- list()  
  
  clust <- makeCluster(num_cores)
  
  
  clusterEvalQ(clust, library(sf))
  
  # Export the objects you need for your calculations from your environment to each node's environment
  clusterExport(clust, varlist = c("XYtemp", "unique_dates", "Metrics", "df", "dat_daymet_list"), envir = environment())
  # Parallelize the extraction process
  
  system.time({
    dat_daymet_list <- clusterApplyLB(clust, 1:length(unique_dates), function(i) {
      
      row <- XYtemp[XYtemp$formatted_dates == unique_dates[i], ]
      date_str <- row$formatted_dates[1]
      
      toreturn <- do.call(cbind, lapply(Metrics, function(z) {
        # Filter url by Metric and date
        url_for_date <- df$url[df$sampDate == date_str & df$metric == z]
        
        if (length(url_for_date) == 0) {
          return(rep(NA, nrow(row)))  # No raster available for this date and metric
        } else {
          # Extract raster
          r <- terra::rast(as.character(url_for_date))
          return(terra::extract(r, row)[, 2])  # Extract second value from the raster
        }
      }))
      colnames(toreturn) <- Metrics  
      return(toreturn)
    })
  })
  
  # Stop the cluster
  stopCluster(clust)
  
  # Combine extracted data from the list into a data frame
  df_list <- lapply(dat_daymet_list, as.data.frame)
  dat_daymet_df <- do.call(rbind, df_list)
  
  # Bind the extracted data to the original XYdata
  XYdata <- cbind(XYdata, dat_daymet_df)
  
  # Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  return(XYdata)
}
