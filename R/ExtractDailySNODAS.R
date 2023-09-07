#' Daily SNODAS Extraction Function
#' 
#' The Extract Daily SNODAS function processes data from SNODAS
#' to extract specific metrics for given geographic points and dates.Users provide point spatial data
#', desired SNODAS metrics either SWE or SnowDepth, and a date range. It extracts metric values for the provided points. 
#' 
#' @param XYdata sf point object
#' @param Metrics character vector of SNODAS metrics to use (SWE or SnowDepth)
#' @param start_date
#' @param end_date
#' @param datesname name of your column representing date as POSIXct
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
                           start_date = "2005-01-01",
                           end_date = "2010-01-01",
                           datesname = "date",
                           Metrics = c("SWE","SnowDepth")) {  
  
  if (!inherits(XYdata, "sf")) 
    stop("XYdata is not an sf object")

  dat <- httr::POST("https://devise.uwyo.edu/Umbraco/api/SnodasApi/GetData",
                    httr::content_type_json(),
                    body = jsonlite::toJSON(list(StartDate = jsonlite::unbox(start_date),
                                                 EndDate = jsonlite::unbox(end_date),
                                                 Metrics = Metrics),  # Use the provided Metrics argument
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
  
  unique_dates <- unique(dates)


  formatted_dates <- paste0(format(unique_dates, "%Y-%m-%d"))
  XYdata$formatted_dates <- formatted_dates
  
  XYdata <- st_transform(XYdata, crs = 5072)
  
  for (Metric in Metrics) { 
    productName <- Metric  
    
    # Initialize a new column for extracted Metric values
    col_name <- paste0("Snodas_", productName)
    XYdata[[col_name]] <- NA
    
    # Create a list to store extracted values
    extracted_vals_list <- list()  
    
    # Loop over unique_dates and extract values
    for (i in 1:length(unique_dates)) {
      date <- formatted_dates[i]
      date_str <- format(unique_dates[i], "%Y-%m-%d")
      url_for_date <- df$url[df$sampDate == date_str & df$metric == Metric]  # Filter by Metric
      r <- try(terra::rast(as.character(url_for_date)), silent = TRUE)
      
      if (inherits(r, "try-error")) {
        print(paste0("Warning: Error fetching raster for ", date_str, " and metric ", Metric))
      } else if (!inherits(r, "SpatRaster")) {
        print(paste0("Warning: Fetched object is not a SpatRaster for ", date_str, " and metric ", Metric))
      } else {
        extracted_vals <- extract(r, XYdata[dates == unique_dates[i], , drop = FALSE])  
        extracted_vals_list[[i]] <- extracted_vals[[2]]  # Store the extracted value
      }
    }
    
    # Assign the extracted values back to the correct rows in XYdata
    for (i in 1:length(unique_dates)) {
      XYdata[dates == unique_dates[i], col_name] <- extracted_vals_list[[i]]
    }
  }
  
  #Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  #Remove formatted date field
  XYdata <- XYdata %>%
    select(-formatted_dates)
  
  return(XYdata)
}


