#' NDVI Extraction Function
#' 
#' The ExtractNDVI function processes data from the MODIS NDVI datasets
#' to extract specific NDVI metrics for given geographic points and dates. Provide spatial data 
#' (XYdata), desired NDVI metrics (e.g., MaxNDVIDay, MaxIRGday), and a date column name. 
#' For each year and metric, it fetches the associated NDVI raster dataset 
#' and extracts metric values for the provided points. 
#' 
#' @param XYdata sf point object
#' @param NDVImetric character vector of NDVI metrics to use (e.g., MaxNDVIDay, MaxIRGday).
#' @param datesname Name of the column in XYdata representing date as POSIXct.
#' @param maxcpus Specifies how many cores to use for parallel processing.
#' @return Returns a dataframe with rows corresponding to XYdata and the extracted values of NDVImetric for each point/date combination.
#' 
#' @import sf
#' @import terra
#' @import snowfall
#' @export
#' 
#' @examples
#' # To use this function, you would typically follow the pattern:
#' # NDVI <- ExtractNDVI(XYdata = mySpatialPoints, NDVImetric = c("MaxNDVIDay", "MaxIRGday"), datesname = "myDateColumn", maxcpus = 4)
#' 


ExtractNDVI <- function(XYdata, NDVImetric, datesname, maxcpus = 4){
  
  # Import necessary libraries
  require("snowfall")
  require("sf")
  require("terra")
  
  # Check for input type
  if(!inherits(XYdata, "sf")) stop("XYdata is not an sf object")
  
  # Other checks
  if(any(is.na(XYdata[[datesname]]))) 
    stop("You have NAs in your date column")
  
  if(any(NDVImetric %in% c("MaxNDVIDay","MaxIRGday", "SpringStartDay", "SpringEndDay","IntegratedNDVI","MaxBrownDownDate",
                           "NDVI_scaled","IRG_scaled","SE_springDLCfit", "SpringLength","sumIRG","SpringScale")==FALSE)){
    stop("The NDVImetric must only be MaxNDVIDay, MaxIRGday, SpringStartDay, SpringEndDay,
         IntegratedNDVI, NDVI_scaled, IRG_scaled, MaxBrownDownDate, SE_springDLCfit,SpringScale, SpringLength, or sumIRG.")
  }
  
  original_crs <- st_crs(XYdata)
  
  dt <- bucket()
  MODIS_NDVI <- dt[dt$Category == "MODIS_NDVI",]
  MODIS_NDVI <- MODIS_NDVI[complete.cases(MODIS_NDVI$url), ]
  drs <- MODIS_NDVI$filename
  xyCRS <- 5072
  
  # Transformation & Feature extraction
  XYdata <- st_transform(XYdata, crs = xyCRS)
  tz <- attr(XYdata[[datesname]],"tzone")
  XYdata$year <- as.numeric(strftime(XYdata[[datesname]], format = "%Y", tz = tz))
  XYdata$jul <- as.numeric(strftime(XYdata[[datesname]], format = "%j", tz = tz))
  XYdata$jul[XYdata$jul > 365] <- 365
  XYdata$unique <- 1:nrow(XYdata)
  u <- unique(XYdata$year)
  #u <- unique(XYdata$year[XYdata$year != 2023])

  
  vals_list <- list() # Empty list to store results for each metric
  sfInit(parallel=TRUE, cpus= maxcpus) 
  
  on.exit(sfStop(), add = TRUE)
  sfExport('MODIS_NDVI','vals_list')
  
  for(metric in NDVImetric){
    
    vals <- do.call(rbind, sfClusterApplyLB(1:length(u), function(i){
      library(sf)
      temp <- XYdata[XYdata$year==u[i],]
      if(metric %in% c("MaxNDVIDay","MaxIRGday","SpringStartDay","SpringEndDay","IntegratedNDVI","SpringLength")){
        selected_url <- MODIS_NDVI$url[MODIS_NDVI$filename == paste0("MOD09Q1_", u[i], "_", metric, ".tif")]
        if (length(selected_url) == 0) {
          # File not found, set the value to NA
          toreturn <- data.frame(unique = temp$unique, setNames(list(NA), metric))
        } else {
        
        stk <- terra::rast(paste0("/vsicurl/", selected_url))
        
        stk_5072 <- terra::project(stk, "EPSG:5072")
        vals <- terra::extract(stk_5072, temp)$layer 

        toreturn <- data.frame(unique = temp$unique, 
                               setNames(list(vals), metric))
        }
        names(toreturn) <- c("unique", metric)
        return(toreturn)
      }else{
        if(metric %in% c("SpringScale","MaxBrownDownDate","SE_springDLCfit")){
          if(metric == "SpringScale"){
            selected_url <- MODIS_NDVI$url[MODIS_NDVI$filename == paste0("DLC_", u[i], "_scalS_Estimate.tif")]
            if (length(selected_url) == 0) {
              # File not found, set the value to NA
              toreturn <- data.frame(unique = temp$unique, setNames(list(NA), metric))
            } else {
             stk <- terra::rast(paste0("/vsicurl/", selected_url))
            
            
            stk_5072 <- terra::project(stk, "EPSG:5072")
            vals <- terra::extract(stk_5072, temp)$layer 
            
            toreturn <- data.frame(unique=temp$unique, vals=vals)
            }
            names(toreturn) <- c("unique", metric)
            return(toreturn)
          }
          if(metric == "MaxBrownDownDate"){
            
            selected_url <- MODIS_NDVI$url[MODIS_NDVI$filename == paste0("DLC_", u[i], "_xmidA_Estimate.tif")]
            if (length(selected_url) == 0) {
              # File not found, set the value to NA
              toreturn <- data.frame(unique = temp$unique, setNames(list(NA), metric))
            } else {
            stk <- terra::rast(paste0("/vsicurl/", selected_url))
            
            stk_5072 <- terra::project(stk, "EPSG:5072")
            vals <- terra::extract(stk_5072, temp)$layer 
            
            toreturn <- data.frame(unique=temp$unique, vals=vals)
            }
            names(toreturn) <- c("unique", metric)
            return(toreturn)
          }
          if(metric == "SE_springDLCfit"){
            selected_url <- MODIS_NDVI$url[MODIS_NDVI$filename == paste0("DLC_", u[i], "_xmidS_SD_Estimate.tif")]
            if (length(selected_url) == 0) {
              # File not found, set the value to NA
              toreturn <- data.frame(unique = temp$unique, setNames(list(NA), metric))
            } else {
            stk <- terra::rast(paste0("/vsicurl/", selected_url))
            
            stk_5072 <- terra::project(stk, "EPSG:5072")
            vals <- terra::extract(stk_5072, temp)$layer 
            
            toreturn <- data.frame(unique=temp$unique, vals=vals)
            }
            names(toreturn) <- c("unique", metric)
            return(toreturn)
          }
        }else{
          filtered_DLC <- MODIS_NDVI[grep("^DLC_", MODIS_NDVI$filename), ]
          
          stk <- filtered_DLC$url
          
          stk <- grep(u[i], stk, value = TRUE)
          
          stk <- stk[grepl("SD", stk) == FALSE]
          stk <- stk[c(4, 2, 3, 1)]  # need to reorder so they are in the correct order for the logistic equation
          
          # Check if stk is empty
          if (length(stk) == 0) {
            vals <- rep(NA, 365)  # Set 'NA' as the value when stk is empty
          } else {
            
            stk <- lapply(paste0("/vsicurl/", stk), function(url) {
              tryCatch(
                {
                  terra::rast(url)
                },
                error = function(e) {
                  warning(paste("[rast] Error:", e$message))
                  return(NULL)  # Return NULL for failed URLs
                }
              )
            })
            
            # Remove NULL values from stk
            stk <- stk[!sapply(stk, is.null)]
            
            if (length(stk) == 0) {
              vals <- rep(NA, 365)  # Set 'NA' as the value when URLs fail
            } else {
              stk <- lapply(stk, function(rst) terra::project(rst, "EPSG:5072"))
              
              vals_list <- lapply(stk, function(rst) terra::extract(rst, temp)$layer)
              
              # Combine the extracted values
              vals <- do.call(cbind, vals_list)
              
              time <- 1:365
              if (metric == c("NDVI_scaled")) {
                vals <- do.call(rbind, lapply(1:nrow(vals), function(e) {
                  (1 / (1 + exp((vals[e, 1] - time) / vals[e, 2]))) - (1 / (1 + exp((vals[e, 3] - time) / vals[e, 4])))
                }))
                vals <- do.call(c, lapply(1:nrow(vals), function(e) {
                  return(vals[e, temp$jul[e]])
                }))
                toreturn <- data.frame(unique = temp$unique, vals = vals)
                names(toreturn) <- c("unique", metric)
                return(toreturn)
              } else {
                vals <- do.call(rbind, lapply(1:nrow(vals), function(e) {
                  temp <- (1 / (1 + exp((vals[e, 1] - time) / vals[e, 2])))
                  temp <- c((diff(temp) / diff(time)), NA)
                  temp[temp < 0] <- 0
                  if (all(is.na(temp)) == FALSE) {
                    temp <- (temp - min(temp, na.rm = TRUE)) / (max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE))
                  }
                  if (metric == "sumIRG") {
                    temp[is.na(temp) == TRUE] <- 0
                    return(cumsum(temp))
                  } else {
                    return(temp)
                  }
                }))
              }
              
              vals <- do.call(c, lapply(1:nrow(vals), function(e) {
                return(vals[e, temp$jul[e]])
              }))
              
              toreturn <- data.frame(unique = temp$unique, vals = vals)
              names(toreturn) <- c("unique", metric)
              return(toreturn)
            }
          }
          
        }
      }
    }))
    
    vals_list[[metric]] <- vals # Store the result for this metric in the list
    
  }
 
  # Combine the results for all metrics
  for(i in 1:length(NDVImetric)){
    metric = NDVImetric[i]
    if(i == 1){
      XYdata <- merge(XYdata, vals_list[[metric]], all.x=TRUE, by="unique")
    } else {
      XYdata <- merge(XYdata, vals_list[[metric]][,c("unique", metric)], all.x=TRUE, by="unique")
    }
  }
  
  XYdata <- XYdata[order(XYdata$unique),]
  # Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  return(XYdata)
  #return(XYdata[,c("unique", datesname, NDVImetric)])
}
