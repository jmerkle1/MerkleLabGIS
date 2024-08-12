#' RAP Extraction Function
#' 
#' The ExtractRAP function processes data from the Rangeland Analysis Platform (RAP)
#'to extract specific metrics for given geographic points and dates.Users provide spatial data
#'(XYdata), desired RAP metrics (e.g., Biomass, Cover), a date column name, and a bio year,
#' determining the growing season. For each year and metric, it fetches the associated RAP 
#' raster data set and extracts metric values for the provided points. 
#' 
#' @param XYdata sf point object
#' @param RAPmetric character vector of rap metrics to use (e.g. Biomass_AnnualForbsGrasses, Cover_Bareground)
#' @param bio_year_start  julian date at which the biological year or growing season starts. In other words, 
#' before this julian day of a given year, the previous year's rap metrics will be extracted 
#' @param datesname name of your column representing date as POSIXct
#' @param maxcpus how many cores
#' @return Returns a vector with length of XYdata with the point/date values of RAPmetrics
#' 
#' 
#' @return
#' @import sf
#' @import terra
#' @import dplyr
#' @import MerkleLabGIS
#' @export
#' 
#' 
#' 
#' 

ExtractRAP <- function(XYdata, 
                       RAPmetric = c("Biomass_AnnualForbsGrasses", "Cover_BareGround"),
                       bio_year_start = NULL,
                       datesname,
                       maxcpus = NULL) {
  
  if(all(c("terra","sf","parallel") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: raster, sf, and parallel")
  require("terra")
  require("sf")
  require("parallel")
  
  if (is.null(bio_year_start))
  {
    bio_year_start <- 150
  } else{
    bio_year_start <- bio_year_start
  }
  
  #Check cores
  if(is.null(maxcpus)){
    maxcpus <- detectCores()-1
  }else{
    maxcpus <- maxcpus
  }
  
  #Fetch RAP data from pathfinder
  dt <- bucket()
  RAP <- dt[dt$category == "Landcover_RAP",]
  drs <- RAP$filename
  
  if(inherits(XYdata, "sf") == FALSE) stop("XYdata is not an sf object")
  dates <- st_drop_geometry(XYdata)[[datesname]]
  
  if(inherits(dates, "POSIXct") != TRUE) 
    stop("XYdata[,datesname] is not POSIXct")
  if(any(is.na(dates) == TRUE)) 
    stop("You have NAs in your date column")

  if("temp" %in% colnames(XYdata))
    stop("Please remove the column named 'temp' in your XYdata. Thank you!")
  
  if(any(RAPmetric %in% c("Biomass_AnnualForbsGrasses","Biomass_PerennialForbsGrasses", 
                          "Cover_AnnualForbsGrasses","Cover_BareGround","Cover_Litter","Cover_PerennialForbsGrasses",
                          "Cover_Shrubs","Cover_Trees")==FALSE)){
    stop("The RAPmetric must only be Biomass_AnnualForbsGrasses,Biomass_PerennialForbsGrasses, 
                    Cover_AnnualForbsGrasses,Cover_BareGround,Cover_Litter,Cover_PerennialForbsGrasses,
                       Cover_Shrubs,Cover_Trees.")
  }
  
  tz <- attr(dates,"tzone")
  year <- as.numeric(format(dates, "%Y"))
  jul <- as.numeric(format(dates, "%j"))
  
  year_bio <- ifelse(jul > bio_year_start, year, year-1)  # bio year... basically, this is the year of the growing season and after scenecense. thus any days prior to bio_year_start in a given calendar year will be given the previous calendar years' RAP value.
  yrs <- unique(year_bio)
  
  
  # identify cores (use 1 less than you have)
  no_cores <- ifelse(length(yrs) < maxcpus, length(yrs), maxcpus)
  # Setup cluster
  clust <- parallel::makeCluster(no_cores) 
  # export the objects you need for your calculations from your environment to each node's environment
  parallel::clusterExport(clust, varlist=c("XYdata","year_bio","yrs","drs","RAPmetric","RAP"),envir=environment())
  rap_list <- list()  
  
  #Save original crs
  original_crs <- st_crs(XYdata)
  
  #Transform for extraction
  XYdata <- st_transform(XYdata, crs = 4326)
  XYdata <- XYdata[order(XYdata[[datesname]]), ]
  
  rap_list <- do.call(rbind, parallel::clusterApplyLB(clust, 1:length(yrs), function(i){
    library(sf)
    library(terra)
    
    # grab the data for the given year
    tmp <- XYdata[year_bio == yrs[i],]
    
    # Initialize toreturn dataframe
    toreturn <- data.frame(matrix(ncol = length(RAPmetric), nrow = nrow(tmp)))
    colnames(toreturn) <- RAPmetric
    
    # loop over the metrics for this year
    for(e in 1:length(RAPmetric)) {

    url <- RAP$url[which(RAP$filename == paste0("RAP_", yrs[i], "_", RAPmetric[e], ".tif"))]
      
      # Ensure URL is valid, if not assign NA and continue
      if(length(url) == 0 || url == "") {
        toreturn[,e] <- NA
        next
      }
      
      file_url <- paste0("/vsicurl/", url)
      
      # Try to get raster data, if an error occurs, set the metric to NA
      result <- tryCatch({
        r <- terra::rast(file_url)
        terra::extract(r, tmp)[, 2]
      }, error = function(err) {
        NA
      })
      
      toreturn[,e] <- result
    }
    
    
    return(toreturn)
  }))
  
  
  parallel::stopCluster(clust)  
  
  # Bind the extracted data to the original XYdata
  XYdata <- cbind(XYdata, rap_list)
  
  # Reproject back to original data
  XYdata <- st_transform(XYdata, crs = original_crs)
  
  return(XYdata)
}
