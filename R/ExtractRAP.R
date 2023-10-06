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
#' @import sp
#' @import dplyr
#' @import MerkleLabGIS
#' @export
#' 
#' 
#' 
#' 

ExtractRAP <- function(XYdata, 
                       RAPmetric = c("Biomass_AnnualForbsGrasses", "Cover_BareGround"),
                       bio_year_start = 150,
                       datesname,
                       maxcpus = NULL) {
  
  #Check cores
  if(is.null(maxcpus)){
    maxcpus <- detectCores()-1
  }else{
    maxcpus <- maxcpus
  }
  
  #Fetch RAP data from pathfinder
  dt <- bucket()
  RAP <- dt[dt$Category == "Landcover_RAP",]
  drs <- RAP$filename
  
  if(all(c("terra","sf","parallel") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: raster, sf, and parallel")
  require("terra")
  require("sf")
  require("parallel")
  
  if(inherits(XYdata, "sf") == FALSE) stop("XYdata is not an sf object")
  # dates <- st_drop_geometry(XYdata)[,datesname]
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
  clust <- makeCluster(no_cores) 
  # export the objects you need for your calculations from your environment to each node's environment
  clusterExport(clust, varlist=c("XYdata","year_bio","yrs","drs","RAPmetric","RAP"),envir=environment())
  rap_list <- list()  
  
  
  rap_list <- do.call(rbind, clusterApplyLB(clust, 1:length(yrs), function(i){
    library(sf)
    library(terra)
    
    # grab the data for the given year
    tmp <- XYdata[year_bio == yrs[i],]
    
    # Initialize toreturn dataframe
    toreturn <- data.frame(matrix(ncol = length(RAPmetric), nrow = nrow(tmp)))
    colnames(toreturn) <- RAPmetric
    
    # loop over the metrics for this year
    for(e in 1:length(RAPmetric)) {
      
      # This retrieves the URL from the RAP data frame for the given year and metric
      file_url <- paste0("/vsicurl/", RAP$url[RAP$filename == paste0("RAP_", yrs[i], "_", RAPmetric[e], ".tif")])
      
      if(length(file_url) == 0) {
        toreturn[,e] <- NA  # No raster available for this date and metric
      } else {
        r <- terra::rast(file_url)
        toreturn[,e] <- terra::extract(r, tmp)[, 2] 
      }
    }
    
    return(toreturn)
  }))
  
  
  stopCluster(clust)  
  
  # Bind the extracted data to the original XYdata
  XYdata <- cbind(XYdata, rap_list)
  
  
  return(XYdata)
}
