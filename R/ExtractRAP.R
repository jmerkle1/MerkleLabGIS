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
#' @param resolution spatial resolution of RAP data to extract as character. Options are 30m or 10m available only for 2018–2024, and required for some metrics such as 
#'   `Cover_Sagebrush`, `Cover_PinyonJuniper`, and `Cover_AnnualInvasiveGrasses`. 
#' @return Returns a vector with length of XYdata with the point/date values of RAPmetrics
#' 
#' 
#' @return
#' @import sf
#' @import terra
#' @import dplyr
#' @import MerkleLabGIS
#' @import parallel
#' @export
#' 
#' 
#' 
#' 

ExtractRAP <- function(
    XYdata, 
    RAPmetric = c("Biomass_AnnualForbsGrasses", "Cover_BareGround"),
    bio_year_start = NULL,
    datesname,
    maxcpus = NULL,
    resolution = "30m"
) {
  message("--------------------------------------------------")
  message("Starting RAP extraction...")
  
  if (all(c("terra","sf","parallel") %in% installed.packages()[,1]) == FALSE)
    stop("You must install the following packages: terra, sf, and parallel")
  
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

  resolution <- gsub(" ", "", resolution)
  if (length(resolution) > 1) {
    warning(
      "Multiple resolutions requested (", paste(resolution, collapse = ", "),
      "), but only one resolution is allowed. Using the first: ", resolution[1],
      immediate. = TRUE
    )
    resolution <- resolution[1]
  }
  
  resolution <- match.arg(resolution, choices = c("30m","10m"))
  
  
  res_meta <- ifelse(resolution == "10m", "10 m", "30 m")
  message("Resolution requested: ", resolution, " (metadata value: ", res_meta, ")")
  message("Using up to ", maxcpus, " cores.")
  
  metric_map <- c(
    "Biomass_AnnualForbsGrasses"    = "Biomass Annual Forbs Grasses",
    "Biomass_PerennialForbsGrasses" = "Biomass Perennial Forbs Grasses",
    "Cover_AnnualForbsGrasses"      = "Cover Annual Forbs Grasses",
    "Cover_BareGround"              = "Cover Bare Ground",
    "Cover_Litter"                  = "Cover Litter",
    "Cover_PerennialForbsGrasses"   = "Cover Perennial Forbs Grasses",
    "Cover_Shrubs"                  = "Cover Shrubs",
    "Cover_Trees"                   = "Cover Trees",
    "Cover_PinyonJuniper"           = "Cover Pinyon-juniper",
    "Cover_Sagebrush"               = "Cover Sagebrush",
    "Cover_AnnualInvasiveGrass"   = "Cover Annual Invasive Grass"
    
  )
 
  if (any(!RAPmetric %in% names(metric_map))) {
    stop("RAPmetric must be one of: ", paste(names(metric_map), collapse = ", "))
  }
  
  tenm_only <- c("Cover_Sagebrush", "Cover_PinyonJuniper", "Cover_AnnualInvasiveGrasses")
  if (resolution == "30m" && any(RAPmetric %in% tenm_only)) {
    warning(
      "The following metrics are only available at 10 m resolution: ",
      paste(intersect(RAPmetric, tenm_only), collapse = ", "),
      ". Expect NA values when using 30 m resolution.",
      immediate. = TRUE
    )
  }
  
  if (!inherits(XYdata, "sf")) stop("XYdata is not an sf object")
  dates <- sf::st_drop_geometry(XYdata)[[datesname]]
  if (!inherits(dates, "POSIXct")) stop("XYdata[,datesname] is not POSIXct")
  if (any(is.na(dates))) stop("You have NAs in your date column")
  if ("temp" %in% colnames(XYdata)) stop("Please remove the column named 'temp' in your XYdata.")
  
  # --- biological year ---
  yr  <- as.integer(format(dates, "%Y"))
  jul <- as.integer(format(dates, "%j"))
  year_bio <- ifelse(jul > bio_year_start, yr, yr - 1) # bio year... basically, this is the year of the growing season and after scenecense. thus any days prior to bio_year_start in a given calendar year will be given the previous calendar years' RAP value.
  
  # --- reorder XYdata and year_bio ---
  ord <- order(dates)
  XYdata   <- XYdata[ord, ]
  dates    <- dates[ord]
  year_bio <- year_bio[ord]
  
  yrs <- sort(unique(year_bio))

  if (resolution == "10m" && any(yrs < 2018 | yrs > 2024)) {
    warning("10 m RAP data is only available for 2018–2024. Expect NA values outside this range.", immediate. = TRUE)
  }
  
  dt  <- bucket()
  RAP <- dt[dt$category == "Landcover_RAP", ]
  
  # --- build URL table once ---
  url_table <- data.frame(Year=integer(), Metric=character(), URL=character(), stringsAsFactors=FALSE)
  for (yy in yrs) {
    for (m in RAPmetric) {
      meta_metric <- metric_map[[m]]
      rap_sub <- RAP[
        RAP$metric == meta_metric &
          RAP$sampYear == yy &
          !is.na(RAP$resolution) &
          RAP$resolution == res_meta,
      ]
      url <- if (nrow(rap_sub) > 0) rap_sub$url[1] else NA
      url_table <- rbind(url_table, data.frame(Year=yy, Metric=m, URL=url, stringsAsFactors=FALSE))
    }
  }
  message("--------------------------------------------------")
  message("RAP files to be used:")
  print(url_table)
  message("--------------------------------------------------")

  original_crs <- sf::st_crs(XYdata)
  out <- as.data.frame(matrix(NA_real_, nrow(XYdata), length(RAPmetric)))
  colnames(out) <- RAPmetric
  
  no_cores <- min(length(yrs), maxcpus)
  clust <- parallel::makeCluster(no_cores)
  parallel::clusterExport(
    clust,
    varlist=c("XYdata","year_bio","yrs","RAPmetric","url_table"),
    envir=environment()
  )
  
  rap_list <- parallel::parLapply(clust, 1:length(yrs), function(i) {
    library(sf)
    library(terra)
    yy <- yrs[i]
    idx <- which(year_bio == yy)
    tmp <- XYdata[idx, ]
    toreturn <- data.frame(matrix(NA_real_, nrow = length(idx), ncol = length(RAPmetric)))
    colnames(toreturn) <- RAPmetric
    
    for (m in RAPmetric) {
      url <- url_table$URL[url_table$Year == yy & url_table$Metric == m]
      url <- if (length(url)) url[1] else NA
      
      if (is.na(url) || url == "") {
        toreturn[, m] <- NA_real_
        next
      }
      
      file_url <- paste0("/vsicurl/", url)
      vals <- tryCatch({
        r <- terra::rast(file_url)
        tmp_proj <- sf::st_transform(tmp, crs = sf::st_crs(terra::crs(r, proj=TRUE)))
        df <- terra::extract(r, tmp_proj)
        df[[ ncol(df) ]]
      }, error = function(e) {
        rep(NA_real_, length(idx))
      })
      toreturn[, m] <- vals
    }
    return(list(idx=idx, data=toreturn))
  })
  parallel::stopCluster(clust)
  
  # merge results back
  for (res in rap_list) {
    out[res$idx, ] <- res$data
  }
  
  XYout <- cbind(XYdata, out)
  XYout <- sf::st_transform(XYout, crs = original_crs)
  attr(XYout, "RAP_urls") <- url_table
  
  message("RAP extraction finished.")
  message("--------------------------------------------------")
  return(XYout)
}
                  