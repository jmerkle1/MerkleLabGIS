
#' Title
#'
#' @return
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @export
#'
#' @examples
#' bucket()
bucket <- function() {
  current_date <- format(Sys.time(), "%Y-%m-%d")
  
  #Get data in MerkleLab GIS 
  MerkleLabGIS <- GET("https://devise.uwyo.edu/umbraco/api/MerkleGisDataApi/GetFiles")
  MerkleLabGIS <- fromJSON(rawToChar(MerkleLabGIS$content))
  
  # URL to be parsed
  MerkleLabGIS$url <- MerkleLabGIS$fileUrl
  
  MerkleLabGIS$url <- sub("\\?.*", "", MerkleLabGIS$url)
  MerkleLabGIS$urlmodified_url <- sub("https://devise\\.pathfinder\\.arcc\\.uwyo\\.edu/", "https://pathfinder.arcc.uwyo.edu/devise/", MerkleLabGIS$url)
  
  # Filter URLs that do not end with .tif, .shp, or .xml
  valid_extensions <- c(".tif", ".zip", ".geojson")
  valid_urls <- grepl(paste0(paste0(valid_extensions, collapse = "|"), "$"), MerkleLabGIS$urlmodified_url)
  MerkleLabGIS <- MerkleLabGIS[valid_urls, ]
  
  # Parsing the URL
  parts <- strsplit(MerkleLabGIS$urlmodified_url, "/")
  max_parts <- max(sapply(parts, length))
  parsed_parts <- matrix("", nrow = length(parts), ncol = max_parts)
  
  for (i in 1:length(parts)) {
    parsed_parts[i, 1:length(parts[[i]])] <- parts[[i]]
  }
  
  parsed_parts_df <- data.frame(parsed_parts, stringsAsFactors = FALSE)
  MerkleLabGIS <- cbind(MerkleLabGIS, parsed_parts_df)
  MerkleLabGIS$Category <- MerkleLabGIS$X6
  MerkleLabGIS$filename <- MerkleLabGIS$fileName
  
  
  MerkleLabGIS$url <- MerkleLabGIS$urlmodified_url
  MerkleLabGIS <- MerkleLabGIS[, c("filename", "lastModified", "Category", "url")]

  # metadata <- GET("https://wildlifemovetools.org/api/layersmetadata")
  # metadata <- fromJSON(rawToChar(metadata$content))
  # metadata <- metadata[, !names(metadata) %in% c("oid", "category")]
  
  # MerkleLabGIS <- inner_join(MerkleLabGIS, metadata, by = c("filename", "url"))
  
  #Get Daily SNODAS Data
  snodas<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/SnodasApi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("SWE", "SnowDepth")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  snodas <- do.call(rbind.data.frame, snodas)
  snodas$Category[snodas$metric == "SnowDepth"] <- "Snodas_Snowdepth"
  snodas$Category[snodas$metric == "SWE"] <- "Snodas_SWE"
  
  #Get Annual SNODAS Data
  snodasAnnual<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/SnodasApi/GetDerivedAnnualData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("MaxSnowDepth", "MaxSWE","MeanSnowDepth","MeanSWE","MedianSnowDepth","MedianSWE")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  snodasAnnual <- do.call(rbind.data.frame, snodasAnnual)
  snodasAnnual$Category[snodasAnnual$metric == "MaxSnowDepth"] <- "Snodas_MaxSnowDepth"
  snodasAnnual$Category[snodasAnnual$metric == "MaxSWE"] <- "Snodas_MaxSWE"
  snodasAnnual$Category[snodasAnnual$metric == "MeanSnowDepth"] <- "Snodas_MeanSnowDepth"
  snodasAnnual$Category[snodasAnnual$metric == "MeanSWE"] <- "Snodas_MeanSWE"
  snodasAnnual$Category[snodasAnnual$metric == "MedianSnowDepth"] <- "Snodas_MedianSnowDepth"
  snodasAnnual$Category[snodasAnnual$metric == "MedianSWE"] <- "Snodas_MedianSWE"
  
  
  #Get Daily Daymet Data
  daymet<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/DaymetApi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("prcp", "swe", "tmax")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  daymet <- do.call(rbind.data.frame, daymet)
  daymet$Category[daymet$metric == "swe"] <- "Daymet_SWE"
  daymet$Category[daymet$metric == "prcp"] <- "Daymet_PRCP"
  daymet$Category[daymet$metric == "tmax"] <- "Daymet_TMAX"
  
  
  #Get Annual Daymet Data
  daymetAnnual<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/DaymetApi/GetDerivedAnnualData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("Maxprcp", "Maxswe", "Maxtmax", "Maxtmin", "Meanprcp","Meanswe", "Meantmax","Meantmin", "Medianprcp", "Medianswe", "Mediantmax", "Mediantmin", "Sumprcp")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  daymetAnnual <- do.call(rbind.data.frame, daymetAnnual)
  daymetAnnual$Category[daymetAnnual$metric == "Maxprcp"] <- "Daymet_Maxprcp"
  daymetAnnual$Category[daymetAnnual$metric == "Maxswe"] <- "Daymet_Maxswe"
  daymetAnnual$Category[daymetAnnual$metric == "Maxtmin"] <- "Daymet_Maxtmin"
  daymetAnnual$Category[daymetAnnual$metric == "Maxtmax"] <- "Daymet_Maxtmax"
  daymetAnnual$Category[daymetAnnual$metric == "Meanprcp"] <- "Daymet_Meanprcp"
  daymetAnnual$Category[daymetAnnual$metric == "Meanswe"] <- "Daymet_Meanswe"
  daymetAnnual$Category[daymetAnnual$metric == "Meantmax"] <- "Daymet_Meantmax"
  daymetAnnual$Category[daymetAnnual$metric == "Meantmin"] <- "Daymet_Meantmin"
  daymetAnnual$Category[daymetAnnual$metric == "Medianprcp"] <- "Daymet_Medianprcp"
  daymetAnnual$Category[daymetAnnual$metric == "Medianswe"] <- "Daymet_Medianswe"
  daymetAnnual$Category[daymetAnnual$metric == "Mediantmax"] <- "Daymet_Mediantmax"
  daymetAnnual$Category[daymetAnnual$metric == "Mediantmin"] <- "Daymet_Mediantmin"
  daymetAnnual$Category[daymetAnnual$metric == "Sumprcp"] <- "Daymet_Sumprcp"
  
  #Get eMODIS data

  eModis<- httr::POST(
    "https://devise.uwyo.edu/umbraco/api/erosapi/GetDerivedAnnualData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2005-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("dur","eost","maxt","sost","tin")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  
  dat <- do.call(rbind.data.frame, eModis)
  dat$Category[dat$metric == "dur"] <- "eMODIS_dur"
  dat$Category[dat$metric == "eost"] <- "eMODIS_eost"
  dat$Category[dat$metric == "maxt"] <- "eMODIS_maxt"
  dat$Category[dat$metric == "sost"] <- "eMODIS_sost"
  dat$Category[dat$metric == "tin"] <- "eMODIS_tin"
  

  #Get Daily PRISM Data
  prism_daily<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/PrismApi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("ppt", "tdmean", "tmax","tmean","tmin","vpdmax","vpdmin")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  prism_daily <- do.call(rbind.data.frame, prism_daily)
  prism_daily$Category[prism_daily$metric == "ppt"] <- "PRISM_PPT"
  prism_daily$Category[prism_daily$metric == "tdmean"] <- "PRSIM_TDMEAN"
  prism_daily$Category[prism_daily$metric == "tmax"] <- "PRISM_TMAX"
  prism_daily$Category[prism_daily$metric == "tmean"] <- "PRISM_TMEAN"
  prism_daily$Category[prism_daily$metric == "tmin"] <- "PRISM_TMIN"
  prism_daily$Category[prism_daily$metric == "vpdmax"] <- "PRISM_VPDMAX"
  prism_daily$Category[prism_daily$metric == "vpdmin"] <- "PRISM_VPDMIN"
  
  
  
  #Get Annual PRISM Data
  prism_annual<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/PrismApi/GetDerivedAnnualData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2000-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("Maxppt", "Maxtdmean", "Maxtmax", "Maxtmean", "Maxtmin","Maxvpdmax", "Maxvpdmin","Meanppt", 
                       "Meantmax", "Meantmean", "Meantmin", "Meanvpdmax", "Meanvpdmin","Medianppt","Mediantdmean","Mediantmax"
                       ,"Mediantmean","Mediantmin","Medianvpdmax","Medianvpdmin","Sumppt")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  prism_annual <- do.call(rbind.data.frame, prism_annual)
  prism_annual$Category[prism_annual$metric == "Maxppt"] <- "PRISM_Maxppt"
  prism_annual$Category[prism_annual$metric == "Maxtdmean"] <- "PRISM_Maxtdmean"
  prism_annual$Category[prism_annual$metric == "Maxtmax"] <- "PRISM_Maxtmax"
  prism_annual$Category[prism_annual$metric == "Maxtmean"] <- "PRISM_Maxtmean"
  prism_annual$Category[prism_annual$metric == "Maxtmin"] <- "PRISM_Maxtmin"
  prism_annual$Category[prism_annual$metric == "Maxvpdmax"] <- "PRISM_Maxvpdmax"
  prism_annual$Category[prism_annual$metric == "Maxvpdmin"] <- "PRISM_Maxvpdmin"
  prism_annual$Category[prism_annual$metric == "Meanppt"] <- "PRISM_Meanppt"
  prism_annual$Category[prism_annual$metric == "Meantmax"] <- "PRISM_Meantmax"
  prism_annual$Category[prism_annual$metric == "Meantmean"] <- "PRISM_Meantmean"
  prism_annual$Category[prism_annual$metric == "Meantmin"] <- "PRISM_Meantmin"
  prism_annual$Category[prism_annual$metric == "Meanvpdmax"] <- "PRISM_Meanvpdmax"
  prism_annual$Category[prism_annual$metric == "Medianppt"] <- "PRISM_Medianppt"
  prism_annual$Category[prism_annual$metric == "Mediantdmean"] <- "PRISM_Mediantdmean"
  prism_annual$Category[prism_annual$metric == "Mediantmax"] <- "PRISM_Mediantmax"
  prism_annual$Category[prism_annual$metric == "Mediantmean"] <- "PRISM_Mediantmean"
  prism_annual$Category[prism_annual$metric == "Mediantmin"] <- "PRISM_Mediantmin"
  prism_annual$Category[prism_annual$metric == "Medianvpdmax"] <- "PRISM_Medianvpdmax"
  prism_annual$Category[prism_annual$metric == "Medianvpdmin"] <- "PRISM_Medianvpdmin"
  prism_annual$Category[prism_annual$metric == "Sumppt"] <- "PRISM_Sumppt"
  
  
  MerkleLabGIS <- merge(merge(merge(merge(merge(merge(merge(MerkleLabGIS, snodas,all = TRUE), daymet, all = TRUE), snodasAnnual, all = TRUE), daymetAnnual, all = TRUE), dat, all = TRUE), prism_annual, all = TRUE), prism_daily, all = TRUE)

  return(MerkleLabGIS)
}
