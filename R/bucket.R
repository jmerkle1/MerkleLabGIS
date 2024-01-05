
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
  
  
  MerkleLabGIS <- merge(merge(merge(merge(merge(MerkleLabGIS, snodas,all = TRUE), daymet, all = TRUE), snodasAnnual, all = TRUE), daymetAnnual, all = TRUE), dat, all = TRUE)

  return(MerkleLabGIS)
}
