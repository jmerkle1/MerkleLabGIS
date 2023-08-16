
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
  
  #Get SNODAS Data
  snodas<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/SnodasApi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2005-01-01"),
           EndDate = jsonlite::unbox(current_date),
           Metrics = c("SWE", "SnowDepth")),
      auto_unbox = FALSE
    )
  ) %>%
    content()
  snodas <- do.call(rbind.data.frame, snodas)
  snodas$Category[snodas$metric == "SnowDepth"] <- "Snodas_Snowdepth"
  snodas$Category[snodas$metric == "SWE"] <- "Snodas_SWE"
  
  
  #Get Daymet Data
  daymet<- httr::POST(
    "https://devise.uwyo.edu/Umbraco/api/DaymetApi/GetData",
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(StartDate = jsonlite::unbox("2005-01-01"),
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
  
  
  MerkleLabGIS <- merge(merge(MerkleLabGIS, snodas,all = TRUE), daymet, all = TRUE)

  return(MerkleLabGIS)
}

