
#' Retrieve Available Spatial Layers Metadata
#'
#' This function queries the Wildlife Move Tools API to obtain metadata for 
#' all available spatial layers in the Merkle Lab GIS system. The metadata 
#' includes details about each spatial layer, such as its name, description, 
#' type, and other relevant attributes.
#' #'
#' 
#' @return A data frame containing metadata fora ll available spatial layers
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @export
#'
#' @examples
#' bucket()
bucket <- function() {

  MerkleLabGIS <- GET("https://wildlifemovetools.org/api/layersmetadata")
  MerkleLabGIS <- fromJSON(rawToChar(MerkleLabGIS$content))  
  
  return(MerkleLabGIS)
}
