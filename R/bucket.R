
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

  MerkleLabGIS <- GET("https://wildlifemovetools.org/api/layersmetadata")
  MerkleLabGIS <- fromJSON(rawToChar(MerkleLabGIS$content))  
  
  return(MerkleLabGIS)
}
