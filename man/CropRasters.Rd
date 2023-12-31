% Generated by roxygen2: do not edit by hand
\name{CropRasters}
\alias{CropRasters}
\title{Crop Rasters}
\usage{
CropRasters(cog_urls, polygon_sf, output_folder)

}
\arguments{
\item{cog_urls}{List of raster urls}

\item{polygon_sf}{Polygon object of study area}

\item{output_folder}{Output location of raster files}


}

\description{
Crop a list of rasters to a study area. Cropped rasters are stored on the local computer.
}
\examples{
#Sample data
polygon_sf <- st_polygon(list(matrix(c(-111, 45, 
                                            -104, 45, 
                                            -104, 41, 
                                            -111, 41, 
                                            -111, 45), 
                                          ncol=2, byrow=TRUE)))

# Convert to an sf object
polygon_sf <- st_sf(geometry = st_sfc(polygon_sf, crs = 4326))

#List URLS
cog_urls <- c(
  "https://pathfinder.arcc.uwyo.edu/devise/SNODAS/SNODAS_SWE/SNODAS_20120405_SWE.tif",
  "https://pathfinder.arcc.uwyo.edu/devise/SNODAS/SNODAS_SnowDepth/SNODAS_20120218_SnowDepth.tif",
  "https://pathfinder.arcc.uwyo.edu/devise/DAYMET/DAYMET_prcp/DAYMET_v4_20050101_prcp.tif"
)

#Alternatively create a dataframe of urls from the Merkle Research Group Database
dt <- bucket()

unique(dt$Category)

DEM <- dt[dt$Category == "DEM",]

cog_urls <- DEM$url

#Set output folder
output_folder <- "C:/path/to/folder"

CropRasters(cog_urls, polygon_sf, output_folder)

}

\concept{internal}