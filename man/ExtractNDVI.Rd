% Generated by roxygen2: do not edit by hand
\name{ExtractNDVI}
\alias{ExtractNDVI}
\title{Extract NDVI Metrics}
\usage{
ExtractNDVI <- function(XYdata, 
                                NDVImetric = c( "MaxNDVIDay", "MaxBrownDownDate"),
                                datesname = "date",
                                maxcpus = 2)
                                

}
\arguments{
\item{XYdata}{sf point object}

\item{NDVImetric}{Character vector of NDVI metrics to use ("MaxNDVIDay","MaxIRGday", "SpringStartDay", "SpringEndDay","IntegratedNDVI","MaxBrownDownDate","NDVI_scaled","IRG_scaled","SE_springDLCfit", "SpringLength","sumIRG","SpringScale" )}

\item{datesname}{Name of the column in XYdata representing date as POSIXct. }

\item{maxcpus}{Specifies how many cores to use for parallel processing.}

}

\description{
The ExtractNDVI function processes data from the MODIS NDVI datasets to extract specific NDVI metrics for given geographic points and dates. Provide spatial data (XYdata), desired NDVI metrics (e.g., MaxNDVIDay, MaxIRGday), and a date column name. For each year and metric, it fetches the associated NDVI raster dataset 
and extracts metric values for the provided points.
}
\examples{
# Create dataframe
df <- data.frame(
  longitude = c(-107.7219, -107.6194, -105.6952, -105.9229, -107.0843),
  latitude = c(43.83566, 41.94561, 42.81093, 43.73319, 43.07281),
  random_date = as.POSIXct(c("2006-01-14 17:16:11", "2016-07-21 08:12:33", "2008-08-02 21:28:18", 
                             "2018-07-17 18:50:37", "2019-10-01 10:13:03"))
)

# Convert dataframe to sf object
sf_obj <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

result <- ExtractNDVI(XYdata = sf_obj, NDVImetric = c( "MaxNDVIDay", "MaxBrownDownDate"), datesname = "random_date", maxcpus = 6)


}

\concept{internal}