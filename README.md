# MerkleLabGIS

## About
The Merkle Research Group provides a code repository dedicated to extracting and managing GIS data for Wyoming and the Greater Yellowstone Ecosystem (GYE). We utilize a cloud storage solution to make geospatial data, which is freely available, easily accessible. You can access these data either through our code or by using our [data viewer](https://wildlifemovetools.org/gis).
### Pathfinder
Pathfinder is our chosen storage solution, facilitating a cloud-like presence powered by ARCC. At its core, it provides services such as onsite backups, data sharing, and collaboration. Pathfinder employs the Simple Storage Service (S3) protocol, initially devised by Amazon. The object storage in S3 operates via a service known as Ceph, a feature of Red Hat Enterprise Linux.

### Cloud Optimized GeoTiffs
COGs are geospatial raster files tailored for cloud environments. Their design ensures efficient access and storage, allowing users to fetch specific portions of raster data without the need to download the entire file.

For those interested in reading COGs in R via the terra package, the code snippet below will prove helpful. By prefixing the URL with /vsicurl/, GDAL interprets the remote file as a local one. This means you can read and process the data without downloading it explicitly:
```
 library(terra)
                #Change to the raster url you want to read in. 
                cog <- rast("/vsicurl/https://pathfinder.arcc.uwyo.edu/devise/MerkleLabGIS/Landcover_NLCD/NLCD_2008_Landcover.tif") 
                plot(cog)
```
## Functions
To maximize utility, we offer multiple ways to interact with our raster and vector data. This repository houses several extraction functions designed to streamline the use of our GIS data.
### Read all available GIS Data
Read all geospatial layers in the S3 bucket using bucket(). After running the bucket() function, it returns a dataframe of the URLS and other metadata for each geospatial layer.
```
library(devtools)
devtools::install_github("jmerkle1/MerkleLabGIS")
library(MerkleLabGIS)
dt <- bucket()
```

### Extract Rangleland Analysis Platform Data
The ExtractRAP function is designed to extract spatially and temporally explicit Rangeland Analysis Platform (RAP) data for given geographic points and dates. To use this function, you need to provide the following inputs:

1. **XYdata**: Spatial data representing the geographic points for which you want to extract RAP metrics.
2. **RAPmetric**: Specify the metrics you are interested in (e.g., Biomass_AnnualForbsGrasses,Biomass_PerennialForbsGrasses, Cover_AnnualForbsGrasses,Cover_BareGround,Cover_Litter,Cover_PerennialForbsGrasses,Cover_Shrubs,Cover_Trees).
3. **datesname**: Indicate the name of the column representing date as POSIXct.
4. **bio_year_start**: This parameter determines the growing season for which you want to run the function. This sets the beginning of the growing season and should be specified as a Julian day (an integer between 1 and 365). If not specified, the default value is 150, corresponding to May 30th. Any dates before the "bio_year_start" will result in the extraction of RAP metrics for the growing season of the prior year.
5. **maxcpus**: Number of cores to use for parallel processing. If no value is defined, the function will auto detect how many cores are available on your machine. 
```
#Sample data
set.seed(42) 

# Create data frame
pts <- data.frame(
  long = runif(5, -111, -104),
  lat = runif(5, 41, 45),
  dates = as.POSIXct(sample(seq(as.Date('2010/01/01'), as.Date('2022/12/31'), by="day"), 5, replace = TRUE), tz = "UTC")
)
pts_sf <- st_as_sf(pts, coords = c("long", "lat"), crs = 4326, agr = "constant")
rap <- ExtractRAP(pts_sf, RAPmetric = "Cover_BareGround", datesname = "dates",bio_year_start = 150, maxcpus = 10 )
```
### Extract Annual SNODAS
The Extract Annual SNODAS function processes data from SNODAS
to extract specific annual metrics for given geographic points and dates.To use this function, you need to provide the following inputs:

1. **point_data**: Spatial data representing the geographic points for which you want to extract daily SNODAS metrics.
2. **start_date**: This is the beginning of the date range from which you want to start extracting the metric.
3. **end_date**: This indicates the end of the date range up to which you want to extract the metric.
4. **metric_name**: Specify the metrics you are interested in (e.g., "snowdepth", "snowdepth-accum", "swe", "snowdays", "snowmelt").


```
#Sample data
points <- data.frame(id = c(1, 2), x = c(-108.36312, -109.36312),  y = c(45.54731, 45.54731)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
MaxSnowDepth <- ExtractAnnualSNODAS(point_data = points, start_date = "2005-01-01", 
                        end_date = "2010-01-01", metric_name = "snowdepth")
```

### Extract Daily SNODAS
The Extract Daily SNODAS function processes data from SNODAS and extracts specific daily metrics for given geographic points and dates. To use this function, you need to provide the following inputs:

1. **XYdata**: Spatial data representing the geographic points for which you want to extract daily SNODAS metrics.
3. **datesname**: Indicate the name of the column representing date as POSIXct.
3. **Metrics**: Specify the metrics you are interested in ("snowdepth", "snowdepth-accum", "swe", "snowdays", "snowmelt") .
4. **num_cores**: Number of cores to use for parallel processing. If no value is defined, the function will auto detect how many cores are available on your machine. 

```
#Sample data

pts <- data.frame(
  long = runif(5, -111, -104),
  lat = runif(5, 41, 45),
  dates = as.POSIXct(sample(seq(as.Date('2010-01-01'), as.Date('2022-12-31'), by="day"), 5, replace = TRUE))
)
pts_sf <- st_as_sf(pts, coords = c("long", "lat"), crs = 4326, agr = "constant")


result <- ExtractDailySNODAS(XYdata = pts_sf,
                             datesname = "dates",
                             Metrics = c("snowdepth"), 
                             num_cores = 10)  

```


### Extract Annual DAYMET
The Extract Annual DAYMET function processes data from DAYMET to extract specific metrics for given geographic points and dates.To use this function, you need to provide the following inputs:

1. **point_data**: Spatial data representing the geographic points for which you want to extract daily DAYMET metrics.
2. **start_date**: This is the beginning of the date range from which you want to start extracting the metric.
3. **end_date**: This indicates the end of the date range up to which you want to extract the metric.
4. **metric_name**: Specify the metrics you are interested in ("prcp", "swe", "tmax", "tmin").

```
#Sample data
points <- data.frame(id = c(1, 2), x = c(-108.36312, -109.36312),  y = c(45.54731, 45.54731)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
maxprcp<- ExtractDAYMET(point_data= points, start_date = "2005-01-01", 
                        end_date = "2010-01-01", metric_name = "prcp")
```

### Extract Daily DAYMET
The Extract Daily DAYMET function processes data from DAYMET and extracts specific daily metrics for given geographic points and dates.To use this function, you need to provide the following inputs:

1. **XYdata**: Spatial data representing the geographic points for which you want to extract daily DAYMET metrics.
2. **datesname**: Indicate the name of the column representing date as POSIXct.
3. **Metrics**: Specify the metrics you are interested in ("prcp", "swe", "tmax", "tmin") .
4. **num_cores**: Number of cores to use for parallel processing. If no value is defined, the function will auto detect how many cores are available on your machine. 

```
#Sample data

pts <- data.frame(
  long = runif(5, -111, -104),
  lat = runif(5, 41, 45),
  dates = as.POSIXct(sample(seq(as.Date('2010-01-01'), as.Date('2022-12-31'), by="day"), 5, replace = TRUE))
)
pts_sf <- st_as_sf(pts, coords = c("long", "lat"), crs = 4326, agr = "constant")


result <- ExtractDailyDAYMET(XYdata = pts_sf,
                             datesname = "dates",
                             Metrics = c("prcp", "swe", "tmax", "tmin"), 
                             num_cores = 10)  

```
### Extract NDVI
The Extract NDVI function processes data from the MODIS NDVI datasets to extract specific NDVI metrics for given geographic points and dates.To use this function, you need to provide the following inputs:

1. **XYdata**: Spatial data representing the geographic points for which you want to extract NDVI metrics.
2. **datesname**: Indicate the name of the column representing date as POSIXct.
3. **NDVImetric**: Specify the metrics you are interested in ("MaxNDVIDay","MaxIRGday", "SpringStartDay", "SpringEndDay","MaxBrownDownDay",
                           "NDVI_scaled","IRG_scaled","SpringSE", "SpringLength","sumIRG","SpringScale").
4. **maxcpus**: Number of cores to use for parallel processing. 

```
#Sample data

df <- data.frame(
  longitude = c(-107.7219, -107.6194, -105.6952, -105.9229, -107.0843),
  latitude = c(43.83566, 41.94561, 42.81093, 43.73319, 43.07281),
  random_date = as.POSIXct(c("2006-01-14 17:16:11", "2016-07-21 08:12:33", "2008-08-02 21:28:18", 
                             "2018-07-17 18:50:37", "2019-10-01 10:13:03"))
)

# Convert dataframe to sf object
sf_obj <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

result <- ExtractNDVI(XYdata = sf_obj, NDVImetric = c( "MaxNDVIDay", "MaxBrownDownDay"), datesname = "random_date", maxcpus = 6)

```

### Crop Rasters
Crop a list of rasters to a study area from the Merkle Research Group database. Cropped rasters are saved to the local computer.
```
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

```
