# MerkleLabGIS

## About
The Merkle Research Group provides a code repository dedicated to extracting and managing GIS data for Wyoming and the Greater Yellowstone Ecosystem (GYE). We utilize a cloud storage solution to make geospatial data, which is freely available, easily accessible. You can access these data either through our code or by using our [data viewer](https://pathfinder.arcc.uwyo.edu/devise/MerkleLabGIS/WebApp/map.html).
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
The ExtractRAP function processes data from the Rangeland Analysis Platform (RAP) to extract specific metrics for given geographic points and dates.Povide spatial data (XYdata), desired RAP metrics (e.g., Biomass, Cover), a date column name, and a bio year, determining the growing season to run our function. For each year and metric, it fetches the associated RAP raster data set and extracts metric values for the provided points.
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
rap <- ExtractRAP(pts_sf, RAPmetric = "Cover_BareGround", datesname = "dates", )
```
### Extract Annual SNODAS
The Extract Annual SNODAS function processes data from SNODAS
to extract specific annual metrics for given geographic points and dates. Provide point spatial data
desired SNODAS metrics (e.g., MaxSnowDepth, MaxSWE, MeanSnowDepth), and a date range to run our SNODAS function. The function extracts metric values for the provided points.
```
#Sample data
points <- data.frame(id = c(1, 2), x = c(-108.36312, -109.36312),  y = c(45.54731, 45.54731)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
MaxSnowDepth <- ExtractAnnualSNODAS(points, start_date = "2005-01-01", 
                        end_date = "2010-01-01", metric_name = "MaxSnowDepth")
```

### Extract Daily SNODAS
The Extract Daily SNODAS function processes data from SNODAS and extracts specific daily metrics for given geographic points and dates. Provide point spatial data, a column representing date as POSIXct, 
desired SNODAS metrics (SWE or SnowDepth), and a date range to run our daily SNODAS function. The function extracts metric values for the provided points.

```
#Sample data

pts <- data.frame(
  long = runif(5, -111, -104),
  lat = runif(5, 41, 45),
  dates = as.POSIXct(sample(seq(as.Date('2010-01-01'), as.Date('2022-12-31'), by="day"), 5, replace = TRUE))
)
pts_sf <- st_as_sf(pts, coords = c("long", "lat"), crs = 4326, agr = "constant")


result <- ExtractDailySNODAS(XYdata = pts_sf,
                             start_date = "2005-01-01",
                             end_date = "2023-01-01",
                             datesname = "dates",
                             Metrics = c("SWE", "SnowDepth"))  

```


### Extract DAYMET
The Extract DAYMET function processes data from DAYMET to extract specific metrics for given geographic points and dates. Provide point spatial data, desired DAYMET metrics ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin"), and a date range to run the DAYMET function.
```
#Sample data
points <- data.frame(id = c(1, 2), x = c(-108.36312, -109.36312),  y = c(45.54731, 45.54731)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
maxprcp<- ExtractDAYMET(points, start_date = "2005-01-01", 
                        end_date = "2010-01-01", metric_name = "Maxprcp")
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
