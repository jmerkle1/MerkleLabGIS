% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_git2rdata.R
\name{ExtractDAYMET}
\alias{ExtractDAYMET}
\title{Extract DAYMET Metrics}
\usage{
ExtractDAYMET <- function(point_data, 
                                start_date = "2005-01-01", 
                                end_date = "2010-01-01", 
                                metric_name = "Maxprcp")
}
\arguments{
\item{point_data}{sf point object}

\item{metric_name}{Character vector of DAYMET metrics to use ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin")}

\item{start_date}{Start date }

\item{end_date}{End date}

}

\description{
The Extract DAYMET function processes data from DAYMET to extract specific metrics for given geographic points and dates.Users provide point spatial data, desired DAYMET metrics ("Maxprcp","Maxswe","Maxtmax","Maxtmin","Meanprcp","Meanswe","Meantmax","Meantmin","Medianprcp","Medianswe","Mediantmax","Mediantmin"), and a date range. It extracts metric values for the provided points. 
}
\examples{
#Sample data
points <- data.frame(id = c(1, 2), x = c(-108.36312, -109.36312),  y = c(45.54731, 45.54731)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
maxprcp<- ExtractDAYMET(points, start_date = "2005-01-01", 
                        end_date = "2010-01-01", metric_name = "Maxprcp")
}

\concept{internal}