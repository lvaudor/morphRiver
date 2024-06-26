% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basin_width.r
\name{basin_width}
\alias{basin_width}
\title{Calculates the transversal width of the basin immediately surrounding each point of a series}
\usage{
basin_width(points, rasterDEM, radius)
}
\arguments{
\item{points}{a sf object with multiple features with POINT geometry}

\item{rasterDEM}{a raster object giving elevations}
}
\value{
a sf object with multiple features with POINT geometry and attributes corresponding to curvilinear coordinate (S) and curvature (C)
}
\description{
Calculates the transversal width of the basin immediately surrounding each point of a series
}
\examples{
points=st_read(system.file("data/points.shp", package="morphRiver"))
rasterDEM=raster(system.file("data/rasterDEM.tif", package="morphRiver"))

plot(rasterDEM)
plot(basin_width(points,rasterDEM, radius=500),
     add=TRUE, col=4, lwd=3)
}
