#' Calculates the elevation at each point of a series based on a DEM raster
#' @param points_sf a sf object with multiple features with POINT geometry
#' @param rasterDEM a raster object giving elevations
#' @return a sf object with multiple features with POINT geometry and attributes corresponding to curvilinear coordinate (S) and elevation (Z))
#' @export
#' @examples
#' points=st_read(system.file("data/points.shp", package="morphRiver"))
#' rasterDEM=raster(system.file("data/rasterDEM.tif", package="morphRiver"))
#' plot(rasterDEM)
#' plot(points,
#'      add=TRUE, col=1)
#' relev=elevation(points,rasterDEM) %>% points_to_linestring()
#' plot(relev["Z"],
#'      add=TRUE, lwd=3)
#' # Elevation as longitudinal signal
#' library(ggplot2)
#' ggplot(data=relev, aes(x=S,y=Z))+
#'   geom_path()


elevation=function(points_sf,rasterDEM){
  coords=st_coordinates(points_sf) %>% as_tibble()
  data <- points_sf %>%
    mutate(X=coords$X,
           Y=coords$Y) %>%
    mutate(d1=X-lag(X,1),
           d2=Y-lag(Y,1)) %>%
    mutate(S=sqrt(d1^2+d2^2)) %>%
    mutate(S=c(0,S[2:length(S)])) %>%
    mutate(S=cumsum(S)) %>%
    mutate(Z=raster::extract(rasterDEM,  coords, along=TRUE)) %>%
    dplyr::select(-X,-Y, -d1,-d2)
  return(data)
}
