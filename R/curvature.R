#' Calculates the curvature at each point of a series
#' @param points a sf object with multiple features with POINT geometry
#' @return a sf object with multiple features with POINT geometry and attributes corresponding to curvilinear coordinate (S) and curvature (C)
#' @export
#' @examples
#' points=st_read(system.file("data/points.shp", package="morphRiver"))
#' rasterDEM=raster(system.file("data/rasterDEM.tif", package="morphRiver"))
#' plot(rasterDEM)
#' plot(points,
#'      add=TRUE, col=1)
#' rcurv=curvature(points) %>% points_to_linestring()
#' plot(rcurv["C"],
#'     add=TRUE, lwd=3)
#'
#' # Curvature as longitudinal signal
#' library(ggplot2)
#' ggplot(data=rcurv, aes(x=S,y=C))+
#'   geom_path()

curvature=function(points){
    coords=st_coordinates(points) %>% as_tibble()
    result <- points   %>%
      mutate(X=coords$X,
             Y=coords$Y) %>%
      mutate(d1=X-lag(X,1),
             d2=Y-lag(Y,1)) %>%
      mutate(S=sqrt(d1^2+d2^2)) %>%
      mutate(S=c(0,S[2:length(S)])) %>%
      mutate(S=cumsum(S)) %>%
      mutate(ratio=d2/d1) %>%
      mutate(phi=atan(ratio)) %>%
      mutate(phi=case_when(d1<0 & d2>=0 ~ phi+pi,
                           d1<0 & d2<0  ~ phi-pi,
                           d1>0 ~ phi,
                           d1==0 & d2>0 ~ pi/2,
                           d1==0 & d2<0 ~ pi/2,
                           d1==0 & d2==0 ~ 0)) %>%
      mutate(phi1=phi,
             phi2=lead(phi,1)) %>%
      mutate(change=sign(phi1)!=sign(phi2)) %>%
      mutate(cas1=abs(phi2-phi1),
             cas2=abs(phi1)+abs(phi2)) %>%
      mutate(compl_cas2=2*pi-cas2) %>%
      mutate(C=case_when(change & cas2 > compl_cas2 ~ compl_cas2,
                         change & cas2<= compl_cas2 ~ cas2,
                         !change ~cas1)) %>%
      dplyr::select(S,C)
  return(result)
}
