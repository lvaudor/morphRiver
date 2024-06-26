#' Calculates the transversal width of the basin immediately surrounding each point of a series
#' @param points_sf a sf object with multiple features with POINT geometry
#' @param rasterDEM a raster object giving elevations
#' @return a sf object with multiple features with POINT geometry and attributes corresponding to curvilinear coordinate (S) and curvature (C)
#' @export
#' @examples
#' points=st_read(system.file("data/points.shp", package="morphRiver"))
#' rasterDEM=raster(system.file("data/rasterDEM.tif", package="morphRiver"))
#'
#' plot(rasterDEM)
#' plot(basin_width(points,rasterDEM, radius=500),
#'      add=TRUE, col=4, lwd=3)


basin_width=function(points_sf,rasterDEM, radius){

  width_ind=function(data){
      matrixXY=tibble(X=seq(data$X[1],data$X[2],length.out=1000),
                      Y=seq(data$Y[1],data$Y[2],length.out=1000)) %>%
      as.matrix()
      cells=raster::extract(rasterDEM,
                            matrixXY,
                            cellnumbers=T,
                            along=TRUE) %>%
        as_tibble() %>%
        mutate(id=1:n(),
               X=matrixXY[,1],
               Y=matrixXY[,2])
      colnames(cells)=c("cells","values","id","X","Y")
      centerline= cells %>%
        mutate(minvalue=min(values, na.rm=TRUE))%>%
        filter(values==minvalue) %>%
        slice(floor(n()/2):ceiling(n()/2)) %>%
        slice(1)
      cells <- cells %>%
        mutate(side=case_when(id>centerline$id~"R",
                              id<centerline$id~"L")) %>%
        group_by(side) %>%
        mutate(index=case_when(side=="L"~ rev(1:length(side)),
                               side=="R"~ 1:length(side))) %>%
        ungroup() %>%
        mutate(diff = case_when(side=="L" ~ sign(values-lag(values,1)),
                                side=="R" ~ sign(values-lead(values,1)))) %>%
        filter(diff==-1) %>%
        group_by(side) %>%
        filter(index==min(index)) %>%
        mutate(index=index) %>%
        ungroup()

      xylims=cells %>% dplyr::select(X,Y)
      W=sqrt(diff(xylims$X)^2 + diff(xylims$Y)^2)
      geom=paste0("LINESTRING(",xylims[1,1]," ", xylims[1,2],",",xylims[2,1]," ", xylims[2,2],")")
      tibW=tibble(W=W,
                  geom=geom)
      return(tibW)
  }
  tibW <- transects(points_sf, radius)
  widths=tibW %>%
    st_coordinates() %>%
    as_tibble() %>%
    group_by(L1) %>%
    tidyr::nest() %>%
    pull("data") %>%
    purrr::map(width_ind) %>%
    bind_rows()
  widths_sf=widths %>% dplyr::select(-geom) %>%
    mutate(S=tibW$S) %>%
    dplyr::select(S,W)
  widths_sf$geometry=st_as_sfc(widths$geom)
  widths_sf=st_as_sf(widths_sf)
  return(widths_sf)
}
