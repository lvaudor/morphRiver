#' Calculates the curvature at each point of a series
#' @param points_sf a sf object with multiple features with POINT geometry
#' @return a sf object with multiple features with POINT geometry and attributes corresponding to curvilinear coordinate (S) and curvature (C)
#' @export
#' @examples
#' plot(rasterDEM)
#'plot(points,
#'     add=TRUE, col=1)
#'plot(points_to_linestring(points),
#'     add=TRUE, col=2)

points_to_linestring=function(points_sf){
  coords=st_coordinates(points_sf) %>% as_tibble()
  point_data=points_sf %>%
    mutate(X=coords$X,
           Y=coords$Y) %>%
    mutate(Xb=X+(lead(X,1)-X)/2,
           Yb=Y+(lead(Y,1)-Y)/2) %>%
    mutate(Xa=lag(Xb,1),
           Ya=lag(Yb,1)) %>%
    mutate(id=1:nrow(points_sf)) %>%
    slice(2:(nrow(points_sf)-1)) %>%
    group_by(id) %>%
    tidyr::nest() %>%
    pull(data) %>%
    purrr::map(~paste0("LINESTRING(",
                       .$Xa," ",.$Ya,",",
                       .$X," ",.$Y,",",
                       .$Xb," ",.$Yb,")")) %>%
    unlist()
  result <- points_sf %>%
    slice(2:(nrow(points_sf)-1)) %>%
    as_tibble() %>%
    dplyr::select(-geometry)
  result$geom=st_as_sfc(point_data)
  result=st_as_sf(result)
  return(result)
}
