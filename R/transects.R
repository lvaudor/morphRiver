#' Calculates the perpendicular transects to a series of points
#' @param points a sf object with multiple features with POINT geometry
#' @param radius the radius to consider for the transect
#' @return a sf object with multiple features with LINESTRING geometry, corresponding to transects
#' @export
#' @examples
#' points=st_read(system.file("data/points.shp", package="morphRiver"))
#' rasterDEM=raster(system.file("data/rasterDEM.tif", package="morphRiver"))
#'
#' plot(rasterDEM)
#' plot(points,
#'      add=TRUE, col=1)
#' plot(points_to_linestring(points),
#'      add=TRUE, col=2)
#' plot(transects(points,500),
#'      add=TRUE, col=4)

transects=function(points, radius){
    qs=function(A,B,C){
      solns=tibble(soln1=as.complex(NA),soln2=as.complex(NA))
      if(!is.na(A)&!is.na(B)&!is.na(C)){
        A=as.complex(A); B=as.complex(B); C=as.complex(C)
        i2=(A!=0); i1=((A==0)&(B!=0));
        a2=A[i2]; b2=B[i2]; c2=C[i2]
        solns[i2,1]=(-b2 + sqrt(b2^2 - 4*a2*c2))/(2*a2)
        solns[i2,2]=(-b2 - sqrt(b2^2 - 4*a2*c2))/(2*a2)
        b1=B[i1]; c1=C[i1]
        solns[i1,1]=(-c1)/b1
      }
      return(solns)
    }
    coords=st_coordinates(points) %>% as_tibble()
    result <- points %>%
      mutate(X=coords$X,
             Y=coords$Y) %>%
      mutate(xA=lag(X,1),
             yA=lag(Y,1),
             xB=X,
             yB=Y,
             xC=lead(X,1),
             yC=lead(Y,1)) %>%
      mutate(deltaY=yC-yA,
             deltaX=xC-xA) %>%
      mutate(gamma=deltaY/deltaX) %>%
      mutate(alpha=-1/gamma) %>%
      mutate(beta=yB-alpha*xB,
             R2=radius^2,
             a=1+alpha^2,
             b=-2*xB+2*alpha*(beta-yB),
             c=xB^2+(beta-yB)^2-R2)
    solns=purrr::pmap(list(A=result$a,B=result$b,C=result$c),qs) %>%
      bind_rows()

    result=bind_cols(result,solns) %>%
      mutate(xP1=Re(soln1),
             xP2=Re(soln2)) %>%
      mutate(yP1=alpha*xP1+beta,
             yP2=alpha*xP2+beta) %>%
      slice(2:(n()-1))
    result=result %>%
      mutate(geom=paste0("LINESTRING(",
                             .$xP1," ",.$yP1,",",
                             .$xP2," ",.$yP2, ")"))
    geom=result$geom

    transects=as_tibble(points) %>%
      slice(2:(n()-1))
    transects$geometry=st_as_sfc(geom)
    transects=st_as_sf(transects)
    return(transects)
}
