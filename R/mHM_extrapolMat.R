#' @title Simple extrapolation of 1D-arrays in invalid/border locations. 
#' @description \code{mHM_extrapolMat} extrapolates matrix values in invalid and border locations, e.g. originating from 
#' meteorological input fields for mHM (netCDF format).
#' 
#' @details Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning
#' no meteorological input available for whole simulation domain will be printed. 
#' This function extrapolates grids in invalid locations within a radius given by the \code{ext} argument.
#' Extrapolation is performed by a mean value of the surrounding cells of the invalid location.
#' 
#' @param data matrix.
#' @param ext radius within grid cells are extended, starting from valid values.
#' 
#' @return matrix.
#' 
#' @examples
#' # read netcdf file minimum air temperature
#' pkg_path <- path.package("mHMr")
#' tmin_netcdf <- file.path(pkg_path, "data", "tmin.nc")
#' tmin_op <-  RNetCDF::open.nc(tmin_netcdf)
#' tmin <- RNetCDF::var.get.nc(tmin_op,"tmin")
#' image(tmin[,,1], zlim = c(2.5,4.5))
#' 
#' tmin1 <- mHM_extrapolMat(tmin[,,1], ext=1)
#' image(tmin1, zlim = c(2.5,4.5))
#' 
#' tmin2 <- mHM_extrapolMat(tmin[,,1], ext=2)
#' image(tmin2, zlim = c(2.5,4.5))
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso \code{\link[mHMr]{mHM_extrapolNetCDF}}
#' 
#' @keywords
#' 
#' @export mHM_extrapolMat
#' @importFrom 

mHM_extrapolMat <- function(data, ext)
{
  for (i in 1:ext)
  {
    # get invalid locations in array
    arrTind <- which(is.na(data), arr.ind = T)
    # apply function over these locations by row
    corr <- apply(X = arrTind, MARGIN = 1, function(x, data) {
      # define rows and colums to use for aggregation
      rowsl <- x[1]-1; rowsu <- x[1]+1
      if (rowsl < 1) rowsl <- 1
      if (rowsu > dim(data)[1]) rowsu <- dim(data)[1]
      rows <- (rowsl):(rowsu)
      
      colsl <- x[2]-1; colsu <- x[2]+1
      if (colsl < 1) colsl <- 1
      if (colsu > dim(data)[2]) colsu <- dim(data)[2]
      cols <- (colsl):(colsu)
      
      # if there are only NAs within choosen values set new value to NA
      # otherwise take mean value
      if (any(!is.na(data[rows,cols]))) {
        y <- mean(data[rows,cols], na.rm=T)
      } else {
        y <- NA
      }
      return(y)
    }, data=data)
    
    # update values in data matrix
    for (i in 1:dim(arrTind)[1]) data[arrTind[i,1],arrTind[i,2]] <- corr[i]
  }
  # give back matrix
  return(data)
}