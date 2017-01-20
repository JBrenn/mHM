#' Simple extrapolation of netCDF in invalid locations. 
#' 
#' \code{mHM_extrapolNetCDF} uses mHM_extrapolMat to extrapolate its matrix values in invalid location, e.g. originating from 
#' meteorological input fields for mHM (netCDF format).
#' 
#' Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning
#' no meteorological input available for whole simulation domain will be printed. 
#' This function extrapolates grids in invalid locations within a radius given by \code{ext}.
#' Extrapolation is performed by a mean value of the surrounding cells of the invalid location.
#' 
#' @param netCDF netCDF file name, full path.
#' @param var name of variable to extrapolate.
#' @param ext radius within grid cells are extended, starting from valid values.
#' 
#' @return netCDF file.
#' 
#' @examples
#' # read netcdf file minimum air temperature
#' pkg_path <- path.package("mHMr")
#' tmin_netcdf <- file.path(pkg_path, "data", "tmin.nc")
#' 
#' tmin_op <-  RNetCDF::open.nc(tmin_netcdf)
#' tmin <- RNetCDF::var.get.nc(tmin_op,"tmin")
#' image(tmin[,,1], zlim = c(2.5,4.5))
#' 
#' tmin_ext <- mHM_tmin_netcdf(netCDF = tmin_netcdf, vat = "tmin", ext = 1)
#' image(tmin_ext[,,1], zlim = c(2.5,4.5))
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mHM_extrapolNetCDF
#' 

mHM_extrapolNetCDF <- function(netCDF, var, ext)
{
  # open connection to netCDF file
  nccon <- RNetCDF::open.nc(netCDF, write = TRUE)
  
  # read in variable from connection
  ncvar <- RNetCDF::var.get.nc(nccon,var)
  
  # extrapolate for each layer
  for (i in 1:dim(ncvar)[3])
  {
    ncvar[,,i] <- mHMr::mHM_extrapolMat(ncvar[,,i], ext=ext)
  }
  
  # write back matrix to con
  RNetCDF::var.put.nc(ncfile = nccon, variable = var, data = ncvar)
  
  # close connection
  RNetCDF::close.nc(nccon)
  
  # return matrix
  return(ncvar)
}




