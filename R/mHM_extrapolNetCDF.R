#' @title Simple extrapolation of netCDF in invalid/border locations. 
#' 
#' @description \code{mHM_extrapolNetCDF} uses mHM_extrapolMat to extrapolate its matrix values in invalid and border location, 
#' e.g. originating from meteorological input fields for mHM (netCDF format).
#' 
#' @details Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning
#' no meteorological input available for whole simulation domain will be printed. 
#' This function extrapolates grids in invalid locations within a radius (number of grid cells) given by \code{ext}.
#' Extrapolation is performed by a mean value of the surrounding cells of the invalid location.
#' 
#' Caution: netCDF input file will be overwritten, backup original file before use.
#' 
#' @param netCDF netCDF file name (full path).
#' @param var character, name of variable to extrapolate.
#' @param ext integer, radius (number of grid cells) within grid cells are extended, starting from border grid cells.
#' @param return boolean, should extrapolated array be returned in console? default=FALSE
#' 
#' @return netCDF file 
#' 
#' @examples
#'# read netcdf file minimum air temperature for Júcar domain
#'  pkg_path <- path.package("mHMr")
#'  tmin_netcdf <- file.path(pkg_path, "data", "tmin.nc")
#'
#'# open connection to netCDF
#'  tmin_op <-  RNetCDF::open.nc(tmin_netcdf)
#'# retrieve min. air temp  
#'  tmin <- RNetCDF::var.get.nc(tmin_op,"tmin")
#'# visualise original field
#'  image(tmin[,,1], zlim = c(2.5,4.5))
#'
#'# extrapolate (radius: 1 grid cell)
#'  tmin_ext <- mHM_tmin_netcdf(netCDF = tmin_netcdf, vat = "tmin", ext = 1)
#'# visualise extrapolated field
#'  image(tmin_ext[,,1], zlim = c(2.5,4.5))
#'
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso \code{\link[mHMr]{mHM_extrapolMat}}
#' 
#' @keywords
#' 
#' @export mHM_extrapolNetCDF
#' @importFrom RNetCDF open.nc var.get.nc var.put.nc close.nc

mHM_extrapolNetCDF <- function(netCDF, var, ext, return=FALSE)
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
  if (return) return(ncvar) else return(NULL)
}




