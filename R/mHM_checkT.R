#' @title Check minimum and maximum air temperature for consistency in NetCDF.
#' @description \code{mHM_checkT} proves if minimum air temperature is lower than maximum air temperature. 
#' If it is not the case values are interchanged.
#' @details Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning Tmax smaller than Tmin (or wise versa) will be printed. 
#' This function makes consistant minmum and maximum air temperature fields in netCDF format.
#' 
#' @param nc.tmin netCDF of minimum air temperature.
#' @param nc.tmax netCDF of maximum air temperature.
#' 
#' @return re-write input netCDF files with consistent air temperature extremes.
#' 
#' @examples
#' pkg_path <- path.package("mHMr")
#' tmin_netcdf <- file.path(pkg_path, "inst/ext_data", "tmin.nc")
#' tmax_netcdf <- file.path(pkg_path, "inst/ext_data", "tmax.nc")
#' 
#' mHM_checkT(nc.tmin = tmin_netcdf, nc.tmax = tmax_netcdf)
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mHM_checkT
#' @importFrom RNetCDF open.nc var.get.nc var.put.nc close.nc

mHM_checkT <- function(nc.tmin, nc.tmax)
{
  # read netcdf files 
  # minimum air temperature
  tmin_op <-  RNetCDF::open.nc(nc.tmin, write = TRUE)
  tmin <- RNetCDF::var.get.nc(tmin_op,"tmin")
  
  # maximum air temperature
  tmax_op <-  RNetCDF::open.nc(nc.tmax, write = TRUE)
  tmax <- RNetCDF::var.get.nc(tmax_op,"tmax")
  
  # if more than 0 tmax smaller than tmin
  if (any(tmax < tmin, na.rm=TRUE)) {
    # if tmax < tmin or tmin > tmax interchange values
    tmax_check <- ifelse(tmax < tmin, tmin, tmax)
    tmin_check <- ifelse(tmin > tmax, tmax, tmin)
    # give back values to netCDF object
    RNetCDF::var.put.nc(tmax_op, "tmax", tmax_check)
    RNetCDF::var.put.nc(tmin_op, "tmin", tmin_check)
  } else {
    # netCDF files are consistent
    print("maximum air temperature higher than minimum air temperature for whole field. Nothing to do.")
  }
  # close open connections
  RNetCDF::close.nc(tmax_op)
  RNetCDF::close.nc(tmin_op)
}


