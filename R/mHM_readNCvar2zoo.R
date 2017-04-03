#' read netCDF variable to zoo. 
#' 
#' \code{mHM_readNCvar2zoo} is reading time series data of a specific variable from netCDF. Use function only for data set with a single valid grid cell.
#' 
#' 
#' @param ncfile character, netCDF file path.
#' @param var character, variable name.
#' 
#' @return array
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#'  
#' @export mHM_readNCvar2zoo
#' 
#' 
mHM_readNCvar2zoo <- function(ncfile, var, out_unit="d") {
  # open connection to netCDF file
  nc_con <- RNetCDF::open.nc(ncfile)
  # retrieve variable from connection
  var_data <- RNetCDF::var.get.nc(ncfile = nc_con, variable = var)
  # close connection
  RNetCDF::close.nc(nc_con)
  # mask data if x,y dim > 1
  if (!is.na(dim(var_data)[2]))
    var_data <- mHMr::mHM_maskNCvar(var_data)
  # retrieve datetime
  datetime <- mHMr::mHM_getDateTime(nc_file = ncfile, out_unit = out_unit)
  # create zoo object
  var_data_zoo <- zoo::zoo(var_data, datetime)
  # retrun array
  return(var_data_zoo)
}