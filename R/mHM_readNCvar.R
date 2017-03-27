#' read netCDF variable. 
#' 
#' \code{mHM_readNCvar} is reading data of a specific variable from netCDF.
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
#' @export mHM_readNCvar
#' 
#' 
mHM_readNCvar <- function(ncfile, var) {
  # open connection to netCDF file
  nc_con <- RNetCDF::open.nc(ncfile)
  # retrieve variable from connection
  var_data <- RNetCDF::var.get.nc(ncfile = nc_con, variable = var)
  # close connection
  RNetCDF::close.nc(nc_con)
  # retrun array
  return(var_data)
}