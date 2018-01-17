#' @title prolonge netCDF climatology
#' @description prolongue netCDF (daily) climatology for specific time period given by netCDF file
#' @param r raster file to be prolonged (climatology)
#' @param nc_time netCDF file, will be copied to nc_out
#' @param nc_time_name variable name of netCDF file nc_time
#' @param nc_out netCDF file, prolonged data will be set in
#' @param nc_out_name variable name of netCDF file nc_out
#' @param standard_name standard name in nc_out
#' @param long_name long name in nc_out
#' @param units units in nc_out
#' @return nc_out
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[mHMr]{mHM_getDateTime}}
#'  \code{\link[raster]{values}}
#'  \code{\link[RNetCDF]{open.nc}},\code{\link[RNetCDF]{var.put.nc}},\code{\link[RNetCDF]{var.rename.nc}},\code{\link[RNetCDF]{att.put.nc}},\code{\link[RNetCDF]{close.nc}}
#' @rdname mHM_prolongRintime
#' @export mHM_prolongRintime
#' @importFrom raster values
#' @importFrom RNetCDF open.nc var.put.nc var.rename.nc att.put.nc close.nc

mHM_prolongRintime <- function(r, nc_time, nc_time_name, nc_out, nc_out_name,
  standard_name, long_name, units)
{
  # get time from netCDF
  date <- mHMr::mHM_getDateTime(nc_file = nc_time, out_unit = "d")
  
  # dummy array
  dummy <- array(NA, dim = c(r@ncols, r@nrows, length(date)))
  
  # fill dummy with raster object
  for (i in 1:length(date)) {
    pl = i - floor(x = i/dim(r)[3]) * dim(r)[3]
    if (pl==0) pl = dim(r)[3]
    dummy[,,i] <- matrix(data = raster::values(r)[,pl], byrow = F, nrow = r@nrows, ncol = r@ncols)
  }
  
  # copy nc
  file.copy(nc_time, nc_out)
  
  # open nc_out for writing
  con <- RNetCDF::open.nc(con = nc_out, write = TRUE)
  
  # write data in netCDF
  RNetCDF::var.put.nc(ncfile = con, variable = var_out, data = dummy, start = c(1,1,1), count = dim(dummy))
  
  # change variable name
  RNetCDF::var.rename.nc(ncfile = con, variable = nc_time_name, newname = nc_out_name)
  # put attributes correctly
  RNetCDF::att.put.nc(ncfile = con, variable = nc_out_name, name = "standard_name", type = "NC_CHAR", value = standard_name)
  RNetCDF::att.put.nc(ncfile = con, variable = nc_out_name, name = "long_name", type = "NC_CHAR", value = long_name)
  RNetCDF::att.put.nc(ncfile = con, variable = nc_out_name, name = "units", type = "NC_CHAR", value = units)
  
  # close nc_out
  RNetCDF::close.nc(con = con)
}