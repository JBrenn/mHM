#' impute timeseries in netCDF (at site). 
#' 
#' \code{mHM_ts2existNC} takes an existing netCDF file at a single site and feeds it with new time series data.
#' 
#' @param nc_file connection to netCDF file, see \code{\link{open.nc}}.
#' @param ts zoo object, time series data
#' 
#' @return netCDF file, new time series data imputed.
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
#' @export mHM_ts2existNC
#'
#'
mHM_ts2existNC <- function(nc_file, var, ts)
{
  # open netCDF connection
  nc_con <- RNetCDF::open.nc(nc_file, write = T)
  
  # get time unit
  units <- RNetCDF::att.get.nc(ncfile = nc_con, variable = "time", attribute = "units")
  
  # extract unit
  if (grepl("hours", units)) in_unit <- "h"
  if (grepl("days", units)) in_unit <- "d"
  if (grepl("months", units)) in_unit <- "m"
  
  # start datetime
  dashes_units <- gregexpr("-", units)[[1]]
  start_datetime <- stringr::str_sub(string = units, start = dashes_units[1]-4, end = nchar(units)) 
  # get start date
  tab_start_datetime <- gregexpr(" ", start_datetime)[[1]]
  start_date <- stringr::str_sub(string = start_datetime, start = 1, end = tab_start_datetime[1]-1)
  # get start time
  start_time <- stringr::str_sub(string = start_datetime, start = tab_start_datetime[1]+1, end = nchar(start_datetime))
  # create chron object origin
  start_chron <- chron::chron(dates. = start_date, times. = start_time, format = c(dates="y-m-d", times="h:m:s"))
  
  # time series
  if (in_unit == "d") start_date <- as.Date(start_chron)
  dateintfrom1950 <- as.integer(time(ts)) - as.integer(as.Date("1950-01-01"))
  
  # get time and variable data
  time    <- RNetCDF::var.get.nc(nc_con, "time")
  vardata <- RNetCDF::var.get.nc(nc_con, "pre")
  
  # create zoo object for return
  var_old_zoo <- zoo::zoo(vardata, as.Date(time, origin=start_date))
  # merge time series 
  ts_zoo <- merge(var_old_zoo, ts)
  names(ts_zoo) <- c("old", "new")
  
  # impute NAs of "old"
  coredata(ts_zoo[,"old"])[is.na(coredata(ts_zoo[,"old"]))] <- coredata(ts_zoo[,"new"])[is.na(coredata(ts_zoo[,"old"]))]
  # impute NAs of "new"
  coredata(ts_zoo[,"new"])[is.na(coredata(ts_zoo[,"new"]))] <- coredata(ts_zoo[,"old"])[is.na(coredata(ts_zoo[,"new"]))]
  
  # write back time data
  dateintfrom1950 <- as.numeric(time(ts_zoo)) - as.numeric(as.Date("1950-01-01"))
  RNetCDF::var.put.nc(ncfile = nc_con, variable = "time", data = dateintfrom1950, start = 1, count = length(dateintfrom1950))
  # write back variable data
  RNetCDF::var.put.nc(ncfile = nc_con, variable = var, data = coredata(ts_zoo[,"new"]), start = c(1,1,1), 
                      count = c(1,1,length(coredata(ts_zoo[,"new"]))))

  # close nc connection
  RNetCDF::close.nc(nc_con)
  
  # return
  return(ts_zoo)
}