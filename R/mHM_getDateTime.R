#' get date(time) object from netCDF. 
#' 
#' \code{mHM_getDateTime} is retrieving a date(time) object (e.g. \code{\link{chron}}, \code{\link{as.Date}}, \code{\link{as.yearmon}}) from netCDF.
#' 
#' 
#' 
#' @param con connection to netCDF file, see \code{\link{open.nc}}.
#' 
#' @return Chron datetime, Date or Yearmon object, depending on mHM output definition.
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
#' @export mHM_getDateTime
#'
#'
mHM_getDateTime <- function(con)
{
  # get time units
  units <- RNetCDF::att.get.nc(ncfile = con, variable = "time", attribute = "units")
  # get time var
  datetime_int <- RNetCDF::var.get.nc(ncfile = con, variable = "time")  
  
  # start datetime
  start_datetime <- stringr::str_sub(string = units, start = nchar(units)-18, end = nchar(units)) 
  # get start date
  start_date <- stringr::str_sub(string = start_datetime, start = 1, end = 10)
  # get start time
  start_time <- stringr::str_sub(string = start_datetime, start = 12, end = 19)
  # create chron object origin
  start_chron <- chron::chron(dates. = start_date, times. = start_time, format = c(dates="y-m-d", times="h:m:s"))
  # create chron time series
  ts_chron <- start_chron + datetime_int/24
  
  # if daily data return Date object
  if (unique(diff(ts_chron)) == 1) ts_date <- as.Date(ts_chron)
  
  # if monthly data return YearMon object
  if (mean(diff(ts_chron)) > 29) ts_date <- zoo::as.yearmon(ts_chron)
   
  # return time series date
  return(ts_date)
}