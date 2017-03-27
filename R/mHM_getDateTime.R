#' get date(time) object from netCDF. 
#' 
#' \code{mHM_getDateTime} is retrieving a date(time) object (e.g. \code{\link{chron}}, \code{\link{as.Date}}, \code{\link{as.yearmon}}) from netCDF.
#' 
#' 
#' 
#' @param con connection to netCDF file, see \code{\link{open.nc}}.
#' @param out_unit definition of datetime unit out; 'h'=hour ,'d'=day, 'm'=month.
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
mHM_getDateTime <- function(nc_file, out_unit="d")
{
  con <- RNetCDF::open.nc(nc_file)
  # get time units
  #units <- RNetCDF::att.get.nc(ncfile = con, variable = "time", attribute = "units")
  # get time var
  datetime_int <- RNetCDF::var.get.nc(ncfile = con, variable = "time")  
  
  # get time units
  units <- RNetCDF::att.get.nc(ncfile = con, variable = "time", attribute = "units")
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
  # create chron time series
  if (in_unit == "h") fact <- 24
  if (in_unit == "d") fact <- 1
  ts_chron <- start_chron + datetime_int/fact
  
  # if daily data return Date object
  if (out_unit == "d") ts_date <- as.Date(ts_chron) else
    # if monthly data return YearMon object
    if (out_unit == "m") ts_date <- zoo::as.yearmon(ts_chron) else 
      # stay by hourly
      if (out_unit == "h")  ts_date <- ts_chron else 
        # print error message
        print("ERROR: please give valid date (time) specification for argument unit! h: hourly, d: daily, m: monthly")

  RNetCDF::close.nc(con) 
  
  # return time series date
  return(ts_date)
}