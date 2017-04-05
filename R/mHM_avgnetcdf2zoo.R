#' get summarized zoo object for mHM basin. 
#' 
#' \code{mHM_avg2zoo} is retrieving a basin summary and giving back a zoo object.
#' 
#' 
#' @param ncfile mHM netCDF output file.
#' @param var variable name.
#' @param fun function to summarize/aggregate basin.
#' @param out_unit definition of datetime unit out; 'h'=hour ,'d'=day, 'm'=month.
#' 
#' @return Zoo object.
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso \code{\link{mHM_getDateTime}}
#' 
#' @keywords
#'  
#' @export mHM_avgnetcdf2zoo
#'
#'
mHM_avgnetcdf2zoo <- function(ncfile, var, fun, out_unit = "d")
{
  con <- RNetCDF::open.nc(ncfile)
  
  # dummy
  var_fun <- c()
  # loop over variable names
  for (i in var)
  {
    # read variable from connection
    var_array <- RNetCDF::var.get.nc(ncfile = con, variable = i)
    
    # convert array to list
    var_list <- lapply(seq(dim(var_array)[3]), function(x) var_array[ , , x])
    # also possible with plyr
    # var_list <- plyr::alply(var_array,3)
    
    # apply function over array
    var_fun_ <- sapply(X = var_list, FUN = fun, na.rm=TRUE)
    var_fun <- cbind(var_fun, var_fun_)
  }
  # name array var_fun by variable names
  colnames(var_fun) <- var
  # get datetime
  datetime <- mHMr::mHM_getDateTime(nc_file = ncfile, out_unit = out_unit)
  # create zoo object
  var_zoo <- zoo::zoo(var_fun, datetime)
  
  RNetCDF::close.nc(con)
  
  # return zoo
  return(var_zoo)
}