#' get summarized zoo object for mHM basin. 
#' 
#' \code{mHM_avg2zoo} is retrieving a basin summary, depicted by function \code{fun}, and giving back a zoo object.
#' 
#' 
#' @param con coneection to netCDF.
#' @param var variable name.
#' @param datetime Date, POSIX or Yearmon object.
#' @param fun function to summarize/aggregate basin.
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
#' @export mHM_getDateTime
#'
#'
mHM_avg2zoo <- function(con, var, datetime, fun)
{
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
  # create zoo object
  var_zoo <- zoo::zoo(var_fun, datetime)
  
  # return zoo
  return(var_zoo)
}