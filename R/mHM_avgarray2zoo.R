#' get summarized zoo object for mHM basin. 
#' 
#' \code{mHM_avgarray2zoo} is retrieving a basin summary, depicted by function \code{fun}, and giving back a zoo object.
#' 
#' 
#' @param array data array.
#' @param datetime Date, POSIX or Yearmon object.
#' @param fun function to summarize/aggregate basin.
#' @param na.rm boolean, remove NA
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
#' @export mHM_avgarray2zoo
#'
#'
mHM_avgarray2zoo <- function(array, datetime, fun, na.rm = FALSE)
{
    # convert array to list
    var_list <- lapply(seq(dim(array)[3]), function(x) array[ , , x])
    # also possible with plyr
    # var_list <- plyr::alply(var_array,3)
    
    # apply function over array
    if (na.rm) {
      var_fun <- sapply(X = var_list, FUN = fun, na.rm=TRUE)
    } else {
      var_fun <- sapply(X = var_list, FUN = fun)
    }
    
  
  # create zoo object
  var_zoo <- zoo::zoo(var_fun, datetime)
  
  # return zoo
  return(var_zoo)
}