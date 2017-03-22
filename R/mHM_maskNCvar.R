#' mask array.  
#' 
#' \code{mHM_maskNCvar} is masking array of a specific variable from netCDF retrival (mhm_readNCvar)
#' 
#' 
#' @param ncfile character, netCDF file path.
#' @param var character, variable name.
#' 
#' @return matrix mask of variable field or if only a single valid cell exist: time series vector of valid cell
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
#' @export mHM_maskNCvar
#' 
#' 
mHM_maskNCvar <- function(x) {
  # mask - matrix
  mask <- !is.na(x[,,1])
  
  # give back vector or mask matrix 
  if (sum(mask > 1)) {
    return(mask)
  } else {
    return(x[mask])
  }
}