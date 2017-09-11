#' Read best parameter set.
#' 
#' \code{mhm_read_paramset} reads best parameterset of mHM calibration run.
#' 
#' @param file file name with full path.
#' 
#' @return
#' 
#' @examples
#' nml <- mhm_read_paramset(file="your/path/FinalParam.out")
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mhm_read_paramset
#' 
mhm_read_paramset <- function(file)
{
  # get parameterset
  ps <- read_table(file)
  
  # return best_gof
  return(ps)
}