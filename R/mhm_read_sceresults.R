#' Read best GOF SCE
#' 
#' \code{mhm_read_sceresults} reads best GOF value of SCE calibration run.
#' 
#' @param file file name with full path.
#' 
#' @return Best GOF value of SCE calibration run
#' 
#' @examples
#' nml <- mhm_read_sceresults(file="your/path/sce_results.out")
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mhm_read_sceresults
#' 
mhm_read_sceresults <- function(file)
{
  # get last line number 
  lines <- as.integer(system2("wc", args = c("-l", file, " | awk '{print $1}'"), 
                              stdout = TRUE))
  # read last line
  lines <- read_lines(file = file, skip = lines-1)
  
  # pick KGE value
  gof <- as.numeric(substr(lines, 49, 55))
  best_gof <- 1-round(gof[which.min(gof)],2)
  
  # return best_gof
  return(best_gof)
}