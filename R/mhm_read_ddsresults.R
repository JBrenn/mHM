#' Read best GOF DDS.
#' 
#' \code{mhm_read_ddsresults} reads best GOF value of DDS calibration run.
#' 
#' @param file file name with full path.
#' @param NRlines2read integer, number of lines to read from end of file, default=100.
#' 
#' @return Best GOF value of DDS calibration run
#' 
#' @examples
#' nml <- mhm_read_ddsresults(file="your/path/dds_results.out")
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mhm_read_ddsresults
#' 
mhm_read_ddsresults <- function(file, NRlines2read=100)
{
  # get last line number 
  lines <- as.integer(system2("wc", args = c("-l", file, " | awk '{print $1}'"), 
                             stdout = TRUE))
  # read last line
  lines <- read_lines(file = file, skip = lines-NRlines2read)
  
  # pick KGE value
  gof <- as.numeric(substr(lines, 25, 32))
  best_gof <- 1-round(gof[which.min(gof)],2)
  
  # return best_gof
  return(best_gof)
}