#' Read mHM daily_discharge.out in zoo object.
#' 
#' @param outQpath path to folder containing the file daily_discharge.
#' 
#' @return daily zoo object including observed and simulated discharge.
#' 
#' @examples
#' outQ <- mHM_readQ(outQpath="your/path")
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mHM_readQ

mHM_readQ <- function(outQpath, dischargeFile = NA)
{
  # read daily_discharge.out file
  if (is.na(dischargeFile)) {
    qout <- readr::read_table(file = file.path(outQpath,"daily_discharge.out"))
  } else {
    qout <- readr::read_table(file = file.path(outQpath, dischargeFile))
  }
  
  
  # make time series / zoo object
  days <- as.Date(paste(qout$Year, qout$Mon, qout$Day, sep="-"), format = "%Y-%m-%d")
  qout_zoo <- zoo::zoo(qout[,c(5,6)], days)
  
  return(qout_zoo)
}
