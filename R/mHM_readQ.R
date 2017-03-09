#' Read mHM daily_discharge.out in zoo object.
#' 
#' @param outQpath path to folder containing the file daily_discharge.
#' @param dischargeFile name of discharge file, default = "daily_discharge.out"
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

mHM_readQ <- function(outQpath, dischargeFile = "daily_discharge.out")
{
  # read daily_discharge.out file
  qout <- readr::read_table(file = file.path(outQpath, dischargeFile), na = c("NA", "-9999"))
  
  # make time series / zoo object
  days <- as.Date(paste(qout$Year, qout$Mon, qout$Day, sep="-"), format = "%Y-%m-%d")
  qout_zoo <- zoo::zoo(qout[,c(5,6)], days)
  
  return(qout_zoo)
}
