#' Read mHM discharge data in zoo object.
#'
#' @param Qpath path to folder containing the file daily_discharge (mHM constrain or output).
#' @param dischargeFile name of discharge file, default = "daily_discharge.out"
#' @param inFile boolean, is given discharge file in mHM constraining data, default = FALSE
#'
#' @return daily zoo object including observed and simulated discharge.
#'
#' @examples
#' outQ <- mHM_readQ(Qpath="your/path")
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

mHM_readQ <- function(Qpath, dischargeFile = "daily_discharge.out", inFile=FALSE)
{
  if (inFile) {
    # read daily_discharge.in file
    qin  <- readr::read_table(file = file.path(Qpath, dischargeFile),
                              na = c("NA", "-9999", "-9999.000", "999.000"),
                              skip = 5, col_names = F)
    # make time series / zoo object
    days <- as.Date(paste(qin$X1, qin$X2, qin$X3, sep="-"), format = "%Y-%m-%d")
    qout_zoo <- zoo::zoo(qin$X6, days)
  } else {
    # read daily_discharge.out file
    qout <- readr::read_table(file = file.path(Qpath, dischargeFile),
                              na = c("NA", "-9999"))

    # make time series / zoo object
    days <- as.Date(paste(qout$Year, qout$Mon, qout$Day, sep="-"), format = "%Y-%m-%d")
    qout_zoo <- zoo::zoo(qout[,c(5,6)], days)
  }


  return(qout_zoo)
}
