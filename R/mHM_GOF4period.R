#' Calculate GOFs for time intervals.
#' 
#' \code{mHM_GOF4period} returns Goodness of Fit (GOF) measures for specific time intervals. 
#' GOF measures are calculated with \code{hydroGOF::gof}.
#' 
#' Besides calculation of GOFs \code{mHM_GOF4period} provides ploting facilities from \code{hydroGOF::ggof}. 
#' By default three plots are produced: daily and monthly aggregation, seasonal aggregation, 
#' all of them comparing simulated and observed discharge time series.
#' 
#' @param zoo_obj zoo object containing observed and simulated daily discharge.
#' @param start start date, either Date object or character in the following format yyyy-mm-dd.
#' @param end end date, either Date object or character in the following format yyyy-mm-dd.
#' @param plot boolean, if TRUE ggof plot from hydroGOF package.
#' 
#' @return numeric vector of GOF values.
#' 
#' @examples
#' data(Qout_mHM)
#' cal_gof <- mHM_GOF4period(zoo_obj = Qout_mHM, start = "2000-01-01", end = "2008-03-30", plot = F)
#'
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mHM_GOF4period

mHM_GOF4period <- function(zoo_obj, start=NA, end=NA, plot=FALSE, pdf=FALSE)
{
  # set start to start date of zoo_obj if is NA
  # create date object if not provided
  if (is.na(start)) {
    start <- time(zoo_obj)[1]
  } else {
    if (class(start) != "Date") start <- as.Date(start)
  }
  
  # set end to end date of zoo_obj if is NA
  # create date object if not provided
  if (is.na(end)) {
    end <- tail(time(zoo_obj),1)
  } else {
    if (class(end) != "Date") end <- as.Date(end)
  }
  
  # window by start-end period
  zoo_obj <- window(zoo_obj, start = start, end = end)
  
  # locate sim and obs data in zoo object
  sim <- grep("sim", names(zoo_obj))
  obs <- grep("obs", names(zoo_obj))
  # name of sim and obs col
  sim_col <- names(zoo_obj)[sim]
  obs_col <- names(zoo_obj)[obs]
  
  # calculate GOFs
  gofs <- hydroGOF::gof(sim = zoo_obj[,sim], obs =  zoo_obj[,obs])
  
  if (plot) {
    if (pdf) {
      pdf("dm.pdf")
      hydroGOF::ggof(sim = zoo_obj[,sim], obs =  zoo_obj[,obs], ftype = "dm", FUN = mean, legend = c(sim_col,obs_col))
      dev.off()
      pdf("sesonal.pdf")
      hydroGOF::ggof(sim = zoo_obj[,sim], obs =  zoo_obj[,obs], ftype = "seasonal", FUN = mean, legend = c(sim_col,obs_col))
      dev.off()
    } else {
      hydroGOF::ggof(sim = zoo_obj[,sim], obs =  zoo_obj[,obs], ftype = "dm", FUN = mean, legend = c(sim_col,obs_col))
      hydroGOF::ggof(sim = zoo_obj[,sim], obs =  zoo_obj[,obs], ftype = "seasonal", FUN = mean, legend = c(sim_col,obs_col))
    }
  }
 
  return(gofs)   
}
