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
#' @param KGEonly boolean, if TRUE only GOF KGE is given back, in addition to the Kling-Gupta efficiency  the Pearson product-moment correlation coefficient (‘r’), the ratio between the mean of the simulated values to the mean of observations (‘Beta’), and the variability measure (‘Gamma’ or ‘Alpha’ the Pearson product-moment correlation coefficient (‘r’), the ratio between the mean of the simulated values to the mean of observations (‘Beta’), and the variability measure (‘Gamma’ or ‘Alpha’, depending on the value of method))
#' @param method see help(hydroGOF::KGE), default: "2009"
#' 
#' @return numeric vector of GOF values.
#' 
#' @examples
#' library(zoo)
#' library(hydroGOF)
#' data(Qout_mHM)
#' cal_gof <- mHM_GOF4period(zoo_obj = Qout_mHM, start = "2000-01-01", end = "2008-03-30", plot = FALSE)
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

mHM_GOF4period <- function(zoo_obj, start=NA, end=NA, plot=FALSE, pdf=FALSE, KGEonly=TRUE, method="2009")
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
  if (KGEonly) 
    gofs <- hydroGOF::KGE(sim = zoo_obj[,sim], obs =  zoo_obj[,obs], method = method, out.type = "full") else 
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
