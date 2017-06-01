#' Plot daily river flow of multiple simulations. 
#' 
#' \code{mHM_plotQmulti_d} is ploting a comparison of daily river flow data - simulation vs. observation in pdf file. 
#' Additionally GOFs (KGE, NSE) for cross-validation simulations are calculated, if calibration period is given.
#' 
#' @param ts1 zoo object, observed and simulated river flow data, as retrived from \code{\link{mhm_readQ}}.
#' @param ts2 zoo object, observed and simulated river flow data, as retrived from \code{\link{mhm_readQ}}.
#' @param windows vector of Date objects, defining start and end of series being ploted.
#' @param calibTime vector of Date objects, defining start and end of calibration period.
#' @param validTime vector of Date objects, defining start and end of validation period.
#' @param rollsteps integer width of the rolling window, see \code{\link{zoo::rollmean}}
#' @param outfile output pdf 
#' @param ylims numeric vector for y axes limitations, c(min,max)
#' @param cols color definition, lenght = 3
#' 
#' @return NULL
#' 
#' @examples
#' data("Qout_mHM")
#' 
#' # devide calibration and validation period
#' gof <- mHM_plotQ_d(ts = Qout_mHM, windows = c(start=as.Date("1989-10-01"), end=as.Date("2008-03-30")), 
#'                    calibTime = c(start=as.Date("2000-01-01"), end=as.Date("2008-03-30")),
#'                    validTime = c(start=as.Date("1990-01-01"), end=as.Date("1999-12-31"))
#'                    rollsteps = 10)
#'                    
#' # do not consider calibration/validatio period                  
#' gof <- mHM_plotQ_d(ts = Qout_mHM, windows = c(start=as.Date("1989-10-01"), end=as.Date("2008-03-30")), 
#'                    calibTime = NULL, validTime = NULL,
#'                    rollsteps = 10)
#'                    
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords 
#'  
#' @export mHM_plotQmulti_d
#'
#'
mHM_plotQmulti_d <- function(ts1, ts2, windows = c(start=as.Date("1989-10-01"), end=as.Date("2008-03-30")), 
                        calibTime = c(start=as.Date("2000-01-01"), end=as.Date("2008-03-30")), 
                        validTime = c(start=as.Date("1990-01-01"), end=as.Date("1999-12-31")),
                        rollsteps = 10, outfile = "out.pdf", ylims=NULL, 
                        cols=c(rgb(1,0,0,.75),grey.colors(1,.2,.2,alpha = .75),rgb(0,0,1,.7))) 
{
  # rolling mean, k steps
  ts_roll1 <- zoo::rollmean(ts1, k = rollsteps)
  ts_roll2 <- zoo::rollmean(ts2, k = rollsteps)
  
  # window data for ploting
  ts_win1  <- window(ts_roll1, start = windows[1], end = windows[2])
  ts_win2  <- window(ts_roll2, start = windows[1], end = windows[2])
  
  # merge data 
  ts_win <- merge(ts_win1, ts_win2[,2])
  names(ts_win) <- c(names(ts1), names(ts2)[2])
  
  kge <- c()
  for (i in 2:dim(ts_win)[2])
    kge[i-1] <- hydroGOF::KGE(sim = ts_win[,i], obs = ts_win[,1])
  kge <- round(kge,2)
  
  # open pdf dev.out
  pdf(outfile, width = 10)
  # define plot parameters
  op <- par(cex=1.5, mar=c(4.1,4.6,1.1,1.1))
  # empty plot
  if (is.null(ylims))
    ylims <- c(0,max(ts_win, na.rm=TRUE))
  # main plot    
  plot(ts_win, screens = c(1,1), col="white", lwd=2.5, ylab=expression(paste("river flow [",m^3,"/",s,"]",sep="")), xlab="", bty="n", ylim=ylims)  
  # plot polygon, calibration period
  if (!is.null(calibTime))
    xblocks(time(ts_win), time(ts_win)>calibTime[1] & time(ts_win)<calibTime[2],
            col = grey.colors(n = 1, start = .5, end = .5, alpha = .3), bty="n")
  # horizontal lines at tickmarks
  yaxp <- par("yaxp")
  ticks <- seq(yaxp[1],yaxp[2],(yaxp[2]-yaxp[1])/yaxp[3])
  abline(h = ticks[c(-1,-length(ticks))],  col=grey.colors(1,.5,.5,alpha = 1), lty=5)             
  # plot time series
  # simulation
  for (i in 2:dim(ts_win)[2])
    lines(ts_win[,i], col=cols[i], lwd=2.25)
  # observation
  lines(ts_win[,1], col=cols[1], lwd=.75)
  lines(ts_win[,1], col=cols[1], lwd=3.5, lty=3)
  # legend observation
  legend("topright", legend = names(ts_win)[1], lwd = 3, col=cols[1], bty="n")
  # legend simulation with kge
  legend("topleft", legend = paste(names(ts_win)[2:dim(ts_win)[2]], " (KGE=", kge, ")", sep=""), 
         lwd = 3, col=cols[2:dim(ts_win)[2]], bty="n")
  par(op)
  dev.off()
}
