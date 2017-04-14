#' Plot daily river flow. 
#' 
#' \code{mHM_plotQ_d} is ploting a comparison of daily river flow data - mutiple simulations vs. observation in pdf file. Additionally GOFs (KGE, NSE) for cross-validation simulations are calculated, if calibration period is given.
#' 
#' @param ts zoo object, simulated and observed river flow data, as retrived from \code{\link{mhm_readQ}}.
#' @param windows vector of Date objects, defining start and end of series being ploted.
#' @param calibTime vector of Date objects, defining start and end of calibration period.
#' @param validTime vector of Date objects, defining start and end of validation period.
#' @param rollsteps integer width of the rolling window, see \code{\link{zoo::rollmean}}
#' @param outfile output pdf
#' @param basinid character, basin id
#' @param ylims numeric vector for y axes limitations, c(min,max)
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
#' # do not consider calibration/validation period                  
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
#' @export mHM_plotQ_d
#'
#'
mHM_plotQ_d <- function(ts, windows = c(start=as.Date("1989-10-01"), end=as.Date("2008-03-30")), 
                        calibTime = c(start=as.Date("2000-01-01"), end=as.Date("2008-03-30")), 
                        validTime = c(start=as.Date("1990-01-01"), end=as.Date("1999-12-31")),
                        rollsteps = 10, outfile = "out.pdf", basinid, ylims=NULL) 
{

  # KGE calculation on daily data
  if (!is.null(calibTime) && !is.null(validTime)) {
    # window calibration period
    ts_win <- window(ts, start = calibTime[1], end = calibTime[2])
    GOFs_c <- KGE(sim = ts_win[,2], obs = ts_win[,1], out.type=c("full"))
    GOFs_c$NSE <- NSE(sim = ts_win[,2], obs = ts_win[,1])
    # window validation period
    ts_win <- window(ts, start = validTime[1], end = validTime[2])
    GOFs_v <- KGE(sim = ts_win[,2], obs = ts_win[,1], out.type=c("full"))
    GOFs_v$NSE <- NSE(sim = ts_win[,2], obs = ts_win[,1])
    GOFs <- data.frame(gof_type=rep(names(unlist(GOFs_c)),2),
                       gof_value=round(c(unlist(GOFs_c),unlist(GOFs_v)), 2), 
                       sim_per=c(rep("calib",5),rep("valid",5)), basin=basinid)
  } 
  
  if (is.null(validTime) && is.null(validTime)) {
    ts_win <- window(ts, start = windows[1], end = windows[2])
    GOFs <- KGE(sim = ts_win[,2], obs = ts_win[,1], out.type=c("full"))
    GOFs$NSE <- NSE(sim = ts_win[,2], obs = ts_win[,1])
    GOFs <- unlist(GOFs_c)
    GOFs <- data.frame(gof_type=names(unlist(GOFs_c)), gof_value=unlist(GOFs_c),
                       basin=basinid)
  }
  
  # rolling mean, k steps
  ts_roll <- zoo::rollmean(ts, k = rollsteps)
  
  # window data for ploting
  ts_win  <- window(ts_roll, start = windows[1], end = windows[2])
  
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
  lines(ts_win[,2], col=rgb(0,0,1,.7), lwd=2.25)
  # observation
  lines(ts_win[,1], col=rgb(1,0,0,.75), lwd=.75)
  lines(ts_win[,1], col=rgb(1,0,0,.8), lwd=3.5, lty=3)
  # legend
  legend("topright", legend = c("observation","simulation"), lwd = 3, col=c(rgb(1,0,0,.75), rgb(0,0,1,.75)), bty="n")
  # legend KGE
  kgetxtcal <- paste(c("KGE=", "r=", "beta=", "alpha="), 
                  GOFs$gof_value[grepl("KGE", GOFs$gof_type) & grepl("calib", GOFs$sim_per)], 
                  sep="")
  kgetxtcal <- paste(kgetxtcal[1], kgetxtcal[2], kgetxtcal[3] ,kgetxtcal[4], sep=", ")
  kgetxtcal <- paste("cal: ", kgetxtcal, sep="")
  kgetxtval <- paste(c("KGE=", "r=", "beta=", "alpha="), 
                     GOFs$gof_value[grepl("KGE", GOFs$gof_type) & grepl("valid", GOFs$sim_per)], 
                     sep="")
  kgetxtval <- paste(kgetxtval[1], kgetxtval[2], kgetxtval[3] ,kgetxtval[4], sep=", ")
  kgetxtval <- paste("val: ", kgetxtval, sep="")
  legend("topleft", legend = c(kgetxtcal, kgetxtval), bty="n")
  par(op)
  dev.off()
  
  return(GOFs)
}
