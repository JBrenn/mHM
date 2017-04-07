#' plot comparing obs and sim for single var. 
#' 
#' \code{mhm_plotTSaggr} creates a plot (in pdf) comparing .
#' 
#' Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning
#' no meteorological input available for whole simulation domain will be printed. 
#' This function extrapolates grids in invalid locations within a radius given by \code{ext}.
#' Extrapolation is performed by a mean value of the surrounding cells of the invalid location.
#' 
#' @param obs .
#' @param sim .
#' @param obs_var .
#' @param sim_var .
#' @param aggr .
#' @param aggr_fun .
#' @param outfile .
#' @param simnames . 
#' @param cols .
#' @param cumsums .
#' 
#' @return data.frame containing GOF measures (KGE, NSE).
#' 
#' @examples
#' 
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mhm_plotTSaggr
#' 
mhm_plotTSaggr <- function(obs = "/Users/brennerj/tmp/eve_f2_data/data/processed/Jucar/EC_sites/06/input/optional_data/level11/et.nc",
                           sim = c("/Users/brennerj/tmp/eve_f2_work/mHMruns/002_Jucar_EC/005_forward_06_local_qparam/output/mHM_Fluxes_States.nc",
                                     "/Users/brennerj/tmp/eve_f2_home/mHMruns/002_Jucar_EC/06/001/out/mHM_Fluxes_States.nc"), 
                           obs_var="et", sim_var="aET", aggr="month", aggr_fun=sum, outfile, simnames = c("optim_Q","optim_ET"), 
                           cols = c(rgb(1,0,0,.7), rgb(0,1,0,.7),  rgb(0,0,1,.7)), cumsums=F)
{
  # read observation time series
  data_ts <- mHM_readNCvar2zoo(obs, obs_var)
  
  for (infile in sim)
  {  
    # read in simulation time series
    sim_ts <- mHM_readNCvar2zoo(infile, sim_var)
    # sim time series with NAs where there are also NAs in observation
    zoo::coredata(sim_ts)[is.na(zoo::coredata(data_ts[,1]))] <- NA
    # merge with obs data
    data_ts <- merge(data_ts, sim_ts)
  }
  
  # name series
  names(data_ts) <- c("observation", simnames)
  
  # time aggregation data
  if (aggr=="day") {
    data_ts <- aggregate(x = data_ts, by = as.Date(time(data_ts)), FUN=FUN, na.rm=T)
  } else if (aggr=="month") {
    data_ts <- aggregate(x = data_ts, by = as.yearmon(time(data_ts)), FUN=FUN, na.rm=T)
  } else if (aggr=="year") {
    data_ts <- aggregate(x = data_ts, by = as.integer(format(time(data_ts), "%Y")), FUN=FUN, na.rm=T)
  } else {
    print("WARNING. no valid aggregation char given: do not aggregate data")
  }
  
  # plot
  if(!is.null(outfile))
  {
    if (cumsums) {
      data_ts_p <- cumsum(data_ts)
    } else {
      data_ts_p <- data_ts
    }
    data_dim2 <- dim(data_ts_p)[2]
    # open pdf dev.out
    pdf(outfile)
    op <- par(cex=1.5, lwd=2)
    plot(data_ts_p, screen=rep(1,data_dim2), type=c("p", rep("s",data_dim2-1)), 
         col=cols, pch=c(19,rep(2,data_dim2-1)), 
         ylab = "ET [mm/month]", xlab="Year")
    points(data_ts_p[,1], pch=19, col=rgb(1,0,0,.7), type="p")
    legend("topleft", legend = names(data_ts), col = cols, 
           pch=c(19,rep(0,data_dim2-1)), bty="n")
    par(op)
    dev.off()
  }

  
  # KGE calculation
  df <- data.frame()
  for (i in 2:(dim(data_ts)[2]))
  {
    kge <- unlist(KGE(sim = data_ts[,i], obs = data_ts$observation, out.type = "full"))
    nse <- NSE(sim = data_ts[,i], obs = data_ts$observation)
    dfgof <- data.frame(GOF_value=c(kge,NSE=nse))
    df <- rbind(df, data.frame(GOF=row.names(dfgof), dfgof, VSsim = simnames[i-1]))
  }
  row.names(df) <- NULL
  
  # give back GOF table
  return(df)
}



