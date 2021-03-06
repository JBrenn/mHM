% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mHM_readQ.R
\name{mHM_readQ}
\alias{mHM_readQ}
\title{Read mHM discharge data in zoo object.}
\usage{
mHM_readQ(Qpath, dischargeFile = "daily_discharge.out", inFile = FALSE)
}
\arguments{
\item{Qpath}{path to folder containing the file daily_discharge (mHM constrain or output).}

\item{dischargeFile}{name of discharge file, default = "daily_discharge.out"}

\item{inFile}{boolean, is given discharge file in mHM constraining data, default = FALSE}
}
\value{
daily zoo object including observed and simulated discharge.
}
\description{
Read mHM discharge data in zoo object.
}
\examples{
outQ <- mHM_readQ(Qpath="your/path")

}
\references{

}
\seealso{

}
\author{
Johannes Brenner \email{johannes.brenner@ufz.de}
}
\keyword{}
