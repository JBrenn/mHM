#' Prepare daily discharge files for mHM. 
#' 
#' \code{mHM_prepdailyQ} writes mHM standard formated discharge information for model calibration and validation from a zoo object.
#' 
#' 
#' 
#' 
#' @param zooobj zoo object, daily time steps.
#' @param na_value NA value. 
#' @param na_ext boolean, if TRUE time period written out will be defined by begin and end date of zoo object.
#' @param writeIN character vector, setting utput file location, if is NA output files will be written in working directory.
#' 
#' @return write mHM conform files to \code{wrriteIN} or working directory
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export mHM_prepdailyQ
#' 

mHM_prepdailyQ <- function(zooobj, na_value = -9999, na_ext = TRUE, writeIN = NA)
{
  # if writeIN is NA write output files in working dir
  if (is.na(writeIN)) writeIN <- getwd()
  
  # print info on which file will be written
  print(paste("Writing gauge data files to", writeIN, sep=" "))
  # loop over names of zoo object, each col will be written in seperate file
  for (i in names(zooobj))
  {
    # header info
    # gauge id, info
    line1 <- paste(i, "Gauge (daily discharge)",sep=" ")
    # nodata info(
    line2 <- paste("nodata", na_value, sep="  ")
    # measurements per day
    line3 <- paste("n       ",1,"       measurements per day", sep="")
    # start date & end date
    if (na_ext) {
      # if na_ext is TRUE time period to write is from start to end of zoo object
      # for time series starting later or ending earlier NAs will be written
      startdate <- time(zooobj[,i])[1]
      enddate   <- tail(time(zooobj[,i]),1)
    } else {
      # write from start to end of time series
      startdate <- time(zooobj[,i])[which(!is.na(zooobj[,i]))[1]]
      enddate   <- time(zooobj[,i])[tail(which(!is.na(zooobj[,i])),1)]
    }
    # start day
    line4 <- paste("start ", format(startdate,"%Y"), format(startdate,"%m"), format(startdate,"%d"), 
                   "00 00   (YYYY MM DD HH MM)", sep=" ")
    # end day
    line5 <- paste("end   ", format(enddate,"%Y"), format(enddate,"%m"), format(enddate,"%d"), 
                   "00 00   (YYYY MM DD HH MM)", sep=" ")
    
    # bind header lines together
    header <- paste(line1, line2, line3, line4, line5, sep="\n")
    header <- paste(header, "\n", sep="")
    
    # create file
    file.create(file.path(writeIN, paste(i,".txt", sep="")), showWarnings = F)
    
    # write header
    # open connection to new file
    con <- file(file.path(writeIN, paste(i,".txt", sep="")))
    # write header in file
    cat(header, file = con)
    # close connection
    close(con)
    
    # create data frame 2 write
    zoodata <- window(zooobj[,i], start = startdate, end = enddate)
    # extract date
    date <- time(zoodata)
    df <- data.frame(Y=format(date,"%Y"),m=format(date,"%m"),d=format(date,"%d"),
                     H="00",M="00",data=round(coredata(zoodata),3))
    # write mHM input files
    write.table(x = df, file = file.path(writeIN,paste(i,".txt", sep="")), append = T, quote = F, sep = "  ", na = "-9999", 
                row.names = F, col.names = F)
    #print(paste(i,".txt", sep=""))
  }
}