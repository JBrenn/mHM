mHM_prepdailyQ <- function(zooobj, na_value = -9999, na_ext = TRUE, writeIN = NULL)
{
  if (is.null(writeIN)) writeIN <- getwd()
  
  print(paste("Writing gauge data files to", writeIN, sep=" "))
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
      startdate <- time(zooobj[,i])[1]
      enddate   <- tail(time(zooobj[,i]),1)
    } else {
      startdate <- time(zooobj[,i])[which(!is.na(zooobj[,i]))[1]]
      enddate   <- time(zooobj[,i])[tail(which(!is.na(zooobj[,i])),1)]
    }
   
    line4 <- paste("start ", format(startdate,"%Y"), format(startdate,"%m"), format(startdate,"%d"), 
                   "00 00   (YYYY MM DD HH MM)", sep=" ")
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
    
    date <- time(zoodata)
    df <- data.frame(Y=format(date,"%Y"),m=format(date,"%m"),d=format(date,"%d"),
                     H="00",M="00",data=round(coredata(zoodata),3))
    # write mHM input files
    write.table(x = df, file = file.path(writeIN,paste(i,".txt", sep="")), append = T, quote = F, sep = "  ", na = "-9999", 
                row.names = F, col.names = F)
    
    print(paste(i,".txt", sep=""))
  }

}