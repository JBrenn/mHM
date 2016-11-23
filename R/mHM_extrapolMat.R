mHM_extrapolMat <- function(data, ext)
{
  data <- data; ext <- ext
  
  arrTind <- which(is.na(data), arr.ind = T)
  corr <- apply(X = arrTind, MARGIN = 1, function(x, data, ext) {
    
    rowsl <- x[1]-ext; rowsu <- x[1]+ext
    if (rowsl < 1) rowsl <- 1
    if (rowsu > dim(data)[1]) rowsu <- dim(data)[1]
    rows <- (rowsl):(rowsu)
    
    colsl <- x[2]-ext; colsu <- x[2]+ext
    if (colsl < 1) colsl <- 1
    if (colsu > dim(data)[2]) colsu <- dim(data)[2]
    cols <- (colsl):(colsu)
    
    if (any(!is.na(data[rows,cols]))) {
      y <- mean(data[rows,cols], na.rm=T)
    } else {
      y <- NA
    }
    return(y)
  }, data=data, ext=ext)
  
  for (i in 1:dim(arrTind)[1]) data[arrTind[i,1],arrTind[i,2]] <- corr[i]
  
  return(data)
}