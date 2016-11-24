#' Simple extrapolation arrays in invalid locations. 
#' 
#' \code{mHM_extrapolMat} extrapolates matrix values in invalid location, e.g. originating from 
#' meteorological input fields for mHM (netCDF format).
#' 
#' Runing mHM the first time for some netCDF products (e.g. Spain02, E-OBS) the warning
#' no meteorological input available for whole simulation domain will be printed. 
#' This function extrapolates grids in invalid locations within a radius given by \code{ext}.
#' Extrapolation is performed by a mean value of the surrounding cells of the invalid location.
#' 
#' @param data matrix.
#' @param ext radius within grid cells are extended, starting from valid values.
#' 
#' @return matrix.
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
#' @export mHM_extrapolMat
#' 

mHM_extrapolMat <- function(data, ext)
{
  data <- data; ext <- ext
  
  # get invalid locations in array
  arrTind <- which(is.na(data), arr.ind = T)
  # apply function over these locations by row
  corr <- apply(X = arrTind, MARGIN = 1, function(x, data, ext) {
    # define rows and colums to use for aggregation
    rowsl <- x[1]-ext; rowsu <- x[1]+ext
    if (rowsl < 1) rowsl <- 1
    if (rowsu > dim(data)[1]) rowsu <- dim(data)[1]
    rows <- (rowsl):(rowsu)
    
    colsl <- x[2]-ext; colsu <- x[2]+ext
    if (colsl < 1) colsl <- 1
    if (colsu > dim(data)[2]) colsu <- dim(data)[2]
    cols <- (colsl):(colsu)
    
    # if there are only NAs within choosen values set new value to NA
    # otherwise take mean value
    if (any(!is.na(data[rows,cols]))) {
      y <- mean(data[rows,cols], na.rm=T)
    } else {
      y <- NA
    }
    return(y)
  }, data=data, ext=ext)
  
  # impute new values in data matrix
  for (i in 1:dim(arrTind)[1]) data[arrTind[i,1],arrTind[i,2]] <- corr[i]
  
  # give back matrix
  return(data)
}