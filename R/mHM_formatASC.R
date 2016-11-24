#' Reformat ASCII file. 
#' 
#' \code{mHM_formatASC} is rewriting the header of an ASCII file being written by \code{\link{raster::writeRaster}}.
#' 
#' ASCII files written by \code{\link{raster::writeRaster}} do not fullfil conventions for mHM input ASCII files.
#' This function rewrites the header and find&replaces additional zeros appended to -9999 values.
#' 
#' @param inASC character vector, gives location of raster files in ASCII format.
#' 
#' @return NULL
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords ascii
#'  
#' @export mHM_formatASC
#'
#'

mHM_formatASC <- function(inASC)
{
  for (write2 in inASC)
  {
    # correct header format
    system(paste("sed -i -e 's/.000000000000000 / /g' ", write2, sep=""))
    system(paste("sed -i -e 's/NCOLS/ncols/g' ", write2, sep=""))
    system(paste("sed -i -e 's/NROWS/nrows/g' ", write2, sep=""))
    system(paste("sed -i -e 's/NCOLS/ncols/g' ", write2, sep=""))
    system(paste("sed -i -e 's/XLLCORNER/xllcorner/g' ", write2, sep=""))
    system(paste("sed -i -e 's/YLLCORNER/yllcorner/g' ", write2, sep=""))
    system(paste("sed -i -e 's/CELLSIZE/cellsize/g' ", write2, sep=""))
    system(paste("rm ", write2, "-e", sep=""))
  }
}