mHM_formatASC <- function(inRaster)
{
  for (write2 in inRaster)
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