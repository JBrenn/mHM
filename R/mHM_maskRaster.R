# mask raster map same proj and extent
# library(raster)

mHM_maskRaster <- function(mask, inRaster, outRaster)
{
  # read in mask
  maskR <- raster(mask)
  na_mask <- which(is.na(values(maskR)))
  
  # read in raster files2mask
  for (i in 1:length(inRaster))
  {
    print(i)
    inR <-  raster(inRaster[i])
    inR[na_mask] <- NA
    writeRaster(x = inR, filename = outRaster[i], overwrite=T)
  }
}

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