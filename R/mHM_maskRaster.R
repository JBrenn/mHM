#' Mask raster maps with same projection and extent. 
#' 
#' \code{mHM_maskRaster} masks a raster with same projection and extent as the provided mask.
#' 
#' 
#' 
#' @param mask character, locating and defining raster format (either ASCII or GEOtif) or Raster* object.
#' @param inRaster character vector, gives location and format of raster files (either ASCII or GEOtif), 
#' with same projection and extent as provided raster mask.
#' @param outRaster character vector, defining raster format and location of the outfile, same length as \code{inRaster}.
#' 
#' @return raster in given format (\code{outRaster}) written to outRaster
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
#' @export mHM_maskRaster
#' 

mHM_maskRaster <- function(mask, inRaster, outRaster)
{
  # read in mask
  if (class(mask) == "character") maskR <- raster(mask)
  
  # locations of invalid values in raster mask 
  na_mask <- which(is.na(values(maskR)))
  
  # read in raster files
  # loop over inRaster
  for (i in 1:length(inRaster))
  {
    # print raster map that will be masked
    print(i)
    # load raster
    inR <-  raster(inRaster[i])
    # mask raster
    inR[na_mask] <- NA
    # write out raster
    writeRaster(x = inR, filename = outRaster[i], overwrite=T)
  }
}