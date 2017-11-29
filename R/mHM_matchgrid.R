#' @title match grids
#' @description \code{mHM_matchgrid} does mask grids, e.g. morphological maps (.asc) by netCDF meteorological mHM input.
#' @param grids grid files to be masked
#' @param mask_grid grid mask
#' @param proj4 PARAM_DESCRIPTION, Default: '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'
#' @return grids will be written back to original file
#' @details grids will be backed up before processing
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[raster]{raster}},\code{\link[raster]{crs}},\code{\link[raster]{resample}},\code{\link[raster]{mask}},\code{\link[raster]{writeRaster}}
#' @rdname mHM_matchgrid
#' @export mHM_matchgrid
#' @importFrom raster raster crs resample mask writeRaster
mHM_matchgrid <- function(grids, mask_grid, 
                          proj4="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
{
  for (grid in grids)
  {
    # 
    print(paste("processing", grid))
    
    # save backup of grid1
    system(paste("cp ", grid, " ", grid, ".bak", sep=""))
    
    # read in grid1 
    r1 <- raster::raster(grid)
    
    # read in grid2 
    r2 <- raster::raster(mask_grid, varname="pre")
    
    # add projection
    raster::crs(r1) <- raster::crs(r2) <- proj4
    
    # crop data r1
    r1_crop <- raster::crop(r1, r2)
    
    # resample data r2
    #r2_res <- raster::resample(r2, r1_crop)
    
    # disaggregate r2
    fact <- res(r2)[1] / res(r1)[1]
    r2_res <- raster::disaggregate(r2, fact=fact)
   
    # crop data r2
    r2_crop <- raster::crop(r2_res, r1_crop)
    
    # mask r1_crop data
    r <- raster::mask(x = r1_crop, mask = r2_crop)
    
    #r_crop_new <- ifelse(is.na(getValues(r2_crop)),NA,getValues(r1_crop))
    #r <- setValues(r1_crop, r_crop_new)
    
    # write raster
    raster::writeRaster(r, grid, overwrite=T)
    
    # format asc to mHM format
    mHMr::mHM_formatASC(inASC = grid)
    
  }
}
