#' @title resample netCDF
#' @description resample netCDF data based on grid information of additional netCDF file. 
#' @param nc_in netCDF file to be resampled
#' @param var_in variable name
#' @param nc_out netCDF file from which raster info is drawn from
#' @return raster object
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[raster]{brick}},\code{\link[raster]{raster}},\code{\link[raster]{resample}}
#' @rdname mHM_resamplenc
#' @export mHM_resamplenc
#' @importFrom raster brick raster resample

mHM_resamplenc <- function(nc_in, var_in, nc_out)
{
  # load data
  # nc_in
  rb <- raster::brick(x = nc_in, variable = var_in)
  # nc_out
  r  <- raster::raster(x = nc_out)
  
  # resample
  rb_res <- raster::resample(x = rb, y = r)
  
  #return
  return(rb_res)
}
