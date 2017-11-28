#' @title convert ascii to netCDF.
#' @description \code{mHM_asc2netCDF} is converting ascii spatial file format to netCDF.
#' @param rst raster input object, either raster obj or files formate for read in with \code{\link[raster]{raster}}.
#' @param outfile character, output file name, default: "out.nc".
#' @param crs either character proj4 coordinate projection definition, e.g. "+proj=utm +zone=30 +ellps=intl +units=m +no_defs" for "epsg:23030". Or character epsg coordinate projection definition, e.g. "epsg:23030".
#' @param varname character, variable name to be set in netCDF.
#' @param varunit character, variable unit to be set in netCDF.
#' @param longname character, variable long name to be set in netCDF.
#' @param xname character, x coordinate name, default: "lon"
#' @param xname character, y coordinate name, default: "lat"
#' @param project boolean, project data, DEFAULT: FALSE
#' 
#' @return netCDF written to outfile.
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso 
#' \code{\link[raster]{raster}}, \code{\link[raster]{crs}}, \code{\link[raster]{writeRaster}}, \code{\link[raster]{projectRaster}}
#' @rdname mHM_asc2netCDF
#' @export mHM_asc2netCDF
#' @importFrom raster raster crs writeRaster projectRaster
#'
mHM_asc2netCDF <- function(rst = NA, outfile = "out.nc", crs,
                           varname, varunit, longname,
                           xname = "lon", yname = "lat", 
                           project = FALSE) {
  
  # convert asc to raster object
  if (!is.na(rst)) {
    rst <- raster::raster(rst)
  }
  
  # epsg to proj4 character
  if (grepl("epsg", crs))
    crs <- raster::crs(paste("+init=", crs, sep=""))@projargs
  
  # project raster    
  raster::crs(rst) <- crs

  # re-projection coordinates
  if (project) {
    rst_proj <- raster::projectRaster(rst, crs = crs)
  }
  
  # write raster to netCDF4
  raster::writeRaster(rst, outfile, overwrite=TRUE, format="CDF", 
                      varname=varname, varunit=varunit, 
                      longname=longname, xname=xname, yname=yname)
}
