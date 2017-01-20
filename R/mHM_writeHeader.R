#' Write mHM header file. 
#' 
#' \code{mHM_writeHeader} writes mHM header files, needed in addition to meteo netCDF input files. Moreover,
#' a .griddes file can be calculated for later use with CDO to remap the netCDF meteo input according to morphological input maps.
#' 
#' For remaping the netCDF meteo input see \code{cdo --help remap}. 
#' The python bash script remap-grid is located in the library directory, folder inst/bash of the package \code{mHMr} (\code{path.package("mHMr")}). 
#' For further information type \code{./remap-grid --help} after changing directory.
#' 
#' @param rst raster object, which will deliver extent, e.g. DEM.
#' @param L2_res resolution of level L2 data, meteorological input.
#' @param res resolution in header file.
#' @param na na. value
#' @param crs_espg projection information for \code{rst} in ESPG code.
#' @param writeHeader boolean, if TRUE header will be written to working directory.
#' @param pre character, added to header file name.
#' @param make.griddes boolean, if TRUE calculate .griddes info
#' @param remap_py_file full path python-bash script remap-grid
#'  
#' @return reclassified raster object, wrote to ASCII if writeHeader is TRUE.
#' If make.griddes is TRUE .griddes file, wrote to working directory.
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
#' @export mHM_writeHeader
#' 

mHM_writeHeader <- function(rst, L2_res=10000, res=10000, na=-9999, crs_espg=23030, writeHeader=TRUE, 
                            pre="headerAtLevel11",
                            make.griddes=FALSE, remap_py_file)
{
  # read raster
  rst <- raster::raster(rst)
  
  # calculate x, y range
  rst_ext <- raster::extent(rst)
  x_range <- rst_ext@xmax - rst_ext@xmin + L2_res
  y_range <- rst_ext@ymax - rst_ext@ymin + L2_res
  
  # number of rows and cols
  rownm <- floor(y_range/L2_res + 1) * L2_res/res
  colnm <- floor(x_range/L2_res + 1) * L2_res/res
  
  if (writeHeader) 
  {
    # write header
      # create file
    filename <- paste(pre,"_",res,"m.txt", sep="")
    file.create(filename)
      # write in file
    con <- file(description = filename, open = "write")
    writeLines(paste("ncols", colnm), con)
    writeLines(paste("nrows", rownm), con)
    writeLines(paste("xllcorner", rst_ext@xmin), con)
    writeLines(paste("yllcorner", rst_ext@ymin), con)
    writeLines(paste("cellsize", res), con)
    writeLines(paste("NODATA_value", na), con)
    close(con)
    print(paste(filename, "created.", sep=" "))
  }

  # get extent in degree
  EPSG <- rgdal::make_EPSG()
  # Get PROJ.4 information for a particular EPSG code
  crs <- EPSG$prj4[which(EPSG$code==crs_espg)]
  # project with rgdal
  ext_deg <- rgdal::project(cbind(c(rst_ext@xmin, rst_ext@xmax+L2_res),c(rst_ext@ymin, rst_ext@ymax+L2_res)), 
                            proj = crs, inv = T)
  colnames(ext_deg) <- c("x_deg", "y_deg")
  
  #create new grid for netCDF files with python function remap-grid
  if (make.griddes)
  {
    bahs_call <- paste(remap_py_file," -c 'epsg:",crs_espg,"' -g ",pre,"_",L2_res,"m.txt -o ",res,"m.griddes",sep="")
    system(bahs_call)
    
    print("if an error occurs in creating new grid for netCDF try to run the following line in the terminal:")
    print(bahs_call)
    
  }
    
  return(ext_deg)
}