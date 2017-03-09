#' point calib/valid ts data to netCDF. 
#' 
#' \code{mHM_ts2netCDF} is converting time series data to netCDF
#' 
#' 
#' @param dem morphological dummy file of mHM simulation, default: "./input/morph/dem.asc".
#' @param latlon latlon.nc file of mHM simulation, default: "./input/latlon/latlon.nc".
#' @param ts time series zoo data
#' @param outfile character, output file name, default: "out.nc".
#' @param var.name character, variable name to be set in netCDF.
#' @param unit character, variable unit to be set in netCDF.
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
#' 
#' @keywords
#'  
#' @export mHM_ts2netCDF
#' 
mHM_ts2netCDF <- function(dem = "./input/morph/dem.asc", latlon = "./input/latlon/latlon.nc", ts,
                          outfile,
                          var.name, unit) {
  # read asc
  dem_rst <- raster::raster(dem)
  # read latlon
  latlon_nc <- RNetCDF::open.nc(latlon)
  lon_l0 <- RNetCDF::var.get.nc(ncfile = latlon_nc, variable = "lon_l0")
  lat_l0 <- RNetCDF::var.get.nc(ncfile = latlon_nc, variable = "lat_l0")
  RNetCDF::close.nc(latlon_nc)
  # create new output netCDF  
  new_nc <- RNetCDF::create.nc(filename = outfile)  
  
  # A dimension length is an arbitrary positive integer,
  # except that one dimension in a classic
  # or 64-bit offset netCDF dataset can have the length UNLIMITED.
  
  # time
  if (class(time(ts)) == "Date") {
    times <- as.numeric(time(ts))
  } else {
    if (class(time(ts))[2] == "POSIXt") {
      times <- as.numeric(time(ts)) / (3600 *24)
    } else {
      times <- as.numeric(time(ts))
    }
  }
  # set time dimension, unlimited
  RNetCDF::dim.def.nc(ncfile = new_nc, dimname = "time", unlim=TRUE)
  # set time variable definition
  RNetCDF::var.def.nc(ncfile = new_nc, varname = "time", vartype = "NC_DOUBLE", dimensions = "time")
  # put numeric/double time steps
  RNetCDF::var.put.nc(ncfile = new_nc, variable = "time", data = times)
  # set attributes
  # units defining origin, origin can be changed in global settings of zoo - POSIX
  RNetCDF::att.put.nc(new_nc, "time", "units", "NC_CHAR", paste("days since", as.Date(0, origin = lubridate::origin), "00:00:00", sep=" "))
  RNetCDF::att.put.nc(new_nc, "time", "standard_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(new_nc, "time", "long_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(new_nc, "time", "calendar", "NC_CHAR", "standard")
  RNetCDF::att.put.nc(new_nc, "time", "axis", "NC_CHAR", "T")
  # copy attributes to new_nc
  
  # set y-lat dimension, length: dim(lat)
  RNetCDF::dim.def.nc(ncfile = new_nc, dimname = "y", dimlength = dim(lat_l0)[1])
  
  # set x-lon dimension, length: dim(lon)
  RNetCDF::dim.def.nc(ncfile = new_nc, dimname = "x", dimlength = dim(lon_l0)[1])
  
  # latitude, longitude
  # variable definition
  # latitude, dim(x,y)
  RNetCDF::var.def.nc(ncfile = new_nc, varname = "lat", vartype = "NC_DOUBLE", dimensions = c("x","y"))
  # longitude, dim(x,y)
  RNetCDF::var.def.nc(ncfile = new_nc, varname = "lon", vartype = "NC_DOUBLE", dimensions = c("x","y"))
  # put latitude in degree
  RNetCDF::var.put.nc(ncfile = new_nc, variable = "lat", data = lat_l0)
  # put longitude in degree
  RNetCDF::var.put.nc(ncfile = new_nc, variable = "lon", data = lon_l0)
  
  RNetCDF::att.put.nc(new_nc, "lat", "units", "NC_CHAR", "degrees_north")
  RNetCDF::att.put.nc(new_nc, "lat", "standard_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(new_nc, "lat", "long_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(new_nc, "lat", "_CoordinateAxisType", "NC_CHAR", "Lat")
  
  RNetCDF::att.put.nc(new_nc, "lon", "units", "NC_CHAR", "degrees_east")
  RNetCDF::att.put.nc(new_nc, "lon", "standard_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(new_nc, "lon", "long_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(new_nc, "lon", "_CoordinateAxisType", "NC_CHAR", "Lon")
  
  # data
  # set na to -9999 in ts data
  data <- ifelse(is.na(zoo::coredata(ts)),NA,zoo::coredata(ts))
  
  # define variable with st.name, dimensions x,y,time
  RNetCDF::var.def.nc(ncfile = new_nc, varname = var.name, vartype = "NC_DOUBLE",
                      dimensions = c("x","y","time"))

  # define attributes
  RNetCDF::att.put.nc(new_nc, var.name, "units", "NC_CHAR", unit)
  RNetCDF::att.put.nc(new_nc, var.name, "standard_name", "NC_CHAR", var.name)
  RNetCDF::att.put.nc(new_nc, var.name, "long_name", "NC_CHAR", var.name)
  RNetCDF::att.put.nc(new_nc, var.name, "missing_value", "NC_INT", -9999)
  RNetCDF::att.put.nc(new_nc, var.name, "coordinates", "NC_CHAR", "lat lon")
  
  rst_val <- raster::values(dem_rst)
  rst_imp <- which(!is.na(rst_val))
  
  for (i in 1:length(zoo::coredata(data)))
  # set data in rst
  {
    # put var value in valid asc cell
    rst_val[rst_imp] <- zoo::coredata(data)[i]
    
    # put data
    RNetCDF::var.put.nc(ncfile = new_nc, variable = var.name, data = rst_val,
                        start = c(1,1,i), count=c(dim(lon_l0)[1],dim(lat_l0)[1],1))
    # print 
    print(paste(i, "of", length(times), "| value:",round(zoo::coredata(data)[i],2), unit))
  }
  
  RNetCDF::close.nc(new_nc)
  
  print( paste(outfile, " created.", sep="") )

}