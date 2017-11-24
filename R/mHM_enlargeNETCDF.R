#' @title enlarge NetCDF files
#' @description enlarge netCDF file in its x and y dimension
#' @param netcdf netCDF file
#' @param x integer, x dimension length after enlargement of field 
#' @param y integer, y dimension length after enlargement of field 
#' @param var name of varable to process 
#' @param fixcorner field corner not to be moved, Default: 1. See detail
#' @return NULL, new netCDF file will be written beside exiating in-netCDF
#' @details fixcorner 1: bottomleft, fixcorner 2: topleft, fixcorner 3: topright, fixcorner 4: bottomright
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[RNetCDF]{open.nc}},\code{\link[RNetCDF]{var.get.nc}},\code{\link[RNetCDF]{create.nc}},\code{\link[RNetCDF]{dim.def.nc}},\code{\link[RNetCDF]{var.def.nc}},\code{\link[RNetCDF]{att.copy.nc}},\code{\link[RNetCDF]{var.put.nc}},\code{\link[RNetCDF]{close.nc}}
#' @rdname mHM_enlargeNETCDF
#' @export
#' @importFrom RNetCDF open.nc var.get.nc create.nc dim.def.nc var.def.nc att.copy.nc var.put.nc close.nc
mHM_enlargeNETCDF <- function(netcdf, x, y, var, fixcorner=1)
{
  # open connection to netcdf
  con <- RNetCDF::open.nc(con = netcdf, write = FALSE)
  # read in netCDF var
  old.data <- RNetCDF::var.get.nc(ncfile = con, variable = var)
  
  # enlarge array to x,y dim
  new.data <- array(data=NA, dim=c(x,y,dim(old.data)[3]))
  
  # fixcorner==
  #1 (DEFAULT) - bottomleft
  if (fixcorner==1)
    new.data[1:dim(old.data)[1],1:dim(old.data)[2]+y-dim(old.data)[2],] <- old.data
  #2 - topleft
  if (fixcorner==2)
    new.data[1:dim(old.data)[1],1:dim(old.data)[2],] <- old.data
  #3 - topright
  if (fixcorner==3)
    new.data[1:dim(old.data)[1]+x-dim(old.data)[1],1:dim(old.data)[2],] <- old.data
  #4 - bottomleft
  if (fixcorner==4)
    new.data[1:dim(old.data)[1]+x-dim(old.data)[1],1:dim(old.data)[2]+y-dim(old.data)[2],] <- old.data
  
  # create new netCDF
  con_new <- RNetCDF::create.nc(paste(substr(netcdf,1,nchar(netcdf)-3),"_new.nc",sep=""))
  
  # define dim
  RNetCDF::dim.def.nc(con_new, "time", dimlength = 12)
  RNetCDF::dim.def.nc(con_new, "x", dimlength = x)
  RNetCDF::dim.def.nc(con_new, "y", dimlength = y)
  
  # define var
  RNetCDF::var.def.nc(con_new, var, "NC_DOUBLE", c("x","y","time"))
  RNetCDF::var.def.nc(con_new, "x", "NC_DOUBLE", "x")
  RNetCDF::var.def.nc(con_new, "y", "NC_DOUBLE", "y")
  RNetCDF::var.def.nc(con_new, "time", "NC_DOUBLE", "time")
  
  # copy attributes
  #var
  RNetCDF::att.copy.nc(con, var, "missing_value", con_new, var)
  RNetCDF::att.copy.nc(con, var, "_FillValue", con_new, var)
  RNetCDF::att.copy.nc(con, var, "long_name", con_new, var)
  RNetCDF::att.copy.nc(con, var, "units", con_new, var)
  #x
  RNetCDF::att.copy.nc(con, "x", "standard_name", con_new, "x")
  RNetCDF::att.copy.nc(con, "x", "long_name", con_new, "x")
  RNetCDF::att.copy.nc(con, "x", "units", con_new, "x")
  #y
  RNetCDF::att.copy.nc(con, "y", "standard_name", con_new, "y")
  RNetCDF::att.copy.nc(con, "y", "long_name", con_new, "y")
  RNetCDF::att.copy.nc(con, "y", "units", con_new, "y")
  #time
  RNetCDF::att.copy.nc(con, "time", "_FillValue", con_new, "time")
  RNetCDF::att.copy.nc(con, "time", "missing_value", con_new, "time")
  RNetCDF::att.copy.nc(con, "time", "long_name", con_new, "time")
  RNetCDF::att.copy.nc(con, "time", "units", con_new, "time")
  
  # parse data
  #time
  t <- RNetCDF::var.get.nc(con, "time")
  RNetCDF::var.put.nc(con_new, "time", t)
  #x
  x_con <- RNetCDF::var.get.nc(con,"x")
  x_new <- seq(x_con[1], x_con[1]+(x-1)*(x_con[2]-x_con[1]), x_con[2]-x_con[1])
  RNetCDF::var.put.nc(con_new, "x", x_new)
  #y 
  y_con <- RNetCDF::var.get.nc(con,"y")
  y_new <- seq(y_con[1]+(y-1)*abs(y_con[2]-y_con[1]), y_con[1], -(y_con[1]-y_con[2]))
  RNetCDF::var.put.nc(con_new, "y", y_new)
  #var
  RNetCDF::var.put.nc(con_new, var, new.data, start = c(1,1,1), count = c(x,y,length(t)))
  
  # close connections
  RNetCDF::close.nc(con)
  RNetCDF::close.nc(con_new)
}
