#' Retrieve netCDF variable names.
#'
#' \code{mHM_getnetCDFvarnames} is reading variable names from a netCDF file.
#'
#' @param ncfile netCDF file name plus path.
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
#' @keywords
#'
#' @export mHM_getnetCDFvarnames
#'
mHM_getnetCDFvarnames <- function(ncfile)
{
  # extract variable names with netcdf4
    netcdf <- ncdf4::nc_open(netcdf)
    variables <- names(netcdf[["var"]])
    ncdf4::nc_close(netcdf)

  return(variables)
}
