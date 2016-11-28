#' get multiple variables from netCDF. 
#' 
#' \code{mHM_vargetmulti} is retrieving multiple variables from a netCDF connection.
#' 
#' 
#' @param con connection to netCDF.
#' @param vars variable names.
#' 
#' @return list containing arrays of variables.
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
#' @export mHM_vargetmulti
#'
#'
mHM_vargetmulti <- function(con, vars)
{
  # dummy
  var_list <- list()
  
  for (i in vars)
  {
    # read variable from connection
    var_list[[i]] <- RNetCDF::var.get.nc(ncfile = con, variable = i)
  }
  
  return(var_list)
}