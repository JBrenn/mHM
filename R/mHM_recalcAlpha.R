#' Back-calculate $alpha$. 
#' 
#' \code{mHM_recalcAlpha} is back-calculating the \eqn{\alpha} parameter for one soil layer \eqn{i} of a mHM model simulation. 
#' Back-calculation is based on soil water content (\eqn{x_{3}}), permanent wilting point (\eqn{\beta_{15}}) and porosity (\eqn{\beta_{16}}). 
#' 
#' \eqn{\alpha} is calculated as follows: 
#' \deqn{\alpha = \frac{x_{3}(t-1) -  \beta_{15}}{\beta_{16} - \beta_{15}}}
#' 
#' @param swc array 3d, containing soil moisture data [mm] for soil layer \eqn{i}. Third dimension is time.
#' @param pwp array 2d, containing permanent wilting point [mm] for soil layer \eqn{i}.
#' @param sat array 2d, containing soil moisture limit (saturation, porosity) [mm] for soil layer \eqn{i}.
#' 
#' @return array of $alpha$ values for soil layer \eqn{i}.
#' 
#' @examples
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso \code{\link{mHM_vargetmulti}}
#' 
#' @keywords
#'  
#' @export mHM_recalcAlpha
#'
#'
mHM_recalcAlpha <- function(swc, pwp, sat)
{
  # loop over time
  alpha <- swc
  for (i in 1:dim(swc)[3])
  {
    # 
    alpha[,,i] = (swc[,,i] - pwp) / (sat - pwp)
  }
  
  return(alpha)
}