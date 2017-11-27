#' @title enlarge or cut grid
#' @description \code{mHM_enlcutgrid} is enlarging or cutting grids (.asc) according to given column and row number in a working dirctoy to be specified
#' @param wd working directory
#' @param ncol integer, requested number of columns
#' @param nrow integer, requested number of rows
#' @return writting new ,asc file in mHM format
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[raster]{raster}},\code{\link[raster]{getValues}},\code{\link[raster]{setValues}}
#'  \code{\link[mHMr]{mHM_formatASC}}
#' @rdname mHM_enlcutgrid
#' @export mHM_enlcutgrid
#' @importFrom raster raster getValues setValues
#' 
mHM_enlcutgrid <- function(wd, ncol, nrow)
{
  # get all grid files
  files <- dir(wd, full.names = T)
  files <- files[grepl(".asc", files)]
  files <- files[!grepl(".asc.", files)]
  
  # loop over files
  for (i in files)
  {
    # make back up of file
    system(paste("cp ", i, " ", i, ".bak", sep=""))
    # read raster
    rst <- raster::raster(x = i)
    #get raster values
    rst_val <- raster::getValues(rst)
    # make matrix
    rst_mat <- matrix(rst_val, nrow = rst@nrows, ncol = rst@ncols, byrow = TRUE)
    
    ## cols
    if (ncol > rst@ncols) {
      # new ncol > old ncol
      # add NA cols to matrix
      rst_mat_new <- matrix(NA, nrow = nrow(rst_mat), ncol = ncol-ncol(rst_mat))
      rst_mat_new <- cbind(rst_mat, rst_mat_new)
    } else if (ncol > rst@ncols) {
      # new ncol < old ncol
      # cut cols from matrix
      rst_mat_new <- rst_mat[,-c((ncol+1):nrow(rst_mat))]
    } else {
      rst_mat_new <- rst_mat
      print("nothing to do, ncol equal.")
    }
    
    ## rows
    if (nrow > rst@nrows) {
      # new nrow > old nrow
      # add NA rows to matrix
      rst_mat_new_row <- matrix(NA, nrow = nrow-nrow(rst_mat_new), ncol = ncol(rst_mat_new))
      rst_mat_new <- rbind(rst_mat_new_row, rst_mat_new)
    } else if (nrow > rst@nrows) {
      # new nrow < old nrow
      # cut rows from matrix
      rst_mat_new <- rst_mat_new[-c(1:(nrow(rst_mat_new)-nrow)),]
    } else {
      print("nothing to do, nrow equal.")
    }
    
    # create new raster
    # set new extent 
    ext <- rst@extent
    xmx <- ext@xmin + ncol* res(rst)[2]
    ymx <- ext@ymin + nrow* res(rst)[1]
    r <- raster(xmn=ext@xmin,xmx=xmx,ymn=ext@ymin,ymx=ymx,nrow=nrow,ncol=ncol)
    
    res(r) <- c(500,500)
    
    # set new data
    r <- raster::setValues(x = r, values = rst_mat_new)
    
    # write raster
    writeRaster(x = r, filename = i, overwrite=T)
    
    # format asc
    mHMr::mHM_formatASC(inASC = i)
    
    # return
    return(NULL)
  }
}
