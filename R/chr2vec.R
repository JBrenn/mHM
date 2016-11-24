#' Helper for mHM_read.nml
#' 
#' @param chr vector of characters to be splited and trimed.
#' 
#' @return splited, trimed characters in list.
#' 
#' @examples
#' chr2vec()
#' 
#' @author Johannes Brenner \email{johannes.brenner@ufz.de}
#' 
#' @references
#' 
#' @seealso
#' 
#' @keywords
#' 
#' @export chr2vec

chr2vec <- function(chr)
{
  # split string by "="
  strs <- stringr::str_split(string = chr, pattern = "=")[[1]]
  # trim string
  strs <- stringr::str_trim(string = strs)
  
  # dummy list
  out <- list()
  # detect string type
  if (strs[2]==".FALSE." | strs[2]==".False." | strs[2]==".false.") {
    # boolean (FALSE)
    out[[strs[1]]] <- FALSE
  } else if (strs[2]==".TRUE."  | strs[2]==".True."  | strs[2]==".true.") {
    # boolean (TRUE)
    out[[strs[1]]] <- TRUE
  } else if (grepl(pattern = ",", x = strs[2])) {
    # numeric vector
    str_num <- stringr::str_trim(stringr::str_split(string = strs[2], pattern = ",")[[1]])
    out[[strs[1]]] <- as.numeric(str_num)
  } else {
    if (is.na(as.numeric(strs[2]))) {
      # character
      out[[strs[1]]] <- gsub(pattern = "\"", replacement = "", x = strs[2])
    } else {
      # numeric
      out[[strs[1]]] <- as.numeric(strs[2])
    }
  }
  # give back list
  return(out)
}
