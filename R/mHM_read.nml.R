#' Read mHM daily_discharge.out in zoo object.
#' 
#' @param simpath path to mHM simulation folder, where mhm.nml is located.
#' @return list of mHM namelist entries.
#' @examples
#' nml <- mHM_read.nml(simpath="your/path")
#' 
#' @export mHM_read.nml

mHM_read.nml <- function(simpath)
{
  nml_lines <- readr::read_lines(file = file.path(simpath, "mhm.nml"))
  
  # remove empty lines and out-commented ones
  nml_lines <- nml_lines[-which(grepl("!",substr(nml_lines,1,1)))]
  nml_lines <- nml_lines[-which(nchar(substr(nml_lines,1,1))<1)]
  
  # namelist names and start
  nml_namelists_start <- which(grepl("&",substr(nml_lines,1,1)))
  #nml_namelists_end   <- which(grepl("/",substr(nml_lines,1,1)))
  nml_names <- sapply(nml_namelists_start, FUN = function(x) substr(nml_lines[x],2,nchar(nml_lines[x])))
  
  forloop <- c(nml_namelists_start,length(nml_lines)+1)
  # read out namelist values
  nml <- list()
  for (i in 1:(length(forloop)-1))
  {
    #block i
    block_i <- nml_lines[(forloop[i]+1):((forloop[i+1])-2)]
    nml[[nml_names[i]]] <- sapply(X = block_i, FUN = chr2vec, USE.NAMES = F)
  }
  
  return(nml)
}
