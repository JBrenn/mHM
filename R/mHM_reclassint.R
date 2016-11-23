mHM_reclassint <- function(rst, lookuptbl, tif=F, asc=F, varname="luse")
{
  # raster map --> .acs

  # reclassify values according to lookuptbl
  print("reclassifying")
  rst_reclass <- raster::reclassify(x = rst, rcl = lookuptbl)
  
  # write back .asc and .tif
  if (asc)
  {
    print("writing raster")
    raster::writeRaster(x = rst_reclass, filename = paste(varname,".asc",sep=""), overwrite=TRUE)
  }
  if (tif)
  {
    print("writing geotif")
    raster::writeRaster(x = rst_reclass, filename = paste(varname,".tif",sep=""), overwrite=TRUE)
  }
    
  return(rst_reclass)
}

  # convert corine land cover data to mHM lai input
  # raster map --> .acs
  # reclassify to 3 lai classes: 
  # (1) coniferious forest
  # (2) deciduous forest
  # (3) mixed forest
  # (4) sparesly populated forest
  # (5) sealed water bodys
  # (6) viniculture
  # (7) intensive orchards
  # (8) pastures
  # (9) fields
  #(10) wetlands
  #(11) artificial surfaces
  #(12) arable land (non-irrigated)
  #(13) arable land (irrigated)

  # convert corine land cover data to mHM luse input  
  # reclassify to 3 land use types: 
  # (1) forest
  # (2) impervious
  # (3) pervious