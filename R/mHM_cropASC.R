# function for .asc preprocessing

#lookup_lai <- read.csv2("/home/jbre/ownCloudUFZ/temp/R/lai_reclass.csv")
#lookup_lc  <- read.csv2("/home/jbre/ownCloudUFZ/temp/R/luse_reclass.csv")

mHM_cropASC <- function(main="dem.asc", path_dir="/home/jbre/Schreibtisch/Jucar_Basin/SAGA/Xuquer/mHMASC100", 
                        crs_espg=23030, lookup_lai, lookup_lc)
{
  EPSG <- rgdal::make_EPSG()
  # Get PROJ.4 information for a particular EPSG code
  crs <- EPSG$prj4[which(EPSG$code==crs_espg)]
  
  # get full path plus names of inpu maps
  maps <- dir(path_dir, full.names = T)
  
  # maps without idgauges
  maps_ <- maps[!grepl("idgauge",maps)]
  
  # create mask
    # read raster
  dem <- raster::raster(file.path(path_dir,main))
  raster::crs(dem) <- as.character(crs)
    # get extent of dem
  dem_ext <- raster::extent(dem)
    # get values of main variable
  dem_val <- raster::values(dem)
  
  rst_ <- list()
  print ("read raster and reproject")
  for (i in maps_)
  {
    print(i)
    # get raster
    rst <- raster::raster(i)
    
    # project rst - end up in same extent
    raster::crs(rst) <- as.character(crs)
    rst <- raster::projectRaster(rst, dem)
    
    # get raster val
    val <- raster::values(rst)
    
    # update dem_val
    dem_val[is.na(val)] <- NA
    
    # save rst
    rst_[[i]] <- rst
  }
  
  for (i in maps) 
  {
    if (any(names(rst_)==i)) {
      rst <- rst_[[i]]
    } else {
      # get variable values
      rst <- raster::raster(i)
      # project rst - end up in same extent
      raster::crs(rst) <- as.character(crs)
      rst <- raster::projectRaster(rst, dem)
    }

    # check for integer
    if (grepl("facc",i) | grepl("fdir",i) | grepl("lc",i) | grepl("class",i) | grepl("geology",i) | 
        grepl("LAI",i) | grepl("soil",i) | grepl("idgauges",i)) 
      raster::values(rst) <- as.integer(round(raster::values(rst), 0))
    
    # check for fdir range
    if (grepl("fdir",i) & max(raster::values(rst), na.rm=T) < 8)
    {
      rst[which(raster::values(rst) == 2)]  <- 1
      rst[which(raster::values(rst) == 3)]  <- 2
      rst[which(raster::values(rst) == 4)]  <- 4
      rst[which(raster::values(rst) == 5)]  <- 8
      rst[which(raster::values(rst) == 6)]  <- 16
      rst[which(raster::values(rst) == 7)]  <- 32
      rst[which(raster::values(rst) == 0)]  <- 64
      rst[which(raster::values(rst) == 1)]  <- 128
    }
    
    # reclassify from corine landcover  
    if (grepl("lc", i) & max(raster::values(rst), na.rm=T) > 3) 
    {
      #lookup <- read.csv2("../luse_reclass.csv")
      lookup <- lookup_lc
      rst <- mHM_reclassint(rst = rst, lookuptbl = lookup)
    }
    
    if (grepl("LAI", i) & max(raster::values(rst), na.rm=T) > 13) 
    {
      #lookup <- read.csv2("../lai_reclass.csv")
      lookup <- lookup_lai
      rst <- mHM_reclassint(rst = rst, lookuptbl = lookup)
    }
    
    # mask with updated dem values
    rst[which(is.na(dem_val))] <- NA
    #values(rst) <- round(values(rst), 3)
    
    # write .asc
    fname <- paste(substr(i,1,nchar(i)-4),".asc",sep="")
    write2 <- fname 
    print(paste("write ", fname, sep=" "))
    raster::writeRaster(x = rst, filename = write2, NAflag = -9999, overwrite=TRUE)
    
    # correct header format
    system(paste("sed -i -e 's/.000000000000000 / /g' ", write2, sep=""))
    system(paste("sed -i -e 's/NCOLS/ncols/g' ", write2, sep=""))
    system(paste("sed -i -e 's/NROWS/nrows/g' ", write2, sep=""))
    system(paste("sed -i -e 's/NCOLS/ncols/g' ", write2, sep=""))
    system(paste("sed -i -e 's/XLLCORNER/xllcorner/g' ", write2, sep=""))
    system(paste("sed -i -e 's/YLLCORNER/yllcorner/g' ", write2, sep=""))
    system(paste("sed -i -e 's/CELLSIZE/cellsize/g' ", write2, sep=""))
  }
  
  # delte -e files
  system(paste("cd ", path_dir, ";rm *.asc-e", sep=""))
}