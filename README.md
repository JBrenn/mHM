=======
# mHMr

R package for pre- and postprocessing of the mesoscale hydrological model (mHM)
*   pre-processing
    
    mHM_checkT: check on consistency of minimum and maximum air temperature (netCDF format).
    
    mHM_extraploMat: simple extrapolation of 1D-arrays in invalid locations.
    
    mHM_formatASC: reformat ASCII file. 
    
    mHM_maskRaster: mask raster maps with same projection and extent. 
    
    mHM_prepdailyQ: prepare daily discharge files for mHM. 
    
    mHM_reclassint: reclassify raster with lookuptable.
    
    mHM_writeHeader: write mHM header file. 
    
*   post-processing

    mHM_GOF4period: calculate GOFs for time intervals.

    mHM_readnml: read mHM simulation namelist mhm.nml.
    
    mHM_readQ:  read mHM daily_discharge.out in zoo object.
    
=======

# How to start

install the package and load:

```R
install.packages("devtools")
library(devtools)
install_github("JBrenn/mHMr")
library(mHMr)
```
