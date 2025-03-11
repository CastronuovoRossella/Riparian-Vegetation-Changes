##%######################################################%##
#                 Riparian Vegetation Changes             #
##%######################################################%##

# load all required packages
require(raster)
require(rkt)
require(trend)
require(zoo)
library(terra)
library(sf)

rasterOptions(maxmemory=1e+06, chunksize=1e+07, progress = 'text')

##%######################################################%##
#                Load shapefile AOI                        #
##%######################################################%##

shapefilepath <- "E:/DELL/Riparian_Vegetation_Change/Layers/1_AOI/AOI/Area_studio_finale_PROJ_wgs.shp"
shapefile <- st_read(shapefilepath)

# Print extent and CRS for shapefile
cat("Shapefile AOI:\n")
cat("Extent:\n")
print(st_bbox(shapefile))
cat("CRS:\n")
print(st_crs(shapefile))

# Assess AOI area
shapefile$area_m2 <- st_area(shapefile)                            # square meters
shapefile$area_ha <- as.numeric(shapefile$area_m2) / 10000         # hectares
AOI_ha <- sum(shapefile$area_ha)
cat("Total AOI area:", AOI_ha, "hectares.\n")

##%######################################################%##
#             Derive slope starting by DEM                #
##%######################################################%##

# Load DEM
raster_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/ue_DEM_aoi/bas_33n_aoi/UE_DEM_Bas_33N.tif"
DEM <- raster(raster_path)
 
# Check raster
   cat("Raster Extent:\n")
   print(extent(DEM))
   cat("Raster CRS:\n")
   print(crs(DEM))

# Slope in percentage
  cat("Calcolo della pendenza...\n")
  Slope <- terrain(DEM, v = "slope", unit = "degrees")
  Slope_percent <- tan(Slope * pi / 180) * 100
  cat("Calcolo della pendenza completato.\n")
  summary(Slope_percent)

# Save
slope_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/SOILSLOPEBAS.tif"
writeRaster(Slope_percent, slope_path, format = "GTiff", overwrite = TRUE)
cat("Pendenza non clippata salvata in:", slope_path, "\n")


##%######################################################%##
#       Function to Clean, Align, Clip, and Save Raster    #
##%######################################################%##

process_raster <- function(raster_path, shapefile, output_path) {
  # Load raster
  raster <- raster(raster_path)
  cat("\nProcessing raster:", raster_path, "\n")
  
  # Replace NoData with NA (skip 0, as it is a valid value)
  raster <- calc(raster, fun = function(x) { x[x == -Inf | is.na(x)] <- NA; return(x) })
  cat("NoData values replaced with NA (excluding 0).\n")
  
  # Print extent and CRS of raster
  cat("Raster Extent:\n")
  print(extent(raster))
  cat("Raster CRS:\n")
  print(crs(raster))
  
  # Align projection to shapefile
  shapefile_crs <- st_crs(shapefile)$proj4string  # Extract the CRS string for comparison
  raster_crs <- crs(raster)  # Get CRS for raster
  
  # Check if the CRS of raster and shapefile match
  if (!identical(shapefile_crs, raster_crs)) {
    cat("Raster projection does not match AOI, reprojecting...\n")
    raster <- projectRaster(raster, crs = shapefile_crs)
    cat("Raster projection aligned to match AOI.\n")
  } else {
    cat("Raster projection already matches AOI.\n")
  }
  
  # Clip raster to AOI extent
  raster_clipped <- crop(raster, extent(shapefile))
  raster_clipped <- mask(raster_clipped, shapefile)
  
  # Save the clipped raster
  writeRaster(raster_clipped, filename = output_path, format = "GTiff", overwrite = TRUE)
  cat("Clipped raster saved to:", output_path, "\n")
}

##%##############################################
#       Process DEM Rasters                    #
##%##############################################

# dem rasters
process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/SOILSLOPEBAS.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/SOILSLOPEBASclipped.tif"
)




