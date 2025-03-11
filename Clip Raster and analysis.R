##%######################################################%##
#                                                          #
####             Riparian Vegetation changes            ####
#                                                          #
##%######################################################%##

# load all required packages
require(raster)
require(rkt)
require(trend)
require(zoo)
library(raster)
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

##%######################################################%##
#       Process NDVI and MNDWI Rasters                    #
##%######################################################%##

# NDVI rasters
process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/mk_tau_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/mk_tau_clipped.tif"
)

process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_clipped.tif"
)

process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/ts_slope_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/ts_slope_clipped.tif"
)

# MNDWI rasters
process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/mk_tau_w_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/mk_tau_w_clipped.tif"
)

process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/tau_mask005w_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/tau_mask005w_clipped.tif"
)

process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/ts_slopew_proj.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/ts_slopew_clipped.tif"
)

# dem rasters
process_raster(
  "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/ue_DEM_aoi/bas_33n_aoi/UE_DEM_Bas_33N.tif",
  shapefile,
  "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/ue_DEM_aoi/bas_33n_aoi/UE_DEM_Bas_33N_clipped.tif"
)
