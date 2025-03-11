##%######################################################%##
#                                                          #
####      Pixel based trends correlation analysis       ####     
#                                                          #
##%######################################################%##

# Load Library
              library(terra)
              library(ggplot2)
              library(patchwork)
              library(tidyr)
              library(dplyr)
              library(tidyverse)
              library(sf)

##%######################################################%##
#                Load shapefile AOI                        #
##%######################################################%##

shapefilepath <- "E:/DELL/Riparian_Vegetation_Change/Layers/1_AOI/AOI/Area_studio_finale_PROJ_wgs.shp"
shapefile <- st_read(shapefilepath)

# Assess AOI area
shapefile$area_m2 <- st_area(shapefile)                            # square meters
shapefile$area_ha <- as.numeric(shapefile$area_m2) / 10000         # hectares
AOI_ha <- sum(shapefile$area_ha)
cat("Total AOI area:", AOI_ha, "hectares.\n")


##%######################################################%##
#                  Load raster mk                          #
##%######################################################%##

raster_paths <- c(
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/mk_tau_clipped.tif",  
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_clipped.tif",
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/ts_slope_clipped.tif",
  
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/mk_tau_w_clipped.tif",
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/tau_mask005w_clipped.tif",
  "E:/DELL/Riparian_Vegetation_Change/Layers/2_MNDWI_Max/Results/ts_slopew_clipped.tif"
                  )

###%######################################################%#
#                  Process and classify trends             #
##%######################################################%##

# Initialise results tibble
Raster_area <- tibble(
  Raster_Name = character(),
  Area_Ha = numeric(),
  Ratio_To_AOI = numeric(),
  Positive_Area_Ha = numeric(),
  Negative_Area_Ha = numeric(),
  Ratio_Positive = numeric(),
  Ratio_Negative = numeric(),
  Min_Value = numeric(),
  Max_Value = numeric(),
  Median_Value = numeric(),
  Mean_Value = numeric()
)

# Define the threshold values
threshold_tau_mask005 <- 0.1  # Threshold for tau_mask005
threshold_tau_mask005w <- 0   # Threshold for tau_mask005w

# Process each raster
for (raster_path in raster_paths) {
  raster <- rast(raster_path)  
  
  # Compute raster resolution and valid cell count
  raster_resolution <- res(raster)                               # Raster resolution
  cell_area_m2 <- raster_resolution[1] * raster_resolution[2]    # Cell area in square meters
  num_cells <- sum(!is.na(values(raster)))                       # Count non-NA cells 
  
  # Calculate raster area in hectares
  raster_area_m2 <- num_cells * cell_area_m2
  raster_area_ha <- raster_area_m2 / 10000 
  
  # Calculate ratio to AOI
  ratio_to_aoi <- raster_area_ha / AOI_ha
  
  # Initialize results for trend variables
  positive_area_ha <- 0
  negative_area_ha <- 0
  ratio_positive <- 0
  ratio_negative <- 0
  
  # Extract raster values for statistics
  raster_values <- values(raster, na.rm = TRUE)
  
  # Calculate the statistics: min, max, median, mean
  min_value <- min(raster_values, na.rm = TRUE)
  max_value <- max(raster_values, na.rm = TRUE)
  median_value <- median(raster_values, na.rm = TRUE)
  mean_value <- mean(raster_values, na.rm = TRUE)
  
  ###%######################%#
  # Process NDVI Trends
  # Trend classification thresholds
  # Classify trends for NDVI (threshold 0.1)
  
  if (basename(raster_path) == "tau_mask005_clipped.tif") {
    positive_trends <- sum(raster_values > threshold_tau_mask005, na.rm = TRUE)
    negative_trends <- sum(raster_values <= threshold_tau_mask005, na.rm = TRUE)
    
    positive_area_ha <- positive_trends * cell_area_m2 / 10000
    negative_area_ha <- negative_trends * cell_area_m2 / 10000
    ratio_positive <- positive_area_ha / raster_area_ha
    ratio_negative <- negative_area_ha / raster_area_ha
  } 
  
  else if (basename(raster_path) == "tau_mask005w_clipped.tif") {
    positive_trends <- sum(raster_values > threshold_tau_mask005w, na.rm = TRUE)
    negative_trends <- sum(raster_values <= threshold_tau_mask005w, na.rm = TRUE)
    
    positive_area_ha <- positive_trends * cell_area_m2 / 10000
    negative_area_ha <- negative_trends * cell_area_m2 / 10000
    ratio_positive <- positive_area_ha / raster_area_ha
    ratio_negative <- negative_area_ha / raster_area_ha
  }
  
  # Append results to tibble
  Raster_area <- Raster_area %>% add_row(
    Raster_Name = basename(raster_path),
    Area_Ha = raster_area_ha,
    Ratio_To_AOI = ratio_to_aoi,
    Positive_Area_Ha = positive_area_ha,
    Negative_Area_Ha = negative_area_ha,
    Ratio_Positive = ratio_positive,
    Ratio_Negative = ratio_negative,
    Min_Value = min_value,
    Max_Value = max_value,
    Median_Value = median_value,
    Mean_Value = mean_value
  )
  
  # Print raster details
  cat("Raster:", basename(raster_path), 
      "- Area (ha):", raster_area_ha, "\n")
  cat("Positive Area (ha):", positive_area_ha, 
      "Negative Area (ha):", negative_area_ha, "\n")
}


output_path_csv <- "E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/Rasters_Area_Analysis.csv"
write.csv(Raster_area, output_path_csv, row.names = FALSE)
cat("Talbe exported:", output_path_csv, "\n")






