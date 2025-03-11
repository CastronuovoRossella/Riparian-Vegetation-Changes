##%######################################################%##
#                                                          #
####             Riparian Vegetation changes            ####
#                                                          #
##%######################################################%##

# Load all required Libraries
library(terra)
library(sf)
library(raster)
library(dplyr)
library(writexl)


##%########################################%#
#                Load Rasters               #
##%#######################################%##

Slope <- rast("E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/ts_slope_clipped.tif")
Tau <- rast("E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_clipped.tif")

# Assess min and max values
print(minmax(Slope))  
print(minmax(Tau))  


##%########################################%#
#             Process Rasters               #
##%#######################################%##

# Process rasters function
Process_rasters <- function(Slope, Tau) {
  
                                           if (!identical(crs(Slope), crs(Tau))) {
                                                                                  cat("CRS mismatch. Reprojecting Tau...\n")
                                                                                  Tau <- project(Tau, crs(Slope))
                                                                                  } else {
                                                                                          cat("CRS match.\n")
                                                                                          }
  
                                           # Handle NA values in Tau
                                             Tau <- classify(Tau, cbind(NA, NA))
                                             cat("NA values in Tau handled.\n")
  
                                           # Mask Slope using Tau
                                             Slope_clipped <- mask(Slope, Tau)
                                             cat("Slope clipped using Tau.\n")
  
                    return(Slope_clipped)
                                       }

# Apply function to get clipped Slope
slope_result <- Process_rasters(Slope, Tau)


##%########################################%#
#             Check surfaces                #
##%#######################################%##

# Calculate area function
calculate_area <- function(raster) {
                                      cell_area <- res(raster)[1] * res(raster)[2]
                                      total_area <- sum(!is.na(values(raster))) * cell_area
                                      return(total_area / 1e6) # Convert to km²
                                    }

# Area calculation before and after masking
Slope_before <- calculate_area(Slope)
cat("Total area before masking (km²):", Slope_before, "\n")
Slope_after <- calculate_area(slope_result)
cat("Total area after masking (km²):", Slope_after, "\n")


##%########################################%#
#             Classify slope                #
##%#######################################%##  

# Slope classification (defining breaks and labels)
slope_breaks <- matrix(c(
                          -Inf, -0.035, 1,
                          -0.035, -0.01, 2,
                          -0.01, 0, 3,
                           0, 0.01, 4,
                           0.01, 0.035, 5,
                           0.035, 0.05, 6,
                           0.05, Inf, 7
                           ), ncol = 3, byrow = TRUE)


Slope_Class <- classify(slope_result, slope_breaks)

# Ricollegare le etichette delle classi
slope_labels <- c("-Inf > x > -0.035",
                  "-0.035 > x > -0.01",
                  "-0.01 > x > 0",
                  "0 > x > 0.01",
                  "0.01 > x > 0.035",
                  "0.035 > x > 0.05",
                  "0.05 > x > Inf")

##%########################################%#
#            Check surfaces by class        #
##%#######################################%##

# Calculate area for each slope class
cell_area_hectares <- res(slope_result)[1] * res(slope_result)[2] / 10000 # Convert to hectares

Slope_with_values <- cbind(values(slope_result), 
                           values(Slope_Class))
Slope_with_values <- Slope_with_values[!is.na(Slope_with_values[, 1]),]

Slope_area_table <- table(factor(Slope_with_values[, 2], 
                                 levels = 1:length(slope_labels)))

class_area_hectares <- Slope_area_table * cell_area_hectares

total_area_hectares <- sum(class_area_hectares)

class_percentage <- (class_area_hectares / total_area_hectares) * 100

pixel_count_table <- table(factor(Slope_with_values[, 2], 
                                  levels = 1:length(slope_labels)))

# Create the final slope area table
Slope_area_tibble <- data.frame(
  SLOPE_CLASS_RANGE = slope_labels,
  SLOPE_AREA_ha = class_area_hectares,
  Percentage = class_percentage,
  PIXEL_COUNT = pixel_count_table[1:length(slope_labels)]
)
print(Slope_area_tibble)


##%########################################%#
#             Classify MK Tau               #
##%#######################################%## 


# Function for mean Tau by slope class
Meantau <- function(Tau, Slope_Class) {
                                       Taubyslope <- zonal(Tau, 
                                                           Slope_Class, 
                                                           fun = "mean", 
                                                           na.rm = TRUE)
            return(Taubyslope)
                                      }

mean_tau_slope <- Meantau(Tau, Slope_Class)
cat("Mean Tau by slope class:\n")
print(mean_tau_slope)

# Classify Tau values (positive/negative)
Tau_classify <- function(Tau) {
                                tau_classified <- classify(Tau, rbind(c(-Inf, 0.1, -1), c(0.1, Inf, 1)))
                return(tau_classified)
                              }

tau_classified <- Tau_classify(Tau)
plot(tau_classified, main = "Tau Reclassified (Negative vs Positive)")


##%########################################%#
#                 Merge data                #
##%#######################################%## 

# Merge the two data frames on the SLOPE_CLASS_RANGE and ts_slope_clipped columns
final_table <- merge(Slope_area_tibble, 
                     mean_tau_slope, 
                     by.x = "SLOPE_CLASS_RANGE", 
                     by.y = "ts_slope_clipped", 
                     all = TRUE)

# Classify Tau values (positive/negative)
final_table$TAU_CLASS <- ifelse(final_table$tau_mask005_clipped > 0, "Positive", 
                                ifelse(final_table$tau_mask005_clipped < 0, "Negative", "NA"))

# Print the final table
print(final_table)



output_path_csv <- "E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/NDVI_surfaces_by_slope.csv"
write.csv(final_table, output_path_csv, row.names = FALSE)
cat("Talbe exported:", output_path_csv, "\n")


