##%######################################################%##
#                 Riparian Vegetation Changes             #
##%######################################################%##

# load all required packages
require(raster)
require(rkt)
require(trend)
require(zoo)
library(sf)

rasterOptions(maxmemory=1e+06, chunksize=1e+07, progress = 'text')

# Load slope raster
PEND <- rast("E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/penqgisrecclipped.tif")
print(minmax(PEND))

# Mask to remove NA values
PEND <- mask(PEND, PEND, maskvalue = NA)

# Limit range to 0-100%
PEND_limited <- clamp(PEND, lower = 0, upper = 100)

# Define reclassification matrix
rcl <- matrix(c(
  0, 5, 5,    # 0-5% -> 5
  5, 15, 15,  # 5-15% -> 15
  15, 30, 30, # 15-30% -> 30
  30, 60, 60, # 30-60% -> 60
  60, 100, 100 # >60% -> 100
), ncol = 3, byrow = TRUE)

# Classify the slope raster
Slope_Class <- classify(PEND_limited, rcl = rcl)

# Plot the classified raster
plot(Slope_Class, main = "Classificazione della Pendenza", col = terrain.colors(5), 
     legend.args = list(text = "Classe di Pendenza", side = 3))

# Calculate the area of each raster cell in hectares
cell_area_hectares <- res(Slope_Class)[1] * res(Slope_Class)[2] / 10000  

# Define slope class labels
slope_labels <- c("0-5% (Pianeggiante)", 
                  "5-15% (Leggermente inclinato)", 
                  "15-30% (Pendii moderati)", 
                  "30-60% (Zone ripide)", 
                  ">60% (Zone molto ripide)")

# Calculate the frequency of each class
freq_table <- freq(Slope_Class, digits = 0)  # Frequency table
freq_table <- na.omit(freq_table)  # Remove rows with NA, if any

# Map the reclassified values to labels
valid_classes <- unique(rcl[, 3])  # Extract valid class values from reclassification matrix
filtered_freq_table <- freq_table[freq_table$value %in% valid_classes, ]

# Check if the filtered frequency table matches the number of labels
if (nrow(filtered_freq_table) != length(slope_labels)) {
  stop("Mismatch between slope class labels and classified values in the raster.")
}

# Calculate the area for each class in hectares
class_area_hectares <- filtered_freq_table[, "count"] * cell_area_hectares

# Calculate the total area
total_area_hectares <- sum(class_area_hectares)

# Calculate the percentage of area for each class
class_percentage <- (class_area_hectares / total_area_hectares) * 100

# Create the final table with class labels
Slope_area_tibble <- data.frame(
  SLOPE_CLASS_RANGE = slope_labels,
  SLOPE_AREA_ha = class_area_hectares,
  Percentage = class_percentage,
  PIXEL_COUNT = filtered_freq_table[, "count"]
)

# Print the final table
print(Slope_area_tibble)

# Save the classified raster
classified_slope_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/4_UE-DEM/penqgisrecclipped_Classified.tif"
writeRaster(Slope_Class, classified_slope_path, overwrite = TRUE)
cat("Raster classificato salvato in:", classified_slope_path, "\n")



# Create the final table with class labels
Slope_area_tibble <- data.frame(
  SLOPE_CLASS_RANGE = slope_labels,
  SLOPE_AREA_ha = class_area_hectares,
  Percentage = class_percentage,
  PIXEL_COUNT = filtered_freq_table[, "count"]
)

# Print the final table
print(Slope_area_tibble)

# Save the tibble as a CSV file
tibble_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/Slope_area_tibble.csv"
write.csv(Slope_area_tibble, file = tibble_path, row.names = FALSE)
cat("Tibble salvato in:", tibble_path, "\n")


##%########################################%#
#             Classify MK Tau               #
##%#######################################%## 


Tau <- rast("E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_clipped.tif")
Slope_Class <- resample(Slope_Class, Tau, method = "near") # Allinea la risoluzione
  

# Funzione per calcolare l'area di pixel Tau positivi e negativi per classe di pendenza
AreaTau <- function(Tau, Slope_Class) {
  # Classifica Tau in positivo e negativo
  tau_positive <- classify(Tau, rbind(c(-Inf, 0.1, 0), c(0.1, Inf, 1)))  # Tau positivo
  tau_negative <- classify(Tau, rbind(c(-Inf, 0.1, 1), c(0.1, Inf, 0)))  # Tau negativo
  
  cell_area_hectares <- (res(Tau)[1] * res(Tau)[2]) / 10000  # Converting from m² to hectares (1 hectare = 10,000 m²)
  
  # Calcola le aree in ettari
  positive_area <- zonal(tau_positive, Slope_Class, fun = "sum", na.rm = TRUE) * cell_area_hectares
  negative_area <- zonal(tau_negative, Slope_Class, fun = "sum", na.rm = TRUE) * cell_area_hectares
  
  return(list(positive_area = positive_area, negative_area = negative_area))
}

# Calcola l'area per Tau positivo e negativo
area_tau <- AreaTau(Tau, Slope_Class)
area_tau

# Estrai i valori di area per Tau positivo e negativo
positive_area_hectares <- area_tau$positive_area
negative_area_hectares <- area_tau$negative_area

# Crea la tabella finale con le aree di Tau positivi e negativi
final_table <- data.frame(
  SLOPE_CLASS_RANGE = slope_labels,
  POSITIVE_TAU_AREA_ha = positive_area_hectares,
  NEGATIVE_TAU_AREA_ha = negative_area_hectares
)

# Stampa la tabella finale
print(final_table)

# Salva la tabella finale come CSV
final_table_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/SOIL_SLOPE_MKTAU.csv"
write.csv(final_table, file = final_table_path, row.names = FALSE)
cat("Tabella finale salvata in:", final_table_path, "\n")



