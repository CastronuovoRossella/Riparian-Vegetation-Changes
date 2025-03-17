##%######################################################%##
#                 Riparian Vegetation Changes             #
##%######################################################%##

# Load required packages
require(raster)
require(terra)
require(rkt)
require(trend)
require(zoo)
library(sf)
library(dplyr)

# Load the hydro buffer shapefile
hydro_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/4. EU_Hydro/Hydro_buffer.shp"
hydro <- st_read(hydro_path)

# Reproject to WGS 84 / UTM zone 33N (EPSG:32633)
hydro <- st_transform(hydro, crs = 32633)

# Retrieve the STRAHLER classification column
classification_column <- "STRAHLER"  
if (!(classification_column %in% colnames(hydro))) {
  stop("Error: The 'STRAHLER' column does not exist in the shapefile.")
}

# Print unique STRAHLER classes
unique_classes <- unique(hydro[[classification_column]])
cat("STRAHLER Classes Present:\n")
print(unique_classes)

# Merge STRAHLER classes from 6 to 26 into "WB" (Water Bodies)
hydro_merged <- hydro %>%
  mutate(!!classification_column := ifelse(!!sym(classification_column) >= 6 & 
                                             !!sym(classification_column) <= 26, 
                                           "WB", 
                                           as.character(!!sym(classification_column))))

# Print the new class distribution
cat("New classes after merging:\n")
print(unique(hydro_merged[[classification_column]]))

##%######################################################%##
#                    Spatial Analysis                     #
##%######################################################%##

# Load the reclassified Tau raster (+1 for positive trends, -1 for negative trends)
Tau <- rast("E:/DELL/Riparian_Vegetation_Change/Layers/2_NDVI_Max/Results/tau_mask005_clipped_reclass.tif")

# Function to compute Tau-positive (+1) and Tau-negative (-1) area for each STRAHLER class
AreaTau <- function(Tau, Hydro_Merged) {
  results <- data.frame(STRAHLER_CLASS = character(),
                        POSITIVE_TAU_AREA_ha = numeric(),
                        NEGATIVE_TAU_AREA_ha = numeric(),
                        stringsAsFactors = FALSE)
  
  # Compute pixel area in hectares
  cell_area_hectares <- (res(Tau)[1] * res(Tau)[2]) / 10000  # Convert m² to hectares
  
  for (class in unique(Hydro_Merged$STRAHLER)) {
    # Extract the polygon for the current STRAHLER class
    class_polygon <- Hydro_Merged %>% filter(STRAHLER == class)
    
    # Check if class_polygon exists
    if (nrow(class_polygon) == 0) {
      next  # Skip to next class if no polygon is found
    }
    
    # Mask the Tau raster to the STRAHLER class polygon
    Tau_masked <- mask(Tau, vect(class_polygon))
    
    # Count Tau-positive (+1) and Tau-negative (-1) pixels
    positive_count <- sum(values(Tau_masked) == 1, na.rm = TRUE)
    negative_count <- sum(values(Tau_masked) == -1, na.rm = TRUE)
    
    # Compute area in hectares
    positive_area <- positive_count * cell_area_hectares
    negative_area <- negative_count * cell_area_hectares
    
    # Append results
    results <- rbind(results, data.frame(
      STRAHLER_CLASS = class,
      POSITIVE_TAU_AREA_ha = positive_area,
      NEGATIVE_TAU_AREA_ha = negative_area
    ))
  }
  
  return(results)
}

# Compute Tau-positive and Tau-negative area per STRAHLER class
final_table <- AreaTau(Tau, hydro_merged)

# Print final table
print(final_table)

# Save the final table as CSV
final_table_path <- "E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/STRAHLER_TAU_AREA.csv"
write.csv(final_table, file = final_table_path, row.names = FALSE)
cat("Final table saved at:", final_table_path, "\n")

# Load required packages
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# Import data
Hydro <- read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/STRAHLER_TAU_AREA_OPERATE.xlsx")

# Reshape data: Convert wide format into long format
hydro_long <- Hydro %>%
  pivot_longer(cols = c(POSITIVE_TAU_AREA_ha, NEGATIVE_TAU_AREA_ha), 
               names_to = "Trend", values_to = "Area") %>%
  mutate(Area = ifelse(Trend == "NEGATIVE_TAU_AREA_ha", -Area, Area)) %>%
  drop_na(Area)  # Remove any NA values in the Area column

# Recode column "Trend" for better labels
hydro_long$Trend <- recode(hydro_long$Trend, 
                           "POSITIVE_TAU_AREA_ha" = "Positive MK Trend", 
                           "NEGATIVE_TAU_AREA_ha" = "Negative MK Trend")

# Define Y-axis limits rounded to the nearest multiple of 300
y_min <- floor(min(hydro_long$Area) / 300) * 300
y_max <- ceiling(max(hydro_long$Area) / 300) * 300

# Set label offset to bring them closer to bars
label_offset <- 150  # Reduce offset from 300 to 150 for better positioning

# Plot
p <- ggplot(hydro_long, aes(x = STRAHLER_CLASS, y = Area, fill = Trend)) +
  geom_bar(stat = "identity", position = "identity", aes(colour = Trend), size = 0.5) +  # Add black outline
  scale_fill_manual(values = c("Positive MK Trend" = "white", "Negative MK Trend" = "black")) +  # White for positive bars
  scale_color_manual(values = c("Positive MK Trend" = "black", "Negative MK Trend" = "black")) +  # Black outline for both
  labs(
    x = "Strahler Order Classes",
    y = "Surface Area (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.6) +  # Reference line at y = 0
  geom_text(aes(label = round(abs(Area), 2), y = Area + ifelse(Area > 0, label_offset, -label_offset)), 
            colour = "black", size = 3) +  # **Smaller text & closer labels**
  scale_y_continuous(breaks = seq(y_min, y_max, by = 300)) +  # Set Y-axis breaks every 300 units
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Center X-axis text
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(colour = "black", linewidth = 0.6),
    axis.ticks.y = element_line(colour = "black", linewidth = 0.6),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Show the plot
print(p)

# Path to save the image
desktop_path <- "C:/Users/rosse/OneDrive/Desktop/"

# Define image dimensions and quality
width_px <- 12000   
height_px <- 11000  
dpi <- 1200         

# Save the PNG directly to the Desktop
png(paste0(desktop_path, "hydro_Plot_HighQuality.png"), 
    width = width_px, height = height_px, res = dpi, type = "cairo-png")

# Print the plot
print(p)

# Close the graphics device to properly save the file
dev.off()
