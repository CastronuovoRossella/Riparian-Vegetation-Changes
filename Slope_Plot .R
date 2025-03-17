# Riparian change trend for Basilicata region

# Libraries
library("dplyr")          
library("readr")
library("ggplot2")
library("gridExtra") 
library("readxl")
  
# Import data 
Senn <-read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/NDVI_surfaces_by_slope_operate.xlsx")
Senn

# # Reshape data
Senn <- Senn %>%
  mutate(
    Adjusted_area = ifelse(`mean Tau_mask005` < 0, -Slope_area_ha, Slope_area_ha), 
    Trend = ifelse(`mean Tau_mask005` < 0, "Decreasing MK Trend", "Increasing MK Trend")
  )

Senn$`Slope Class` <- factor(Senn$`Slope Class`, levels = unique(Senn$`Slope Class`))

# plot
p <- ggplot(Senn, aes(x = `Slope Class`, y = Adjusted_area, fill = Trend)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = c("black", "lightgrey")) +
  labs(
    x = "Theil Senn slope classes",
    y = "Surfaces (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) + 
  geom_text(aes(label = round(Slope_area_ha, 2), y = Adjusted_area + ifelse(Adjusted_area > 0, 300, -300)), 
            colour = "black", 
            size = 4) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_line(colour = "black", size = 0.6),
    axis.ticks.y = element_line(colour = "black", size = 0.6), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_blank(), 
    plot.margin = margin(10, 10, 10, 10) 
  )

print(p)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Import data for MKtrends
Senn <-read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/MNDWI_surfaces_by_slope_operate.xlsx")
Senn

# Reshape data
Senn <- Senn %>%
  mutate(
    Adjusted_area = ifelse(`tau_mask005w_clipped` < 0, -SLOPE_AREA_ha.Freq, SLOPE_AREA_ha.Freq), 
    Trend = ifelse(`tau_mask005w_clipped` < 0, "Decreasing MK Trend", "Increasing MK Trend")
  )

# Reshape data
Senn$`SLOPE_CLASS_RANGE` <- factor(Senn$`SLOPE_CLASS_RANGE`, levels = unique(Senn$`SLOPE_CLASS_RANGE`))

# Create the plot
p <- ggplot(Senn, aes(x = `SLOPE_CLASS_RANGE`, y = Adjusted_area, fill = Trend)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = c("black", "lightgrey")) +
  labs(
    x = "Theil Senn slope classes",
    y = "Surfaces (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) + 
  geom_text(aes(label = round(SLOPE_AREA_ha.Freq, 2), y = Adjusted_area + ifelse(Adjusted_area > 0, 300, -300)), 
            colour = "black", 
            size = 4) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_line(colour = "black", size = 0.6), 
    axis.ticks.y = element_line(colour = "black", size = 0.6), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_blank(), 
    plot.margin = margin(10, 10, 10, 10) 
  )


print(p)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load required packages
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# Import data
Slope <- read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/SOIL_SLOPE_MKTAU_operate.xlsx")

# Remove NA values
Slope <- Slope %>% drop_na(SLOPE_CLASS_RANGE, POSITIVE_TAU_ha, NEGATIVE_TAU_ha)

# Ensure correct sorting of factor levels
Slope$SLOPE_CLASS_RANGE <- factor(Slope$SLOPE_CLASS_RANGE, levels = unique(Slope$SLOPE_CLASS_RANGE))

# Reshape data from wide to long format
Slope_long <- Slope %>%
  pivot_longer(cols = c(POSITIVE_TAU_ha, NEGATIVE_TAU_ha), names_to = "Trend", values_to = "Area") %>%
  mutate(Area = ifelse(Trend == "NEGATIVE_TAU_ha", -Area, Area)) %>%
  drop_na(Area)  # Remove any NA values in the Area column

# Recode column "Trend" for better labels
Slope_long$Trend <- recode(Slope_long$Trend, 
                           "POSITIVE_TAU_ha" = "Positive MK Trend", 
                           "NEGATIVE_TAU_ha" = "Negative MK Trend")

# Define Y-axis limits rounded to the nearest multiple of 300
y_min <- floor(min(Slope_long$Area) / 300) * 300
y_max <- ceiling(max(Slope_long$Area) / 300) * 300

# Set label offset to bring them closer to bars
label_offset <- 150  # Reduced from 300 for better positioning

# Create the plot
p <- ggplot(Slope_long, aes(x = SLOPE_CLASS_RANGE, y = Area, fill = Trend)) +
  geom_bar(stat = "identity", position = "identity", aes(colour = Trend), size = 0.5) +  # Thin black outline
  scale_fill_manual(values = c("Positive MK Trend" = "white", "Negative MK Trend" = "black")) +  # White fill for positive bars
  scale_color_manual(values = c("Positive MK Trend" = "black", "Negative MK Trend" = "black")) +  # Black outline for both
  labs(
    x = "Soil Slope Classes",
    y = "Surface Area (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.6) +  # Reference line at y = 0
  geom_text(aes(label = round(abs(Area), 2), y = Area + ifelse(Area > 0, label_offset, -label_offset)), 
            colour = "black", size = 3) +  # **Smaller text & closer labels**
  scale_y_continuous(breaks = seq(y_min, y_max, by = 300)) +  # Y-axis breaks every 300 units
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


