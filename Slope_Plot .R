# Riparian change trend for Basilicata region

# Libraries
library("dplyr")          
library("readr")
library("ggplot2")
library("gridExtra") 
library("readxl")

      

# Import data for MKtrends
Senn <-read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/NDVI_surfaces_by_slope_operate.xlsx")
Senn

# Modifica i dati
Senn <- Senn %>%
  mutate(
    Adjusted_area = ifelse(`mean Tau_mask005` < 0, -Slope_area_ha, Slope_area_ha), # Valori negativi per trend decrescente
    Trend = ifelse(`mean Tau_mask005` < 0, "Decreasing MK Trend", "Increasing MK Trend")
  )

# Ordina l'asse x come nell'Excel
Senn$`Slope Class` <- factor(Senn$`Slope Class`, levels = unique(Senn$`Slope Class`))

# Crea il grafico
p <- ggplot(Senn, aes(x = `Slope Class`, y = Adjusted_area, fill = Trend)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = c("black", "lightgrey")) +
  labs(
    x = "Theil Senn slope classes",
    y = "Surfaces (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) + # Aggiunge l'asse x al centro
  geom_text(aes(label = round(Slope_area_ha, 2), y = Adjusted_area + ifelse(Adjusted_area > 0, 300, -300)), 
            colour = "black", 
            size = 4) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), # Etichette sull'asse x orizzontali
    axis.line.x = element_blank(), # Rimuove la linea sotto l'asse x
    axis.ticks.x = element_blank(), # Rimuove i segni sulle tacche dell'asse x
    axis.line.y = element_line(colour = "black", size = 0.6), # Rende visibile l'asse y
    axis.ticks.y = element_line(colour = "black", size = 0.6), # Rende visibili i segni sull'asse y
    panel.background = element_blank(), # Rimuove lo sfondo del grafico
    panel.grid.major = element_blank(), # Rimuove le linee della griglia principale
    panel.grid.minor = element_blank(), # Rimuove le linee della griglia secondaria
    plot.background = element_blank(), # Rimuove lo sfondo del grafico
    plot.margin = margin(10, 10, 10, 10) # Aggiunge margini al grafico
  )

# Mostra il grafico
print(p)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Import data for MKtrends
Senn <-read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/MNDWI_surfaces_by_slope_operate.xlsx")
Senn

# Modifica i dati
Senn <- Senn %>%
  mutate(
    Adjusted_area = ifelse(`tau_mask005w_clipped` < 0, -SLOPE_AREA_ha.Freq, SLOPE_AREA_ha.Freq), 
    Trend = ifelse(`tau_mask005w_clipped` < 0, "Decreasing MK Trend", "Increasing MK Trend")
  )

# Ordina l'asse x come nell'Excel
Senn$`SLOPE_CLASS_RANGE` <- factor(Senn$`SLOPE_CLASS_RANGE`, levels = unique(Senn$`SLOPE_CLASS_RANGE`))

# Crea il grafico
p <- ggplot(Senn, aes(x = `SLOPE_CLASS_RANGE`, y = Adjusted_area, fill = Trend)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = c("black", "lightgrey")) +
  labs(
    x = "Theil Senn slope classes",
    y = "Surfaces (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.6) + # Aggiunge l'asse x al centro
  geom_text(aes(label = round(SLOPE_AREA_ha.Freq, 2), y = Adjusted_area + ifelse(Adjusted_area > 0, 300, -300)), 
            colour = "black", 
            size = 4) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), # Etichette sull'asse x orizzontali
    axis.line.x = element_blank(), # Rimuove la linea sotto l'asse x
    axis.ticks.x = element_blank(), # Rimuove i segni sulle tacche dell'asse x
    axis.line.y = element_line(colour = "black", size = 0.6), # Rende visibile l'asse y
    axis.ticks.y = element_line(colour = "black", size = 0.6), # Rende visibili i segni sull'asse y
    panel.background = element_blank(), # Rimuove lo sfondo del grafico
    panel.grid.major = element_blank(), # Rimuove le linee della griglia principale
    panel.grid.minor = element_blank(), # Rimuove le linee della griglia secondaria
    plot.background = element_blank(), # Rimuove lo sfondo del grafico
    plot.margin = margin(10, 10, 10, 10) # Aggiunge margini al grafico
  )

# Mostra il grafico
print(p)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# Import data
Slope <- read_excel("E:/DELL/Riparian_Vegetation_Change/Layers/3_MK_Senn_Tests/SOIL_SLOPE_MKTAU_operate.xlsx")

# NA 
Slope <- Slope %>% drop_na(SLOPE_CLASS_RANGE, POSITIVE_TAU_ha, NEGATIVE_TAU_ha)

# sort
Slope$SLOPE_CLASS_RANGE <- factor(Slope$SLOPE_CLASS_RANGE, levels = unique(Slope$SLOPE_CLASS_RANGE))

# df
Slope_long <- Slope %>%
  pivot_longer(cols = c(POSITIVE_TAU_ha, NEGATIVE_TAU_ha), names_to = "Trend", values_to = "Area") %>%
  mutate(Area = ifelse(Trend == "NEGATIVE_TAU_ha", -Area, Area)) %>%
  drop_na(Area)  # Rimuove eventuali NA nella colonna Area

# recode coloumn "Trend"
Slope_long$Trend <- recode(Slope_long$Trend, 
                           "POSITIVE_TAU_ha" = "Positive MK Trend", 
                           "NEGATIVE_TAU_ha" = "Negative MK Trend")

# plot
p <- ggplot(Slope_long, aes(x = SLOPE_CLASS_RANGE, y = Area, fill = Trend)) +
  geom_bar(stat = "identity", position = "identity", colour = "black") +
  scale_fill_manual(values = c("Positive MK Trend" = "lightgrey", "Negative MK Trend" = "black")) +
  labs(
    x = "Soil slope classes",
    y = "Surfaces (ha)"
  ) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.6) + 
  geom_text(aes(label = round(abs(Area), 2), y = Area + ifelse(Area > 0, 300, -300)), 
            colour = "black", size = 4) + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
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

# Mostra il grafico
print(p)


