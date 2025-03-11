# ______________________________#
# Serial Correlation Assessment #
# ______________________________#

# Load Libraries
library(raster)

# Memory efficiency
                rasterOptions(maxmemory = 1e+06, 
                              chunksize = 1e+07, 
                              progress = 'text')

# Load NDVI stack 
                NDVIStack <- stack(list.files(
                                    path = "E:/DELL/Riparian_Vegetation_Change/Layers/4. GEE trend",
                                    pattern = ".*.tif$", 
                                    full.names = TRUE, 
                                    recursive = TRUE
                                   )[1:24])

# Convert raster stack to matrix
               NDVI_values <- as.matrix(NDVIStack)

# Function ACF for a pixel time series
ACF <- function(pixel_values, 
                max_lag = 24) {
                                 if (all(is.na(pixel_values))) 
                                 return(rep(NA, max_lag + 1)) 
acf_result <- acf(pixel_values, 
                  plot = FALSE, 
                  lag.max = max_lag,
                  na.action = na.pass)$acf
        return(as.numeric(acf_result[1:(max_lag + 1)]))
                                }

# Apply ACF function row-wise and collect results in a matrix
S_corr <- t(apply(NDVI_values, 1, ACF, max_lag = 24))

# Calculate the average ACF for each lag (ignoring NAs)
avg_acf <- colMeans(S_corr, na.rm = TRUE)

# Ensure avg_acf has no NA values
if (!all(is.na(avg_acf))) {
  # Remove any NA values from avg_acf for plotting
  avg_acf_clean <- avg_acf[!is.na(avg_acf)]
  lags <- which(!is.na(avg_acf)) - 1  # Lags corresponding to the cleaned data
  
  # Plot the average ACF
  plot(
    lags, avg_acf_clean, 
    type = "h",  # Histogram-like lines
    col = "black",
    xlab = "Lag",
    ylab = "Average Autocorrelation",
    lwd = 2,  # Line thickness
    ylim = c(min(avg_acf_clean, -0.5), max(avg_acf_clean, 0.5))  # Adjust for better display
  )
  
  # Add horizontal dashed lines for confidence intervals
  ci <- 1.96 / sqrt(ncol(NDVI_values))  # Approximate CI
  abline(h = c(-ci, ci), col = "grey", lty = 2)  # Dashed blue lines for CI
  abline(h = 0, col = "black")  # Solid line at 0 for reference
} else {
  message("The average ACF contains only NA values. Check your data.")
}
