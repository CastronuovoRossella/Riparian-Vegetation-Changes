##%######################################################%##
#                                                          #
####             Step 2: Mann-Kenndal trend             ####
####             test and Theil Sen's slope             ####
#                                                          #
##%######################################################%##

# Trend analysis was based on the Mann-Kendall (MK) test for trend using rkt library (Marchetto 2017). The purpose of the MK test is to statistically asses the presence of a consistently increasing or 
# decreasing trend in the variable of interest through time (Mann 1945, Kendall 1975). The MK test is a non-parametric (distribution-free) test and can be applied to the data which doesn’t follow a normal 
# distribution. The Theil-Sen’s (TS) slope estimator was used to quantify the direction and magnitude of change over time. Because it is calculated as the median of all slopes between every pair of values, 
# the estimator is less sensitive to outliers, skewness, and heteroskedasticity in the data (Wilcox 2001). As a proven robust estimator of trend magnitude and direction, the TS slope has already been 
# successfully applied to detect various aspects of spatio-temporal landscape dynamics (Nill et al. 2019).

# Additionally the Kendall rank correlation coefficient (τ) and two-sided p-value were applied to assess the strength and significance of association between variables of LST and time. 
# Kendall τ as a non-parametric test for statistical association between variables accounts for the number of concordant (agreeable) and discordant (disagreeable) pairs of observations among 
# all possible pairs of observations and thus less sensitive to errors in the data. Also it yields more accurate p-values with smaller sample size which is the case for this analysis when 4 ≤ n ≤ 36. 
# Based on the outputs of the analysis only statistically significant (p ≤ 0.01) TS slope values confirmed by strong to moderate Kendall coefficient (−0.5 ≥ τ ≥ 0.5) were included in the final results.

# load all required packages
require(raster)
require(rkt)
require(trend)
require(zoo)

rasterOptions(maxmemory=1e+06, chunksize=1e+07, progress = 'text')

###########################################################################################
## MK-test + Seasonal and Regional Kendall tests (SKT / RKT) + Theil-Sen's Slope estimator
###########################################################################################

# NDVIStack
NDVIStack <-
  list.files(
    path = "E:/DELL/Riparian_Vegetation_Change/Strati cartografici output/2. NDVI_Max",
    pattern = paste("", ".*.tif$", sep = ""),
    all.files = FALSE,
    full.names = TRUE,
    recursive = TRUE
  )
NDVIStack
s <- lapply(NDVIStack, raster)
s

# La funzione 'brick' prende una lista di oggetti raster e li combina in un unico oggetto brick. 
# Un brick è un oggetto raster a più bande che permette di lavorare con tutti i layers contemporaneamente.
  
bricked_files <- brick(s)
gc()                                                     # Libera la memoria per gestire grandi dataset raster.
names(bricked_files)
mylayers<- paste0('NDVI_max.',seq(1,25,1));mylayers
bricked_files<-bricked_files[[mylayers]]                 # Rinomina brick con passo 1

# Set year range for analysis (number of years have to be the same as number of bands)
years <- seq(from = 2000, length.out = 25, by = 1);years
names(bricked_files)<-paste0('NDVI_max_',years)
names(bricked_files)

# Rimuovere pixel acqua
# bricked_files[bricked_files <= 0] = NA

# Analysis function
rktFun <-function(x) {
  if(all(is.na(x))){		# if no data is available for the given pixel NA is returned as results
    c(NA,NA,NA)
  } else {
    analysis <-rkt(years, x)	# this executes the rkt function for a NDVI time series of an individual pixel
    a <-analysis$B # this will extract the results: theil sen slope
    b <-analysis$sl # this will extract the results: pvalue
    c <-analysis$tau # this will extract the results: Mann-Kendall tau
    return(cbind(a, b, c)) # return all results
  } }

start_time <- Sys.time()
rRaster <-calc(bricked_files, rktFun)
end_time <- Sys.time()
end_time - start_time

rRaster

#------- Write to Results folder
results<- 'E:/DELL/Riparian_Vegetation_Change/Strati cartografici output/2. NDVI_Max/Results'

writeRaster(rRaster[[1]], paste0(results,"/ts_slope.tif"), overwrite=T)
writeRaster(rRaster[[2]], paste0(results,"/mk_pvalue.tif"), overwrite=T)
writeRaster(rRaster[[3]], paste0(results,"/mk_tau.tif"), overwrite=T)

plot(rRaster)

##################################
## 5. P-Value Masking ##
##################################

# Load tau raster
tau <-raster(paste0(results, "/mk_tau.tif"))
# Set p-values to create masks for
p_values <-c(0.01, 0.05, 0.1)
# Loop through p-values, producing a mask for each
for(i in 1:length(p_values)){
  # Load the P-value raster 
  p_value_raster <-raster(paste0(results, "/mk_pvalue.tif")) 
  # Select current p-value 
  p_val <-p_values[[i]] 
  # Create string vers for filenaming 
  p_val_str <-gsub("\\.", "", as.character(p_val)) 
  # Mask 
  p_value_raster[p_value_raster > p_val] <-NA 
  p_masked <-mask(tau, p_value_raster) 
  # Write result 
  writeRaster(p_masked, paste0(results, "/pvalue_mask", p_val_str, ".tif"),
              overwrite = T)
  # Cleanup
}

#############################################
## 6. P-Value Masking with Significant Tau ##
## Note: Must be run after section 5 ##
#############################################

# Isolate tau values > 0.4 and < -0.4
sigTau <-raster(paste0(results, "/mk_tau.tif"))
sigTau[sigTau>(-0.4) & sigTau< 0.4] <-NA

# Loop through p-values, producing a mask for each
for(i in 1:length(p_values)){
  # Select current p-value
  p_val <-p_values[[i]]
  # Create string vers for filenaming
  p_val_str <-gsub("\\.", "", as.character(p_val))
  # Read current p-value raster
  p_value_raster <-raster(paste0(results, "/pvalue_mask", p_val_str,".tif")) 
  # Mask significant tau with p-value raster 
  tau_masked <-mask(sigTau, p_value_raster) 
  # Write result 
  writeRaster(tau_masked, paste0(results, "/tau_mask", p_val_str, ".tif"),
              overwrite = TRUE) 
}
gc()

# To have a look at the last created result (tau values with trends greater than 0.4 and a p-value of smaller than 0.1) we can run:
plot(tau_masked)

##%######################################################%##
#                                                          #
####          Step 3: Identify turning points           ####
#                                                          #
##%######################################################%##



vals <- values(bricked_files)
head(vals)

# create empty matrix to store results
res <- matrix(nrow=nrow(vals), ncol=2)

# start looping through the pixels 
for (i in 1:nrow(vals)){
  
  # get time series of first pixel
  x <- vals[i,]
  
  # check if there is data in the pixel
  if(all(is.na(x))){
    
    # if not, save NA, NA as result
    res[i,] <- c(NA,NA)
    
  } else {
    
    # if there is data, fill data gaps in the time series using simple interpolation
    x1 <- na.approx(na.approx(x))
    # apply the pettitt test
    analysis <- pettitt.test(x1)
    # extract the results
    a <-as.numeric(analysis$estimate)[1] # pettitt test (id at which time step the change occurred)
    b <-analysis$p.value 
    # save the results
    res[i,] <- cbind(a, b)
    
  } 
  # print current iteration
  print(i)
} 
 
# overwrite the pixel values with the results
values(pettitt.timep) <- turnp
# plot the turning point raster
plot(pettitt.timep)
# Write result 
writeRaster(pettitt.timep, paste0(results, "/pettitt_timep",  ".tif"),
            overwrite = TRUE)
mask <- pettitt.p.val < 0.05
plot(mask)

timep_fin <- mask(pettitt.timep, mask, maskvalue=0, updatevalue=NA)
plot(timep_fin)
# Write result 
writeRaster(timep_fin, paste0(results, "/pettitt_timep_mask",  ".tif"),
            overwrite = TRUE)
