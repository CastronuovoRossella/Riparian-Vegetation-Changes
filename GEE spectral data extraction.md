//********************************************************************************************************//
//    Comparing RS-based spectral vegetation indices for the analysis of temporal and spatial patterns 
//                   of the riparian vegetation in the Mediterranean region.
//********************************************************************************************************//

// Select study area (Tile Basilicata Region-Italy)

//Map.centerObject(AOI_DRZ, 8);

// Applies scaling factors

function applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    return image.addBands(opticalBands, null, true); }

// Set Cloud_mask function for Lansdat 7  

function Cloud_mask(image) {
  var cloudShadowBitMask = (1 << 4);
  var cloudsBitMask = (1 << 3);
  var qa = image.select('QA_PIXEL');
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
                return image.updateMask(mask); }
  
// Set Cloud_mask function for Lansdat 8

function Cloud_mask_cirrus(image) {
  var cloudShadowBitMask = (1 << 4);
  var cloudsBitMask = (1 << 3);
  var cirrusBitMask = (1 << 2)
  
  var qa = image.select('QA_PIXEL');

  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                 .and(qa.bitwiseAnd(cloudsBitMask).eq(0))
                 .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask); }
  
// Reneme bands 

var Bands_name = ['blue','green','red', 'NIR','SWIR1', 'SWIR2','pixel_qa']
var L7_bands = ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','QA_PIXEL']
var L8_bands = ['SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7','QA_PIXEL']

var Rename_bands = function (img, input) {
                   return img.select (input, Bands_name) }  
  
// Import Landsat 7 Collection

var L7 = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")
        .filterDate('2000-01-01', '2012-12-31')
        .filterMetadata('CLOUD_COVER','Less_than',20)
        .filter(ee.Filter.eq('WRS_PATH', 200))
        .filter(ee.Filter.eq('WRS_ROW', 37))
        .map (applyScaleFactors)
        .map(Cloud_mask)
        
var Coll_L7 = Rename_bands (L7, L7_bands)       
        print(Coll_L7, 'L7_COLLECTION')
        Map.addLayer(Coll_L7,{},'L7_COLLECTION')
        

// Import Landsat 7 Collection

var L8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
        .filterDate('2013-01-01', '2024-12-31')
        .filterMetadata('CLOUD_COVER','Less_than',20)
        .filter(ee.Filter.eq('WRS_PATH', 200))
        .filter(ee.Filter.eq('WRS_ROW', 37))
        .map (applyScaleFactors)
        .map(Cloud_mask_cirrus)

var Coll_L8 = Rename_bands (L8, L8_bands)        
        print(Coll_L8, 'L8_COLLECTION')
        Map.addLayer(Coll_L8,{},'L8_COLLECTION')

/*
// Set NDVI_function                   

var Addband_NDVI=function (image){
            var NDVI=image.normalizedDifference(['NIR','red']).rename('NDVI');
            return image.addBands(NDVI);}

// Set EVI_function     

var Addband_EVI = function (image){
            var EVI = image.expression('2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
            'NIR': image.select('NIR'),
            'RED': image.select('red'),
            'BLUE': image.select('blue')}) .rename('EVI');
            return image.addBands(EVI);}
            
// Set MNDWI_function 

var Addband_MNDWI =function (image){
            var MNDWI=image.normalizedDifference(['green','SWIR1']).rename('MNDWI');            
            return image.addBands(MNDWI);}
            
// Create a single collection with L7 and L8  

var Complete_Landsat_collection = Coll_L7.merge(Coll_L8)
                         .map(Addband_NDVI)
                         .map(Addband_EVI)
                         .map(Addband_MNDWI)
                         print(Complete_Landsat_collection, 'COMPLETE_COLLECTION')
                         Map.addLayer(Complete_Landsat_collection,{},'COMPLETE_COLLECTION')

// Set a loop for NDVI function

var years = ee.List.sequence(2000, 2024)

var collectYear = ee.ImageCollection(years.map(function(y) {
    var start = ee.Date.fromYMD(y, 1, 1)
    var end = start.advance(12, 'month');
    return Complete_Landsat_collection.filterDate(start, end).select('NDVI','MNDWI','EVI').reduce(ee.Reducer.max()).set('year', y).clip (AOI_DRZ)}))
    print(collectYear, 'NDVI_coll_Years')
    Map.addLayer(collectYear,{},'NDVI_coll_Years')                          
                        
// chart

var chart =
    ui.Chart.image.series({
          imageCollection: collectYear,
          region: AOI_DRZ,
          scale: 30,
          xProperty: 'year' })
   .setSeriesNames(['EVI','MNDWI','NDVI'])
   .setOptions({
          title: 'Average Vegetation Index Value ',
          hAxis: {title: 'Year', format:'y', gridlines:{count: 12}},
          vAxis: {title: 'Index'},
          series: {
    0: {lineWidth: 3, color: '8bc64a', pointSize: 7}, 
    1: {lineWidth: 3, color: '3baeff', pointSize: 7}, 
    2: {lineWidth: 3, color: '249737', pointSize: 7}}, 
          curveType: 'function' });
          print(chart);



// import the batch tool for Download images 


var batch = require('users/fitoprincipe/geetools:batch')

batch.Download.ImageCollection.toDrive(collectYear, 'NDVI_max',
                {region: AOI_DRZ, 
                 type: 'float',
                 description: 'MAX_NDVI',
                 scale: 30,
                 fileFormat: 'GeoTIFF',
                 crs: 'EPSG:32633', 
                 maxPixels:  744992896
                                  });
                                  
                 
        */
