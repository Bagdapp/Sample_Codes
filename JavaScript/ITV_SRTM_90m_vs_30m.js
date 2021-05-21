// Load and visualize the SRTM image.
// CGIAR/SRTM90_V4 --> SRTM 90M
// USGS/SRTMGL1_003 --> SRTM 30M
 var srtm = ee.Image('CGIAR/SRTM90_V4');
 Map.addLayer(srtm);

// Calculate and visualize the slope.
 var slope = ee.Terrain.slope(srtm);
 Map.addLayer(slope)


// Select region of intereset (é o grid de 25_25 inserido como table)
Map.addLayer(table)

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: slope,
  description: 'declividade',
  scale: 90,
  maxPixels: 1e12,
  region: table,
  crs: 'EPSG:5880'
});

//Export the image, specifying scale and region.
Export.image.toDrive({
  image: srtm,
  description: 'altitude',
  scale: 90,
  maxPixels: 1e12,
  region: table,
   crs: 'EPSG:5880'
});

