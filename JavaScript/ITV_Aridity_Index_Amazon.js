var PET = ee.ImageCollection("MODIS/006/MOD16A2"),
    CHIRPS = ee.ImageCollection("UCSB-CHG/CHIRPS/PENTAD"),
    LSIB = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017"),
    table = ee.FeatureCollection("users/cesareneto/Grid_25_25_5880");
	
// Calculate aridity Index


//Calculate PET for 2011
var PET2011 = PET.filterDate('2015-01-01', '2015-12-31')
                 .select('PET')
                 .sum().divide(10)//PET values are scaled by 10; for water kg/m2 corresponds to mm (precipitation)
                 .clip(table);


Map.addLayer(PET2011, {}, 'PET2011', false);

//Calculate aprecipitation for 2011
var Chirps2011 = CHIRPS.filterDate('2015-01-01', '2015-12-31')
                       .select('precipitation')
                       .sum()
                       .clip(table);


Map.addLayer(Chirps2011, {}, 'Chirps2011', false);

//Calculate Aridity 
var Aridity2011 = Chirps2011.divide(PET2011).select([0], ['aridity2011']);

Map.addLayer(Aridity2011, {min: 0, max: 1, palette: ['red', 'yellow', 'green', 'blue']}, 'Aridity2011');


//Export the image, specifying scale and region.
Export.image.toDrive({
  image: Aridity2011,
  description: 'Aridity_2015',
  scale: 500,
  maxPixels: 1e12,
  region: table,
  crs: 'EPSG:5880'
});


	