// Adicionar a cena landsat q ira trabalhar, o nome da variavel dela deve ser table

Map.addLayer(table);

// Shape da cena foi adicionado ao mapa

// Funcao de remover nuvem usando a 'QA60', só rodar

function MascaraNubesS(image) {
  var qa = image.select('QA60');
// var mediumcloud = 1 << 8;
// var highcloud = 1 << 9;
  var RecorteNubesMascaraS = 1 << 10;
  var RecorteCirrosMascaraS = 1 << 11;
  var MascaraS = qa.bitwiseAnd(RecorteNubesMascaraS).eq(0)
      .and(qa.bitwiseAnd(RecorteCirrosMascaraS).eq(0));
//      .and(qa.bitwiseAnd(RecorteCirrosMascaraS).eq(0)));
//.and(qa.bitwiseAnd(highcloud).eq(0))
//.and(qa.bitwiseAnd(mediumcloud).eq(0));
  return image.updateMask(MascaraS);}

// Selecao da colecao Sentinel-2

var collection = ee.ImageCollection('COPERNICUS/S2_SR') // Sentinel 2 SR...
  //YYYY-MM-DD
//.filterDate('2018-12-01', '2019-03-31') //Filtrar por data
  .filterDate('2019-07-01', '2019-08-31') //Filtrar por data
  .filterBounds(table) //Pega os limites da cena
  .filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 10)) // ...filters on the metadata for pixels less than 10% cloud
  .map(MascaraNubesS); //aplica a mascara de nuvens do espanhol
  
  
print(collection); // gera a lista de imagens obtidas na aba console, expandir o que saiu e salvar em um txt
  
// reduzindo as imagens pra mediana

var medianpixels = collection.median(); // seleciona a mediana para a composicao
var medianpixelsclipped = medianpixels.clip(table); // recorta pela area e reescalona de 0 a 1     

// visualiza o mosaico
Map.addLayer(medianpixelsclipped, {bands: ['B8', 'B4', 'B3'], min: 0, max: 10000, gamma: 1.5}, 'Sentinel_2 mosaic');

//selecionando bandas pra exportar
var medianexport = medianpixelsclipped.select(['B3','B4', 'B8', 'B8A', 'B11', 'B12']);


//selecionando o MLME para a imagem composta de 7 bandas, somente rodar

var bands = ['B3','B4', 'B8', 'B8A', 'B11', 'B12'];

var image = medianexport
    .select(bands);

 var solo = [1505, 2111, 2871, 2890, 4539, 4003];
 var water = [264, 177, 234, 249, 215, 125];
 var veg = [524, 221, 3276, 3472, 1392, 539];
 
 var fractions = medianexport.unmix([solo, veg, water], true, true)
     .rename(['solo', 'veg', 'water']);
 Map.addLayer(fractions, {}, 'unmixed');
// Se quiser exibir no mapa é só rodar a linha de cima

// Calculando NDVI (já tem funcao pronta)
var image_ndvi = medianexport.normalizedDifference(['B8','B4']);
 Map.addLayer(image_ndvi);
// Se quiser exibir no mapa é só rodar a linha de cima

//print(fractions);
//print(image_ndvi);
//print(medianexport);
//esses comandos são para ver as infos das bandas acima, sai na aba console

//Fazendo o stacking com tudo
var stacked_MLME = medianexport.addBands(fractions);
var stack_Final = stacked_MLME.addBands(image_ndvi);

// Verificar se o stacking final esta correto
print(stack_Final.bandNames());

//Map.addLayer(limpo);
//Map.addLayer(sujo);
//Map.addLayer(VS);

//Map.addLayer(amostras)

//var classNames = limpo.merge(sujo).merge(VS);
//Map.addLayer(classNames);

// Adicionar um arquivo shp em pontos como amostras
// 0 --> VS
// 1 --> sujo
// 2 --> limpo
// ele deve conter uma unica coluna numerica com esse dado
// padronizar o nome como "Classe"
// Inserir como asset e transformar em variável da cena

var amostras
print(amostras);

// preparando o conjunto de treinamento

var bands_train = ['B3','B4', 'B8', 'B8A', 'B11', 'B12', 'solo', 'veg', 'water','nd' ];
var training = stack_Final.select(bands_train).sampleRegions({
  collection: amostras,
  properties: ['Classe'],
  scale: 20
});
print(training);

// Preparando o RF com 1000 arvores

var ntree = 1000;

var classificacao = ee.Classifier.smileRandomForest(ntree).train({
features: training,
classProperty: 'Classe',
inputProperties: bands_train,
});

// Plotando o RF na área selecionada

var random_forest = stack_Final.select(bands_train).classify(classificacao);
Map.addLayer(random_forest, {min: 0, max: 2, palette: ['24F331', 'F4AB19',
'FAF185 '] }, 'Classificacao Random Forest');

 //-- Exportação da imagem pro drive (não passa de 10 Mb)
Export.image.toDrive({
  image:random_forest,
  description: 'Random_forest_218_69_218_70',
  folder: 'Engine',
  maxPixels:1e9,
  scale: 20,
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:4326',
  region: table
});