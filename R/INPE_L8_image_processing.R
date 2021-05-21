###Conversao das imagens de reflectancia de superficie em tipo float32 e de 0 a 1 (para rodar no ecognition)

rm(list=ls())

getwd()
setwd("C:/Users/Cesare/Desktop/Dados/TESE_DOUTORADO/Areas_Selecionadas/PNB/Imagens/OLI_16_07_2014_MasQuei")

library(raster)
library(RStoolbox)
library(rgdal)
library(gdalUtils)
library(sp)

#OLI-8

#b2
imagem<-raster("RS_OLI_B2.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B2_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B2_TRUNC.tif", "RS_OLI_B2_FLOAT.tif", ot="Float32")



#b3
imagem<-raster("RS_OLI_B3.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B3_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B3_TRUNC.tif", "RS_OLI_B3_FLOAT.tif", ot="Float32")


#b4
imagem<-raster("RS_OLI_B4.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B4_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B4_TRUNC.tif", "RS_OLI_B4_FLOAT.tif", ot="Float32")


#b5
imagem<-raster("RS_OLI_B5.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B5_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B5_TRUNC.tif", "RS_OLI_B5_FLOAT.tif", ot="Float32")


#b6
imagem<-raster("RS_OLI_B6.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B6_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B6_TRUNC.tif", "RS_OLI_B6_FLOAT.tif", ot="Float32")

#b7
imagem<-raster("RS_OLI_B7.tif")
imagem
imagem[imagem<0]<-0
imagem[imagem>10000]<-10000
imagem<-imagem/10000
imagem
writeRaster(imagem,filename = "RS_OLI_B7_TRUNC.tif", format="GTiff")
gdal_translate ("RS_OLI_B7_TRUNC.tif", "RS_OLI_B7_FLOAT.tif", ot="Float32")


##### TERMINA AKI ######

##### INICIO DO CALCULO DOS IVS######

######################################################indices_OLI



rm(list=ls())
getwd()
setwd("C:/Users/Cesare/Desktop/Dados/TESE_DOUTORADO/Areas_Selecionadas/PNB/Imagens/OLI_16_07_2014_MasQuei")
getwd()


#Atribuicao das bandas
b2<-raster("RS_OLI_B2.tif")
b3<-raster("RS_OLI_B3.tif")
b4<-raster("RS_OLI_B4.tif")
b5<-raster("RS_OLI_B5.tif")

#normalizacao
b2<-b2/10000
b3<-b3/10000
b4<-b4/10000
b5<-b5/10000



#NDVI calc
ndvi <- overlay(b4,b5, fun = function(b4,b5){(b5-b4)/(b5+b4)},unstack=TRUE)
ndvi[ndvi<(-1)]<-(-1)
ndvi[ndvi>1]<-1
writeRaster(ndvi,filename = "NDVI.tif", format="GTiff")
gdal_translate ("NDVI.tif", "NDVI_float32.tif", ot="Float32", overwrite=TRUE)

#VDVI calc
vdvi <- overlay(b2,b3,b4, fun = function(b2,b3,b4){(2*b2-b3-b4)/(2*b2+b3+b4)},unstack=TRUE)
vdvi[vdvi<(-1)]<-(-1)
vdvi[vdvi>1]<-1
writeRaster(vdvi,filename = "VDVI.tif", format="GTiff")
gdal_translate ("VDVI.tif", "VDVI_float32.tif", ot="Float32", overwrite=TRUE)

#SAVI calc
savi <- overlay(b4,b5, fun = function(b4,b5){2*((b5-b4)/(b5+b4+1))},unstack=TRUE)
savi[savi<(-1)]<-(-1)
savi[savi>1]<-1
writeRaster(savi,filename = "SAVI.tif", format="GTiff")
gdal_translate ("SAVI.tif", "SAVI_float32.tif", ot="Float32", overwrite=TRUE)

#MSAVI2 calc
msavi2 <- overlay(b4,b5, fun = function(b4,b5){0.5*((2*b5)+1-(sqrt(((2*b5+1)^2)-8*(b5-b4))))},unstack=TRUE)
msavi2[msavi2<(-1)]<-(-1)
msavi2[msavi2>1]<-1
writeRaster(msavi2,filename = "MSAVI2.tif", format="GTiff")
gdal_translate ("MSAVI2.tif", "MSAVI2_float32.tif", ot="Float32", overwrite=TRUE)

#EVI calc
evi <- overlay(b2,b4,b5, fun = function(b2,b4,b5){2.5*((b5-b4)/(b5+6*b4-7.5*b2+1))},unstack=TRUE)
evi[evi<(-1)]<-(-1)
evi[evi>1]<-1
writeRaster(evi,filename = "EVI.tif", format="GTiff")
gdal_translate ("EVI.tif", "EVI_float32.tif", ot="Float32", overwrite=TRUE)

#EVI2 calc
evi2 <- overlay(b4,b5, fun = function(b4,b5){2.5*((b5-b4)/(b5+2.4*b4+1))},unstack=TRUE)
evi2[evi2<(-1)]<-(-1)
evi2[evi2>1]<-1
writeRaster(evi2,filename = "EVI2.tif", format="GTiff")
gdal_translate ("EVI2.tif", "EVI2_float32.tif", ot="Float32", overwrite=TRUE)


#####TERMINA AKI#####

##### CALCULO DO TASSELED CAP#####


#OLI

rm(list=ls())
getwd()
setwd("C:/Users/Cesare/Desktop/Dados/TESE_DOUTORADO/Areas_Selecionadas/PNB/Imagens/OLI_16_07_2014_MasQuei")
getwd()


#Atribuicao das bandas
b2<-raster("RS_OLI_B2.tif")
b3<-raster("RS_OLI_B3.tif")
b4<-raster("RS_OLI_B4.tif")
b5<-raster("RS_OLI_B5.tif")
b6<-raster("RS_OLI_B6.tif")
b7<-raster("RS_OLI_B7.tif")


b2<-b2/10000
b3<-b3/10000
b4<-b4/10000
b5<-b5/10000
b6<-b6/10000
b7<-b7/10000


brightness <- overlay(b2,b3,b4,b5,b6,b7, fun = function(b2,b3,b4,b5,b6,b7){0.3029*b2+0.2786*b3+0.4733*b4+0.5599*b5+0.508*b6+0.1872*b7},unstack=TRUE)

writeRaster(brightness,filename = "TC_brightness.tif", format="GTiff")

gdal_translate ("TC_brightness.tif", "TC_brightness_float32.tif", ot="Float32", overwrite=TRUE)

greenness <- overlay(b2,b3,b4,b5,b6,b7, fun = function(b2,b3,b4,b5,b6,b7){(-0.2941)*b2-0.243*b3-0.5424*b4+0.7276*b5+0.0713*b6-0.1608*b7},unstack=TRUE)

writeRaster(greenness,filename = "TC_greenness.tif", format="GTiff")

gdal_translate ("TC_greenness.tif", "TC_greennesss_float32.tif", ot="Float32", overwrite=TRUE)

wetness <- overlay(b2,b3,b4,b5,b6,b7, fun = function(b2,b3,b4,b5,b6,b7){0.1511*b2+0.1973*b3+0.3283*b4+0.3407*b5-0.7117*b6-0.4559*b7},unstack=TRUE)

writeRaster(wetness,filename = "TC_wetness.tif", format="GTiff")

gdal_translate ("TC_wetness.tif", "TC_wetness_float32.tif", ot="Float32", overwrite=TRUE)



#####TERMINA AKI#####

##### CALCULO MLME#####

##### O MLME DEVE SER CALCULADO PELO ENVI E DEPOIS AS IMAGENS PRECISAM SER TRANSFORMADAS EM FLOAT#####

##### NAO ESQUECER DA CONSTRAINT NO ENVI (999999) E SALVAR CADA COMPONENTE SEPARADAMENTE######

SOMBRA<-raster("MLME_SOMBRA.tif")
VEG<-raster("MLME_VEG.tif")

SOLO
SOMBRA
VEG

plot(SOLO)
plot(SOMBRA)
plot(VEG)



gdal_translate ("MLME_SOLO.tif", "MLME_Solo_float32.tif", ot="Float32", overwrite=TRUE)
gdal_translate ("MLME_SOMBRA.tif", "MLME_Sombra_float32.tif", ot="Float32", overwrite=TRUE)
gdal_translate ("MLME_VEG.tif", "MLME_Vegetacao_float32.tif", ot="Float32", overwrite=TRUE)

