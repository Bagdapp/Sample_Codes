# Carregar bibliotecas

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(tidyverse)
library(tictoc)
library(randomForest) 
library(caret)        
library(ggplot2)
library(SpatialML)
library(reshape2)
library(data.table)


#limpar ambiente
cat("\f")
rm(list = ls())

#Setar a pasta
setwd("E:/Posdoc_ITV/Base_qgis/999_Master_Join/Modelagem_002/backup")
getwd()

# Carregar o shape de 2016_2017 - Treinamento
# Gera uma Df com as coordenadas separadas pois eh necessário para avaliar o GRF
shape_16_17 <- st_read("Cond_2016_def_2017_coords.shp")
str(shape_16_17)
shape_16_17_df <- data.frame(shape_16_17)

coords_shape_16_17_df <- shape_16_17_df[c(39,40)]
names(coords_shape_16_17_df)[names(coords_shape_16_17_df) == 'Long_4326'] <- 'X'
names(coords_shape_16_17_df)[names(coords_shape_16_17_df) == 'Lat_4326'] <- 'Y'
str(coords_shape_16_17_df)

shape_16_17_df <- shape_16_17_df[-c(41,40,39,38,37)]
str(shape_16_17_df)


# Carregar o shape de 2017_2018 - Teste
#shape_17_18_df <- shape_17_18_df[-c(37)]
#str(shape_17_18_df)

shape_17_18 <- st_read("Cond_2017_def_2018_coords.shp")
str(shape_17_18)
shape_17_18_df <- data.frame(shape_17_18)

coords_shape_17_18_df <- shape_17_18_df[c(39,40)]
names(coords_shape_17_18_df)[names(coords_shape_17_18_df) == 'Long_4326'] <- 'X'
names(coords_shape_17_18_df)[names(coords_shape_17_18_df) == 'Lat_4326'] <- 'Y'
str(coords_shape_17_18_df)

# Carregar o shape de 2018_2019 (Já removidos: Pc_IL, Pc_PA, Pc_Ast, Lat, Long) - Validação

shape_18_19 <- st_read("Cond_2018_def_2019_coords.shp")
str(shape_18_19)
shape_18_19_df <- data.frame(shape_18_19)

coords_shape_18_19_df <- shape_18_19_df[c(39,40)]
names(coords_shape_18_19_df)[names(coords_shape_18_19_df) == 'Long_4326'] <- 'X'
names(coords_shape_18_19_df)[names(coords_shape_18_19_df) == 'Lat_4326'] <- 'Y'
str(coords_shape_18_19_df)

# Carregar o shape de 2019 (Previsão 2020)

predict_20 <- st_read("predict_2020_cond_2019_coords.shp")
str(predict_20)
predict_20_df <- data.frame(predict_20)

coords_predict_20_df <- predict_20_df[c(39,40)]
names(coords_predict_20_df)[names(coords_predict_20_df) == 'Long_4326'] <- 'X'
names(coords_predict_20_df)[names(coords_predict_20_df) == 'Lat_4326'] <- 'Y'
str(coords_predict_20_df)


#############construir o GRF###############
#https://www.tandfonline.com/doi/full/10.1080/10106049.2019.1595177
#Treina um modelo do Random Forest no arquivo de treinamento
#avalia a importancia das variáveis e os resultados no conjunto de teste

options(scipen=999999)
memory.limit (9999999999)

tic()

grf <- grf(Def ~ Ar_IL + Dst_IL + Ar_PA + Dst_PA +
           Sum_Wat + Dst_Wat + Sum_Riv + Dst_Riv +
           Sum_FR_SR + Dst_AR + IDH + IDH_Rn + IDH_Ed + IDH_Lg +
           POP + GDP + GDPPC + ARI + Elevation + Slope +
           PcNat + PcWat + PcAgr + PcPas + PcUrb + PcMin +
           DstUrb + DstMin + Dst_Agr + DstPst + FED +
           Dst_PM_km + Ar_Ast_km + Dst_Ast_km,                #Variavel alvo ~ Preditores (tem q por um a um)
           dframe=shape_16_17_df,                             #Df de entrada
           bw=100,                                             #Parametro do modelo ()
           kernel="adaptive",                                 #Parametro do modelo ()
           coords=coords_shape_16_17_df,                      #Df com coordenadas
           mtry = 7,
           ntree = 1200,
           )

toc()

#calibration of the bw (started with mtry and ntree from normal RF)
#has to be done manually

#avaliando o modelo nos dados de 2018 (100% do modelo local)
tic()
pred_18 <- predict.grf(grf, shape_17_18_df, x.var.name="Long_4326", y.var.name="Lat_4326", local.w=1, global.w=0)
toc()
tic()
pred_19 <- predict.grf(grf, shape_18_19_df, x.var.name="Long_4326", y.var.name="Lat_4326", local.w=1, global.w=0)
toc()
tic()
pred_20 <- predict.grf(grf, predict_20_df, x.var.name="Long_4326", y.var.name="Lat_4326", local.w=1, global.w=0)
toc()


predictions <- as.data.frame(cbind( NumId = shape_16_17_df$NumI,
                                    train_value = grf$Global.Model$y,
                                    train_pred_global = grf$Global.Model$predicted,
                                    train_pred_local = grf$LGofFit$LM_yfitPred,
                                    train_2018 = shape_17_18_df$Def,
                                    pred_2018_local = pred_18,
                                    train_2019 = shape_18_19_df$Def,
                                    pred_2019_local = pred_19,
                                    pred_2020_local = pred_20))

#MSE E RMSE de treinamento pelo arquivo de saida (para comparar tirando direto dos modelos)
RSS_train_output = (sum((predictions$train_value - predictions$train_pred_local)^2))  
RSS_train_output

MSE_train_output = RSS_train_output/length(predictions$train_pred_local)  
MSE_train_output

RMSE_train_output = sqrt(MSE_train_output)  
RMSE_train_output

#MSE E RMSE de treinamento pelo arquivo de saida (para comparar tirando direto dos modelos)
RSS_train_output_2018 = (sum((predictions$train_2018 - predictions$pred_2018_local)^2))  
RSS_train_output_2018

MSE_train_output_2018 = RSS_train_output_2018/length(predictions$pred_2018_local)  
MSE_train_output_2018

RMSE_train_output_2018 = sqrt(MSE_train_output_2018)  
RMSE_train_output_2018

#MSE E RMSE de treinamento pelo arquivo de saida (para comparar tirando direto dos modelos)
RSS_train_output_2019 = (sum((predictions$train_2019 - predictions$pred_2019_local)^2))  
RSS_train_output_2019

MSE_train_output_2019 = RSS_train_output_2019/length(predictions$pred_2019_local)  
MSE_train_output_2019

RMSE_train_output_2019 = sqrt(MSE_train_output_2019)  
RMSE_train_output_2019

########salva o csv com as predições de saida#######

write.csv(predictions, "resultados_GRF_bw_100.csv")

######salva o modelo de random forest#####

save(grf,file = "grf_bw100.rda")

#ate aki ok, vamos testar um predict:

######plotar os resultados do treinamento (todos salvos em csv)

bw_train <- data.frame(read.csv("bw_cal_train.csv", header = TRUE, sep = ';'))
#tempo calibracao
sum(bw_train$time)/3600
#plot
ggplot(data = bw_train, aes(x = bw_train$Bw.4326., y = bw_train$RMSE)) + geom_point() +
  geom_line(color = "blue") +
labs( y = "RMSE", x = "Bandwidth (NN)", title = "RMSE vs BW - Train (2017)") +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"), 
          plot.title = element_text(hjust = 0.5))
  


bw_val <- data.frame(read.csv("bw_cal_test.csv", header = TRUE, sep = ';'))
#tempo perdicao 2018
sum(bw_val$time_18)/3600
#tempo perdicao 2019
sum(bw_val$time19)/3600
#tempo perdicao 2020
sum(bw_val$time20)/3600



ggplot(data = bw_val, aes(x = bw_val$Bw.4326., y = bw_val$RMSE_2018_set)) + geom_point() +
  geom_line(color = "blue") +
  labs( y = "RMSE", x = "Bandwidth (NN)", title = "RMSE vs BW - Test (2018)") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        plot.title = element_text(hjust = 0.5))



ggplot(data = bw_val, aes(x = bw_val$Bw.4326., y = bw_val$RMSE_2019_set)) + geom_point() +
  geom_line(color = "blue") +
  labs( y = "RMSE", x = "Bandwidth (NN)", title = "RMSE vs BW - Valid (2019)") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        plot.title = element_text(hjust = 0.5))


#importancia de variaveis

Var_importance <- data.frame(row.names(grf$LocalModelSummary$l.IncMSE), grf$LocalModelSummary$l.IncMSE, grf$LocalModelSummary$l.IncNodePurity)

ggplot(data = Var_importance, aes(reorder(row.names.grf.LocalModelSummary.l.IncMSE., Mean), Mean)) + geom_point() + coord_flip() +
  labs( y = "Mean_IncMSE", x = "Feature", title = "Variable Importance - Train 2017") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        plot.title = element_text(hjust = 0.5))


ggplot(data = Var_importance, aes(reorder(row.names.grf.LocalModelSummary.l.IncMSE., Mean.1), Mean.1)) + geom_point() + coord_flip() +
  labs( y = "Mean_IncNodePurity", x = "Feature", title = "Variable Importance - Train 2017") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray"), 
        plot.title = element_text(hjust = 0.5))

#exportar a importância de variaveis por celula:


Local_IncMSE_2017 <- data.frame(grf$Local.Pc.IncMSE, shape_16_17$NumId)
Local_NodePur_2017 <- data.frame(grf$Local.IncNodePurity, shape_16_17$NumId)

write.csv(Local_IncMSE_2017, "Local_IncMSE_2017.csv")
write.csv(Local_NodePur_2017, "Local_NodePur_2017.csv")


####Seleciona a variavel com mair IncMSE

DT <- data.table(Local_IncMSE_2017)

  DT[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")], .SDcols = c(1:34)]

  write_csv(DT, file = "Local_IncMSE_2017_DT.csv")
  
  
####Seleciona a variavel com mair NodePur
  
  DT2 <- data.table(Local_NodePur_2017)
  
  DT2[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")], .SDcols = c(1:34)]
  
  write_csv(DT2, file = "Local_NodePur_2017_DT.csv")

######PAREI AKI!!!!!

#achar o numero da arvore com menor MSE - Nem precisa rodar
#which.min(model1$mse)
#sqrt(model1$mse[which.min(model1$mse)])

#importancia de variaveis pela Taxa de Ganho de Info (GainRatio - Entropia)

weights <- data.frame(gain.ratio(Def~., TrainSet[-c(1)]))
str(weights)
weights2 <- melt(as.matrix(weights))

#Ativar abaixo somente para plotar os dados
#ggplot(data = weights2, aes(reorder(Var1, value), value)) + geom_point() + coord_flip() +
#  labs( y = "Gain Ratio Value", x = "Feature", title = "Gain Ratio Variable Importance - Cond 2018") +
#  theme(panel.background = element_rect(fill = "white",
#                                        colour = "black",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "gray"), 
#        plot.title = element_text(hjust = 0.5))


#Não utilizado
#seleção de subconjunto otimo pelo Correlation Feature Selection (CFS)
#subset <- cfs(Def~., TrainSet[-c(1)])
#f <- as.simple.formula(subset, "Def")
#print(subset)
      



# Plot predictions vs Training data
ggplot(Output_predictions,aes(pred_train, train_value)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+
  ggtitle("RF: prediction vs training data (RMSE 8,09*10^(-3)") +
  xlab("prediction ") +
  ylab("Training data") +
  theme(plot.title = element_text(color="darkgreen",size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Plot predictions vs Test data
ggplot(Output_predictions,aes(pred_test, test_value)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+
  ggtitle("RF: prediction vs test data (RMSE 11,39*10^(-3))") +
  xlab("prediction ") +
  ylab("Test data") +
  theme(plot.title = element_text(color="darkgreen",size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Plot predictions vs validation data
ggplot(Output_predictions,aes(pred_valid, valid_value)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+
  ggtitle("RF: prediction vs validation data (RMSE 8,82*10^(-3))") +
  xlab("prediction ") +
  ylab("Validation data") +
  theme(plot.title = element_text(color="darkgreen",size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


###########TENHO QUE VER COMO ENCAIXAR ESSA PARTE JUNTO COM A DE CIMA, SE NAO FICA SEPARADO MESMO
###########MAS EH COMPUTACIONALMENTE RUIM
###########ACHO QUE TEM Q FAZER O TEST JUNTO PARA PLOTAR AS RF

Model1_v2 <- randomForest(
  formula = Def ~ ., 
  data = TrainSet[-c(1)],# remove o Id do treinamento
  ytest = y_test,
  xtest = x_test[-c(1)],
  importance = TRUE, # Se colocar TRUE calcula tanto o INCNODEPURITY quanto o %INCMSE
  ntree = 1200,
  mtry = 7)

Model1_v2


##############Testando no conjunto de validação

Model1_v3 <- randomForest(
  formula = Def ~ ., 
  data = TrainSet[-c(1)],# remove o Id do treinamento
  ytest = y_valid,
  xtest = x_valid[-c(1)],
  importance = TRUE, # Se colocar TRUE calcula tanto o INCNODEPURITY quanto o %INCMSE
  ntree = 1200,
  mtry = 7)

Model1_v3


# extract OOB & validation errors
oob <- sqrt(Model1_v3$mse)
test_error <- sqrt(Model1_v2$test$mse)
validation <- sqrt(Model1_v3$test$mse)



# compare error rates
tibble::tibble(
  `Training set error` = oob,
  `Test set error` = test_error,
  `Validation set error` = validation,
  ntrees = 1:Model1_v3$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous() +
  xlab("Number of trees")


#####avaliações estatisticas

summary(Output_predictions$train_value)
summary(log10(Output_predictions$train_value*100))
summary(log10(Output_predictions$train_value*100+1))
plot(density(log10(Output_predictions$train_value*100+1)))
plot(density(sqrt(Output_predictions$train_value)))

qplot(x = train_value, data = Output_predictions)
qplot(x = log10(train_value+1), data = Output_predictions)
qplot(x = log(train_value+1), data = Output_predictions)
qplot(x = log(train_value+ sqrt(train_value^2 +1)), data = Output_predictions)
qplot(x = 1/train_value, data = Output_predictions)
qplot(x = 1/(sqrt(train_value)), data = Output_predictions)
qplot(x = sqrt(train_value), data = Output_predictions)
qplot(x = 1/log10(train_value+1), data = Output_predictions)


ln(x + \sqrt(x^2 + 1))

test_log_transform <- log10(Output_predictions$train_value)


hist(Output_predictions$train_value, breaks=50, col="red")
plot(density(Output_predictions$train_value))

hist((log10(Output_predictions$train_value + 1)), breaks=50, col="red")
plot(density(log(Output_predictions$train_value)))


ggplot(data = Output_predictions, aes(x = pred_train, y = train_value)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()



#required libraries
list.of.packages <- c("caret","digest","doParallel","foreach","foreign","fpc","ggplot2","gtools","GWmodel","jpeg","kohonen","mclust","NbClust","parallel","plyr","pracma","ranger","raster","reshape","raster",
                      "rgdal","rgeos","scales","spdep","spgwr","stringr","tmap","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)

#install GWRFC
require(devtools)
install_github("FSantosCodes/GWRFC")
library(GWRFC)

#view deforestation data
load("deforestation_GWRFC.rda")

tmap_mode("view")
tm_basemap("OpenStreetMap") +
  tm_shape(deforestation) +
  tm_polygons(col="fao",style="cat",title="Annual deforestation rate  2000-2010 (FAO) - categorical (quantiles)",palette="YlOrRd")

#run GWRFC
GWRFC(input_shapefile = deforestation, #can be a spatial dataframe (points or polygons) or the complete filename of the shapefile to analyze.
      remove_columns = c("ID_grid","L_oth","A_cao","A_fru","A_mlk","A_plm"), #for remove variables if they are not informative. Put NA to avoid removal.
      dependent_varName = "fao", #the depedent variable to evaluate. It should be of factor or character data type.
      kernel_function = "exponential", #the weightening function. See help for other available functions.
      kernel_adaptative = T, #use TRUE for adaptative kernel distance or FALSE for a fixed kernel distance.
      kernel_bandwidth = 400, #as the kernel is adaptative, 400 refers to the minimun number of observations to use in modelling.
      upsampling = T, #improves accuracy (recommended) but is a bit more computing costly.
      save_models = T, #save RF models. Beware of hard disk space and extra processing time.
      enable_pdp = F, #experimental, use with caution as is sensible to noise.
      number_cores = 10, #defines the number of CPU cores to use
      output_folder = "E:/demo/deforestation") #check this folder for GWRFC outputs.

deforestation_GWRFC <- shapefile("E:/demo/deforestation/GWRFC_ADP_400_EX_ACC.shp")
summary(deforestation_GWRFC$KAPPA) #observe que el rango va de 0.38 a 0.94
tm_basemap("OpenStreetMap") +
  tm_shape(deforestation_GWRFC) +
  tm_polygons(col="KAPPA",style="kmeans",title="Índice Kappa",palette="RdYlBu")

tm_basemap("OpenStreetMap") +
  tm_shape(deforestation_GWRFC) +
  tm_polygons(col="P_Q4",style="kmeans",title="Probabilidad deforestación (Q4)",palette="YlOrRd")

LVIclust(input_LVI = "E:/demo/deforestation/GWRFC_ADP_400_EX_LVI.shp", #filename of the GWRFC LVI output
         remove_columns=NA,
         method_clustering="ward.D2", #hierarchical clustering is applied here.
         ncluster = 4, #number of clusters.
         plots=T, #available only for all hierarchical clustering methods and kohonen.
         output_folder = "E:/demo/deforestation")


deforestation_clusters <- shapefile("E:/demo/deforestation/LVI_4_clusters.shp")
tmap_mode("view")
tm_basemap("OpenStreetMap") +
  tm_shape(deforestation_clusters) +
  tm_polygons(col="CLUSTER",style="cat",title="Grupos homogeneos",palette="Dark2")

graficos <- list.files("E:/demo/deforestation",full.names=T, pattern=".jpg")
plotJPEG(graficos[1])
















