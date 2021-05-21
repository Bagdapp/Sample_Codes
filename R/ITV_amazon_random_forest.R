# Carregar bibliotecas

library(sf)
library(raster)
library(dplyr)
library(spData)
library(corrplot)
library(rgdal)
library(tidyverse)
library(Hmisc)
library(tictoc)
library(rsample)     
library(randomForest) 
library(ranger)       
library(caret)        
library(ggplot2)
library(randomForestExplainer)
library(e1071)
library(FSelector)
library(ggplot2)
library(reshape2)

#limpar ambiente
cat("\f")
rm(list = ls())

#Setar a pasta
setwd("E:/Posdoc_ITV/Base_qgis/999_Master_Join/Modelagem_001/backup")
getwd()

# Carregar o shape de 2016_2017 (Já removidos: Pc_IL, Pc_PA, Pc_Ast, Lat, Long) - Treinamento
# Transforma em Df, remove a col geom (os nomes dos atributos estão iguais ja)
shape_16_17 <- st_read("Cond_2016_def_2017.shp")
str(shape_16_17)
shape_16_17_df <- data.frame(shape_16_17)
shape_16_17_df <- shape_16_17_df[-c(37)]
str(shape_16_17_df)

# Carregar o shape de 2017_2018 (Já removidos: Pc_IL, Pc_PA, Pc_Ast, Lat, Long) - Teste
shape_17_18 <- st_read("Cond_2017_def_2018.shp")
str(shape_17_18)
shape_17_18_df <- data.frame(shape_17_18)
shape_17_18_df <- shape_17_18_df[-c(37)]
str(shape_17_18_df)

# Carregar o shape de 2018_2019 (Já removidos: Pc_IL, Pc_PA, Pc_Ast, Lat, Long) - Validação
shape_18_19 <- st_read("Cond_2018_def_2019.shp")
str(shape_18_19)
shape_18_19_df <- data.frame(shape_18_19)
shape_18_19_df <- shape_18_19_df[-c(37)]
str(shape_18_19_df)


# Carregar o shape de 2019 (Previsão 2020)
predict_20 <- st_read("predict_2020_cond_2019.shp")
str(predict_20)
predict_20_df <- data.frame(predict_20)
predict_20_df <- predict_20_df[-c(36)]
str(predict_20_df)


# avaliação inicial de correlação de pearson (por esse que removi as colunas acima)
# não eh necessario rodar nesse script
#mydata.cor = cor(shape_16_17_df)
#corrplot(mydata.cor, tl.col = "black", tl.srt = 45, method = "square",number.cex=0.65, addCoef.col="black")


#criar train/test/validation para testar inicialmente o Random Forest

TrainSet <- shape_16_17_df
TestSet  <- shape_17_18_df
ValidSet <- shape_18_19_df



##############
############## TUNING SIMPLES #####APENAS PARA TESTE RAPIDO DO MODELO/CODIGO
##############
#Tuning do mtry pela função do caret

tic()
set.seed(123)

Tune <- tuneRF(
  x          = TrainSet[-c(1,36)],
  y          = TrainSet$Def,
  ntreeTry   = 1000,
  mtryStart  = 10,
  stepFactor = 1.5,
  improve    = 0.005,
  trace      = TRUE, # to show real-time progress
  plot       = TRUE  
)
toc()

Tune

##############
############## TUNING DETALHADO
##############
############## Bastante demorado, para teste do codigo rodar o simples

####tunar o modelo por grid - mtry
tic()
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 30))
rf_mtry <- train(Def~.,
                 data = TrainSet[-c(1)],
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
plot(rf_mtry)
toc()
best_mtry <- rf_mtry$bestTune$mtry 

#save(rf_mtry, file = "rf_mtry.rda")
load(file = "rf_mtry.rda")

####tunar o modelo por maxnodes, com o best mtry
tic()
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(1: 1000)) {
  set.seed(1234)
  rf_maxnode <- train(Def~.,
                      data = TrainSet[-c(1)],
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      maxnodes = maxnodes
                      )
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
toc()

#Ativar abaixo somente para plotar os dados
#options(max.print=1000000)
#save(results_mtry, file = "results_mtry.rda")
#tune_max_nodes <- read.csv("tuning_max_nodes.csv", header = TRUE, sep = ";")
#str(tune_max_nodes)
#ggplot(data = tune_max_nodes, aes(x=ï..RMSE, y=Mean)) + geom_line(colour = "dodgerblue") +
#  labs( y = "RMSE (Cross-Validation)", x = "Max Nodes") +
#  theme(panel.background = element_rect(fill = "white",
#                                        colour = "black",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "gray"))


####tunar o modelo por ntrees - com mtry anterior, o maxnodes tem q colocar direto no modelo
#250,300,350,400,450,500,600,700,800,900,1000,1200,1400,1600,1800,2000)

tic()
store_maxtrees <- list()
for (ntree in c(1600, 1700, 1800, 1900, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Def~.,
                       data = TrainSet[-c(1)],
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
toc()

#Ativar abaixo somente para plotar os dados
#options(max.print=1000000)
#tune_ntree <- read.csv("tuning_ntrees.csv", header = TRUE, sep = ";")
#str(tune_ntree)
#ggplot(data = tune_ntree, aes(x=ï..ntree, y=mean)) + geom_line(colour = "dodgerblue") +
#  labs( y = "RMSE (Cross-Validation)", x = "Number of trees") +
#  theme(panel.background = element_rect(fill = "white",
#                                        colour = "black",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "gray"))






#############construir o modelo calibrado###############
#Treina um modelo do Random Forest no arquivo de treinamento
#avalia a importancia das variáveis e os resultados no conjunto de teste

tic()

Model1 <- randomForest(
  formula = Def ~ ., 
  data = TrainSet[-c(1)], # remove o Id do treinamento
  importance = TRUE, # Se colocar TRUE calcula tanto o INCNODEPURITY quanto o %INCMSE
  ntree = 1200,
  mtry = 7)

Model1

print(Model1)
toc()

#importancia de variaveis

varImpPlot(Model1, 
           sort = TRUE, 
           main = 'Variable Importance - Cond 2018', 
           n.var = 34) #Conjunto tem 36 variaveis (tira o ID e a resposta)


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
      
##### Faz a predição do Modelo acima para os dados de treinamento,
##### validação e predição 2020

#dividir o conjunto de teste com os dados e com a variável resposta
x_test <- TestSet[setdiff(names(TestSet), "Def")]
y_test <- TestSet$Def

x_valid <- ValidSet[setdiff(names(ValidSet), "Def")]
y_valid <- ValidSet$Def

x_2020  <- predict_20_df

# predição com o conjunto de teste
pred_test = predict(Model1, x_test[-c(1)])

# predição com o conjunto de validação
pred_val = predict(Model1, x_valid[-c(1)])

# predição com o conjunto de validação
pred_2020 = predict(Model1, x_2020[-c(1)])

# salvando as variaveis de saida para integrar com o shape depois

options(scipen=999)

Output_predictions = as.data.frame(cbind(NumId = TrainSet$NumId,
                                train_value = Model1$y,
                                pred_train = Model1$predicted,
                                id_test = x_test$NumId,
                                test_value = y_test,
                                pred_test = pred_test,
                                id_validation = x_valid$NumId,
                                valid_value = y_valid,
                                pred_valid = pred_val,
                                id_pred = x_2020$NumId,
                                Pred_2020 = pred_2020))

str(Output_predictions)

write.csv(Output_predictions, "resultados_17_18_19_20.csv")

summary(Output_predictions)


#MSE E RMSE de treinamento pelo arquivo de saida (para comparar tirando direto dos modelos)
MSE_train_output = (sum((Output_predictions$train_value - Output_predictions$pred_train)^2))/length(Output_predictions$pred_train)  
MSE_train_output

RMSE_train_output = sqrt(MSE_train_output)  
RMSE_train_output

#MSE E RMSE de teste pelo arquivo de saida (para comparar tirando direto dos modelos)
MSE_test_output = (sum((Output_predictions$test_value - Output_predictions$pred_test)^2))/length(Output_predictions$pred_test)  
MSE_test_output

RMSE_test_output = sqrt(MSE_test_output)  
RMSE_test_output

#MSE E RMSE de teste pelo arquivo de saida (para comparar tirando direto dos modelos)
MSE_valid_output = (sum((Output_predictions$valid_value - Output_predictions$pred_valid)^2))/length(Output_predictions$pred_valid)  
MSE_valid_output

RMSE_valid_output = sqrt(MSE_valid_output)  
RMSE_valid_output

##### Aki nao precisa rodar era só para ver se estava certo com os dados de cima
# RMSE do modelo no treinamento ok, bateu com o de cima
#MSE_Train = mean(Model1$mse)
#MSE_Train
#
#RMSE_Train = sqrt(MSE_Train)
#RMSE_Train


# estatísticas da performance do algoritmo no cojnuto de teste (ok bateu com la em cima)
# soma dos erros quadráticos
#MSE_test = (sum((y_test - pred_test)^2))/length(pred_test)  
#MSE_test

# erro quadrático médio
#RMSE_test = sqrt(MSE_test)
#RMSE_test

# estatísticas da performance do algoritmo no cojnuto de validacao, ok com o de cima tb
# soma dos erros quadráticos
#MSE_valid = (sum((y_valid - pred_val)^2))/length(pred_val)  
#MSE_valid

# erro quadrático médio
#RMSE_valid = sqrt(MSE_valid)
#RMSE_valid


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



















