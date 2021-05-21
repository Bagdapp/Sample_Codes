#Exercicio de análise espacial - ITV

#Cesare Di Girolamo Neto

#Este exercicio sera resolvido fazendo uma regressão linear multipla seguindo as seguintes etapas

# 1 - Definicao de objeto de estudo, escalas, classes de uso e outros --> Ja preparado

# 2 - Preparar o banco de dados espacial (poligonos e relacoes) -- Ja preparado

# 3 - Analise exploratoria dos dados

# 4 - Parametrizar componentes

# 5 - regressão linear multipla


###############################

rm(list=ls())


# ajustar e verificar o caminho dos dados de entrada:

setwd("E:/exercicio")
getwd()

# carregar os pacotes

library(raster)
library(rgdal)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(nortest)
library(foreign)
library(tidyr)



#abrir e verificar atributos do shp


shp<-readOGR(dsn=".",layer = "Grid_data_2018_2019")
summary(shp)
class(shp)
crs(shp)
plot(shp, col="green")



#transformar em uma df e verificar propriedades

df<-data.frame(shp)
class(df)
View(df)
str(df)



#histogramas dos dados referentes a classe com curvas de distribuicao normal (Def, NDef e TotPix)
#cuidado com a variavel x, rodar todo o bloco pq ele atualizar o x a cada calculo de normal

#Def

x = df$Def
mean = mean(df$Def)
sd = sd(df$Def)

hist(df$Def, freq = FALSE, main = "Classe Def", xlab="pixels",ylab="total",col="orange")
curve(dnorm(x, mean, sd), col = 2, add = TRUE)


#N_Def

x = df$NDef
mean = mean(df$NDef)
sd = sd(df$NDef)

hist(df$NDef, freq = FALSE, main = "Classe NDef",xlab="pixels",ylab="total",col="orange")
curve(dnorm(x, mean, sd), col = 2, add = TRUE)

#_Tot_pix

x = df$TotPix
mean = mean(df$TotPix)
sd = sd(df$TotPix)

hist(df$TotPix, freq = FALSE, main = "Classe TotPix",xlab="pixels",ylab="total",col="orange")
curve(dnorm(x, mean, sd), col = 2, add = TRUE)

# Ja era esperado pelo metadado, mas vai ser necessario fazer alguma transformacao na variavel alvo, 
# até porque estou pensando em utilizar um modelo continuo
# vou gerar uma nova coluna com a % de desflorestamento de 0-100, vai precisar passar pra escala ln ou alguma outra
# ja resolve também as celulas com numeros diferentes de Totpix
# também já vou avaliar a normalidade em todos os casos (vou testar Andersol Darling, mas poderia ser outro, tipo Kolmogorov-Smirnov)

df$class_def <- c((df$Def/df$TotPix)*100)
#View (df)

hist(df$class_def, main = "class_def",xlab="% desmatamento",ylab="Nº de Segmentos",col="orange")


# Anderson-Darling
ad.test(df$class_def) 

###Não passou no teste de normalidade



############testando a transformacao em log10

df$class_def_log <- c(log10(df$class_def))
#View (df)

hist(df$class_def_log, main = "df$class_def_log",xlab="Log desmatamento",ylab="Nº de Segmentos",col="orange")

# Anderson-Darling
ad.test(df$class_def_log) 


########### a cara do grafico ja esta melhor, mas nao passou no teste de normalidade


#testando a transformacao em sqrt 

df$class_def_sqrt <- c(sqrt(df$class_def))
#View (df)

hist(df$class_def_sqrt, main = "df$class_def_sqrt",xlab="Sqrt desmatamento",ylab="Nº de Segmentos",col="orange")

# Anderson-Darling
ad.test(df$class_def_sqrt) 


########## nao passou no teste de normal


#testando a transformacao em 1/X

df$class_def_1_x <- c(1/df$class_def)
#View (df)

hist(df$class_def_1_x, main = "df$class_def_1_x",xlab="1/x desmatamento",ylab="Nº de Segmentos",col="orange")

# a cara do grafico ja esta melhor, vou testar normalidade

# Anderson-Darling
ad.test(df$class_def_1_x) 

# nao passou no teste de normal


######Nenhuma transformação classica fez efeito, poderia testar outros ajustes para tentar normalizar,
######entretanto, vou optar por deixar o dado classe como está, na escala de 0-100
###### vou apagar as colunas desnecessárias

df2 <- df[,c(5:26)]
str(df2)

#converter POP_201 para numeric (ficar toda a df numerica)

df2$POP_201 <- as.numeric(df2$POP_201)
str(df2)


##################avaliação dos atributos preditivos##############

#histograma de todos os atributos

#hist.data.frame(df2)

#testes de normalidade (vou testar só AD)

ad.test(df2$ed_3)
ad.test(df2$plnd_15)
ad.test(df2$pland_3)
ad.test(df2$ln_st_r)
ad.test(df2$ln_f_rd)
ad.test(df2$dst_rds)
ad.test(df2$ln_rvrs)
ad.test(df2$ln_wtrw)
ad.test(df2$dst_rvr)
ad.test(df2$ar_prtc)
ad.test(df2$ar_ndgn)
ad.test(df2$IDHM)
ad.test(df2$POP_201)
ad.test(df2$PIB_PC)
ad.test(df2$dem_dat)
ad.test(df2$slop_dt)
ad.test(df2$wi_data)
ad.test(df2$dst_mnn)
ad.test(df2$dst_rbn)
ad.test(df2$dist_pa)
ad.test(df2$dist_il)

###nenhum teste deu normal, vou testar ver se o ad.test está funcionando
### exemplo de distribuicao normal da internet


##### ROTINA A PARTE#######

library(tidyverse)
library(ggpubr)
library(rstatix)

set.seed(1234)
ToothGrowth %>% sample_n_by(supp, dose, size = 1)
ggdensity(ToothGrowth$len, fill = "lightgray")
shapiro_test(ToothGrowth$len)
ad.test(ToothGrowth$len)
ks.test(ToothGrowth$len, mean, sd)

###deu normal nos 3 testes, então acho que esta correto que nenhum dado deu normal

### aqui teria que ter um pouco mais de tempo e pensar em possiveis transformações de variáveis
### fazer testes similares aos que fiz na classe de desflorestamento, mas me parece um problema
### bem real, então eh normal nada dar distribuicao normal
### poderia avaliar as transformacoes e mesmo que nao desse distribuicao normal fazer a avaliacao de
### correlação e verificar quais atributos com tem menores correlacoes em transformacoes diferentes
### de qualquer maneira vou seguir para a regressao linear multipla utilizando os dados da df2

### vou fazer as analises de correlação entre os atributos para ver se pode descartar alguma coisa

corr_matr<-cor(df2) 
corrplot(corr_matr,method="number", type = "upper")

### nao tem nenhuma correlacao maior do que 0.8, então não vou descartar nenhum atributo
### poderia descartar as maiores de 0.5 para dar uma limpada na regressão linear

# regressao linear (tinha que pesquisar mais como fazer puxando os indices do df2 de uma vez so, mas fiz um a um pq sabia de cabeça)

model1 <- lm (df2$class_def ~ df2$ed_3 + df2$plnd_15 + df2$pland_3 + df2$ln_st_r + df2$ln_f_rd + df2$dst_rds +df2$ln_rvrs +df2$ln_wtrw + df2$dst_rvr + df2$ar_prtc +df2$ar_ndgn +df2$IDHM +df2$POP_201 +df2$PIB_PC +df2$dem_dat +df2$slop_dt +df2$wi_data +df2$dst_mnn +df2$dst_rbn +df2$dist_pa +df2$dist_il)
summary(model1)
plot (model1)
plot (model1$residuals)

#eh, deu bem ruim o R2, 53%... e varios atributos sem significancia nenhuma... teria que trabalhar melhor os atributos
# fiz um teste rapido no GeoDa utilizando o atributo Def como meta e deu R2 de 56% (então nao mudou muito pela alteracao da variavel principal)
# vou fazer uma abordagem considerando as relacoes espaciais em outro script