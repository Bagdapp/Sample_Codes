#Exercicio de análise espacial - ITV #2

#Cesare Di Girolamo Neto

#Este exercicio sera resolvido fazendo uma regressão espacial seguindo as seguintes etapas

# 1 - Definicao de objeto de estudo, escalas, classes de uso e outros --> Ja preparado

# 2 - Preparar o banco de dados espacial (poligonos e relacoes) -- Ja preparado

# 3 - Analise exploratoria dos dados***

# 4 - regressao linear

# 5 - analise de moran nos residuos

# 6 - gerar um Spatial Lag Model

# não vou fazer transformações no dados nem analises de correlação pois ja estao no outro script



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
library(spdep)
library(maptools)



#abrir e verificar atributos do shp


shp<-readOGR(dsn=".",layer = "Grid_data_2018_2019")
summary(shp)
#class(shp)
#crs(shp)
plot(shp, col="green")


#criar um objeto de spatial polygons

spols<-polygons(shp)
summary(spols)
#class(spols)
#crs(spols)
#plot(spols, col="green")

#transformar o shp em uma df e verificar propriedades

df<-data.frame(shp)
#class(df)
#View(df)
str(df)

# criar matriz de pesos espaciais W

polgal<-poly2nb(spols)
polgal<-poly2nb(spols, queen = TRUE) #optei pelo queen pq tinham umas ilhas no meio q nao tinham continuidade usando rook
plot(polgal,coordinates(spols))
plot(spols,add=T)

class(polgal)
summary(polgal)

W_polgal<-nb2listw(polgal)
#summary(W_polgal)
#class(W_polgal)
W_polgal$weights


#nao vou fazer aquela analise exploratoria dos dados do script 1, vou direto para a regressao linear e espacial
# só um exemplo de plot de dados

plot(Def~ed_3, data=df)

#regressao linear considerando a classe Def como variavel resposta e removendo NDef e Tot_pix

model1 <- lm (df$Def ~ df$ed_3 + df$plnd_15 + df$pland_3 + df$ln_st_r + df$ln_f_rd + df$dst_rds +df$ln_rvrs +df$ln_wtrw + df$dst_rvr + df$ar_prtc +df$ar_ndgn +df$IDHM +df$POP_201 +df$PIB_PC +df$dem_dat +df$slop_dt +df$wi_data +df$dst_mnn +df$dst_rbn +df$dist_pa +df$dist_il)
summary(model1)
#plot (model1)
plot (model1$residuals)


#adicionar o modelo acima como um atributo da df

class(lm)
df.lm <- lm (df$Def ~ df$ed_3 + df$plnd_15 + df$pland_3 + df$ln_st_r + df$ln_f_rd + df$dst_rds +df$ln_rvrs +df$ln_wtrw + df$dst_rvr + df$ar_prtc +df$ar_ndgn +df$IDHM +df$POP_201 +df$PIB_PC +df$dem_dat +df$slop_dt +df$wi_data +df$dst_mnn +df$dst_rbn +df$dist_pa +df$dist_il, data = df)

#class(df.lm)
#is.list(df.lm)
summary(df.lm)
names(df.lm)

#analise dos residuos

brks <- round(fivenum(df.lm$res), digits=2)
print(brks)
cols <- rev(heat.colors(4))
plot(spols,col = cols[findInterval(df.lm$residuals,brks)])
title(main="Mapa dos resíduos da regressão linear multipla - Variável Def")
hist(df.lm$residuals)

#residuos vs atributos (escolher o atrb que vai colocar aqui) e residuos vc reg. linear

plot(df.lm$residuals, df$ln_st_r)
title(main= "Mapa dos resíduos x Ln_st_r")

plot(df.lm$fitted.values, df.lm$residuals)
title(main= "Gráfico dos valores ajustados X resíduos")

#teste de moram para ver autocorrelacao espacial

tmoran <- lm.morantest (df.lm, W_polgal, alternative="two.sided")
tmoran

# existe correlacao espacial

# vou utilizar os multiplicadores de lagrange para distinguir os modelos de spatial error/ spatial lag

df.lagrange <- lm.LMtests(df.lm,W_polgal,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
df.lagrange

# avaliando o p valor dop LMerr e do LMlag os dois sao significantes, e os valores dos robustos também!
# eles são todos iguais ainda por cima, normalmente eu escolheria o mais significante, mas aqui acho que
# pode ser qualquer um dos dois, ai depois selecionaria pelo melhor AIC, mas vou fazer um deles apenas

# Spatial Lag Model - demora uns 8-9 min pra rodar essa parte

df.lag <- lagsarlm(df$Def ~ df$ed_3 + df$plnd_15 + df$pland_3 + df$ln_st_r + df$ln_f_rd + df$dst_rds +df$ln_rvrs +df$ln_wtrw + df$dst_rvr + df$ar_prtc +df$ar_ndgn +df$IDHM +df$POP_201 +df$PIB_PC +df$dem_dat +df$slop_dt +df$wi_data +df$dst_mnn +df$dst_rbn +df$dist_pa +df$dist_il, data=df,listw=W_polgal)
summary(df.lag)
names(df.lag)

df.lag #mostra os coeficientes do modelo de regressão

# plotando os residuos

plot(spols)
brks <- round(fivenum(df.lag$residuals), digits=2)
brks
cols <- rev(heat.colors(4))
plot(spols,col = cols[findInterval(df.lag$residuals,brks)])
title(main="Mapa dos resíduos do Spatial Lag Model")

# com um modelo espacial conctruido, nao basta extrair os mesmos atributos para o ano seguinte e rodar no modelo
# para ter as predicoes de cenarios futuros deve-se calcular a Demanda (quantidade de desflorestamento estimado)
# potencial (capacidade de transicao de cada celula) e relacionar os 2 pela variável
# de alocação, fazendo uma modelagem ciclica para gera mapas em um ano futuro

