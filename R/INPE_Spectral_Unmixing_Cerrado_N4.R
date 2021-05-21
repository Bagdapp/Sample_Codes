#Graficos de correlação e avaliação das distribuições no R

rm(list=ls())

getwd()
setwd("C:/Users/Cesare/Desktop/")

library(ggplot2)
library(ggcorrplot)
library(GGally)
library(matlib)
library(plyr)
library(reshape2)


#Leitura dos CSVs que sairam do QGIS

WV2 <- read.csv(file = "DADOS_WV2_FINAL.csv", header = TRUE, sep = ",")
LAND8 <- read.csv(file = "Conjunto_Landsat8_final_csv.csv", header = TRUE, sep = ",")

# Geração dos conjuntos Espectrais Nível3

WV2MLME_Nivel4 = WV2[,c(8,9,10,138)]
LAND8MLME_Nivel4 = LAND8[,c(7,8,9,24)]


#correlação dos dados - É a mesma para todos os casos, independentemente da classe, 
#mas vou deixar tudo aqui para ter certeza

ggcorr(WV2MLME_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 2)
ggcorr(LAND8MLME_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 3)

######


WV2_boxplot <- melt(WV2MLME_Nivel4)
WV2_boxplot$Nivel4 <- factor(WV2_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(WV2_boxplot)


ggplot(WV2_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot(outlier.size=0.25, lwd=0.25)+facet_wrap(~Nivel4) +
  labs(x = "Componentes do MLME WorldView-2",y = "Valores") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) + ylim(-0.5,1.5)+ theme(text = element_text(size=20))




Landsat8_boxplot <- melt(LAND8MLME_Nivel4)
Landsat8_boxplot$Nivel4 <- factor(Landsat8_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(Landsat8_boxplot)


ggplot(Landsat8_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot(outlier.size=0.25, lwd=0.25)+facet_wrap(~Nivel4) +
  labs(x = "Componentes do MLME Landsat-8",y = "Valores") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) + ylim(-0.5,1.5)+ theme(text = element_text(size=20))


#plot dos dados com ggplot especificos para cada combinacao banda a banda e cada combinação de classes
#####ribeiro e walter nivel 3
###
#

ggplot(WV2MLME_Nivel4) +
  geom_point(aes(x=WV2MLME_Nivel4$Mean_Solo, 
                 y=WV2MLME_Nivel4$Mean_Sombra, 
                 colour=factor(WV2MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Solo",y = "WV2 Sombra")+ scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))

ggplot(WV2MLME_Nivel4) +
  geom_point(aes(x=WV2MLME_Nivel4$Mean_Solo, 
                 y=WV2MLME_Nivel4$Mean_Veg, 
                 colour=factor(WV2MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Solo",y = "WV2 Vegetação") + scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))

ggplot(WV2MLME_Nivel4) +
  geom_point(aes(x=WV2MLME_Nivel4$Mean_Sombra, 
                 y=WV2MLME_Nivel4$Mean_Veg, 
                 colour=factor(WV2MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Sombra",y = "WV2 Vegetação") + scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))

#landsat
ggplot(LAND8MLME_Nivel4) +
  geom_point(aes(x=LAND8MLME_Nivel4$Mean_Solo, 
                 y=LAND8MLME_Nivel4$Mean_Sombra, 
                 colour=factor(LAND8MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Solo",y = "L8 Sombra")+ scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))

ggplot(LAND8MLME_Nivel4) +
  geom_point(aes(x=LAND8MLME_Nivel4$Mean_Solo, 
                 y=LAND8MLME_Nivel4$Mean_Veg, 
                 colour=factor(LAND8MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Solo",y = "L8 Vegetação") + scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))

ggplot(LAND8MLME_Nivel4) +
  geom_point(aes(x=LAND8MLME_Nivel4$Mean_Sombra, 
                 y=LAND8MLME_Nivel4$Mean_Veg, 
                 colour=factor(LAND8MLME_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Sombra",y = "L8 Vegetação") + scale_y_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5)) + scale_x_continuous(breaks=c(-0.25,0,0.25,0.50,0.75,1.00,1.25,1.50), limits=c(-0.35,1.5))
#calculo das medias de cada atributo para cada classe


#####NIVEL4
Medias_WV2MLME_Nivel4<-ddply(WV2MLME_Nivel4, .(Nivel4), summarize, Mean_Solo=mean(Mean_Solo), 
                             Mean_Sombra=mean(Mean_Sombra), Mean_Veg=mean(Mean_Veg))

Medias_LAND8MLME_Nivel4<-ddply(LAND8MLME_Nivel4, .(Nivel4), summarize, Mean_Solo=mean(Mean_Solo), 
                               Mean_Sombra=mean(Mean_Sombra), Mean_Veg=mean(Mean_Veg))




#plots das medias das bandas - Comportamento espectral

####NIVEL4
###
#

dfWV2_N4 <- melt(Medias_WV2MLME_Nivel4)  #the function melt reshapes it from wide to long
dfWV2_N4$rowid <- 1:10  #add a rowid identifying variable


ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Componente",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação"))

ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Componente",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação"))


dfLAND8_N4 <- melt(Medias_LAND8MLME_Nivel4)  #the function melt reshapes it from wide to long
dfLAND8_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Componente",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação"))

ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Componente",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Solo" = "Solo", "Mean_Sombra" = "Sombra","Mean_Veg" = "Vegetação"))




#plots dos pontos médios das distribuicoes (escolher as bandas)

#WV2

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Solo, y=Medias_WV2MLME_Nivel4$Mean_Sombra, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "WV2 Solo",y = "WV2 Sombra")

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Solo, y=Medias_WV2MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "WV2 Solo",y = "WV2 Vegetação")

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Sombra, y=Medias_WV2MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "WV2 Sombra",y = "WV2 Vegetação")


###LAND8

ggplot(Medias_LAND8MLME_Nivel4) + 
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Solo, y=Medias_LAND8MLME_Nivel4$Mean_Sombra, 
                 shape=factor(Medias_LAND8MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "LAND8 Solo",y = "LAND8 Sombra")

ggplot(Medias_LAND8MLME_Nivel4) + 
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Solo, y=Medias_LAND8MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_LAND8MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "LAND8 Solo",y = "LAND8 Vegetação")

ggplot(Medias_LAND8MLME_Nivel4) + 
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Sombra, y=Medias_LAND8MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_LAND8MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8MLME_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "LAND8 Sombra",y = "LAND8 Vegetação")


#Aquisicao dos dados do dataframe para calcular a distancia banda x banda

#bandas do WV2

SOLO_CL_WV2=dfWV2_N4[1,3]
SOLO_CLU_WV2=dfWV2_N4[2,3]
SOLO_CLUCM_WV2=dfWV2_N4[3,3]
SOLO_CRUP_WV2=dfWV2_N4[4,3]
SOLO_CS_WV2=dfWV2_N4[5,3]
SOLO_CD_WV2=dfWV2_N4[6,3]
SOLO_CR_WV2=dfWV2_N4[7,3]
SOLO_CT_WV2=dfWV2_N4[8,3]
SOLO_MG_WV2=dfWV2_N4[9,3]
SOLO_VE_WV2=dfWV2_N4[10,3]

SOMBRA_CL_WV2=dfWV2_N4[11,3]
SOMBRA_CLU_WV2=dfWV2_N4[12,3]
SOMBRA_CLUCM_WV2=dfWV2_N4[13,3]
SOMBRA_CRUP_WV2=dfWV2_N4[14,3]
SOMBRA_CS_WV2=dfWV2_N4[15,3]
SOMBRA_CD_WV2=dfWV2_N4[16,3]
SOMBRA_CR_WV2=dfWV2_N4[17,3]
SOMBRA_CT_WV2=dfWV2_N4[18,3]
SOMBRA_MG_WV2=dfWV2_N4[19,3]
SOMBRA_VE_WV2=dfWV2_N4[20,3]

VEG_CL_WV2=dfWV2_N4[21,3]
VEG_CLU_WV2=dfWV2_N4[22,3]
VEG_CLUCM_WV2=dfWV2_N4[23,3]
VEG_CRUP_WV2=dfWV2_N4[24,3]
VEG_CS_WV2=dfWV2_N4[25,3]
VEG_CD_WV2=dfWV2_N4[26,3]
VEG_CR_WV2=dfWV2_N4[27,3]
VEG_CT_WV2=dfWV2_N4[28,3]
VEG_MG_WV2=dfWV2_N4[29,3]
VEG_VE_WV2=dfWV2_N4[30,3]


#bandas do landsat8


SOLO_CL_LAND8=dfLAND8_N4[1,3]
SOLO_CLU_LAND8=dfLAND8_N4[2,3]
SOLO_CLUCM_LAND8=dfLAND8_N4[3,3]
SOLO_CRUP_LAND8=dfLAND8_N4[4,3]
SOLO_CS_LAND8=dfLAND8_N4[5,3]
SOLO_CD_LAND8=dfLAND8_N4[6,3]
SOLO_CR_LAND8=dfLAND8_N4[7,3]
SOLO_CT_LAND8=dfLAND8_N4[8,3]
SOLO_MG_LAND8=dfLAND8_N4[9,3]
SOLO_VE_LAND8=dfLAND8_N4[10,3]

SOMBRA_CL_LAND8=dfLAND8_N4[11,3]
SOMBRA_CLU_LAND8=dfLAND8_N4[12,3]
SOMBRA_CLUCM_LAND8=dfLAND8_N4[13,3]
SOMBRA_CRUP_LAND8=dfLAND8_N4[14,3]
SOMBRA_CS_LAND8=dfLAND8_N4[15,3]
SOMBRA_CD_LAND8=dfLAND8_N4[16,3]
SOMBRA_CR_LAND8=dfLAND8_N4[17,3]
SOMBRA_CT_LAND8=dfLAND8_N4[18,3]
SOMBRA_MG_LAND8=dfLAND8_N4[19,3]
SOMBRA_VE_LAND8=dfLAND8_N4[20,3]

VEG_CL_LAND8=dfLAND8_N4[21,3]
VEG_CLU_LAND8=dfLAND8_N4[22,3]
VEG_CLUCM_LAND8=dfLAND8_N4[23,3]
VEG_CRUP_LAND8=dfLAND8_N4[24,3]
VEG_CS_LAND8=dfLAND8_N4[25,3]
VEG_CD_LAND8=dfLAND8_N4[26,3]
VEG_CR_LAND8=dfLAND8_N4[27,3]
VEG_CT_LAND8=dfLAND8_N4[28,3]
VEG_MG_LAND8=dfLAND8_N4[29,3]
VEG_VE_LAND8=dfLAND8_N4[30,3]


#distancia euclidiana dos pontos médios 

#distancia euclidiana dos pontos médios 
#SOLO X SOMBRA

Matriz_distancias_WV2_N4_SOLOxSOMBRA <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_SOLOxSOMBRA) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_SOLOxSOMBRA) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_SOLOxSOMBRA <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_SOLOxSOMBRA) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_SOLOxSOMBRA) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_SOLOxSOMBRA = sqrt(((SOLO_CLUCM_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CLUCM_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[2,1] <- distCLU_CLUCM_WV2_SOLOxSOMBRA

distCLU_CRUP_WV2_SOLOxSOMBRA = sqrt(((SOLO_CRUP_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CRUP_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[3,1] <- distCLU_CRUP_WV2_SOLOxSOMBRA

distCLU_CL_WV2_SOLOxSOMBRA = sqrt(((SOLO_CL_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CL_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[4,1] <- distCLU_CL_WV2_SOLOxSOMBRA

distCLU_CS_WV2_SOLOxSOMBRA = sqrt(((SOLO_CS_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CS_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[5,1] <- distCLU_CS_WV2_SOLOxSOMBRA

distCLU_CR_WV2_SOLOxSOMBRA = sqrt(((SOLO_CR_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CR_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[6,1] <- distCLU_CR_WV2_SOLOxSOMBRA

distCLU_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,1] <- distCLU_CT_WV2_SOLOxSOMBRA

distCLU_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,1] <- distCLU_CD_WV2_SOLOxSOMBRA

distCLU_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,1] <- distCLU_VE_WV2_SOLOxSOMBRA

distCLU_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CLU_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,1] <- distCLU_MG_WV2_SOLOxSOMBRA


#
distCLUCM_CRUP_WV2_SOLOxSOMBRA = sqrt(((SOLO_CRUP_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CRUP_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[3,2] <- distCLUCM_CRUP_WV2_SOLOxSOMBRA

distCLUCM_CL_WV2_SOLOxSOMBRA = sqrt(((SOLO_CL_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CL_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[4,2] <- distCLUCM_CL_WV2_SOLOxSOMBRA

distCLUCM_CS_WV2_SOLOxSOMBRA = sqrt(((SOLO_CS_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CS_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[5,2] <- distCLUCM_CS_WV2_SOLOxSOMBRA

distCLUCM_CR_WV2_SOLOxSOMBRA = sqrt(((SOLO_CR_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CR_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[6,2] <- distCLUCM_CR_WV2_SOLOxSOMBRA

distCLUCM_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,2] <- distCLUCM_CT_WV2_SOLOxSOMBRA

distCLUCM_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,2] <- distCLUCM_CD_WV2_SOLOxSOMBRA

distCLUCM_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,2] <- distCLUCM_VE_WV2_SOLOxSOMBRA

distCLUCM_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CLUCM_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,2] <- distCLUCM_MG_WV2_SOLOxSOMBRA


#
distCRUP_CL_WV2_SOLOxSOMBRA = sqrt(((SOLO_CL_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_CL_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[4,3] <- distCRUP_CL_WV2_SOLOxSOMBRA

distCRUP_CS_WV2_SOLOxSOMBRA = sqrt(((SOLO_CS_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_CS_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[5,3] <- distCRUP_CS_WV2_SOLOxSOMBRA

distCRUP_CR_WV2_SOLOxSOMBRA = sqrt(((SOLO_CR_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_CR_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[6,3] <- distCRUP_CR_WV2_SOLOxSOMBRA

distCRUP_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,3] <- distCRUP_CT_WV2_SOLOxSOMBRA

distCRUP_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,3] <- distCRUP_CD_WV2_SOLOxSOMBRA

distCRUP_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,3] <- distCRUP_VE_WV2_SOLOxSOMBRA

distCRUP_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CRUP_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,3] <- distCRUP_MG_WV2_SOLOxSOMBRA


#
distCL_CS_WV2_SOLOxSOMBRA = sqrt(((SOLO_CS_WV2-SOLO_CL_WV2)^2) +((SOMBRA_CS_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[5,4] <- distCL_CS_WV2_SOLOxSOMBRA

distCL_CR_WV2_SOLOxSOMBRA = sqrt(((SOLO_CR_WV2-SOLO_CL_WV2)^2) +((SOMBRA_CR_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[6,4] <- distCL_CR_WV2_SOLOxSOMBRA

distCL_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CL_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,4] <- distCL_CT_WV2_SOLOxSOMBRA

distCL_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CL_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,4] <- distCL_CD_WV2_SOLOxSOMBRA

distCL_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CL_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,4] <- distCL_VE_WV2_SOLOxSOMBRA

distCL_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CL_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,4] <- distCL_MG_WV2_SOLOxSOMBRA


#
distCS_CR_WV2_SOLOxSOMBRA = sqrt(((SOLO_CR_WV2-SOLO_CS_WV2)^2) +((SOMBRA_CR_WV2-SOMBRA_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[6,5] <- distCS_CR_WV2_SOLOxSOMBRA

distCS_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CS_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,5] <- distCS_CT_WV2_SOLOxSOMBRA

distCS_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CS_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,5] <- distCS_CD_WV2_SOLOxSOMBRA

distCS_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CS_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,5] <- distCS_VE_WV2_SOLOxSOMBRA

distCS_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CS_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,5] <- distCS_MG_WV2_SOLOxSOMBRA


#
distCR_CT_WV2_SOLOxSOMBRA = sqrt(((SOLO_CT_WV2-SOLO_CR_WV2)^2) +((SOMBRA_CT_WV2-SOMBRA_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[7,6] <- distCR_CT_WV2_SOLOxSOMBRA

distCR_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CR_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,6] <- distCR_CD_WV2_SOLOxSOMBRA

distCR_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CR_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,6] <- distCR_VE_WV2_SOLOxSOMBRA

distCR_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CR_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,6] <- distCR_MG_WV2_SOLOxSOMBRA


#
distCT_CD_WV2_SOLOxSOMBRA = sqrt(((SOLO_CD_WV2-SOLO_CT_WV2)^2) +((SOMBRA_CD_WV2-SOMBRA_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[8,7] <- distCT_CD_WV2_SOLOxSOMBRA

distCT_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CT_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,7] <- distCT_VE_WV2_SOLOxSOMBRA

distCT_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CT_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,7] <- distCT_MG_WV2_SOLOxSOMBRA

#
distCD_VE_WV2_SOLOxSOMBRA = sqrt(((SOLO_VE_WV2-SOLO_CD_WV2)^2) +((SOMBRA_VE_WV2-SOMBRA_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[9,8] <- distCD_VE_WV2_SOLOxSOMBRA

distCD_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_CD_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,8] <- distCD_MG_WV2_SOLOxSOMBRA

#
distVE_MG_WV2_SOLOxSOMBRA = sqrt(((SOLO_MG_WV2-SOLO_VE_WV2)^2) +((SOMBRA_MG_WV2-SOMBRA_VE_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxSOMBRA[10,9] <- distVE_MG_WV2_SOLOxSOMBRA


#LAND8

distCLU_CLUCM_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CLUCM_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CLUCM_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[2,1] <- distCLU_CLUCM_LAND8_SOLOxSOMBRA

distCLU_CRUP_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CRUP_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CRUP_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[3,1] <- distCLU_CRUP_LAND8_SOLOxSOMBRA

distCLU_CL_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CL_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CL_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[4,1] <- distCLU_CL_LAND8_SOLOxSOMBRA

distCLU_CS_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CS_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CS_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[5,1] <- distCLU_CS_LAND8_SOLOxSOMBRA

distCLU_CR_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CR_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CR_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[6,1] <- distCLU_CR_LAND8_SOLOxSOMBRA

distCLU_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,1] <- distCLU_CT_LAND8_SOLOxSOMBRA

distCLU_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,1] <- distCLU_CD_LAND8_SOLOxSOMBRA

distCLU_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,1] <- distCLU_VE_LAND8_SOLOxSOMBRA

distCLU_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CLU_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,1] <- distCLU_MG_LAND8_SOLOxSOMBRA


#
distCLUCM_CRUP_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CRUP_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CRUP_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[3,2] <- distCLUCM_CRUP_LAND8_SOLOxSOMBRA

distCLUCM_CL_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CL_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CL_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[4,2] <- distCLUCM_CL_LAND8_SOLOxSOMBRA

distCLUCM_CS_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CS_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CS_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[5,2] <- distCLUCM_CS_LAND8_SOLOxSOMBRA

distCLUCM_CR_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CR_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CR_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[6,2] <- distCLUCM_CR_LAND8_SOLOxSOMBRA

distCLUCM_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,2] <- distCLUCM_CT_LAND8_SOLOxSOMBRA

distCLUCM_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,2] <- distCLUCM_CD_LAND8_SOLOxSOMBRA

distCLUCM_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,2] <- distCLUCM_VE_LAND8_SOLOxSOMBRA

distCLUCM_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CLUCM_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,2] <- distCLUCM_MG_LAND8_SOLOxSOMBRA


#
distCRUP_CL_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CL_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_CL_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[4,3] <- distCRUP_CL_LAND8_SOLOxSOMBRA

distCRUP_CS_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CS_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_CS_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[5,3] <- distCRUP_CS_LAND8_SOLOxSOMBRA

distCRUP_CR_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CR_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_CR_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[6,3] <- distCRUP_CR_LAND8_SOLOxSOMBRA

distCRUP_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,3] <- distCRUP_CT_LAND8_SOLOxSOMBRA

distCRUP_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,3] <- distCRUP_CD_LAND8_SOLOxSOMBRA

distCRUP_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,3] <- distCRUP_VE_LAND8_SOLOxSOMBRA

distCRUP_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CRUP_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,3] <- distCRUP_MG_LAND8_SOLOxSOMBRA


#
distCL_CS_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CS_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_CS_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[5,4] <- distCL_CS_LAND8_SOLOxSOMBRA

distCL_CR_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CR_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_CR_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[6,4] <- distCL_CR_LAND8_SOLOxSOMBRA

distCL_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,4] <- distCL_CT_LAND8_SOLOxSOMBRA

distCL_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,4] <- distCL_CD_LAND8_SOLOxSOMBRA

distCL_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,4] <- distCL_VE_LAND8_SOLOxSOMBRA

distCL_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CL_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,4] <- distCL_MG_LAND8_SOLOxSOMBRA


#
distCS_CR_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CR_LAND8-SOLO_CS_LAND8)^2) +((SOMBRA_CR_LAND8-SOMBRA_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[6,5] <- distCS_CR_LAND8_SOLOxSOMBRA

distCS_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CS_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,5] <- distCS_CT_LAND8_SOLOxSOMBRA

distCS_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CS_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,5] <- distCS_CD_LAND8_SOLOxSOMBRA

distCS_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CS_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,5] <- distCS_VE_LAND8_SOLOxSOMBRA

distCS_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CS_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,5] <- distCS_MG_LAND8_SOLOxSOMBRA


#
distCR_CT_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CT_LAND8-SOLO_CR_LAND8)^2) +((SOMBRA_CT_LAND8-SOMBRA_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[7,6] <- distCR_CT_LAND8_SOLOxSOMBRA

distCR_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CR_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,6] <- distCR_CD_LAND8_SOLOxSOMBRA

distCR_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CR_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,6] <- distCR_VE_LAND8_SOLOxSOMBRA

distCR_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CR_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,6] <- distCR_MG_LAND8_SOLOxSOMBRA


#
distCT_CD_LAND8_SOLOxSOMBRA = sqrt(((SOLO_CD_LAND8-SOLO_CT_LAND8)^2) +((SOMBRA_CD_LAND8-SOMBRA_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[8,7] <- distCT_CD_LAND8_SOLOxSOMBRA

distCT_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CT_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,7] <- distCT_VE_LAND8_SOLOxSOMBRA

distCT_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CT_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,7] <- distCT_MG_LAND8_SOLOxSOMBRA

#
distCD_VE_LAND8_SOLOxSOMBRA = sqrt(((SOLO_VE_LAND8-SOLO_CD_LAND8)^2) +((SOMBRA_VE_LAND8-SOMBRA_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[9,8] <- distCD_VE_LAND8_SOLOxSOMBRA

distCD_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_CD_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,8] <- distCD_MG_LAND8_SOLOxSOMBRA

#
distVE_MG_LAND8_SOLOxSOMBRA = sqrt(((SOLO_MG_LAND8-SOLO_VE_LAND8)^2) +((SOMBRA_MG_LAND8-SOMBRA_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxSOMBRA[10,9] <- distVE_MG_LAND8_SOLOxSOMBRA



#teste da matriz resultante

Matriz_result_SOLOxSOMBRA = Matriz_distancias_WV2_N4_SOLOxSOMBRA - Matriz_distancias_LAND8_N4_SOLOxSOMBRA

melted_matriz_SOLOxSOMBRA <- melt(Matriz_result_SOLOxSOMBRA)
head(Matriz_result_SOLOxSOMBRA)

ggplot(data = melted_matriz_SOLOxSOMBRA, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-0.1,0.6),breaks=c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Solo, y=Medias_WV2MLME_Nivel4$Mean_Sombra, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Solo, y=Medias_LAND8MLME_Nivel4$Mean_Sombra, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Solo",y = "Sombra")


#distancia euclidiana dos pontos médios 

#distancia euclidiana dos pontos médios 
#SOLO X VEG

Matriz_distancias_WV2_N4_SOLOxVEG <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_SOLOxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_SOLOxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_SOLOxVEG <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_SOLOxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_SOLOxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_SOLOxVEG = sqrt(((SOLO_CLUCM_WV2-SOLO_CLU_WV2)^2) +((VEG_CLUCM_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[2,1] <- distCLU_CLUCM_WV2_SOLOxVEG

distCLU_CRUP_WV2_SOLOxVEG = sqrt(((SOLO_CRUP_WV2-SOLO_CLU_WV2)^2) +((VEG_CRUP_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[3,1] <- distCLU_CRUP_WV2_SOLOxVEG

distCLU_CL_WV2_SOLOxVEG = sqrt(((SOLO_CL_WV2-SOLO_CLU_WV2)^2) +((VEG_CL_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[4,1] <- distCLU_CL_WV2_SOLOxVEG

distCLU_CS_WV2_SOLOxVEG = sqrt(((SOLO_CS_WV2-SOLO_CLU_WV2)^2) +((VEG_CS_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[5,1] <- distCLU_CS_WV2_SOLOxVEG

distCLU_CR_WV2_SOLOxVEG = sqrt(((SOLO_CR_WV2-SOLO_CLU_WV2)^2) +((VEG_CR_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[6,1] <- distCLU_CR_WV2_SOLOxVEG

distCLU_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CLU_WV2)^2) +((VEG_CT_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,1] <- distCLU_CT_WV2_SOLOxVEG

distCLU_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CLU_WV2)^2) +((VEG_CD_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,1] <- distCLU_CD_WV2_SOLOxVEG

distCLU_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CLU_WV2)^2) +((VEG_VE_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,1] <- distCLU_VE_WV2_SOLOxVEG

distCLU_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CLU_WV2)^2) +((VEG_MG_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,1] <- distCLU_MG_WV2_SOLOxVEG


#
distCLUCM_CRUP_WV2_SOLOxVEG = sqrt(((SOLO_CRUP_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CRUP_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[3,2] <- distCLUCM_CRUP_WV2_SOLOxVEG

distCLUCM_CL_WV2_SOLOxVEG = sqrt(((SOLO_CL_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CL_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[4,2] <- distCLUCM_CL_WV2_SOLOxVEG

distCLUCM_CS_WV2_SOLOxVEG = sqrt(((SOLO_CS_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CS_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[5,2] <- distCLUCM_CS_WV2_SOLOxVEG

distCLUCM_CR_WV2_SOLOxVEG = sqrt(((SOLO_CR_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CR_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[6,2] <- distCLUCM_CR_WV2_SOLOxVEG

distCLUCM_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CT_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,2] <- distCLUCM_CT_WV2_SOLOxVEG

distCLUCM_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CLUCM_WV2)^2) +((VEG_CD_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,2] <- distCLUCM_CD_WV2_SOLOxVEG

distCLUCM_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CLUCM_WV2)^2) +((VEG_VE_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,2] <- distCLUCM_VE_WV2_SOLOxVEG

distCLUCM_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CLUCM_WV2)^2) +((VEG_MG_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,2] <- distCLUCM_MG_WV2_SOLOxVEG


#
distCRUP_CL_WV2_SOLOxVEG = sqrt(((SOLO_CL_WV2-SOLO_CRUP_WV2)^2) +((VEG_CL_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[4,3] <- distCRUP_CL_WV2_SOLOxVEG

distCRUP_CS_WV2_SOLOxVEG = sqrt(((SOLO_CS_WV2-SOLO_CRUP_WV2)^2) +((VEG_CS_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[5,3] <- distCRUP_CS_WV2_SOLOxVEG

distCRUP_CR_WV2_SOLOxVEG = sqrt(((SOLO_CR_WV2-SOLO_CRUP_WV2)^2) +((VEG_CR_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[6,3] <- distCRUP_CR_WV2_SOLOxVEG

distCRUP_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CRUP_WV2)^2) +((VEG_CT_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,3] <- distCRUP_CT_WV2_SOLOxVEG

distCRUP_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CRUP_WV2)^2) +((VEG_CD_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,3] <- distCRUP_CD_WV2_SOLOxVEG

distCRUP_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CRUP_WV2)^2) +((VEG_VE_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,3] <- distCRUP_VE_WV2_SOLOxVEG

distCRUP_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CRUP_WV2)^2) +((VEG_MG_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,3] <- distCRUP_MG_WV2_SOLOxVEG


#
distCL_CS_WV2_SOLOxVEG = sqrt(((SOLO_CS_WV2-SOLO_CL_WV2)^2) +((VEG_CS_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[5,4] <- distCL_CS_WV2_SOLOxVEG

distCL_CR_WV2_SOLOxVEG = sqrt(((SOLO_CR_WV2-SOLO_CL_WV2)^2) +((VEG_CR_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[6,4] <- distCL_CR_WV2_SOLOxVEG

distCL_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CL_WV2)^2) +((VEG_CT_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,4] <- distCL_CT_WV2_SOLOxVEG

distCL_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CL_WV2)^2) +((VEG_CD_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,4] <- distCL_CD_WV2_SOLOxVEG

distCL_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CL_WV2)^2) +((VEG_VE_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,4] <- distCL_VE_WV2_SOLOxVEG

distCL_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CL_WV2)^2) +((VEG_MG_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,4] <- distCL_MG_WV2_SOLOxVEG


#
distCS_CR_WV2_SOLOxVEG = sqrt(((SOLO_CR_WV2-SOLO_CS_WV2)^2) +((VEG_CR_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[6,5] <- distCS_CR_WV2_SOLOxVEG

distCS_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CS_WV2)^2) +((VEG_CT_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,5] <- distCS_CT_WV2_SOLOxVEG

distCS_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CS_WV2)^2) +((VEG_CD_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,5] <- distCS_CD_WV2_SOLOxVEG

distCS_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CS_WV2)^2) +((VEG_VE_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,5] <- distCS_VE_WV2_SOLOxVEG

distCS_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CS_WV2)^2) +((VEG_MG_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,5] <- distCS_MG_WV2_SOLOxVEG


#
distCR_CT_WV2_SOLOxVEG = sqrt(((SOLO_CT_WV2-SOLO_CR_WV2)^2) +((VEG_CT_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[7,6] <- distCR_CT_WV2_SOLOxVEG

distCR_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CR_WV2)^2) +((VEG_CD_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,6] <- distCR_CD_WV2_SOLOxVEG

distCR_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CR_WV2)^2) +((VEG_VE_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,6] <- distCR_VE_WV2_SOLOxVEG

distCR_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CR_WV2)^2) +((VEG_MG_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,6] <- distCR_MG_WV2_SOLOxVEG


#
distCT_CD_WV2_SOLOxVEG = sqrt(((SOLO_CD_WV2-SOLO_CT_WV2)^2) +((VEG_CD_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[8,7] <- distCT_CD_WV2_SOLOxVEG

distCT_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CT_WV2)^2) +((VEG_VE_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,7] <- distCT_VE_WV2_SOLOxVEG

distCT_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CT_WV2)^2) +((VEG_MG_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,7] <- distCT_MG_WV2_SOLOxVEG

#
distCD_VE_WV2_SOLOxVEG = sqrt(((SOLO_VE_WV2-SOLO_CD_WV2)^2) +((VEG_VE_WV2-VEG_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[9,8] <- distCD_VE_WV2_SOLOxVEG

distCD_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_CD_WV2)^2) +((VEG_MG_WV2-VEG_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,8] <- distCD_MG_WV2_SOLOxVEG

#
distVE_MG_WV2_SOLOxVEG = sqrt(((SOLO_MG_WV2-SOLO_VE_WV2)^2) +((VEG_MG_WV2-VEG_VE_WV2)^2))
Matriz_distancias_WV2_N4_SOLOxVEG[10,9] <- distVE_MG_WV2_SOLOxVEG


#LAND8

distCLU_CLUCM_LAND8_SOLOxVEG = sqrt(((SOLO_CLUCM_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CLUCM_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[2,1] <- distCLU_CLUCM_LAND8_SOLOxVEG

distCLU_CRUP_LAND8_SOLOxVEG = sqrt(((SOLO_CRUP_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CRUP_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[3,1] <- distCLU_CRUP_LAND8_SOLOxVEG

distCLU_CL_LAND8_SOLOxVEG = sqrt(((SOLO_CL_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CL_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[4,1] <- distCLU_CL_LAND8_SOLOxVEG

distCLU_CS_LAND8_SOLOxVEG = sqrt(((SOLO_CS_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CS_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[5,1] <- distCLU_CS_LAND8_SOLOxVEG

distCLU_CR_LAND8_SOLOxVEG = sqrt(((SOLO_CR_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CR_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[6,1] <- distCLU_CR_LAND8_SOLOxVEG

distCLU_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CT_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,1] <- distCLU_CT_LAND8_SOLOxVEG

distCLU_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CLU_LAND8)^2) +((VEG_CD_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,1] <- distCLU_CD_LAND8_SOLOxVEG

distCLU_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CLU_LAND8)^2) +((VEG_VE_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,1] <- distCLU_VE_LAND8_SOLOxVEG

distCLU_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CLU_LAND8)^2) +((VEG_MG_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,1] <- distCLU_MG_LAND8_SOLOxVEG


#
distCLUCM_CRUP_LAND8_SOLOxVEG = sqrt(((SOLO_CRUP_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CRUP_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[3,2] <- distCLUCM_CRUP_LAND8_SOLOxVEG

distCLUCM_CL_LAND8_SOLOxVEG = sqrt(((SOLO_CL_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CL_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[4,2] <- distCLUCM_CL_LAND8_SOLOxVEG

distCLUCM_CS_LAND8_SOLOxVEG = sqrt(((SOLO_CS_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CS_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[5,2] <- distCLUCM_CS_LAND8_SOLOxVEG

distCLUCM_CR_LAND8_SOLOxVEG = sqrt(((SOLO_CR_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CR_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[6,2] <- distCLUCM_CR_LAND8_SOLOxVEG

distCLUCM_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CT_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,2] <- distCLUCM_CT_LAND8_SOLOxVEG

distCLUCM_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_CD_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,2] <- distCLUCM_CD_LAND8_SOLOxVEG

distCLUCM_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_VE_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,2] <- distCLUCM_VE_LAND8_SOLOxVEG

distCLUCM_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CLUCM_LAND8)^2) +((VEG_MG_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,2] <- distCLUCM_MG_LAND8_SOLOxVEG


#
distCRUP_CL_LAND8_SOLOxVEG = sqrt(((SOLO_CL_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_CL_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[4,3] <- distCRUP_CL_LAND8_SOLOxVEG

distCRUP_CS_LAND8_SOLOxVEG = sqrt(((SOLO_CS_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_CS_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[5,3] <- distCRUP_CS_LAND8_SOLOxVEG

distCRUP_CR_LAND8_SOLOxVEG = sqrt(((SOLO_CR_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_CR_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[6,3] <- distCRUP_CR_LAND8_SOLOxVEG

distCRUP_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_CT_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,3] <- distCRUP_CT_LAND8_SOLOxVEG

distCRUP_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_CD_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,3] <- distCRUP_CD_LAND8_SOLOxVEG

distCRUP_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_VE_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,3] <- distCRUP_VE_LAND8_SOLOxVEG

distCRUP_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CRUP_LAND8)^2) +((VEG_MG_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,3] <- distCRUP_MG_LAND8_SOLOxVEG


#
distCL_CS_LAND8_SOLOxVEG = sqrt(((SOLO_CS_LAND8-SOLO_CL_LAND8)^2) +((VEG_CS_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[5,4] <- distCL_CS_LAND8_SOLOxVEG

distCL_CR_LAND8_SOLOxVEG = sqrt(((SOLO_CR_LAND8-SOLO_CL_LAND8)^2) +((VEG_CR_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[6,4] <- distCL_CR_LAND8_SOLOxVEG

distCL_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CL_LAND8)^2) +((VEG_CT_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,4] <- distCL_CT_LAND8_SOLOxVEG

distCL_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CL_LAND8)^2) +((VEG_CD_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,4] <- distCL_CD_LAND8_SOLOxVEG

distCL_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CL_LAND8)^2) +((VEG_VE_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,4] <- distCL_VE_LAND8_SOLOxVEG

distCL_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CL_LAND8)^2) +((VEG_MG_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,4] <- distCL_MG_LAND8_SOLOxVEG


#
distCS_CR_LAND8_SOLOxVEG = sqrt(((SOLO_CR_LAND8-SOLO_CS_LAND8)^2) +((VEG_CR_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[6,5] <- distCS_CR_LAND8_SOLOxVEG

distCS_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CS_LAND8)^2) +((VEG_CT_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,5] <- distCS_CT_LAND8_SOLOxVEG

distCS_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CS_LAND8)^2) +((VEG_CD_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,5] <- distCS_CD_LAND8_SOLOxVEG

distCS_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CS_LAND8)^2) +((VEG_VE_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,5] <- distCS_VE_LAND8_SOLOxVEG

distCS_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CS_LAND8)^2) +((VEG_MG_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,5] <- distCS_MG_LAND8_SOLOxVEG


#
distCR_CT_LAND8_SOLOxVEG = sqrt(((SOLO_CT_LAND8-SOLO_CR_LAND8)^2) +((VEG_CT_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[7,6] <- distCR_CT_LAND8_SOLOxVEG

distCR_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CR_LAND8)^2) +((VEG_CD_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,6] <- distCR_CD_LAND8_SOLOxVEG

distCR_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CR_LAND8)^2) +((VEG_VE_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,6] <- distCR_VE_LAND8_SOLOxVEG

distCR_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CR_LAND8)^2) +((VEG_MG_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,6] <- distCR_MG_LAND8_SOLOxVEG


#
distCT_CD_LAND8_SOLOxVEG = sqrt(((SOLO_CD_LAND8-SOLO_CT_LAND8)^2) +((VEG_CD_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[8,7] <- distCT_CD_LAND8_SOLOxVEG

distCT_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CT_LAND8)^2) +((VEG_VE_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,7] <- distCT_VE_LAND8_SOLOxVEG

distCT_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CT_LAND8)^2) +((VEG_MG_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,7] <- distCT_MG_LAND8_SOLOxVEG

#
distCD_VE_LAND8_SOLOxVEG = sqrt(((SOLO_VE_LAND8-SOLO_CD_LAND8)^2) +((VEG_VE_LAND8-VEG_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[9,8] <- distCD_VE_LAND8_SOLOxVEG

distCD_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_CD_LAND8)^2) +((VEG_MG_LAND8-VEG_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,8] <- distCD_MG_LAND8_SOLOxVEG

#
distVE_MG_LAND8_SOLOxVEG = sqrt(((SOLO_MG_LAND8-SOLO_VE_LAND8)^2) +((VEG_MG_LAND8-VEG_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_SOLOxVEG[10,9] <- distVE_MG_LAND8_SOLOxVEG



#teste da matriz resultante

Matriz_result_SOLOxVEG = Matriz_distancias_WV2_N4_SOLOxVEG - Matriz_distancias_LAND8_N4_SOLOxVEG

melted_matriz_SOLOxVEG <- melt(Matriz_result_SOLOxVEG)
head(Matriz_result_SOLOxVEG)

ggplot(data = melted_matriz_SOLOxVEG, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-0.1,0.6),breaks=c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Solo, y=Medias_WV2MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Solo, y=Medias_LAND8MLME_Nivel4$Mean_Veg, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Solo",y = "Vegetação")


#distancia euclidiana dos pontos médios 
#SOMBRA X VEG

Matriz_distancias_WV2_N4_SOMBRAxVEG <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_SOMBRAxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_SOMBRAxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_SOMBRAxVEG <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_SOMBRAxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_SOMBRAxVEG) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CLUCM_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CLUCM_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[2,1] <- distCLU_CLUCM_WV2_SOMBRAxVEG

distCLU_CRUP_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CRUP_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CRUP_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[3,1] <- distCLU_CRUP_WV2_SOMBRAxVEG

distCLU_CL_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CL_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CL_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[4,1] <- distCLU_CL_WV2_SOMBRAxVEG

distCLU_CS_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CS_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CS_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[5,1] <- distCLU_CS_WV2_SOMBRAxVEG

distCLU_CR_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CR_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CR_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[6,1] <- distCLU_CR_WV2_SOMBRAxVEG

distCLU_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CT_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,1] <- distCLU_CT_WV2_SOMBRAxVEG

distCLU_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CLU_WV2)^2) +((VEG_CD_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,1] <- distCLU_CD_WV2_SOMBRAxVEG

distCLU_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CLU_WV2)^2) +((VEG_VE_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,1] <- distCLU_VE_WV2_SOMBRAxVEG

distCLU_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CLU_WV2)^2) +((VEG_MG_WV2-VEG_CLU_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,1] <- distCLU_MG_WV2_SOMBRAxVEG


#
distCLUCM_CRUP_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CRUP_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CRUP_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[3,2] <- distCLUCM_CRUP_WV2_SOMBRAxVEG

distCLUCM_CL_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CL_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CL_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[4,2] <- distCLUCM_CL_WV2_SOMBRAxVEG

distCLUCM_CS_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CS_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CS_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[5,2] <- distCLUCM_CS_WV2_SOMBRAxVEG

distCLUCM_CR_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CR_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CR_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[6,2] <- distCLUCM_CR_WV2_SOMBRAxVEG

distCLUCM_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CT_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,2] <- distCLUCM_CT_WV2_SOMBRAxVEG

distCLUCM_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_CD_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,2] <- distCLUCM_CD_WV2_SOMBRAxVEG

distCLUCM_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_VE_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,2] <- distCLUCM_VE_WV2_SOMBRAxVEG

distCLUCM_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CLUCM_WV2)^2) +((VEG_MG_WV2-VEG_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,2] <- distCLUCM_MG_WV2_SOMBRAxVEG


#
distCRUP_CL_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CL_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_CL_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[4,3] <- distCRUP_CL_WV2_SOMBRAxVEG

distCRUP_CS_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CS_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_CS_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[5,3] <- distCRUP_CS_WV2_SOMBRAxVEG

distCRUP_CR_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CR_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_CR_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[6,3] <- distCRUP_CR_WV2_SOMBRAxVEG

distCRUP_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_CT_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,3] <- distCRUP_CT_WV2_SOMBRAxVEG

distCRUP_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_CD_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,3] <- distCRUP_CD_WV2_SOMBRAxVEG

distCRUP_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_VE_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,3] <- distCRUP_VE_WV2_SOMBRAxVEG

distCRUP_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CRUP_WV2)^2) +((VEG_MG_WV2-VEG_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,3] <- distCRUP_MG_WV2_SOMBRAxVEG


#
distCL_CS_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CS_WV2-SOMBRA_CL_WV2)^2) +((VEG_CS_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[5,4] <- distCL_CS_WV2_SOMBRAxVEG

distCL_CR_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CR_WV2-SOMBRA_CL_WV2)^2) +((VEG_CR_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[6,4] <- distCL_CR_WV2_SOMBRAxVEG

distCL_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CL_WV2)^2) +((VEG_CT_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,4] <- distCL_CT_WV2_SOMBRAxVEG

distCL_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CL_WV2)^2) +((VEG_CD_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,4] <- distCL_CD_WV2_SOMBRAxVEG

distCL_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CL_WV2)^2) +((VEG_VE_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,4] <- distCL_VE_WV2_SOMBRAxVEG

distCL_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CL_WV2)^2) +((VEG_MG_WV2-VEG_CL_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,4] <- distCL_MG_WV2_SOMBRAxVEG


#
distCS_CR_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CR_WV2-SOMBRA_CS_WV2)^2) +((VEG_CR_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[6,5] <- distCS_CR_WV2_SOMBRAxVEG

distCS_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CS_WV2)^2) +((VEG_CT_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,5] <- distCS_CT_WV2_SOMBRAxVEG

distCS_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CS_WV2)^2) +((VEG_CD_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,5] <- distCS_CD_WV2_SOMBRAxVEG

distCS_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CS_WV2)^2) +((VEG_VE_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,5] <- distCS_VE_WV2_SOMBRAxVEG

distCS_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CS_WV2)^2) +((VEG_MG_WV2-VEG_CS_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,5] <- distCS_MG_WV2_SOMBRAxVEG


#
distCR_CT_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CT_WV2-SOMBRA_CR_WV2)^2) +((VEG_CT_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[7,6] <- distCR_CT_WV2_SOMBRAxVEG

distCR_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CR_WV2)^2) +((VEG_CD_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,6] <- distCR_CD_WV2_SOMBRAxVEG

distCR_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CR_WV2)^2) +((VEG_VE_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,6] <- distCR_VE_WV2_SOMBRAxVEG

distCR_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CR_WV2)^2) +((VEG_MG_WV2-VEG_CR_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,6] <- distCR_MG_WV2_SOMBRAxVEG


#
distCT_CD_WV2_SOMBRAxVEG = sqrt(((SOMBRA_CD_WV2-SOMBRA_CT_WV2)^2) +((VEG_CD_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[8,7] <- distCT_CD_WV2_SOMBRAxVEG

distCT_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CT_WV2)^2) +((VEG_VE_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,7] <- distCT_VE_WV2_SOMBRAxVEG

distCT_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CT_WV2)^2) +((VEG_MG_WV2-VEG_CT_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,7] <- distCT_MG_WV2_SOMBRAxVEG

#
distCD_VE_WV2_SOMBRAxVEG = sqrt(((SOMBRA_VE_WV2-SOMBRA_CD_WV2)^2) +((VEG_VE_WV2-VEG_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[9,8] <- distCD_VE_WV2_SOMBRAxVEG

distCD_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_CD_WV2)^2) +((VEG_MG_WV2-VEG_CD_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,8] <- distCD_MG_WV2_SOMBRAxVEG

#
distVE_MG_WV2_SOMBRAxVEG = sqrt(((SOMBRA_MG_WV2-SOMBRA_VE_WV2)^2) +((VEG_MG_WV2-VEG_VE_WV2)^2))
Matriz_distancias_WV2_N4_SOMBRAxVEG[10,9] <- distVE_MG_WV2_SOMBRAxVEG


#LAND8

distCLU_CLUCM_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CLUCM_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CLUCM_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[2,1] <- distCLU_CLUCM_LAND8_SOMBRAxVEG

distCLU_CRUP_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CRUP_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CRUP_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[3,1] <- distCLU_CRUP_LAND8_SOMBRAxVEG

distCLU_CL_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CL_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CL_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[4,1] <- distCLU_CL_LAND8_SOMBRAxVEG

distCLU_CS_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CS_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CS_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[5,1] <- distCLU_CS_LAND8_SOMBRAxVEG

distCLU_CR_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CR_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CR_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[6,1] <- distCLU_CR_LAND8_SOMBRAxVEG

distCLU_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CT_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,1] <- distCLU_CT_LAND8_SOMBRAxVEG

distCLU_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_CD_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,1] <- distCLU_CD_LAND8_SOMBRAxVEG

distCLU_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_VE_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,1] <- distCLU_VE_LAND8_SOMBRAxVEG

distCLU_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CLU_LAND8)^2) +((VEG_MG_LAND8-VEG_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,1] <- distCLU_MG_LAND8_SOMBRAxVEG


#
distCLUCM_CRUP_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CRUP_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CRUP_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[3,2] <- distCLUCM_CRUP_LAND8_SOMBRAxVEG

distCLUCM_CL_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CL_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CL_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[4,2] <- distCLUCM_CL_LAND8_SOMBRAxVEG

distCLUCM_CS_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CS_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CS_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[5,2] <- distCLUCM_CS_LAND8_SOMBRAxVEG

distCLUCM_CR_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CR_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CR_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[6,2] <- distCLUCM_CR_LAND8_SOMBRAxVEG

distCLUCM_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CT_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,2] <- distCLUCM_CT_LAND8_SOMBRAxVEG

distCLUCM_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_CD_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,2] <- distCLUCM_CD_LAND8_SOMBRAxVEG

distCLUCM_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_VE_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,2] <- distCLUCM_VE_LAND8_SOMBRAxVEG

distCLUCM_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CLUCM_LAND8)^2) +((VEG_MG_LAND8-VEG_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,2] <- distCLUCM_MG_LAND8_SOMBRAxVEG


#
distCRUP_CL_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CL_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_CL_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[4,3] <- distCRUP_CL_LAND8_SOMBRAxVEG

distCRUP_CS_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CS_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_CS_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[5,3] <- distCRUP_CS_LAND8_SOMBRAxVEG

distCRUP_CR_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CR_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_CR_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[6,3] <- distCRUP_CR_LAND8_SOMBRAxVEG

distCRUP_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_CT_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,3] <- distCRUP_CT_LAND8_SOMBRAxVEG

distCRUP_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_CD_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,3] <- distCRUP_CD_LAND8_SOMBRAxVEG

distCRUP_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_VE_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,3] <- distCRUP_VE_LAND8_SOMBRAxVEG

distCRUP_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CRUP_LAND8)^2) +((VEG_MG_LAND8-VEG_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,3] <- distCRUP_MG_LAND8_SOMBRAxVEG


#
distCL_CS_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CS_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_CS_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[5,4] <- distCL_CS_LAND8_SOMBRAxVEG

distCL_CR_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CR_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_CR_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[6,4] <- distCL_CR_LAND8_SOMBRAxVEG

distCL_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_CT_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,4] <- distCL_CT_LAND8_SOMBRAxVEG

distCL_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_CD_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,4] <- distCL_CD_LAND8_SOMBRAxVEG

distCL_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_VE_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,4] <- distCL_VE_LAND8_SOMBRAxVEG

distCL_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CL_LAND8)^2) +((VEG_MG_LAND8-VEG_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,4] <- distCL_MG_LAND8_SOMBRAxVEG


#
distCS_CR_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CR_LAND8-SOMBRA_CS_LAND8)^2) +((VEG_CR_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[6,5] <- distCS_CR_LAND8_SOMBRAxVEG

distCS_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CS_LAND8)^2) +((VEG_CT_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,5] <- distCS_CT_LAND8_SOMBRAxVEG

distCS_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CS_LAND8)^2) +((VEG_CD_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,5] <- distCS_CD_LAND8_SOMBRAxVEG

distCS_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CS_LAND8)^2) +((VEG_VE_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,5] <- distCS_VE_LAND8_SOMBRAxVEG

distCS_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CS_LAND8)^2) +((VEG_MG_LAND8-VEG_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,5] <- distCS_MG_LAND8_SOMBRAxVEG


#
distCR_CT_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CT_LAND8-SOMBRA_CR_LAND8)^2) +((VEG_CT_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[7,6] <- distCR_CT_LAND8_SOMBRAxVEG

distCR_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CR_LAND8)^2) +((VEG_CD_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,6] <- distCR_CD_LAND8_SOMBRAxVEG

distCR_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CR_LAND8)^2) +((VEG_VE_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,6] <- distCR_VE_LAND8_SOMBRAxVEG

distCR_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CR_LAND8)^2) +((VEG_MG_LAND8-VEG_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,6] <- distCR_MG_LAND8_SOMBRAxVEG


#
distCT_CD_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_CD_LAND8-SOMBRA_CT_LAND8)^2) +((VEG_CD_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[8,7] <- distCT_CD_LAND8_SOMBRAxVEG

distCT_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CT_LAND8)^2) +((VEG_VE_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,7] <- distCT_VE_LAND8_SOMBRAxVEG

distCT_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CT_LAND8)^2) +((VEG_MG_LAND8-VEG_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,7] <- distCT_MG_LAND8_SOMBRAxVEG

#
distCD_VE_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_VE_LAND8-SOMBRA_CD_LAND8)^2) +((VEG_VE_LAND8-VEG_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[9,8] <- distCD_VE_LAND8_SOMBRAxVEG

distCD_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_CD_LAND8)^2) +((VEG_MG_LAND8-VEG_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,8] <- distCD_MG_LAND8_SOMBRAxVEG

#
distVE_MG_LAND8_SOMBRAxVEG = sqrt(((SOMBRA_MG_LAND8-SOMBRA_VE_LAND8)^2) +((VEG_MG_LAND8-VEG_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_SOMBRAxVEG[10,9] <- distVE_MG_LAND8_SOMBRAxVEG



#teste da matriz resultante

Matriz_result_SOMBRAxVEG = Matriz_distancias_WV2_N4_SOMBRAxVEG - Matriz_distancias_LAND8_N4_SOMBRAxVEG

melted_matriz_SOMBRAxVEG <- melt(Matriz_result_SOMBRAxVEG)
head(Matriz_result_SOMBRAxVEG)

ggplot(data = melted_matriz_SOMBRAxVEG, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-0.1,0.6),breaks=c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS

ggplot(Medias_WV2MLME_Nivel4) + 
  geom_point(aes(x=Medias_WV2MLME_Nivel4$Mean_Sombra, y=Medias_WV2MLME_Nivel4$Mean_Veg, 
                 shape=factor(Medias_WV2MLME_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2MLME_Nivel4$Nivel4)),size=4) +
  geom_point(aes(x=Medias_LAND8MLME_Nivel4$Mean_Sombra, y=Medias_LAND8MLME_Nivel4$Mean_Veg, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Sombra",y = "Vegetação")
