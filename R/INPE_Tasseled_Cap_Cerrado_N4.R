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

WV2TC_Nivel4 = WV2[,c(11,12,13,138)]
LAND8TC_Nivel4 = LAND8[,c(10,11,12,24)]

####


WV2_boxplot <- melt(WV2TC_Nivel4)
WV2_boxplot$Nivel4 <- factor(WV2_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(WV2_boxplot)


ggplot(WV2_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot(outlier.size=0.25, lwd=0.25)+facet_wrap(~Nivel4) +
  labs(x = "Componentes do MLME WorldView-2",y = "Valores") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_Green" = "Greenness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) + ylim(-0.3,0.4) + theme(text = element_text(size=20))




Landsat8_boxplot <- melt(LAND8TC_Nivel4)
Landsat8_boxplot$Nivel4 <- factor(Landsat8_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(Landsat8_boxplot)


ggplot(Landsat8_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot(outlier.size=0.25, lwd=0.25)+facet_wrap(~Nivel4) +
  labs(x = "Componentes do MLME Landsat-8",y = "Valores") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_Green" = "Greenness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) + ylim(-0.3,0.4) + theme(text = element_text(size=20))


#correlação dos dados - É a mesma para todos os casos, independentemente da classe, 
#mas vou deixar tudo aqui para ter certeza

ggcorr(WV2TC_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 2)
ggcorr(LAND8TC_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 3)



#plot dos dados com ggplot especificos para cada combinacao banda a banda e cada combinação de classes
#####ribeiro e walter nivel 3
###
#

ggplot(WV2TC_Nivel4) +
  geom_point(aes(x=WV2TC_Nivel4$Mean_Bright, y=WV2TC_Nivel4$Mean_Green, 
                 colour=factor(WV2TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Brightness",y = "WV2 Greeness")+ xlim(0.1,0.4) +ylim(0.0,0.4)

ggplot(WV2TC_Nivel4) +
  geom_point(aes(x=WV2TC_Nivel4$Mean_Wet, y=WV2TC_Nivel4$Mean_Green, 
                 colour=factor(WV2TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Wetness",y = "WV2 Greeness") + xlim(-0.25,0.05) +ylim(0.0,0.45)

ggplot(WV2TC_Nivel4) +
  geom_point(aes(x=WV2TC_Nivel4$Mean_Bright, y=WV2TC_Nivel4$Mean_Wet, 
                 colour=factor(WV2TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Brightness",y = "WV2 Wetness")  + xlim(0.15,0.40) +ylim(-0.25,0.05)



ggplot(LAND8TC_Nivel4) +
  geom_point(aes(x=LAND8TC_Nivel4$Mean_Bright, y=LAND8TC_Nivel4$Mean_Green, 
                 colour=factor(LAND8TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "LAND8 Brightness",y = "LAND8 Greeness")+ xlim(0.1,0.4) +ylim(0.0,0.4)

ggplot(LAND8TC_Nivel4) +
  geom_point(aes(x=LAND8TC_Nivel4$Mean_Wet, y=LAND8TC_Nivel4$Mean_Green, 
                 colour=factor(LAND8TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "LAND8 Wetness",y = "LAND8 Greeness") + xlim(-0.25,0.05) +ylim(0.0,0.45)

ggplot(LAND8TC_Nivel4) +
  geom_point(aes(x=LAND8TC_Nivel4$Mean_Bright, y=LAND8TC_Nivel4$Mean_Wet, 
                 colour=factor(LAND8TC_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "LAND8 Brightness",y = "LAND8 Wetness")  + xlim(0.15,0.40) +ylim(-0.25,0.05)



#calculo das medias de cada atributo para cada classe


#####NIVEL4
Medias_WV2TC_Nivel4<-ddply(WV2TC_Nivel4, .(Nivel4), summarize, Mean_Green=mean(Mean_Green), 
                           Mean_Bright=mean(Mean_Bright), Mean_Wet=mean(Mean_Wet))

Medias_LAND8TC_Nivel4<-ddply(LAND8TC_Nivel4, .(Nivel4), summarize, Mean_Green=mean(Mean_Green), 
                             Mean_Bright=mean(Mean_Bright), Mean_Wet=mean(Mean_Wet))




#plots das medias das bandas - Comportamento espectral

####NIVEL4
###
#

dfWV2_N4 <- melt(Medias_WV2TC_Nivel4)  #the function melt reshapes it from wide to long
dfWV2_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Componente TC WV2",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Green" = "Greeness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness"))


ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Componente TC WV2",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Green" = "Greeness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness"))


dfLAND8_N4 <- melt(Medias_LAND8TC_Nivel4)  #the function melt reshapes it from wide to long
dfLAND8_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Componente TC L8",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Green" = "Greeness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness")) + ylim(-0.2,0.3)


ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Componente TC L8",y = "Valores") +
  scale_x_discrete(labels=c("Mean_Green" = "Greeness", "Mean_Bright" = "Brightness","Mean_Wet" = "Wetness"))+ ylim(-0.2,0.3)


#plots dos pontos médios das distribuicoes (escolher as bandas)

ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(x=Medias_WV2TC_Nivel4$Mean_Bright, y=Medias_WV2TC_Nivel4$Mean_Green, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "WV2 Bright",y = "WV2 Greeness")

ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(y=Medias_WV2TC_Nivel4$Mean_Green, x=Medias_WV2TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(y = "WV2 Greeness",x = "WV2 Wetness")

ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(x=Medias_WV2TC_Nivel4$Mean_Bright, y=Medias_WV2TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "WV2 Brightness",y = "WV2 Wetness")


###LAND8

ggplot(Medias_LAND8TC_Nivel4) + 
  geom_point(aes(y=Medias_LAND8TC_Nivel4$Mean_Green, x=Medias_LAND8TC_Nivel4$Mean_Bright, 
                 shape=factor(Medias_LAND8TC_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(y = "LAND8 Greeness",x = "LAND8 Bright")

ggplot(Medias_LAND8TC_Nivel4) + 
  geom_point(aes(y=Medias_LAND8TC_Nivel4$Mean_Green, x=Medias_LAND8TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_LAND8TC_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(y = "LAND8 Greeness",x = "LAND8 Wetness")

ggplot(Medias_LAND8TC_Nivel4) + 
  geom_point(aes(x=Medias_LAND8TC_Nivel4$Mean_Bright, y=Medias_LAND8TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_LAND8TC_Nivel4$Nivel4), 
                 colour=factor(Medias_LAND8TC_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "LAND8 Brightness",y = "LAND8 Wetness")

#Aquisicao dos dados do dataframe para calcular a distancia banda x banda

#bandas do WV2

Green_CL_WV2=dfWV2_N4[1,3]
Green_CLU_WV2=dfWV2_N4[2,3]
Green_CLUCM_WV2=dfWV2_N4[3,3]
Green_CRUP_WV2=dfWV2_N4[4,3]
Green_CS_WV2=dfWV2_N4[5,3]
Green_CD_WV2=dfWV2_N4[6,3]
Green_CR_WV2=dfWV2_N4[7,3]
Green_CT_WV2=dfWV2_N4[8,3]
Green_MG_WV2=dfWV2_N4[9,3]
Green_VE_WV2=dfWV2_N4[10,3]

Bright_CL_WV2=dfWV2_N4[11,3]
Bright_CLU_WV2=dfWV2_N4[12,3]
Bright_CLUCM_WV2=dfWV2_N4[13,3]
Bright_CRUP_WV2=dfWV2_N4[14,3]
Bright_CS_WV2=dfWV2_N4[15,3]
Bright_CD_WV2=dfWV2_N4[16,3]
Bright_CR_WV2=dfWV2_N4[17,3]
Bright_CT_WV2=dfWV2_N4[18,3]
Bright_MG_WV2=dfWV2_N4[19,3]
Bright_VE_WV2=dfWV2_N4[20,3]

Wet_CL_WV2=dfWV2_N4[21,3]
Wet_CLU_WV2=dfWV2_N4[22,3]
Wet_CLUCM_WV2=dfWV2_N4[23,3]
Wet_CRUP_WV2=dfWV2_N4[24,3]
Wet_CS_WV2=dfWV2_N4[25,3]
Wet_CD_WV2=dfWV2_N4[26,3]
Wet_CR_WV2=dfWV2_N4[27,3]
Wet_CT_WV2=dfWV2_N4[28,3]
Wet_MG_WV2=dfWV2_N4[29,3]
Wet_VE_WV2=dfWV2_N4[30,3]


#bandas do landsat8


Green_CL_LAND8=dfLAND8_N4[1,3]
Green_CLU_LAND8=dfLAND8_N4[2,3]
Green_CLUCM_LAND8=dfLAND8_N4[3,3]
Green_CRUP_LAND8=dfLAND8_N4[4,3]
Green_CS_LAND8=dfLAND8_N4[5,3]
Green_CD_LAND8=dfLAND8_N4[6,3]
Green_CR_LAND8=dfLAND8_N4[7,3]
Green_CT_LAND8=dfLAND8_N4[8,3]
Green_MG_LAND8=dfLAND8_N4[9,3]
Green_VE_LAND8=dfLAND8_N4[10,3]

Bright_CL_LAND8=dfLAND8_N4[11,3]
Bright_CLU_LAND8=dfLAND8_N4[12,3]
Bright_CLUCM_LAND8=dfLAND8_N4[13,3]
Bright_CRUP_LAND8=dfLAND8_N4[14,3]
Bright_CS_LAND8=dfLAND8_N4[15,3]
Bright_CD_LAND8=dfLAND8_N4[16,3]
Bright_CR_LAND8=dfLAND8_N4[17,3]
Bright_CT_LAND8=dfLAND8_N4[18,3]
Bright_MG_LAND8=dfLAND8_N4[19,3]
Bright_VE_LAND8=dfLAND8_N4[20,3]

Wet_CL_LAND8=dfLAND8_N4[21,3]
Wet_CLU_LAND8=dfLAND8_N4[22,3]
Wet_CLUCM_LAND8=dfLAND8_N4[23,3]
Wet_CRUP_LAND8=dfLAND8_N4[24,3]
Wet_CS_LAND8=dfLAND8_N4[25,3]
Wet_CD_LAND8=dfLAND8_N4[26,3]
Wet_CR_LAND8=dfLAND8_N4[27,3]
Wet_CT_LAND8=dfLAND8_N4[28,3]
Wet_MG_LAND8=dfLAND8_N4[29,3]
Wet_VE_LAND8=dfLAND8_N4[30,3]


#distancia euclidiana dos pontos médios 

#distancia euclidiana dos pontos médios 
#Green X Bright

Matriz_distancias_WV2_N4_GreenxBright <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_GreenxBright) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_GreenxBright) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_GreenxBright <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_GreenxBright) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_GreenxBright) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_GreenxBright = sqrt(((Green_CLUCM_WV2-Green_CLU_WV2)^2) +((Bright_CLUCM_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[2,1] <- distCLU_CLUCM_WV2_GreenxBright

distCLU_CRUP_WV2_GreenxBright = sqrt(((Green_CRUP_WV2-Green_CLU_WV2)^2) +((Bright_CRUP_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[3,1] <- distCLU_CRUP_WV2_GreenxBright

distCLU_CL_WV2_GreenxBright = sqrt(((Green_CL_WV2-Green_CLU_WV2)^2) +((Bright_CL_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[4,1] <- distCLU_CL_WV2_GreenxBright

distCLU_CS_WV2_GreenxBright = sqrt(((Green_CS_WV2-Green_CLU_WV2)^2) +((Bright_CS_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[5,1] <- distCLU_CS_WV2_GreenxBright

distCLU_CR_WV2_GreenxBright = sqrt(((Green_CR_WV2-Green_CLU_WV2)^2) +((Bright_CR_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[6,1] <- distCLU_CR_WV2_GreenxBright

distCLU_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CLU_WV2)^2) +((Bright_CT_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,1] <- distCLU_CT_WV2_GreenxBright

distCLU_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CLU_WV2)^2) +((Bright_CD_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,1] <- distCLU_CD_WV2_GreenxBright

distCLU_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CLU_WV2)^2) +((Bright_VE_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,1] <- distCLU_VE_WV2_GreenxBright

distCLU_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CLU_WV2)^2) +((Bright_MG_WV2-Bright_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,1] <- distCLU_MG_WV2_GreenxBright


#
distCLUCM_CRUP_WV2_GreenxBright = sqrt(((Green_CRUP_WV2-Green_CLUCM_WV2)^2) +((Bright_CRUP_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[3,2] <- distCLUCM_CRUP_WV2_GreenxBright

distCLUCM_CL_WV2_GreenxBright = sqrt(((Green_CL_WV2-Green_CLUCM_WV2)^2) +((Bright_CL_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[4,2] <- distCLUCM_CL_WV2_GreenxBright

distCLUCM_CS_WV2_GreenxBright = sqrt(((Green_CS_WV2-Green_CLUCM_WV2)^2) +((Bright_CS_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[5,2] <- distCLUCM_CS_WV2_GreenxBright

distCLUCM_CR_WV2_GreenxBright = sqrt(((Green_CR_WV2-Green_CLUCM_WV2)^2) +((Bright_CR_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[6,2] <- distCLUCM_CR_WV2_GreenxBright

distCLUCM_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CLUCM_WV2)^2) +((Bright_CT_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,2] <- distCLUCM_CT_WV2_GreenxBright

distCLUCM_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CLUCM_WV2)^2) +((Bright_CD_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,2] <- distCLUCM_CD_WV2_GreenxBright

distCLUCM_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CLUCM_WV2)^2) +((Bright_VE_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,2] <- distCLUCM_VE_WV2_GreenxBright

distCLUCM_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CLUCM_WV2)^2) +((Bright_MG_WV2-Bright_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,2] <- distCLUCM_MG_WV2_GreenxBright


#
distCRUP_CL_WV2_GreenxBright = sqrt(((Green_CL_WV2-Green_CRUP_WV2)^2) +((Bright_CL_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[4,3] <- distCRUP_CL_WV2_GreenxBright

distCRUP_CS_WV2_GreenxBright = sqrt(((Green_CS_WV2-Green_CRUP_WV2)^2) +((Bright_CS_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[5,3] <- distCRUP_CS_WV2_GreenxBright

distCRUP_CR_WV2_GreenxBright = sqrt(((Green_CR_WV2-Green_CRUP_WV2)^2) +((Bright_CR_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[6,3] <- distCRUP_CR_WV2_GreenxBright

distCRUP_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CRUP_WV2)^2) +((Bright_CT_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,3] <- distCRUP_CT_WV2_GreenxBright

distCRUP_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CRUP_WV2)^2) +((Bright_CD_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,3] <- distCRUP_CD_WV2_GreenxBright

distCRUP_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CRUP_WV2)^2) +((Bright_VE_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,3] <- distCRUP_VE_WV2_GreenxBright

distCRUP_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CRUP_WV2)^2) +((Bright_MG_WV2-Bright_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,3] <- distCRUP_MG_WV2_GreenxBright


#
distCL_CS_WV2_GreenxBright = sqrt(((Green_CS_WV2-Green_CL_WV2)^2) +((Bright_CS_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[5,4] <- distCL_CS_WV2_GreenxBright

distCL_CR_WV2_GreenxBright = sqrt(((Green_CR_WV2-Green_CL_WV2)^2) +((Bright_CR_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[6,4] <- distCL_CR_WV2_GreenxBright

distCL_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CL_WV2)^2) +((Bright_CT_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,4] <- distCL_CT_WV2_GreenxBright

distCL_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CL_WV2)^2) +((Bright_CD_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,4] <- distCL_CD_WV2_GreenxBright

distCL_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CL_WV2)^2) +((Bright_VE_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,4] <- distCL_VE_WV2_GreenxBright

distCL_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CL_WV2)^2) +((Bright_MG_WV2-Bright_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,4] <- distCL_MG_WV2_GreenxBright


#
distCS_CR_WV2_GreenxBright = sqrt(((Green_CR_WV2-Green_CS_WV2)^2) +((Bright_CR_WV2-Bright_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[6,5] <- distCS_CR_WV2_GreenxBright

distCS_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CS_WV2)^2) +((Bright_CT_WV2-Bright_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,5] <- distCS_CT_WV2_GreenxBright

distCS_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CS_WV2)^2) +((Bright_CD_WV2-Bright_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,5] <- distCS_CD_WV2_GreenxBright

distCS_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CS_WV2)^2) +((Bright_VE_WV2-Bright_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,5] <- distCS_VE_WV2_GreenxBright

distCS_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CS_WV2)^2) +((Bright_MG_WV2-Bright_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,5] <- distCS_MG_WV2_GreenxBright


#
distCR_CT_WV2_GreenxBright = sqrt(((Green_CT_WV2-Green_CR_WV2)^2) +((Bright_CT_WV2-Bright_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[7,6] <- distCR_CT_WV2_GreenxBright

distCR_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CR_WV2)^2) +((Bright_CD_WV2-Bright_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,6] <- distCR_CD_WV2_GreenxBright

distCR_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CR_WV2)^2) +((Bright_VE_WV2-Bright_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,6] <- distCR_VE_WV2_GreenxBright

distCR_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CR_WV2)^2) +((Bright_MG_WV2-Bright_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,6] <- distCR_MG_WV2_GreenxBright


#
distCT_CD_WV2_GreenxBright = sqrt(((Green_CD_WV2-Green_CT_WV2)^2) +((Bright_CD_WV2-Bright_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[8,7] <- distCT_CD_WV2_GreenxBright

distCT_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CT_WV2)^2) +((Bright_VE_WV2-Bright_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,7] <- distCT_VE_WV2_GreenxBright

distCT_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CT_WV2)^2) +((Bright_MG_WV2-Bright_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,7] <- distCT_MG_WV2_GreenxBright

#
distCD_VE_WV2_GreenxBright = sqrt(((Green_VE_WV2-Green_CD_WV2)^2) +((Bright_VE_WV2-Bright_CD_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[9,8] <- distCD_VE_WV2_GreenxBright

distCD_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_CD_WV2)^2) +((Bright_MG_WV2-Bright_CD_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,8] <- distCD_MG_WV2_GreenxBright

#
distVE_MG_WV2_GreenxBright = sqrt(((Green_MG_WV2-Green_VE_WV2)^2) +((Bright_MG_WV2-Bright_VE_WV2)^2))
Matriz_distancias_WV2_N4_GreenxBright[10,9] <- distVE_MG_WV2_GreenxBright


#LAND8

distCLU_CLUCM_LAND8_GreenxBright = sqrt(((Green_CLUCM_LAND8-Green_CLU_LAND8)^2) +((Bright_CLUCM_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[2,1] <- distCLU_CLUCM_LAND8_GreenxBright

distCLU_CRUP_LAND8_GreenxBright = sqrt(((Green_CRUP_LAND8-Green_CLU_LAND8)^2) +((Bright_CRUP_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[3,1] <- distCLU_CRUP_LAND8_GreenxBright

distCLU_CL_LAND8_GreenxBright = sqrt(((Green_CL_LAND8-Green_CLU_LAND8)^2) +((Bright_CL_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[4,1] <- distCLU_CL_LAND8_GreenxBright

distCLU_CS_LAND8_GreenxBright = sqrt(((Green_CS_LAND8-Green_CLU_LAND8)^2) +((Bright_CS_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[5,1] <- distCLU_CS_LAND8_GreenxBright

distCLU_CR_LAND8_GreenxBright = sqrt(((Green_CR_LAND8-Green_CLU_LAND8)^2) +((Bright_CR_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[6,1] <- distCLU_CR_LAND8_GreenxBright

distCLU_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CLU_LAND8)^2) +((Bright_CT_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,1] <- distCLU_CT_LAND8_GreenxBright

distCLU_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CLU_LAND8)^2) +((Bright_CD_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,1] <- distCLU_CD_LAND8_GreenxBright

distCLU_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CLU_LAND8)^2) +((Bright_VE_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,1] <- distCLU_VE_LAND8_GreenxBright

distCLU_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CLU_LAND8)^2) +((Bright_MG_LAND8-Bright_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,1] <- distCLU_MG_LAND8_GreenxBright


#
distCLUCM_CRUP_LAND8_GreenxBright = sqrt(((Green_CRUP_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CRUP_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[3,2] <- distCLUCM_CRUP_LAND8_GreenxBright

distCLUCM_CL_LAND8_GreenxBright = sqrt(((Green_CL_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CL_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[4,2] <- distCLUCM_CL_LAND8_GreenxBright

distCLUCM_CS_LAND8_GreenxBright = sqrt(((Green_CS_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CS_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[5,2] <- distCLUCM_CS_LAND8_GreenxBright

distCLUCM_CR_LAND8_GreenxBright = sqrt(((Green_CR_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CR_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[6,2] <- distCLUCM_CR_LAND8_GreenxBright

distCLUCM_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CT_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,2] <- distCLUCM_CT_LAND8_GreenxBright

distCLUCM_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CLUCM_LAND8)^2) +((Bright_CD_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,2] <- distCLUCM_CD_LAND8_GreenxBright

distCLUCM_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CLUCM_LAND8)^2) +((Bright_VE_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,2] <- distCLUCM_VE_LAND8_GreenxBright

distCLUCM_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CLUCM_LAND8)^2) +((Bright_MG_LAND8-Bright_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,2] <- distCLUCM_MG_LAND8_GreenxBright


#
distCRUP_CL_LAND8_GreenxBright = sqrt(((Green_CL_LAND8-Green_CRUP_LAND8)^2) +((Bright_CL_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[4,3] <- distCRUP_CL_LAND8_GreenxBright

distCRUP_CS_LAND8_GreenxBright = sqrt(((Green_CS_LAND8-Green_CRUP_LAND8)^2) +((Bright_CS_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[5,3] <- distCRUP_CS_LAND8_GreenxBright

distCRUP_CR_LAND8_GreenxBright = sqrt(((Green_CR_LAND8-Green_CRUP_LAND8)^2) +((Bright_CR_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[6,3] <- distCRUP_CR_LAND8_GreenxBright

distCRUP_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CRUP_LAND8)^2) +((Bright_CT_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,3] <- distCRUP_CT_LAND8_GreenxBright

distCRUP_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CRUP_LAND8)^2) +((Bright_CD_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,3] <- distCRUP_CD_LAND8_GreenxBright

distCRUP_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CRUP_LAND8)^2) +((Bright_VE_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,3] <- distCRUP_VE_LAND8_GreenxBright

distCRUP_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CRUP_LAND8)^2) +((Bright_MG_LAND8-Bright_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,3] <- distCRUP_MG_LAND8_GreenxBright


#
distCL_CS_LAND8_GreenxBright = sqrt(((Green_CS_LAND8-Green_CL_LAND8)^2) +((Bright_CS_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[5,4] <- distCL_CS_LAND8_GreenxBright

distCL_CR_LAND8_GreenxBright = sqrt(((Green_CR_LAND8-Green_CL_LAND8)^2) +((Bright_CR_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[6,4] <- distCL_CR_LAND8_GreenxBright

distCL_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CL_LAND8)^2) +((Bright_CT_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,4] <- distCL_CT_LAND8_GreenxBright

distCL_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CL_LAND8)^2) +((Bright_CD_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,4] <- distCL_CD_LAND8_GreenxBright

distCL_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CL_LAND8)^2) +((Bright_VE_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,4] <- distCL_VE_LAND8_GreenxBright

distCL_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CL_LAND8)^2) +((Bright_MG_LAND8-Bright_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,4] <- distCL_MG_LAND8_GreenxBright


#
distCS_CR_LAND8_GreenxBright = sqrt(((Green_CR_LAND8-Green_CS_LAND8)^2) +((Bright_CR_LAND8-Bright_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[6,5] <- distCS_CR_LAND8_GreenxBright

distCS_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CS_LAND8)^2) +((Bright_CT_LAND8-Bright_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,5] <- distCS_CT_LAND8_GreenxBright

distCS_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CS_LAND8)^2) +((Bright_CD_LAND8-Bright_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,5] <- distCS_CD_LAND8_GreenxBright

distCS_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CS_LAND8)^2) +((Bright_VE_LAND8-Bright_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,5] <- distCS_VE_LAND8_GreenxBright

distCS_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CS_LAND8)^2) +((Bright_MG_LAND8-Bright_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,5] <- distCS_MG_LAND8_GreenxBright


#
distCR_CT_LAND8_GreenxBright = sqrt(((Green_CT_LAND8-Green_CR_LAND8)^2) +((Bright_CT_LAND8-Bright_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[7,6] <- distCR_CT_LAND8_GreenxBright

distCR_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CR_LAND8)^2) +((Bright_CD_LAND8-Bright_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,6] <- distCR_CD_LAND8_GreenxBright

distCR_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CR_LAND8)^2) +((Bright_VE_LAND8-Bright_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,6] <- distCR_VE_LAND8_GreenxBright

distCR_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CR_LAND8)^2) +((Bright_MG_LAND8-Bright_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,6] <- distCR_MG_LAND8_GreenxBright


#
distCT_CD_LAND8_GreenxBright = sqrt(((Green_CD_LAND8-Green_CT_LAND8)^2) +((Bright_CD_LAND8-Bright_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[8,7] <- distCT_CD_LAND8_GreenxBright

distCT_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CT_LAND8)^2) +((Bright_VE_LAND8-Bright_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,7] <- distCT_VE_LAND8_GreenxBright

distCT_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CT_LAND8)^2) +((Bright_MG_LAND8-Bright_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,7] <- distCT_MG_LAND8_GreenxBright

#
distCD_VE_LAND8_GreenxBright = sqrt(((Green_VE_LAND8-Green_CD_LAND8)^2) +((Bright_VE_LAND8-Bright_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[9,8] <- distCD_VE_LAND8_GreenxBright

distCD_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_CD_LAND8)^2) +((Bright_MG_LAND8-Bright_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,8] <- distCD_MG_LAND8_GreenxBright

#
distVE_MG_LAND8_GreenxBright = sqrt(((Green_MG_LAND8-Green_VE_LAND8)^2) +((Bright_MG_LAND8-Bright_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxBright[10,9] <- distVE_MG_LAND8_GreenxBright



#teste da matriz resultante

Matriz_result_GreenxBright = Matriz_distancias_WV2_N4_GreenxBright - Matriz_distancias_LAND8_N4_GreenxBright

Matriz_result_GreenxBright <- Matriz_result_GreenxBright*10

melted_matriz_GreenxBright <- melt(Matriz_result_GreenxBright)
head(Matriz_result_GreenxBright)

ggplot(data = melted_matriz_GreenxBright, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-1.6,1.0)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS

ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(y=Medias_WV2TC_Nivel4$Mean_Green, x=Medias_WV2TC_Nivel4$Mean_Bright, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  geom_point(aes(y=Medias_LAND8TC_Nivel4$Mean_Green, x=Medias_LAND8TC_Nivel4$Mean_Bright, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(y = "Greeness",x = "Brghtness") +
  ylim(0.06,0.32) +xlim(0.06,0.32)


#distancia euclidiana dos pontos médios 
#Green X Wet

Matriz_distancias_WV2_N4_GreenxWet <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_GreenxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_GreenxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_GreenxWet <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_GreenxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_GreenxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_GreenxWet = sqrt(((Green_CLUCM_WV2-Green_CLU_WV2)^2) +((Wet_CLUCM_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[2,1] <- distCLU_CLUCM_WV2_GreenxWet

distCLU_CRUP_WV2_GreenxWet = sqrt(((Green_CRUP_WV2-Green_CLU_WV2)^2) +((Wet_CRUP_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[3,1] <- distCLU_CRUP_WV2_GreenxWet

distCLU_CL_WV2_GreenxWet = sqrt(((Green_CL_WV2-Green_CLU_WV2)^2) +((Wet_CL_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[4,1] <- distCLU_CL_WV2_GreenxWet

distCLU_CS_WV2_GreenxWet = sqrt(((Green_CS_WV2-Green_CLU_WV2)^2) +((Wet_CS_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[5,1] <- distCLU_CS_WV2_GreenxWet

distCLU_CR_WV2_GreenxWet = sqrt(((Green_CR_WV2-Green_CLU_WV2)^2) +((Wet_CR_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[6,1] <- distCLU_CR_WV2_GreenxWet

distCLU_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CLU_WV2)^2) +((Wet_CT_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,1] <- distCLU_CT_WV2_GreenxWet

distCLU_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CLU_WV2)^2) +((Wet_CD_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,1] <- distCLU_CD_WV2_GreenxWet

distCLU_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CLU_WV2)^2) +((Wet_VE_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,1] <- distCLU_VE_WV2_GreenxWet

distCLU_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CLU_WV2)^2) +((Wet_MG_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,1] <- distCLU_MG_WV2_GreenxWet


#
distCLUCM_CRUP_WV2_GreenxWet = sqrt(((Green_CRUP_WV2-Green_CLUCM_WV2)^2) +((Wet_CRUP_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[3,2] <- distCLUCM_CRUP_WV2_GreenxWet

distCLUCM_CL_WV2_GreenxWet = sqrt(((Green_CL_WV2-Green_CLUCM_WV2)^2) +((Wet_CL_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[4,2] <- distCLUCM_CL_WV2_GreenxWet

distCLUCM_CS_WV2_GreenxWet = sqrt(((Green_CS_WV2-Green_CLUCM_WV2)^2) +((Wet_CS_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[5,2] <- distCLUCM_CS_WV2_GreenxWet

distCLUCM_CR_WV2_GreenxWet = sqrt(((Green_CR_WV2-Green_CLUCM_WV2)^2) +((Wet_CR_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[6,2] <- distCLUCM_CR_WV2_GreenxWet

distCLUCM_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CLUCM_WV2)^2) +((Wet_CT_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,2] <- distCLUCM_CT_WV2_GreenxWet

distCLUCM_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CLUCM_WV2)^2) +((Wet_CD_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,2] <- distCLUCM_CD_WV2_GreenxWet

distCLUCM_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CLUCM_WV2)^2) +((Wet_VE_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,2] <- distCLUCM_VE_WV2_GreenxWet

distCLUCM_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CLUCM_WV2)^2) +((Wet_MG_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,2] <- distCLUCM_MG_WV2_GreenxWet


#
distCRUP_CL_WV2_GreenxWet = sqrt(((Green_CL_WV2-Green_CRUP_WV2)^2) +((Wet_CL_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[4,3] <- distCRUP_CL_WV2_GreenxWet

distCRUP_CS_WV2_GreenxWet = sqrt(((Green_CS_WV2-Green_CRUP_WV2)^2) +((Wet_CS_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[5,3] <- distCRUP_CS_WV2_GreenxWet

distCRUP_CR_WV2_GreenxWet = sqrt(((Green_CR_WV2-Green_CRUP_WV2)^2) +((Wet_CR_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[6,3] <- distCRUP_CR_WV2_GreenxWet

distCRUP_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CRUP_WV2)^2) +((Wet_CT_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,3] <- distCRUP_CT_WV2_GreenxWet

distCRUP_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CRUP_WV2)^2) +((Wet_CD_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,3] <- distCRUP_CD_WV2_GreenxWet

distCRUP_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CRUP_WV2)^2) +((Wet_VE_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,3] <- distCRUP_VE_WV2_GreenxWet

distCRUP_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CRUP_WV2)^2) +((Wet_MG_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,3] <- distCRUP_MG_WV2_GreenxWet


#
distCL_CS_WV2_GreenxWet = sqrt(((Green_CS_WV2-Green_CL_WV2)^2) +((Wet_CS_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[5,4] <- distCL_CS_WV2_GreenxWet

distCL_CR_WV2_GreenxWet = sqrt(((Green_CR_WV2-Green_CL_WV2)^2) +((Wet_CR_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[6,4] <- distCL_CR_WV2_GreenxWet

distCL_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CL_WV2)^2) +((Wet_CT_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,4] <- distCL_CT_WV2_GreenxWet

distCL_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CL_WV2)^2) +((Wet_CD_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,4] <- distCL_CD_WV2_GreenxWet

distCL_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CL_WV2)^2) +((Wet_VE_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,4] <- distCL_VE_WV2_GreenxWet

distCL_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CL_WV2)^2) +((Wet_MG_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,4] <- distCL_MG_WV2_GreenxWet


#
distCS_CR_WV2_GreenxWet = sqrt(((Green_CR_WV2-Green_CS_WV2)^2) +((Wet_CR_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[6,5] <- distCS_CR_WV2_GreenxWet

distCS_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CS_WV2)^2) +((Wet_CT_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,5] <- distCS_CT_WV2_GreenxWet

distCS_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CS_WV2)^2) +((Wet_CD_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,5] <- distCS_CD_WV2_GreenxWet

distCS_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CS_WV2)^2) +((Wet_VE_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,5] <- distCS_VE_WV2_GreenxWet

distCS_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CS_WV2)^2) +((Wet_MG_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,5] <- distCS_MG_WV2_GreenxWet


#
distCR_CT_WV2_GreenxWet = sqrt(((Green_CT_WV2-Green_CR_WV2)^2) +((Wet_CT_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[7,6] <- distCR_CT_WV2_GreenxWet

distCR_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CR_WV2)^2) +((Wet_CD_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,6] <- distCR_CD_WV2_GreenxWet

distCR_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CR_WV2)^2) +((Wet_VE_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,6] <- distCR_VE_WV2_GreenxWet

distCR_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CR_WV2)^2) +((Wet_MG_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,6] <- distCR_MG_WV2_GreenxWet


#
distCT_CD_WV2_GreenxWet = sqrt(((Green_CD_WV2-Green_CT_WV2)^2) +((Wet_CD_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[8,7] <- distCT_CD_WV2_GreenxWet

distCT_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CT_WV2)^2) +((Wet_VE_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,7] <- distCT_VE_WV2_GreenxWet

distCT_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CT_WV2)^2) +((Wet_MG_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,7] <- distCT_MG_WV2_GreenxWet

#
distCD_VE_WV2_GreenxWet = sqrt(((Green_VE_WV2-Green_CD_WV2)^2) +((Wet_VE_WV2-Wet_CD_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[9,8] <- distCD_VE_WV2_GreenxWet

distCD_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_CD_WV2)^2) +((Wet_MG_WV2-Wet_CD_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,8] <- distCD_MG_WV2_GreenxWet

#
distVE_MG_WV2_GreenxWet = sqrt(((Green_MG_WV2-Green_VE_WV2)^2) +((Wet_MG_WV2-Wet_VE_WV2)^2))
Matriz_distancias_WV2_N4_GreenxWet[10,9] <- distVE_MG_WV2_GreenxWet


#LAND8

distCLU_CLUCM_LAND8_GreenxWet = sqrt(((Green_CLUCM_LAND8-Green_CLU_LAND8)^2) +((Wet_CLUCM_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[2,1] <- distCLU_CLUCM_LAND8_GreenxWet

distCLU_CRUP_LAND8_GreenxWet = sqrt(((Green_CRUP_LAND8-Green_CLU_LAND8)^2) +((Wet_CRUP_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[3,1] <- distCLU_CRUP_LAND8_GreenxWet

distCLU_CL_LAND8_GreenxWet = sqrt(((Green_CL_LAND8-Green_CLU_LAND8)^2) +((Wet_CL_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[4,1] <- distCLU_CL_LAND8_GreenxWet

distCLU_CS_LAND8_GreenxWet = sqrt(((Green_CS_LAND8-Green_CLU_LAND8)^2) +((Wet_CS_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[5,1] <- distCLU_CS_LAND8_GreenxWet

distCLU_CR_LAND8_GreenxWet = sqrt(((Green_CR_LAND8-Green_CLU_LAND8)^2) +((Wet_CR_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[6,1] <- distCLU_CR_LAND8_GreenxWet

distCLU_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CLU_LAND8)^2) +((Wet_CT_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,1] <- distCLU_CT_LAND8_GreenxWet

distCLU_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CLU_LAND8)^2) +((Wet_CD_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,1] <- distCLU_CD_LAND8_GreenxWet

distCLU_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CLU_LAND8)^2) +((Wet_VE_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,1] <- distCLU_VE_LAND8_GreenxWet

distCLU_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CLU_LAND8)^2) +((Wet_MG_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,1] <- distCLU_MG_LAND8_GreenxWet


#
distCLUCM_CRUP_LAND8_GreenxWet = sqrt(((Green_CRUP_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CRUP_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[3,2] <- distCLUCM_CRUP_LAND8_GreenxWet

distCLUCM_CL_LAND8_GreenxWet = sqrt(((Green_CL_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CL_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[4,2] <- distCLUCM_CL_LAND8_GreenxWet

distCLUCM_CS_LAND8_GreenxWet = sqrt(((Green_CS_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CS_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[5,2] <- distCLUCM_CS_LAND8_GreenxWet

distCLUCM_CR_LAND8_GreenxWet = sqrt(((Green_CR_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CR_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[6,2] <- distCLUCM_CR_LAND8_GreenxWet

distCLUCM_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CT_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,2] <- distCLUCM_CT_LAND8_GreenxWet

distCLUCM_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CLUCM_LAND8)^2) +((Wet_CD_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,2] <- distCLUCM_CD_LAND8_GreenxWet

distCLUCM_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CLUCM_LAND8)^2) +((Wet_VE_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,2] <- distCLUCM_VE_LAND8_GreenxWet

distCLUCM_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CLUCM_LAND8)^2) +((Wet_MG_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,2] <- distCLUCM_MG_LAND8_GreenxWet


#
distCRUP_CL_LAND8_GreenxWet = sqrt(((Green_CL_LAND8-Green_CRUP_LAND8)^2) +((Wet_CL_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[4,3] <- distCRUP_CL_LAND8_GreenxWet

distCRUP_CS_LAND8_GreenxWet = sqrt(((Green_CS_LAND8-Green_CRUP_LAND8)^2) +((Wet_CS_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[5,3] <- distCRUP_CS_LAND8_GreenxWet

distCRUP_CR_LAND8_GreenxWet = sqrt(((Green_CR_LAND8-Green_CRUP_LAND8)^2) +((Wet_CR_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[6,3] <- distCRUP_CR_LAND8_GreenxWet

distCRUP_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CRUP_LAND8)^2) +((Wet_CT_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,3] <- distCRUP_CT_LAND8_GreenxWet

distCRUP_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CRUP_LAND8)^2) +((Wet_CD_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,3] <- distCRUP_CD_LAND8_GreenxWet

distCRUP_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CRUP_LAND8)^2) +((Wet_VE_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,3] <- distCRUP_VE_LAND8_GreenxWet

distCRUP_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CRUP_LAND8)^2) +((Wet_MG_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,3] <- distCRUP_MG_LAND8_GreenxWet


#
distCL_CS_LAND8_GreenxWet = sqrt(((Green_CS_LAND8-Green_CL_LAND8)^2) +((Wet_CS_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[5,4] <- distCL_CS_LAND8_GreenxWet

distCL_CR_LAND8_GreenxWet = sqrt(((Green_CR_LAND8-Green_CL_LAND8)^2) +((Wet_CR_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[6,4] <- distCL_CR_LAND8_GreenxWet

distCL_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CL_LAND8)^2) +((Wet_CT_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,4] <- distCL_CT_LAND8_GreenxWet

distCL_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CL_LAND8)^2) +((Wet_CD_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,4] <- distCL_CD_LAND8_GreenxWet

distCL_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CL_LAND8)^2) +((Wet_VE_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,4] <- distCL_VE_LAND8_GreenxWet

distCL_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CL_LAND8)^2) +((Wet_MG_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,4] <- distCL_MG_LAND8_GreenxWet


#
distCS_CR_LAND8_GreenxWet = sqrt(((Green_CR_LAND8-Green_CS_LAND8)^2) +((Wet_CR_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[6,5] <- distCS_CR_LAND8_GreenxWet

distCS_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CS_LAND8)^2) +((Wet_CT_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,5] <- distCS_CT_LAND8_GreenxWet

distCS_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CS_LAND8)^2) +((Wet_CD_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,5] <- distCS_CD_LAND8_GreenxWet

distCS_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CS_LAND8)^2) +((Wet_VE_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,5] <- distCS_VE_LAND8_GreenxWet

distCS_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CS_LAND8)^2) +((Wet_MG_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,5] <- distCS_MG_LAND8_GreenxWet


#
distCR_CT_LAND8_GreenxWet = sqrt(((Green_CT_LAND8-Green_CR_LAND8)^2) +((Wet_CT_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[7,6] <- distCR_CT_LAND8_GreenxWet

distCR_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CR_LAND8)^2) +((Wet_CD_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,6] <- distCR_CD_LAND8_GreenxWet

distCR_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CR_LAND8)^2) +((Wet_VE_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,6] <- distCR_VE_LAND8_GreenxWet

distCR_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CR_LAND8)^2) +((Wet_MG_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,6] <- distCR_MG_LAND8_GreenxWet


#
distCT_CD_LAND8_GreenxWet = sqrt(((Green_CD_LAND8-Green_CT_LAND8)^2) +((Wet_CD_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[8,7] <- distCT_CD_LAND8_GreenxWet

distCT_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CT_LAND8)^2) +((Wet_VE_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,7] <- distCT_VE_LAND8_GreenxWet

distCT_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CT_LAND8)^2) +((Wet_MG_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,7] <- distCT_MG_LAND8_GreenxWet

#
distCD_VE_LAND8_GreenxWet = sqrt(((Green_VE_LAND8-Green_CD_LAND8)^2) +((Wet_VE_LAND8-Wet_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[9,8] <- distCD_VE_LAND8_GreenxWet

distCD_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_CD_LAND8)^2) +((Wet_MG_LAND8-Wet_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,8] <- distCD_MG_LAND8_GreenxWet

#
distVE_MG_LAND8_GreenxWet = sqrt(((Green_MG_LAND8-Green_VE_LAND8)^2) +((Wet_MG_LAND8-Wet_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_GreenxWet[10,9] <- distVE_MG_LAND8_GreenxWet



#teste da matriz resultante

Matriz_result_GreenxWet = Matriz_distancias_WV2_N4_GreenxWet - Matriz_distancias_LAND8_N4_GreenxWet

Matriz_result_GreenxWet <- Matriz_result_GreenxWet*10

melted_matriz_GreenxWet <- melt(Matriz_result_GreenxWet)
head(Matriz_result_GreenxWet)

ggplot(data = melted_matriz_GreenxWet, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-1.6,1.0)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS


ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(y=Medias_WV2TC_Nivel4$Mean_Green, x=Medias_WV2TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  geom_point(aes(y=Medias_LAND8TC_Nivel4$Mean_Green, x=Medias_LAND8TC_Nivel4$Mean_Wet, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(y = "Greeness",x = "Wetness") +
  ylim(0.05, 0.35) +xlim(-0.30, 0.0)


#distancia euclidiana dos pontos médios 
#Bright X Wet

Matriz_distancias_WV2_N4_BrightxWet <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_BrightxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_BrightxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_BrightxWet <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_BrightxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_BrightxWet) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_BrightxWet = sqrt(((Bright_CLUCM_WV2-Bright_CLU_WV2)^2) +((Wet_CLUCM_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[2,1] <- distCLU_CLUCM_WV2_BrightxWet

distCLU_CRUP_WV2_BrightxWet = sqrt(((Bright_CRUP_WV2-Bright_CLU_WV2)^2) +((Wet_CRUP_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[3,1] <- distCLU_CRUP_WV2_BrightxWet

distCLU_CL_WV2_BrightxWet = sqrt(((Bright_CL_WV2-Bright_CLU_WV2)^2) +((Wet_CL_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[4,1] <- distCLU_CL_WV2_BrightxWet

distCLU_CS_WV2_BrightxWet = sqrt(((Bright_CS_WV2-Bright_CLU_WV2)^2) +((Wet_CS_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[5,1] <- distCLU_CS_WV2_BrightxWet

distCLU_CR_WV2_BrightxWet = sqrt(((Bright_CR_WV2-Bright_CLU_WV2)^2) +((Wet_CR_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[6,1] <- distCLU_CR_WV2_BrightxWet

distCLU_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CLU_WV2)^2) +((Wet_CT_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,1] <- distCLU_CT_WV2_BrightxWet

distCLU_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CLU_WV2)^2) +((Wet_CD_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,1] <- distCLU_CD_WV2_BrightxWet

distCLU_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CLU_WV2)^2) +((Wet_VE_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,1] <- distCLU_VE_WV2_BrightxWet

distCLU_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CLU_WV2)^2) +((Wet_MG_WV2-Wet_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,1] <- distCLU_MG_WV2_BrightxWet


#
distCLUCM_CRUP_WV2_BrightxWet = sqrt(((Bright_CRUP_WV2-Bright_CLUCM_WV2)^2) +((Wet_CRUP_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[3,2] <- distCLUCM_CRUP_WV2_BrightxWet

distCLUCM_CL_WV2_BrightxWet = sqrt(((Bright_CL_WV2-Bright_CLUCM_WV2)^2) +((Wet_CL_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[4,2] <- distCLUCM_CL_WV2_BrightxWet

distCLUCM_CS_WV2_BrightxWet = sqrt(((Bright_CS_WV2-Bright_CLUCM_WV2)^2) +((Wet_CS_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[5,2] <- distCLUCM_CS_WV2_BrightxWet

distCLUCM_CR_WV2_BrightxWet = sqrt(((Bright_CR_WV2-Bright_CLUCM_WV2)^2) +((Wet_CR_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[6,2] <- distCLUCM_CR_WV2_BrightxWet

distCLUCM_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CLUCM_WV2)^2) +((Wet_CT_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,2] <- distCLUCM_CT_WV2_BrightxWet

distCLUCM_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CLUCM_WV2)^2) +((Wet_CD_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,2] <- distCLUCM_CD_WV2_BrightxWet

distCLUCM_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CLUCM_WV2)^2) +((Wet_VE_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,2] <- distCLUCM_VE_WV2_BrightxWet

distCLUCM_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CLUCM_WV2)^2) +((Wet_MG_WV2-Wet_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,2] <- distCLUCM_MG_WV2_BrightxWet


#
distCRUP_CL_WV2_BrightxWet = sqrt(((Bright_CL_WV2-Bright_CRUP_WV2)^2) +((Wet_CL_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[4,3] <- distCRUP_CL_WV2_BrightxWet

distCRUP_CS_WV2_BrightxWet = sqrt(((Bright_CS_WV2-Bright_CRUP_WV2)^2) +((Wet_CS_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[5,3] <- distCRUP_CS_WV2_BrightxWet

distCRUP_CR_WV2_BrightxWet = sqrt(((Bright_CR_WV2-Bright_CRUP_WV2)^2) +((Wet_CR_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[6,3] <- distCRUP_CR_WV2_BrightxWet

distCRUP_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CRUP_WV2)^2) +((Wet_CT_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,3] <- distCRUP_CT_WV2_BrightxWet

distCRUP_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CRUP_WV2)^2) +((Wet_CD_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,3] <- distCRUP_CD_WV2_BrightxWet

distCRUP_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CRUP_WV2)^2) +((Wet_VE_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,3] <- distCRUP_VE_WV2_BrightxWet

distCRUP_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CRUP_WV2)^2) +((Wet_MG_WV2-Wet_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,3] <- distCRUP_MG_WV2_BrightxWet


#
distCL_CS_WV2_BrightxWet = sqrt(((Bright_CS_WV2-Bright_CL_WV2)^2) +((Wet_CS_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[5,4] <- distCL_CS_WV2_BrightxWet

distCL_CR_WV2_BrightxWet = sqrt(((Bright_CR_WV2-Bright_CL_WV2)^2) +((Wet_CR_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[6,4] <- distCL_CR_WV2_BrightxWet

distCL_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CL_WV2)^2) +((Wet_CT_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,4] <- distCL_CT_WV2_BrightxWet

distCL_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CL_WV2)^2) +((Wet_CD_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,4] <- distCL_CD_WV2_BrightxWet

distCL_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CL_WV2)^2) +((Wet_VE_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,4] <- distCL_VE_WV2_BrightxWet

distCL_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CL_WV2)^2) +((Wet_MG_WV2-Wet_CL_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,4] <- distCL_MG_WV2_BrightxWet


#
distCS_CR_WV2_BrightxWet = sqrt(((Bright_CR_WV2-Bright_CS_WV2)^2) +((Wet_CR_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[6,5] <- distCS_CR_WV2_BrightxWet

distCS_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CS_WV2)^2) +((Wet_CT_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,5] <- distCS_CT_WV2_BrightxWet

distCS_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CS_WV2)^2) +((Wet_CD_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,5] <- distCS_CD_WV2_BrightxWet

distCS_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CS_WV2)^2) +((Wet_VE_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,5] <- distCS_VE_WV2_BrightxWet

distCS_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CS_WV2)^2) +((Wet_MG_WV2-Wet_CS_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,5] <- distCS_MG_WV2_BrightxWet


#
distCR_CT_WV2_BrightxWet = sqrt(((Bright_CT_WV2-Bright_CR_WV2)^2) +((Wet_CT_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[7,6] <- distCR_CT_WV2_BrightxWet

distCR_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CR_WV2)^2) +((Wet_CD_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,6] <- distCR_CD_WV2_BrightxWet

distCR_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CR_WV2)^2) +((Wet_VE_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,6] <- distCR_VE_WV2_BrightxWet

distCR_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CR_WV2)^2) +((Wet_MG_WV2-Wet_CR_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,6] <- distCR_MG_WV2_BrightxWet


#
distCT_CD_WV2_BrightxWet = sqrt(((Bright_CD_WV2-Bright_CT_WV2)^2) +((Wet_CD_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[8,7] <- distCT_CD_WV2_BrightxWet

distCT_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CT_WV2)^2) +((Wet_VE_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,7] <- distCT_VE_WV2_BrightxWet

distCT_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CT_WV2)^2) +((Wet_MG_WV2-Wet_CT_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,7] <- distCT_MG_WV2_BrightxWet

#
distCD_VE_WV2_BrightxWet = sqrt(((Bright_VE_WV2-Bright_CD_WV2)^2) +((Wet_VE_WV2-Wet_CD_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[9,8] <- distCD_VE_WV2_BrightxWet

distCD_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_CD_WV2)^2) +((Wet_MG_WV2-Wet_CD_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,8] <- distCD_MG_WV2_BrightxWet

#
distVE_MG_WV2_BrightxWet = sqrt(((Bright_MG_WV2-Bright_VE_WV2)^2) +((Wet_MG_WV2-Wet_VE_WV2)^2))
Matriz_distancias_WV2_N4_BrightxWet[10,9] <- distVE_MG_WV2_BrightxWet


#LAND8

distCLU_CLUCM_LAND8_BrightxWet = sqrt(((Bright_CLUCM_LAND8-Bright_CLU_LAND8)^2) +((Wet_CLUCM_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[2,1] <- distCLU_CLUCM_LAND8_BrightxWet

distCLU_CRUP_LAND8_BrightxWet = sqrt(((Bright_CRUP_LAND8-Bright_CLU_LAND8)^2) +((Wet_CRUP_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[3,1] <- distCLU_CRUP_LAND8_BrightxWet

distCLU_CL_LAND8_BrightxWet = sqrt(((Bright_CL_LAND8-Bright_CLU_LAND8)^2) +((Wet_CL_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[4,1] <- distCLU_CL_LAND8_BrightxWet

distCLU_CS_LAND8_BrightxWet = sqrt(((Bright_CS_LAND8-Bright_CLU_LAND8)^2) +((Wet_CS_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[5,1] <- distCLU_CS_LAND8_BrightxWet

distCLU_CR_LAND8_BrightxWet = sqrt(((Bright_CR_LAND8-Bright_CLU_LAND8)^2) +((Wet_CR_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[6,1] <- distCLU_CR_LAND8_BrightxWet

distCLU_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CLU_LAND8)^2) +((Wet_CT_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,1] <- distCLU_CT_LAND8_BrightxWet

distCLU_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CLU_LAND8)^2) +((Wet_CD_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,1] <- distCLU_CD_LAND8_BrightxWet

distCLU_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CLU_LAND8)^2) +((Wet_VE_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,1] <- distCLU_VE_LAND8_BrightxWet

distCLU_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CLU_LAND8)^2) +((Wet_MG_LAND8-Wet_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,1] <- distCLU_MG_LAND8_BrightxWet


#
distCLUCM_CRUP_LAND8_BrightxWet = sqrt(((Bright_CRUP_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CRUP_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[3,2] <- distCLUCM_CRUP_LAND8_BrightxWet

distCLUCM_CL_LAND8_BrightxWet = sqrt(((Bright_CL_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CL_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[4,2] <- distCLUCM_CL_LAND8_BrightxWet

distCLUCM_CS_LAND8_BrightxWet = sqrt(((Bright_CS_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CS_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[5,2] <- distCLUCM_CS_LAND8_BrightxWet

distCLUCM_CR_LAND8_BrightxWet = sqrt(((Bright_CR_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CR_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[6,2] <- distCLUCM_CR_LAND8_BrightxWet

distCLUCM_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CT_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,2] <- distCLUCM_CT_LAND8_BrightxWet

distCLUCM_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_CD_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,2] <- distCLUCM_CD_LAND8_BrightxWet

distCLUCM_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_VE_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,2] <- distCLUCM_VE_LAND8_BrightxWet

distCLUCM_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CLUCM_LAND8)^2) +((Wet_MG_LAND8-Wet_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,2] <- distCLUCM_MG_LAND8_BrightxWet


#
distCRUP_CL_LAND8_BrightxWet = sqrt(((Bright_CL_LAND8-Bright_CRUP_LAND8)^2) +((Wet_CL_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[4,3] <- distCRUP_CL_LAND8_BrightxWet

distCRUP_CS_LAND8_BrightxWet = sqrt(((Bright_CS_LAND8-Bright_CRUP_LAND8)^2) +((Wet_CS_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[5,3] <- distCRUP_CS_LAND8_BrightxWet

distCRUP_CR_LAND8_BrightxWet = sqrt(((Bright_CR_LAND8-Bright_CRUP_LAND8)^2) +((Wet_CR_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[6,3] <- distCRUP_CR_LAND8_BrightxWet

distCRUP_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CRUP_LAND8)^2) +((Wet_CT_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,3] <- distCRUP_CT_LAND8_BrightxWet

distCRUP_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CRUP_LAND8)^2) +((Wet_CD_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,3] <- distCRUP_CD_LAND8_BrightxWet

distCRUP_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CRUP_LAND8)^2) +((Wet_VE_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,3] <- distCRUP_VE_LAND8_BrightxWet

distCRUP_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CRUP_LAND8)^2) +((Wet_MG_LAND8-Wet_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,3] <- distCRUP_MG_LAND8_BrightxWet


#
distCL_CS_LAND8_BrightxWet = sqrt(((Bright_CS_LAND8-Bright_CL_LAND8)^2) +((Wet_CS_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[5,4] <- distCL_CS_LAND8_BrightxWet

distCL_CR_LAND8_BrightxWet = sqrt(((Bright_CR_LAND8-Bright_CL_LAND8)^2) +((Wet_CR_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[6,4] <- distCL_CR_LAND8_BrightxWet

distCL_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CL_LAND8)^2) +((Wet_CT_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,4] <- distCL_CT_LAND8_BrightxWet

distCL_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CL_LAND8)^2) +((Wet_CD_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,4] <- distCL_CD_LAND8_BrightxWet

distCL_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CL_LAND8)^2) +((Wet_VE_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,4] <- distCL_VE_LAND8_BrightxWet

distCL_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CL_LAND8)^2) +((Wet_MG_LAND8-Wet_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,4] <- distCL_MG_LAND8_BrightxWet


#
distCS_CR_LAND8_BrightxWet = sqrt(((Bright_CR_LAND8-Bright_CS_LAND8)^2) +((Wet_CR_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[6,5] <- distCS_CR_LAND8_BrightxWet

distCS_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CS_LAND8)^2) +((Wet_CT_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,5] <- distCS_CT_LAND8_BrightxWet

distCS_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CS_LAND8)^2) +((Wet_CD_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,5] <- distCS_CD_LAND8_BrightxWet

distCS_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CS_LAND8)^2) +((Wet_VE_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,5] <- distCS_VE_LAND8_BrightxWet

distCS_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CS_LAND8)^2) +((Wet_MG_LAND8-Wet_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,5] <- distCS_MG_LAND8_BrightxWet


#
distCR_CT_LAND8_BrightxWet = sqrt(((Bright_CT_LAND8-Bright_CR_LAND8)^2) +((Wet_CT_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[7,6] <- distCR_CT_LAND8_BrightxWet

distCR_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CR_LAND8)^2) +((Wet_CD_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,6] <- distCR_CD_LAND8_BrightxWet

distCR_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CR_LAND8)^2) +((Wet_VE_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,6] <- distCR_VE_LAND8_BrightxWet

distCR_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CR_LAND8)^2) +((Wet_MG_LAND8-Wet_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,6] <- distCR_MG_LAND8_BrightxWet


#
distCT_CD_LAND8_BrightxWet = sqrt(((Bright_CD_LAND8-Bright_CT_LAND8)^2) +((Wet_CD_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[8,7] <- distCT_CD_LAND8_BrightxWet

distCT_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CT_LAND8)^2) +((Wet_VE_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,7] <- distCT_VE_LAND8_BrightxWet

distCT_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CT_LAND8)^2) +((Wet_MG_LAND8-Wet_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,7] <- distCT_MG_LAND8_BrightxWet

#
distCD_VE_LAND8_BrightxWet = sqrt(((Bright_VE_LAND8-Bright_CD_LAND8)^2) +((Wet_VE_LAND8-Wet_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[9,8] <- distCD_VE_LAND8_BrightxWet

distCD_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_CD_LAND8)^2) +((Wet_MG_LAND8-Wet_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,8] <- distCD_MG_LAND8_BrightxWet

#
distVE_MG_LAND8_BrightxWet = sqrt(((Bright_MG_LAND8-Bright_VE_LAND8)^2) +((Wet_MG_LAND8-Wet_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_BrightxWet[10,9] <- distVE_MG_LAND8_BrightxWet



#teste da matriz resultante

Matriz_result_BrightxWet = Matriz_distancias_WV2_N4_BrightxWet - Matriz_distancias_LAND8_N4_BrightxWet

Matriz_result_BrightxWet <- Matriz_result_BrightxWet*10

melted_matriz_BrightxWet <- melt(Matriz_result_BrightxWet)
head(Matriz_result_BrightxWet)

ggplot(data = melted_matriz_BrightxWet, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-1.6,1.0)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))


#Plots das dos pontos médios dos dois sensores

###ARRUMAR OS PCHS

ggplot(Medias_WV2TC_Nivel4) + 
  geom_point(aes(x=Medias_WV2TC_Nivel4$Mean_Bright, y=Medias_WV2TC_Nivel4$Mean_Wet, 
                 shape=factor(Medias_WV2TC_Nivel4$Nivel4), 
                 colour=factor(Medias_WV2TC_Nivel4$Nivel4)),size=4) +
  geom_point(aes(x=Medias_LAND8TC_Nivel4$Mean_Bright, y=Medias_LAND8TC_Nivel4$Mean_Wet, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=4) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Brightness",y = "Wetness") +
  xlim(0.05, 0.35) +ylim(-0.30, 0.0)
