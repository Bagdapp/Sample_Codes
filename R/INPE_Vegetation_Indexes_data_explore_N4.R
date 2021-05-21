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

WV2 <- read.csv(file = "DADOS_WV2_FINAL.csv", header = TRUE, sep = ";")
LAND8 <- read.csv(file = "Conjunto_Landsat8_final_csv.csv", header = TRUE, sep = ";")

# Geração dos conjuntos Espectrais Nível3

WV2IVS_Nivel4 = WV2[,c(14,15,16,17,18,19,138)]
LAND8IVS_Nivel4 = LAND8[,c(13,14,15,16,17,18,24)]

WV2IVS_Nivel3 = WV2[,c(14,15,16,17,18,19,137)]
LAND8IVS_Nivel3 = LAND8[,c(13,14,15,16,17,18,23)]

WV2IVS_Nivel2 = WV2[,c(14,15,16,17,18,19,136)]
LAND8IVS_Nivel2 = LAND8[,c(13,14,15,16,17,18,22)]

WV2IVS_Nivel1 = WV2[,c(14,15,16,17,18,19,134)]
LAND8IVS_Nivel1 = LAND8[,c(13,14,15,16,17,18,20)]


#correlação dos dados - É a mesma para todos os casos, independentemente da classe, 
#mas vou deixar tudo aqui para ter certeza

ggcorr(WV2IVS_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 5, label_round = 3, low = "steelblue", mid = "green", high = "steelblue") +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5))


ggcorr(LAND8IVS_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 5, label_round = 3, low = "steelblue", mid = "green", high = "steelblue") +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5))



#plot dos dados com ggplot especificos para cada combinacao banda a banda e cada combinação de classes
#####ribeiro e walter nivel 3
###
#

ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_NDVI, y=factor(WV2IVS_Nivel4$Nivel4, 
            c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
              "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
              "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
            colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
            "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
            "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
            "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 NDVI",y = "")



ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_EVI, y=factor(WV2IVS_Nivel4$Nivel4, 
                                                     c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                       "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                       "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                       "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                       "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                       "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 EVI",y = "")


ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_EVI2, y=factor(WV2IVS_Nivel4$Nivel4, 
                                                    c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                      "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                      "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                       "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                       "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                       "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 EVI2",y = "")


ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_MSAV2, y=factor(WV2IVS_Nivel4$Nivel4, 
                                                    c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                      "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                      "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                       "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                       "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                       "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 MSAVI2",y = "")

ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_SAVI, y=factor(WV2IVS_Nivel4$Nivel4, 
                                                    c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                      "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                      "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                       "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                       "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                       "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 SAVI",y = "")

ggplot(WV2IVS_Nivel4) +
  geom_point(aes(x=WV2IVS_Nivel4$Mean_VDVI, y=factor(WV2IVS_Nivel4$Nivel4, 
                                                     c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                       "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                       "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                       "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                       "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                       "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 VDVI",y = "")



ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_NDVI, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 NDVI",y = "")



ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_EVI, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                      c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                        "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                        "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 EVI",y = "")


ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_EVI2, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 EVI2",y = "")


ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_MSAVI2, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                        c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                          "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                          "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 MSAVI2",y = "")

ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_SAVI, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 SAVI",y = "")

ggplot(LAND8IVS_Nivel4) +
  geom_point(aes(x=LAND8IVS_Nivel4$Mean_VDVI, y=factor(LAND8IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 VDVI",y = "")

#NIVEL 3

ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_NDVI, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 NDVI",y = "")



ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_EVI, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                    c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 EVI",y = "")


ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_EVI2, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 EVI2",y = "")


ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_MSAV2, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 MSAVI2",y = "")

ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_SAVI, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 SAVI",y = "")

ggplot(WV2IVS_Nivel3) +
  geom_point(aes(x=WV2IVS_Nivel3$Mean_VDVI, y=factor(WV2IVS_Nivel3$Nivel3, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "WV2 VDVI",y = "")



ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_NDVI, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 NDVI",y = "")



ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_EVI, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 EVI",y = "")


ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_EVI2, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 EVI2",y = "")


ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_MSAVI2, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 MSAVI2",y = "")

ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_SAVI, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 SAVI",y = "")

ggplot(LAND8IVS_Nivel3) +
  geom_point(aes(x=LAND8IVS_Nivel3$Mean_VDVI, y=factor(LAND8IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "LAND8 VDVI",y = "")



#NIVEL2B

ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_NDVI, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 NDVI",y = "")



ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_EVI, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                    c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 EVI",y = "")


ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_EVI2, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 EVI2",y = "")


ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_MSAV2, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 MSAVI2",y = "")

ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_SAVI, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 SAVI",y = "")

ggplot(WV2IVS_Nivel2) +
  geom_point(aes(x=WV2IVS_Nivel2$Mean_VDVI, y=factor(WV2IVS_Nivel2$Nivel2B, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "WV2 VDVI",y = "")



ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_NDVI, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 NDVI",y = "")



ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_EVI, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 EVI",y = "")


ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_EVI2, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 EVI2",y = "")


ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_MSAVI2, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 MSAVI2",y = "")

ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_SAVI, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 SAVI",y = "")

ggplot(LAND8IVS_Nivel2) +
  geom_point(aes(x=LAND8IVS_Nivel2$Mean_VDVI, y=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "LAND8 VDVI",y = "")



#NIVEL1

ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_NDVI, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                     c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 NDVI",y = "")



ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_EVI, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                    c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 EVI",y = "")


ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_EVI2, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                     c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 EVI2",y = "")


ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_MSAV2, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 MSAVI2",y = "")

ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_SAVI, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                     c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 SAVI",y = "")

ggplot(WV2IVS_Nivel1) +
  geom_point(aes(x=WV2IVS_Nivel1$Mean_VDVI, y=factor(WV2IVS_Nivel1$Nivel1, 
                                                     c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "WV2 VDVI",y = "")



ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_NDVI, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                       c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 NDVI",y = "")



ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_EVI, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 EVI",y = "")


ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_EVI2, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                       c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 EVI2",y = "")


ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_MSAVI2, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                         c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 MSAVI2",y = "")

ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_SAVI, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                       c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 SAVI",y = "")

ggplot(LAND8IVS_Nivel1) +
  geom_point(aes(x=LAND8IVS_Nivel1$Mean_VDVI, y=factor(LAND8IVS_Nivel1$Nivel1, 
                                                       c("Campestre", "Savanica", "Florestal")),
                 colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal")))) +
  theme(legend.position="none") +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "LAND8 VDVI",y = "") 


#calculo das medias de cada atributo para cada classe


#####NIVEL4
Medias_WV2IVS_Nivel4<-ddply(WV2IVS_Nivel4, .(Nivel4), summarize, Mean_NDVI=mean(Mean_NDVI), 
                           Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                           Mean_MSAV2=mean(Mean_MSAV2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

Medias_LAND8IVS_Nivel4<-ddply(LAND8IVS_Nivel4, .(Nivel4), summarize, Mean_NDVI=mean(Mean_NDVI), 
                             Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                             Mean_MSAV2=mean(Mean_MSAVI2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

#####NIVEL3
Medias_WV2IVS_Nivel3<-ddply(WV2IVS_Nivel3, .(Nivel3), summarize, Mean_NDVI=mean(Mean_NDVI), 
                            Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                            Mean_MSAV2=mean(Mean_MSAV2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

Medias_LAND8IVS_Nivel3<-ddply(LAND8IVS_Nivel3, .(Nivel3), summarize, Mean_NDVI=mean(Mean_NDVI), 
                              Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                              Mean_MSAV2=mean(Mean_MSAVI2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))


#####NIVEL2
Medias_WV2IVS_Nivel2<-ddply(WV2IVS_Nivel2, .(Nivel2B), summarize, Mean_NDVI=mean(Mean_NDVI), 
                            Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                            Mean_MSAV2=mean(Mean_MSAV2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

Medias_LAND8IVS_Nivel2<-ddply(LAND8IVS_Nivel2, .(Nivel2B), summarize, Mean_NDVI=mean(Mean_NDVI), 
                              Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                              Mean_MSAV2=mean(Mean_MSAVI2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

#####NIVEL1
Medias_WV2IVS_Nivel1<-ddply(WV2IVS_Nivel1, .(Nivel1), summarize, Mean_NDVI=mean(Mean_NDVI), 
                            Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                            Mean_MSAV2=mean(Mean_MSAV2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))

Medias_LAND8IVS_Nivel1<-ddply(LAND8IVS_Nivel1, .(Nivel1), summarize, Mean_NDVI=mean(Mean_NDVI), 
                              Mean_EVI=mean(Mean_EVI), Mean_EVI2=mean(Mean_EVI2), 
                              Mean_MSAV2=mean(Mean_MSAVI2), Mean_SAVI=mean(Mean_SAVI), mean_VDVI=mean(Mean_VDVI))



#plots das medias das bandas - Comportamento espectral

####NIVEL4
###
#

dfWV2_N4 <- melt(Medias_WV2IVS_Nivel4)  #the function melt reshapes it from wide to long
dfWV2_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=2) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)



dfLAND8_N4 <- melt(Medias_LAND8IVS_Nivel4)  #the function melt reshapes it from wide to long
dfLAND8_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=0.75) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


#####
dfWV2_N3 <- melt(Medias_WV2IVS_Nivel3)  #the function melt reshapes it from wide to long
dfWV2_N3$rowid <- 1:8  #add a rowid identifying variable

ggplot(dfWV2_N3, aes(variable, value, group=factor(Nivel3))) + geom_point(aes(color=factor(Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfWV2_N3, aes(variable, value, group=factor(Nivel3))) + geom_line(aes(color=factor(Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=0.75) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)



dfLAND8_N3 <- melt(Medias_LAND8IVS_Nivel3)  #the function melt reshapes it from wide to long
dfLAND8_N3$rowid <- 1:8  #add a rowid identifying variable

ggplot(dfLAND8_N3, aes(variable, value, group=factor(Nivel3))) + geom_point(aes(color=factor(Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfLAND8_N3, aes(variable, value, group=factor(Nivel3))) + geom_line(aes(color=factor(Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=0.75) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


#####
dfWV2_N2 <- melt(Medias_WV2IVS_Nivel2)  #the function melt reshapes it from wide to long
dfWV2_N2$rowid <- 1:6  #add a rowid identifying variable

ggplot(dfWV2_N2, aes(variable, value, group=factor(Nivel2B))) + geom_point(aes(color=factor(Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfWV2_N2, aes(variable, value, group=factor(Nivel2B))) + geom_line(aes(color=factor(Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))), size=0.75) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)




dfLAND8_N2 <- melt(Medias_LAND8IVS_Nivel2)  #the function melt reshapes it from wide to long
dfLAND8_N2$rowid <- 1:6  #add a rowid identifying variable

ggplot(dfLAND8_N2, aes(variable, value, group=factor(Nivel2B))) + geom_point(aes(color=factor(Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


ggplot(dfLAND8_N2, aes(variable, value, group=factor(Nivel2B))) + geom_line(aes(color=factor(Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))), size=0.75) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.93,0.93)


#####
dfWV2_N1 <- melt(Medias_WV2IVS_Nivel1)  #the function melt reshapes it from wide to long
dfWV2_N1$rowid <- 1:3  #add a rowid identifying variable

ggplot(dfWV2_N1, aes(variable, value, group=factor(Nivel1))) + geom_point(aes(color=factor(Nivel1, c("Campestre", "Savanica", "Florestal"))), size=3) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI"))  +
  ylim(-0.93,0.93) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(dfWV2_N1, aes(variable, value, group=factor(Nivel1))) + geom_line(aes(color=factor(Nivel1, c("Campestre", "Savanica", "Florestal"))), size=0.75) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  ylim(-0.93,0.93) +
  labs(title="WorldView-2") +
  theme(plot.title = element_text(hjust = 0.5))



dfLAND8_N1 <- melt(Medias_LAND8IVS_Nivel1)  #the function melt reshapes it from wide to long
dfLAND8_N1$rowid <- 1:3  #add a rowid identifying variable

ggplot(dfLAND8_N1, aes(variable, value, group=factor(Nivel1))) + geom_point(aes(color=factor(Nivel1, c("Campestre", "Savanica", "Florestal"))), size=3) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  ylim(-0.93,0.93) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(dfLAND8_N1, aes(variable, value, group=factor(Nivel1))) + geom_line(aes(color=factor(Nivel1, c("Campestre", "Savanica", "Florestal"))), size=0.75) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) + theme(legend.title = element_blank()) + labs(x = "Índice de Vegetação",y = "Valor") +
  scale_x_discrete(labels=c("Mean_NDVI" = "NDVI", "Mean_EVI" = "EVI","Mean_EVI2" = "EVI2","Mean_MSAV2" = "MSAVI2","Mean_SAVI" = "SAVI","mean_VDVI" = "VDVI")) +
  ylim(-0.93,0.93) +
  labs(title="Landsat-8") +
  theme(plot.title = element_text(hjust = 0.5))




#BOXPLOT NIVEL 4

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_NDVI, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                         c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                           "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                           "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "NDVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_EVI, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                         c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                           "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                           "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_EVI2, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                        c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                          "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                          "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_MSAVI2, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                         c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                           "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                           "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+  
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_SAVI, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                          c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                            "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                            "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "SAVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel4) +
  geom_boxplot(aes(y=LAND8IVS_Nivel4$Mean_VDVI, x=factor(LAND8IVS_Nivel4$Nivel4, 
                                                         c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                           "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                           "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                           "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                           "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                           "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+  
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "VDVI Landsat-8",y = "Valor")




ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_NDVI, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "NDVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_EVI, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                       c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                         "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                         "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_EVI2, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                      c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                        "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                        "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_MSAV2, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                      c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                        "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                        "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_SAVI, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                      c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                        "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                        "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "SAVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel4) +
  geom_boxplot(aes(y=WV2IVS_Nivel4$Mean_VDVI, x=factor(WV2IVS_Nivel4$Nivel4, 
                                                      c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu",
                                                        "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", 
                                                        "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido",
                                                         "Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", 
                                                         "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", 
                                                         "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+  
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "VDVI WorldView-2",y = "Valor")



#BOXPLOT NIVEL 1

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_NDVI, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                     c("Campestre", "Savanica", "Florestal")),
                 colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "NDVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_EVI, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                       c("Campestre", "Savanica", "Florestal")),
                   colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "EVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_EVI2, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                   colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "EVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_MSAV2, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                   colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "MSAVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_MSAV, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                   colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "MSAVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel1) +
  geom_boxplot(aes(y=WV2IVS_Nivel1$Mean_VDVI, x=factor(WV2IVS_Nivel1$Nivel1, 
                                                      c("Campestre", "Savanica", "Florestal")),
                   colour=factor(WV2IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "VDVI WorldView-2",y = "Valor")







ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_NDVI, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                         c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "NDVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_EVI, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                        c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "EVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_EVI2, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                         c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "EVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_MSAVI2, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                          c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "MSAVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_MSAV, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                         c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "MSAVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel1) +
  geom_boxplot(aes(y=LAND8IVS_Nivel1$Mean_VDVI, x=factor(LAND8IVS_Nivel1$Nivel1, 
                                                         c("Campestre", "Savanica", "Florestal")),
                   colour=factor(LAND8IVS_Nivel1$Nivel1, c("Campestre", "Savanica", "Florestal"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF9933", "#339933", "#003300")) +
  labs(x = "VDVI Landsat-8",y = "Valor")

#BOXPLOT NIVEL 2

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_NDVI, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "NDVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_EVI, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "EVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_EVI2, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "EVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_MSAV2, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "MSAVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_MSAV, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                        c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "MSAVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel2) +
  geom_boxplot(aes(y=WV2IVS_Nivel2$Mean_VDVI, x=factor(WV2IVS_Nivel2$Nivel2B, 
                                                        c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "VDVI WorldView-2",y = "Valor")



ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_NDVI, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "NDVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_EVI, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                        c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "EVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_EVI2, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "EVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_MSAVI2, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                          c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "MSAVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_MSAV, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "MSAVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel2) +
  geom_boxplot(aes(y=LAND8IVS_Nivel2$Mean_VDVI, x=factor(LAND8IVS_Nivel2$Nivel2B, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel2$Nivel2B, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Strictu_Sensu", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF3333", "#FFFF00", "#FF9933", "#336600", "#33FFFF", "#003300")) +
  labs(x = "VDVI Landsat-8",y = "Valor")

#BOXPLOT NIVEL 3

ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_NDVI, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                     c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                 colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "NDVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_EVI, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_EVI2, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                      c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI2 WorldView-2",y = "Valor")


ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_MSAV2, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI2 WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_MSAV, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                        c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI WorldView-2",y = "Valor")

ggplot(WV2IVS_Nivel3) +
  geom_boxplot(aes(y=WV2IVS_Nivel3$Mean_VDVI, x=factor(WV2IVS_Nivel3$Nivel3, 
                                                       c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(WV2IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+  
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "VDVI WorldView-2",y = "Valor")







ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_NDVI, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.3,0.9),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "NDVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_EVI, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                        c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_EVI2, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "EVI2 Landsat-8",y = "Valor")


ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_MSAVI2, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                          c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI2 Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_MSAV, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(0.1,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6))+ 
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "MSAVI Landsat-8",y = "Valor")

ggplot(LAND8IVS_Nivel3) +
  geom_boxplot(aes(y=LAND8IVS_Nivel3$Mean_VDVI, x=factor(LAND8IVS_Nivel3$Nivel3, 
                                                         c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")),
                   colour=factor(LAND8IVS_Nivel3$Nivel3, c("Campo_rupestre", "Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))),outlier.size=1, width=0.4, outlier.colour = "black") + coord_flip()+ scale_y_continuous(limits=c(-0.7,-0.3),breaks=c(-0.7,-0.6,-0.5,-0.4,-0.3))+  
  theme(legend.position="none", axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
  scale_color_manual(values=c("#FF0000", "#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) +
  labs(x = "VDVI Landsat-8",y = "Valor")