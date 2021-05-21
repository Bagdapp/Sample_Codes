#Graficos de correlagco e avaliagco das distribuigues no R

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

# Geragco dos conjuntos Espectrais Nmvel3

WV2Spectral_Nivel4 = WV2[,c(1,2,3,4,5,6,7,138)]
LAND8Spectral_Nivel4 = LAND8[,c(1,2,3,4,5,6,24)]


#correlagco dos dados - I a mesma para todos os casos, independentemente da classe, 
#mas vou deixar tudo aqui para ter certeza

ggcorr(WV2Spectral_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 2)
ggcorr(LAND8Spectral_Nivel4,  method = c("all.obs", "pearson"), label = TRUE, label_size = 3, label_round = 3)

####BOXPLOTS


WV2_boxplot <- melt(WV2Spectral_Nivel4)
WV2_boxplot$Nivel4 <- factor(WV2_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(WV2_boxplot)


ggplot(WV2_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot()+facet_wrap(~Nivel4) +
  labs(x = "Bandas",y = "Reflectância") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green","Mean_b4" = "Yellow","Mean_b5" = "Red","Mean_b6" = "RE","Mean_b7" = "NIR1","Mean_b8" = "NIR2")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25))+ theme(text = element_text(size=20))




Landsat8_boxplot <- melt(LAND8Spectral_Nivel4)
Landsat8_boxplot$Nivel4 <- factor(Landsat8_boxplot$Nivel4, levels = c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))
str(Landsat8_boxplot)


ggplot(Landsat8_boxplot, aes(x=variable,y=value, fill=Nivel4))+geom_boxplot()+facet_wrap(~Nivel4) +
  labs(x = "Bandas",y = "Reflectância") +
  scale_fill_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600"), name = "Classes") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green", "Mean_b4" = "Red","Mean_b5" = "NIR","Mean_b6" = "SWIR1","Mean_b7" = "SWIR2")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25))+ theme(text = element_text(size=20))


#plot dos dados com ggplot especificos para cada combinacao banda a banda e cada combinagco de classes
#####ribeiro e walter nivel 3
###


ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b3, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 Green (510-580 nm)") 

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b4, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 Yellow (585-625 nm)") 

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b5, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 Red (630-690 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b6, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 Red-Edge (705-745 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b7, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 NIR1 (770-895 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b2, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Blue (450-510 nm)",y = "WV2 NIR2 (860-1040 nm)") 

# Comecando com banda Green


ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b3, 
                 y=WV2Spectral_Nivel4$Mean_b4, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Green (510-580 nm)",y = "WV2 Yellow (585-625 nm)") 

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b3, 
                 y=WV2Spectral_Nivel4$Mean_b5, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Green (510-580 nm)",y = "WV2 Red (630-690 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b3, 
                 y=WV2Spectral_Nivel4$Mean_b6, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Green (510-580 nm)",y = "WV2 Red-Edge (705-745 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b3, 
                 y=WV2Spectral_Nivel4$Mean_b7, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Green (510-580 nm)",y = "WV2 NIR1 (770-895 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b3, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Green (510-580 nm)",y = "WV2 NIR2 (860-1040 nm)") 


# Comecando com banda Yellow


ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b4, 
                 y=WV2Spectral_Nivel4$Mean_b5, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Yellow (585-625 nm)",y = "WV2 Red (630-690 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b4, 
                 y=WV2Spectral_Nivel4$Mean_b6, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Yellow (585-625 nm)",y = "WV2 Red-Edge (705-745 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b4, 
                 y=WV2Spectral_Nivel4$Mean_b7, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Yellow (585-625 nm)",y = "WV2 NIR1 (770-895 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b4, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Yellow (585-625 nm)",y = "WV2 NIR2 (860-1040 nm)") 

# Comecando com banda RED

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b5, 
                 y=WV2Spectral_Nivel4$Mean_b6, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Red (630-690 nm)",y = "WV2 Red-Edge (705-745 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b5, 
                 y=WV2Spectral_Nivel4$Mean_b7, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + xlim(0.02,0.09) + ylim(0.15,0.28) +
  labs(x = "WV2 Red (630-690 nm)",y = "WV2 NIR1 (770-895 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b5, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Red (630-690 nm)",y = "WV2 NIR2 (860-1040 nm)") 

# Comecando com banda RED-EDGE

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b6, 
                 y=WV2Spectral_Nivel4$Mean_b7, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Red-Edge (705-745 nm)",y = "WV2 NIR1 (770-895 nm)")

ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b6, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 Red-Edge (705-745 nm)",y = "WV2 NIR2 (860-1040 nm)")


# Comecando com banda NIR1
ggplot(WV2Spectral_Nivel4) +
  geom_point(aes(x=WV2Spectral_Nivel4$Mean_b7, 
                 y=WV2Spectral_Nivel4$Mean_b8, 
                 colour=factor(WV2Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "WV2 NIR1 (770-895 nm)",y = "WV2 NIR2 (860-1040 nm)")


#landsat-8

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b2, 
                 y=LAND8Spectral_Nivel4$Mean_b3, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Blue (452-512 nm)",y = "L8 Green (533-590 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b2, 
                 y=LAND8Spectral_Nivel4$Mean_b4, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Blue (452-512 nm)",y = "L8 Red (636-673 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b2, 
                 y=LAND8Spectral_Nivel4$Mean_b5, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Blue (452-512 nm)",y = "L8 NIR (851-879 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b2, 
                 y=LAND8Spectral_Nivel4$Mean_b6, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Blue (452-512 nm)",y = "L8 SWIR1 (1566-1651 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b2, 
                 y=LAND8Spectral_Nivel4$Mean_b7, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Blue (452-512 nm)",y = "L8 SWIR2 (2107-2294 nm)") 


#comecando com a banda green

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b3, 
                 y=LAND8Spectral_Nivel4$Mean_b4, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Green (533-590 nm)",y = "L8 Red (636-673 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b3, 
                 y=LAND8Spectral_Nivel4$Mean_b5, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Green (533-590 nm)",y = "L8 NIR (851-879 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b3, 
                 y=LAND8Spectral_Nivel4$Mean_b6, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Green (533-590 nm)",y = "L8 SWIR1 (1566-1651 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b3, 
                 y=LAND8Spectral_Nivel4$Mean_b7, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Green (533-590 nm)",y = "L8 SWIR2 (2107-2294 nm)") 

#comecando com a banda RED

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b4, 
                 y=LAND8Spectral_Nivel4$Mean_b5, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + xlim(0,0.4) +ylim(0,0.4) +
  labs(x = "L8 Red (636-673 nm)",y = "L8 NIR (851-879 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b4, 
                 y=LAND8Spectral_Nivel4$Mean_b6, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Red (636-673 nm)",y = "L8 SWIR1 (1566-1651 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b4, 
                 y=LAND8Spectral_Nivel4$Mean_b7, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 Red (636-673 nm)",y = "L8 SWIR2 (2107-2294 nm)") 


#comecando com a banda NIR

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b5, 
                 y=LAND8Spectral_Nivel4$Mean_b6, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 NIR (851-879 nm)",y = "L8 SWIR1 (1566-1651 nm)")

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b5, 
                 y=LAND8Spectral_Nivel4$Mean_b7, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 NIR (851-879 nm)",y = "L8 SWIR2 (2107-2294 nm)")

#comecando com a banda SWIR1

ggplot(LAND8Spectral_Nivel4) +
  geom_point(aes(x=LAND8Spectral_Nivel4$Mean_b6, 
                 y=LAND8Spectral_Nivel4$Mean_b7, 
                 colour=factor(LAND8Spectral_Nivel4$Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria")))) + 
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "L8 SWIR1 (1566-1651 nm)",y = "L8 SWIR2 (2107-2294 nm)")



#calculo das medias de cada atributo para cada classe


#####NIVEL4
medias_WV2_Nivel4<-ddply(WV2Spectral_Nivel4, .(Nivel4), summarize, Mean_b2=mean(Mean_b2), 
                         Mean_b3=mean(Mean_b3), Mean_b4=mean(Mean_b4), Mean_b5=mean(Mean_b5),
                         Mean_b6=mean(Mean_b6), Mean_b7=mean(Mean_b7), Mean_b8=mean(Mean_b8))

medias_LAND8_Nivel4<-ddply(LAND8Spectral_Nivel4, .(Nivel4), summarize, Mean_b2=mean(Mean_b2), 
                           Mean_b3=mean(Mean_b3), Mean_b4=mean(Mean_b4), Mean_b5=mean(Mean_b5),
                           Mean_b6=mean(Mean_b6), Mean_b7=mean(Mean_b7))




#plots das medias das bandas - Comportamento espectral

####NIVEL4
###
#

dfWV2_N4 <- melt(medias_WV2_Nivel4)  #the function melt reshapes it from wide to long
dfWV2_N4$rowid <- 1:10  #add a rowid identifying variable


ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Bandas",y = "Reflectância") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green","Mean_b4" = "Yellow","Mean_b5" = "Red","Mean_b6" = "RE","Mean_b7" = "NIR1","Mean_b8" = "NIR2"))


ggplot(dfWV2_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + labs(x = "Bandas",y = "Reflectância") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green","Mean_b4" = "Yellow","Mean_b5" = "Red","Mean_b6" = "RE","Mean_b7" = "NIR1","Mean_b8" = "NIR2"))



dfLAND8_N4 <- melt(medias_LAND8_Nivel4)  #the function melt reshapes it from wide to long
dfLAND8_N4$rowid <- 1:10  #add a rowid identifying variable

ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_point(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=3) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + 
  labs(x = "Bandas",y = "Reflectância") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green", "Mean_b4" = "Red","Mean_b5" = "NIR","Mean_b6" = "SWIR1","Mean_b7" = "SWIR2"))


ggplot(dfLAND8_N4, aes(variable, value, group=factor(Nivel4))) + 
  geom_line(aes(color=factor(Nivel4, c("Campo_rupestre", "Campo_limpo_umido","Campo_limpo_umido_com_murundu","Campo_limpo", "Campo_Sujo", "Cerrado_ralo", "Cerrado_tipico", "Cerrado_denso", "Vereda", "Mata_de_Galeria"))), size=1.5) +
  scale_color_manual(values=c("#FF0000", "#0000FF", "#330033","#996699", "#FF9933", "#FFFF33", "#66FF66", "#339966", "#FF99FF", "#336600")) + 
  theme(legend.title = element_blank()) + labs(x = "Bandas",y = "Reflectância") +
  scale_x_discrete(labels=c("Mean_b2" = "Blue", "Mean_b3" = "Green", "Mean_b4" = "Red","Mean_b5" = "NIR","Mean_b6" = "SWIR1","Mean_b7" = "SWIR2"))


#plots dos pontos midios das distribuicoes (escolher as bandas)

#WV2

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b3, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 Green (510-580 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b4, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 Yellow (585-625 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b5, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 Red (630-690 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b6, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 Red-Edge (705-745 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 NIR1 (770-895 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Blue (450-510 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")

#banda green

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b4, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Green (510-580 nm)",y = "Reflectância WV2 Yellow (585-625 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b5, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Green (510-580 nm)",y = "Reflectância WV2 Red (630-690 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b6, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Green (510-580 nm)",y = "Reflectância WV2 Red-Edge (705-745 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Green (510-580 nm)",y = "Reflectância WV2 NIR1 (770-895 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Green (510-580 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")


#banda Yellow

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b4, y=medias_WV2_Nivel4$Mean_b5, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Yellow (585-625 nm)",y = "Reflectância WV2 Red (630-690 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b4, y=medias_WV2_Nivel4$Mean_b6, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Yellow (585-625 nm)",y = "Reflectância WV2 Red-Edge (705-745 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b4, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Yellow (585-625 nm)",y = "Reflectância WV2 NIR1 (770-895 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b4, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Yellow (585-625 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")




#RED

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b5, y=medias_WV2_Nivel4$Mean_b6, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Red (630-690 nm)",y = "Reflectância WV2 Red-Edge (705-745 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b5, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +xlim(0,0.1) + ylim(0.1,0.35) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Red (630-690 nm)",y = "Reflectância WV2 NIR1 (770-895 nm)") + theme(text = element_text(size=20))

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b5, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Red (630-690 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")


#NIR1 e REDEDGE

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b6, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Red-Edge (705-745 nm)",y = "Reflectância WV2 NIR1 (770-895 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b6, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 Red-Edge (705-745 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")

ggplot(medias_WV2_Nivel4) + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b7, y=medias_WV2_Nivel4$Mean_b8, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância WV2 NIR1 (770-895 nm)",y = "Reflectância WV2 NIR2 (860-1040 nm)")


#LAND8 - Banda  Blue

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b3, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Blue (452-512 nm)",y = "Reflectância L8 Green (533-590 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b4, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Blue (452-512 nm)",y = "Reflectância L8 Red (636-673 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Blue (452-512 nm)",y = "Reflectância L8 NIR (851-879 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b6, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Blue (452-512 nm)",y = "Reflectância L8 SWIR1 (1566-1651 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b7, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Blue (452-512 nm)",y = "Reflectância L8 SWIR2 (2107-2294 nm)")


#banda green


ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b4, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Green (533-590 nm)",y = "Reflectância L8 Red (636-673 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Green (533-590 nm)",y = "Reflectância L8 NIR (851-879 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b6, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Green (533-590 nm)",y = "Reflectância L8 SWIR1 (1566-1651 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b7, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Green (533-590 nm)",y = "Reflectância L8 SWIR2 (2107-2294 nm)")




#banda RED

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b4, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=4) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) + xlim(0.02,0.09) + ylim(0.15,0.28) +
  labs(x = "Reflectância L8 Red (636-673 nm)",y = "Reflectância L8 NIR (851-879 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b4, y=medias_LAND8_Nivel4$Mean_b6, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Red (636-673 nm)",y = "Reflectância L8 SWIR1 (1566-1651 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b4, y=medias_LAND8_Nivel4$Mean_b7, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 Red (636-673 nm)",y = "Reflectância L8 SWIR2 (2107-2294 nm)")


#banda NIR e SWIR1

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b5, y=medias_LAND8_Nivel4$Mean_b6, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 NIR (851-879 nm)",y = "Reflectância L8 SWIR1 (1566-1651 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b5, y=medias_LAND8_Nivel4$Mean_b7, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 NIR (851-879 nm)",y = "Reflectância L8 SWIR2 (2107-2294 nm)")

ggplot(medias_LAND8_Nivel4) + 
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b6, y=medias_LAND8_Nivel4$Mean_b7, 
                 shape=factor(medias_LAND8_Nivel4$Nivel4), 
                 colour=factor(medias_LAND8_Nivel4$Nivel4)),size=5) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  scale_shape_manual(values = c(15,16,17,18,15,16,17,18,15,16)) +
  labs(x = "Reflectância L8 SWIR1 (1566-1651 nm)",y = "Reflectância L8 SWIR2 (2107-2294 nm)")





#Aquisicao dos dados do dataframe para calcular a distancia banda x banda

#bandas do WV2

b2_CL_WV2=dfWV2_N4[1,3]
b2_CLU_WV2=dfWV2_N4[2,3]
b2_CLUCM_WV2=dfWV2_N4[3,3]
b2_CRUP_WV2=dfWV2_N4[4,3]
b2_CS_WV2=dfWV2_N4[5,3]
b2_CD_WV2=dfWV2_N4[6,3]
b2_CR_WV2=dfWV2_N4[7,3]
b2_CT_WV2=dfWV2_N4[8,3]
b2_MG_WV2=dfWV2_N4[9,3]
b2_VE_WV2=dfWV2_N4[10,3]

b3_CL_WV2=dfWV2_N4[11,3]
b3_CLU_WV2=dfWV2_N4[12,3]
b3_CLUCM_WV2=dfWV2_N4[13,3]
b3_CRUP_WV2=dfWV2_N4[14,3]
b3_CS_WV2=dfWV2_N4[15,3]
b3_CD_WV2=dfWV2_N4[16,3]
b3_CR_WV2=dfWV2_N4[17,3]
b3_CT_WV2=dfWV2_N4[18,3]
b3_MG_WV2=dfWV2_N4[19,3]
b3_VE_WV2=dfWV2_N4[20,3]

b4_CL_WV2=dfWV2_N4[21,3]
b4_CLU_WV2=dfWV2_N4[22,3]
b4_CLUCM_WV2=dfWV2_N4[23,3]
b4_CRUP_WV2=dfWV2_N4[24,3]
b4_CS_WV2=dfWV2_N4[25,3]
b4_CD_WV2=dfWV2_N4[26,3]
b4_CR_WV2=dfWV2_N4[27,3]
b4_CT_WV2=dfWV2_N4[28,3]
b4_MG_WV2=dfWV2_N4[29,3]
b4_VE_WV2=dfWV2_N4[30,3]

b5_CL_WV2=dfWV2_N4[31,3]
b5_CLU_WV2=dfWV2_N4[32,3]
b5_CLUCM_WV2=dfWV2_N4[33,3]
b5_CRUP_WV2=dfWV2_N4[34,3]
b5_CS_WV2=dfWV2_N4[35,3]
b5_CD_WV2=dfWV2_N4[36,3]
b5_CR_WV2=dfWV2_N4[37,3]
b5_CT_WV2=dfWV2_N4[38,3]
b5_MG_WV2=dfWV2_N4[39,3]
b5_VE_WV2=dfWV2_N4[40,3]

b6_CL_WV2=dfWV2_N4[41,3]
b6_CLU_WV2=dfWV2_N4[42,3]
b6_CLUCM_WV2=dfWV2_N4[43,3]
b6_CRUP_WV2=dfWV2_N4[44,3]
b6_CS_WV2=dfWV2_N4[45,3]
b6_CD_WV2=dfWV2_N4[46,3]
b6_CR_WV2=dfWV2_N4[47,3]
b6_CT_WV2=dfWV2_N4[48,3]
b6_MG_WV2=dfWV2_N4[49,3]
b6_VE_WV2=dfWV2_N4[50,3]

b7_CL_WV2=dfWV2_N4[51,3]
b7_CLU_WV2=dfWV2_N4[52,3]
b7_CLUCM_WV2=dfWV2_N4[53,3]
b7_CRUP_WV2=dfWV2_N4[54,3]
b7_CS_WV2=dfWV2_N4[55,3]
b7_CD_WV2=dfWV2_N4[56,3]
b7_CR_WV2=dfWV2_N4[57,3]
b7_CT_WV2=dfWV2_N4[58,3]
b7_MG_WV2=dfWV2_N4[59,3]
b7_VE_WV2=dfWV2_N4[60,3]

b7_CL_WV2=dfWV2_N4[51,3]
b7_CLU_WV2=dfWV2_N4[52,3]
b7_CLUCM_WV2=dfWV2_N4[53,3]
b7_CRUP_WV2=dfWV2_N4[54,3]
b7_CS_WV2=dfWV2_N4[55,3]
b7_CD_WV2=dfWV2_N4[56,3]
b7_CR_WV2=dfWV2_N4[57,3]
b7_CT_WV2=dfWV2_N4[58,3]
b7_MG_WV2=dfWV2_N4[59,3]
b7_VE_WV2=dfWV2_N4[60,3]

b8_CL_WV2=dfWV2_N4[61,3]
b8_CLU_WV2=dfWV2_N4[62,3]
b8_CLUCM_WV2=dfWV2_N4[63,3]
b8_CRUP_WV2=dfWV2_N4[64,3]
b8_CS_WV2=dfWV2_N4[65,3]
b8_CD_WV2=dfWV2_N4[66,3]
b8_CR_WV2=dfWV2_N4[67,3]
b8_CT_WV2=dfWV2_N4[68,3]
b8_MG_WV2=dfWV2_N4[69,3]
b8_VE_WV2=dfWV2_N4[70,3]

#bandas do landsat8


b2_CL_LAND8=dfLAND8_N4[1,3]
b2_CLU_LAND8=dfLAND8_N4[2,3]
b2_CLUCM_LAND8=dfLAND8_N4[3,3]
b2_CRUP_LAND8=dfLAND8_N4[4,3]
b2_CS_LAND8=dfLAND8_N4[5,3]
b2_CD_LAND8=dfLAND8_N4[6,3]
b2_CR_LAND8=dfLAND8_N4[7,3]
b2_CT_LAND8=dfLAND8_N4[8,3]
b2_MG_LAND8=dfLAND8_N4[9,3]
b2_VE_LAND8=dfLAND8_N4[10,3]

b3_CL_LAND8=dfLAND8_N4[11,3]
b3_CLU_LAND8=dfLAND8_N4[12,3]
b3_CLUCM_LAND8=dfLAND8_N4[13,3]
b3_CRUP_LAND8=dfLAND8_N4[14,3]
b3_CS_LAND8=dfLAND8_N4[15,3]
b3_CD_LAND8=dfLAND8_N4[16,3]
b3_CR_LAND8=dfLAND8_N4[17,3]
b3_CT_LAND8=dfLAND8_N4[18,3]
b3_MG_LAND8=dfLAND8_N4[19,3]
b3_VE_LAND8=dfLAND8_N4[20,3]

b4_CL_LAND8=dfLAND8_N4[21,3]
b4_CLU_LAND8=dfLAND8_N4[22,3]
b4_CLUCM_LAND8=dfLAND8_N4[23,3]
b4_CRUP_LAND8=dfLAND8_N4[24,3]
b4_CS_LAND8=dfLAND8_N4[25,3]
b4_CD_LAND8=dfLAND8_N4[26,3]
b4_CR_LAND8=dfLAND8_N4[27,3]
b4_CT_LAND8=dfLAND8_N4[28,3]
b4_MG_LAND8=dfLAND8_N4[29,3]
b4_VE_LAND8=dfLAND8_N4[30,3]

b5_CL_LAND8=dfLAND8_N4[31,3]
b5_CLU_LAND8=dfLAND8_N4[32,3]
b5_CLUCM_LAND8=dfLAND8_N4[33,3]
b5_CRUP_LAND8=dfLAND8_N4[34,3]
b5_CS_LAND8=dfLAND8_N4[35,3]
b5_CD_LAND8=dfLAND8_N4[36,3]
b5_CR_LAND8=dfLAND8_N4[37,3]
b5_CT_LAND8=dfLAND8_N4[38,3]
b5_MG_LAND8=dfLAND8_N4[39,3]
b5_VE_LAND8=dfLAND8_N4[40,3]

b6_CL_LAND8=dfLAND8_N4[41,3]
b6_CLU_LAND8=dfLAND8_N4[42,3]
b6_CLUCM_LAND8=dfLAND8_N4[43,3]
b6_CRUP_LAND8=dfLAND8_N4[44,3]
b6_CS_LAND8=dfLAND8_N4[45,3]
b6_CD_LAND8=dfLAND8_N4[46,3]
b6_CR_LAND8=dfLAND8_N4[47,3]
b6_CT_LAND8=dfLAND8_N4[48,3]
b6_MG_LAND8=dfLAND8_N4[49,3]
b6_VE_LAND8=dfLAND8_N4[50,3]

b7_CL_LAND8=dfLAND8_N4[51,3]
b7_CLU_LAND8=dfLAND8_N4[52,3]
b7_CLUCM_LAND8=dfLAND8_N4[53,3]
b7_CRUP_LAND8=dfLAND8_N4[54,3]
b7_CS_LAND8=dfLAND8_N4[55,3]
b7_CD_LAND8=dfLAND8_N4[56,3]
b7_CR_LAND8=dfLAND8_N4[57,3]
b7_CT_LAND8=dfLAND8_N4[58,3]
b7_MG_LAND8=dfLAND8_N4[59,3]
b7_VE_LAND8=dfLAND8_N4[60,3]

b7_CL_LAND8=dfLAND8_N4[51,3]
b7_CLU_LAND8=dfLAND8_N4[52,3]
b7_CLUCM_LAND8=dfLAND8_N4[53,3]
b7_CRUP_LAND8=dfLAND8_N4[54,3]
b7_CS_LAND8=dfLAND8_N4[55,3]
b7_CD_LAND8=dfLAND8_N4[56,3]
b7_CR_LAND8=dfLAND8_N4[57,3]
b7_CT_LAND8=dfLAND8_N4[58,3]
b7_MG_LAND8=dfLAND8_N4[59,3]
b7_VE_LAND8=dfLAND8_N4[60,3]


#distancia euclidiana dos pontos midios 
#BLUE X GREEN (banda2 x banda3)

Matriz_distancias_WV2_N4_BLUExGREEN <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_BLUExGREEN) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_BLUExGREEN) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_BLUExGREEN <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_BLUExGREEN) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_BLUExGREEN) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_BLUExGREEN = sqrt(((b2_CLUCM_WV2-b2_CLU_WV2)^2) +((b3_CLUCM_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[2,1] <- distCLU_CLUCM_WV2_BLUExGREEN

distCLU_CRUP_WV2_BLUExGREEN = sqrt(((b2_CRUP_WV2-b2_CLU_WV2)^2) +((b3_CRUP_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[3,1] <- distCLU_CRUP_WV2_BLUExGREEN

distCLU_CL_WV2_BLUExGREEN = sqrt(((b2_CL_WV2-b2_CLU_WV2)^2) +((b3_CL_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[4,1] <- distCLU_CL_WV2_BLUExGREEN

distCLU_CS_WV2_BLUExGREEN = sqrt(((b2_CS_WV2-b2_CLU_WV2)^2) +((b3_CS_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[5,1] <- distCLU_CS_WV2_BLUExGREEN

distCLU_CR_WV2_BLUExGREEN = sqrt(((b2_CR_WV2-b2_CLU_WV2)^2) +((b3_CR_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[6,1] <- distCLU_CR_WV2_BLUExGREEN

distCLU_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CLU_WV2)^2) +((b3_CT_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,1] <- distCLU_CT_WV2_BLUExGREEN

distCLU_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CLU_WV2)^2) +((b3_CD_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,1] <- distCLU_CD_WV2_BLUExGREEN

distCLU_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CLU_WV2)^2) +((b3_VE_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,1] <- distCLU_VE_WV2_BLUExGREEN

distCLU_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CLU_WV2)^2) +((b3_MG_WV2-b3_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,1] <- distCLU_MG_WV2_BLUExGREEN


#
distCLUCM_CRUP_WV2_BLUExGREEN = sqrt(((b2_CRUP_WV2-b2_CLUCM_WV2)^2) +((b3_CRUP_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[3,2] <- distCLUCM_CRUP_WV2_BLUExGREEN

distCLUCM_CL_WV2_BLUExGREEN = sqrt(((b2_CL_WV2-b2_CLUCM_WV2)^2) +((b3_CL_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[4,2] <- distCLUCM_CL_WV2_BLUExGREEN

distCLUCM_CS_WV2_BLUExGREEN = sqrt(((b2_CS_WV2-b2_CLUCM_WV2)^2) +((b3_CS_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[5,2] <- distCLUCM_CS_WV2_BLUExGREEN

distCLUCM_CR_WV2_BLUExGREEN = sqrt(((b2_CR_WV2-b2_CLUCM_WV2)^2) +((b3_CR_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[6,2] <- distCLUCM_CR_WV2_BLUExGREEN

distCLUCM_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CLUCM_WV2)^2) +((b3_CT_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,2] <- distCLUCM_CT_WV2_BLUExGREEN

distCLUCM_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CLUCM_WV2)^2) +((b3_CD_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,2] <- distCLUCM_CD_WV2_BLUExGREEN

distCLUCM_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CLUCM_WV2)^2) +((b3_VE_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,2] <- distCLUCM_VE_WV2_BLUExGREEN

distCLUCM_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CLUCM_WV2)^2) +((b3_MG_WV2-b3_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,2] <- distCLUCM_MG_WV2_BLUExGREEN


#
distCRUP_CL_WV2_BLUExGREEN = sqrt(((b2_CL_WV2-b2_CRUP_WV2)^2) +((b3_CL_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[4,3] <- distCRUP_CL_WV2_BLUExGREEN

distCRUP_CS_WV2_BLUExGREEN = sqrt(((b2_CS_WV2-b2_CRUP_WV2)^2) +((b3_CS_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[5,3] <- distCRUP_CS_WV2_BLUExGREEN

distCRUP_CR_WV2_BLUExGREEN = sqrt(((b2_CR_WV2-b2_CRUP_WV2)^2) +((b3_CR_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[6,3] <- distCRUP_CR_WV2_BLUExGREEN

distCRUP_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CRUP_WV2)^2) +((b3_CT_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,3] <- distCRUP_CT_WV2_BLUExGREEN

distCRUP_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CRUP_WV2)^2) +((b3_CD_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,3] <- distCRUP_CD_WV2_BLUExGREEN

distCRUP_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CRUP_WV2)^2) +((b3_VE_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,3] <- distCRUP_VE_WV2_BLUExGREEN

distCRUP_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CRUP_WV2)^2) +((b3_MG_WV2-b3_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,3] <- distCRUP_MG_WV2_BLUExGREEN


#
distCL_CS_WV2_BLUExGREEN = sqrt(((b2_CS_WV2-b2_CL_WV2)^2) +((b3_CS_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[5,4] <- distCL_CS_WV2_BLUExGREEN

distCL_CR_WV2_BLUExGREEN = sqrt(((b2_CR_WV2-b2_CL_WV2)^2) +((b3_CR_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[6,4] <- distCL_CR_WV2_BLUExGREEN

distCL_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CL_WV2)^2) +((b3_CT_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,4] <- distCL_CT_WV2_BLUExGREEN

distCL_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CL_WV2)^2) +((b3_CD_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,4] <- distCL_CD_WV2_BLUExGREEN

distCL_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CL_WV2)^2) +((b3_VE_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,4] <- distCL_VE_WV2_BLUExGREEN

distCL_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CL_WV2)^2) +((b3_MG_WV2-b3_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,4] <- distCL_MG_WV2_BLUExGREEN


#
distCS_CR_WV2_BLUExGREEN = sqrt(((b2_CR_WV2-b2_CS_WV2)^2) +((b3_CR_WV2-b3_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[6,5] <- distCS_CR_WV2_BLUExGREEN

distCS_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CS_WV2)^2) +((b3_CT_WV2-b3_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,5] <- distCS_CT_WV2_BLUExGREEN

distCS_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CS_WV2)^2) +((b3_CD_WV2-b3_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,5] <- distCS_CD_WV2_BLUExGREEN

distCS_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CS_WV2)^2) +((b3_VE_WV2-b3_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,5] <- distCS_VE_WV2_BLUExGREEN

distCS_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CS_WV2)^2) +((b3_MG_WV2-b3_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,5] <- distCS_MG_WV2_BLUExGREEN


#
distCR_CT_WV2_BLUExGREEN = sqrt(((b2_CT_WV2-b2_CR_WV2)^2) +((b3_CT_WV2-b3_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[7,6] <- distCR_CT_WV2_BLUExGREEN

distCR_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CR_WV2)^2) +((b3_CD_WV2-b3_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,6] <- distCR_CD_WV2_BLUExGREEN

distCR_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CR_WV2)^2) +((b3_VE_WV2-b3_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,6] <- distCR_VE_WV2_BLUExGREEN

distCR_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CR_WV2)^2) +((b3_MG_WV2-b3_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,6] <- distCR_MG_WV2_BLUExGREEN


#
distCT_CD_WV2_BLUExGREEN = sqrt(((b2_CD_WV2-b2_CT_WV2)^2) +((b3_CD_WV2-b3_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[8,7] <- distCT_CD_WV2_BLUExGREEN

distCT_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CT_WV2)^2) +((b3_VE_WV2-b3_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,7] <- distCT_VE_WV2_BLUExGREEN

distCT_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CT_WV2)^2) +((b3_MG_WV2-b3_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,7] <- distCT_MG_WV2_BLUExGREEN

#
distCD_VE_WV2_BLUExGREEN = sqrt(((b2_VE_WV2-b2_CD_WV2)^2) +((b3_VE_WV2-b3_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[9,8] <- distCD_VE_WV2_BLUExGREEN

distCD_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_CD_WV2)^2) +((b3_MG_WV2-b3_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,8] <- distCD_MG_WV2_BLUExGREEN

#
distVE_MG_WV2_BLUExGREEN = sqrt(((b2_MG_WV2-b2_VE_WV2)^2) +((b3_MG_WV2-b3_VE_WV2)^2))
Matriz_distancias_WV2_N4_BLUExGREEN[10,9] <- distVE_MG_WV2_BLUExGREEN


#LAND8

distCLU_CLUCM_LAND8_BLUExGREEN = sqrt(((b2_CLUCM_LAND8-b2_CLU_LAND8)^2) +((b3_CLUCM_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[2,1] <- distCLU_CLUCM_LAND8_BLUExGREEN

distCLU_CRUP_LAND8_BLUExGREEN = sqrt(((b2_CRUP_LAND8-b2_CLU_LAND8)^2) +((b3_CRUP_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[3,1] <- distCLU_CRUP_LAND8_BLUExGREEN

distCLU_CL_LAND8_BLUExGREEN = sqrt(((b2_CL_LAND8-b2_CLU_LAND8)^2) +((b3_CL_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[4,1] <- distCLU_CL_LAND8_BLUExGREEN

distCLU_CS_LAND8_BLUExGREEN = sqrt(((b2_CS_LAND8-b2_CLU_LAND8)^2) +((b3_CS_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[5,1] <- distCLU_CS_LAND8_BLUExGREEN

distCLU_CR_LAND8_BLUExGREEN = sqrt(((b2_CR_LAND8-b2_CLU_LAND8)^2) +((b3_CR_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[6,1] <- distCLU_CR_LAND8_BLUExGREEN

distCLU_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CLU_LAND8)^2) +((b3_CT_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,1] <- distCLU_CT_LAND8_BLUExGREEN

distCLU_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CLU_LAND8)^2) +((b3_CD_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,1] <- distCLU_CD_LAND8_BLUExGREEN

distCLU_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CLU_LAND8)^2) +((b3_VE_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,1] <- distCLU_VE_LAND8_BLUExGREEN

distCLU_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CLU_LAND8)^2) +((b3_MG_LAND8-b3_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,1] <- distCLU_MG_LAND8_BLUExGREEN


#
distCLUCM_CRUP_LAND8_BLUExGREEN = sqrt(((b2_CRUP_LAND8-b2_CLUCM_LAND8)^2) +((b3_CRUP_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[3,2] <- distCLUCM_CRUP_LAND8_BLUExGREEN

distCLUCM_CL_LAND8_BLUExGREEN = sqrt(((b2_CL_LAND8-b2_CLUCM_LAND8)^2) +((b3_CL_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[4,2] <- distCLUCM_CL_LAND8_BLUExGREEN

distCLUCM_CS_LAND8_BLUExGREEN = sqrt(((b2_CS_LAND8-b2_CLUCM_LAND8)^2) +((b3_CS_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[5,2] <- distCLUCM_CS_LAND8_BLUExGREEN

distCLUCM_CR_LAND8_BLUExGREEN = sqrt(((b2_CR_LAND8-b2_CLUCM_LAND8)^2) +((b3_CR_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[6,2] <- distCLUCM_CR_LAND8_BLUExGREEN

distCLUCM_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CLUCM_LAND8)^2) +((b3_CT_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,2] <- distCLUCM_CT_LAND8_BLUExGREEN

distCLUCM_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CLUCM_LAND8)^2) +((b3_CD_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,2] <- distCLUCM_CD_LAND8_BLUExGREEN

distCLUCM_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CLUCM_LAND8)^2) +((b3_VE_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,2] <- distCLUCM_VE_LAND8_BLUExGREEN

distCLUCM_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CLUCM_LAND8)^2) +((b3_MG_LAND8-b3_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,2] <- distCLUCM_MG_LAND8_BLUExGREEN


#
distCRUP_CL_LAND8_BLUExGREEN = sqrt(((b2_CL_LAND8-b2_CRUP_LAND8)^2) +((b3_CL_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[4,3] <- distCRUP_CL_LAND8_BLUExGREEN

distCRUP_CS_LAND8_BLUExGREEN = sqrt(((b2_CS_LAND8-b2_CRUP_LAND8)^2) +((b3_CS_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[5,3] <- distCRUP_CS_LAND8_BLUExGREEN

distCRUP_CR_LAND8_BLUExGREEN = sqrt(((b2_CR_LAND8-b2_CRUP_LAND8)^2) +((b3_CR_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[6,3] <- distCRUP_CR_LAND8_BLUExGREEN

distCRUP_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CRUP_LAND8)^2) +((b3_CT_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,3] <- distCRUP_CT_LAND8_BLUExGREEN

distCRUP_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CRUP_LAND8)^2) +((b3_CD_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,3] <- distCRUP_CD_LAND8_BLUExGREEN

distCRUP_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CRUP_LAND8)^2) +((b3_VE_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,3] <- distCRUP_VE_LAND8_BLUExGREEN

distCRUP_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CRUP_LAND8)^2) +((b3_MG_LAND8-b3_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,3] <- distCRUP_MG_LAND8_BLUExGREEN


#
distCL_CS_LAND8_BLUExGREEN = sqrt(((b2_CS_LAND8-b2_CL_LAND8)^2) +((b3_CS_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[5,4] <- distCL_CS_LAND8_BLUExGREEN

distCL_CR_LAND8_BLUExGREEN = sqrt(((b2_CR_LAND8-b2_CL_LAND8)^2) +((b3_CR_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[6,4] <- distCL_CR_LAND8_BLUExGREEN

distCL_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CL_LAND8)^2) +((b3_CT_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,4] <- distCL_CT_LAND8_BLUExGREEN

distCL_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CL_LAND8)^2) +((b3_CD_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,4] <- distCL_CD_LAND8_BLUExGREEN

distCL_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CL_LAND8)^2) +((b3_VE_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,4] <- distCL_VE_LAND8_BLUExGREEN

distCL_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CL_LAND8)^2) +((b3_MG_LAND8-b3_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,4] <- distCL_MG_LAND8_BLUExGREEN


#
distCS_CR_LAND8_BLUExGREEN = sqrt(((b2_CR_LAND8-b2_CS_LAND8)^2) +((b3_CR_LAND8-b3_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[6,5] <- distCS_CR_LAND8_BLUExGREEN

distCS_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CS_LAND8)^2) +((b3_CT_LAND8-b3_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,5] <- distCS_CT_LAND8_BLUExGREEN

distCS_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CS_LAND8)^2) +((b3_CD_LAND8-b3_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,5] <- distCS_CD_LAND8_BLUExGREEN

distCS_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CS_LAND8)^2) +((b3_VE_LAND8-b3_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,5] <- distCS_VE_LAND8_BLUExGREEN

distCS_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CS_LAND8)^2) +((b3_MG_LAND8-b3_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,5] <- distCS_MG_LAND8_BLUExGREEN


#
distCR_CT_LAND8_BLUExGREEN = sqrt(((b2_CT_LAND8-b2_CR_LAND8)^2) +((b3_CT_LAND8-b3_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[7,6] <- distCR_CT_LAND8_BLUExGREEN

distCR_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CR_LAND8)^2) +((b3_CD_LAND8-b3_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,6] <- distCR_CD_LAND8_BLUExGREEN

distCR_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CR_LAND8)^2) +((b3_VE_LAND8-b3_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,6] <- distCR_VE_LAND8_BLUExGREEN

distCR_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CR_LAND8)^2) +((b3_MG_LAND8-b3_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,6] <- distCR_MG_LAND8_BLUExGREEN


#
distCT_CD_LAND8_BLUExGREEN = sqrt(((b2_CD_LAND8-b2_CT_LAND8)^2) +((b3_CD_LAND8-b3_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[8,7] <- distCT_CD_LAND8_BLUExGREEN

distCT_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CT_LAND8)^2) +((b3_VE_LAND8-b3_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,7] <- distCT_VE_LAND8_BLUExGREEN

distCT_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CT_LAND8)^2) +((b3_MG_LAND8-b3_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,7] <- distCT_MG_LAND8_BLUExGREEN

#
distCD_VE_LAND8_BLUExGREEN = sqrt(((b2_VE_LAND8-b2_CD_LAND8)^2) +((b3_VE_LAND8-b3_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[9,8] <- distCD_VE_LAND8_BLUExGREEN

distCD_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_CD_LAND8)^2) +((b3_MG_LAND8-b3_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,8] <- distCD_MG_LAND8_BLUExGREEN

#
distVE_MG_LAND8_BLUExGREEN = sqrt(((b2_MG_LAND8-b2_VE_LAND8)^2) +((b3_MG_LAND8-b3_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExGREEN[10,9] <- distVE_MG_LAND8_BLUExGREEN

Matriz_distancias_WV2_N4_BLUExGREEN <- round(Matriz_distancias_WV2_N4_BLUExGREEN*100, digits = 4)

Matriz_distancias_LAND8_N4_BLUExGREEN <- round(Matriz_distancias_LAND8_N4_BLUExGREEN*100, digits = 4)


#teste da matriz resultante

Matriz_result_BLUExGREEN = Matriz_distancias_WV2_N4_BLUExGREEN - Matriz_distancias_LAND8_N4_BLUExGREEN

Matriz_result_BLUExGREEN <- Matriz_result_BLUExGREEN*100

melted_matriz_BLUExGREEN <- melt(Matriz_result_BLUExGREEN)
head(Matriz_result_BLUExGREEN)

ggplot(data = melted_matriz_BLUExGREEN, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))


#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b3, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b3, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Blue",y = "Green")


#BLUE X RED

Matriz_distancias_WV2_N4_BLUExRED <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_BLUExRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_BLUExRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_BLUExRED <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_BLUExRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_BLUExRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_BLUExRED = sqrt(((b2_CLUCM_WV2-b2_CLU_WV2)^2) +((b5_CLUCM_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[2,1] <- distCLU_CLUCM_WV2_BLUExRED

distCLU_CRUP_WV2_BLUExRED = sqrt(((b2_CRUP_WV2-b2_CLU_WV2)^2) +((b5_CRUP_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[3,1] <- distCLU_CRUP_WV2_BLUExRED

distCLU_CL_WV2_BLUExRED = sqrt(((b2_CL_WV2-b2_CLU_WV2)^2) +((b5_CL_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[4,1] <- distCLU_CL_WV2_BLUExRED

distCLU_CS_WV2_BLUExRED = sqrt(((b2_CS_WV2-b2_CLU_WV2)^2) +((b5_CS_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[5,1] <- distCLU_CS_WV2_BLUExRED

distCLU_CR_WV2_BLUExRED = sqrt(((b2_CR_WV2-b2_CLU_WV2)^2) +((b5_CR_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[6,1] <- distCLU_CR_WV2_BLUExRED

distCLU_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CLU_WV2)^2) +((b5_CT_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,1] <- distCLU_CT_WV2_BLUExRED

distCLU_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CLU_WV2)^2) +((b5_CD_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,1] <- distCLU_CD_WV2_BLUExRED

distCLU_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CLU_WV2)^2) +((b5_VE_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,1] <- distCLU_VE_WV2_BLUExRED

distCLU_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CLU_WV2)^2) +((b5_MG_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,1] <- distCLU_MG_WV2_BLUExRED


#
distCLUCM_CRUP_WV2_BLUExRED = sqrt(((b2_CRUP_WV2-b2_CLUCM_WV2)^2) +((b5_CRUP_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[3,2] <- distCLUCM_CRUP_WV2_BLUExRED

distCLUCM_CL_WV2_BLUExRED = sqrt(((b2_CL_WV2-b2_CLUCM_WV2)^2) +((b5_CL_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[4,2] <- distCLUCM_CL_WV2_BLUExRED

distCLUCM_CS_WV2_BLUExRED = sqrt(((b2_CS_WV2-b2_CLUCM_WV2)^2) +((b5_CS_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[5,2] <- distCLUCM_CS_WV2_BLUExRED

distCLUCM_CR_WV2_BLUExRED = sqrt(((b2_CR_WV2-b2_CLUCM_WV2)^2) +((b5_CR_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[6,2] <- distCLUCM_CR_WV2_BLUExRED

distCLUCM_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CLUCM_WV2)^2) +((b5_CT_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,2] <- distCLUCM_CT_WV2_BLUExRED

distCLUCM_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CLUCM_WV2)^2) +((b5_CD_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,2] <- distCLUCM_CD_WV2_BLUExRED

distCLUCM_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CLUCM_WV2)^2) +((b5_VE_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,2] <- distCLUCM_VE_WV2_BLUExRED

distCLUCM_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CLUCM_WV2)^2) +((b5_MG_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,2] <- distCLUCM_MG_WV2_BLUExRED


#
distCRUP_CL_WV2_BLUExRED = sqrt(((b2_CL_WV2-b2_CRUP_WV2)^2) +((b5_CL_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[4,3] <- distCRUP_CL_WV2_BLUExRED

distCRUP_CS_WV2_BLUExRED = sqrt(((b2_CS_WV2-b2_CRUP_WV2)^2) +((b5_CS_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[5,3] <- distCRUP_CS_WV2_BLUExRED

distCRUP_CR_WV2_BLUExRED = sqrt(((b2_CR_WV2-b2_CRUP_WV2)^2) +((b5_CR_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[6,3] <- distCRUP_CR_WV2_BLUExRED

distCRUP_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CRUP_WV2)^2) +((b5_CT_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,3] <- distCRUP_CT_WV2_BLUExRED

distCRUP_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CRUP_WV2)^2) +((b5_CD_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,3] <- distCRUP_CD_WV2_BLUExRED

distCRUP_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CRUP_WV2)^2) +((b5_VE_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,3] <- distCRUP_VE_WV2_BLUExRED

distCRUP_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CRUP_WV2)^2) +((b5_MG_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,3] <- distCRUP_MG_WV2_BLUExRED


#
distCL_CS_WV2_BLUExRED = sqrt(((b2_CS_WV2-b2_CL_WV2)^2) +((b5_CS_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[5,4] <- distCL_CS_WV2_BLUExRED

distCL_CR_WV2_BLUExRED = sqrt(((b2_CR_WV2-b2_CL_WV2)^2) +((b5_CR_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[6,4] <- distCL_CR_WV2_BLUExRED

distCL_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CL_WV2)^2) +((b5_CT_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,4] <- distCL_CT_WV2_BLUExRED

distCL_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CL_WV2)^2) +((b5_CD_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,4] <- distCL_CD_WV2_BLUExRED

distCL_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CL_WV2)^2) +((b5_VE_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,4] <- distCL_VE_WV2_BLUExRED

distCL_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CL_WV2)^2) +((b5_MG_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,4] <- distCL_MG_WV2_BLUExRED


#
distCS_CR_WV2_BLUExRED = sqrt(((b2_CR_WV2-b2_CS_WV2)^2) +((b5_CR_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[6,5] <- distCS_CR_WV2_BLUExRED

distCS_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CS_WV2)^2) +((b5_CT_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,5] <- distCS_CT_WV2_BLUExRED

distCS_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CS_WV2)^2) +((b5_CD_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,5] <- distCS_CD_WV2_BLUExRED

distCS_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CS_WV2)^2) +((b5_VE_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,5] <- distCS_VE_WV2_BLUExRED

distCS_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CS_WV2)^2) +((b5_MG_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,5] <- distCS_MG_WV2_BLUExRED


#
distCR_CT_WV2_BLUExRED = sqrt(((b2_CT_WV2-b2_CR_WV2)^2) +((b5_CT_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[7,6] <- distCR_CT_WV2_BLUExRED

distCR_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CR_WV2)^2) +((b5_CD_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,6] <- distCR_CD_WV2_BLUExRED

distCR_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CR_WV2)^2) +((b5_VE_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,6] <- distCR_VE_WV2_BLUExRED

distCR_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CR_WV2)^2) +((b5_MG_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,6] <- distCR_MG_WV2_BLUExRED


#
distCT_CD_WV2_BLUExRED = sqrt(((b2_CD_WV2-b2_CT_WV2)^2) +((b5_CD_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[8,7] <- distCT_CD_WV2_BLUExRED

distCT_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CT_WV2)^2) +((b5_VE_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,7] <- distCT_VE_WV2_BLUExRED

distCT_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CT_WV2)^2) +((b5_MG_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,7] <- distCT_MG_WV2_BLUExRED

#
distCD_VE_WV2_BLUExRED = sqrt(((b2_VE_WV2-b2_CD_WV2)^2) +((b5_VE_WV2-b5_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[9,8] <- distCD_VE_WV2_BLUExRED

distCD_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_CD_WV2)^2) +((b5_MG_WV2-b5_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,8] <- distCD_MG_WV2_BLUExRED

#
distVE_MG_WV2_BLUExRED = sqrt(((b2_MG_WV2-b2_VE_WV2)^2) +((b5_MG_WV2-b5_VE_WV2)^2))
Matriz_distancias_WV2_N4_BLUExRED[10,9] <- distVE_MG_WV2_BLUExRED


#LAND8

distCLU_CLUCM_LAND8_BLUExRED = sqrt(((b2_CLUCM_LAND8-b2_CLU_LAND8)^2) +((b4_CLUCM_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[2,1] <- distCLU_CLUCM_LAND8_BLUExRED

distCLU_CRUP_LAND8_BLUExRED = sqrt(((b2_CRUP_LAND8-b2_CLU_LAND8)^2) +((b4_CRUP_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[3,1] <- distCLU_CRUP_LAND8_BLUExRED

distCLU_CL_LAND8_BLUExRED = sqrt(((b2_CL_LAND8-b2_CLU_LAND8)^2) +((b4_CL_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[4,1] <- distCLU_CL_LAND8_BLUExRED

distCLU_CS_LAND8_BLUExRED = sqrt(((b2_CS_LAND8-b2_CLU_LAND8)^2) +((b4_CS_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[5,1] <- distCLU_CS_LAND8_BLUExRED

distCLU_CR_LAND8_BLUExRED = sqrt(((b2_CR_LAND8-b2_CLU_LAND8)^2) +((b4_CR_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[6,1] <- distCLU_CR_LAND8_BLUExRED

distCLU_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CLU_LAND8)^2) +((b4_CT_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,1] <- distCLU_CT_LAND8_BLUExRED

distCLU_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CLU_LAND8)^2) +((b4_CD_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,1] <- distCLU_CD_LAND8_BLUExRED

distCLU_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CLU_LAND8)^2) +((b4_VE_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,1] <- distCLU_VE_LAND8_BLUExRED

distCLU_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CLU_LAND8)^2) +((b4_MG_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,1] <- distCLU_MG_LAND8_BLUExRED


#
distCLUCM_CRUP_LAND8_BLUExRED = sqrt(((b2_CRUP_LAND8-b2_CLUCM_LAND8)^2) +((b4_CRUP_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[3,2] <- distCLUCM_CRUP_LAND8_BLUExRED

distCLUCM_CL_LAND8_BLUExRED = sqrt(((b2_CL_LAND8-b2_CLUCM_LAND8)^2) +((b4_CL_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[4,2] <- distCLUCM_CL_LAND8_BLUExRED

distCLUCM_CS_LAND8_BLUExRED = sqrt(((b2_CS_LAND8-b2_CLUCM_LAND8)^2) +((b4_CS_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[5,2] <- distCLUCM_CS_LAND8_BLUExRED

distCLUCM_CR_LAND8_BLUExRED = sqrt(((b2_CR_LAND8-b2_CLUCM_LAND8)^2) +((b4_CR_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[6,2] <- distCLUCM_CR_LAND8_BLUExRED

distCLUCM_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CLUCM_LAND8)^2) +((b4_CT_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,2] <- distCLUCM_CT_LAND8_BLUExRED

distCLUCM_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CLUCM_LAND8)^2) +((b4_CD_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,2] <- distCLUCM_CD_LAND8_BLUExRED

distCLUCM_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CLUCM_LAND8)^2) +((b4_VE_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,2] <- distCLUCM_VE_LAND8_BLUExRED

distCLUCM_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CLUCM_LAND8)^2) +((b4_MG_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,2] <- distCLUCM_MG_LAND8_BLUExRED


#
distCRUP_CL_LAND8_BLUExRED = sqrt(((b2_CL_LAND8-b2_CRUP_LAND8)^2) +((b4_CL_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[4,3] <- distCRUP_CL_LAND8_BLUExRED

distCRUP_CS_LAND8_BLUExRED = sqrt(((b2_CS_LAND8-b2_CRUP_LAND8)^2) +((b4_CS_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[5,3] <- distCRUP_CS_LAND8_BLUExRED

distCRUP_CR_LAND8_BLUExRED = sqrt(((b2_CR_LAND8-b2_CRUP_LAND8)^2) +((b4_CR_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[6,3] <- distCRUP_CR_LAND8_BLUExRED

distCRUP_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CRUP_LAND8)^2) +((b4_CT_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,3] <- distCRUP_CT_LAND8_BLUExRED

distCRUP_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CRUP_LAND8)^2) +((b4_CD_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,3] <- distCRUP_CD_LAND8_BLUExRED

distCRUP_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CRUP_LAND8)^2) +((b4_VE_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,3] <- distCRUP_VE_LAND8_BLUExRED

distCRUP_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CRUP_LAND8)^2) +((b4_MG_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,3] <- distCRUP_MG_LAND8_BLUExRED


#
distCL_CS_LAND8_BLUExRED = sqrt(((b2_CS_LAND8-b2_CL_LAND8)^2) +((b4_CS_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[5,4] <- distCL_CS_LAND8_BLUExRED

distCL_CR_LAND8_BLUExRED = sqrt(((b2_CR_LAND8-b2_CL_LAND8)^2) +((b4_CR_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[6,4] <- distCL_CR_LAND8_BLUExRED

distCL_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CL_LAND8)^2) +((b4_CT_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,4] <- distCL_CT_LAND8_BLUExRED

distCL_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CL_LAND8)^2) +((b4_CD_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,4] <- distCL_CD_LAND8_BLUExRED

distCL_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CL_LAND8)^2) +((b4_VE_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,4] <- distCL_VE_LAND8_BLUExRED

distCL_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CL_LAND8)^2) +((b4_MG_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,4] <- distCL_MG_LAND8_BLUExRED


#
distCS_CR_LAND8_BLUExRED = sqrt(((b2_CR_LAND8-b2_CS_LAND8)^2) +((b4_CR_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[6,5] <- distCS_CR_LAND8_BLUExRED

distCS_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CS_LAND8)^2) +((b4_CT_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,5] <- distCS_CT_LAND8_BLUExRED

distCS_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CS_LAND8)^2) +((b4_CD_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,5] <- distCS_CD_LAND8_BLUExRED

distCS_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CS_LAND8)^2) +((b4_VE_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,5] <- distCS_VE_LAND8_BLUExRED

distCS_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CS_LAND8)^2) +((b4_MG_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,5] <- distCS_MG_LAND8_BLUExRED


#
distCR_CT_LAND8_BLUExRED = sqrt(((b2_CT_LAND8-b2_CR_LAND8)^2) +((b4_CT_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[7,6] <- distCR_CT_LAND8_BLUExRED

distCR_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CR_LAND8)^2) +((b4_CD_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,6] <- distCR_CD_LAND8_BLUExRED

distCR_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CR_LAND8)^2) +((b4_VE_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,6] <- distCR_VE_LAND8_BLUExRED

distCR_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CR_LAND8)^2) +((b4_MG_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,6] <- distCR_MG_LAND8_BLUExRED


#
distCT_CD_LAND8_BLUExRED = sqrt(((b2_CD_LAND8-b2_CT_LAND8)^2) +((b4_CD_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[8,7] <- distCT_CD_LAND8_BLUExRED

distCT_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CT_LAND8)^2) +((b4_VE_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,7] <- distCT_VE_LAND8_BLUExRED

distCT_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CT_LAND8)^2) +((b4_MG_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,7] <- distCT_MG_LAND8_BLUExRED

#
distCD_VE_LAND8_BLUExRED = sqrt(((b2_VE_LAND8-b2_CD_LAND8)^2) +((b4_VE_LAND8-b4_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[9,8] <- distCD_VE_LAND8_BLUExRED

distCD_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_CD_LAND8)^2) +((b4_MG_LAND8-b4_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,8] <- distCD_MG_LAND8_BLUExRED

#
distVE_MG_LAND8_BLUExRED = sqrt(((b2_MG_LAND8-b2_VE_LAND8)^2) +((b4_MG_LAND8-b4_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExRED[10,9] <- distVE_MG_LAND8_BLUExRED

Matriz_distancias_WV2_N4_BLUExRED <- round(Matriz_distancias_WV2_N4_BLUExRED*100, digits = 4)

Matriz_distancias_LAND8_N4_BLUExRED <- round(Matriz_distancias_LAND8_N4_BLUExRED*100, digits = 4)

#teste da matriz resultante

Matriz_result_BLUExRED = Matriz_distancias_WV2_N4_BLUExRED - Matriz_distancias_LAND8_N4_BLUExRED

Matriz_result_BLUExRED <- Matriz_result_BLUExRED*100

melted_matriz_BLUExRED <- melt(Matriz_result_BLUExRED)
head(Matriz_result_BLUExRED)

ggplot(data = melted_matriz_BLUExRED, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))


#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b5, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b4, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Blue",y = "Red")

#BLUE X NIR

Matriz_distancias_WV2_N4_BLUExNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_BLUExNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_BLUExNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_BLUExNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_BLUExNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_BLUExNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_BLUExNIR = sqrt(((b2_CLUCM_WV2-b2_CLU_WV2)^2) +((b7_CLUCM_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[2,1] <- distCLU_CLUCM_WV2_BLUExNIR

distCLU_CRUP_WV2_BLUExNIR = sqrt(((b2_CRUP_WV2-b2_CLU_WV2)^2) +((b7_CRUP_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[3,1] <- distCLU_CRUP_WV2_BLUExNIR

distCLU_CL_WV2_BLUExNIR = sqrt(((b2_CL_WV2-b2_CLU_WV2)^2) +((b7_CL_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[4,1] <- distCLU_CL_WV2_BLUExNIR

distCLU_CS_WV2_BLUExNIR = sqrt(((b2_CS_WV2-b2_CLU_WV2)^2) +((b7_CS_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[5,1] <- distCLU_CS_WV2_BLUExNIR

distCLU_CR_WV2_BLUExNIR = sqrt(((b2_CR_WV2-b2_CLU_WV2)^2) +((b7_CR_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[6,1] <- distCLU_CR_WV2_BLUExNIR

distCLU_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CLU_WV2)^2) +((b7_CT_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,1] <- distCLU_CT_WV2_BLUExNIR

distCLU_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CLU_WV2)^2) +((b7_CD_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,1] <- distCLU_CD_WV2_BLUExNIR

distCLU_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CLU_WV2)^2) +((b7_VE_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,1] <- distCLU_VE_WV2_BLUExNIR

distCLU_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CLU_WV2)^2) +((b7_MG_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,1] <- distCLU_MG_WV2_BLUExNIR


#
distCLUCM_CRUP_WV2_BLUExNIR = sqrt(((b2_CRUP_WV2-b2_CLUCM_WV2)^2) +((b7_CRUP_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[3,2] <- distCLUCM_CRUP_WV2_BLUExNIR

distCLUCM_CL_WV2_BLUExNIR = sqrt(((b2_CL_WV2-b2_CLUCM_WV2)^2) +((b7_CL_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[4,2] <- distCLUCM_CL_WV2_BLUExNIR

distCLUCM_CS_WV2_BLUExNIR = sqrt(((b2_CS_WV2-b2_CLUCM_WV2)^2) +((b7_CS_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[5,2] <- distCLUCM_CS_WV2_BLUExNIR

distCLUCM_CR_WV2_BLUExNIR = sqrt(((b2_CR_WV2-b2_CLUCM_WV2)^2) +((b7_CR_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[6,2] <- distCLUCM_CR_WV2_BLUExNIR

distCLUCM_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CLUCM_WV2)^2) +((b7_CT_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,2] <- distCLUCM_CT_WV2_BLUExNIR

distCLUCM_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CLUCM_WV2)^2) +((b7_CD_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,2] <- distCLUCM_CD_WV2_BLUExNIR

distCLUCM_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CLUCM_WV2)^2) +((b7_VE_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,2] <- distCLUCM_VE_WV2_BLUExNIR

distCLUCM_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CLUCM_WV2)^2) +((b7_MG_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,2] <- distCLUCM_MG_WV2_BLUExNIR


#
distCRUP_CL_WV2_BLUExNIR = sqrt(((b2_CL_WV2-b2_CRUP_WV2)^2) +((b7_CL_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[4,3] <- distCRUP_CL_WV2_BLUExNIR

distCRUP_CS_WV2_BLUExNIR = sqrt(((b2_CS_WV2-b2_CRUP_WV2)^2) +((b7_CS_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[5,3] <- distCRUP_CS_WV2_BLUExNIR

distCRUP_CR_WV2_BLUExNIR = sqrt(((b2_CR_WV2-b2_CRUP_WV2)^2) +((b7_CR_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[6,3] <- distCRUP_CR_WV2_BLUExNIR

distCRUP_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CRUP_WV2)^2) +((b7_CT_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,3] <- distCRUP_CT_WV2_BLUExNIR

distCRUP_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CRUP_WV2)^2) +((b7_CD_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,3] <- distCRUP_CD_WV2_BLUExNIR

distCRUP_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CRUP_WV2)^2) +((b7_VE_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,3] <- distCRUP_VE_WV2_BLUExNIR

distCRUP_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CRUP_WV2)^2) +((b7_MG_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,3] <- distCRUP_MG_WV2_BLUExNIR


#
distCL_CS_WV2_BLUExNIR = sqrt(((b2_CS_WV2-b2_CL_WV2)^2) +((b7_CS_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[5,4] <- distCL_CS_WV2_BLUExNIR

distCL_CR_WV2_BLUExNIR = sqrt(((b2_CR_WV2-b2_CL_WV2)^2) +((b7_CR_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[6,4] <- distCL_CR_WV2_BLUExNIR

distCL_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CL_WV2)^2) +((b7_CT_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,4] <- distCL_CT_WV2_BLUExNIR

distCL_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CL_WV2)^2) +((b7_CD_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,4] <- distCL_CD_WV2_BLUExNIR

distCL_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CL_WV2)^2) +((b7_VE_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,4] <- distCL_VE_WV2_BLUExNIR

distCL_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CL_WV2)^2) +((b7_MG_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,4] <- distCL_MG_WV2_BLUExNIR


#
distCS_CR_WV2_BLUExNIR = sqrt(((b2_CR_WV2-b2_CS_WV2)^2) +((b7_CR_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[6,5] <- distCS_CR_WV2_BLUExNIR

distCS_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CS_WV2)^2) +((b7_CT_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,5] <- distCS_CT_WV2_BLUExNIR

distCS_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CS_WV2)^2) +((b7_CD_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,5] <- distCS_CD_WV2_BLUExNIR

distCS_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CS_WV2)^2) +((b7_VE_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,5] <- distCS_VE_WV2_BLUExNIR

distCS_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CS_WV2)^2) +((b7_MG_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,5] <- distCS_MG_WV2_BLUExNIR


#
distCR_CT_WV2_BLUExNIR = sqrt(((b2_CT_WV2-b2_CR_WV2)^2) +((b7_CT_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[7,6] <- distCR_CT_WV2_BLUExNIR

distCR_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CR_WV2)^2) +((b7_CD_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,6] <- distCR_CD_WV2_BLUExNIR

distCR_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CR_WV2)^2) +((b7_VE_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,6] <- distCR_VE_WV2_BLUExNIR

distCR_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CR_WV2)^2) +((b7_MG_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,6] <- distCR_MG_WV2_BLUExNIR


#
distCT_CD_WV2_BLUExNIR = sqrt(((b2_CD_WV2-b2_CT_WV2)^2) +((b7_CD_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[8,7] <- distCT_CD_WV2_BLUExNIR

distCT_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CT_WV2)^2) +((b7_VE_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,7] <- distCT_VE_WV2_BLUExNIR

distCT_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CT_WV2)^2) +((b7_MG_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,7] <- distCT_MG_WV2_BLUExNIR

#
distCD_VE_WV2_BLUExNIR = sqrt(((b2_VE_WV2-b2_CD_WV2)^2) +((b7_VE_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[9,8] <- distCD_VE_WV2_BLUExNIR

distCD_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_CD_WV2)^2) +((b7_MG_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,8] <- distCD_MG_WV2_BLUExNIR

#
distVE_MG_WV2_BLUExNIR = sqrt(((b2_MG_WV2-b2_VE_WV2)^2) +((b7_MG_WV2-b7_VE_WV2)^2))
Matriz_distancias_WV2_N4_BLUExNIR[10,9] <- distVE_MG_WV2_BLUExNIR


#LAND8

distCLU_CLUCM_LAND8_BLUExNIR = sqrt(((b2_CLUCM_LAND8-b2_CLU_LAND8)^2) +((b5_CLUCM_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[2,1] <- distCLU_CLUCM_LAND8_BLUExNIR

distCLU_CRUP_LAND8_BLUExNIR = sqrt(((b2_CRUP_LAND8-b2_CLU_LAND8)^2) +((b5_CRUP_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[3,1] <- distCLU_CRUP_LAND8_BLUExNIR

distCLU_CL_LAND8_BLUExNIR = sqrt(((b2_CL_LAND8-b2_CLU_LAND8)^2) +((b5_CL_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[4,1] <- distCLU_CL_LAND8_BLUExNIR

distCLU_CS_LAND8_BLUExNIR = sqrt(((b2_CS_LAND8-b2_CLU_LAND8)^2) +((b5_CS_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[5,1] <- distCLU_CS_LAND8_BLUExNIR

distCLU_CR_LAND8_BLUExNIR = sqrt(((b2_CR_LAND8-b2_CLU_LAND8)^2) +((b5_CR_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[6,1] <- distCLU_CR_LAND8_BLUExNIR

distCLU_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CLU_LAND8)^2) +((b5_CT_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,1] <- distCLU_CT_LAND8_BLUExNIR

distCLU_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CLU_LAND8)^2) +((b5_CD_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,1] <- distCLU_CD_LAND8_BLUExNIR

distCLU_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CLU_LAND8)^2) +((b5_VE_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,1] <- distCLU_VE_LAND8_BLUExNIR

distCLU_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CLU_LAND8)^2) +((b5_MG_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,1] <- distCLU_MG_LAND8_BLUExNIR


#
distCLUCM_CRUP_LAND8_BLUExNIR = sqrt(((b2_CRUP_LAND8-b2_CLUCM_LAND8)^2) +((b5_CRUP_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[3,2] <- distCLUCM_CRUP_LAND8_BLUExNIR

distCLUCM_CL_LAND8_BLUExNIR = sqrt(((b2_CL_LAND8-b2_CLUCM_LAND8)^2) +((b5_CL_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[4,2] <- distCLUCM_CL_LAND8_BLUExNIR

distCLUCM_CS_LAND8_BLUExNIR = sqrt(((b2_CS_LAND8-b2_CLUCM_LAND8)^2) +((b5_CS_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[5,2] <- distCLUCM_CS_LAND8_BLUExNIR

distCLUCM_CR_LAND8_BLUExNIR = sqrt(((b2_CR_LAND8-b2_CLUCM_LAND8)^2) +((b5_CR_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[6,2] <- distCLUCM_CR_LAND8_BLUExNIR

distCLUCM_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CLUCM_LAND8)^2) +((b5_CT_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,2] <- distCLUCM_CT_LAND8_BLUExNIR

distCLUCM_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CLUCM_LAND8)^2) +((b5_CD_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,2] <- distCLUCM_CD_LAND8_BLUExNIR

distCLUCM_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CLUCM_LAND8)^2) +((b5_VE_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,2] <- distCLUCM_VE_LAND8_BLUExNIR

distCLUCM_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CLUCM_LAND8)^2) +((b5_MG_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,2] <- distCLUCM_MG_LAND8_BLUExNIR


#
distCRUP_CL_LAND8_BLUExNIR = sqrt(((b2_CL_LAND8-b2_CRUP_LAND8)^2) +((b5_CL_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[4,3] <- distCRUP_CL_LAND8_BLUExNIR

distCRUP_CS_LAND8_BLUExNIR = sqrt(((b2_CS_LAND8-b2_CRUP_LAND8)^2) +((b5_CS_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[5,3] <- distCRUP_CS_LAND8_BLUExNIR

distCRUP_CR_LAND8_BLUExNIR = sqrt(((b2_CR_LAND8-b2_CRUP_LAND8)^2) +((b5_CR_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[6,3] <- distCRUP_CR_LAND8_BLUExNIR

distCRUP_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CRUP_LAND8)^2) +((b5_CT_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,3] <- distCRUP_CT_LAND8_BLUExNIR

distCRUP_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CRUP_LAND8)^2) +((b5_CD_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,3] <- distCRUP_CD_LAND8_BLUExNIR

distCRUP_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CRUP_LAND8)^2) +((b5_VE_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,3] <- distCRUP_VE_LAND8_BLUExNIR

distCRUP_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CRUP_LAND8)^2) +((b5_MG_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,3] <- distCRUP_MG_LAND8_BLUExNIR


#
distCL_CS_LAND8_BLUExNIR = sqrt(((b2_CS_LAND8-b2_CL_LAND8)^2) +((b5_CS_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[5,4] <- distCL_CS_LAND8_BLUExNIR

distCL_CR_LAND8_BLUExNIR = sqrt(((b2_CR_LAND8-b2_CL_LAND8)^2) +((b5_CR_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[6,4] <- distCL_CR_LAND8_BLUExNIR

distCL_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CL_LAND8)^2) +((b5_CT_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,4] <- distCL_CT_LAND8_BLUExNIR

distCL_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CL_LAND8)^2) +((b5_CD_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,4] <- distCL_CD_LAND8_BLUExNIR

distCL_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CL_LAND8)^2) +((b5_VE_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,4] <- distCL_VE_LAND8_BLUExNIR

distCL_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CL_LAND8)^2) +((b5_MG_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,4] <- distCL_MG_LAND8_BLUExNIR


#
distCS_CR_LAND8_BLUExNIR = sqrt(((b2_CR_LAND8-b2_CS_LAND8)^2) +((b5_CR_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[6,5] <- distCS_CR_LAND8_BLUExNIR

distCS_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CS_LAND8)^2) +((b5_CT_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,5] <- distCS_CT_LAND8_BLUExNIR

distCS_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CS_LAND8)^2) +((b5_CD_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,5] <- distCS_CD_LAND8_BLUExNIR

distCS_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CS_LAND8)^2) +((b5_VE_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,5] <- distCS_VE_LAND8_BLUExNIR

distCS_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CS_LAND8)^2) +((b5_MG_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,5] <- distCS_MG_LAND8_BLUExNIR


#
distCR_CT_LAND8_BLUExNIR = sqrt(((b2_CT_LAND8-b2_CR_LAND8)^2) +((b5_CT_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[7,6] <- distCR_CT_LAND8_BLUExNIR

distCR_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CR_LAND8)^2) +((b5_CD_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,6] <- distCR_CD_LAND8_BLUExNIR

distCR_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CR_LAND8)^2) +((b5_VE_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,6] <- distCR_VE_LAND8_BLUExNIR

distCR_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CR_LAND8)^2) +((b5_MG_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,6] <- distCR_MG_LAND8_BLUExNIR


#
distCT_CD_LAND8_BLUExNIR = sqrt(((b2_CD_LAND8-b2_CT_LAND8)^2) +((b5_CD_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[8,7] <- distCT_CD_LAND8_BLUExNIR

distCT_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CT_LAND8)^2) +((b5_VE_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,7] <- distCT_VE_LAND8_BLUExNIR

distCT_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CT_LAND8)^2) +((b5_MG_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,7] <- distCT_MG_LAND8_BLUExNIR

#
distCD_VE_LAND8_BLUExNIR = sqrt(((b2_VE_LAND8-b2_CD_LAND8)^2) +((b5_VE_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[9,8] <- distCD_VE_LAND8_BLUExNIR

distCD_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_CD_LAND8)^2) +((b5_MG_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,8] <- distCD_MG_LAND8_BLUExNIR

#
distVE_MG_LAND8_BLUExNIR = sqrt(((b2_MG_LAND8-b2_VE_LAND8)^2) +((b5_MG_LAND8-b5_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_BLUExNIR[10,9] <- distVE_MG_LAND8_BLUExNIR

Matriz_distancias_WV2_N4_BLUExNIR <- round(Matriz_distancias_WV2_N4_BLUExNIR*100, digits = 4)

Matriz_distancias_LAND8_N4_BLUExNIR <- round(Matriz_distancias_LAND8_N4_BLUExNIR*100, digits = 4)



#teste da matriz resultante

Matriz_result_BLUExNIR = Matriz_distancias_WV2_N4_BLUExNIR - Matriz_distancias_LAND8_N4_BLUExNIR

Matriz_result_BLUExNIR <- Matriz_result_BLUExNIR*100

melted_matriz_BLUExNIR <- melt(Matriz_result_BLUExNIR)
head(Matriz_result_BLUExNIR)

ggplot(data = melted_matriz_BLUExNIR, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "RED", high = "blue", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))


#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b2, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b2, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Blue",y = "NIR")



#GREEN X RED

Matriz_distancias_WV2_N4_GREENxRED <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_GREENxRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_GREENxRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_GREENxRED <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_GREENxRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_GREENxRED) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_GREENxRED = sqrt(((b3_CLUCM_WV2-b3_CLU_WV2)^2) +((b5_CLUCM_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[2,1] <- distCLU_CLUCM_WV2_GREENxRED

distCLU_CRUP_WV2_GREENxRED = sqrt(((b3_CRUP_WV2-b3_CLU_WV2)^2) +((b5_CRUP_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[3,1] <- distCLU_CRUP_WV2_GREENxRED

distCLU_CL_WV2_GREENxRED = sqrt(((b3_CL_WV2-b3_CLU_WV2)^2) +((b5_CL_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[4,1] <- distCLU_CL_WV2_GREENxRED

distCLU_CS_WV2_GREENxRED = sqrt(((b3_CS_WV2-b3_CLU_WV2)^2) +((b5_CS_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[5,1] <- distCLU_CS_WV2_GREENxRED

distCLU_CR_WV2_GREENxRED = sqrt(((b3_CR_WV2-b3_CLU_WV2)^2) +((b5_CR_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[6,1] <- distCLU_CR_WV2_GREENxRED

distCLU_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CLU_WV2)^2) +((b5_CT_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,1] <- distCLU_CT_WV2_GREENxRED

distCLU_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CLU_WV2)^2) +((b5_CD_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,1] <- distCLU_CD_WV2_GREENxRED

distCLU_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CLU_WV2)^2) +((b5_VE_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,1] <- distCLU_VE_WV2_GREENxRED

distCLU_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CLU_WV2)^2) +((b5_MG_WV2-b5_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,1] <- distCLU_MG_WV2_GREENxRED


#
distCLUCM_CRUP_WV2_GREENxRED = sqrt(((b3_CRUP_WV2-b3_CLUCM_WV2)^2) +((b5_CRUP_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[3,2] <- distCLUCM_CRUP_WV2_GREENxRED

distCLUCM_CL_WV2_GREENxRED = sqrt(((b3_CL_WV2-b3_CLUCM_WV2)^2) +((b5_CL_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[4,2] <- distCLUCM_CL_WV2_GREENxRED

distCLUCM_CS_WV2_GREENxRED = sqrt(((b3_CS_WV2-b3_CLUCM_WV2)^2) +((b5_CS_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[5,2] <- distCLUCM_CS_WV2_GREENxRED

distCLUCM_CR_WV2_GREENxRED = sqrt(((b3_CR_WV2-b3_CLUCM_WV2)^2) +((b5_CR_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[6,2] <- distCLUCM_CR_WV2_GREENxRED

distCLUCM_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CLUCM_WV2)^2) +((b5_CT_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,2] <- distCLUCM_CT_WV2_GREENxRED

distCLUCM_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CLUCM_WV2)^2) +((b5_CD_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,2] <- distCLUCM_CD_WV2_GREENxRED

distCLUCM_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CLUCM_WV2)^2) +((b5_VE_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,2] <- distCLUCM_VE_WV2_GREENxRED

distCLUCM_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CLUCM_WV2)^2) +((b5_MG_WV2-b5_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,2] <- distCLUCM_MG_WV2_GREENxRED


#
distCRUP_CL_WV2_GREENxRED = sqrt(((b3_CL_WV2-b3_CRUP_WV2)^2) +((b5_CL_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[4,3] <- distCRUP_CL_WV2_GREENxRED

distCRUP_CS_WV2_GREENxRED = sqrt(((b3_CS_WV2-b3_CRUP_WV2)^2) +((b5_CS_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[5,3] <- distCRUP_CS_WV2_GREENxRED

distCRUP_CR_WV2_GREENxRED = sqrt(((b3_CR_WV2-b3_CRUP_WV2)^2) +((b5_CR_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[6,3] <- distCRUP_CR_WV2_GREENxRED

distCRUP_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CRUP_WV2)^2) +((b5_CT_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,3] <- distCRUP_CT_WV2_GREENxRED

distCRUP_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CRUP_WV2)^2) +((b5_CD_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,3] <- distCRUP_CD_WV2_GREENxRED

distCRUP_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CRUP_WV2)^2) +((b5_VE_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,3] <- distCRUP_VE_WV2_GREENxRED

distCRUP_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CRUP_WV2)^2) +((b5_MG_WV2-b5_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,3] <- distCRUP_MG_WV2_GREENxRED


#
distCL_CS_WV2_GREENxRED = sqrt(((b3_CS_WV2-b3_CL_WV2)^2) +((b5_CS_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[5,4] <- distCL_CS_WV2_GREENxRED

distCL_CR_WV2_GREENxRED = sqrt(((b3_CR_WV2-b3_CL_WV2)^2) +((b5_CR_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[6,4] <- distCL_CR_WV2_GREENxRED

distCL_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CL_WV2)^2) +((b5_CT_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,4] <- distCL_CT_WV2_GREENxRED

distCL_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CL_WV2)^2) +((b5_CD_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,4] <- distCL_CD_WV2_GREENxRED

distCL_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CL_WV2)^2) +((b5_VE_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,4] <- distCL_VE_WV2_GREENxRED

distCL_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CL_WV2)^2) +((b5_MG_WV2-b5_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,4] <- distCL_MG_WV2_GREENxRED


#
distCS_CR_WV2_GREENxRED = sqrt(((b3_CR_WV2-b3_CS_WV2)^2) +((b5_CR_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[6,5] <- distCS_CR_WV2_GREENxRED

distCS_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CS_WV2)^2) +((b5_CT_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,5] <- distCS_CT_WV2_GREENxRED

distCS_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CS_WV2)^2) +((b5_CD_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,5] <- distCS_CD_WV2_GREENxRED

distCS_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CS_WV2)^2) +((b5_VE_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,5] <- distCS_VE_WV2_GREENxRED

distCS_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CS_WV2)^2) +((b5_MG_WV2-b5_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,5] <- distCS_MG_WV2_GREENxRED


#
distCR_CT_WV2_GREENxRED = sqrt(((b3_CT_WV2-b3_CR_WV2)^2) +((b5_CT_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[7,6] <- distCR_CT_WV2_GREENxRED

distCR_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CR_WV2)^2) +((b5_CD_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,6] <- distCR_CD_WV2_GREENxRED

distCR_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CR_WV2)^2) +((b5_VE_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,6] <- distCR_VE_WV2_GREENxRED

distCR_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CR_WV2)^2) +((b5_MG_WV2-b5_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,6] <- distCR_MG_WV2_GREENxRED


#
distCT_CD_WV2_GREENxRED = sqrt(((b3_CD_WV2-b3_CT_WV2)^2) +((b5_CD_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[8,7] <- distCT_CD_WV2_GREENxRED

distCT_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CT_WV2)^2) +((b5_VE_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,7] <- distCT_VE_WV2_GREENxRED

distCT_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CT_WV2)^2) +((b5_MG_WV2-b5_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,7] <- distCT_MG_WV2_GREENxRED

#
distCD_VE_WV2_GREENxRED = sqrt(((b3_VE_WV2-b3_CD_WV2)^2) +((b5_VE_WV2-b5_CD_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[9,8] <- distCD_VE_WV2_GREENxRED

distCD_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_CD_WV2)^2) +((b5_MG_WV2-b5_CD_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,8] <- distCD_MG_WV2_GREENxRED

#
distVE_MG_WV2_GREENxRED = sqrt(((b3_MG_WV2-b3_VE_WV2)^2) +((b5_MG_WV2-b5_VE_WV2)^2))
Matriz_distancias_WV2_N4_GREENxRED[10,9] <- distVE_MG_WV2_GREENxRED


#LAND8

distCLU_CLUCM_LAND8_GREENxRED = sqrt(((b3_CLUCM_LAND8-b3_CLU_LAND8)^2) +((b4_CLUCM_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[2,1] <- distCLU_CLUCM_LAND8_GREENxRED

distCLU_CRUP_LAND8_GREENxRED = sqrt(((b3_CRUP_LAND8-b3_CLU_LAND8)^2) +((b4_CRUP_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[3,1] <- distCLU_CRUP_LAND8_GREENxRED

distCLU_CL_LAND8_GREENxRED = sqrt(((b3_CL_LAND8-b3_CLU_LAND8)^2) +((b4_CL_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[4,1] <- distCLU_CL_LAND8_GREENxRED

distCLU_CS_LAND8_GREENxRED = sqrt(((b3_CS_LAND8-b3_CLU_LAND8)^2) +((b4_CS_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[5,1] <- distCLU_CS_LAND8_GREENxRED

distCLU_CR_LAND8_GREENxRED = sqrt(((b3_CR_LAND8-b3_CLU_LAND8)^2) +((b4_CR_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[6,1] <- distCLU_CR_LAND8_GREENxRED

distCLU_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CLU_LAND8)^2) +((b4_CT_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,1] <- distCLU_CT_LAND8_GREENxRED

distCLU_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CLU_LAND8)^2) +((b4_CD_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,1] <- distCLU_CD_LAND8_GREENxRED

distCLU_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CLU_LAND8)^2) +((b4_VE_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,1] <- distCLU_VE_LAND8_GREENxRED

distCLU_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CLU_LAND8)^2) +((b4_MG_LAND8-b4_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,1] <- distCLU_MG_LAND8_GREENxRED


#
distCLUCM_CRUP_LAND8_GREENxRED = sqrt(((b3_CRUP_LAND8-b3_CLUCM_LAND8)^2) +((b4_CRUP_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[3,2] <- distCLUCM_CRUP_LAND8_GREENxRED

distCLUCM_CL_LAND8_GREENxRED = sqrt(((b3_CL_LAND8-b3_CLUCM_LAND8)^2) +((b4_CL_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[4,2] <- distCLUCM_CL_LAND8_GREENxRED

distCLUCM_CS_LAND8_GREENxRED = sqrt(((b3_CS_LAND8-b3_CLUCM_LAND8)^2) +((b4_CS_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[5,2] <- distCLUCM_CS_LAND8_GREENxRED

distCLUCM_CR_LAND8_GREENxRED = sqrt(((b3_CR_LAND8-b3_CLUCM_LAND8)^2) +((b4_CR_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[6,2] <- distCLUCM_CR_LAND8_GREENxRED

distCLUCM_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CLUCM_LAND8)^2) +((b4_CT_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,2] <- distCLUCM_CT_LAND8_GREENxRED

distCLUCM_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CLUCM_LAND8)^2) +((b4_CD_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,2] <- distCLUCM_CD_LAND8_GREENxRED

distCLUCM_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CLUCM_LAND8)^2) +((b4_VE_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,2] <- distCLUCM_VE_LAND8_GREENxRED

distCLUCM_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CLUCM_LAND8)^2) +((b4_MG_LAND8-b4_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,2] <- distCLUCM_MG_LAND8_GREENxRED


#
distCRUP_CL_LAND8_GREENxRED = sqrt(((b3_CL_LAND8-b3_CRUP_LAND8)^2) +((b4_CL_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[4,3] <- distCRUP_CL_LAND8_GREENxRED

distCRUP_CS_LAND8_GREENxRED = sqrt(((b3_CS_LAND8-b3_CRUP_LAND8)^2) +((b4_CS_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[5,3] <- distCRUP_CS_LAND8_GREENxRED

distCRUP_CR_LAND8_GREENxRED = sqrt(((b3_CR_LAND8-b3_CRUP_LAND8)^2) +((b4_CR_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[6,3] <- distCRUP_CR_LAND8_GREENxRED

distCRUP_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CRUP_LAND8)^2) +((b4_CT_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,3] <- distCRUP_CT_LAND8_GREENxRED

distCRUP_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CRUP_LAND8)^2) +((b4_CD_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,3] <- distCRUP_CD_LAND8_GREENxRED

distCRUP_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CRUP_LAND8)^2) +((b4_VE_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,3] <- distCRUP_VE_LAND8_GREENxRED

distCRUP_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CRUP_LAND8)^2) +((b4_MG_LAND8-b4_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,3] <- distCRUP_MG_LAND8_GREENxRED


#
distCL_CS_LAND8_GREENxRED = sqrt(((b3_CS_LAND8-b3_CL_LAND8)^2) +((b4_CS_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[5,4] <- distCL_CS_LAND8_GREENxRED

distCL_CR_LAND8_GREENxRED = sqrt(((b3_CR_LAND8-b3_CL_LAND8)^2) +((b4_CR_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[6,4] <- distCL_CR_LAND8_GREENxRED

distCL_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CL_LAND8)^2) +((b4_CT_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,4] <- distCL_CT_LAND8_GREENxRED

distCL_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CL_LAND8)^2) +((b4_CD_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,4] <- distCL_CD_LAND8_GREENxRED

distCL_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CL_LAND8)^2) +((b4_VE_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,4] <- distCL_VE_LAND8_GREENxRED

distCL_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CL_LAND8)^2) +((b4_MG_LAND8-b4_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,4] <- distCL_MG_LAND8_GREENxRED


#
distCS_CR_LAND8_GREENxRED = sqrt(((b3_CR_LAND8-b3_CS_LAND8)^2) +((b4_CR_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[6,5] <- distCS_CR_LAND8_GREENxRED

distCS_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CS_LAND8)^2) +((b4_CT_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,5] <- distCS_CT_LAND8_GREENxRED

distCS_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CS_LAND8)^2) +((b4_CD_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,5] <- distCS_CD_LAND8_GREENxRED

distCS_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CS_LAND8)^2) +((b4_VE_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,5] <- distCS_VE_LAND8_GREENxRED

distCS_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CS_LAND8)^2) +((b4_MG_LAND8-b4_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,5] <- distCS_MG_LAND8_GREENxRED


#
distCR_CT_LAND8_GREENxRED = sqrt(((b3_CT_LAND8-b3_CR_LAND8)^2) +((b4_CT_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[7,6] <- distCR_CT_LAND8_GREENxRED

distCR_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CR_LAND8)^2) +((b4_CD_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,6] <- distCR_CD_LAND8_GREENxRED

distCR_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CR_LAND8)^2) +((b4_VE_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,6] <- distCR_VE_LAND8_GREENxRED

distCR_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CR_LAND8)^2) +((b4_MG_LAND8-b4_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,6] <- distCR_MG_LAND8_GREENxRED


#
distCT_CD_LAND8_GREENxRED = sqrt(((b3_CD_LAND8-b3_CT_LAND8)^2) +((b4_CD_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[8,7] <- distCT_CD_LAND8_GREENxRED

distCT_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CT_LAND8)^2) +((b4_VE_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,7] <- distCT_VE_LAND8_GREENxRED

distCT_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CT_LAND8)^2) +((b4_MG_LAND8-b4_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,7] <- distCT_MG_LAND8_GREENxRED

#
distCD_VE_LAND8_GREENxRED = sqrt(((b3_VE_LAND8-b3_CD_LAND8)^2) +((b4_VE_LAND8-b4_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[9,8] <- distCD_VE_LAND8_GREENxRED

distCD_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_CD_LAND8)^2) +((b4_MG_LAND8-b4_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,8] <- distCD_MG_LAND8_GREENxRED

#
distVE_MG_LAND8_GREENxRED = sqrt(((b3_MG_LAND8-b3_VE_LAND8)^2) +((b4_MG_LAND8-b4_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxRED[10,9] <- distVE_MG_LAND8_GREENxRED

Matriz_distancias_WV2_N4_GREENxRED <- round(Matriz_distancias_WV2_N4_GREENxRED*100, digits = 4)

Matriz_distancias_LAND8_N4_GREENxRED <- round(Matriz_distancias_LAND8_N4_GREENxRED*100, digits = 4)

#teste da matriz resultante

Matriz_result_GREENxRED = Matriz_distancias_WV2_N4_GREENxRED - Matriz_distancias_LAND8_N4_GREENxRED

Matriz_result_GREENxRED <- Matriz_result_GREENxRED*100

melted_matriz_GREENxRED <- melt(Matriz_result_GREENxRED)
head(Matriz_result_GREENxRED)

ggplot(data = melted_matriz_GREENxRED, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "RED", high = "BLUE", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))


#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b5, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b4, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Green",y = "Red")



#GREEN X NIR

Matriz_distancias_WV2_N4_GREENxNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_GREENxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_GREENxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_GREENxNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_GREENxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_GREENxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_GREENxNIR = sqrt(((b3_CLUCM_WV2-b3_CLU_WV2)^2) +((b7_CLUCM_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[2,1] <- distCLU_CLUCM_WV2_GREENxNIR

distCLU_CRUP_WV2_GREENxNIR = sqrt(((b3_CRUP_WV2-b3_CLU_WV2)^2) +((b7_CRUP_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[3,1] <- distCLU_CRUP_WV2_GREENxNIR

distCLU_CL_WV2_GREENxNIR = sqrt(((b3_CL_WV2-b3_CLU_WV2)^2) +((b7_CL_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[4,1] <- distCLU_CL_WV2_GREENxNIR

distCLU_CS_WV2_GREENxNIR = sqrt(((b3_CS_WV2-b3_CLU_WV2)^2) +((b7_CS_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[5,1] <- distCLU_CS_WV2_GREENxNIR

distCLU_CR_WV2_GREENxNIR = sqrt(((b3_CR_WV2-b3_CLU_WV2)^2) +((b7_CR_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[6,1] <- distCLU_CR_WV2_GREENxNIR

distCLU_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CLU_WV2)^2) +((b7_CT_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,1] <- distCLU_CT_WV2_GREENxNIR

distCLU_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CLU_WV2)^2) +((b7_CD_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,1] <- distCLU_CD_WV2_GREENxNIR

distCLU_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CLU_WV2)^2) +((b7_VE_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,1] <- distCLU_VE_WV2_GREENxNIR

distCLU_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CLU_WV2)^2) +((b7_MG_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,1] <- distCLU_MG_WV2_GREENxNIR


#
distCLUCM_CRUP_WV2_GREENxNIR = sqrt(((b3_CRUP_WV2-b3_CLUCM_WV2)^2) +((b7_CRUP_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[3,2] <- distCLUCM_CRUP_WV2_GREENxNIR

distCLUCM_CL_WV2_GREENxNIR = sqrt(((b3_CL_WV2-b3_CLUCM_WV2)^2) +((b7_CL_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[4,2] <- distCLUCM_CL_WV2_GREENxNIR

distCLUCM_CS_WV2_GREENxNIR = sqrt(((b3_CS_WV2-b3_CLUCM_WV2)^2) +((b7_CS_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[5,2] <- distCLUCM_CS_WV2_GREENxNIR

distCLUCM_CR_WV2_GREENxNIR = sqrt(((b3_CR_WV2-b3_CLUCM_WV2)^2) +((b7_CR_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[6,2] <- distCLUCM_CR_WV2_GREENxNIR

distCLUCM_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CLUCM_WV2)^2) +((b7_CT_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,2] <- distCLUCM_CT_WV2_GREENxNIR

distCLUCM_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CLUCM_WV2)^2) +((b7_CD_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,2] <- distCLUCM_CD_WV2_GREENxNIR

distCLUCM_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CLUCM_WV2)^2) +((b7_VE_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,2] <- distCLUCM_VE_WV2_GREENxNIR

distCLUCM_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CLUCM_WV2)^2) +((b7_MG_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,2] <- distCLUCM_MG_WV2_GREENxNIR


#
distCRUP_CL_WV2_GREENxNIR = sqrt(((b3_CL_WV2-b3_CRUP_WV2)^2) +((b7_CL_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[4,3] <- distCRUP_CL_WV2_GREENxNIR

distCRUP_CS_WV2_GREENxNIR = sqrt(((b3_CS_WV2-b3_CRUP_WV2)^2) +((b7_CS_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[5,3] <- distCRUP_CS_WV2_GREENxNIR

distCRUP_CR_WV2_GREENxNIR = sqrt(((b3_CR_WV2-b3_CRUP_WV2)^2) +((b7_CR_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[6,3] <- distCRUP_CR_WV2_GREENxNIR

distCRUP_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CRUP_WV2)^2) +((b7_CT_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,3] <- distCRUP_CT_WV2_GREENxNIR

distCRUP_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CRUP_WV2)^2) +((b7_CD_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,3] <- distCRUP_CD_WV2_GREENxNIR

distCRUP_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CRUP_WV2)^2) +((b7_VE_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,3] <- distCRUP_VE_WV2_GREENxNIR

distCRUP_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CRUP_WV2)^2) +((b7_MG_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,3] <- distCRUP_MG_WV2_GREENxNIR


#
distCL_CS_WV2_GREENxNIR = sqrt(((b3_CS_WV2-b3_CL_WV2)^2) +((b7_CS_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[5,4] <- distCL_CS_WV2_GREENxNIR

distCL_CR_WV2_GREENxNIR = sqrt(((b3_CR_WV2-b3_CL_WV2)^2) +((b7_CR_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[6,4] <- distCL_CR_WV2_GREENxNIR

distCL_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CL_WV2)^2) +((b7_CT_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,4] <- distCL_CT_WV2_GREENxNIR

distCL_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CL_WV2)^2) +((b7_CD_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,4] <- distCL_CD_WV2_GREENxNIR

distCL_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CL_WV2)^2) +((b7_VE_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,4] <- distCL_VE_WV2_GREENxNIR

distCL_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CL_WV2)^2) +((b7_MG_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,4] <- distCL_MG_WV2_GREENxNIR


#
distCS_CR_WV2_GREENxNIR = sqrt(((b3_CR_WV2-b3_CS_WV2)^2) +((b7_CR_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[6,5] <- distCS_CR_WV2_GREENxNIR

distCS_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CS_WV2)^2) +((b7_CT_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,5] <- distCS_CT_WV2_GREENxNIR

distCS_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CS_WV2)^2) +((b7_CD_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,5] <- distCS_CD_WV2_GREENxNIR

distCS_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CS_WV2)^2) +((b7_VE_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,5] <- distCS_VE_WV2_GREENxNIR

distCS_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CS_WV2)^2) +((b7_MG_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,5] <- distCS_MG_WV2_GREENxNIR


#
distCR_CT_WV2_GREENxNIR = sqrt(((b3_CT_WV2-b3_CR_WV2)^2) +((b7_CT_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[7,6] <- distCR_CT_WV2_GREENxNIR

distCR_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CR_WV2)^2) +((b7_CD_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,6] <- distCR_CD_WV2_GREENxNIR

distCR_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CR_WV2)^2) +((b7_VE_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,6] <- distCR_VE_WV2_GREENxNIR

distCR_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CR_WV2)^2) +((b7_MG_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,6] <- distCR_MG_WV2_GREENxNIR


#
distCT_CD_WV2_GREENxNIR = sqrt(((b3_CD_WV2-b3_CT_WV2)^2) +((b7_CD_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[8,7] <- distCT_CD_WV2_GREENxNIR

distCT_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CT_WV2)^2) +((b7_VE_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,7] <- distCT_VE_WV2_GREENxNIR

distCT_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CT_WV2)^2) +((b7_MG_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,7] <- distCT_MG_WV2_GREENxNIR

#
distCD_VE_WV2_GREENxNIR = sqrt(((b3_VE_WV2-b3_CD_WV2)^2) +((b7_VE_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[9,8] <- distCD_VE_WV2_GREENxNIR

distCD_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_CD_WV2)^2) +((b7_MG_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,8] <- distCD_MG_WV2_GREENxNIR

#
distVE_MG_WV2_GREENxNIR = sqrt(((b3_MG_WV2-b3_VE_WV2)^2) +((b7_MG_WV2-b7_VE_WV2)^2))
Matriz_distancias_WV2_N4_GREENxNIR[10,9] <- distVE_MG_WV2_GREENxNIR


#LAND8

distCLU_CLUCM_LAND8_GREENxNIR = sqrt(((b3_CLUCM_LAND8-b3_CLU_LAND8)^2) +((b5_CLUCM_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[2,1] <- distCLU_CLUCM_LAND8_GREENxNIR

distCLU_CRUP_LAND8_GREENxNIR = sqrt(((b3_CRUP_LAND8-b3_CLU_LAND8)^2) +((b5_CRUP_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[3,1] <- distCLU_CRUP_LAND8_GREENxNIR

distCLU_CL_LAND8_GREENxNIR = sqrt(((b3_CL_LAND8-b3_CLU_LAND8)^2) +((b5_CL_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[4,1] <- distCLU_CL_LAND8_GREENxNIR

distCLU_CS_LAND8_GREENxNIR = sqrt(((b3_CS_LAND8-b3_CLU_LAND8)^2) +((b5_CS_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[5,1] <- distCLU_CS_LAND8_GREENxNIR

distCLU_CR_LAND8_GREENxNIR = sqrt(((b3_CR_LAND8-b3_CLU_LAND8)^2) +((b5_CR_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[6,1] <- distCLU_CR_LAND8_GREENxNIR

distCLU_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CLU_LAND8)^2) +((b5_CT_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,1] <- distCLU_CT_LAND8_GREENxNIR

distCLU_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CLU_LAND8)^2) +((b5_CD_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,1] <- distCLU_CD_LAND8_GREENxNIR

distCLU_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CLU_LAND8)^2) +((b5_VE_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,1] <- distCLU_VE_LAND8_GREENxNIR

distCLU_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CLU_LAND8)^2) +((b5_MG_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,1] <- distCLU_MG_LAND8_GREENxNIR


#
distCLUCM_CRUP_LAND8_GREENxNIR = sqrt(((b3_CRUP_LAND8-b3_CLUCM_LAND8)^2) +((b5_CRUP_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[3,2] <- distCLUCM_CRUP_LAND8_GREENxNIR

distCLUCM_CL_LAND8_GREENxNIR = sqrt(((b3_CL_LAND8-b3_CLUCM_LAND8)^2) +((b5_CL_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[4,2] <- distCLUCM_CL_LAND8_GREENxNIR

distCLUCM_CS_LAND8_GREENxNIR = sqrt(((b3_CS_LAND8-b3_CLUCM_LAND8)^2) +((b5_CS_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[5,2] <- distCLUCM_CS_LAND8_GREENxNIR

distCLUCM_CR_LAND8_GREENxNIR = sqrt(((b3_CR_LAND8-b3_CLUCM_LAND8)^2) +((b5_CR_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[6,2] <- distCLUCM_CR_LAND8_GREENxNIR

distCLUCM_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CLUCM_LAND8)^2) +((b5_CT_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,2] <- distCLUCM_CT_LAND8_GREENxNIR

distCLUCM_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CLUCM_LAND8)^2) +((b5_CD_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,2] <- distCLUCM_CD_LAND8_GREENxNIR

distCLUCM_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CLUCM_LAND8)^2) +((b5_VE_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,2] <- distCLUCM_VE_LAND8_GREENxNIR

distCLUCM_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CLUCM_LAND8)^2) +((b5_MG_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,2] <- distCLUCM_MG_LAND8_GREENxNIR


#
distCRUP_CL_LAND8_GREENxNIR = sqrt(((b3_CL_LAND8-b3_CRUP_LAND8)^2) +((b5_CL_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[4,3] <- distCRUP_CL_LAND8_GREENxNIR

distCRUP_CS_LAND8_GREENxNIR = sqrt(((b3_CS_LAND8-b3_CRUP_LAND8)^2) +((b5_CS_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[5,3] <- distCRUP_CS_LAND8_GREENxNIR

distCRUP_CR_LAND8_GREENxNIR = sqrt(((b3_CR_LAND8-b3_CRUP_LAND8)^2) +((b5_CR_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[6,3] <- distCRUP_CR_LAND8_GREENxNIR

distCRUP_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CRUP_LAND8)^2) +((b5_CT_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,3] <- distCRUP_CT_LAND8_GREENxNIR

distCRUP_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CRUP_LAND8)^2) +((b5_CD_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,3] <- distCRUP_CD_LAND8_GREENxNIR

distCRUP_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CRUP_LAND8)^2) +((b5_VE_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,3] <- distCRUP_VE_LAND8_GREENxNIR

distCRUP_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CRUP_LAND8)^2) +((b5_MG_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,3] <- distCRUP_MG_LAND8_GREENxNIR


#
distCL_CS_LAND8_GREENxNIR = sqrt(((b3_CS_LAND8-b3_CL_LAND8)^2) +((b5_CS_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[5,4] <- distCL_CS_LAND8_GREENxNIR

distCL_CR_LAND8_GREENxNIR = sqrt(((b3_CR_LAND8-b3_CL_LAND8)^2) +((b5_CR_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[6,4] <- distCL_CR_LAND8_GREENxNIR

distCL_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CL_LAND8)^2) +((b5_CT_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,4] <- distCL_CT_LAND8_GREENxNIR

distCL_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CL_LAND8)^2) +((b5_CD_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,4] <- distCL_CD_LAND8_GREENxNIR

distCL_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CL_LAND8)^2) +((b5_VE_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,4] <- distCL_VE_LAND8_GREENxNIR

distCL_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CL_LAND8)^2) +((b5_MG_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,4] <- distCL_MG_LAND8_GREENxNIR


#
distCS_CR_LAND8_GREENxNIR = sqrt(((b3_CR_LAND8-b3_CS_LAND8)^2) +((b5_CR_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[6,5] <- distCS_CR_LAND8_GREENxNIR

distCS_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CS_LAND8)^2) +((b5_CT_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,5] <- distCS_CT_LAND8_GREENxNIR

distCS_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CS_LAND8)^2) +((b5_CD_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,5] <- distCS_CD_LAND8_GREENxNIR

distCS_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CS_LAND8)^2) +((b5_VE_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,5] <- distCS_VE_LAND8_GREENxNIR

distCS_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CS_LAND8)^2) +((b5_MG_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,5] <- distCS_MG_LAND8_GREENxNIR


#
distCR_CT_LAND8_GREENxNIR = sqrt(((b3_CT_LAND8-b3_CR_LAND8)^2) +((b5_CT_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[7,6] <- distCR_CT_LAND8_GREENxNIR

distCR_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CR_LAND8)^2) +((b5_CD_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,6] <- distCR_CD_LAND8_GREENxNIR

distCR_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CR_LAND8)^2) +((b5_VE_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,6] <- distCR_VE_LAND8_GREENxNIR

distCR_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CR_LAND8)^2) +((b5_MG_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,6] <- distCR_MG_LAND8_GREENxNIR


#
distCT_CD_LAND8_GREENxNIR = sqrt(((b3_CD_LAND8-b3_CT_LAND8)^2) +((b5_CD_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[8,7] <- distCT_CD_LAND8_GREENxNIR

distCT_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CT_LAND8)^2) +((b5_VE_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,7] <- distCT_VE_LAND8_GREENxNIR

distCT_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CT_LAND8)^2) +((b5_MG_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,7] <- distCT_MG_LAND8_GREENxNIR

#
distCD_VE_LAND8_GREENxNIR = sqrt(((b3_VE_LAND8-b3_CD_LAND8)^2) +((b5_VE_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[9,8] <- distCD_VE_LAND8_GREENxNIR

distCD_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_CD_LAND8)^2) +((b5_MG_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,8] <- distCD_MG_LAND8_GREENxNIR

#
distVE_MG_LAND8_GREENxNIR = sqrt(((b3_MG_LAND8-b3_VE_LAND8)^2) +((b5_MG_LAND8-b5_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_GREENxNIR[10,9] <- distVE_MG_LAND8_GREENxNIR

Matriz_distancias_WV2_N4_GREENxNIR <- round(Matriz_distancias_WV2_N4_GREENxNIR*100, digits = 4)

Matriz_distancias_LAND8_N4_GREENxNIR <- round(Matriz_distancias_LAND8_N4_GREENxNIR*100, digits = 4)

#teste da matriz resultante

Matriz_result_GREENxNIR = Matriz_distancias_WV2_N4_GREENxNIR - Matriz_distancias_LAND8_N4_GREENxNIR

Matriz_result_GREENxNIR <- Matriz_result_GREENxNIR*100

melted_matriz_GREENxNIR <- melt(Matriz_result_GREENxNIR)
head(Matriz_result_GREENxNIR)

ggplot(data = melted_matriz_GREENxNIR, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "RED", high = "BLUE", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))




#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b3, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b3, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Green",y = "NIR")


#RED X NIR

Matriz_distancias_WV2_N4_REDxNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_N4_REDxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_N4_REDxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")

Matriz_distancias_LAND8_N4_REDxNIR <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_LAND8_N4_REDxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_LAND8_N4_REDxNIR) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")



#WV2

distCLU_CLUCM_WV2_REDxNIR = sqrt(((b5_CLUCM_WV2-b5_CLU_WV2)^2) +((b7_CLUCM_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[2,1] <- distCLU_CLUCM_WV2_REDxNIR

distCLU_CRUP_WV2_REDxNIR = sqrt(((b5_CRUP_WV2-b5_CLU_WV2)^2) +((b7_CRUP_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[3,1] <- distCLU_CRUP_WV2_REDxNIR

distCLU_CL_WV2_REDxNIR = sqrt(((b5_CL_WV2-b5_CLU_WV2)^2) +((b7_CL_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[4,1] <- distCLU_CL_WV2_REDxNIR

distCLU_CS_WV2_REDxNIR = sqrt(((b5_CS_WV2-b5_CLU_WV2)^2) +((b7_CS_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[5,1] <- distCLU_CS_WV2_REDxNIR

distCLU_CR_WV2_REDxNIR = sqrt(((b5_CR_WV2-b5_CLU_WV2)^2) +((b7_CR_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[6,1] <- distCLU_CR_WV2_REDxNIR

distCLU_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CLU_WV2)^2) +((b7_CT_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,1] <- distCLU_CT_WV2_REDxNIR

distCLU_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CLU_WV2)^2) +((b7_CD_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,1] <- distCLU_CD_WV2_REDxNIR

distCLU_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CLU_WV2)^2) +((b7_VE_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,1] <- distCLU_VE_WV2_REDxNIR

distCLU_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CLU_WV2)^2) +((b7_MG_WV2-b7_CLU_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,1] <- distCLU_MG_WV2_REDxNIR


#
distCLUCM_CRUP_WV2_REDxNIR = sqrt(((b5_CRUP_WV2-b5_CLUCM_WV2)^2) +((b7_CRUP_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[3,2] <- distCLUCM_CRUP_WV2_REDxNIR

distCLUCM_CL_WV2_REDxNIR = sqrt(((b5_CL_WV2-b5_CLUCM_WV2)^2) +((b7_CL_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[4,2] <- distCLUCM_CL_WV2_REDxNIR

distCLUCM_CS_WV2_REDxNIR = sqrt(((b5_CS_WV2-b5_CLUCM_WV2)^2) +((b7_CS_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[5,2] <- distCLUCM_CS_WV2_REDxNIR

distCLUCM_CR_WV2_REDxNIR = sqrt(((b5_CR_WV2-b5_CLUCM_WV2)^2) +((b7_CR_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[6,2] <- distCLUCM_CR_WV2_REDxNIR

distCLUCM_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CLUCM_WV2)^2) +((b7_CT_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,2] <- distCLUCM_CT_WV2_REDxNIR

distCLUCM_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CLUCM_WV2)^2) +((b7_CD_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,2] <- distCLUCM_CD_WV2_REDxNIR

distCLUCM_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CLUCM_WV2)^2) +((b7_VE_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,2] <- distCLUCM_VE_WV2_REDxNIR

distCLUCM_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CLUCM_WV2)^2) +((b7_MG_WV2-b7_CLUCM_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,2] <- distCLUCM_MG_WV2_REDxNIR


#
distCRUP_CL_WV2_REDxNIR = sqrt(((b5_CL_WV2-b5_CRUP_WV2)^2) +((b7_CL_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[4,3] <- distCRUP_CL_WV2_REDxNIR

distCRUP_CS_WV2_REDxNIR = sqrt(((b5_CS_WV2-b5_CRUP_WV2)^2) +((b7_CS_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[5,3] <- distCRUP_CS_WV2_REDxNIR

distCRUP_CR_WV2_REDxNIR = sqrt(((b5_CR_WV2-b5_CRUP_WV2)^2) +((b7_CR_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[6,3] <- distCRUP_CR_WV2_REDxNIR

distCRUP_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CRUP_WV2)^2) +((b7_CT_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,3] <- distCRUP_CT_WV2_REDxNIR

distCRUP_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CRUP_WV2)^2) +((b7_CD_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,3] <- distCRUP_CD_WV2_REDxNIR

distCRUP_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CRUP_WV2)^2) +((b7_VE_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,3] <- distCRUP_VE_WV2_REDxNIR

distCRUP_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CRUP_WV2)^2) +((b7_MG_WV2-b7_CRUP_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,3] <- distCRUP_MG_WV2_REDxNIR


#
distCL_CS_WV2_REDxNIR = sqrt(((b5_CS_WV2-b5_CL_WV2)^2) +((b7_CS_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[5,4] <- distCL_CS_WV2_REDxNIR

distCL_CR_WV2_REDxNIR = sqrt(((b5_CR_WV2-b5_CL_WV2)^2) +((b7_CR_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[6,4] <- distCL_CR_WV2_REDxNIR

distCL_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CL_WV2)^2) +((b7_CT_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,4] <- distCL_CT_WV2_REDxNIR

distCL_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CL_WV2)^2) +((b7_CD_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,4] <- distCL_CD_WV2_REDxNIR

distCL_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CL_WV2)^2) +((b7_VE_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,4] <- distCL_VE_WV2_REDxNIR

distCL_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CL_WV2)^2) +((b7_MG_WV2-b7_CL_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,4] <- distCL_MG_WV2_REDxNIR


#
distCS_CR_WV2_REDxNIR = sqrt(((b5_CR_WV2-b5_CS_WV2)^2) +((b7_CR_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[6,5] <- distCS_CR_WV2_REDxNIR

distCS_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CS_WV2)^2) +((b7_CT_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,5] <- distCS_CT_WV2_REDxNIR

distCS_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CS_WV2)^2) +((b7_CD_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,5] <- distCS_CD_WV2_REDxNIR

distCS_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CS_WV2)^2) +((b7_VE_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,5] <- distCS_VE_WV2_REDxNIR

distCS_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CS_WV2)^2) +((b7_MG_WV2-b7_CS_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,5] <- distCS_MG_WV2_REDxNIR


#
distCR_CT_WV2_REDxNIR = sqrt(((b5_CT_WV2-b5_CR_WV2)^2) +((b7_CT_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[7,6] <- distCR_CT_WV2_REDxNIR

distCR_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CR_WV2)^2) +((b7_CD_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,6] <- distCR_CD_WV2_REDxNIR

distCR_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CR_WV2)^2) +((b7_VE_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,6] <- distCR_VE_WV2_REDxNIR

distCR_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CR_WV2)^2) +((b7_MG_WV2-b7_CR_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,6] <- distCR_MG_WV2_REDxNIR


#
distCT_CD_WV2_REDxNIR = sqrt(((b5_CD_WV2-b5_CT_WV2)^2) +((b7_CD_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[8,7] <- distCT_CD_WV2_REDxNIR

distCT_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CT_WV2)^2) +((b7_VE_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,7] <- distCT_VE_WV2_REDxNIR

distCT_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CT_WV2)^2) +((b7_MG_WV2-b7_CT_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,7] <- distCT_MG_WV2_REDxNIR

#
distCD_VE_WV2_REDxNIR = sqrt(((b5_VE_WV2-b5_CD_WV2)^2) +((b7_VE_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[9,8] <- distCD_VE_WV2_REDxNIR

distCD_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_CD_WV2)^2) +((b7_MG_WV2-b7_CD_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,8] <- distCD_MG_WV2_REDxNIR

#
distVE_MG_WV2_REDxNIR = sqrt(((b5_MG_WV2-b5_VE_WV2)^2) +((b7_MG_WV2-b7_VE_WV2)^2))
Matriz_distancias_WV2_N4_REDxNIR[10,9] <- distVE_MG_WV2_REDxNIR


#LAND8

distCLU_CLUCM_LAND8_REDxNIR = sqrt(((b4_CLUCM_LAND8-b4_CLU_LAND8)^2) +((b5_CLUCM_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[2,1] <- distCLU_CLUCM_LAND8_REDxNIR

distCLU_CRUP_LAND8_REDxNIR = sqrt(((b4_CRUP_LAND8-b4_CLU_LAND8)^2) +((b5_CRUP_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[3,1] <- distCLU_CRUP_LAND8_REDxNIR

distCLU_CL_LAND8_REDxNIR = sqrt(((b4_CL_LAND8-b4_CLU_LAND8)^2) +((b5_CL_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[4,1] <- distCLU_CL_LAND8_REDxNIR

distCLU_CS_LAND8_REDxNIR = sqrt(((b4_CS_LAND8-b4_CLU_LAND8)^2) +((b5_CS_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[5,1] <- distCLU_CS_LAND8_REDxNIR

distCLU_CR_LAND8_REDxNIR = sqrt(((b4_CR_LAND8-b4_CLU_LAND8)^2) +((b5_CR_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[6,1] <- distCLU_CR_LAND8_REDxNIR

distCLU_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CLU_LAND8)^2) +((b5_CT_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,1] <- distCLU_CT_LAND8_REDxNIR

distCLU_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CLU_LAND8)^2) +((b5_CD_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,1] <- distCLU_CD_LAND8_REDxNIR

distCLU_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CLU_LAND8)^2) +((b5_VE_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,1] <- distCLU_VE_LAND8_REDxNIR

distCLU_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CLU_LAND8)^2) +((b5_MG_LAND8-b5_CLU_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,1] <- distCLU_MG_LAND8_REDxNIR


#
distCLUCM_CRUP_LAND8_REDxNIR = sqrt(((b4_CRUP_LAND8-b4_CLUCM_LAND8)^2) +((b5_CRUP_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[3,2] <- distCLUCM_CRUP_LAND8_REDxNIR

distCLUCM_CL_LAND8_REDxNIR = sqrt(((b4_CL_LAND8-b4_CLUCM_LAND8)^2) +((b5_CL_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[4,2] <- distCLUCM_CL_LAND8_REDxNIR

distCLUCM_CS_LAND8_REDxNIR = sqrt(((b4_CS_LAND8-b4_CLUCM_LAND8)^2) +((b5_CS_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[5,2] <- distCLUCM_CS_LAND8_REDxNIR

distCLUCM_CR_LAND8_REDxNIR = sqrt(((b4_CR_LAND8-b4_CLUCM_LAND8)^2) +((b5_CR_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[6,2] <- distCLUCM_CR_LAND8_REDxNIR

distCLUCM_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CLUCM_LAND8)^2) +((b5_CT_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,2] <- distCLUCM_CT_LAND8_REDxNIR

distCLUCM_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CLUCM_LAND8)^2) +((b5_CD_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,2] <- distCLUCM_CD_LAND8_REDxNIR

distCLUCM_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CLUCM_LAND8)^2) +((b5_VE_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,2] <- distCLUCM_VE_LAND8_REDxNIR

distCLUCM_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CLUCM_LAND8)^2) +((b5_MG_LAND8-b5_CLUCM_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,2] <- distCLUCM_MG_LAND8_REDxNIR


#
distCRUP_CL_LAND8_REDxNIR = sqrt(((b4_CL_LAND8-b4_CRUP_LAND8)^2) +((b5_CL_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[4,3] <- distCRUP_CL_LAND8_REDxNIR

distCRUP_CS_LAND8_REDxNIR = sqrt(((b4_CS_LAND8-b4_CRUP_LAND8)^2) +((b5_CS_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[5,3] <- distCRUP_CS_LAND8_REDxNIR

distCRUP_CR_LAND8_REDxNIR = sqrt(((b4_CR_LAND8-b4_CRUP_LAND8)^2) +((b5_CR_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[6,3] <- distCRUP_CR_LAND8_REDxNIR

distCRUP_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CRUP_LAND8)^2) +((b5_CT_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,3] <- distCRUP_CT_LAND8_REDxNIR

distCRUP_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CRUP_LAND8)^2) +((b5_CD_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,3] <- distCRUP_CD_LAND8_REDxNIR

distCRUP_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CRUP_LAND8)^2) +((b5_VE_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,3] <- distCRUP_VE_LAND8_REDxNIR

distCRUP_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CRUP_LAND8)^2) +((b5_MG_LAND8-b5_CRUP_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,3] <- distCRUP_MG_LAND8_REDxNIR


#
distCL_CS_LAND8_REDxNIR = sqrt(((b4_CS_LAND8-b4_CL_LAND8)^2) +((b5_CS_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[5,4] <- distCL_CS_LAND8_REDxNIR

distCL_CR_LAND8_REDxNIR = sqrt(((b4_CR_LAND8-b4_CL_LAND8)^2) +((b5_CR_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[6,4] <- distCL_CR_LAND8_REDxNIR

distCL_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CL_LAND8)^2) +((b5_CT_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,4] <- distCL_CT_LAND8_REDxNIR

distCL_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CL_LAND8)^2) +((b5_CD_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,4] <- distCL_CD_LAND8_REDxNIR

distCL_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CL_LAND8)^2) +((b5_VE_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,4] <- distCL_VE_LAND8_REDxNIR

distCL_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CL_LAND8)^2) +((b5_MG_LAND8-b5_CL_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,4] <- distCL_MG_LAND8_REDxNIR


#
distCS_CR_LAND8_REDxNIR = sqrt(((b4_CR_LAND8-b4_CS_LAND8)^2) +((b5_CR_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[6,5] <- distCS_CR_LAND8_REDxNIR

distCS_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CS_LAND8)^2) +((b5_CT_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,5] <- distCS_CT_LAND8_REDxNIR

distCS_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CS_LAND8)^2) +((b5_CD_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,5] <- distCS_CD_LAND8_REDxNIR

distCS_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CS_LAND8)^2) +((b5_VE_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,5] <- distCS_VE_LAND8_REDxNIR

distCS_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CS_LAND8)^2) +((b5_MG_LAND8-b5_CS_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,5] <- distCS_MG_LAND8_REDxNIR


#
distCR_CT_LAND8_REDxNIR = sqrt(((b4_CT_LAND8-b4_CR_LAND8)^2) +((b5_CT_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[7,6] <- distCR_CT_LAND8_REDxNIR

distCR_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CR_LAND8)^2) +((b5_CD_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,6] <- distCR_CD_LAND8_REDxNIR

distCR_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CR_LAND8)^2) +((b5_VE_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,6] <- distCR_VE_LAND8_REDxNIR

distCR_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CR_LAND8)^2) +((b5_MG_LAND8-b5_CR_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,6] <- distCR_MG_LAND8_REDxNIR


#
distCT_CD_LAND8_REDxNIR = sqrt(((b4_CD_LAND8-b4_CT_LAND8)^2) +((b5_CD_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[8,7] <- distCT_CD_LAND8_REDxNIR

distCT_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CT_LAND8)^2) +((b5_VE_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,7] <- distCT_VE_LAND8_REDxNIR

distCT_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CT_LAND8)^2) +((b5_MG_LAND8-b5_CT_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,7] <- distCT_MG_LAND8_REDxNIR

#
distCD_VE_LAND8_REDxNIR = sqrt(((b4_VE_LAND8-b4_CD_LAND8)^2) +((b5_VE_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[9,8] <- distCD_VE_LAND8_REDxNIR

distCD_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_CD_LAND8)^2) +((b5_MG_LAND8-b5_CD_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,8] <- distCD_MG_LAND8_REDxNIR

#
distVE_MG_LAND8_REDxNIR = sqrt(((b4_MG_LAND8-b4_VE_LAND8)^2) +((b5_MG_LAND8-b5_VE_LAND8)^2))
Matriz_distancias_LAND8_N4_REDxNIR[10,9] <- distVE_MG_LAND8_REDxNIR

Matriz_distancias_WV2_N4_REDxNIR <- round(Matriz_distancias_WV2_N4_REDxNIR*100, digits = 4)

Matriz_distancias_LAND8_N4_REDxNIR <- round(Matriz_distancias_LAND8_N4_REDxNIR*100, digits = 4)

#teste da matriz resultante

Matriz_result_REDxNIR = Matriz_distancias_WV2_N4_REDxNIR - Matriz_distancias_LAND8_N4_REDxNIR

Matriz_result_REDxNIR <- Matriz_result_REDxNIR*100

melted_matriz_REDxNIR <- melt(Matriz_result_REDxNIR)
head(Matriz_result_REDxNIR)

ggplot(data = melted_matriz_REDxNIR, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "RED", high = "BLUE", limits=c(0,2.84), breaks=c(0,0.5,1.0,1.5,2.0,2.5)) + 
  geom_text(aes(label = round(value, digits = 4)), size =5) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distbncias"))


#Plots das dos pontos midios dos dois sensores

###ARRUMAR OS PCHS

ggplot() + 
  geom_point(aes(x=medias_WV2_Nivel4$Mean_b5, y=medias_WV2_Nivel4$Mean_b7, 
                 shape=factor(medias_WV2_Nivel4$Nivel4), 
                 colour=factor(medias_WV2_Nivel4$Nivel4)),size=5) +
  geom_point(aes(x=medias_LAND8_Nivel4$Mean_b4, y=medias_LAND8_Nivel4$Mean_b5, 
                 shape=factor(0:9), 
                 colour=factor(0:9)),size=5) +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0,1,2,5,0,1,2,5,0,1,15,16,17,18,15,16,17,18,15,16)) +
  scale_color_manual(values=c("#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF", "#996699", "#0000FF", "#330033","#FF0000", "#FF9933", "#339966", "#FFFF33", "#66FF66", "#336600", "#FF99FF")) +
  labs(x = "Red",y = "NIR")

