# RODAR AS FUNCOES DO WV2 E LANDSAT8 SEPARADAMENTE

#Calculo de mahalanobis para os meus dados do LANDSAT8

getwd()
setwd("C:/Users/Cesare/Desktop/Scripts_R_final/Scripts_R_final/Mahalanobis/Landsat8/Nivel4")



library(ggplot2)
library(ggcorrplot)
library(GGally)
library(matlib)
library(reshape2)

#inserindo os dados de cada classe (pois precisa da matriz de covariancia para cada classe)

CLU <- read.csv(file = "WV2_TC_CLU.csv", header = TRUE, sep = ";")
CLUCM <- read.csv(file = "WV2_TC_CLUCM.csv", header = TRUE, sep = ";")
CL <- read.csv(file = "WV2_TC_CL.csv", header = TRUE, sep = ";")
CS <- read.csv(file = "WV2_TC_CS.csv", header = TRUE, sep = ";")
CRUP <- read.csv(file = "WV2_TC_CRUP.csv", header = TRUE, sep = ";")
CR <- read.csv(file = "WV2_TC_CR.csv", header = TRUE, sep = ";")
CT <- read.csv(file = "WV2_TC_CT.csv", header = TRUE, sep = ";")
CD <- read.csv(file = "WV2_TC_CD.csv", header = TRUE, sep = ";")
VE <- read.csv(file = "WV2_TC_VE.csv", header = TRUE, sep = ";")
MG <- read.csv(file = "WV2_TC_MG.csv", header = TRUE, sep = ";")

CLUCM <- CLUCM[,c(2,3)]
CLU <- CLU[,c(2,3)]
CL <- CL[,c(2,3)]
CS <- CS[,c(2,3)]
CRUP <- CRUP[,c(2,3)]
CR <- CR[,c(2,3)]
CT <- CT[,c(2,3)]
CD <- CD[,c(2,3)]
VE <- VE[,c(2,3)]
MG <- MG[,c(2,3)]


# tirando as medias e calculando os conjuntos deslocados

mean_CLU<-colMeans(CLU[1:2])
mean_CLUCM<-colMeans(CLUCM[1:2])
mean_CL<-colMeans(CL[1:2])
mean_CS<-colMeans(CS[1:2])
mean_CRUP<-colMeans(CRUP[1:2])
mean_CR<-colMeans(CR[1:2])
mean_CT<-colMeans(CT[1:2])
mean_CD<-colMeans(CD[1:2])
mean_VE<-colMeans(VE[1:2])
mean_MG<-colMeans(MG[1:2])

#matriz inicial menos a media

CLU1 <- CLU[,1] - mean_CLU[1]
CLU2 <- CLU[,2] - mean_CLU[2]

CLUCM1 <- CLUCM[,1] - mean_CLUCM[1]
CLUCM2 <- CLUCM[,2] - mean_CLUCM[2]

CL1 <- CL[,1] - mean_CL[1]
CL2 <- CL[,2] - mean_CL[2]

CS1 <- CS[,1] - mean_CS[1]
CS2 <- CS[,2] - mean_CS[2]

CRUP1 <- CRUP[,1] - mean_CRUP[1]
CRUP2 <- CRUP[,2] - mean_CRUP[2]

CR1 <- CR[,1] - mean_CR[1]
CR2 <- CR[,2] - mean_CR[2]

CT1 <- CT[,1] - mean_CT[1]
CT2 <- CT[,2] - mean_CT[2]

CD1 <- CD[,1] - mean_CD[1]
CD2 <- CD[,2] - mean_CD[2]

VE1 <- VE[,1] - mean_VE[1]
VE2 <- VE[,2] - mean_VE[2]

MG1 <- MG[,1] - mean_MG[1]
MG2 <- MG[,2] - mean_MG[2]

#matrizes deslocadas finais

CLUfinal <- matrix(c(CL1,CL2),ncol = 2)
CLUCMfinal <- matrix(c(CL1,CL2),ncol = 2)
CLfinal <- matrix(c(CL1,CL2),ncol = 2)
CSfinal <- matrix(c(CS1,CS2),ncol = 2)
CRUPfinal <- matrix(c(CRUP1,CRUP2),ncol = 2)
CRfinal <- matrix(c(CR1,CR2),ncol = 2)
CTfinal <- matrix(c(CT1,CT2),ncol = 2)
CDfinal <- matrix(c(CD1,CD2),ncol = 2)
VEfinal <- matrix(c(VE1,VE2),ncol = 2)
MGfinal <- matrix(c(MG1,MG2),ncol = 2)


#diferenca das medias para usar na formula de mahalanobis

DiffCLUCLUCM <- matrix(c(mean_CLU- mean_CLUCM))
DiffCLUCL <- matrix(c(mean_CLU- mean_CL))
DiffCLUCS <- matrix(c(mean_CLU- mean_CS))
DiffCLUCRUP <- matrix(c(mean_CLU- mean_CRUP))
DiffCLUCR <- matrix(c(mean_CLU- mean_CR))
DiffCLUCT <- matrix(c(mean_CLU- mean_CT))
DiffCLUCD <- matrix(c(mean_CLU- mean_CD))
DiffCLUVE <- matrix(c(mean_CLU- mean_VE))
DiffCLUMG <- matrix(c(mean_CLU- mean_MG))



DiffCLUCMCL <- matrix(c(mean_CLUCM- mean_CL))
DiffCLUCMCS <- matrix(c(mean_CLUCM- mean_CS))
DiffCLUCMCRUP <- matrix(c(mean_CLUCM- mean_CRUP))
DiffCLUCMCR <- matrix(c(mean_CLUCM- mean_CR))
DiffCLUCMCT <- matrix(c(mean_CLUCM- mean_CT))
DiffCLUCMCD <- matrix(c(mean_CLUCM- mean_CD))
DiffCLUCMVE <- matrix(c(mean_CLUCM- mean_VE))
DiffCLUCMMG <- matrix(c(mean_CLUCM- mean_MG))


DiffCLCS <- matrix(c(mean_CL- mean_CS))
DiffCLCRUP <- matrix(c(mean_CL- mean_CRUP))
DiffCLCR <- matrix(c(mean_CL- mean_CR))
DiffCLCT <- matrix(c(mean_CL- mean_CT))
DiffCLCD <- matrix(c(mean_CL- mean_CD))
DiffCLVE <- matrix(c(mean_CL- mean_VE))
DiffCLMG <- matrix(c(mean_CL- mean_MG))

DiffCSCRUP <- matrix(c(mean_CS- mean_CRUP))
DiffCSCR <- matrix(c(mean_CS- mean_CR))
DiffCSCT <- matrix(c(mean_CS- mean_CT))
DiffCSCD <- matrix(c(mean_CS- mean_CD))
DiffCSVE <- matrix(c(mean_CS- mean_VE))
DiffCSMG <- matrix(c(mean_CS- mean_MG))

DiffCRUPCR <- matrix(c(mean_CRUP- mean_CR))
DiffCRUPCT <- matrix(c(mean_CRUP- mean_CT))
DiffCRUPCD <- matrix(c(mean_CRUP- mean_CD))
DiffCRUPVE <- matrix(c(mean_CRUP- mean_VE))
DiffCRUPMG <- matrix(c(mean_CRUP- mean_MG))

DiffCRCT <- matrix(c(mean_CR- mean_CT))
DiffCRCD <- matrix(c(mean_CR- mean_CD))
DiffCRVE <- matrix(c(mean_CR- mean_VE))
DiffCRMG <- matrix(c(mean_CR- mean_MG))

DiffCTCD <- matrix(c(mean_CT- mean_CD))
DiffCTVE <- matrix(c(mean_CT- mean_VE))
DiffCTMG <- matrix(c(mean_CT- mean_MG))

DiffCDVE <- matrix(c(mean_CD- mean_VE))
DiffCDMG <- matrix(c(mean_CD- mean_MG))

DiffVEMG <- matrix(c(mean_VE- mean_MG))



# contagem dos dados para a pooled covariance matrix

nCLU <- nrow(CLUfinal)
nCLUCM <- nrow(CLUCMfinal)
nCL <- nrow(CLfinal)
nCS <- nrow(CSfinal)
nCRUP <- nrow(CRUPfinal)
nCR <- nrow(CRfinal)
nCT <- nrow(CTfinal)
nCD <- nrow(CDfinal)
nVE <- nrow(VEfinal)
nMG <- nrow(MGfinal)


#numeros totais

ntCLUCLUCM <- nCLU+nCLUCM
ntCLUCL <- nCLU+nCL
ntCLUCS <- nCLU+nCS
ntCLUCRUP <- nCLU+nCRUP
ntCLUCR <- nCLU+nCR
ntCLUCT <- nCLU+nCT
ntCLUCD <- nCLU+nCD
ntCLUVE <- nCLU+nVE
ntCLUMG <- nCLU+nMG

ntCLUCMCL <- nCLUCM+nCL
ntCLUCMCS <- nCLUCM+nCS
ntCLUCMCRUP <- nCLUCM+nCRUP
ntCLUCMCR <- nCLUCM+nCR
ntCLUCMCT <- nCLUCM+nCT
ntCLUCMCD <- nCLUCM+nCD
ntCLUCMVE <- nCLUCM+nVE
ntCLUCMMG <- nCLUCM+nMG

ntCLCS <- nCL+nCS
ntCLCRUP <- nCL+nCRUP
ntCLCR <- nCL+nCR
ntCLCT <- nCL+nCT
ntCLCD <- nCL+nCD
ntCLVE <- nCL+nVE
ntCLMG <- nCL+nMG

ntCSCRUP <- nCS+nCRUP
ntCSCR <- nCS+nCR
ntCSCT <- nCS+nCT
ntCSCD <- nCS+nCD
ntCSVE <- nCS+nVE
ntCSMG <- nCS+nMG

ntCRUPCR <- nCRUP+nCR
ntCRUPCT <- nCRUP+nCT
ntCRUPCD <- nCRUP+nCD
ntCRUPVE <- nCRUP+nVE
ntCRUPMG <- nCRUP+nMG


ntCRCT <- nCR+nCT
ntCRCD <- nCR+nCD
ntCRVE <- nCR+nVE
ntCRMG <- nCR+nMG

ntCTCD <- nCT+nCD
ntCTVE <- nCT+nVE
ntCTMG <- nCT+nMG

ntCDVE <- nCD+nVE
ntCDMG <- nCD+nMG

ntVEMG <- nVE+nMG


# matrizes de covariancia

COVCLU <- cov.wt(CLUfinal, method = "ML") $cov
COVCLUCM <- cov.wt(CLUCMfinal, method = "ML") $cov
COVCL <- cov.wt(CLfinal, method = "ML") $cov
COVCS <- cov.wt(CSfinal, method = "ML") $cov
COVCRUP <- cov.wt(CRUPfinal, method = "ML") $cov
COVCR <- cov.wt(CRfinal, method = "ML") $cov
COVCT <- cov.wt(CTfinal, method = "ML") $cov
COVCD <- cov.wt(CDfinal, method = "ML") $cov
COVVE <- cov.wt(VEfinal, method = "ML") $cov
COVMG <- cov.wt(MGfinal, method = "ML") $cov


# pooled covariance


PooledCLUCCLUCM <- COVCLU * (nCLU/ntCLUCLUCM) + COVCLUCM * (nCLUCM/ntCLUCLUCM)
PooledCLUCL <- COVCLU * (nCLU/ntCLUCL) + COVCL * (nCL/ntCLUCL)
PooledCLUCS <- COVCLU * (nCLU/ntCLUCS) + COVCS * (nCS/ntCLUCS)
PooledCLUCRUP <- COVCLU * (nCLU/ntCLUCRUP) + COVCRUP * (nCS/ntCLUCRUP)
PooledCLUCR <- COVCLU * (nCLU/ntCLUCR) + COVCR * (nCR/ntCLUCR)
PooledCLUCT <- COVCLU * (nCLU/ntCLUCT) + COVCR * (nCT/ntCLUCT)
PooledCLUCD <- COVCLU * (nCLU/ntCLUCD) + COVCR * (nCD/ntCLUCD)
PooledCLUVE <- COVCLU * (nCLU/ntCLUVE) + COVVE * (nVE/ntCLUVE)
PooledCLUMG <- COVCLU * (nCLU/ntCLUMG) + COVMG * (nMG/ntCLUMG)

PooledCLUCMCL <- COVCLUCM * (nCLUCM/ntCLUCMCL) + COVCL * (nCL/ntCLUCMCL)
PooledCLUCMCS <- COVCLUCM * (nCLUCM/ntCLUCMCS) + COVCS * (nCS/ntCLUCMCS)
PooledCLUCMCRUP <- COVCLUCM * (nCLUCM/ntCLUCMCRUP) + COVCRUP * (nCS/ntCLUCMCRUP)
PooledCLUCMCR <- COVCLUCM * (nCLUCM/ntCLUCMCR) + COVCR * (nCR/ntCLUCMCR)
PooledCLUCMCT <- COVCLUCM * (nCLUCM/ntCLUCMCT) + COVCR * (nCT/ntCLUCMCT)
PooledCLUCMCD <- COVCLUCM * (nCLUCM/ntCLUCMCD) + COVCR * (nCD/ntCLUCMCD)
PooledCLUCMVE <- COVCLUCM * (nCLUCM/ntCLUCMVE) + COVVE * (nVE/ntCLUCMVE)
PooledCLUCMMG <- COVCLUCM * (nCLUCM/ntCLUCMMG) + COVMG * (nMG/ntCLUCMMG)

PooledCLCS <- COVCL * (nCL/ntCLCS) + COVCS * (nCS/ntCLCS)
PooledCLCRUP <- COVCL * (nCL/ntCLCRUP) + COVCRUP * (nCS/ntCLCRUP)
PooledCLCR <- COVCL * (nCL/ntCLCR) + COVCR * (nCR/ntCLCR)
PooledCLCT <- COVCL * (nCL/ntCLCT) + COVCR * (nCT/ntCLCT)
PooledCLCD <- COVCL * (nCL/ntCLCD) + COVCR * (nCD/ntCLCD)
PooledCLVE <- COVCL * (nCL/ntCLVE) + COVVE * (nVE/ntCLVE)
PooledCLMG <- COVCL * (nCL/ntCLMG) + COVMG * (nMG/ntCLMG)

PooledCSCRUP <- COVCS * (nCS/ntCSCRUP) + COVCRUP * (nCS/ntCSCRUP)
PooledCSCR <- COVCS * (nCS/ntCSCR) + COVCR * (nCR/ntCSCR)
PooledCSCT <- COVCS * (nCS/ntCSCT) + COVCT * (nCT/ntCSCT)
PooledCSCD <- COVCS * (nCS/ntCSCD) + COVCD * (nCD/ntCSCD)
PooledCSVE <- COVCS * (nCS/ntCSVE) + COVVE * (nVE/ntCSVE)
PooledCSMG <- COVCS * (nCS/ntCSMG) + COVMG * (nMG/ntCSMG)

PooledCRUPCR <- COVCRUP * (nCRUP/ntCRUPCR) + COVCR * (nCR/ntCRUPCR)
PooledCRUPCT <- COVCRUP * (nCRUP/ntCRUPCT) + COVCR * (nCT/ntCRUPCT)
PooledCRUPCD <- COVCRUP * (nCRUP/ntCRUPCD) + COVCD * (nCD/ntCRUPCD)
PooledCRUPVE <- COVCRUP * (nCRUP/ntCRUPVE) + COVVE * (nVE/ntCRUPVE)
PooledCRUPMG <- COVCRUP * (nCRUP/ntCRUPMG) + COVMG * (nMG/ntCRUPMG)

PooledCRCT <- COVCR * (nCR/ntCRCT) + COVCT * (nCT/ntCRCT)
PooledCRCD <- COVCR * (nCR/ntCRCD) + COVCD * (nCD/ntCRCD)
PooledCRVE <- COVCR * (nCR/ntCRVE) + COVVE * (nVE/ntCRVE)
PooledCRMG <- COVCR * (nCR/ntCRMG) + COVMG * (nMG/ntCRMG)

PooledCTCD <- COVCT * (nCT/ntCTCD) + COVCD * (nCD/ntCTCD)
PooledCTVE <- COVCT * (nCT/ntCTVE) + COVVE * (nVE/ntCTVE)
PooledCTMG <- COVCT * (nCT/ntCTMG) + COVMG * (nMG/ntCTMG)

PooledCDVE <- COVCD * (nCD/ntCDVE) + COVVE * (nVE/ntCDVE)
PooledCDMG <- COVCD * (nCD/ntCDMG) + COVMG * (nMG/ntCDMG)

PooledVEMG <- COVVE * (nVE/ntVEMG) + COVMG * (nMG/ntVEMG)


# mahalanobis

dmCLUCLUCM <- sqrt(t(DiffCLUCLUCM) %*% inv(PooledCLUCCLUCM) %*% DiffCLUCLUCM )
dmCLUCL <- sqrt(t(DiffCLUCL) %*% inv(PooledCLUCL) %*% DiffCLUCL )
dmCLUCS <- sqrt(t(DiffCLUCS) %*% inv(PooledCLUCS) %*% DiffCLUCS )
dmCLUCRUP <- sqrt(t(DiffCLUCRUP) %*% inv(PooledCLUCRUP) %*% DiffCLUCRUP )
dmCLUCR <- sqrt(t(DiffCLUCR) %*% inv(PooledCLUCR) %*% DiffCLUCR )
dmCLUCT <- sqrt(t(DiffCLUCT) %*% inv(PooledCLUCT) %*% DiffCLUCT )
dmCLUCD <- sqrt(t(DiffCLUCD) %*% inv(PooledCLUCD) %*% DiffCLUCD )
dmCLUVE <- sqrt(t(DiffCLUVE) %*% inv(PooledCLUVE) %*% DiffCLUVE )
dmCLUMG <- sqrt(t(DiffCLUMG) %*% inv(PooledCLUMG) %*% DiffCLUMG )

dmCLUCMCL <- sqrt(t(DiffCLUCMCL) %*% inv(PooledCLUCMCL) %*% DiffCLUCMCL )
dmCLUCMCS <- sqrt(t(DiffCLUCMCS) %*% inv(PooledCLUCMCS) %*% DiffCLUCMCS )
dmCLUCMCRUP <- sqrt(t(DiffCLUCMCRUP) %*% inv(PooledCLUCMCRUP) %*% DiffCLUCMCRUP )
dmCLUCMCR <- sqrt(t(DiffCLUCMCR) %*% inv(PooledCLUCMCR) %*% DiffCLUCMCR )
dmCLUCMCT <- sqrt(t(DiffCLUCMCT) %*% inv(PooledCLUCMCT) %*% DiffCLUCMCT )
dmCLUCMCD <- sqrt(t(DiffCLUCMCD) %*% inv(PooledCLUCMCD) %*% DiffCLUCMCD )
dmCLUCMVE <- sqrt(t(DiffCLUCMVE) %*% inv(PooledCLUCMVE) %*% DiffCLUCMVE )
dmCLUCMMG <- sqrt(t(DiffCLUCMMG) %*% inv(PooledCLUCMMG) %*% DiffCLUCMMG )

dmCLCS <- sqrt(t(DiffCLCS) %*% inv(PooledCLCS) %*% DiffCLCS )
dmCLCRUP <- sqrt(t(DiffCLCRUP) %*% inv(PooledCLCRUP) %*% DiffCLCRUP )
dmCLCR <- sqrt(t(DiffCLCR) %*% inv(PooledCLCR) %*% DiffCLCR )
dmCLCT <- sqrt(t(DiffCLCT) %*% inv(PooledCLCT) %*% DiffCLCT )
dmCLCD <- sqrt(t(DiffCLCD) %*% inv(PooledCLCD) %*% DiffCLCD )
dmCLVE <- sqrt(t(DiffCLVE) %*% inv(PooledCLVE) %*% DiffCLVE )
dmCLMG <- sqrt(t(DiffCLMG) %*% inv(PooledCLMG) %*% DiffCLMG )

dmCSCRUP <- sqrt(t(DiffCSCRUP) %*% inv(PooledCSCRUP) %*% DiffCSCRUP )
dmCSCR <- sqrt(t(DiffCSCR) %*% inv(PooledCSCR) %*% DiffCSCR )
dmCSCT <- sqrt(t(DiffCSCT) %*% inv(PooledCSCT) %*% DiffCSCT )
dmCSCD <- sqrt(t(DiffCSCD) %*% inv(PooledCSCD) %*% DiffCSCD )
dmCSVE <- sqrt(t(DiffCSVE) %*% inv(PooledCSVE) %*% DiffCSVE )
dmCSMG <- sqrt(t(DiffCSMG) %*% inv(PooledCSMG) %*% DiffCSMG )

dmCRUPCR <- sqrt(t(DiffCRUPCR) %*% inv(PooledCRUPCR) %*% DiffCRUPCR )
dmCRUPCT <- sqrt(t(DiffCRUPCT) %*% inv(PooledCRUPCT) %*% DiffCRUPCT )
dmCRUPCD <- sqrt(t(DiffCRUPCD) %*% inv(PooledCRUPCD) %*% DiffCRUPCD )
dmCRUPVE <- sqrt(t(DiffCRUPVE) %*% inv(PooledCRUPVE) %*% DiffCRUPVE )
dmCRUPMG <- sqrt(t(DiffCRUPMG) %*% inv(PooledCRUPMG) %*% DiffCRUPMG )

dmCRCT <- sqrt(t(DiffCRCT) %*% inv(PooledCRCT) %*% DiffCRCT )
dmCRCD <- sqrt(t(DiffCRCD) %*% inv(PooledCRCD) %*% DiffCRCD )
dmCRVE <- sqrt(t(DiffCRVE) %*% inv(PooledCRVE) %*% DiffCRVE )
dmCRMG <- sqrt(t(DiffCRMG) %*% inv(PooledCRMG) %*% DiffCRMG )

dmCTCD <- sqrt(t(DiffCTCD) %*% inv(PooledCTCD) %*% DiffCTCD )
dmCTVE <- sqrt(t(DiffCTVE) %*% inv(PooledCTVE) %*% DiffCTVE )
dmCTMG <- sqrt(t(DiffCTMG) %*% inv(PooledCTMG) %*% DiffCTMG )

dmCDVE <- sqrt(t(DiffCDVE) %*% inv(PooledCDVE) %*% DiffCDVE )
dmCDMG <- sqrt(t(DiffCDMG) %*% inv(PooledCDMG) %*% DiffCDMG )

dmVEMG <- sqrt(t(DiffVEMG) %*% inv(PooledVEMG) %*% DiffVEMG )



#transformando em matriz de diferenças para ficar igual aos dados de dist. euclidiana

Matriz_distancias_WV2_BRIGHT_WET <- matrix(nrow = 10, ncol = 10)
colnames (Matriz_distancias_WV2_BRIGHT_WET) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")
rownames (Matriz_distancias_WV2_BRIGHT_WET) <-c("CLU","CLUCM","CRup","CL","CS","CR","CT","CD","VE","MG")


Matriz_distancias_WV2_BRIGHT_WET[2,1] <- dmCLUCLUCM
Matriz_distancias_WV2_BRIGHT_WET[3,1] <- dmCLUCRUP
Matriz_distancias_WV2_BRIGHT_WET[4,1] <- dmCLUCS
Matriz_distancias_WV2_BRIGHT_WET[5,1] <- dmCLUCL
Matriz_distancias_WV2_BRIGHT_WET[6,1] <- dmCLUCR
Matriz_distancias_WV2_BRIGHT_WET[7,1] <- dmCLUCT
Matriz_distancias_WV2_BRIGHT_WET[8,1] <- dmCLUCD
Matriz_distancias_WV2_BRIGHT_WET[9,1] <- dmCLUVE
Matriz_distancias_WV2_BRIGHT_WET[10,1] <- dmCLUMG

Matriz_distancias_WV2_BRIGHT_WET[3,2] <- dmCLUCMCRUP
Matriz_distancias_WV2_BRIGHT_WET[4,2] <- dmCLUCMCS
Matriz_distancias_WV2_BRIGHT_WET[5,2] <- dmCLUCMCL
Matriz_distancias_WV2_BRIGHT_WET[6,2] <- dmCLUCMCR
Matriz_distancias_WV2_BRIGHT_WET[7,2] <- dmCLUCMCT
Matriz_distancias_WV2_BRIGHT_WET[8,2] <- dmCLUCMCD
Matriz_distancias_WV2_BRIGHT_WET[9,2] <- dmCLUCMVE
Matriz_distancias_WV2_BRIGHT_WET[10,2] <- dmCLUCMMG

Matriz_distancias_WV2_BRIGHT_WET[4,3] <- dmCLCRUP
Matriz_distancias_WV2_BRIGHT_WET[5,3] <- dmCSCRUP
Matriz_distancias_WV2_BRIGHT_WET[6,3] <- dmCRUPCR
Matriz_distancias_WV2_BRIGHT_WET[7,3] <- dmCRUPCT
Matriz_distancias_WV2_BRIGHT_WET[8,3] <- dmCRUPCD
Matriz_distancias_WV2_BRIGHT_WET[9,3] <- dmCRUPVE
Matriz_distancias_WV2_BRIGHT_WET[10,3] <- dmCRUPMG

Matriz_distancias_WV2_BRIGHT_WET[5,4] <- dmCLCS
Matriz_distancias_WV2_BRIGHT_WET[6,4] <- dmCLCR
Matriz_distancias_WV2_BRIGHT_WET[7,4] <- dmCLCT
Matriz_distancias_WV2_BRIGHT_WET[8,4] <- dmCLCD
Matriz_distancias_WV2_BRIGHT_WET[9,4] <- dmCLVE
Matriz_distancias_WV2_BRIGHT_WET[10,4] <- dmCLMG

Matriz_distancias_WV2_BRIGHT_WET[6,5] <- dmCSCR
Matriz_distancias_WV2_BRIGHT_WET[7,5] <- dmCSCT
Matriz_distancias_WV2_BRIGHT_WET[8,5] <- dmCSCD
Matriz_distancias_WV2_BRIGHT_WET[9,5] <- dmCSVE
Matriz_distancias_WV2_BRIGHT_WET[10,5] <- dmCSMG

Matriz_distancias_WV2_BRIGHT_WET[7,6] <- dmCRCT
Matriz_distancias_WV2_BRIGHT_WET[8,6] <- dmCRCD
Matriz_distancias_WV2_BRIGHT_WET[9,6] <- dmCRVE
Matriz_distancias_WV2_BRIGHT_WET[10,6] <- dmCRMG

Matriz_distancias_WV2_BRIGHT_WET[8,7] <- dmCTCD
Matriz_distancias_WV2_BRIGHT_WET[9,7] <- dmCTVE
Matriz_distancias_WV2_BRIGHT_WET[10,7] <- dmCTMG

Matriz_distancias_WV2_BRIGHT_WET[9,8] <- dmCDVE
Matriz_distancias_WV2_BRIGHT_WET[10,8] <- dmCDMG

Matriz_distancias_WV2_BRIGHT_WET[10,9] <- dmVEMG

Matriz_distancias_WV2_BRIGHT_WET


#Calcular as matrizes manualmente alterando os valores das bandas escolhidas
Matriz_distancias_L8_GREEN_BRIGHT
Matriz_distancias_L8_GREEN_WET
Matriz_distancias_L8_BRIGHT_WET


Matriz_distancias_WV2_GREEN_BRIGHT
Matriz_distancias_WV2_GREEN_WET
Matriz_distancias_WV2_BRIGHT_WET


#matrizes pra ficar igual a dist. euclidiana #GREENxBRIGHT

Matriz_result_GREEN_BRIGHT = Matriz_distancias_WV2_GREEN_BRIGHT - Matriz_distancias_L8_GREEN_BRIGHT

melted_matriz_GREEN_BRIGHT <- melt(Matriz_result_GREEN_BRIGHT)
Matriz_result_GREEN_BRIGHT

ggplot(data = melted_matriz_GREEN_BRIGHT, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", limits=c(-8.0,3.0), breaks=c(-8.0,-7.0,-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0,1.0,2.0,3.0)) + 
  geom_text(aes(label = round(value, digits = 4)), size =4) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))

#GREENxWET

Matriz_result_GREEN_WET = Matriz_distancias_WV2_GREEN_WET - Matriz_distancias_L8_GREEN_WET

melted_matriz_GREEN_WET <- melt(Matriz_result_GREEN_WET)
Matriz_result_GREEN_WET

ggplot(data = melted_matriz_GREEN_WET, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue",limits=c(-8.0,3.0), breaks=c(-8.0,-7.0,-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0,1.0,2.0,3.0)) +  
  geom_text(aes(label = round(value, digits = 4)), size =4) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))

#BRIGHTxWET

Matriz_result_BRIGHT_WET = Matriz_distancias_WV2_BRIGHT_WET - Matriz_distancias_L8_BRIGHT_WET

melted_matriz_BRIGHT_WET <- melt(Matriz_result_BRIGHT_WET)
Matriz_result_BRIGHT_WET

ggplot(data = melted_matriz_BRIGHT_WET, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue",limits=c(-8.0,3.0), breaks=c(-8.0,-7.0,-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0,1.0,2.0,3.0)) + 
  geom_text(aes(label = round(value, digits = 4)), size =4) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="Distâncias"))