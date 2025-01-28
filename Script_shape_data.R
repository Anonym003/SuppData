#An?lises Cap 1 
setwd("")
#install.packages("openxlsx")
#loading package
require(geomorph)
require(RRPP)
require(writexl)
require(ape)
require(geomorph)
require(MASS)
require(Morpho)
require(Rvcg)
require(stats)
require(vegan)
require(ggplot2)
library(openxlsx)

#Loadin GPAged cured Data
##################################################################################################
shape_p<-readRDS("shape_p.rds")
shape_v<-readRDS("shape_v.rds")
shape_m<-readRDS("shape_m.rds")
plan_p<-readRDS("plan_p.rds")
plan_v<-readRDS("plan_v.rds")
plan_m<-readRDS("plan_m.rds")



#################################################################################################
#Forma média
meanspec_p<-findMeanSpec(shape_p)
meanspec_v<-findMeanSpec(shape_v)
meanspec_m<-findMeanSpec(shape_m)

#Espécime que representa a forma média
mean_p<-mshape(shape_p)
mean_v<-mshape(shape_v)
mean_m<-mshape(shape_m)

#################################################################################################

#definelinks

GP1<-gridPar(pt.bg="gray",link.col="gray",link.lty=1)

plotRefToTarget(mean_p,shape_p[,,122], gridPars = GP1, method="TPS")
plotRefToTarget(mean_v,shape_v[,,115], gridPars = GP1, method="TPS")
plotRefToTarget(mean_m,shape_m[,,125], gridPars = GP1, method="TPS")
dev.off()

## Dimorfismo sexual em adultos
#Separando data frame por idade
Databy_Age_p<-split(plan_p,plan_p$Age)
Databy_Age_v<-split(plan_v,plan_v$Age)
Databy_Age_m<-split(plan_m,plan_m$Age)

#Separando shape por idade 
Shapeby_Age_p<-coords.subset(shape_p,plan_p$Age)
Shapeby_Age_v<-coords.subset(shape_v,plan_v$Age)
Shapeby_Age_m<-coords.subset(shape_m,plan_m$Age)

## Separando o data set por esp?cie

dat_A_p<-split(Databy_Age_p$A,Databy_Age_p$A$Sp)
dat_A_v<-split(Databy_Age_v$A,Databy_Age_v$A$Sp)
dat_A_m<-split(Databy_Age_m$A,Databy_Age_m$A$Sp)

## Separando o data set por esp?cie
shp_A_p<-coords.subset(Shapeby_Age_p$A,Databy_Age_p$A$Sp)
shp_A_v<-coords.subset(Shapeby_Age_v$A,Databy_Age_v$A$Sp)
shp_A_m<-coords.subset(Shapeby_Age_m$A,Databy_Age_m$A$Sp)


## Rodando loop analyse PROCD LM DIMORFISMO SEXUAL ALL
res_SD_A_p<-list()
res_SD_A_v<-list()
res_SD_A_m<-list()

#DORSAL VIEW 
for(i in names(dat_A_p)){
  dados_especie<-dat_A_p[[i]]
  shapes<-shp_A_p[[i]]
  Sex_p<-procD.lm(shapes~dados_especie$Sex)
  res_SD_A_p[[i]]<-Sex_p}

#VENTRAL VIEW 
for(i in names(dat_A_v)){
  dados_especie<-dat_A_v[[i]]
  shapes<-shp_A_v[[i]]
  Sex_v<-procD.lm(shapes~dados_especie$Sex)
  res_SD_A_v[[i]]<-Sex_v}

#MANDIBLE VIEW 
for(i in names(dat_A_m)){
  dados_especie<-dat_A_m[[i]]
  shapes<-shp_A_m[[i]]
  Sex_m<-procD.lm(shapes~dados_especie$Sex)
  res_SD_A_m[[i]]<-Sex_m}

########################################
SD_res_BOTALT_p<-res_SD_A_p$BOTALT$aov.table
SD_res_BOTJAR_p<-res_SD_A_p$BOTJAR$aov.table
SD_res_BOTPUB_p<-res_SD_A_p$BOTPUB$aov.table

SD_res_BOTALT_v<-res_SD_A_v$BOTALT$aov.table
SD_res_BOTJAR_v<-res_SD_A_v$BOTJAR$aov.table
SD_res_BOTPUB_v<-res_SD_A_v$BOTPUB$aov.table

SD_res_BOTALT_m<-res_SD_A_m$BOTALT$aov.table
SD_res_BOTJAR_m<-res_SD_A_m$BOTJAR$aov.table
SD_res_BOTPUB_m<-res_SD_A_m$BOTPUB$aov.table

SD_ALT<-list(SD_res_BOTALT_p,SD_res_BOTALT_v,SD_res_BOTALT_m)
SD_JAR<-list(SD_res_BOTJAR_p,SD_res_BOTJAR_v,SD_res_BOTJAR_m)
SD_PUB<-list(SD_res_BOTPUB_p,SD_res_BOTPUB_v,SD_res_BOTPUB_m)

SD_comb<-c(SD_ALT,SD_FON,SD_JAR,SD_PUB)

# <--------<-#---------#---------#->-----> #
# <--<--<- Loop para salvar estes resultados->-->---> # ##############
# Caminho para o arquivo existente
caminho_arquivo1 <- "SupplyMat_01 - Copia.xlsx"

###LOOPING PARA SALVAR TABELA #####################################################
# Carregar o arquivo existente
wb <- loadWorkbook(caminho_arquivo1)

# Definir a posi??o inicial para escrever os dados#########################
startRow <- 1
# Loop para cada tabela no objeto######################
for (i in 1:length(SD_comb)) {
  # Obter a tabela atual
  tabela <- SD_comb[[i]]
  
  # Obter o n?mero de linhas e colunas da tabela
  num_linhas <-nrow(tabela)
  num_colunas <- ncol(tabela)
  
  # Obter o nome das colunas
  colunas <- colnames(tabela)
  
  # Escrever o cabe?alho das colunas
  writeData(wb, sheet = "SD_SEX_ADULTS", x = colunas, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para os dados
  startRow <- startRow + 1
  
  # Escrever os dados da tabela
  writeData(wb, sheet = "SD_SEX_ADULTS", x = tabela, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para a pr?xima tabela
  startRow <- startRow + num_linhas + 3
}
# Salvar o arquivo Excel
saveWorkbook(wb, "SupplyMat_02.xlsx")


################################################################################################

## RODANDO ALOMETRIA com adultos 
alo_SD_A_p<-list()
alo_SD_A_v<-list()
alo_SD_A_m<-list()


#DORSAL VIEW 
for(i in names(dat_A_p)){
  dados_especie<-dat_A_p[[i]]
  shapes<-shp_A_p[[i]]
  Alo_Sex_p<-procD.lm(shapes~dados_especie$Length)
  alo_SD_A_p[[i]]<-Alo_Sex_p}

#VENTRAL VIEW 
for(i in names(dat_A_v)){
  dados_especie<-dat_A_v[[i]]
  shapes<-shp_A_v[[i]]
  Alo_Sex_v<-procD.lm(shapes~dados_especie$Length)
  alo_SD_A_v[[i]]<-Alo_Sex_v}

#MANDIBLE VIEW 
for(i in names(dat_A_m)){
  dados_especie<-dat_A_m[[i]]
  shapes<-shp_A_m[[i]]
  Alo_Sex_m<-procD.lm(shapes~dados_especie$Length)
  alo_SD_A_m[[i]]<-Alo_Sex_m}


#SALVANDO TABELAS
tab_SD_alo_BOTALT_p<-alo_SD_A_p$BOTALT$aov.table
tab_SD_alo_BOTJAR_p<-alo_SD_A_p$BOTJAR$aov.table
tab_SD_alo_BOTPUB_p<-alo_SD_A_p$BOTPUB$aov.table
tab_SD_alo_BOTALT_v<-alo_SD_A_v$BOTALT$aov.table
tab_SD_alo_BOTJAR_v<-alo_SD_A_v$BOTJAR$aov.table
tab_SD_alo_BOTPUB_v<-alo_SD_A_v$BOTPUB$aov.table
tab_SD_alo_BOTALT_m<-alo_SD_A_m$BOTALT$aov.table
tab_SD_alo_BOTJAR_m<-alo_SD_A_m$BOTJAR$aov.table
tab_SD_alo_BOTPUB_m<-alo_SD_A_m$BOTPUB$aov.table

## COMBINANDO BY SPP

alt_comb_SD<-list(tab_SD_alo_BOTALT_p,tab_SD_alo_BOTALT_v,tab_SD_alo_BOTALT_m)
jar_comb_SD<-list(tab_SD_alo_BOTJAR_p,tab_SD_alo_BOTJAR_v,tab_SD_alo_BOTJAR_m)
pub_comb_SD<-list(tab_SD_alo_BOTPUB_p,tab_SD_alo_BOTPUB_v,tab_SD_alo_BOTPUB_m)


# COMbinado all

all_SD_alo<-c(alt_comb_SD,fon_comb_SD,jar_comb_SD,pub_comb_SD)

# Caminho para o arquivo existente
caminho_arquivo00 <- "SupplyMat_02 - Copia.xlsx"

###LOOPING PARA SALVAR TABELA no EXCEL  #############################################
# Carregar o arquivo existente
wb <- loadWorkbook(caminho_arquivo00)

# Definir a posi??o inicial para escrever os dados #################################
startRow <- 1
# Loop para cada tabela no objeto
for (i in 1:length(all_SD_alo)) {
  # Obter a tabela atual
  tabela <- all_SD_alo[[i]]
  
  # Obter o n?mero de linhas e colunas da tabela
  num_linhas <- nrow(tabela)
  num_colunas <- ncol(tabela)
  
  # Obter o nome das colunas
  colunas <- colnames(tabela)
  
  # Escrever o cabe?alho das colunas
  writeData(wb, sheet = "Alo_adults", x = colunas, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para os dados
  startRow <- startRow + 1
  
  # Escrever os dados da tabela
  writeData(wb, sheet = "Alo_adults", x = tabela, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para a pr?xima tabela
  startRow <- startRow + num_linhas + 3
}
# Salvar o arquivo Excel
saveWorkbook(wb, "SupplyMat_03.xlsx")


###########################################  ###################################  ########################################  ##############################  ###############
################################  ###################################  ##################################  ##################################  ############################
###########################################  ###################################  ########################################  ##############################  ###############
#Separando data_set por esp?cie
# Alometria Separada x Dimorfismo sexual na allometria 
#Separando data frame por esp?cie
Databy_Spp_p<-split(plan_p,plan_p$Sp)
Databy_Spp_v<-split(plan_v,plan_v$Sp)
Databy_Spp_m<-split(plan_m,plan_m$Sp)

#Separando shape por esp?cie
Shapeby_Spp_p<-coords.subset(shape_p,plan_p$Sp)
Shapeby_Spp_v<-coords.subset(shape_v,plan_v$Sp)
Shapeby_Spp_m<-coords.subset(shape_m,plan_m$Sp)

## Formula-por estrutura por esp[ecie ]
res_alo_sex_p<-list()
res_alo_sex_v<-list()
res_alo_sex_m<-list()
## Loop para rodar a mancova_parietal 
for(i in names(Databy_Spp_p)){
  dados_especie<-Databy_Spp_p[[i]]
  shapes<-Shapeby_Spp_p[[i]]
  Alo_sex_p<-procD.lm(shapes~log(dados_especie$Length))
  res_alo_sex_p[[i]]<-Alo_sex_p}

## Loop para rodar mancoa_vetral
for(i in names(Databy_Spp_v)){
  dados_especie<-Databy_Spp_v[[i]]
  shapes<-Shapeby_Spp_v[[i]]
  Alo_sex_v<-procD.lm(shapes~log(dados_especie$Length))
  res_alo_sex_v[[i]]<-Alo_sex_v}

## Loop para rodar mancoa_mandible
for(i in names(Databy_Spp_m)){
  dados_especie<-Databy_Spp_m[[i]]
  shapes<-Shapeby_Spp_m[[i]]
  Alo_sex_m<-procD.lm(shapes~log(dados_especie$Length))
  res_alo_sex_m[[i]]<-Alo_sex_m}

## Resultado loop para importar para o excel_parietal 
alo_tab_ALT_p<-res_alo_sex_p$BOTALT$aov.table
alo_tab_JAR_p<-res_alo_sex_p$BOTJAR$aov.table
alo_tab_PUB_p<-res_alo_sex_p$BOTPUB$aov.table

## Resultado loop para importar para o excel_ventral
alo_tab_ALT_v<-res_alo_sex_v$BOTALT$aov.table
alo_tab_JAR_v<-res_alo_sex_v$BOTJAR$aov.table
alo_tab_PUB_v<-res_alo_sex_v$BOTPUB$aov.table

## Resultado loop para importar para o excel_mandible
alo_tab_ALT_m<-res_alo_sex_m$BOTALT$aov.table
alo_tab_JAR_m<-res_alo_sex_m$BOTJAR$aov.table
alo_tab_PUB_m<-res_alo_sex_m$BOTPUB$aov.table

## Juntando resultados para cada especie
res_alo_ALT<-list (alo_tab_ALT_p,alo_tab_ALT_v,alo_tab_ALT_m)
res_alo_JAR<-list (alo_tab_JAR_p,alo_tab_JAR_v,alo_tab_JAR_m)
res_alo_PUB<-list (alo_tab_PUB_p,alo_tab_PUB_v,alo_tab_PUB_m)

comb_alo_sex_tab<-list(c(res_alo_ALT,res_alo_FON,res_alo_JAR,res_alo_PUB))

list_comb<-comb_alo_sex_tab[[i]]

# Criar um novo arquivo Excel
wb <- createWorkbook()

# Criar uma nova planilha no arquivo Excel ##############################################
addWorksheet(wb, sheetName = "Todas as Tabelas")

# Definir a posi??o inicial para escrever os dados
startRow <- 1

# Loop para cada tabela no objeto
for (i in 1:length(list_comb)) {
  # Obter a tabela atual
  tabela <- list_comb[[i]]
  
  # Obter o n?mero de linhas e colunas da tabela
  num_linhas <- nrow(tabela)
  num_colunas <- ncol(tabela)
  
  # Obter o nome das colunas
  colunas <- colnames(tabela)
  
  # Escrever o cabe?alho das colunas
  writeData(wb, sheet = "Todas as Tabelas", x = colunas, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para os dados
  startRow <- startRow + 1
  
  # Escrever os dados da tabela
  writeData(wb, sheet = "Todas as Tabelas", x = tabela, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para a pr?xima tabela
  startRow <- startRow + num_linhas + 3
}
# Salvar o arquivo Excel
#saveWorkbook(wb, "Oi.xlsx")
# Salvar o arquivo Excel
#saveWorkbook(wb, "Response.xlsx")

#############################################################################################################################################
############################    ############################################    #################################    #######################
#############################################################################################################################################

## Plotando Alometria sex, species ########################################################
fits_plot_p<-list()
fits_plot_v<-list()
fits_plot_m<-list()

## Loop Dorsal
for(i in names(Databy_Spp_p)){
  dados_especie<-Databy_Spp_p[[i]]
  fits_p<-res_alo_sex_p[[i]]
  Plot_alo_sex_p<-plotAllometry(fits_p,size =dados_especie$Length, logsz=TRUE,
                                method = "PredLine", pch=21, xlab="Log (SVL)", cex=1.5)
  fits_plot_p[[i]]<-Plot_alo_sex_p}

## Loop Ventral
for(i in names(Databy_Spp_v)){
  dados_especie<-Databy_Spp_v[[i]]
  fits_v<-res_alo_sex_v[[i]]
  Plot_alo_sex_v<-plotAllometry(fits_v,size =dados_especie$Length, logsz=TRUE,
                                method = "PredLine", pch=21, xlab="Log (SVL)", cex=1.5)
  fits_plot_v[[i]]<-Plot_alo_sex_v}

## Loop Ventral
for(i in names(Databy_Spp_m)){
  dados_especie<-Databy_Spp_m[[i]]
  fits_m<-res_alo_sex_m[[i]]
  Plot_alo_sex_m<-plotAllometry(fits_m,size =dados_especie$Length, logsz=TRUE,
                                method = "PredLine", pch=21, xlab="Log (SVL)", cex=1.5)
  fits_plot_m[[i]]<-Plot_alo_sex_m}
###########################################################################################################################
## Criando dataframe com RegScore and predlines
Reg_BALT_p<-fits_plot_p$BOTALT$RegScore
Reg_BJAR_p<-fits_plot_p$BOTJAR$RegScore
Reg_BPUB_p<-fits_plot_p$BOTPUB$RegScore
Reg_BALT_v<-fits_plot_v$BOTALT$RegScore
Reg_BJAR_v<-fits_plot_v$BOTJAR$RegScore
Reg_BPUB_v<-fits_plot_v$BOTPUB$RegScore
Reg_BALT_m<-fits_plot_m$BOTALT$RegScore
Reg_BJAR_m<-fits_plot_m$BOTJAR$RegScore
Reg_BPUB_m<-fits_plot_m$BOTPUB$RegScore

## Cores gr?fico 
col_alt<-data.frame(cor=c("darkolivegreen3", "darkgreen")    ,sex=c("F","M"))
col_jar<-data.frame(cor=c("deepskyblue3", "dodgerblue4")     ,sex=c("F","M"))
col_pub<-data.frame(cor=c("darkgoldenrod1", "darkgoldenrod") ,sex=c("F","M"))


# Dataframe com as cores e informa??es de sexo
col_alt<-data.frame(cor=c("darkolivegreen3", "darkgreen")    ,sex=c("F","M"))
col_jar<-data.frame(cor=c("deepskyblue3", "dodgerblue4")     ,sex=c("F","M"))
col_pub<-data.frame(cor=c("darkgoldenrod1", "darkgoldenrod") ,sex=c("F","M"))

# Tabela com as informa??es de sexo
sex_BALT_p<-data.frame(sex=c(Dataf_BALT_p$Sex))#col_alt
sex_BJAR_p<-data.frame(sex=c(Dataf_BJAR_p$Sex))#col_jar
sex_BPUB_p<-data.frame(sex=c(Dataf_BPUB_p$Sex))#col_pub
sex_BALT_v<-data.frame(sex=c(Dataf_BALT_v$Sex))#col_alt
sex_BJAR_v<-data.frame(sex=c(Dataf_BJAR_v$Sex))#col_jar
sex_BPUB_v<-data.frame(sex=c(Dataf_BPUB_v$Sex))#col_pub
sex_BALT_m<-data.frame(sex=c(Dataf_BALT_m$Sex))#col_alt
sex_BJAR_m<-data.frame(sex=c(Dataf_BJAR_m$Sex))#col_jar
sex_BPUB_m<-data.frame(sex=c(Dataf_BPUB_m$Sex))#col_pub

# Combinar dados da tabela com as informa??es de cores
vet_BALT_p<-col_alt$cor[match(sex_BALT_p$sex,col_alt$sex)]
vet_BJAR_p<-col_jar$cor[match(sex_BJAR_p$sex,col_jar$sex)]
vet_BPUB_p<-col_pub$cor[match(sex_BPUB_p$sex,col_pub$sex)]
vet_BALT_v<-col_alt$cor[match(sex_BALT_v$sex,col_alt$sex)]
vet_BJAR_v<-col_jar$cor[match(sex_BJAR_v$sex,col_jar$sex)]
vet_BPUB_v<-col_pub$cor[match(sex_BPUB_v$sex,col_pub$sex)]
vet_BALT_m<-col_alt$cor[match(sex_BALT_m$sex,col_alt$sex)]
vet_BJAR_m<-col_jar$cor[match(sex_BJAR_m$sex,col_jar$sex)]
vet_BPUB_m<-col_pub$cor[match(sex_BPUB_m$sex,col_pub$sex)]


## DATA FRAMES PARA O PLOT 
Dataf_BALT_p<-data.frame(Databy_Spp_p$BOTALT,Reg_BALT_p,vet_BALT_p)
Dataf_BJAR_p<-data.frame(Databy_Spp_p$BOTJAR,Reg_BJAR_p,vet_BJAR_p)
Dataf_BPUB_p<-data.frame(Databy_Spp_p$BOTPUB,Reg_BPUB_p,vet_BPUB_p)
Dataf_BALT_v<-data.frame(Databy_Spp_v$BOTALT,Reg_BALT_v,vet_BALT_v)
Dataf_BJAR_v<-data.frame(Databy_Spp_v$BOTJAR,Reg_BJAR_v,vet_BJAR_v)
Dataf_BPUB_v<-data.frame(Databy_Spp_v$BOTPUB,Reg_BPUB_v,vet_BPUB_v)
Dataf_BALT_m<-data.frame(Databy_Spp_m$BOTALT,Reg_BALT_m,vet_BALT_m)
Dataf_BJAR_m<-data.frame(Databy_Spp_m$BOTJAR,Reg_BJAR_m,vet_BJAR_m)
Dataf_BPUB_m<-data.frame(Databy_Spp_m$BOTPUB,Reg_BPUB_m,vet_BPUB_m)

###############################################################################################################################################################################


#Plotando gr?ficos no ggplot
plot_Alo_sex_BALT_p<-ggplot(Dataf_BALT_p,aes(y=Reg_BALT_p,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BALT_p$pch ,fill=Dataf_BALT_p$vet_BALT_p, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkolivegreen3", "darkgreen"))                                                                              
plot_Alo_sex_BALT_v<-ggplot(Dataf_BALT_v,aes(y=Reg_BALT_v,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BALT_v$pch ,fill=Dataf_BALT_v$vet_BALT_v, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkolivegreen3", "darkgreen"))                                                                              
plot_Alo_sex_BALT_m<-ggplot(Dataf_BALT_m,aes(y=Reg_BALT_m,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BALT_m$pch ,fill=Dataf_BALT_m$vet_BALT_m, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkolivegreen3", "darkgreen"))                                                                              
plot_Alo_sex_BJAR_p<-ggplot(Dataf_BJAR_p,aes(y=Reg_BJAR_p,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BJAR_p$pch ,fill=Dataf_BJAR_p$vet_BJAR_p, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("deepskyblue3", "dodgerblue4"))                                                                              
plot_Alo_sex_BJAR_v<-ggplot(Dataf_BJAR_v,aes(y=Reg_BJAR_v,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BJAR_v$pch ,fill=Dataf_BJAR_v$vet_BJAR_v, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("deepskyblue3", "dodgerblue4"))                                                                              
plot_Alo_sex_BJAR_m<-ggplot(Dataf_BJAR_m,aes(y=Reg_BJAR_m,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BJAR_m$pch ,fill=Dataf_BJAR_m$vet_BJAR_m, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("deepskyblue3", "dodgerblue4"))                                                                              
plot_Alo_sex_BPUB_p<-ggplot(Dataf_BPUB_p,aes(y=Reg_BPUB_p,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BPUB_p$pch ,fill=Dataf_BPUB_p$vet_BPUB_p, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkgoldenrod1", "darkgoldenrod"))                                                                              
plot_Alo_sex_BPUB_v<-ggplot(Dataf_BPUB_v,aes(y=Reg_BPUB_v,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BPUB_v$pch ,fill=Dataf_BPUB_v$vet_BPUB_v, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkgoldenrod1", "darkgoldenrod"))                                                                              
plot_Alo_sex_BPUB_m<-ggplot(Dataf_BPUB_m,aes(y=Reg_BPUB_m,x=log(Length),color=Sex))+stat_smooth(method = "lm", se=FALSE, size=1.5,show.legend = FALSE)+geom_point(aes(colour = Sex), shape=Dataf_BPUB_m$pch ,fill=Dataf_BPUB_m$vet_BPUB_m, size=2.5,show.legend = FALSE) + labs(title ="", subtitle ="", x = "log(SVL)",y = "Regression Score", size=2)+ scale_color_manual(values=c("darkgoldenrod1", "darkgoldenrod"))                                                                              

## statistic names correlation - SD_Allometry 

a.i<-plot_Alo_sex_BALT_p
a.ii<-plot_Alo_sex_BALT_v
a.iii<-plot_Alo_sex_BALT_m

c.i<-plot_Alo_sex_BJAR_p
c.ii<-plot_Alo_sex_BJAR_v
c.iii<-plot_Alo_sex_BJAR_m

d.i<-plot_Alo_sex_BPUB_p
d.ii<-plot_Alo_sex_BPUB_v
d.iii<-plot_Alo_sex_BPUB_m


#################  #################################  #############################  #############################  ##########################  #######################
######   #########################   ################################   #################################   ##########################   #################   #################
#################  #################################  #############################  #############################  ##########################  #######################


############################################################################################################
## Comparando alometria entre especies 
fit_spp_p<-procD.lm(shape_p~log(plan_p$Length)*plan_p$Sp)
fit_spp_v<-procD.lm(shape_v~log(plan_v$Length)*plan_v$Sp)
fit_spp_m<-procD.lm(shape_m~log(plan_m$Length)*plan_m$Sp)

res_spxsp_p<-summary(fit_spp_p)
res_spxsp_v<-summary(fit_spp_v)
res_spxsp_m<-summary(fit_spp_m)

table_spp_p<-res_spxsp_p$table
table_spp_v<-res_spxsp_v$table
table_spp_m<-res_spxsp_m$table





#############################################################################################################

######### SALVANDO RESULTADOS NA TABELA EXCEL DE RESULTADOS #################################################
comb_alo_sp_tab<-list(table_spp_p,table_spp_v,table_spp_m)

# Caminho para o arquivo existente ###############################
caminho_arquivo <- "SuplyMat.xlsx"

###LOOPING PARA SALVAR TABELA 
# Carregar o arquivo existente
wb <- loadWorkbook(caminho_arquivo)

# Definir a posi??o inicial para escrever os dados
startRow <- 1
# Loop para cada tabela no objeto
for (i in 1:length(comb_alo_sp_tab)) {
  # Obter a tabela atual
  tabela <- comb_alo_sp_tab[[i]]
  
  # Obter o n?mero de linhas e colunas da tabela
  num_linhas <- nrow(tabela)
  num_colunas <- ncol(tabela)
  
  # Obter o nome das colunas
  colunas <- colnames(tabela)
  
  # Escrever o cabe?alho das colunas
  writeData(wb, sheet = "Todas as Tabelas", x = colunas, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para os dados
  startRow <- startRow + 1
  
  # Escrever os dados da tabela
  writeData(wb, sheet = "Todas as Tabelas", x = tabela, startRow = startRow, startCol = 1)
  
  # Atualizar a posi??o inicial para a pr?xima tabela
  startRow <- startRow + num_linhas + 3
}
# Salvar o arquivo Excel
saveWorkbook(wb, "caminho_arquivo.xlsx")

############################################################################################################################################

# An?lise pareada 
ppw_p<-pairwise(fit_spp_p,groups = plan_p$Sp, covariate = plan_p$Length)
ppw_v<-pairwise(fit_spp_v,groups = plan_v$Sp, covariate = plan_v$Length)
ppw_m<-pairwise(fit_spp_m,groups = plan_m$Sp, covariate = plan_m$Length)

#summaries ANGLE
summary(ppw_p, test.type="VC", angle.type="deg") #angular differentes 
summary(ppw_v, test.type="VC", angle.type="deg") #angular differentes 
summary(ppw_m, test.type="VC", angle.type="deg") #angular differentes 

#                    r    angle UCL (95%)          Z Pr > angle
#BOTALT:BOTJAR 0.8545354 31.29155  18.98944  4.6559742      0.001
#BOTALT:BOTPUB 0.8707926 29.44912  20.43593  3.8798700      0.001
#BOTJAR:BOTPUB 0.9764786 12.45157  18.68195 -0.5790683      0.727

#                      r    angle UCL (95%)        Z Pr > angle
#BOTALT:BOTJAR 0.7881686 37.98531  32.02050 2.522112      0.003
#BOTALT:BOTPUB 0.7952091 37.32499  34.13048 2.095217      0.019
#BOTJAR:BOTPUB 0.8591331 30.78061  30.78344 1.685111      0.051

#                      r    angle UCL (95%)         Z Pr > angle
#BOTALT:BOTJAR  0.1391994 81.99848  50.59641 3.4865312      0.001
#BOTALT:BOTPUB -0.1254402 97.20618  53.17077 4.2368173      0.001
#BOTJAR:BOTPUB  0.8056695 36.32504  49.09566 0.6178602      0.271




summary(ppw_p, test.type="var", angle.type="deg") 
summary(ppw_v, test.type="VC", angle.type="deg") 
summary(ppw_m, test.type="VC", angle.type="deg") 


#summaries ammount of shape differences
summary(ppw_p, test.type="dist", angle.type="deg") #magnitude differentes 
summary(ppw_v, test.type="dist", angle.type="deg") #magnitude differentes 
summary(ppw_m, test.type="dist", angle.type="deg") #magnitude differentes 


summary(ppw_p, test.type="DL", angle.type="deg") 
summary(ppw_v, test.type="DL", angle.type="deg") 
summary(ppw_m, test.type="DL", angle.type="deg") 

hi2<-trajectory.analysis(fit_spp_p,groups= plan_p$Sp, traj.pts =plan_p$col.Age.2 )
summary(hi2, attribute = "MD")
summary(TA_size_p, attribute = "MD")


###### ESTES DADOS FORAM SALVOS MANUALMENTE NO EXCEL ########################################################################################
############################  ################################################  ###################################  ########################

## Plotando allometria entre esp?cies ##
plot_Alo_p<-plotAllometry(fit_spp_p,size=plan_p$Length,logsz=TRUE,method="PredLine",pch=as.numeric(as.factor(col$Age.2)), xlab="Log (SVL)",cex=1.5)
plot_Alo_v<-plotAllometry(fit_spp_v,size=plan_v$Length,logsz=TRUE,method="PredLine",pch=as.numeric(as.factor(col_v$Age.1)), xlab="Log (SVL)",cex=1.5)
plot_Alo_m<-plotAllometry(fit_spp_m,size=plan_m$Length,logsz=TRUE,method="PredLine",pch=as.numeric(as.factor(col_m$Age.1)), xlab="Log (SVL)",cex=1.5)
col_m<-col_m[-8,]
data_p<-data.frame(plan_p,col,  plot_Alo_p$RegScore,plot_Alo_p$PredLine )
data_v<-data.frame(plan_v,col_v,plot_Alo_v$RegScore,plot_Alo_v$PredLine )
data_m<-data.frame(plan_m,col_m,plot_Alo_m$RegScore,plot_Alo_m$PredLine )


####### Allographs  #########################################################################################################################################
#############################################################################################################################################################
require(ggplot2)
## Dorsal 
Alo_p_SPxSp_fit<-ggplot( data_p, aes(y=(plot_Alo_p$PredLine), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_p$Length/20), col="black",fill=data_p$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "PC1 for fitted values", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))

Alo_p_SPxSp_reg<-ggplot( data_p, aes(y=(plot_Alo_p$RegScore), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_p$Length/20), col="black",fill=data_p$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "Regression Score", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))


## Ventral 
Alo_v_SPxSp_fit<-ggplot( data_v, aes(y=(plot_Alo_v$PredLine), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_v$Length/20), col="black",fill=data_v$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "PC1 for fitted values", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))

Alo_v_SPxSp_reg<-ggplot( data_v, aes(y=(plot_Alo_v$RegScore), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_v$Length/20), col="black",fill=data_v$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "Regression Score", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))


## Mandbibula  
Alo_m_SPxSp_fit<-ggplot( data_m, aes(y=(plot_Alo_m$PredLine), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_m$Length/20), col="black",fill=data_m$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "PC1 for fitted values", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))

Alo_m_SPxSp_reg<-ggplot( data_m, aes(y=(plot_Alo_m$RegScore), x=log(Length), color= Sp ))+
  #stat_smooth(method = "lm", se=TRUE,size=1.5,show.legend = TRUE) +
  geom_point(aes(colour = Sp), shape=21,size=(data_m$Length/20), col="black",fill=data_m$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "Regression Score", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))

#########################################################################################################################################

#SHAPES

alo_alt_p<-procD.lm(Shapeby_Spp_p$BOTALT~log(Databy_Spp_p$BOTALT$Length),iter=999)
alo_alt_v<-procD.lm(Shapeby_Spp_v$BOTALT~log(Databy_Spp_v$BOTALT$Length),iter=999)
alo_alt_m<-procD.lm(Shapeby_Spp_m$BOTALT~log(Databy_Spp_m$BOTALT$Length),iter=999)
alo_jar_p<-procD.lm(Shapeby_Spp_p$BOTJAR~log(Databy_Spp_p$BOTJAR$Length),iter=999)
alo_jar_v<-procD.lm(Shapeby_Spp_v$BOTJAR~log(Databy_Spp_v$BOTJAR$Length),iter=999)
alo_jar_m<-procD.lm(Shapeby_Spp_m$BOTJAR~log(Databy_Spp_m$BOTJAR$Length),iter=999)
alo_pub_p<-procD.lm(Shapeby_Spp_p$BOTPUB~log(Databy_Spp_p$BOTPUB$Length),iter=999)
alo_pub_v<-procD.lm(Shapeby_Spp_v$BOTPUB~log(Databy_Spp_v$BOTPUB$Length),iter=999)
alo_pub_m<-procD.lm(Shapeby_Spp_m$BOTPUB~log(Databy_Spp_m$BOTPUB$Length),iter=999)


alo_alt_p<-procD.lm(Shapeby_Spp_p$BOTALT~log(Databy_Spp_p$BOTALT$ttlength),iter=999)
alo_alt_v<-procD.lm(Shapeby_Spp_v$BOTALT~log(Databy_Spp_v$BOTALT$ttlength),iter=999)
alo_alt_m<-procD.lm(Shapeby_Spp_m$BOTALT~log(Databy_Spp_m$BOTALT$ttlength),iter=999)
alo_jar_p<-procD.lm(Shapeby_Spp_p$BOTJAR~log(Databy_Spp_p$BOTJAR$ttlength),iter=999)
alo_jar_v<-procD.lm(Shapeby_Spp_v$BOTJAR~log(Databy_Spp_v$BOTJAR$ttlength),iter=999)
alo_jar_m<-procD.lm(Shapeby_Spp_m$BOTJAR~log(Databy_Spp_m$BOTJAR$ttlength),iter=999)
alo_pub_p<-procD.lm(Shapeby_Spp_p$BOTPUB~log(Databy_Spp_p$BOTPUB$ttlength),iter=999)
alo_pub_v<-procD.lm(Shapeby_Spp_v$BOTPUB~log(Databy_Spp_v$BOTPUB$ttlength),iter=999)
alo_pub_m<-procD.lm(Shapeby_Spp_m$BOTPUB~log(Databy_Spp_m$BOTPUB$ttlength),iter=999)





plot_alo_ALT_p<-plotAllometry(alo_alt_p,size=Databy_Spp_p$BOTALT$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_JAR_p<-plotAllometry(alo_jar_p,size=Databy_Spp_p$BOTJAR$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_PUB_p<-plotAllometry(alo_pub_p,size=Databy_Spp_p$BOTPUB$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_ALT_v<-plotAllometry(alo_alt_v,size=Databy_Spp_v$BOTALT$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_JAR_v<-plotAllometry(alo_jar_v,size=Databy_Spp_v$BOTJAR$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_PUB_v<-plotAllometry(alo_pub_v,size=Databy_Spp_v$BOTPUB$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_ALT_m<-plotAllometry(alo_alt_m,size=Databy_Spp_m$BOTALT$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_JAR_m<-plotAllometry(alo_jar_m,size=Databy_Spp_m$BOTJAR$ttlength,logsz = TRUE, method = "PredLine")
plot_alo_PUB_m<-plotAllometry(alo_pub_m,size=Databy_Spp_m$BOTPUB$ttlength,logsz = TRUE, method = "PredLine")


pred_alt_p<-shape.predictor(plot_alo_ALT_p$GM$fitted, x=Databy_Spp_p$BOTALT$Length, Intercept=TRUE, predmin=min(Databy_Spp_p$BOTALT$Length),predmax=max(Databy_Spp_p$BOTALT$Length))
pred_alt_v<-shape.predictor(plot_alo_ALT_v$GM$fitted, x=Databy_Spp_v$BOTALT$Length, Intercept=TRUE, predmin=min(Databy_Spp_v$BOTALT$Length),predmax=max(Databy_Spp_v$BOTALT$Length))
pred_alt_m<-shape.predictor(plot_alo_ALT_m$GM$fitted, x=Databy_Spp_m$BOTALT$Length, Intercept=TRUE, predmin=min(Databy_Spp_m$BOTALT$Length),predmax=max(Databy_Spp_m$BOTALT$Length))
pred_jar_p<-shape.predictor(plot_alo_JAR_p$GM$fitted, x=Databy_Spp_p$BOTJAR$Length, Intercept=TRUE, predmin=min(Databy_Spp_p$BOTJAR$Length),predmax=max(Databy_Spp_p$BOTJAR$Length))
pred_jar_v<-shape.predictor(plot_alo_JAR_v$GM$fitted, x=Databy_Spp_v$BOTJAR$Length, Intercept=TRUE, predmin=min(Databy_Spp_v$BOTJAR$Length),predmax=max(Databy_Spp_v$BOTJAR$Length))
pred_jar_m<-shape.predictor(plot_alo_JAR_m$GM$fitted, x=Databy_Spp_m$BOTJAR$Length, Intercept=TRUE, predmin=min(Databy_Spp_m$BOTJAR$Length),predmax=max(Databy_Spp_m$BOTJAR$Length))
pred_pub_p<-shape.predictor(plot_alo_PUB_p$GM$fitted, x=Databy_Spp_p$BOTPUB$Length, Intercept=TRUE, predmin=min(Databy_Spp_p$BOTPUB$Length),predmax=max(Databy_Spp_p$BOTPUB$Length))
pred_pub_v<-shape.predictor(plot_alo_PUB_v$GM$fitted, x=Databy_Spp_v$BOTPUB$Length, Intercept=TRUE, predmin=min(Databy_Spp_v$BOTPUB$Length),predmax=max(Databy_Spp_v$BOTPUB$Length))
pred_pub_m<-shape.predictor(plot_alo_PUB_m$GM$fitted, x=Databy_Spp_m$BOTPUB$Length, Intercept=TRUE, predmin=min(Databy_Spp_m$BOTPUB$Length),predmax=max(Databy_Spp_m$BOTPUB$Length))


preds <- shape.predictor(plot_Alo_p$GM$fitted, x= data_p$Length, Intercept = TRUE,predmin = min(data_p$Length),predmax = max(data_p$Length)) 

M_ALT_p<-mshape(Shapeby_Spp_p$BOTALT)
M_ALT_p
M_JAR_p<-mshape(Shapeby_Spp_p$BOTJAR)
M_PUB_p<-mshape(Shapeby_Spp_p$BOTPUB)

M_ALT_v<-mshape(Shapeby_Spp_v$BOTALT)
M_JAR_v<-mshape(Shapeby_Spp_v$BOTJAR)
M_PUB_v<-mshape(Shapeby_Spp_v$BOTPUB)

M_ALT_m<-mshape(Shapeby_Spp_m$BOTALT)
M_JAR_m<-mshape(Shapeby_Spp_m$BOTJAR)
M_PUB_m<-mshape(Shapeby_Spp_m$BOTPUB)


#Links_p<-define.links(M_ALT_p,ptsize = 2, links=NULL)
Links_p
#Links_v<-define.links(M_ALT_v,ptsize = 2, links=NULL)
Links_v
#Links_m<-define.links(M_ALT_m,ptsize = 2, links=NULL)
Links_m
plot(pred_alt_p$predmax)
plot(pred_alt_p$predmin)




GP1<-gridPar(pt.bg="darkolivegreen3",link.col="darkolivegreen3",link.lty=1,link.lwd=3) # cor dos landmarks e linksGP1<-gridPar(pt.bg="gray",link.col="gray",link.lty=1) # cor dos landmarks e links

GP2<-gridPar(pt.bg="deepskyblue3",link.col="deepskyblue3",link.lty=1,link.lwd=3) # cor dos landmarks e linksGP1<-gridPar(pt.bg="gray",link.col="gray",link.lty=1) # cor dos landmarks e links

GP3<-gridPar(pt.bg="darkgoldenrod1",link.col="darkgoldenrod1",link.lty=1,link.lwd=3) # cor dos landmarks e linksGP1<-gridPar(pt.bg="gray",link.col="gray",link.lty=1) # cor dos landmarks e links


pdf("Shapes_P.pdf", width = 10, height = 10)
par(mfrow=c(3,2))
plotRefToTarget(M_ALT_p, pred_alt_p$predmin, mag=1, links =Links_p, gridPars=GP1,method="vector")
mtext("Alt_p Min")
plotRefToTarget(M_ALT_p, pred_alt_p$predmax, mag=1, links =Links_p, gridPars=GP1,method="vector")
mtext("Alt_p Max")
plotRefToTarget(M_JAR_p, pred_jar_p$predmin, mag=1, links =Links_p, gridPars=GP2,method="vector") 
mtext("Jar_p Min")
plotRefToTarget(M_JAR_p, pred_jar_p$predmax, mag=1, links =Links_p, gridPars=GP2,method="vector")
mtext("Jar_p Max")
plotRefToTarget(M_PUB_p, pred_pub_p$predmin, mag=1, links =Links_p, gridPars=GP3,method="vector")
mtext("Pub_p Min")
plotRefToTarget(M_PUB_p, pred_pub_p$predmax, mag=1, links =Links_p, gridPars=GP3,method="vector")
mtext("Pub_p Max")
dev.off()




pdf("Shapes_V.pdf", width = 10, height = 10)
par(mfrow=c(3,2))
plotRefToTarget(M_ALT_v, pred_alt_v$predmin, mag=1, links =Links_v, gridPars=GP1,method="vector")
mtext("Alt_v Min")
plotRefToTarget(M_ALT_v, pred_alt_v$predmax, mag=1, links =Links_v, gridPars=GP1,method="vector")
mtext("Alt_v Max")
plotRefToTarget(M_JAR_v, pred_jar_v$predmin, mag=1, links =Links_v, gridPars=GP2,method="vector")
mtext("Jar_v Min")
plotRefToTarget(M_JAR_v, pred_jar_v$predmax, mag=1, links =Links_v, gridPars=GP2,method="vector")
mtext("Jar_v Max")
plotRefToTarget(M_PUB_v, pred_pub_v$predmin, mag=1, links =Links_v, gridPars=GP3,method="vector")
mtext("Pub_v Min")
plotRefToTarget(M_PUB_v, pred_pub_v$predmax, mag=1, links =Links_v, gridPars=GP3,method="vector")
mtext("Pub_v Max")
dev.off()

pdf("Shapes_M.pdf", width = 10, height = 10)
par(mfrow=c(3,2))
plotRefToTarget(M_ALT_m, pred_alt_m$predmin, mag=1, links =Links_m, gridPars=GP1,method="vector")
mtext("Alt_m Min")
plotRefToTarget(M_ALT_m, pred_alt_m$predmax, mag=1, links =Links_m, gridPars=GP1,method="vector")
mtext("Alt_m Max")
plotRefToTarget(M_JAR_m, pred_jar_m$predmin, mag=1, links =Links_m, gridPars=GP2,method="vector")
mtext("Jar_m Min")
plotRefToTarget(M_JAR_m, pred_jar_m$predmax, mag=1, links =Links_m, gridPars=GP2,method="vector")
mtext("Jar_m Max")
plotRefToTarget(M_PUB_m, pred_pub_m$predmin, mag=1, links =Links_m, gridPars=GP3,method="vector")
mtext("Pub_m Min")
plotRefToTarget(M_PUB_m, pred_pub_m$predmax, mag=1, links =Links_m, gridPars=GP3,method="vector")
mtext("Pub_m Max")
dev.off()


mtext("Regression Max")
par(mfrow=c(1,1))





ean_p
preds <- shape.predictor(plot_Alo_p$GM$fitted, x= data_p$Length, Intercept = TRUE,predmin = min(data_p$Length), 
                         predmax = max(data_p$Length)) 

par(mfrow=c(1,2))
plot(preds$predmax)
plot(preds$predmin)

par(mfrow=c(1,2))
plotRefToTarget(mean_p, preds$predmin, mag=1)
mtext("Regression Min")
plotRefToTarget(mean_p, preds$predmax, mag=)
mtext("Regression Max")
par(mfrow=c(1,1))


###########################################################################################################################################################

library(cowplot)
library(ggplot2)

# Gráficos individuais
plot1 <- Alo_p_SPxSp_reg
plot2 <- Alo_p_SPxSp_fit
plot3 <- Alo_v_SPxSp_reg
plot4 <- Alo_v_SPxSp_fit
plot5 <- Alo_m_SPxSp_reg
plot6 <- Alo_m_SPxSp_fit


# Organizar os gráficos em um grid
grid <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, labels=c("A","B","C","D","E","F"))


# Salvar o grid no PDF
pdf("meu_pdf_bonito.pdf", width = 10, height = 10)
print(grid)
dev.off()

################################################################################################################################################################################
#########################################################################################################################################################################


reg_ALT_p<-plot_alo_ALT_p$RegScore
reg_ALT_v<-plot_alo_ALT_v$RegScore
reg_ALT_m<-plot_alo_ALT_m$RegScore
reg_JAR_p<-plot_alo_JAR_p$RegScore
reg_JAR_v<-plot_alo_JAR_v$RegScore
reg_JAR_m<-plot_alo_JAR_m$RegScore
reg_PUB_p<-plot_alo_PUB_p$RegScore
reg_PUB_v<-plot_alo_PUB_v$RegScore
reg_PUB_m<-plot_alo_PUB_m$RegScore

TTL_ALT_p<-log(Databy_Spp_p$BOTALT$ttlength)
TTL_ALT_v<-log(Databy_Spp_v$BOTALT$ttlength)
TTL_ALT_m<-log(Databy_Spp_m$BOTALT$ttlength)
TTL_JAR_p<-log(Databy_Spp_p$BOTJAR$ttlength)
TTL_JAR_v<-log(Databy_Spp_v$BOTJAR$ttlength)
TTL_JAR_m<-log(Databy_Spp_m$BOTJAR$ttlength)
TTL_PUB_p<-log(Databy_Spp_p$BOTPUB$ttlength)
TTL_PUB_v<-log(Databy_Spp_v$BOTPUB$ttlength)
TTL_PUB_m<-log(Databy_Spp_m$BOTPUB$ttlength)



# DIET SHAPE ANALYSIS 
df_alt_p<-data.frame(reg_ALT_p,TTL_ALT_p)
df_alt_v<-data.frame(reg_ALT_v,TTL_ALT_v)
df_alt_m<-data.frame(reg_ALT_m,TTL_ALT_m)
df_jar_p<-data.frame(reg_JAR_p,TTL_JAR_p)
df_jar_v<-data.frame(reg_JAR_v,TTL_JAR_v)
df_jar_m<-data.frame(reg_JAR_m,TTL_JAR_m)
df_pub_p<-data.frame(reg_PUB_p,TTL_PUB_p)
df_pub_v<-data.frame(reg_PUB_v,TTL_PUB_v)
df_pub_m<-data.frame(reg_PUB_m,TTL_PUB_m)


model_alt_p<-list(reg_ALT_p~1+TTL_ALT_p,~1+TTL_ALT_p)
model_alt_v<-list(reg_ALT_v~1+TTL_ALT_v,~1+TTL_ALT_v)
model_alt_m<-list(reg_ALT_m~1+TTL_ALT_m,~1+TTL_ALT_m)
model_jar_p<-list(reg_JAR_p~1+TTL_JAR_p,~1+TTL_JAR_p)
model_jar_v<-list(reg_JAR_v~1+TTL_JAR_v,~1+TTL_JAR_v)
model_jar_m<-list(reg_JAR_m~1+TTL_JAR_m,~1+TTL_JAR_m)
model_pub_p<-list(reg_PUB_p~1+TTL_PUB_p,~1+TTL_PUB_p)
model_pub_v<-list(reg_PUB_v~1+TTL_PUB_v,~1+TTL_PUB_v)
model_pub_m<-list(reg_PUB_m~1+TTL_PUB_m,~1+TTL_PUB_m)


library(mcp)
#Ajustando modelos com slope
alt_1<-mcp(model_alt_p,data=df_alt_p, iter=9999, chains=5)
alt_2<-mcp(model_alt_v,data=df_alt_v, iter=9999, chains=5)
alt_3<-mcp(model_alt_m,data=df_alt_m, iter=9999, chains=5)
jar_1<-mcp(model_jar_p,data=df_jar_p, iter=9999, chains=5)
jar_2<-mcp(model_jar_v,data=df_jar_v, iter=9999, chains=5)
jar_3<-mcp(model_jar_m,data=df_jar_m, iter=9999, chains=5)
pub_1<-mcp(model_pub_p,data=df_pub_p, iter=9999, chains=5)
pub_2<-mcp(model_pub_v,data=df_pub_v, iter=9999, chains=5)
pub_3<-mcp(model_pub_m,data=df_pub_m, iter=9999, chains=5)


#####################################################################################
#NULL MODELS
null_alt_1<-list(reg_ALT_p~1+TTL_ALT_p)
null_alt_2<-list(reg_ALT_v~1+TTL_ALT_v)
null_alt_3<-list(reg_ALT_m~1+TTL_ALT_m)

null_jar_1<-list(reg_JAR_p~1+TTL_JAR_p)
null_jar_2<-list(reg_JAR_v~1+TTL_JAR_v)
null_jar_3<-list(reg_JAR_m~1+TTL_JAR_m)

null_pub_1<-list(reg_PUB_p~1+TTL_PUB_p)
null_pub_2<-list(reg_PUB_v~1+TTL_PUB_v)
null_pub_3<-list(reg_PUB_m~1+TTL_PUB_m)

#FITANDO MODELOS NULOS
fit_null_alt_1 = mcp(null_alt_1, data = df_alt_p)  # fit another model here
fit_null_alt_2 = mcp(null_alt_2, data = df_alt_v) 
fit_null_alt_3 = mcp(null_alt_3, data = df_alt_m) 

fit_null_jar_1 = mcp(null_jar_1, data = df_jar_p)
fit_null_jar_2 = mcp(null_jar_2, data = df_jar_v)
fit_null_jar_3 = mcp(null_jar_3, data = df_jar_m)

fit_null_pub_1 = mcp(null_pub_1, data = df_pub_p)
fit_null_pub_2 = mcp(null_pub_2, data = df_pub_v)
fit_null_pub_3 = mcp(null_pub_3, data = df_pub_m)

###### COMPARANDO MODELOS ##################################################

alt_1$loo<-loo(alt_1)
alt_2$loo<-loo(alt_2)
alt_3$loo<-loo(alt_3)

jar_1$loo<-loo(jar_1)
jar_2$loo<-loo(jar_2)
jar_3$loo<-loo(jar_3)

pub_1$loo<-loo(pub_1)
pub_2$loo<-loo(pub_2)
pub_3$loo<-loo(pub_3)

?lm.rrpp()
fit_null_alt_1$loo<-loo(fit_null_alt_1)
fit_null_alt_2$loo<-loo(fit_null_alt_2)
fit_null_alt_3$loo<-loo(fit_null_alt_3)

fit_null_jar_1$loo<-loo(fit_null_jar_1)
fit_null_jar_2$loo<-loo(fit_null_jar_2)
fit_null_jar_3$loo<-loo(fit_null_jar_3)

fit_null_pub_1$loo<-loo(fit_null_pub_1)
fit_null_pub_2$loo<-loo(fit_null_pub_2)
fit_null_pub_3$loo<-loo(fit_null_pub_3)


comp_alt_1<-loo_compare(alt_1$loo,fit_null_alt_1$loo)#1
comp_alt_2<-loo_compare(alt_2$loo,fit_null_alt_2$loo)#1
comp_alt_3<-loo_compare(alt_3$loo,fit_null_alt_3$loo)#1

comp_jar_1<-loo_compare(jar_1$loo,fit_null_jar_1$loo)#2
comp_jar_2<-loo_compare(jar_2$loo,fit_null_jar_2$loo)#2
comp_jar_3<-loo_compare(jar_3$loo,fit_null_jar_3$loo)#1

comp_pub_1<-loo_compare(pub_1$loo,fit_null_pub_1$loo)#2
comp_pub_2<-loo_compare(pub_2$loo,fit_null_pub_2$loo)#2
comp_pub_3<-loo_compare(pub_3$loo,fit_null_pub_3$loo)#2
################################################################################################################
# Tabela modelos
col1_f<-c('1','1','1')
col2_f<-c('2','2','1')
col3_f<-c('2','2','2')

nam<-c("B.alt","B.jar","B.pub")
nomes_linhas <- c('Dorsal Skull View', 'Ventral Skull View', 'Mandible')

DF_ft_r<-data.frame(col1_f,col2_f,col3_f)
colnames(DF_ft_r)<-nam
row.names(DF_ft_r)<-nomes_linhas
DF_ft_r

## TABELA  ## com CHANGE POINTS 

regsum_alt_1<-summary(alt_1)
regsum_alt_2<-summary(alt_2)
regsum_alt_3<-summary(alt_3)
regsum_jar_1<-summary(jar_1)
regsum_jar_2<-summary(jar_2)
regsum_jar_3<-summary(jar_3)
regsum_pub_1<-summary(pub_1)
regsum_pub_2<-summary(pub_2)
regsum_pub_3<-summary(pub_3)


Col1<-c(regsum_alt_1[3,2],regsum_alt_2[3,2],regsum_alt_3[3,2])
Col2<-c(regsum_alt_1[3,3],regsum_alt_2[3,3],regsum_alt_3[3,3])
Col3<-c(regsum_alt_1[3,4],regsum_alt_2[3,4],regsum_alt_3[3,4])
Col4<-c(regsum_jar_1[3,2],regsum_jar_2[3,2],regsum_jar_3[3,2])
Col5<-c(regsum_jar_1[3,3],regsum_jar_2[3,3],regsum_jar_3[3,3])
Col6<-c(regsum_jar_1[3,4],regsum_jar_2[3,4],regsum_jar_3[3,4])
Col7<-c(regsum_pub_1[3,2],regsum_pub_2[3,2],regsum_pub_3[3,2])
Col8<-c(regsum_pub_1[3,3],regsum_pub_2[3,3],regsum_pub_3[3,3])
Col9<-c(regsum_pub_1[3,4],regsum_pub_2[3,4],regsum_pub_3[3,4])


DF_reg<-data.frame(Col1,
                   Col2,
                   Col3,
                   Col4,
                   Col5,
                   Col6,
                   Col7,
                   Col8,
                   Col9)


nomes_linhas <- c('Dorsal Skull View', 'Ventral Skull View', 'Mandible')
nomes_colunas <- c("B.alt(mean)", "B.alt(min)", "B.alt(max)", "B.jar(mean)", "B.jar(min)", "B.jar(max)", "B.pub(mean)", "B.pub(min)", "B.pub(max)")

colnames(DF_reg)<-nomes_colunas
row.names(DF_reg)<-nomes_linhas
DF_reg
##################################################################################################################################################################################



### Plotando gráficos ###

alt_1_gr-plot(alt_1)+geom_point(shape=21, size=(Databy_Spp_p$BOTALT$ttlength/20), col="black", fill="darkolivegreen3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
alt_2_gr<-plot(alt_2)+geom_point(shape=21, size=(Databy_Spp_v$BOTALT$ttlength/20), col="black", fill="darkolivegreen3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
alt_3_gr<-plot(alt_3)+geom_point(shape=21, size=(Databy_Spp_m$BOTALT$ttlength/20), col="black", fill="darkolivegreen3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
jar_1_gr<-plot(jar_1)+geom_point(shape=21, size=(Databy_Spp_p$BOTJAR$ttlength/20), col="black", fill="deepskyblue3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
jar_2_gr<-plot(jar_2)+geom_point(shape=21, size=(Databy_Spp_v$BOTJAR$ttlength/20), col="black", fill="deepskyblue3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
jar_3_gr<-plot(jar_3)+geom_point(shape=21, size=(Databy_Spp_m$BOTJAR$ttlength/20), col="black", fill="deepskyblue3")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
pub_1_gr<-plot(pub_1)+geom_point(shape=21, size=(Databy_Spp_p$BOTPUB$ttlength/20), col="black", fill="darkgoldenrod1")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
pub_2_gr<-plot(pub_2)+geom_point(shape=21, size=(Databy_Spp_v$BOTPUB$ttlength/20), col="black", fill="darkgoldenrod1")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)
pub_3_gr<-plot(pub_3)+geom_point(shape=21, size=(Databy_Spp_m$BOTPUB$ttlength/20), col="black", fill="darkgoldenrod1")+  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "Regression Score", size=2)


#############################################################################################################################################################################################################################
alt_1_gr
jar_1_gr
pub_1_gr
alt_3_gr
jar_3_gr
pub_3_gr
alt_2_gr
jar_2_gr
pub_2_gr
require(cowplot)

plots_shape_diet<-plot_grid(
  alt_1_gr,
  jar_1_gr,
  pub_1_gr,
  alt_2_gr,
  jar_2_gr,
  pub_2_gr,
  alt_3_gr,
  jar_3_gr,
  pub_3_gr, nrow=3, labels=c("A","B","C","D","E","F","G","H","I"))


pdf("Shape_diet.pdf", width = 15, height = 10)
plot(plots_shape_diet)
dev.off()




#models alternatus ####
alt_1<-mcp(model_1,data=da_alt, iter=9999, chains=5)

geom_point(aes(colour = Sp), shape=21,size=(data_m$Length/20), col="black",fill=data_m$Color.1,show.legend =FALSE) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(SVL)",
       y = "PC1 for fitted values", size=2)+ 
  scale_color_manual(values=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"))                     





#################################################################################################################################################################################################################
###############################   ###########################################################   ########################    #######################
#####################################################################################################################################################
##############################   ###########################################################   ########################    #######################
#####################################################################################################################################################

## PHENOTIPIC TRAJECTORY ANALYSIS  ##
#####################################

shape_2d_p<-two.d.array(shape_p)
shape_2d_v<-two.d.array(shape_v)
shape_2d_m<-two.d.array(shape_m)

col  <-read.table("test_class_p.txt", h=T)
col_v<-read.table("test_class_v.txt", h=T)
col_m<-read.table("test_class_m.txt", h=T)
col<-col[-9,]
col_v<-col_v[-9,]
dim(col  )
dim(col_v)
dim(col_m)

####################################################################################
## Removendo fonsecai ##
####################################################################################


col_s<-split(col  ,col$Sp!="BOTFON")
col_v_s<-split(col_v,col_v$Sp!="BOTFON")
col_m_s<-split(col_m,col_m$Sp!="BOTFON")

col<-col_s  $`TRUE`
col_v<-col_v_s$`TRUE`
col_m<-col_m_s$`TRUE`
col_m<-col_m[-8,]
####################################################################################
quartis_p<-tapply(plan_p$ttlength,plan_p$Sp, quantile,probs=c(0.25,0.5,0.75,1))
quartis_v<-tapply(plan_v$ttlength,plan_v$Sp, quantile,probs=c(0.25,0.5,0.75,1))
quartis_m<-tapply(plan_m$ttlength,plan_m$Sp, quantile,probs=c(0.25,0.5,0.75,1))


quartis_pteste<-tapply(plan_p$Length,plan_p$Sp, quantile,probs=c(0.25,0.5,0.75,1))

quartis_pteste$BOTALT



dim(col)
dim(plan_p)

####################################################################################

#Disparity age x group
#Dividindo grupos - smallest and largest - dataframe

#mandrake
plan_p<-data.frame(plan_p,col$Age.2)
plan_v<-data.frame(plan_v,col_v$Age.1)
plan_m<-data.frame(plan_m,col_m$Age.1)
#Criando coluna de interacao
plan_p$inter<-interaction(plan_p$Sp,plan_p$col.Age.2)
col_v$inter<-interaction(col_v$Sp,col_v$Age.1)
col_m$inter<-interaction(col_m$Sp,col_m$Age.1)

plan_p$No.2<-seq(nrow(plan_p))
plan_v$No.2<-seq(nrow(plan_v))
plan_m$No.2<-seq(nrow(plan_m))




#Separando quartis
quartile_cutoffs_p <- with(plan_p, split(ttlength, Sp) |> lapply(quantile, probs = 0.5))
quartile_cutoffs_v <- with(plan_v, split(ttlength, Sp) |> lapply(quantile, probs = 0.5))
quartile_cutoffs_m <- with(plan_m, split(ttlength, Sp) |> lapply(quantile, probs = 0.5))

#criando novo data frame com os minimos e maximo de tamanho 
novo_df_p<- subset(plan_p, ttlength <= 36 | (ttlength >= quartile_cutoffs_p[Sp]))
novo_df_v<- subset(plan_v, ttlength <= 36 | (ttlength >= quartile_cutoffs_v[Sp]))
novo_df_m<- subset(plan_m, ttlength <= 36 | (ttlength >= quartile_cutoffs_v[Sp]))


#criando novo TPS com os minimos e maximo de tamanho 
duplicated(plan_p$No)
plan_p$minmax<-ifelse(plan_p$No.2 %in% novo_df_p$No.2,"Y","N")
plan_v$minmax<-ifelse(plan_v$No.2 %in% novo_df_v$No.2,"Y","N")
plan_m$minmax<-ifelse(plan_m$No.2 %in% novo_df_m$No.2,"Y","N")

shape_p_prun<-coords.subset(shape_p,plan_p$minmax!="Y")
shape_v_prun<-coords.subset(shape_v,plan_v$minmax!="Y")
shape_n_prun<-coords.subset(shape_m,plan_m$minmax!="Y")

shape_p_pr<-shape_p_prun$`FALSE`
shape_v_pr<-shape_v_prun$`FALSE`
shape_m_pr<-shape_n_prun$`FALSE`

length(novo_df_p$No.2)
length(novo_df_v$No.2)
length(novo_df_m$No.2)


PCA_p<-gm.prcomp(shape_p_pr)
PCA_v<-gm.prcomp(shape_v_pr)
PCA_m<-gm.prcomp(shape_m_pr)


#AJUSTANDO MODELOS 
fit_morph_disp_p<-procD.lm(PCA_p$x[,1:2]~novo_df_p$col.Age.2.1)
fit_morph_disp_v<-procD.lm(PCA_v$x[,1:2]~novo_df_v$col_v.Age.1)
fit_morph_disp_m<-procD.lm(PCA_m$x[,1:2]~novo_df_m$col_m.Age.1)

tab_p<-summary(fit_morph_disp_p)
tab_v<-summary(fit_morph_disp_v)
tab_m<-summary(fit_morph_disp_m)

list_tabs<-list(tab_p,tab_v, tab_m)


#                      Df      SS       MS     Rsq      F      Z Pr(>F)   
#plan_p$Age             2 0.12985 0.064923 0.34872 41.447 5.8147  0.001 **
#plan_p$Sp              2 0.04551 0.022756 0.12223 14.527 9.9283  0.001 **
#plan_p$Age:plan_p$Sp   4 0.01216 0.003039 0.03265  1.940 3.7481  0.001 **
#Residuals            118 0.18484 0.001566 0.49641                        
#Total                126 0.37235 

#                      Df      SS       MS     Rsq       F      Z Pr(>F)   
#col_v$Age.1            1 0.12131 0.121308 0.17264 27.3143 7.4639  0.001 **
#col_v$Sp               2 0.05841 0.029207 0.08313  6.5764 6.7236  0.001 **
#col_v$Age.1:col_v$Sp   2 0.01665 0.008326 0.02370  1.8748 2.1604  0.017 * 
#Residuals            114 0.50629 0.004441 0.72053                         
#Total                119 0.70267

#                      Df      SS        MS     Rsq      F      Z Pr(>F)   
#col_m$Age.1            1 0.02713 0.0271329 0.08336 15.475 5.4651  0.001 **
#col_m$Sp               2 0.05315 0.0265747 0.16330 15.157 6.7386  0.001 **
#col_m$Age.1:col_m$Sp   2 0.03655 0.0182738 0.11229 10.422 5.6876  0.001 **
#Residuals            119 0.20865 0.0017533 0.64105                        
#Total                124 0.32548


###########################################################################################
#SETANDO INTERAÇÂO CORRETA
novo_df_p$int<-interaction(novo_df_p$Sp,novo_df_p$col.Age.2.1)
novo_df_v$int<-interaction(novo_df_v$Sp,novo_df_v$col_v.Age.1.1)
novo_df_m$int<-interaction(novo_df_m$Sp,novo_df_m$col_m.Age.1)



PW_p<-pairwise(fit_morph_disp_p, groups=novo_df_p$col.Age.2)
PW_v<-pairwise(fit_morph_disp_v, groups=novo_df_v$col_v.Age.1.1)
PW_m<-pairwise(fit_morph_disp_m, groups=novo_df_m$col_m.Age.1.1)

summary(PW_p, test="dist")
summary(PW_v, test="dist")
summary(PW_m, test="dist")

fit_p<-procD.lm(PCA_p$x[,1:2]~1)
fit_v<-procD.lm(PCA_v$x[,1:2]~1)
fit_m<-procD.lm(PCA_m$x[,1:2]~1)


MD_p1<-morphol.disparity(fit_p,groups=novo_df_p$col.Age.2, partial =TRUE)
MD_v1<-morphol.disparity(fit_v,groups=novo_df_v$col_v.Age.1, partial =TRUE)
MD_m1<-morphol.disparity(fit_m,groups=novo_df_m$col_m.Age.1, partial =TRUE)
summary(MD_p1)
summary(MD_v1)
summary(MD_m1)




MD_p<-morphol.disparity(fit_morph_disp_p, groups = novo_df_p$int)
MD_v<-morphol.disparity(fit_morph_disp_v, groups = novo_df_v$int)
MD_m<-morphol.disparity(fit_morph_disp_m, groups = novo_df_m$int)
summary(MD_p)
summary(MD_v)
summary(MD_m)




### Testando  trajectory analysis size #############################################
# Dorsal 
fit_size_p<- lm.rrpp(shape_2d_p ~ as.factor( plan_p$Sp)*col$Age.2 , iter = 999, print.progress = FALSE)
summary(fit_size_p)
TA_size_p <- trajectory.analysis(fit_size_p, groups =  plan_p$Sp, traj.pts = col$Age.2)
summary(TA_size_p, attribute = "MD")
summary(TA_size_p, attribute = "TC", angle.type = "deg") ### Correlations (angles) between trajectories
## Ventral 
fit_size_v <- lm.rrpp(shape_2d_v ~ as.factor(plan_v$Sp)*col_v$Age.1 , iter = 999, print.progress = FALSE)
TA_size_v <- trajectory.analysis(fit_size_v, groups =  plan_v$Sp, traj.pts =col_v$Age.1)
summary(TA_size_v, attribute = "MD")
summary(TA_size_v, attribute = "TC", angle.type = "deg") ### Correlations (angles) between trajectories
# Mandible 
fit_size_m <- lm.rrpp(shape_2d_m ~ as.factor(plan_m$Sp)*col_m$Age.1 , iter = 999, print.progress = FALSE)
TA_size_m <- trajectory.analysis(fit_size_m, groups =  plan_m$Sp, traj.pts =col_m$Age.1)
summary(TA_size_m, attribute = "MD")
summary(TA_size_m, attribute = "TC", angle.type = "deg") ### Correlations (angles) between trajectories

### Estes dados foram salvos manualmente ###

library(gridExtra)

# Definir o tamanho dos plots
widths <- c(1, 1, 1)  # largura proporcional de cada plot
heights <- c(2, 2, 2)  # altura proporcional de cada plot


library(gridExtra)

# Definir o tamanho dos plots
widths <- c(1, 1, 1)  # largura proporcional de cada plot
heights <- c(2, 2, 2)  # altura proporcional de cada plot

pdf("alo_traj_p.pdf", width = 8, height =7)
## PLOTANDO TRAJETORIAS FENOT?PICAS  
### TRAJECTORIES ANALYSIS _ DORSAL VIEW - Ficou igual 
TP_size_p <- plot(TA_size_p, pch = 21,bg=col$Color.1,cex = log(plan_p$Length/5), col= "black")
add.trajectories(TP_size_p, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_p$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)
dev.off()

pdf("alo_traj_v.pdf", width = 8, height = 7)
### TRAJECTORIES ANALYSIS _ VENTRAL VIEW - Ficou igual 
TP_size_v <- plot(TA_size_v, pch = 21,bg=col_v$Color.1,cex = log(plan_v$Length/5), col= "black")
add.trajectories(TP_size_v, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_v$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)
dev.off()

pdf("alo_traj_m.pdf", width = 8, height = 7)
### TRAJECTORIES ANALYSIS _ MANDIBLE VIEW - Ficou igual 
TP_size_m <- plot(TA_size_m, pch = 21,bg=col_m$Color.1,cex = log(plan_m$Length/5), col= "black")
add.trajectories(TP_size_m, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_m$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)
dev.off()
#######################################################################################################################################################################################################
#########   ###################   ############################   ###########################   ###############################    #########################  #####################    #################
#######################################################################################################################################################################################################

mean_p<-mshape(shape_p)
PCA_P<-TA_size_p$pca
PC1_p<-PCA_P$x[,1]
PC2_p<-PCA_P$x[,2]
sc.preds1_p<-shape.predictor(shape_p,x=PC1_p,Intercept = FALSE,pred1 = min(PC1_p), pred2 = max(PC1_p))
sc.preds2_p<-shape.predictor(shape_p,x=PC2_p,Intercept = FALSE,pred1 = min(PC2_p), pred2 = max(PC2_p))

mean_v<-mshape(shape_v)
PCA_V<-TA_size_v$pca
PC1_v<-PCA_V$x[,1]
PC2_v<-PCA_V$x[,2]
sc.preds1_v<-shape.predictor(shape_v,x=PC1_v,Intercept = FALSE,pred1 = min(PC1_v), pred2 = max(PC1_v))
sc.preds2_v<-shape.predictor(shape_v,x=PC2_v,Intercept = FALSE,pred1 = min(PC2_v), pred2 = max(PC2_v))

mean_m<-mshape(shape_m)
PCA_M<-TA_size_m$pca
PC1_m<-PCA_M$x[,1]
PC2_m<-PCA_M$x[,2]

sc.preds1_m<-shape.predictor(shape_m,x=PC1_m,Intercept = FALSE,pred1 = min(PC1_m), pred2 = max(PC1_m))
sc.preds2_m<-shape.predictor(shape_m,x=PC2_m,Intercept = FALSE,pred1 = min(PC2_m), pred2 = max(PC2_m))


GP0<-gridPar(pt.bg="black",link.col="black",link.lty=1,link.lwd=3) # cor dos landmarks e linksGP1<-gridPar(pt.bg="gray",link.col="gray",link.lty=1) # cor dos landmarks e links


pdf("Shapes_PCA_alotraj_p.pdf", width = 10, height = 10)
par(mfrow=c(2,2))
plotRefToTarget(mean_p, sc.preds1_p$pred1, mag=1,gridPars = GP0, links =Links_p,method="vector")
mtext("P1_Min")
plotRefToTarget(mean_p, sc.preds1_p$pred2, mag=1,gridPars= GP0, links =Links_p,method="vector")
mtext("P1_Max")
plotRefToTarget(mean_p, sc.preds2_p$pred1, mag=1,gridPars= GP0, links =Links_p,method="vector")
mtext("P2_Min")
plotRefToTarget(mean_p, sc.preds2_p$pred2, mag=1,gridPars= GP0, links =Links_p,method="vector")
mtext("P2_Max")
dev.off()

pdf("Shapes_PCA_alotraj_v.pdf", width = 10, height = 10)
par(mfrow=c(2,2))
plotRefToTarget(mean_v, sc.preds1_v$pred1, mag=1, links =Links_v,method="vector")
mtext("P1_Min")
plotRefToTarget(mean_v, sc.preds1_v$pred2, mag=1, links =Links_v,method="vector")
mtext("P1_Max")
plotRefToTarget(mean_v, sc.preds2_v$pred1, mag=1, links =Links_v,method="vector")
mtext("P2_Min")
plotRefToTarget(mean_v, sc.preds2_v$pred2, mag=1, links =Links_v,method="vector")
mtext("P2_Max")
dev.off()

pdf("Shapes_PCA_alotraj_m.pdf", width = 10, height = 10)
par(mfrow=c(2,2))
plotRefToTarget(mean_m, sc.preds1_m$pred1, mag=1, links =Links_m,method="vector")
mtext("P1_Min")
plotRefToTarget(mean_m, sc.preds1_m$pred2, mag=1, links =Links_m,method="vector")
mtext("P1_Max")
plotRefToTarget(mean_m, sc.preds2_m$pred1, mag=1, links =Links_m,method="vector")
mtext("P2_Min")
plotRefToTarget(mean_m, sc.preds2_m$pred2, mag=1, links =Links_m,method="vector")
mtext("P2_Max")
dev.off()




#######################################################################################################################################
library(dplyr)
DF_R_fitornull<-DF_ft_r

#DTA FRAMES ANALISES LINEARES
DF_linear
DF_L_fitornull
# Substituir valores iguais a 1 por "cp model" e valores iguais a 2 por null
#DF_L_fitornull <- transform(DF_L_fitornull,
#B.alt = ifelse(B.alt == 1, "cp model", "null"),
#B.jar = ifelse(B.jar == 1, "cp model", "null"),
#B.pub = ifelse(B.pub == 1, "cp model", "null"))

DF_linear
#      B.alt(mean) B.alt(min) B.alt(max) B.jar(mean) B.jar(min) B.jar(max) B.pub(mean) B.pub(min) B.pub(max)
#SkL    3.868154   3.552035   4.362775    3.625449   3.375558   3.881562    3.541847   3.157120   4.110908
#StL    3.783571   3.262959   4.470614    3.478371   3.277148   3.787240    3.709229   3.163213   4.526394
#QdL    3.673601   3.258101   4.732872    3.495958   3.277186   3.784696    3.715905   3.442011   4.058525
#EcL    3.286092   3.258334   3.311536    3.455806   3.284703   3.526359    3.370163   3.157009   3.770671
#PaL    3.401441   3.258137   3.642050    3.898735   3.474817   4.464650    3.332299   3.157150   3.586911
#PtL    3.739990   3.265147   4.548564    3.547564   3.277148   3.979626    3.328420   3.157105   3.367294
#MxL    3.955204   3.275547   4.818488    3.710455   3.331275   3.881555    3.673866   3.278824   4.598135
#MdL    3.592279   3.258143   4.040109    3.586612   3.278721   3.881561    3.951944   3.506678   4.445661



explff<-c(  3.541847,
            3.709229,
            3.715905,
            3.370163,
            3.332299,
            3.328420,
            3.673866,
            3.951944)

ddd<-exp(explff)
mean(ddd)
sd(ddd)



DF_L_fitornull
#      B.alt    B.jar    B.pub
#SkL cp model cp model     null
#StL     null     null     null
#QdL     null     null cp model
#EcL cp model     null     null
#PaL cp model cp model cp model
#PtL     null     null cp model
#MxL     null     null cp model
#MdL     null cp model cp model



#DTA FRAMES ANALISES SHAPE
DF_reg
DF_R_fitornull

#DF_R_fitornull <- transform(DF_R_fitornull,
#B.alt = ifelse(B.alt == 1, "cp model", "null"),
#B.jar = ifelse(B.jar == 1, "cp model", "null"),
#B.pub = ifelse(B.pub == 1, "cp model", "null"))

DF_reg

#                      B.alt(mean) B.alt(min) B.alt(max) B.jar(mean) B.jar(min) B.jar(max) B.pub(mean) B.pub(min) B.pub(max)
#Dorsal Skull View     4.099724   3.426721   4.451799    3.735500   3.281681   4.346661    3.713315   3.157101   4.102641
#Ventral Skull View    4.355492   3.652708   4.887197    3.828183   3.283752   4.620058    3.890778   3.164899   4.554766
#Mandible              4.142269   3.637501   4.398108    3.823519   3.277685   4.386266    3.772347   3.157001   4.540537


media_cp<-c(3.713315,
            3.890778,
            3.772347)

expon<-exp(media_cp)

mean(expon)
sd(expon)

log(42)
exp(4.199162)


DF_R_fitornull

#                    B.alt    B.jar B.pub
#Dorsal Skull View  cp model     null  null
#Ventral Skull View cp model     null  null
#Mandible           cp model cp model  null
?mcp
?loo_compare()

##############################################################################################################################################
#Agora temos que dividir o grupo em duas partes usando o slope medio e testar se o slope é maior ou menor do que o dos adultos 


#GRÁFICOS QUE VÂO ENTRAR NO ARTIGO 

#ALTERNATUS
slo_tb_res_alt_1l<-summary(alt_1l)
#      name        mean       lower     upper     Rhat n.eff
#1 Total_L_1  0.21815874 -0.06743541 0.5037440 1.046029    89
#2 Total_L_2  0.75135657  0.57532972 0.9099508 1.045238  1816
#3      cp_1  3.86815450  3.55203526 4.3627755 1.073805   235
#4     int_1 -0.37660530 -1.36690205 0.6026100 1.049515    90
#5     int_2  0.47118301  0.21531076 0.7645812 1.083343   262
#6   sigma_1  0.09570144  0.07343463 0.1199317 1.001068  7520

da_alt$sizegp<- ifelse(da_alt$Total_L<= 3.86815450,"S", "B"  )
linear_ALT_SkL<-lm(da_alt$C_L~da_alt$Total_L*da_alt$sizegp)
summary(linear_ALT_SkL)

anov_ALT_SkL<-aov(linear_ALT_SkL)

summary(anov_ALT_SkL)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   -2.62597    0.30459  -8.621 2.79e-10 ***
# da_alt$Total_L                 0.79333    0.06811  11.648 9.08e-14 ***
# da_alt$sizegpS                 1.53919    0.72563   2.121   0.0409 *  
# da_alt$Total_L:da_alt$sizegpS -0.37224    0.19964  -1.865   0.0704


anov







################################################################################################

slo_tb_res_alt_4l<-summary(alt_4l)

#   name  mean lower upper    Rhat n.eff
#Total_L_1 -0.17 -0.70  0.35    1   306
#Total_L_2  0.75  0.67  0.83    1  2852
#cp_1       3.29  3.26  3.31    1 14947
#int_1     -0.65 -2.30  1.11    1   306
#int_2     -0.51 -0.60 -0.42    1  2844
#sigma_1    0.12  0.09  0.14    1 19887

da_alt$sizegp<- ifelse(da_alt$Total_L<= 3.29,"S", "B"  )
linear_ALT_EcL<-lm(da_alt$EcP_L~da_alt$Total_L*da_alt$sizegp)
summary(linear_ALT_EcL)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    -3.0061     0.1783  -16.86  < 2e-16 ***
#da_alt$Total_L                  0.7570     0.0415   18.24  < 2e-16 ***
#da_alt$sizegpS                 -0.6643     0.1221   -5.44 3.59e-06 ***
#da_alt$Total_L:da_alt$sizegpS       NA         NA      NA       NA  


anov_ALT_EcL<-aov(linear_ALT_EcL)

summary(anov_ALT_EcL)



###############################################################################################

slo_tb_res_alt_4l<-summary(alt_5l)

#Population-level parameters:
#  name  mean lower upper Rhat n.eff
#Total_L_1 -0.26 -1.27  0.61  1.1   314
#Total_L_2  0.88  0.59  1.16  1.0  2066
#cp_1  3.40  3.26  3.64  1.2   281
#int_1 -1.99 -4.67  1.49  1.0   343
#int_2 -2.13 -2.49 -1.82  1.0  1477
#sigma_1  0.31  0.24  0.39  1.0  8972

da_alt$sizegp<- ifelse(da_alt$Total_L<= 3.40,"S", "B"  )
linear_ALT_PaL<-lm(da_alt$palatine_L~da_alt$Total_L*da_alt$sizegp)
summary(linear_ALT_PaL)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    -5.2929     0.5132 -10.314 2.71e-12 ***
#  da_alt$Total_L                0.9177     0.1188   7.725 3.76e-09 ***
#  da_alt$sizegpS                2.3925    25.0746   0.095    0.925    
#da_alt$Total_L:da_alt$sizegpS  -0.9177     7.6295  -0.120    0.905 


summary(linear_ALT_PaL)

anov__ALT_PaL<-aov(linear_ALT_PaL)

summary(anov__ALT_PaL)



plot_test<-ggplot(da_alt, aes(y=(C_L), x=Total_L, group=da_alt$sizegp))+
  stat_smooth(method = "lm", se = FALSE, size = 1.5) +
  geom_point(aes(colour=sizegp), shape=da_alt$sizegp,size=(da_alt$Total_L*15/20), col="black",fill="black",show.legend =FALSE)



################################################################################################################################


#JARARACA 
slo_tb_res_jar_1l<-summary(jar_1l)

#Population-level parameters:
#             name   mean  lower upper Rhat n.eff
#Total_L_1  0.160 -0.068 0.414  1.7         23
#Total_L_2  0.689  0.624 0.771  1.0       1013
#cp_1       3.625  3.376 3.882  1.2         77
#int_1      -0.356 -1.221 0.415  1.7        22
#int_2        0.258  0.099 0.447  1.2       93
#sigma_1     0.041  0.032 0.051  1.0      14405



da_jar$sizegp<- ifelse(da_jar$Total_L<= 3.625,"S", "B"  )
da_jar_split<-split(da_jar,da_jar$sizegp )

linear_test_c<-lm(da_jar$C_L~da_jar$Total_L*da_jar$sizegp)
summary(linear_test_c)

anov__test_c<-aov(linear_test_c)
summary(anov__test_c)




#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   -2.33746    0.16414 -14.241  < 2e-16 ***
#da_jar$Total_L                 0.71028    0.03691  19.242  < 2e-16 ***
#da_jar$sizegpS                 1.71848    0.43014   3.995 0.000270 ***
#da_jar$Total_L:da_jar$sizegpS -0.47260    0.12172  -3.883 0.000378 ***



#setando nova cor
da_jar$cor<-ifelse(da_jar$sizegp=="S","deepskyblue3","deepskyblue")

plot_test_jar<-ggplot(da_jar, aes(y=(C_L), x=Total_L, group=da_jar$sizegp))+
  stat_smooth(aes(colour=sizegp),method = "lm", se = FALSE, size = 1.5,show.legend = FALSE) +
  geom_point(aes(colour=sizegp), shape=21,size=(da_jar$Total_L*15/20), col="black",fill=da_jar$cor,show.legend =FALSE)+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(TTL)",
       y = "log(SkL)", size=2)+ 
  scale_color_manual(values=c("deepskyblue","deepskyblue3")) 


########################################################################################################################################################

slo_tb_res_jar_5l<-summary(jar_5l)

#Population-level parameters:
#           name  mean lower upper Rhat n.eff
#Total_L_1  0.24 -0.36  0.80  1.1    46
#Total_L_2  1.22  0.63  1.73  1.1   743
#cp_1       3.90  3.47  4.46  1.2   128
#int_1     -2.71 -4.65 -0.68  1.1    45
#int_2     -1.62 -2.15 -0.88  1.2   142
#sigma_1    0.17  0.13  0.21  1.0  9732

da_jar1<-da_jar
da_jar1$sizegp<- ifelse(da_jar1$Total_L<= 3.90,"S", "B"  )
linear_jar_Pal<-lm(da_jar$palatine_L~da_jar$Total_L*da_jar$sizegp)
summary(linear_jar_Pal)
da_jar1$cor<-ifelse(da_jar1$sizegp=="S","deepskyblue3","deepskyblue")

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    -7.2391     0.7897  -9.167 2.23e-11 ***
#da_jar$Total_L                  1.4165     0.1769   8.007 7.73e-10 ***
#da_jar$sizegpS                  3.4652     1.2810   2.705  0.00998 ** 
#da_jar$Total_L:da_jar$sizegpS  -0.8667     0.3408  -2.543  0.01496 *  

plot_jar_pal<-ggplot(da_jar1, aes(y=(palatine_L), x=Total_L, group=sizegp))+
  stat_smooth(aes(colour = sizegp),method = "lm", se = FALSE, size = 1.5,show.legend = FALSE) +
  geom_point(aes(colour=sizegp), shape=21,size=(da_jar$Total_L*15/20), col="black",fill=da_jar1$cor,show.legend =FALSE)
theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))


plot_jar_pal <- plot_jar_pal+scale_color_manual(values=c("deepskyblue","deepskyblue3")) +
  labs(title ="", subtitle ="", x = "log(TTL)",y = "log(PaL)", size=2)



linear_jar_Pal<-aov(linear_jar_Pal)
summary(linear_jar_Pal)



#########################################################################################################################################################

slo_tb_res_jar_8l<-summary(jar_8l)

#Population-level parameters:
#    name   mean  lower upper Rhat n.eff
#Total_L_1  0.222 -0.121 0.486  1.9    39
#Total_L_2  0.775  0.694 0.879  1.1  1136
#cp_1       3.587  3.279 3.882  1.5    84
#int_1     -0.363 -1.272 0.770  1.9    38
#int_2      0.469  0.274 0.711  1.4   100
#sigma_1    0.055  0.043 0.068  1.0 16260
da_jar2 <-da_jar
da_jar2$sizegp<- ifelse(da_jar2$Total_L<= 3.6,"S", "B"  )
linear_jar_Mnd<-lm(da_jar2$Man_L~da_jar2$Total_L*da_jar2$sizegp)
summary(linear_jar_Mnd)

da_jar2$cor<-ifelse(da_jar2$sizegp=="S","deepskyblue3","deepskyblue")


#Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   -2.37755    0.17321 -13.726   <2e-16 ***
#da_jar$Total_L                 0.78916    0.03917  20.150   <2e-16 ***
#da_jar$sizegpS                 1.50050    0.64584   2.323   0.0253 *  
#da_jar$Total_L:da_jar$sizegpS -0.41579    0.18644  -2.230   0.0314 *


plot_<-ggplot(da_jar, aes(y=(Man_L), x=Total_L, group=da_jar$sizegp))+
  stat_smooth(method = "lm", se = FALSE, size = 1.5) +
  geom_point(aes(colour=sizegp), shape=da_jar$sizegp,size=(da_jar$Total_L*15/20), col="black",fill="black",show.legend =FALSE)



plot_jar_Mnd<-ggplot(da_jar2, aes(y=(Man_L), x=Total_L, group=da_jar2$sizegp))+
  stat_smooth(aes(colour=sizegp),method = "lm", se = FALSE, size = 1.5,show.legend = FALSE) +
  geom_point(aes(colour=sizegp), shape=21,size=(da_jar$Total_L*15/20), col="black",fill=da_jar2$cor,show.legend =FALSE)+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(TTL)",
       y = "log(MdL)", size=2)+ 
  scale_color_manual(values=c("deepskyblue","deepskyblue3")) 



aovMDL<-aov(linear_jar_Mnd)
summary(aovMDL)




####################################################################################################################################################################


slo_tb_res_pub_3l<-summary(pub_3l)

#Population-level parameters:
#    name  mean  lower upper Rhat n.eff
#Total_L_1  0.17 -0.235  0.56  1.1    63
#Total_L_2  1.18  0.909  1.44  1.0  1456
#cp_1       3.72  3.442  4.06  1.0   194
#int_1     -1.54 -2.838 -0.17  1.1    61
#int_2     -0.85 -1.182 -0.43  1.0   201
#sigma_1    0.13  0.097  0.16  1.0  8425

da_pub$sizegp<- ifelse(da_pub$Total_L<= 3.72,"S", "B"  )
linear_pub_QdL<-lm(da_pub$Q_L~da_pub$Total_L*da_pub$sizegp)

summary(linear_pub_QdL)

avoPQDL<-aov(linear_pub_QdL)
summary(avoPQDL)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                    -5.5079     0.5050 -10.907 2.90e-13 ***
#  da_pub$Total_L                  1.2437     0.1172  10.608 6.45e-13 ***
#  da_pub$sizegpS                  3.1075     1.0126   3.069  0.00395 ** 
#  da_pub$Total_L:da_pub$sizegpS  -0.8155     0.2865  -2.846  0.00709 ** 

da_pub$cor<-ifelse(da_pub$sizegp=="S","darkgoldenrod","darkgoldenrod1")

plot_pub_QL<-ggplot(da_pub, aes(y=(Q_L), x=Total_L, group=da_pub$sizegp))+
  stat_smooth(aes(colour=sizegp),method = "lm", se = FALSE, size = 1.5,show.legend = FALSE) +
  geom_point(aes(colour=sizegp), shape=21,size=(da_pub$Total_L*15/20), col="black",fill=da_pub$cor,show.legend =FALSE)+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(TTL)", y = "log(QdL)", size=2)+
  scale_color_manual(values=c("darkgoldenrod1","darkgoldenrod")) 


####################################################################################################################################################################################

slo_tb_res_pub_5l<-summary(pub_5l)
#Population-level parameters:
#     name  mean lower upper Rhat n.eff
#Total_L_1 -0.28 -1.12  0.45  1.0   147
#Total_L_2  1.31  1.07  1.54  1.0  1688
#cp_1       3.33  3.16  3.59  1.1   480
#int_1     -1.43 -3.79  1.25  1.1   143
#int_2     -2.45 -2.74 -2.03  1.0   456
#sigma_1    0.20  0.16  0.25  1.0 10324

da_pub1<-da_pub

da_pub1$sizegp<- ifelse(da_pub1$Total_L<= 3.33,"S", "B"  )
linear_pub_Pal<-lm(da_pub1$palatine_L~da_pub1$Total_L*da_pub1$sizegp)

summary(linear_pub_Pal)

# Coefficients:
# #Estimate Std. Error t value Pr(>|t|)    
# #(Intercept)                    -6.8282     0.4497 -15.184  < 2e-16 ***
# #da_pub$Total_L                  1.3133     0.1064  12.340 7.29e-15 ***
# #da_pub$sizegpS                 11.8037     5.0689   2.329   0.0253 *  
# #da_pub$Total_L:da_pub$sizegpS  -3.5952     1.5525  -2.316   0.0261 *  
# #---
# #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot_pub_PaL<-ggplot(da_pub1, aes(y=(palatine_L), x=Total_L, group=sizegp))+
  stat_smooth(method = "lm", se = FALSE, size = 1.5) +
  geom_point(aes(colour=sizegp), shape=da_pub1$sizegp,size=(da_pub1$Total_L*15/20), col="black",fill="black",show.legend =FALSE)

geom_point(aes(colour=sizegp), shape=21,size=(da_pub$Total_L*15/20), col="black",fill=da_pub$cor,show.legend =FALSE)+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(TTL)", y = "log(QdL)", size=2)+
  scale_color_manual(values=c("darkgoldenrod1","darkgoldenrod")) 






####################################################################################################################################################################################

slo_tb_res_pub_6l<-summary(pub_6l)

#Population-level parameters:
#  name    mean lower upper Rhat n.eff
#Total_L_1 -0.063 -0.71  0.66  1.3    65
#Total_L_2  1.074  0.92  1.24  1.0  1288
#cp_1       3.328  3.16  3.37  1.4   427
#int_1     -0.543 -2.90  1.49  1.3    66
#int_2     -0.447 -0.79 -0.25  1.2   589
#sigma_1    0.134  0.10  0.17  1.0  8025
da_pub2<-da_pub
da_pub2$sizegp<- ifelse(da_pub$Total_L<= 3.328,"S", "B"  )
linear_pub_Ptl<-lm(da_pub2$Ptotal_length~da_pub2$Total_L*da_pub2$sizegp)
summary(linear_pub_Ptl)

aovptl<-aov(linear_pub_Ptl)
summary(aovptl)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   -4.01468    0.29639 -13.545    4e-16 ***
#da_pub$Total_L                 1.07335    0.07014  15.302   <2e-16 ***
#da_pub$sizegpS                 1.79722    3.34077   0.538    0.594    
#da_pub$Total_L:da_pub$sizegpS -0.61964    1.02322  -0.606    0.548  

#################################################################################################################################################################

slo_tb_res_pub_7l<-summary(pub_7l)

#Population-level parameters:
#    name  mean lower upper Rhat n.eff
#Total_L_1  0.10 -0.39  0.88  1.6    55
#Total_L_2  0.89 -0.11  1.30  1.2   746
#cp_1       3.67  3.28  4.60  1.4   224
#int_1     -1.78 -4.40 -0.13  1.6    54
#int_2     -1.24 -1.64 -0.24  1.4   301
#sigma_1    0.12  0.09  0.14  1.0 14245

da_pub$sizegp<- ifelse(da_pub$Total_L<= 3.67,"S", "B"  )
linear_pub_Mxl<-lm(da_pub$ML~da_pub$Total_L*da_pub$sizegp)
summary(linear_pub_Mxl)
aovmxl<-aov(linear_pub_Mxl)
summary(aovmxl)


#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   -5.20915    0.39869 -13.066 1.24e-15 ***
#da_pub$Total_L                 1.06314    0.09293  11.440 7.14e-14 ***
#da_pub$sizegpS                 2.49618    1.30908   1.907   0.0641 .  
#da_pub$Total_L:da_pub$sizegpS -0.67899    0.38707  -1.754   0.0875 .

########################################################################################################################################################################


slo_tb_res_pub_8l<-summary(pub_8l)


#Population-level parameters:
#     name   mean  lower upper Rhat n.eff
#Total_L_1  0.464  0.038 0.695  1.6    59
#Total_L_2  0.981  0.692 1.292  1.2  1012
#cp_1       3.952  3.507 4.446  1.4    84
#int_1     -1.140 -1.924 0.261  1.6    60
#int_2      0.698  0.268 1.046  1.3   103
#sigma_1    0.063  0.048 0.081  1.1  3042
da_pub4<-da_pub
da_pub4$sizegp<- ifelse(da_pub4$Total_L<= 3.952,"S", "B"  )
linear_pub_Man_L<-lm(da_pub4$Man_L~da_pub4$Total_L*da_pub4$sizegp)
summary(linear_pub_Man_L)


aovMdlp<-aov(linear_pub_Man_L)
summary(aovMdlp)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   -3.49740    0.31852 -10.980 2.38e-13 ***
#da_pub$Total_L                 1.05241    0.07346  14.326  < 2e-16 ***
#da_pub$sizegpS                 2.01319    0.40589   4.960 1.50e-05 ***
#da_pub$Total_L:da_pub$sizegpS -0.48626    0.10357  -4.695 3.42e-05 ***

da_pub4$cor<-ifelse(da_pub4$sizegp=="S","darkgoldenrod","darkgoldenrod1")

plot_pub_Man_L<-ggplot(da_pub4, aes(y=(Man_L), x=Total_L, group=da_pub4$sizegp))+
  stat_smooth(aes(colour=sizegp),method = "lm", se = FALSE, size = 1.5,show.legend = FALSE) +
  geom_point(aes(colour=sizegp), shape=21,size=(da_pub$Total_L*15/20), col="black",fill=da_pub4$cor,show.legend =FALSE)+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MdL)", size=2)+
  scale_color_manual(values=c("darkgoldenrod1","darkgoldenrod")) 








######################################################################################################################################################################################
alt_1
alt_2
alt_3


jar_3


#4.099724     3.637501
#4.355492
#4.142269

df_alt_p$gpsize<-ifelse(df_alt_p$TTL_ALT_p<=4.099724, "S","B")
df_alt_v$gpsize<-ifelse(df_alt_v$TTL_ALT_v<=4.355492, "S","B")
df_alt_m$gpsize<-ifelse(df_alt_m$TTL_ALT_m<=4.142269, "S","B")


df_jar_p$gpsize<-ifelse(df_jar_p$TTL_JAR_p<=3.637501, "S","B")

lin_alt_p<-lm(df_alt_p$reg_ALT_p~df_alt_p$TTL_ALT_p*df_alt_p$gpsize)
lin_alt_v<-lm(df_alt_v$reg_ALT_v~df_alt_v$TTL_ALT_v*df_alt_v$gpsize)
lin_alt_m<-lm(df_alt_m$reg_ALT_m~df_alt_m$TTL_ALT_m*df_alt_m$gpsize)
lin_jar_p<-lm(df_jar_p$reg_JAR_p~df_jar_p$TTL_JAR_p*df_jar_p$gpsize)

summary(lin_alt_p)
summary(lin_alt_v)
summary(lin_alt_m)
summary(lin_jar_p)


anov_alt_p<-aov(lin_alt_p)
anov_alt_v<-aov(lin_alt_v)
anov_alt_m<-aov(lin_alt_m)
anov_jar_p<-aov(lin_jar_p)

summary(anov_alt_p)
summary(anov_alt_v)
summary(anov_alt_m)
summary(anov_jar_p)





#Coefficients:
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)                          0.050616   0.043811   1.155    0.256
#df_alt_p$TTL_ALT_p                  -0.012433   0.009743  -1.276    0.210
#df_alt_p$gpsizeS                    -0.007752   0.088287  -0.088    0.931
#df_alt_p$TTL_ALT_p:df_alt_p$gpsizeS  0.005321   0.023499   0.226    0.822

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)                         -0.12439    0.08151  -1.526    0.137
#df_alt_v$TTL_ALT_v                   0.02868    0.01773   1.617    0.116
#df_alt_v$gpsizeS                     0.11385    0.08930   1.275    0.212
#df_alt_v$TTL_ALT_v:df_alt_v$gpsizeS -0.02850    0.02004  -1.422    0.165

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)                          0.011642   0.022588   0.515    0.610
#df_alt_m$TTL_ALT_m                  -0.003364   0.004988  -0.674    0.505
#df_alt_m$gpsizeS                     0.019190   0.028297   0.678    0.502
#df_alt_m$TTL_ALT_m:df_alt_m$gpsizeS -0.002878   0.006782  -0.424    0.674

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                         -0.049562   0.013528  -3.664 0.000706 ***
#  df_jar_p$TTL_JAR_p                   0.012373   0.003042   4.067 0.000211 ***
#  df_jar_p$gpsizeS                    -0.018644   0.034397  -0.542 0.590741    
#df_jar_p$TTL_JAR_p:df_jar_p$gpsizeS  0.004005   0.009696   0.413 0.681729   




##########################################################################################################################################
## PLOTANDO TRAJETORIAS FENOT?PICAS  
### TRAJECTORIES ANALYSIS _ DORSAL VIEW - Ficou igual 
TP_size_p <- plot(TA_size_p, pch = 21,bg=col$Color.1,cex = log(plan_p$Length/5), col= "black")
add.trajectories(TP_size_p, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_p$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)



### TRAJECTORIES ANALYSIS _ VENTRAL VIEW - Ficou igual 
TP_size_v <- plot(TA_size_v, pch = 21,bg=col_v$Color.1,cex = log(plan_v$Length/5), col= "black")
add.trajectories(TP_size_v, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_v$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)
dev.off()

### TRAJECTORIES ANALYSIS _ MANDIBLE VIEW - Ficou igual 
TP_size_m <- plot(TA_size_m, pch = 21,bg=col_m$Color.1,cex = log(plan_m$Length/5), col= "black")
add.trajectories(TP_size_m, traj.pch = c(21,21,21) ,start.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 end.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),
                 traj.col=c("darkolivegreen3","deepskyblue3","darkgoldenrod1"),traj.cex = 2.3, traj.lwd = 3, traj.lty = 8)
#legend("topright", levels(as.factor(plan_m$Sp)), pch =  c(21, 21,21,21), pt.bg =c("darkolivegreen3","deepskyblue3","darkgoldenrod1"), pt.cex = 2)
