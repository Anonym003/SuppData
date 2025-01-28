
#loading packages
library(mcp)
library(geomorph)
library(ggplot2)
library(cowplot)

#Loading data set
data<-readRDS("")

#Splinting data by Species 
split_data<-split(data,data$Species)

#Setting df
dfa<-split_data$alternatus
dfj<-split_data$jararaca
dfp<-split_data$pubescens

#Testing for sexual dimorphism
shapes_a<-data.frame(dfa[13:16],dfa[17:20])
shapes_j<-data.frame(dfj[13:16],dfj[17:20])
shapes_p<-data.frame(dfp[13:16],dfp[17:20])

#MANCOVA size and sex
res_dfa<-procD.lm(as.matrix(log(shapes_a))~log(dfa$SVL)*dfa$Sex)
res_dfj<-procD.lm(as.matrix(log(shapes_j))~log(dfj$SVL)*dfj$Sex)
res_dfp<-procD.lm(as.matrix(log(shapes_p))~log(dfp$SVL)*dfp$Sex)

summary(res_dfa)
summary(res_dfj)
summary(res_dfp)

#Building list for results
aov_resultados_SPPxSPP_SkL <- vector("list", length(data))

# Loop for para realizar a análise de variância em cada coluna
for (i in 1:length(data)) {
  # Obter a informação da variável y e da espécie
  variaveis_y <- colnames(data)[14:20]
  variavel_x <- colnames(data)[13]
  especie <- colnames(data)[27]
  
  # Selecionar as colunas relevantes para a análise
  y <- data[, 14:20]
  x <- data[, 13]
  z <- data[, 27]
  
  # Loop for para realizar a análise de variância em cada coluna
  col_resultados <- vector("list", ncol(y))
  for (j in 1:ncol(y)) {
    col_resultados[[j]] <- summary(aov(log(y[, j]) ~ log(x) * z))
  }
  
  # Armazenar os resultados da análise para o data frame atual
  aov_resultados_SPPxSPP_SkL[[i]] <- list(variaveis_y = variaveis_y, variavel_x = variavel_x, especie = especie, resultados = col_resultados)
}

#SPP X SPP - SVL
lm_resultados_SPPxSPP_SVL <- vector("list", length(data))

# Loop for para realizar a análise de variância em cada coluna
for (i in 1:length(data)) {
  # Obter a informação da variável y e da espécie
  variaveis_y <- colnames(data)[13:20]
  variavel_x <- colnames(data)[11]
  especie <- colnames(data)[27]
  
  # Selecionar as colunas relevantes para a análise
  y <- data[, 13:20]
  x <- data[, 11]
  z <- data[, 27]
  
  # Loop for para realizar a análise de variância em cada coluna
  col_resultados <- vector("list", ncol(y))
  for (j in 1:ncol(y)) {
    col_resultados[[j]] <- lm(log(y[, j]) ~ log(x) + z)
  }
  
  # Armazenar os resultados da análise para o data frame atual
  lm_resultados_SPPxSPP_SVL[[i]] <- list(variaveis_y = variaveis_y, variavel_x = variavel_x, especie = especie, resultados = col_resultados)
}

#####################################################################################################################################################################
#Verificando onde deu interação 
aov_resultados_SPPxSPP_SkL
#MxL =6
#Pterygoid =5
aov_resultados_SPPxSPP_SVL
#MxL=7
#Pterygoid=6
#Palatine=5

# PAIRWISE SLOPE
#MXL6
mxskl<-lm_resultados_SPPxSPP_SkL[[27]]$resultados[[6]]
ptskl<-lm_resultados_SPPxSPP_SkL[[27]]$resultados[[5]]


(PW_mxskl<-emtrends(mxskl,"z", var= 'x' ))
(PW_ptskl<-emtrends(ptskl,"z", var= "x"))

pairs(PW_mxskl)
pairs(PW_ptskl)

#slopes

#z   x.trend     SE  df lower.CL upper.CL
#alt   0.740 0.0336 120    0.674    0.806
#jar   0.788 0.0302 120    0.728    0.848
#pub   0.654 0.0339 120    0.587    0.721
#

#z   x.trend     SE  df lower.CL upper.CL
#alt   0.585 0.0412 120    0.503    0.667
#jar   0.627 0.0371 120    0.553    0.700
#pub   0.901 0.0416 120    0.819    0.984



#> pairs(PW_mxskl)
#contrast  estimate     SE  df t.ratio p.value
#alt - jar  -0.0480 0.0452 120  -1.062  0.5392
#alt - pub   0.0863 0.0477 120   1.809  0.1711
#jar - pub   0.1342 0.0454 120   2.958  0.0104
#
#P value adjustment: tukey method for comparing a family of 3 estimates 

#> pairs(PW_ptskl)
#contrast  estimate     SE  df t.ratio p.value
#alt - jar  -0.0418 0.0554 120  -0.755  0.7313
#alt - pub  -0.3165 0.0585 120  -5.406  <.0001
#jar - pub  -0.2747 0.0557 120  -4.931  <.0001
#
#P value adjustment: tukey method for comparing a family of 3 estimates 


########################################################################################################

plsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[5]]
ptsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[6]]
mxsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[7]]


(PW_plsvl<-emtrends(plsvl,"z", var= 'x' ))
(PW_ptsvl<-emtrends(ptsvl,"z", var= "x"))
(PW_mxsvl<-emtrends(mxsvl,"z", var= "x"))

pairs(PW_plsvl)
pairs(PW_ptsvl) 
pairs(PW_mxsvl) 

####################
#z   x.trend     SE  df lower.CL upper.CL
#alt   0.464 0.0214 120    0.422    0.506
#jar   0.488 0.0189 120    0.450    0.525
#pub   0.412 0.0220 120    0.368    0.455
####################
#z   x.trend     SE  df lower.CL upper.CL
#alt   0.373 0.0240 120    0.325    0.420
#jar   0.386 0.0212 120    0.344    0.428
#pub   0.581 0.0246 120    0.533    0.630
####################
#z   x.trend     SE  df lower.CL upper.CL
#alt   0.509 0.0388 120    0.432    0.586
#jar   0.466 0.0343 120    0.398    0.534
#pub   0.607 0.0398 120    0.528    0.686
####################


#> pairs(PW_plsvl)
#contrast  estimate     SE  df t.ratio p.value
#alt - jar  -0.0238 0.0286 120  -0.831  0.6845
#alt - pub   0.0521 0.0307 120   1.700  0.2092
#jar - pub   0.0759 0.0290 120   2.617  0.0268
#
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(PW_ptsvl) 
#contrast  estimate     SE  df t.ratio p.value
#alt - jar  -0.0136 0.0320 120  -0.425  0.9051
#alt - pub  -0.2087 0.0343 120  -6.081  <.0001
#jar - pub  -0.1951 0.0324 120  -6.012  <.0001
#
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(PW_mxsvl) 
#contrast  estimate     SE  df t.ratio p.value
#alt - jar   0.0432 0.0518 120   0.834  0.6829
#alt - pub  -0.0977 0.0556 120  -1.756  0.1890
#jar - pub  -0.1409 0.0526 120  -2.679  0.0228
#
#P value adjustment: tukey method for comparing a family of 3 estimates 



# PAIRWISE Intercept
sqlskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[1]]
qdlskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[2]]
ecpskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[3]]
pllskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[4]]
#ptlskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[5]]
#mxlskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[6]]
mslskl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[7]]

#PAIRWISE Intercept _ SKL
pwisqlskl<-emmeans(sqlskl,'z')
pwiqdlskl<-emmeans(qdlskl,'z')
pwiecpskl<-emmeans(ecpskl,'z')
pwipllskl<-emmeans(pllskl,'z')
pwimslskl<-emmeans(mslskl,'z')
pairs(pwisqlskl)
pairs(pwiqdlskl)
pairs(pwiecpskl)
pairs(pwipllskl)
pairs(pwimslskl)

### RESULTADOS 

#contrast  estimate     SE  df t.ratio p.value
#alt - jar   0.0869 0.0156 122   5.579  <.0001
#alt - pub   0.0673 0.0159 122   4.237  0.0001
#jar - pub  -0.0197 0.0153 122  -1.285  0.4064
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 


#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwiqdlskl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar   0.1494 0.0285 122   5.251  <.0001
# alt - pub   0.1575 0.0290 122   5.435  <.0001
# jar - pub   0.0081 0.0280 122   0.290  0.9548
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwiecpskl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar  0.17525 0.0281 122   6.228  <.0001
# alt - pub  0.17142 0.0287 122   5.981  <.0001
# jar - pub -0.00383 0.0277 122  -0.139  0.9895
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwipllskl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar  0.16048 0.0251 122   6.397  <.0001
# alt - pub  0.15696 0.0256 122   6.142  <.0001
# jar - pub -0.00352 0.0247 122  -0.143  0.9888
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwimslskl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar   0.1368 0.0291 122   4.694  <.0001
# alt - pub   0.0728 0.0297 122   2.452  0.0410
# jar - pub  -0.0640 0.0286 122  -2.234  0.0696
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
###########################################################################################################################

sklsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[1]]
sqlsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[2]]
qdlsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[3]]
ecpsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[4]]
#pllsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[5]]
#ptlsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[6]]
#mxlsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[7]]
mslsvl<-lm_resultados_SPPxSPP_SVL[[27]]$resultados[[8]]

#################################################################################################################################################################
#PAIRWISE Intercept _ SVL

pwisklsvl<-emmeans(sklsvl,'z')
pwisqlsvl<-emmeans(sqlsvl,'z')
pwiqdlsvl<-emmeans(qdlsvl,'z')
pwiecpsvl<-emmeans(ecpsvl,'z')
pwimslsvl<-emmeans(mslsvl,'z')
pairs(pwisklsvl)
pairs(pwisqlsvl)
pairs(pwiqdlsvl)
pairs(pwiecpsvl)
pairs(pwimslsvl)

###### RESULTADOS

#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwisqlsvl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar   0.1494 0.0285 122   5.251  <.0001
# alt - pub   0.1575 0.0290 122   5.435  <.0001
# jar - pub   0.0081 0.0280 122   0.290  0.9548
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwiqdlsvl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar  0.17525 0.0281 122   6.228  <.0001
# alt - pub  0.17142 0.0287 122   5.981  <.0001
# jar - pub -0.00383 0.0277 122  -0.139  0.9895
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwiecpsvl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar  0.16048 0.0251 122   6.397  <.0001
# alt - pub  0.15696 0.0256 122   6.142  <.0001
# jar - pub -0.00352 0.0247 122  -0.143  0.9888
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 
#> pairs(pwimslsvl)
# contrast  estimate     SE  df t.ratio p.value
# alt - jar   0.1191 0.0184 122   6.463  <.0001
# alt - pub   0.0954 0.0188 122   5.082  <.0001
# jar - pub  -0.0237 0.0181 122  -1.309  0.3931
#
#Note: contrasts are still on the log.[ scale 
#P value adjustment: tukey method for comparing a family of 3 estimates 




###############################################################################################################################################
###############
######## FOR LOOP TESTANDO INTERAÇÃO ###############################

# Criar um vetor para armazenar os resultados da análise
resultados <- vector("list", length(split_data))

# Loop for para realizar a análise de variância em cada coluna
for (i in 1:length(split_data)) {
  # Obter o data frame atual
  df <- split_data[[i]]
  
  # Obter a informação da variável y e da espécie
  variaveis_y <- colnames(df)[11:23]
  variavel_x <- colnames(df)[10]
  especie <- df[, 3][1]
  
  # Selecionar as colunas relevantes para a análise
  y <- df[, 11:23]
  x <- df[, 10]
  z <- df[, 26]
  
  # Loop for para realizar a análise de variância em cada coluna
  col_resultados <- vector("list", ncol(y))
  for (j in 1:ncol(y)) {
    col_resultados[[j]] <- summary(aov(log(y[, j]) ~ log(x) * z))
  }
  
  # Armazenar os resultados da análise para o data frame atual
  resultados[[i]] <- list(variaveis_y = variaveis_y, variavel_x = variavel_x, especie = especie, resultados = col_resultados)
}

# Acessar os resultados individuais
for (i in 1:length(resultados)) {
  cat("Resultados para o data frame", i, ":\n")
  
  variaveis_y <- resultados[[i]]$variaveis_y
  variavel_x <- resultados[[i]]$variavel_x
  especie <- resultados[[i]]$especie
  
  for (j in 1:length(resultados[[i]]$resultados)) {
    cat("Espécie:", especie, "| Variável y:", variaveis_y[j], "| Variável x:", variavel_x, "| Coluna", j, ":\n")
    print(resultados[[i]]$resultados[[j]])
    cat("\n")
  }
  
}


##############################################################################################################################
##### RODANDO MCP 
#############################################################################################################################
require(mcp)
dfa<-split_data$alternatus
dfj<-dfj_n
dfj<-split_data$jararaca
dfp<-split_data$pubescens


da_alt <- data.frame("TTL" = log(dfa[10]), "SkL" = log(dfa[13]), "StL" =log(dfa[14]),"QL"=log(dfa[15]),"EcL"= log(dfa[16]), "PaL" = log(dfa[19]), "PtL"=log(dfa[20]),"MnL"= log(dfa[21]), "MxL"=log(dfa[22]))
da_fon <- data.frame("TTL" = log(dff[10]), "SkL" = log(dff[13]), "StL" = log(dff[14]), "QL" = log(dff[15]), "EcL" = log(dff[16]), "PaL" = log(dff[19]), "PtL" = log(dff[20]), "MnL" = log(dff[21]), "MxL" = log(dff[22]))
da_jar <- data.frame("TTL" = log(dfj[10]), "SkL" = log(dfj[13]), "StL" = log(dfj[14]), "QL" = log(dfj[15]), "EcL" = log(dfj[16]), "PaL" = log(dfj[19]), "PtL" = log(dfj[20]), "MnL" = log(dfj[21]), "MxL" = log(dfj[22]))
da_pub <- data.frame("TTL" = log(dfp[10]), "SkL" = log(dfp[13]), "StL" = log(dfp[14]), "QL" = log(dfp[15]), "EcL" = log(dfp[16]), "PaL" = log(dfp[19]), "PtL" = log(dfp[20]), "MnL" = log(dfp[21]), "MxL" = log(dfp[22]))


model_1 <-list(C_L~1+ Total_L,~1 + Total_L )
model_2 <-list(S_L~1                + Total_L,~1 + Total_L)
model_3 <-list(Q_L ~1               + Total_L,~1 + Total_L)
model_4 <-list(EcP_L~1              + Total_L,~1 + Total_L)
model_5 <-list(palatine_L ~1        + Total_L,~1 + Total_L)
model_6 <-list(Ptotal_length~1      + Total_L,~1 + Total_L)
model_7 <-list(ML~1+ Total_L,~1 + Total_L)
model_8 <-list(Man_L~1              + Total_L,~1 + Total_L)


#models alternatus ####
alt_1l<-mcp(model_1,data=da_alt,iter=9999, chains=5)
alt_2l<-mcp(model_2,data=da_alt, iter=9999, chains=5)
alt_3l<-mcp(model_3,data=da_alt, iter=9999, chains=5)
alt_4l<-mcp(model_4,data=da_alt, iter=9999, chains=5)
alt_5l<-mcp(model_5,data=da_alt, iter=9999, chains=5)
alt_6l<-mcp(model_6,data=da_alt, iter=9999, chains=5)
alt_7l<-mcp(model_7,data=da_alt,iter=9999, chains=5)
alt_8l<-mcp(model_8,data=da_alt, iter=9999, chains=5)


#models jararaca #####

jar_1l<-mcp(model_1,data=da_jar, iter=9999, chains=5)
jar_2l<-mcp(model_2,data=da_jar, iter=9999, chains=5)
jar_3l<-mcp(model_3,data=da_jar, iter=9999, chains=5)
jar_4l<-mcp(model_4,data=da_jar, iter=9999, chains=5)
jar_5l<-mcp(model_5,data=da_jar, iter=9999, chains=5)
jar_6l<-mcp(model_6,data=da_jar, iter=9999, chains=5)
jar_7l<-mcp(model_7,data=da_jar, iter=9999, chains=5)
jar_8l<-mcp(model_8,data=da_jar, iter=9999, chains=5)

jar_list<-
  
  #models pubescens #####
pub_1l<-mcp(model_1,data=da_pub, iter=9999,chains=5)
pub_2l<-mcp(model_2,data=da_pub, iter=9999,chains=5)
pub_3l<-mcp(model_3,data=da_pub, iter=9999,chains=5)
pub_4l<-mcp(model_4,data=da_pub, iter=9999,chains=5)
pub_5l<-mcp(model_5,data=da_pub, iter=9999,chains=5)
pub_6l<-mcp(model_6,data=da_pub, iter=9999,chains=5)
pub_7l<-mcp(model_7,data=da_pub, iter=9999,chains=5)
pub_8l<-mcp(model_8,data=da_pub, iter=9999,chains=5)

pub_list


#################################################
# fazendo uma tabela com OS SLOPE CHANGES Das medidas lineares 

#alt
slo_tb_res_alt_1l<-summary(alt_1l)
slo_tb_res_alt_2l<-summary(alt_2l)
slo_tb_res_alt_3l<-summary(alt_3l)
slo_tb_res_alt_4l<-summary(alt_4l)
slo_tb_res_alt_5l<-summary(alt_5l)
slo_tb_res_alt_6l<-summary(alt_6l)
slo_tb_res_alt_7l<-summary(alt_7l)
slo_tb_res_alt_8l<-summary(alt_8l)
#jar
slo_tb_res_jar_1l<-summary(jar_1l)
slo_tb_res_jar_2l<-summary(jar_2l)
slo_tb_res_jar_3l<-summary(jar_3l)
slo_tb_res_jar_4l<-summary(jar_4l)
slo_tb_res_jar_5l<-summary(jar_5l)
slo_tb_res_jar_6l<-summary(jar_6l)
slo_tb_res_jar_7l<-summary(jar_7l)
slo_tb_res_jar_8l<-summary(jar_8l)
#pub
slo_tb_res_pub_1l<-summary(pub_1l)
slo_tb_res_pub_2l<-summary(pub_2l)
slo_tb_res_pub_3l<-summary(pub_3l)
slo_tb_res_pub_4l<-summary(pub_4l)
slo_tb_res_pub_5l<-summary(pub_5l)
slo_tb_res_pub_6l<-summary(pub_6l)
slo_tb_res_pub_7l<-summary(pub_7l)
slo_tb_res_pub_8l<-summary(pub_8l)


tabs_slo_linear<-c(slo_tb_res_alt_1l[3,2],
                   slo_tb_res_alt_2l,
                   slo_tb_res_alt_3l,
                   slo_tb_res_alt_4l,
                   slo_tb_res_alt_5l,
                   slo_tb_res_alt_6l,
                   slo_tb_res_alt_7l,
                   slo_tb_res_alt_8l,
                   
                   slo_tb_res_jar_1l,
                   slo_tb_res_jar_2l,
                   slo_tb_res_jar_3l,
                   slo_tb_res_jar_4l,
                   slo_tb_res_jar_5l,
                   slo_tb_res_jar_6l,
                   slo_tb_res_jar_7l,
                   slo_tb_res_jar_8l,
                   
                   slo_tb_res_pub_1l,
                   slo_tb_res_pub_2l,
                   slo_tb_res_pub_3l,
                   slo_tb_res_pub_4l,
                   slo_tb_res_pub_5l,
                   slo_tb_res_pub_6l,
                   slo_tb_res_pub_7l,
                   slo_tb_res_pub_8l)

# Nomes das linhas e colunas
nomes_linhas <- c("SkL", "StL", "QdL", "EcL", "PaL", "PtL", "MxL", "MdL")
nomes_colunas <- c("B.alt(mean)", "B.alt(min)", "B.alt(max)", "B.jar(mean)", "B.jar(min)", "B.jar(max)", "B.pub(mean)", "B.pub(min)", "B.pub(max)")

# Criar o data frame vazio
novo_dataframe <- data.frame(matrix(ncol = length(nomes_colunas), nrow = length(nomes_linhas)))
colnames(novo_dataframe) <- nomes_colunas
rownames(novo_dataframe) <- nomes_linhas


coluna1<-c(slo_tb_res_alt_1l[3,2],
           slo_tb_res_alt_2l[3,2],
           slo_tb_res_alt_3l[3,2],
           slo_tb_res_alt_4l[3,2],
           slo_tb_res_alt_5l[3,2],
           slo_tb_res_alt_6l[3,2],
           slo_tb_res_alt_7l[3,2],
           slo_tb_res_alt_8l[3,2])

coluna2<-c(slo_tb_res_alt_1l[3,3],
           slo_tb_res_alt_2l[3,3],
           slo_tb_res_alt_3l[3,3],
           slo_tb_res_alt_4l[3,3],
           slo_tb_res_alt_5l[3,3],
           slo_tb_res_alt_6l[3,3],
           slo_tb_res_alt_7l[3,3],
           slo_tb_res_alt_8l[3,3])

coluna3<-c(slo_tb_res_alt_1l[3,4],
           slo_tb_res_alt_2l[3,4],
           slo_tb_res_alt_3l[3,4],
           slo_tb_res_alt_4l[3,4],
           slo_tb_res_alt_5l[3,4],
           slo_tb_res_alt_6l[3,4],
           slo_tb_res_alt_7l[3,4],
           slo_tb_res_alt_8l[3,4])

coluna4<-c(slo_tb_res_jar_1l[3,2],
           slo_tb_res_jar_2l[3,2],
           slo_tb_res_jar_3l[3,2],
           slo_tb_res_jar_4l[3,2],
           slo_tb_res_jar_5l[3,2],
           slo_tb_res_jar_6l[3,2],
           slo_tb_res_jar_7l[3,2],
           slo_tb_res_jar_8l[3,2])

coluna5<-c(slo_tb_res_jar_1l[3,3],
           slo_tb_res_jar_2l[3,3],
           slo_tb_res_jar_3l[3,3],
           slo_tb_res_jar_4l[3,3],
           slo_tb_res_jar_5l[3,3],
           slo_tb_res_jar_6l[3,3],
           slo_tb_res_jar_7l[3,3],
           slo_tb_res_jar_8l[3,3])

coluna6<-c(slo_tb_res_jar_1l[3,4],
           slo_tb_res_jar_2l[3,4],
           slo_tb_res_jar_3l[3,4],
           slo_tb_res_jar_4l[3,4],
           slo_tb_res_jar_5l[3,4],
           slo_tb_res_jar_6l[3,4],
           slo_tb_res_jar_7l[3,4],
           slo_tb_res_jar_8l[3,4])

coluna7<-c(slo_tb_res_pub_1l[3,2],
           slo_tb_res_pub_2l[3,2],
           slo_tb_res_pub_3l[3,2],
           slo_tb_res_pub_4l[3,2],
           slo_tb_res_pub_5l[3,2],
           slo_tb_res_pub_6l[3,2],
           slo_tb_res_pub_7l[3,2],
           slo_tb_res_pub_8l[3,2])

coluna8<-c(slo_tb_res_pub_1l[3,3],
           slo_tb_res_pub_2l[3,3],
           slo_tb_res_pub_3l[3,3],
           slo_tb_res_pub_4l[3,3],
           slo_tb_res_pub_5l[3,3],
           slo_tb_res_pub_6l[3,3],
           slo_tb_res_pub_7l[3,3],
           slo_tb_res_pub_8l[3,3])

coluna9<-c(slo_tb_res_pub_1l[3,4],
           slo_tb_res_pub_2l[3,4],
           slo_tb_res_pub_3l[3,4],
           slo_tb_res_pub_4l[3,4],
           slo_tb_res_pub_5l[3,4],
           slo_tb_res_pub_6l[3,4],
           slo_tb_res_pub_7l[3,4],
           slo_tb_res_pub_8l[3,4])


DF<-data.frame(coluna1,coluna2,coluna3,coluna4,coluna5,coluna6,coluna7,coluna8,coluna9)

nomes_linhas <- c("SkL", "StL", "QdL", "EcL", "PaL", "PtL", "MxL", "MdL")
nomes_colunas <- c("B.alt(mean)", "B.alt(min)", "B.alt(max)", "B.jar(mean)", "B.jar(min)", "B.jar(max)", "B.pub(mean)", "B.pub(min)", "B.pub(max)")

colnames(DF)<-nomes_colunas
rownames(DF)<-nomes_linhas
DF

#TESTE CONTRA O MODELO NULO 



null_1<-list(C_L~1+Total_L   )
null_2<-list(S_L~1+Total_L           )
null_3<-list(Q_L ~1+Total_L          )
null_4<-list(EcP_L~1+Total_L         )
null_5<-list(palatine_L ~1+Total_L   )
null_6<-list(Ptotal_length~1+Total_L )
null_7<-list(ML~1+Total_L            )
null_8<-list(Man_L~1+Total_L        )


#Alt
fit1_null_alt<-mcp(null_1, data=da_alt)
fit2_null_alt<-mcp(null_2, data=da_alt)
fit3_null_alt<-mcp(null_3, data=da_alt)
fit4_null_alt<-mcp(null_4, data=da_alt)
fit5_null_alt<-mcp(null_5, data=da_alt)
fit6_null_alt<-mcp(null_6, data=da_alt)
fit7_null_alt<-mcp(null_7, data=da_alt)
fit8_null_alt<-mcp(null_8, data=da_alt)
#Jar
fit1_null_jar<-mcp(null_1, data=da_jar)
fit2_null_jar<-mcp(null_2, data=da_jar)
fit3_null_jar<-mcp(null_3, data=da_jar)
fit4_null_jar<-mcp(null_4, data=da_jar)
fit5_null_jar<-mcp(null_5, data=da_jar)
fit6_null_jar<-mcp(null_6, data=da_jar)
fit7_null_jar<-mcp(null_7, data=da_jar)
fit8_null_jar<-mcp(null_8, data=da_jar)
#Pub
fit1_null_pub<-mcp(null_1, data=da_pub)
fit2_null_pub<-mcp(null_2, data=da_pub)
fit3_null_pub<-mcp(null_3, data=da_pub)
fit4_null_pub<-mcp(null_4, data=da_pub)
fit5_null_pub<-mcp(null_5, data=da_pub)
fit6_null_pub<-mcp(null_6, data=da_pub)
fit7_null_pub<-mcp(null_7, data=da_pub)
fit8_null_pub<-mcp(null_8, data=da_pub)

library(loo)
#loo fitsmodelos
alt_1l$loo<-loo(alt_1l)
alt_2l$loo<-loo(alt_2l)
alt_3l$loo<-loo(alt_3l)
alt_4l$loo<-loo(alt_4l)
alt_5l$loo<-loo(alt_5l)
alt_6l$loo<-loo(alt_6l)
alt_7l$loo<-loo(alt_7l)
alt_8l$loo<-loo(alt_8l)
jar_1l$loo<-loo(jar_1l)
jar_2l$loo<-loo(jar_2l)
jar_3l$loo<-loo(jar_3l)
jar_4l$loo<-loo(jar_4l)
jar_5l$loo<-loo(jar_5l)
jar_6l$loo<-loo(jar_6l)
jar_7l$loo<-loo(jar_7l)
jar_8l$loo<-loo(jar_8l)
pub_1l$loo<-loo(pub_1l)
pub_2l$loo<-loo(pub_2l)
pub_3l$loo<-loo(pub_3l)
pub_4l$loo<-loo(pub_4l)
pub_5l$loo<-loo(pub_5l)
pub_6l$loo<-loo(pub_6l)
pub_7l$loo<-loo(pub_7l)
pub_8l$loo<-loo(pub_8l)

#loo null models
fit1_null_alt$loo<-loo(fit1_null_alt)
fit2_null_alt$loo<-loo(fit2_null_alt)
fit3_null_alt$loo<-loo(fit3_null_alt)
fit4_null_alt$loo<-loo(fit4_null_alt)
fit5_null_alt$loo<-loo(fit5_null_alt)
fit6_null_alt$loo<-loo(fit6_null_alt)
fit7_null_alt$loo<-loo(fit7_null_alt)
fit8_null_alt$loo<-loo(fit8_null_alt)
fit1_null_jar$loo<-loo(fit1_null_jar)
fit2_null_jar$loo<-loo(fit2_null_jar)
fit3_null_jar$loo<-loo(fit3_null_jar)
fit4_null_jar$loo<-loo(fit4_null_jar)
fit5_null_jar$loo<-loo(fit5_null_jar)
fit6_null_jar$loo<-loo(fit6_null_jar)
fit7_null_jar$loo<-loo(fit7_null_jar)
fit8_null_jar$loo<-loo(fit8_null_jar)
fit1_null_pub$loo<-loo(fit1_null_pub)
fit2_null_pub$loo<-loo(fit2_null_pub)
fit3_null_pub$loo<-loo(fit3_null_pub)
fit4_null_pub$loo<-loo(fit4_null_pub)
fit5_null_pub$loo<-loo(fit5_null_pub)
fit6_null_pub$loo<-loo(fit6_null_pub)
fit7_null_pub$loo<-loo(fit7_null_pub)
fit8_null_pub$loo<-loo(fit8_null_pub)

comp1a<-loo_compare(alt_1l$loo,fit1_null_alt$loo)#1
comp2a<-loo_compare(alt_2l$loo,fit2_null_alt$loo)#2
comp3a<-loo_compare(alt_3l$loo,fit3_null_alt$loo)#2
comp4a<-loo_compare(alt_4l$loo,fit4_null_alt$loo)#1
comp5a<-loo_compare(alt_5l$loo,fit5_null_alt$loo)#1
comp6a<-loo_compare(alt_6l$loo,fit6_null_alt$loo)#2
comp7a<-loo_compare(alt_7l$loo,fit7_null_alt$loo)#2
comp8a<-loo_compare(alt_8l$loo,fit8_null_alt$loo)#2

comp1j<-loo_compare(jar_1l$loo,fit1_null_jar$loo)#1
comp2j<-loo_compare(jar_2l$loo,fit2_null_jar$loo)#2
comp3j<-loo_compare(jar_3l$loo,fit3_null_jar$loo)#2
comp4j<-loo_compare(jar_4l$loo,fit4_null_jar$loo)#2
comp5j<-loo_compare(jar_5l$loo,fit5_null_jar$loo)#1
comp6j<-loo_compare(jar_6l$loo,fit6_null_jar$loo)#2
comp7j<-loo_compare(jar_7l$loo,fit7_null_jar$loo)#2
comp8j<-loo_compare(jar_8l$loo,fit8_null_jar$loo)#1

comp1p<-loo_compare(pub_1l$loo,fit1_null_pub$loo)#2
comp2p<-loo_compare(pub_2l$loo,fit2_null_pub$loo)#2
comp3p<-loo_compare(pub_3l$loo,fit3_null_pub$loo)#1
comp4p<-loo_compare(pub_4l$loo,fit4_null_pub$loo)#2
comp5p<-loo_compare(pub_5l$loo,fit5_null_pub$loo)#1
comp6p<-loo_compare(pub_6l$loo,fit6_null_pub$loo)#1
comp7p<-loo_compare(pub_7l$loo,fit7_null_pub$loo)#1
comp8p<-loo_compare(pub_8l$loo,fit8_null_pub$loo)#1




coluna_c1<- c("1",
              "2",
              "2",
              "1",
              "1",
              "2",
              "2",
              "2")

coluna_c2<-c("1",
             "2",
             "2",
             "2",
             "1",
             "2",
             "2",
             "1")

coluna_c3<-c("2",
             "2",
             "1",
             "2",
             "1",
             "1",
             "1",
             "1")

DF_C<-data.frame(coluna_c1,coluna_c2,coluna_c3)

nomesc<-c("B.alt", "B.jar", "B.pub")

row.names(DF_C)<-nomes_linhas
colnames(DF_C)<-nomesc

#### AJUSTANDO NOME DOS DATA FRAMES _______ ESSES DATA.FRAMES FORAM SALVOS E ESTÂO PRINTADOS NO SCRIPT REANALIZES CAPTI.R
DF_C
DF_linear<-DF
DF_L_fitornull<-DF_C


#################### PLOTANDO MODELOS ###################################################################################
#alternatus
Palt_1<-plot(alt_1l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(SkL)", size=2)
Palt_2<-plot(alt_2l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(StL)", size=2)
Palt_3<-plot(alt_3l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(QdL)", size=2)
Palt_4<-plot(alt_4l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(EcL)", size=2)
Palt_5<-plot(alt_5l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PaL)", size=2)
Palt_6<-plot(alt_6l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PtL)", size=2)
Palt_7<-plot(alt_7l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MxL)", size=2)
Palt_8<-plot(alt_8l)+geom_point(shape=21, size=(dfa$Total_L/20), col="black", fill="darkolivegreen3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MdL)", size=2)

#jararaca
Pjar_1<-plot(jar_1l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(SkL)", size=2)
Pjar_2<-plot(jar_2l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(StL)", size=2)
Pjar_3<-plot(jar_3l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(QdL)", size=2)
Pjar_4<-plot(jar_4l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(EcL)", size=2)
Pjar_5<-plot(jar_5l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PaL)", size=2)
Pjar_6<-plot(jar_6l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PtL)", size=2)
Pjar_7<-plot(jar_7l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MxL)", size=2)
Pjar_8<-plot(jar_8l)+geom_point(shape=21, size=(dfj$Total_L/20), col="black", fill="deepskyblue3")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MdL)", size=2)

#pubescnes
Ppub_1<-plot(pub_1l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(SkL)", size=2)
Ppub_2<-plot(pub_2l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(StL)", size=2)
Ppub_3<-plot(pub_3l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(QdL)", size=2)
Ppub_4<-plot(pub_4l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(EcL)", size=2)
Ppub_5<-plot(pub_5l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PaL)", size=2)
Ppub_6<-plot(pub_6l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(PtL)", size=2)
Ppub_7<-plot(pub_7l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MxL)", size=2)
Ppub_8<-plot(pub_8l)+geom_point(shape=21, size=(dfp$Total_L/20), col="black", fill="darkgoldenrod1")+theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+labs(title ="", subtitle ="", x = "log(TTL)", y = "log(MdL)", size=2)

#########################################################################################################################################################################################################################################################################################
require(cowplot)



alt_plot<-plot_grid(Palt_1,
                    Palt_2,
                    Palt_3,
                    Palt_4,
                    Palt_5,
                    Palt_6,
                    Palt_7,
                    Palt_8, nrow=4, labels=c("A","B","C","D","E","F","G","H"))

jar_plot<-plot_grid(Pjar_1,
                    Pjar_2,
                    Pjar_3,
                    Pjar_4,
                    Pjar_5,
                    Pjar_6,
                    Pjar_7,
                    Pjar_8, nrow=4, labels=c("I","J","K","L","M","N","O","P"))

pub_plot<-plot_grid(Ppub_1,
                    Ppub_2,
                    Ppub_3,
                    Ppub_4,
                    Ppub_5,
                    Ppub_6,
                    Ppub_7,
                    Ppub_8, nrow=4, labels=c("Q","R","S","T","U","V","X","Y"))

pdf("Shape_diet_linear.pdf", width = 30, height = 15)
plot_grid(alt_plot,jar_plot,pub_plot, nrow=1)
dev.off()

#############################################################################################################################################################################

#Graficos rela??es BONESnSKULL x SVL
a<-aov(tss.omit$S_L~tss.omit$SVL*tss.omit$Species)
summary(a)
a<-lm(tss.omit$S_L~tss.omit$SVL*tss.omit$Species)
summary(a)

a<-procD.lm(tss.omit$S_L~tss.omit$SVL*tss.omit$Species)
summary(a)
#########################################################################################################################################
hell<-data

cores <- c("darkolivegreen3", "deepskyblue3", "darkgoldenrod1")
especies <- c("alternatus", "jararaca", "pubescens")

# Exemplo de vetor com espécies
dados <- data.frame(espécies = c("jararaca", "alternatus", "pubescens", "alternatus"))

# Encontrar as correspondências entre as espécies e as cores
indices_cores <- match(hell$Species, especies)

# Atribuir as cores correspondentes aos dados
hell$cores <- cores[indices_cores]

# Exibir o resultado
print(hell)


A= ggplot(hell, aes(y=log(C_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                    axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (SkL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
B= ggplot(hell, aes(y=log(S_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                    axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (StL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
C= ggplot(hell, aes(y=log(Q_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                    axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (QdL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
D= ggplot(hell, aes(y=log(EcP_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                      axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (EcL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
E= ggplot(hell, aes(y=log(hell$palatine_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                                axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (PaL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Ff=ggplot(hell, aes(y=log(hell$Ptotal_length), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                                   axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (PtL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
G= ggplot(hell, aes(y=log(hell$ML), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                        axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (MxL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
H= ggplot(hell, aes(y=log(hell$Man_L), x=log(SVL), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                           axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SVL)",y = "log (MdL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))



Aa<-ggplot(hell, aes(y=log(S_L), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                     axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (StL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Bb<-ggplot(hell, aes(y=log(Q_L), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                     axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (QdL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Cc<-ggplot(hell, aes(y=log(EcP_L), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                       axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (EcL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Dd<-ggplot(hell, aes(y=log(hell$palatine_L), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                                 axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (PaL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Ee<-ggplot(hell, aes(y=log(hell$Ptotal_length), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                                    axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (PtL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Ffj<-ggplot(hell, aes(y=log(hell$ML), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                         axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (MxL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))
Gg<-ggplot(hell, aes(y=log(hell$Man_L), x=log(C_L), color = Species))+theme(axis.text.x = element_text(size = 12),
                                                                            axis.text.y = element_text(size = 12))+stat_smooth(method = "lm", se=FALSE,size=1.5,show.legend = FALSE)+geom_point(aes(colour = Species), shape=21,size=3, col="black",fill=hell$cores,show.legend =FALSE)+labs(title ="", subtitle ="", x = "log(SkL)",y = "log (MdL)")+scale_color_manual(values=c("darkolivegreen3","deepskyblue3", "darkgoldenrod1"))






svl_p<-plot_grid(A,
                 B,
                 C,
                 D,
                 E,
                 Ff,
                 G,
                 H, labels=c("A","B","C","D","E","F","G","H"), nrow=4)  

skl_p<-plot_grid(Aa,
                 Bb,
                 Cc,
                 Dd,
                 Ee,
                 Ffj,
                 Gg, labels=c("I","J","K","L","M","N","O","P"), nrow=4)



pdf("linear_spxsp_linear.pdf", width = 30, height = 15)
plot_grid(svl_p,skl_p, nrow = 1)
dev.off()









