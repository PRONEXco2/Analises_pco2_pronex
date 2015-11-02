# Analises_pco2_pronex
############GLM 1 - Variaveis espaciais#
library (lme4)
library (MuMIn)
library(ggplot2)
library(grid)
library(HH)
library(vegan)
library(hier.part)
library(MASS)
library(ggplot2)

lm_pco2<- read.table(file.choose(), header=TRUE,dec=".", sep="\t")
lm_pco2

str(lm_pco2)
attach(lm_pco2)
names(lm_pco2)

##### Modelos##########

r1=lm(formula = pco2 ~ p_Flood_For + p_Flood_Herb + p_open_Water + respiracao)# Modelo que inclui a correlaçao entre floresta alagada e macrofitas, e com open waters. Acho que nao rola de usar!
summary(r1)

r2=lm(formula = pco2 ~p_Vegetated_areas +cod+respiracao+Chla)#talvez seja melhor usar esse modelo! pois respeita o pressuposto de ausencia de colinearidade
summary(r2)
resi<-r2$residuals##Para extrair os residuos da variavel resposta
summary(r2)

qplot(p_Vegetated_areas,pco2, data = lm_pco2, geom=c("point", "smooth"),method = "lm")

qplot(respiracao,pco2, data = lm_pco2, geom=c("point", "smooth"),method = "lm")

##GRAFICO DAS PARCIAIS DA REGRESSAO MULTIPLA - FUNCAO VICTOR LANDEIRO (adaptada)####Copiar e colar no R, depois rodar!###
parciais<-function(formula=pco2~pred1+pred2+predN,i,...){
  dados<-as.matrix(model.frame(formula))
  resuY<-lm(dados[,1]~dados[,-c(1,i)])
  resuX<-lm(dados[,i]~dados[,-c(1,i)])
  partial.dependent<-resuY$res
  partial.predictor<-resuX$res
  resuR<-lm(partial.dependent~partial.predictor)
  Ylab<-paste(colnames(dados)[1],"(partial)")
  Xlab<-paste(colnames(dados)[i],"(partial)")
  plot(partial.predictor,partial.dependent,ylab=Ylab,xlab=Xlab,...)
  abline(resuR)
  return(resuR)}
##################################


par(mfrow=c(3,3),mar=c(5,5,3,2))##PARA GERAR 4 GRAFICOS AO MESMO TEMPO. SE QUISER DISATIVAR ESSA FUNÇÃO, USAR dev.off()#
#Modelo geral 
parciais(lm(pco2 ~p_Vegetated_areas +cod+respiracao+Chla),2,pch=16,cex.lab=1.6,font.lab=2,family="serif",cex.axis=1.3)#
mtext("p = 0.0493",font=1.3,cex=1.3,family="serif",adj=1,line=.3)

parciais(lm(pco2 ~p_Vegetated_areas +cod+respiracao+Chla),3,pch=16,cex.lab=1.6,font.lab=2,family="serif",cex.axis=1.3)
mtext("p = 0.0674",font=1.3,cex=1.3,family="serif",adj=1,line=.3)


parciais(lm(pco2 ~p_Vegetated_areas +cod+respiracao+Chla),4,pch=16,cex.lab=1.6,font.lab=2,family="serif",cex.axis=1.3)
mtext("p = 0.5098",font=1.3,cex=1.3,family="serif",adj=1,line=.3)


parciais(lm(pco2 ~p_Vegetated_areas +cod+respiracao+Chla),5,pch=16,cex.lab=1.6,font.lab=2,family="serif",cex.axis=1.3)
mtext("p = 0.2118",font=1.3,cex=1.3,family="serif",adj=1,line=.3)



###O VALOR “2” ANTES DE “pch” SIGNIFICA QUE VC QUER VER A EXPLICAÇÃO PARCIAL DA PRIMEIRA VARIÁVEL PREDITORA DO MODELO. COM ISSO, DEVE-SE TROCAR ESSE VALOR DE ACORDO COM A VARIÁVEL PREDITORA QUE VC DESEJA VER (note que nos comandos acima, usamos 2 (pra p_vegeted_area), 3 para cod, 4 (pra respiracao), e 5 (pra chla)####

mtext("p=0.0493",font=1.3,cex=1.3,family="serif",adj=1,line=.1) ##ESSE COMANDO INSERE UM TEXTO ACIMA DE CADA GRÁFICO DA PARCIAL. FOI USADO PRA INSERIR O VALOR DE ”p”. ASSIM, PRA CADA PARCIAL, DEVE-SE INSERIR O VALOR EXATO DO “p”, QUE PODE SER OBTIDO NO SUMÁRIO DA REGRESSÃO MÚLTIPLA (summary(r2))###





#Dregde - seleçao de modelos#mAis complexo! Explorar depois com os dados de rios
library(MuMIn)
models_phyto<-dredge(r2)#aqui da uma mensagem de erro "na omit"
models_phyto
model.avg(models_phyto)
confset.95p <- get.models(models_phyto, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

######Verificando os pressupostos#########

#Distribuiçao dos residuos#

plot(residuals(r2) ~ predict(r2))

#Normalidade#

hist(pco2)# Normal
hist(cod)#normal
hist(respiracao)#nao normal
hist(p_Vegetated_areas)#nao normal
hist(Chla)#nao normal
hist(Resp_int_d)

###TESTAR VIF para colinearidade#########
library (car)
usair.lm <- lm((formula =pco2 ~p_Vegetated_areas +cod+respiracao+Chla), data=lm_pco2, x=TRUE)
vif(usair.lm)

########correlaçoes######

cor(p_Flood_For,p_Flood_Herb)
cor(p_Flood_For,p_open_Water)
cor(p_Flood_For,respiracao)
cor(p_Flood_Herb,p_open_Water)
cor(p_Flood_Herb,respiracao)
cor(p_open_Water, respiracao)
