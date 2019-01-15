#-----------------------------------------------------------------------------------
# AJUSTE DO MODELO BINÁRIO LOGÍSTICO PARA CORRUPÇÃO --------------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
#----------------------------------------------------------------------------------
library(aod);library(foreign);library(ggplot2);library(lmtest);library(MASS);
library(Hmisc);library(reshape2);library(dplyr);library(Amelia);library(xtable)

#---------------------------------------------------------------------------------
# Importando os dados sav ----------------------------------------
dir   <- "/home/furriel/Dropbox/Observatório 2018/Dados/datafolha_09_2017/04423.SAV"
dados <- read.spss(file=dir, use.value.labels = F, to.data.frame = T) 
# Retirar Nâo sabe - Não respodeu - Recusou
dados <- data.frame(lapply(dados, function(x) {gsub(99, NA, x)}))
dados <- data.frame(lapply(dados, function(x) {gsub(98, NA, x)}))
dados <- data.frame(lapply(dados, function(x) {gsub(95, NA, x)}))
dados <- data.frame(lapply(dados, function(x) {gsub(97, NA, x)}))
dim(dados)

# Sexo
dados$sexo2[dados$sexo == "1"] <- "Masculino"
dados$sexo2[dados$sexo == "2"] <- "Feminino"

# Na sua opinião, o que custa mais ao Brasil: a corrupção ou a incompetência?
table(dados$p18)
dados$corrupcao[dados$p18 == "1"] <- "Corrupção"
dados$corrupcao[dados$p18 == "2"] <- "Incompetência"
dados$corrupcao <- dados$corrupcao%>%as.factor()
dados$corrupcao <- relevel(dados$corrupcao, ref = "Incompetência")

# Escolaridade 
table(dados$escola)

# Qual é o seu partido político de preferência ? 
dados$partido2[dados$partido == 96] <- 1
dados$partido2[dados$partido != 96] <- 0
table(dados$partido2)

# renda
table(dados$rendaf) 
dados$rendaf <- dados$rendaf%>%as.character()%>%as.numeric()

# Idade
dados$idade1 <- dados$idade1%>%as.character()%>%as.numeric()

# Escolaridade 
dados$escola <- dados$escola%>%as.character()%>%as.numeric()

#  De zero a dez que nota você dá para o desempenho do governo Temer?
table(dados$p7)
dados$AvalTemer <- dados$p7%>%as.character()%>%as.numeric()
dados$AvalTemer[dados$AvalTemer == 0.5] <- 0

# Na sua opinião, os deputados federais deveriam ou não autorizar a segunda denúncia do Ministério Público contra o presidente Michel Temer?
levels(dados$p13)   
dados$denunciaTemer[dados$p13 == 1] <- "Sim, deveria"
dados$denunciaTemer[dados$p13 == 2] <- "Não deveria"

#  Na sua opinião, depois da Operação Lava-Jato a corrupção no Brasil irá diminuir, aumentar ou continuará na mesma proporção de sempre?
table(dados$p15)
dados$Ljato[dados$p15 == 1] <- "Irá diminuir"
dados$Ljato[dados$p15 == 2] <- "Irá aumentar"
dados$Ljato[dados$p15 == 3] <- "Continuará igual"

# Visualizar missings
aux <- dados[c("corrupcao","sexo2","idade1", "rendaf", "escola", "Ljato", "partido2", "AvalTemer", "denunciaTemer")]
missmap(aux, main = "Missing values vs observed")

# Exportando o banco
edados <- dados[c("corrupcao","sexo2","idade1", "rendaf", "escola", "Ljato", "partido2", "AvalTemer", "denunciaTemer")]
write.csv(edados, file = "/home/furriel/Dropbox/Observatório 2018/Dados/corrupcao.csv")

#------------------------------------------------------------------------------------------------------
# Modelo de Regressão Logística
mlogit0 <- glm(corrupcao ~ sexo2 + idade1 + rendaf + escola + Ljato + partido2 + AvalTemer + denunciaTemer, 
               data = dados, family = "binomial"(link="logit"))

# Correção para os NAs
update_nested <- function(object, formula., ..., evaluate = TRUE){
  update(object = object, formula. = formula., data = object$model, ..., evaluate = evaluate)
}

# Sem Renda e sem partido e sem sexo e sem AvalTemer
mlogit1 <- update_nested(mlogit0, .~.-(partido2 + rendaf + sexo2 + AvalTemer))
lrtest(mlogit1, mlogit0)
# m1 + sem escola
mlogit2 <- update_nested(mlogit0, .~.-(partido2 + rendaf + sexo2 + AvalTemer + denunciaTemer + escola))
lrtest(mlogit2, mlogit1)
# m1 + sem denunciaTemer
mlogit3 <- update_nested(mlogit0, .~.-(partido2 + rendaf + sexo2 + AvalTemer + denunciaTemer + denunciaTemer))
lrtest(mlogit3, mlogit1)
# m1 + sem Ljato
mlogit4 <- update_nested(mlogit0, .~.-(partido2 + rendaf + sexo2 + AvalTemer + denunciaTemer + Ljato))
lrtest(mlogit4, mlogit1)
# m1 + sem idade
mlogit5 <- update_nested(mlogit0, .~.-(partido2 + rendaf + sexo2 + AvalTemer + denunciaTemer + idade1))
lrtest(mlogit5, mlogit1)

# Estimativas dos modelos ajustados
# Saturado
summary(mlogit0)
anova(mlogit0, test="Chisq")
# Reduzido
summary(mlogit1)
anova(mlogit1, test="Chisq")

#----------------------------------------------------------------------------------------------------------
# Curva Roc -----------------------------------------------------------------------------------------------

# Banco sem missing
dadosx <- edados[c("corrupcao","idade1", "escola", "Ljato", "denunciaTemer")]
dadosx <- na.omit(dadosx)

library(ModelGood)
m1 <- glm(corrupcao ~ idade1 + escola + Ljato + denunciaTemer, data=dadosx, family = "binomial"(link="logit"))

# Tabela
results <- summary(m1)
expb <- exp(coef(m1)) # exponentiated coefficients
cib  <- exp(confint(m1)) # 95% CI for exponentiated coefficients
tab  <- cbind(results$coefficients[,1], results$coefficients[,2],expb, cib)%>%as.data.frame()
names(tab) <- c("b","ep","expb","ic.025","ic.975") 
xtable(tab,digits = 4)

roc.m1 <- Roc(m1)

plot(Roc(m1))
legend(x="topleft",col=1,lwd=3,legend=c("idade1 + escola + Ljato + denunciaTemer"),bty="n")

# Curva Roc ggplot 
Sens <- roc.m1$Roc$glm$Sensitivity
Spec <- roc.m1$Roc$glm$Specificity
roc1 <- as.data.frame(cbind(Sens, Spec))

ggplot() +
  geom_line(data=roc1, aes(x=(1-Spec), y=Sens)) +
  geom_line(data=roc1, aes(x=Sens, y=Sens), linetype = "dashed")

# Curva Roc plotROC
#https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html
library(plotROC)

dadosx$corrupcao2[dadosx$corrupcao == "Corrupção"]     <- 1
dadosx$corrupcao2[dadosx$corrupcao == "Incompetência"] <- 0
dadosx$predict <- predict(m1)
roc <- ggplot(dadosx,aes(d = corrupcao, m = predict)) + 
         geom_roc(increasing = F, labels = FALSE, n.cuts = 0, colour= "#0099ff") +
         style_roc(theme = theme_grey, xlab = "(1 - Especificidade)", ylab = "Sensibilidade")
roc + annotate("text", x = .75, y = .25, 
               label = paste("AUC =", round(calc_auc(roc)$AUC, 4))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      theme(legend.position="none")
ggsave("ROCcorrup.pdf", device = "pdf", height = 4, width = 6)  


