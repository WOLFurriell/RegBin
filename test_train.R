#-----------------------------------------------------------------------------------
# Verificando a capacidade preditiva do modelo  --------------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
#----------------------------------------------------------------------------------
library(foreign);library(ggplot2);library(lmtest);library(MASS);
library(reshape2);library(dplyr);library(wesanderson);library(xtable)

#---------------------------------------------------------------------------------
dados  <- read.csv("/home/furriel/Dropbox/Regressão Logística/scripts/corrupcao.csv", na.strings = ".")
dadosx <- dados[c("corrupcao","idade1", "escola", "Ljato", "denunciaTemer")]
dadosx <- na.omit(dadosx)
dadosx$corrupcao2[dadosx$corrupcao == "Corrupcao"]     <- 1
dadosx$corrupcao2[dadosx$corrupcao == "Incompetencia"] <- 0
dim(dadosx)

# Dividindo o banco entre treino e teste
aux <- sample(dim(dadosx)[1],dim(dadosx)[1])
train <- dadosx[1:2217,]
test  <- dadosx[2218:length(aux),]

model <- glm(corrupcao ~ idade1 + escola + Ljato + denunciaTemer, data = train, family = "binomial"(link="logit"))
summary(model)

# Verificando a capacidade de classificação do modelo - acurácia
fitted.results   <- predict(model, newdata = test, type="response")
fitted.results   <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$corrupcao2)
print(paste('Accuracy',1-misClasificError))

# --------------------------------------------------------------------------------------------
# Predição

m1 <- glm(corrupcao2 ~ idade1 + escola + Ljato + denunciaTemer, data = dadosx, family = "binomial"(link="logit"))
summary(m1)

x1           <- seq(min(dados$idade1, na.rm = T), max(dados$idade1, na.rm = T))
x2           <- seq(min(dados$escola, na.rm = T), max(dados$escola, na.rm = T))
x3           <- levels(dados$Ljato)
x4           <- levels(dados$denunciaTemer)
teste        <- expand.grid(x1,x2, x3,x4)
names(teste) <- c("idade1","escola", "Ljato","denunciaTemer")

preditos <- predict(m1, newdata = teste, type = "response")
teste    <- cbind(teste, preditos = preditos)

teste1 <- subset(teste, (subset = denunciaTemer == "Sim_deveria"))

# Predição quaando a escola == 1 & denunciaTemer == "Nao_deveria"
ggplot(data = teste1, aes(x = idade1, y = preditos, color = factor(Ljato ))) +
  geom_line(size = 1) +
  labs(x = "Idade", y = "Probabilidade", color = "") +
  scale_color_manual(values = wes_palette("BottleRocket2"), 
  labels = c("Continuará igual", "Irá aumentar", "Irá diminuir")) +
  facet_wrap(~escola, ncol = 4) +
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=11), strip.text = element_text(size=14, colour = "white", face = "bold"),
        strip.background = element_rect(fill="black")) + 
  guides(colour = guide_legend(keywidth = 2.5, keyheight = 2.5,title = "")) +
  ggtitle("Predição quando Deputados deveriam autorizar denúncia contra Temer? = Sim") 
ggsave(filename = "predcorrup.pdf", device = "pdf", height = 6, width = 9)

# Tabela
results <- summary(m1)
expb    <- exp(coef(m1)) # exponentiated coefficients
cib     <- exp(confint(m1)) # 95% CI for exponentiated coefficients
tab     <- cbind(expb, results$coefficients[,4], cib)%>%as.data.frame()
tab$term    <-rownames(tab)
names(tab)  <- c("expb","p.value", "ic.025","ic.975","term") 
tab$p.value <- tab$p.value%>%as.character()%>%as.numeric()
tab$expb    <- tab$expb%>%as.character()%>%as.numeric()
tab$ic.025  <- tab$ic.025%>%as.character()%>%as.numeric()
tab$ic.975  <- tab$ic.975%>%as.character()%>%as.numeric()

coef       <- tab[!tab$term=="(Intercept)",]
cbbPalette <- c("#00147E", "#990000")
coef$p.id  <- ifelse(coef$p.value<0.05,"p<0.05","p=>0.05")%>%as.factor()
gcoef      <- ggplot(coef, aes(term, expb,colour=p.id)) + coord_flip() +
              geom_point(aes(colour=p.id),size=3) + 
              geom_hline(yintercept=1,size=1, linetype = "dashed") + 
              geom_pointrange(aes(ymin = ic.025, ymax = ic.975)) +
              scale_color_manual(values=cbbPalette, name="Valor-p") +
              scale_x_discrete("", labels = c("idade1"="Idade", "escola"="Escolaridade",
                                "LjatoIra_aumentar"="LjatoAumentar", 
                                "LjatoIra_diminuir"="LjatoDiminuir","denunciaTemerSim_deveria"="DenúnciaTemerSim")) +
              scale_y_continuous(breaks = seq(0.8,3, by = 0.2)) +
              ylab("Estimativas") + xlab("Parâmetro") +
              theme(text = element_text(size = 13))
gcoef
setwd("/home/furriel/Dropbox/Regressão Logística/plots_corrupcao")
ggsave(gcoef,filename = "coef.pdf", device = "pdf", height = 5, width = 8)

