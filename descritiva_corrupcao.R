#-----------------------------------------------------------------------------------
# ANÁLISE DESCRITIVA PARA CORRUPÇÃO --------------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
setwd("/home/furriel/Dropbox/Regressão Logística/plots_corrupcao")
#----------------------------------------------------------------------------------
library(ggplot2);library(foreign);library(ggplot2);library(MASS);library(reshape);
library(dplyr);library(wesanderson);library(easyGgplot2)
# corrupcao ~ sexo2 + idade1 + rendaf + escola + Ljato + partido2 + AvalTemer + denunciaTemer - COMPLETO
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# Sexo
aux <- dados[c("corrupcao","sexo2")]
sexo <-
ggplot(data = na.omit(aux), aes(sexo2)) + 
  geom_bar(aes(fill = corrupcao), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = 1"") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 13),
        legend.position="none",plot.title = element_text(hjust = 0.5)) + ggtitle("Sexo")
#ggsave("sexo.pdf", device = "pdf", height = 4, width = 7)

#----------------------------------------------------------------------------------
# Partido
aux <- dados[c("corrupcao","partido2")]
aux$partido2 <- aux$partido2%>%as.factor()
partido <-
ggplot(data = na.omit(aux), aes(partido2)) + 
  geom_bar(aes(fill = corrupcao), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A","black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks=c("0","1"),
                   labels = c("Sim", "Não")) +
  labs(x = "", y = "", fill = "") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 13),
        legend.position="none",plot.title = element_text(hjust = 0.5)) + ggtitle("Simpatiza com algum partido")
#ggsave("partido2.pdf", device = "pdf", height = 4, width = 7)

#----------------------------------------------------------------------------------
# Idade
aux <- dados[c("corrupcao","idade1")]
idade <- 
ggplot(na.omit(aux), aes(x=corrupcao, y=idade1, fill=corrupcao)) +
  geom_boxplot() + geom_jitter(aes(colour=corrupcao), alpha = 0.15) +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_colour_manual(values = c("#0B775E","#35274A")) +
  labs(x = "", y = "Idade", fill = "") +
  theme(panel.grid.minor.x = element_blank(), 
        text = element_text(size = 13), legend.position="none",
        plot.title = element_text(hjust = 0.5)) + ggtitle("Idade")
#ggsave("idade.pdf", device = "pdf", height = 4, width = 6)

h<-ggplot(na.omit(aux), aes(idade1)) +
   geom_histogram(binwidth = 5, fill = "#0B775E", colour = "black") + facet_grid(.~corrupcao) +
   scale_x_continuous(breaks = seq(20,90,10)) + xlab("") + ylab("") +
   scale_y_continuous(breaks = seq(0,200,25)) +
   theme(panel.grid.minor.x = element_blank(), 
        text = element_text(size = 13), legend.position="none", 
        plot.title = element_text(hjust = 0.5)) + ggtitle("Idade")
corrup <- subset(aux, corrupcao == "Corrupção")
hist <-
h + geom_histogram(data = corrup, fill = "#35274A", colour = "black", binwidth = 5)  
#ggsave("idade2.pdf", device = "pdf", height = 4, width = 9)

#----------------------------------------------------------------------------------
# Escolaridade
aux <- dados[c("corrupcao","escola")]
aux$escola <- aux$escola%>%as.factor()
esc <-
ggplot(data = na.omit(aux), aes(escola)) + 
  geom_bar(aes(fill = as.factor(corrupcao)), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "") +
  scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8"),
                   labels = c("Analfabeto", "Fundamental I completo","Fundamental II completo", 
                              "Médio incompleto","Médio completo","Superior incompleto"," Superior completo", "Pós-graduação")) +
  theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1), text = element_text(size = 13),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + ggtitle("Escolaridade")
#ggsave("escolaridade.pdf", device = "pdf", height = 6, width = 9)

#----------------------------------------------------------------------------------
# Renda
aux <- dados[c("corrupcao","rendaf")]
aux$rendaf <- aux$rendaf%>%as.factor()
renda <-
ggplot(data = na.omit(aux), aes(rendaf)) + 
  geom_bar(aes(fill = as.factor(corrupcao)), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "") +
  scale_x_discrete(breaks=c("1","2","3","4","5","6","7"),
                   labels = c("Até 1.874,00", "1.875,00 até 2.811,00","2.812,00 até 4.685,00","4.686,00 até 9.370,00", 
                              "9.371,00 até 18.740,00","18.741,00 até 46.850,00","46.851,00 ou mais")) +
  theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1), text = element_text(size = 13),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + ggtitle("Renda")
#ggsave("renda.pdf", device = "pdf", height = 6, width = 9)

#----------------------------------------------------------------------------------
# Lava Jato
aux <- dados[c("corrupcao","Ljato")]
jato <-
ggplot(data = na.omit(aux), aes(Ljato)) + 
  geom_bar(aes(fill = corrupcao), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A","black")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 13),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + ggtitle("Após a Lava-Jato a corrupção no Brasil")
#ggsave("Ljato.pdf", device = "pdf", height = 4, width = 7)

#----------------------------------------------------------------------------------
# Aval Temer
aux <- dados[c("corrupcao","AvalTemer")]
aux$AvalTemer <- aux$AvalTemer%>%as.factor()
aval <-
ggplot(data = na.omit(aux), aes(AvalTemer)) + 
  geom_bar(aes(fill = as.factor(corrupcao)), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "") +
  scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8","9","10")) +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 13), 
        legend.position="none", plot.title = element_text(hjust = 0.5)) + ggtitle("Avaliação governo Temer")
#ggsave("AvalTemer.pdf", device = "pdf", height = 6, width = 9)

#----------------------------------------------------------------------------------
# Denunciar Temer
aux <- dados[c("corrupcao","denunciaTemer")]
aux$denunciaTemer <- aux$denunciaTemer%>%as.factor()
dtemer <-
ggplot(data = na.omit(aux), aes(denunciaTemer)) + 
  geom_bar(aes(fill = corrupcao), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 13),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + ggtitle("Denúncia contra Temer")
#ggsave("dtemer.pdf", device = "pdf", height = 4, width = 7)

# LEGENDA

aux <- dados[c("corrupcao","denunciaTemer")]
aux$denunciaTemer <- aux$denunciaTemer%>%as.factor()
p1 <-
  ggplot(data = na.omit(aux), aes(denunciaTemer)) + 
  geom_bar(aes(fill = corrupcao), position = "fill") +
  scale_fill_manual(values = c("#0B775E","#35274A")) +
  guides(fill = guide_legend(keywidth = 2.5, keyheight = 2.5,title = "", direction = "horizontal")) +
  theme(legend.text=element_text(size=18));p1

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(p1)

library(gridExtra)
library(grid)
plots1 <- grid.arrange(jato, sexo,partido,dtemer,aval,mylegend,ncol=2, left="Porcentatgem")
ggsave(plots1,filename = "plots1.pdf", device = "pdf", height = 6, width = 10)

plots2 <- grid.arrange(hist, mylegend, esc, renda, ncol=2, left="   Porcentatgem                             Frequência")
ggsave(plots2,filename = "plots2.pdf", device = "pdf", height = 6, width = 10)






