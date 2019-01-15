#--------------------------------------------------------------------
# Roscona doida ----------------------------------------------------
#--------------------------------------------------------------------
rm(list=ls())
setwd("/home/furriel/Dropbox/Regressão Logística/scripts")
#----------------------------------------------------------------------------------
library(ggplot2);library(foreign);library(ggplot2);library(MASS);library(reshape);
library(dplyr);library(wesanderson);library(easyGgplot2)
#----------------------------------------------------------------------------------

donutgraph<-function(banco,legenda,filename){
  nome<-as.data.frame(table(banco))
  nome$fraction <- nome$Freq / sum(nome$Freq)
  nome <- nome[order(nome$fraction), ]
  nome$ymax <- cumsum(nome$fraction)
  nome$ymin <- c(0, head(nome$ymax, n=-1))
  
  gnome <- ggplot(nome, aes(fill=banco, ymax=ymax, ymin=ymin, xmax=2, xmin=4)) +
    geom_rect(colour="white") +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.position=c(.5, .5)) +
    scale_fill_manual(values=c("#0B775E","#35274A"),name=legenda) +
    geom_label(aes(label=paste(round(fraction,3)*100,"%"),x=3.5,
                   y=(ymin+ymax)/2),inherit.aes = TRUE, 
               show.legend = FALSE,size=7,color="white") +
    ylab("") +
    xlab("")
  gnome
  ggsave(gnome, file=filename, width=7, height=7,dpi=300)}

#===================================================================
# Corrupcao -------------------------------------

dados  <- read.csv("/home/furriel/Dropbox/Regressão Logística/scripts/corrupcao.csv", na.strings = ".")
dadosx <- dados[c("corrupcao","idade1", "escola", "Ljato", "denunciaTemer")]
dadosx <- na.omit(dadosx)
dadosx$corrupcao2[dadosx$corrupcao == "Corrupcao"]     <- "Corrupção"
dadosx$corrupcao2[dadosx$corrupcao == "Incompetencia"] <- "Incompetência"
table(dadosx$corrupcao2)
legenda1<-"O que custa mais ao Brasil \ncorrupção ou incompetência?"
banco1<-dadosx$corrupcao2
filename<-"donutcorrup.pdf"
donutgraph(banco1,legenda1,filename)

#===================================================================
# Aborto -------------------------------------

dados  <- read.csv("/home/furriel/Dropbox/Regressão Logística/scripts/dados-aborto.csv",sep = ";")
dados$aborto2[dados$aborto == "Nao"] <- "Não"
dados$aborto2[dados$aborto == "Sim"] <- "Sim"
table(dados$aborto2)
legenda1<-"Acredita que se justifica \n um aborto quando a saúde \n da mãe  está em perigo?"
banco1<-dados$aborto2
filename<-"donutaborto.pdf"
donutgraph(banco1,legenda1,filename)

