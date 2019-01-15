rm(list = ls())

pkgs <- c("foreign", "VGAM", "MASS", "ggplot2", "wesanderson", "xtable", "plotROC")
sapply(pkgs, require, character.only = T)

setwd("/home/andrefbm/Dropbox/Observatório 2018/Dados")

FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Leitura de dados
dados           <- read.table("dadosm3.csv", sep = ";", na.strings = ".", stringsAsFactors = F, header = T)
dados$aborto    <- relevel(factor(dados$aborto), ref = "No, it is not justified")
dados$freqmissa <- factor(dados$freqmissa)
dados           <- dados[!is.na(dados$aborto), ]
dados           <- dados[!is.na(dados$freqmissa), ]
dados           <- dados[!is.na(dados$escolaridade), ]

levels(dados$aborto) <- c("Não", "Sim")

head(dados)

# write.table(x = dados, file = "dados-aborto.csv", sep = ";", row.names = F, quote = F)

# Análise descritiva
setwd("/home/andrefbm/Dropbox/5° Série/Análise de Dados Categóricos/Regressão Logística")
with(dados, table(aborto, escolaridade))
with(dados, table(aborto, freqmissa))

## Aborto vs Missa
ggplot(data = dados, aes(aborto)) + 
  geom_bar(aes(fill = freqmissa), position = "dodge") +
  scale_fill_manual(values = wes_palette("Cavalcanti"), 
                     labels = c("Mais de uma vez na semana", "Uma vez na semana",
                                "Uma vez no mês", "Uma ou duas vezes no ano",
                                "Nunca")) +
  labs(x = "", y = "Frequência", fill = "Freq. Missa/Culto religioso") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 16))
ggsave("aborto-plot1.pdf", device = "pdf", height = 5, width = 9)

## Aborto vs Escolaridade
ggplot(data = dados, aes(x = escolaridade)) + 
  geom_bar(aes(fill = aborto), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = sort(unique(dados$escolaridade))) +
  scale_fill_manual(values = wes_palette("Cavalcanti")) +
  labs(x = "Escolaridade", y = "Frequência", fill = "") +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 16))
ggsave("aborto-plot2.pdf", device = "pdf", height = 5, width = 9)

# --------------------------------------------------------------------------------------------
# Predição
x1           <- seq(min(dados$escola, na.rm = T), max(dados$escolaridade, na.rm = T))
x2           <- levels(dados$)[-c(6, 7)]
teste        <- expand.grid(x1, x2)
names(teste) <- c("escolaridade", "freqmissa")

preditos <- predict(mod.aborto, newdata = teste, type = "response")
teste    <- cbind(teste, preditos = preditos)

ggplot(data = teste, aes(x = escolaridade, y = preditos, color = factor(freqmissa))) +
  geom_point(size = 3.0) + geom_line(linetype = 3) +
  labs(x = "Escolaridade", y = "Probabilidade", color = "Freq. Missa/Culto religioso") +
  scale_color_manual(values = wes_palette("Cavalcanti"), 
                     labels = c("Mais de uma vez na semana", "Uma vez na semana",
                                "Uma vez no mês", "Uma ou duas vezes no ano",
                                "Nunca")) +
  scale_x_continuous(breaks = x1) +
  scale_y_continuous(limits = range(preditos), breaks = seq(min(preditos), max(preditos), l = 5),
                     labels = FF(seq(min(preditos), max(preditos), l = 5), 2)) +
  theme(panel.grid.minor.x = element_blank(), text = element_text(size = 16)) +
ggsave("plot-prob.pdf", device = "pdf", height = 5, width = 9)
