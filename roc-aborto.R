rm(list = ls())

setwd("C:/Users/User/Dropbox/5° Série/Análise de Dados Categóricos/Regressão Logística/scripts")

pkgs <- c("foreign", "VGAM", "MASS", "ggplot2", "wesanderson", "xtable", "ROCR")
sapply(pkgs, require, character.only = T)

FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Leitura de dados
dados           <- read.table("dados-aborto.csv", sep = ";", na.strings = ".", stringsAsFactors = F, header = T)
dados$aborto    <- relevel(factor(dados$aborto), ref = "Nao")
dados$freqmissa <- factor(dados$freqmissa)
head(dados)
dim(dados)

# Ajuste do modelo
mod <- glm(aborto ~ escolaridade + freqmissa, data = dados, family = binomial(link = "logit")) 
summary(mod)

# Curva ROC
prob <- predict(mod, type = "response")    
pred <- prediction(prob, dados$aborto)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")  
auc  <- performance(pred, measure = "auc")
df   <- data.frame(x = perf@x.values[[1]], y = perf@y.values[[1]])


setwd("C:/Users/User/Dropbox/5° Série/Análise de Dados Categóricos/Regressão Logística")

ggplot(data = df, aes(x = x, y = y)) + 
  geom_line(size = 1.1, col = "#0080ff") +
  geom_abline(slope = 1, intercept = 0, size = 1.0, linetype = 2) +
  labs(x = "1 - Especificidade", y = "Sensibilidade") +
  ggtitle(label = "", subtitle = paste("AUC =", FF(auc@y.values[[1]], 4))) +
  theme(text = element_text(size = 16), plot.subtitle = element_text(hjust = 0.5)) 
ggsave("roc-aborto.pdf", device = "pdf", height = 7, width = 9)


# Curva ROC ideal
set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1   <- rnorm(200, mean = D.ex, sd = .65)
M2   <- rnorm(200, mean = D.ex, sd = 1.5)
  
test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)  
D.ex <- test$D
M.ex <- test$M1
mu1  <- mean(M.ex[D.ex == 1])
mu0  <- mean(M.ex[D.ex == 0])
s1   <- sd(M.ex[D.ex == 1])
s0   <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)
  
binorm.roc <- data.frame(c = c.ex, FPF = pnorm((mu0 - c.ex)/s0), TPF = pnorm((mu1 - c.ex)/s1))
ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity", n.cuts = 0, size = 1.1, col = "#0080ff") +
  geom_abline(slope = 1, intercept = 0, size = 1.0, linetype = 2) +
  labs(x = "1 - Especificidade", y = "Sensibilidade") +
  theme(text = element_text(size = 16), plot.subtitle = element_text(hjust = 0.5)) 
ggsave("roc-ideal.pdf", device = "pdf", height = 7, width = 9)




