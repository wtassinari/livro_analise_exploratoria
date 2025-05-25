A <- c(11, 20, 5, 23, 16, 21, 18, 16, 27, 24)
B <- c(23, 46, 18, 19, 23, 17, 28, 36, 25, 28)

boxplot(A,B)

dados <- data.frame(Metodo=rep(c("A","B"),each=10),Tempo=c(A,B))
dados

library(tidyverse)

ggplot(dados)+
  geom_boxplot(aes(Metodo,Tempo))+
  xlab("Método de treinamento") + ylab("Tempo (minutos)") 

ggplot(dados,aes(Metodo,Tempo))+
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(aes(fill=Metodo),show.legend = FALSE)+
  geom_point(pch=21, aes(fill=Metodo), alpha=0.5, show.legend = F,
             position=position_jitterdodge())+
  xlab("Método de treinamento") + ylab("Tempo (minutos)") 

