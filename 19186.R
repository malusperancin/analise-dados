rm(list=ls())

#install.packages("ggplot2")
#install.packages("ggExtra")
#install.packages("gridExtra")

#setwd("C:/Users/maria/Desktop/projeto")

library(ggplot2)
library(ggExtra)
library(gridExtra)
library(RColorBrewer)


titulos <- c("horario", "temperatura", "vento", "umidade", "sensacao")

#pegamos o dado no arquivo e colocamos no data frame
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = titulos)

#convertemos e formatamos a data para POSIXct
cepagri[["horario"]] <- as.POSIXct(cepagri[["horario"]], format="%d/%m/%Y-%H:%M")

#pegamos os dados entre 01/01/2015 e 31/12/2020
cepagri <- cepagri[cepagri[["horario"]] >= "2015-01-01" & cepagri[["horario"]] <= "2020-12-31",]

#tiramos os dados que possuem erro/null
cepagri <- na.omit(cepagri) 

#transformamos a temp em numeric (os outros campos ja ta)
cepagri[["temperatura"]] <- as.numeric(cepagri[["temperatura"]])

for(i in 1:nrow(cepagri)){
  if(cepagri$sensacao[i] >= 99.9)
  {
    cepagri$sensacao[i] <- mean(c( cepagri$sensacao[i-1], cepagri$sensacao[i+1]))
  }
  if(cepagri$umidade[i] == 0)
  {
    #faz uma media de todos os indices de umidade do dia 
    cepagri$umidade[i] <- mean(cepagri$umidade[cepagri$horario >= cepagri$horario[i-1] & cepagri$horario < cepagri$horario[i]])
  }
}

dados <- cepagri #dados prontos

meses <- factor(month.abb[as.integer(format(as.Date(dados[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 
anos <- factor(as.integer(format(as.Date(dados[["horario"]]),"%Y")), levels = c(2015:2020), ordered = TRUE)


#TEMPERATURA POR HORARIO LINE
plot1 <- ggplot(dados, aes(y=temperatura, x=horario, colour=temperatura))
plot1 <- plot1 + geom_line()


#TEMPERATURA E SENSACAO JITTER
plot2 <- ggplot(dados, aes(x = sensacao, y = temperatura)) + geom_jitter(color="gray") + geom_smooth(method = "lm")


#VENTO POR MESES BOXPLOT
plot3 <- ggplot(dados,aes(x=meses,y=vento,group=meses,fill=meses))
plot3 <- plot3 +geom_boxplot()


#TEMPERATURA POR VENTO LINE
plot4 <- ggplot(dados, aes(y=temperatura, x=vento, colour=vento))
plot4 <- plot4 + geom_line()


#UMIDADE POR MESES BOXPLOT
plot5 <- ggplot(dados,aes(x=meses,y=umidade,group=meses,fill=meses))
plot5 <- plot5 +geom_boxplot()


plot_grid(plot1, plot2, plot3, plot4,plot5)



