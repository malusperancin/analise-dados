rm(list=ls())

#install.packages("ggplot2")
#install.packages("ggExtra")
#install.packages("gridExtra")
#install.packages("xlsx")

#setwd("C:/Users/maria/Desktop/projeto")

library(ggplot2)
library(ggExtra)
library(gridExtra)
library(RColorBrewer)
library(xlsx)

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

h2018 <- dados[dados[["horario"]] > "2017-12-31" & dados[["horario"]] <= "2018-12-31",]
meses2018 <- factor(month.abb[as.integer(format(as.Date(h2018[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 

h2015 <- dados[dados[["horario"]] >= "2015-01-01" & dados[["horario"]] <= "2015-12-31",]
meses2015 <- factor(month.abb[as.integer(format(as.Date(h2015[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 


#TEMPERATURA POR HORARIO usando geom_line
plot1 <- ggplot(dados, aes(y=temperatura, x=horario, colour=temperatura))
plot1 <- plot1 + geom_line()
plot1


#VENTO E SENSACAO usando geom_smooth
plot2 <- ggplot(dados, aes(x = sensacao, y = vento)) + geom_smooth()
plot2


#VENTO POR MESES usando boxplot
plot3 <- ggplot(dados,aes(x=meses,y=vento,group=meses,fill=meses))
plot3 <- plot3 +geom_boxplot()
plot3

plot31 <- ggplot(h2015,aes(x=meses2015,y=vento,group=meses2015,fill=meses2015))
plot31 <- plot31 +geom_boxplot()
plot31

plot32 <- ggplot(h2018,aes(x=meses2018,y=vento,group=meses2018,fill=meses2018))
plot32 <- plot32 +geom_boxplot()
plot32

#VENTO UMIDADE E TEMPERATURA usando ggMarginal
plot4 <- ggplot(dados, aes(x=temperatura, y=vento, color=umidade)) + geom_point() + theme_bw()
plot4 <- ggMarginal(plot4, type="histogram") 
plot4

#UMIDADE POR ANOS usando boxplot
plot5 <- ggplot(dados,aes(x=anos,y=umidade,group=anos,fill=anos))
plot5 <- plot5 +geom_boxplot()
plot5

#UMIDADE EM 2018 usando boxplot
plot6 <- ggplot(h2018, aes(y=umidade, x=meses2018, group=meses2018,fill=meses2018))
plot6 <- plot6 + geom_boxplot()
plot6

# Tabela comparativa de umidade entre 2018 e 2015 
tabelaUmidade <- cbind(h2015$umidade, h2018$umidade)
colnames(tabelaUmidade) <- c("umidade 2015", "umidade 2018")
write.xlsx(tabelaUmidade,file="tabelaUmidade2015_2018.xlsx")
tabelaUmidade


# Tabela comparativa de ventos entre 09/2015 e 10/2015
outubro2015 <- dados[dados[["horario"]] > "2015-09-01" & dados[["horario"]] <= "2015-11-01",]
outubro2018 <- dados[dados[["horario"]] > "2018-09-01" & dados[["horario"]] <= "2018-11-01",]
tabelaVento <- cbind(as.character(as.Date(outubro2015$horario, format="%d/%m/%y")),outubro2015$vento, outubro2018$vento)
colnames(tabelaVento) <- c("data","vento 2015", "vento 2018")
write.xlsx(tabelaVento,file="tabelaVento2015_2018.xlsx")
tabelaVento


