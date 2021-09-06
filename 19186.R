rm(list=ls())

#install.packages("ggplot2")
#install.packages("ggExtra")
#install.packages("gridExtra")
#install.packages("xlsx")
#install.packages
#tinytex::install_tinytex()

setwd("C:/Users/vinic/Documents/GitHub/analise-dados")

library(ggplot2)
library(ggExtra)
library(gridExtra)
library(RColorBrewer)
library(xlsx)
library(tidyverse)

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

# Tema que será usado em cada gráfico
meu_tema = theme_bw() + 
  theme(plot.title = element_text(hjust = 0, size = 18),
        axis.title = element_text(hjust = 0.5, size = 11),
        axis.text = element_text(size = 10))


#TEMPERATURA POR HORARIO usando geom_line
plot1 <- ggplot(dados, aes(y=temperatura, x=horario, colour=temperatura)) + 
  geom_line() +
  labs(fill='Temperatura em Cº', 
       title="Temperaturas entre os anos de 2015 e 2020", 
       x="Anos", 
       y="Temperatura(Cº)") +
  meu_tema
plot1


#VENTO E SENSACAO usando geom_smooth
plot2 <- ggplot(dados, aes(x = sensacao, y = vento)) +
  geom_smooth(method = "gam") +  
  labs(title="Relação entre vento e sensação térmica", 
       x="Sensação Térmica", 
       y="Vento(Km/h)") +
  meu_tema
plot2


#VENTO POR MESES usando boxplot
plot3 <- ggplot(dados,aes(x=meses,y=vento,group=meses,fill=meses)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Ventos nos meses dos anos de 2015 até 2020", 
       x="Meses", 
       y="Velocidade(Km/h)") +
  meu_tema
plot3

plot31 <- ggplot(h2015,aes(x=meses2015,y=vento,group=meses2015,fill=meses2015)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Ventos nos meses do ano de 2015", 
       x="Meses", 
       y="Velocidade(Km/h)") +
  meu_tema
plot31

plot32 <- ggplot(h2018,aes(x=meses2018,y=vento,group=meses2018,fill=meses2018)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Ventos nos meses do ano de 2018", 
       x="Meses", 
       y="Velocidade(Km/h)") +
  meu_tema
plot32

#VENTO UMIDADE E TEMPERATURA usando ggMarginal
plot4 <- ggplot(dados, aes(x=temperatura, y=vento, color=umidade)) + 
  geom_point() + 
  labs(fill= 'Umidade(g/Kg)', 
       title="Registros da umidade de acordo com a \nrelação entre temperatura e vento", 
       x="Temperatura(Cº)", 
       y="Vento(Km/h)") +
  meu_tema
plot4 <- ggMarginal(plot4, type="histogram") 
plot4

#UMIDADE POR ANOS usando boxplot
plot5 <- ggplot(dados, aes(x=anos, y=umidade, group=anos, fill=anos)) +
  geom_boxplot(show.legend = FALSE) +
  labs(fill= 'Umidade(g/Kg)', 
       title="Registros de umidade do ar entre os anos de 2015 e 2020", 
       x="Anos", 
       y="Umidade(g/Kg)") +
  meu_tema
plot5

#UMIDADE EM 2018 usando boxplot
plot6 <- ggplot(h2018, aes(y=umidade, x=meses2018, group=meses2018,fill=meses2018)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title="Registros da umidade do ar no ano de 2018", 
       x="Meses", 
       y="Umidade(g/Kg)") +
  meu_tema
plot6

# Tabela comparativa de umidade entre 2018 e 2015 

media_dia_2015 <- h2015
media_dia_2018 <- h2018

media_dia_2015$horario <- format(as.Date(media_dia_2015$horario), "%d/%m")
media_dia_2018$horario <- format(as.Date(media_dia_2018$horario), "%d/%m")

media_dia_2015 <- media_dia_2015 %>% group_by(horario) %>% summarise(umidade=mean(umidade))
media_dia_2018 <- media_dia_2018 %>% group_by(horario) %>% summarise(umidade=mean(umidade))

tabelaUmidade <- cbind(media_dia_2015$horario, format(round(media_dia_2015$umidade, 2), nsmall = 2), format(round(media_dia_2018$umidade, 2), nsmall = 2))
colnames(tabelaUmidade) <- c("Data", "Umidade.2015", "Umidade.2018")
write.xlsx(tabelaUmidade,file="tabelaUmidade2015_2018.xlsx")
tabelaUmidade

# Tabela comparativa de ventos entre setembro e outubro nos anos de 2015 e 2018

outubro2015 <- dados[dados[["horario"]] >= "2015-09-01" & dados[["horario"]] < "2015-11-01",]
outubro2018 <- dados[dados[["horario"]] >= "2018-09-01" & dados[["horario"]] < "2018-11-01",]

outubro2015$horario <- as.Date(outubro2015$horario)
outubro2018$horario <- as.Date(outubro2018$horario)

max_2015 <- outubro2015 %>% group_by(horario) %>% summarise(vento=max(vento))
max_2018 <- outubro2018 %>% group_by(horario) %>% summarise(vento=max(vento))

tabela_vento_2015_2018 <- cbind(format(as.Date(max_2015$horario), "%d/%m"), max_2015$vento, max_2018$vento)
colnames(tabela_vento_2015_2018) <- c("Data", "Maximas.2015", "Maximas.2018")

write.xlsx(tabela_vento_2015_2018,file="tabelaVento2015_2018.xlsx")
tabela_vento_2015_2018

# Média da umidade e valores de tempo em todos os anos

umidade_anos <- dados
umidade_anos$horario <- format(as.Date(umidade_anos$horario), "%Y")

# Tem umidade NA ainda
umidade_anos <- umidade_anos[!is.na(umidade_anos$umidade),]

umidade_anos <- umidade_anos %>% group_by(horario) %>%
  summarise(media_umidade=mean(umidade), media_temperatura=mean(temperatura), media_sensacao=mean(sensacao))

colnames(umidade_anos) <- c("Ano", "Media.Umidade", "Media.Temperatura", "Media.Sensacao")

write.xlsx(umidade_anos,file="tabelaUmidade2015_2020.xlsx")
umidade_anos

# Fazer 1 tabelas
# Fazer o PDF se quiser

