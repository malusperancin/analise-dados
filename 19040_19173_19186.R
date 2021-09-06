# Processamento e análise de dados climáticos (CEPAGRI)

# Giovanna Pavani Martelli - 19173
# Maria Luiza Sperancin Mancebo - 19186
# Vinícius Martins Cotrim - 19040

# Data: 7 de setembro de 2021

rm(list=ls()) # Reinicia o escopo

# Instalar as bibliotecas necessárias
install.packages("ggplot2")
install.packages("ggExtra")
install.packages("gridExtra")
install.packages("xlsx")
install.packages("tidyverse")
tinytex::install_tinytex()

# Setta a workplace (local onde o arquivo está e onde serão gerados os resultados)
setwd("C:/Users/Usuário/Documents/GitHub/analise-dados")

# Importa as bibliotecas necessárias
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(RColorBrewer)
library(xlsx)
library(tidyverse)

# Titulos da tabela dados
titulos <- c("horario", "temperatura", "vento", "umidade", "sensacao")

# Resgata os dados do arquivo .csv e colocamos em um data frame
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = titulos)

# Converte e formata a data para POSIXct com o formato dado
cepagri[["horario"]] <- as.POSIXct(cepagri[["horario"]], format="%d/%m/%Y-%H:%M")

# Seleciona os dados entre 01/01/2015 e 31/12/2020
cepagri <- cepagri[cepagri[["horario"]] >= "2015-01-01" & cepagri[["horario"]] <= "2020-12-31",]

# Retira os dados que possuem erro/são null
cepagri <- na.omit(cepagri)
cepagri <- cepagri[!is.na(cepagri$umidade),] # Retira os dados remanescentes no campo umidade

# Transforma os dados do campo temperatura em numeric (os demais já estão no formato correto)
cepagri[["temperatura"]] <- as.numeric(cepagri[["temperatura"]])

# Retira dados duvidosos
for(i in 1:nrow(cepagri)){
  if(cepagri$sensacao[i] >= 99.9) # Retira sensações térmicas >= a 99.9
  {
    # Média entre a sensação térmica do registro anterior e do próximo
    cepagri$sensacao[i] <- mean(c( cepagri$sensacao[i-1], cepagri$sensacao[i+1]))
  }
  if(cepagri$umidade[i] == 0)
  {
    # Troca o dado duvidoso da umidade por uma média de todos os índices de umidade do dia 
    cepagri$umidade[i] <- mean(cepagri$umidade[cepagri$horario >= cepagri$horario[i-1] & cepagri$horario < cepagri$horario[i]])
  }
}

# Dados formatados, prontos para serem processados
dados <- cepagri 

# Armazena todos os meses dos dados
meses <- factor(month.abb[as.integer(format(as.Date(dados[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 
# Armazena todos os anos dos dados
anos <- factor(as.integer(format(as.Date(dados[["horario"]]),"%Y")), levels = c(2015:2020), ordered = TRUE)

# Armazena todos os dados de 2018
h2018 <- dados[dados[["horario"]] > "2017-12-31" & dados[["horario"]] <= "2018-12-31",]
# Armazena os meses de 2018 em factor
meses2018 <- factor(month.abb[as.integer(format(as.Date(h2018[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 

# Armazena todos os dados de 2015
h2015 <- dados[dados[["horario"]] >= "2015-01-01" & dados[["horario"]] <= "2015-12-31",]
# Armazena os meses de 2018 em factor
meses2015 <- factor(month.abb[as.integer(format(as.Date(h2015[["horario"]]),"%m"))], levels = month.abb, ordered = TRUE) 

# Tema (formatação do texto) que será usado em cada gráfico
meu_tema = theme_bw() + 
  theme(plot.title = element_text(hjust = 0, size = 18),
        axis.title = element_text(hjust = 0.5, size = 11),
        axis.text = element_text(size = 10))


# -> Gráficos

#-----------------------------------------------------
# GRÁFICO 1: TEMPERATURA POR HORARIO usando geom_line
#-----------------------------------------------------
plot1 <- ggplot(dados, aes(y=temperatura, x=horario, colour=temperatura)) + 
  geom_line() + # Linha
  labs(fill='Temperatura em Cº', # Legenda
       title="Temperaturas entre os anos de 2015 e 2020", # Título
       x="Anos", # Informações do eixo X
       y="Temperatura(Cº)") + # Informações do eixo y
  meu_tema 
plot1

#------------------------------------------------
# GRÁFICO 2: VENTO E SENSACAO usando geom_smooth
#------------------------------------------------
plot2 <- ggplot(dados, aes(x = sensacao, y = vento)) +
  geom_smooth(method = "gam") +  # Linha com margem de valores
  labs(title="Relação entre vento e sensação térmica entre 2015 e 2020",
       x="Sensação Térmica", 
       y="Vento(Km/h)") +
  meu_tema
plot2

#--------------------------------------------
# GRÁFICOS 3: VENTO POR MESES usando boxplot
#--------------------------------------------

# Gráficos com as caixas (explicação dos elementos no relatório)

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

#-----------------------------------------------------------
# GRÁFICOS 4: VENTO UMIDADE E TEMPERATURA usando ggMarginal
#-----------------------------------------------------------
plot4 <- ggplot(dados, aes(x=temperatura, y=vento, color=umidade)) + 
  geom_point() + # Gráfico que mostra a concentração da relação entre os três dados analisados (vento, temperatura e umidade)  
  labs(fill= 'Umidade(g/Kg)',
       title="Registros da umidade de acordo com a \nrelação entre temperatura e vento\nentre 2015 e 2020", 
       x="Temperatura(Cº)", 
       y="Vento(Km/h)") +
  meu_tema
plot4 <- ggMarginal(plot4, type="histogram") # Parte do gráfico que apresenta as barras para melhor ver a concentração dos pontos
plot4

#---------------------------------------------
# GRÁFICOS 5: UMIDADE POR ANOS usando boxplot
#---------------------------------------------
plot5 <- ggplot(dados, aes(x=anos, y=umidade, group=anos, fill=anos)) +
  geom_boxplot(show.legend = FALSE) + # Gráfico com as caixas (explicação dos elementos no relatório)
  labs(fill= 'Umidade(g/Kg)', 
       title="Registros de umidade do ar entre os anos de 2015 e 2020", 
       x="Anos", 
       y="Umidade(g/Kg)") +
  meu_tema
plot5

#--------------------------------------------
# GRÁFICOS 6: UMIDADE EM 2018 usando boxplot
#--------------------------------------------
plot6 <- ggplot(h2018, aes(y=umidade, x=meses2018, group=meses2018,fill=meses2018)) + 
  geom_boxplot(show.legend = FALSE) + # Gráfico com as caixas (explicação dos elementos no relatório)
  labs(title="Registros da umidade do ar no ano de 2018", 
       x="Meses", 
       y="Umidade(g/Kg)") +
  meu_tema
plot6



# -> Tabelas

#-----------------------------------------------------------------------------------------
# TABELA 1: Tabela de temperaturas mínimas, máximas e médias entre os anos de 2015 a 2020
#-----------------------------------------------------------------------------------------

# Guarda os dados em uma nova variável para não estragar os dados base
dados_data <- dados

# Formata as datas só com o ano
dados_data$horario <- format(as.Date(dados$horario), "%Y")

# Seleciona as temperaturas mínimas, máximas e médias de cada ano (group_by)
temperaturas_anos <- dados_data %>% group_by(horario) %>% summarise(mínima=min(temperatura), máxima=max(temperatura), média=mean(temperatura))

# Muda os nomes dos campos da tabela
colnames(temperaturas_anos) <- c("Ano", "Mínima(ºC)", "Máxima(ºC)", "Média(ºC)")

# Gera arquivo .xlsx (excel)
write.xlsx(temperaturas_anos,file="tabelaTemperaturaAnos2015_2018.xlsx")
temperaturas_anos

#-----------------------------------------------------------
# TABELA 2: Tabela comparativa de umidade entre 2018 e 2015 
#-----------------------------------------------------------

# Guarda os dados em uma nova variável para não estragar os dados base
media_dia_2015 <- h2015
media_dia_2018 <- h2018

# Formata as datas só com o ano
media_dia_2015$horario <- format(as.Date(media_dia_2015$horario), "%d/%m")
media_dia_2018$horario <- format(as.Date(media_dia_2018$horario), "%d/%m")

# Seleciona as médias dos índices de umidade de 2015 e 2018
media_dia_2015 <- media_dia_2015 %>% group_by(horario) %>% summarise(umidade=mean(umidade))
media_dia_2018 <- media_dia_2018 %>% group_by(horario) %>% summarise(umidade=mean(umidade))

# Junta as tabelas de cada ano pelo cbind
tabelaUmidade <- cbind(media_dia_2015$horario, format(round(media_dia_2015$umidade, 2), nsmall = 2), format(round(media_dia_2018$umidade, 2), nsmall = 2))

# Muda os nomes dos campos da tabela
colnames(tabelaUmidade) <- c("Data", "Umidade.2015", "Umidade.2018")

# Gera arquivo .xlsx (excel)
write.xlsx(tabelaUmidade,file="tabelaUmidade2015_2018.xlsx")
tabelaUmidade

#-----------------------------------------------------------------------------------------
# TABELA 3: Tabela comparativa de ventos entre setembro e outubro nos anos de 2015 e 2018
#-----------------------------------------------------------------------------------------

# Seleciona todos os dados entre setembro e outubro de 2015 e de 2018, respectivamente
outubro2015 <- dados[dados[["horario"]] >= "2015-09-01" & dados[["horario"]] < "2015-11-01",]
outubro2018 <- dados[dados[["horario"]] >= "2018-09-01" & dados[["horario"]] < "2018-11-01",]

# Formata a data do horário, tirando o horário
outubro2015$horario <- as.Date(outubro2015$horario)
outubro2018$horario <- as.Date(outubro2018$horario)

# Seleciona os índices máximos de cada dia dentro dos dados já recortados de 2015 e 2018, respectivamente
max_2015 <- outubro2015 %>% group_by(horario) %>% summarise(vento=max(vento))
max_2018 <- outubro2018 %>% group_by(horario) %>% summarise(vento=max(vento))

# Junta as tabelas de cada ano pelo cbind
tabela_vento_2015_2018 <- cbind(format(as.Date(max_2015$horario), "%d/%m"), max_2015$vento, max_2018$vento)

# Muda os nomes dos campos da tabela
colnames(tabela_vento_2015_2018) <- c("Data", "Maximas.2015", "Maximas.2018")

# Gera arquivo .xlsx (excel)
write.xlsx(tabela_vento_2015_2018,file="tabelaVento2015_2018.xlsx")
tabela_vento_2015_2018

#----------------------------------------------------------------
# TABELA 4: Média da umidade e valores de tempo em todos os anos
#----------------------------------------------------------------

# Guarda os dados em uma nova variável para não estragar os dados base
umidade_anos <- dados

# Formata a data do horário, selecionando apenas o ano
umidade_anos$horario <- format(as.Date(umidade_anos$horario), "%Y")

# Seleciona as médias de umidade, temperatura e sensação térmica de cada ano, de 2015 a 2020
umidade_anos <- umidade_anos %>% group_by(horario) %>%
  summarise(media_umidade=mean(umidade), media_temperatura=mean(temperatura), media_sensacao=mean(sensacao))

# Muda os nomes dos campos da tabela
colnames(umidade_anos) <- c("Ano", "Media.Umidade", "Media.Temperatura", "Media.Sensacao")

# Gera arquivo .xlsx (excel)
write.xlsx(umidade_anos,file="tabelaUmidade2015_2020.xlsx")
umidade_anos

# Fim! :)