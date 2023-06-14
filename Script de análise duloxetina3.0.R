# Instalação e leitura das bibliotecas utilizadas

install.packages("dplyr")
install.packages("tidyr")
install.packages("gapminder")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("readr")

library(dplyr)
library(tidyr)
library(gapminder)
library(readxl)
library(writexl)
library(ggplot2)
library(readr)

# 1. Ler o arquivo de vendas de medicamentos da ANVISA (33 milhões de linhas, CUIDADO!)
depressao <- read.csv("DB-Anvisa2.csv")

# 2. Extrair os dados necessários para um novo dataframe
# Neste caso foram todos as linhas da substancia CLORIDRATO DE DULOXETINA de 2019 a 2021
cloridrato <- depressao %>% filter(ANO_VENDA > 2018, PRINCIPIO_ATIVO == "CLORIDRATO DE DULOXETINA" | PRINCIPIO_ATIVO ==  "DULOXETINA")

# 3. Salvar o dataframe filtrado para que não haja a necessidade de carregar sempre ambos os codigos anteriores
write.csv(cloridrato, file = "cloridrato_duloxetina.csv", row.names = FALSE)

# 4. Remover o dataframe de vendas totais da ANVISA para liberar memória pois o mesmo não será mais utilizado
rm(depressao)


#_____________________________________________________________________________________________________________________________#


### CARREGAR O DATAFRAME BRUTO DE VENDAS DO CLORIDRATO DE DULOXETINA ###
cloridrato <- read.csv("cloridrato_duloxetina.csv")

# 5. Verificar quantos valores NA existem no DF para definir se serão tratados ou se as colunas/linhas serão descartadas
colSums(is.na(cloridrato))

# Lembrando que o DF tem 280.256 linhas
# SEXO, IDADE e UNIDADE_IDADE = 217.936 linhas são NA, por ser bem mais do que 50% do dataframe não compensa tratar
# As demais colunas não tem valores NA

# 6. Dropar as colunas que contém dados desnecessários para análise:
# Neste caso, as colunas: 
# QTD_ATIVO_POR_UNID_FARMACOTEC, CONSELHO_PRESCRITOR, UF_CONSELHO_PRESCRITOR, CID10 -> não serão necessárias para a análise a seguir
# SEXO, IDADE e UNIDADE_IDADE -> muitos valores NA
cloridrato <- cloridrato[, -c(7, 11, 12, 14, 15, 16, 17)]

# 7. Dropar todas as linhas cujo:
# UNIDADE_MEDIDA_PRINCIPIO_ATIVO == "UNIDADE"
# TIPO_UNIDADE_FARMACOTECNICA == "MILILITRO" && "GRAMA"
# para padronizar as medidas
cloridrato <- subset(cloridrato, UNIDADE_MEDIDA_PRINCIPIO_ATIVO != "UNIDADE")
cloridrato <- subset(cloridrato, TIPO_UNIDADE_FARMACOTECNICA == "CÁPSULA")

# 9. Alterando "," para "." para que não haja perca de valores ao trocar o tipo de dados para numérico
cloridrato$QTD_UNIDADE_FARMACOTECNICA <- gsub(",", ".", cloridrato$QTD_UNIDADE_FARMACOTECNICA)

# 10. Mudando dados da coluna de caracteres para númericos
cloridrato$QTD_UNIDADE_FARMACOTECNICA <- as.numeric(cloridrato$QTD_UNIDADE_FARMACOTECNICA)

# 11. Padronizando os nomes do principio ativo para CLORIDRATO DE DULOXETINA, alterando os que estavam apenas como DULOXETINA
cloridrato <- cloridrato %>% mutate(PRINCIPIO_ATIVO = ifelse(PRINCIPIO_ATIVO == "DULOXETINA", "CLORIDRATO DE DULOXETINA", PRINCIPIO_ATIVO))

# 12. Agrupar as vendas por estado
cloridrato <- cloridrato %>% group_by(ANO_VENDA, UF_VENDA, PRINCIPIO_ATIVO, TIPO_UNIDADE_FARMACOTECNICA) %>% summarize(sum(QTD_UNIDADE_FARMACOTECNICA))
cloridrato <- rename(cloridrato, 'QTD_UNIDADE_FARMACOTECNICA' = 'sum(QTD_UNIDADE_FARMACOTECNICA)')

# 13. Salvar o dataframe novamente para que não seja necessário refazer os passos anteriores sempre que for executar a análise a seguir,
# mas manter o anterior também como base bruta e este como base tratada
write.csv(cloridrato, file = "cloridrato_duloxetina_tratado.csv", row.names = FALSE)

#_____________________________________________________________________________________________________________________________#


### CARREGAR O DATAFRAME FILTRADO DE VENDAS DO CLORIDRATO DE DULOXETINA ###
cloridrato <- read.csv("cloridrato_duloxetina_tratado.csv")



#_____________________________________________________________________________________________________________________________#


### CARREGAR O DATAFRAME DE POPULACAO ###
populacao <- read_excel("C:/Users/aline/Documents/R Studio/depressao/Populacao UF.xlsx")


# 14. Padronizar os nomes dos munícipios em maíusculas assim como no dataset principal
populacao <- mutate(populacao, NOME_DO_MUNICIPIO = toupper(NOME_DO_MUNICIPIO))

# 15. Salvar o dataset de população com os dados padronizados
write_xlsx(populacao, "C:/Users/aline/Documents/R Studio/depressao/Populacao Municipios1.xlsx")


#_____________________________________________________________________________________________________________________________#


### CARREGAR O DATAFRAME DE POPULACAO PÓS PADRONIZAÇÃO DOS DADOS ###
populacao <- read_excel("C:/Users/aline/Documents/R Studio/depressao/Populacao Municipios1.xlsx")
populacao <- read_excel("C:/Users/822161400/Downloads/Populacao Municipios1.xlsx")


#_____________________________________________________________________________________________________________________________#


# 16. Agrupar as populações estimadas por estado
populacao <- populacao %>% group_by(ANO, UF) %>% summarize(sum(POPULACAO_ESTIMADA))
populacao <- rename(populacao, 'POPULACAO_ESTIMADA' = 'sum(POPULACAO_ESTIMADA)')

# 17. Padronizar os nomes das colunas que serão cruzadas nos dataframes e mescla-los
cloridrato <- rename(cloridrato, UF = UF_VENDA)
cloridrato <- rename(cloridrato, ANO = ANO_VENDA)
vendas_estado_populacao <- inner_join(populacao, cloridrato, by = c("UF", "ANO"))


#_____________________________________________________________________________________________________________________________#


### Calcular a quantidade de capsulas de medicamento por pessoa em cada ano por estado ###

# 18. Converter a coluna "ANO" para o tipo factor para manter a ordem correta na visualização
vendas_estado_populacao$ANO <- factor(vendas_estado_populacao$ANO)

# 19. Criar uma coluna para armazenar quantas cápsulas por pessoa são vendidas/consumidas em cada estado
vendas_estado_populacao$QTD_UNIDADE_FARMACOTECNICA <- as.numeric(vendas_estado_populacao$QTD_UNIDADE_FARMACOTECNICA)
vendas_estado_populacao <- mutate(vendas_estado_populacao, CAPSULAS_POR_PESSOA = QTD_UNIDADE_FARMACOTECNICA /POPULACAO_ESTIMADA)

# 20. Criar o gráfico de barras agrupadas e exibir
grafico_vendas_estado_populacao <- ggplot(vendas_estado_populacao, aes(x = UF, y = CAPSULAS_POR_PESSOA, fill = ANO)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "UF", y = "Quantidade de cápsulas vendidas por pessoa") +
  ggtitle("Análise de demanda de consumo por estado do Brasil") +
  scale_fill_manual(values = c("#00FF00", "#FF0000", "#0000FF"), 
                    labels = c("2019", "2020", "2021")) + theme_bw()
print(grafico_vendas_estado_populacao)


#_____________________________________________________________________________________________________________________________#


### Top 10 do gráfico acima, em tabela ###

print(vendas_estado_populacao[order(vendas_estado_populacao$CAPSULAS_POR_PESSOA, decreasing = TRUE), ])


#_____________________________________________________________________________________________________________________________#


###### Calcular a média de capsulas de medicamento por pessoa em cada ano no Brasil ###

Brasil_indice_de_depressao <- vendas_estado_populacao %>% group_by(ANO, PRINCIPIO_ATIVO, TIPO_UNIDADE_FARMACOTECNICA) %>% summarize(sum(QTD_UNIDADE_FARMACOTECNICA), sum(POPULACAO_ESTIMADA))

# 21. Renomear as colunas criadas
Brasil_indice_de_depressao <- rename(Brasil_indice_de_depressao, QTD_UNIDADE_FARMACOTECNICA = "sum(QTD_UNIDADE_FARMACOTECNICA)")
Brasil_indice_de_depressao <- rename(Brasil_indice_de_depressao, POPULACAO_ESTIMADA = "sum(POPULACAO_ESTIMADA)")

# 22. Criar uma coluna para armazenar a média de cápsulas por pessoa vendidas/consumidas em cada ano
Brasil_indice_de_depressao <- mutate(Brasil_indice_de_depressao, CAPSULAS_POR_PESSOA = QTD_UNIDADE_FARMACOTECNICA / POPULACAO_ESTIMADA)

# 23. Criar o gráfico de barras e exibir

Brasil_grafico <- ggplot(data = Brasil_indice_de_depressao, aes(x = ANO, y = CAPSULAS_POR_PESSOA)) +
  geom_bar(stat = "identity", fill = "#1C49A4") + ylim(0, 1) +
  labs(x = "ANO", y = "MÉDIA DE CAPSULAS POR PESSOA", title = "Media da quantidade de capsulas p/ pessoa no Brasil")
print(Brasil_grafico)


#_____________________________________________________________________________________________________________________________#

##### Teste de Hipotese - Teste t de Student #####
# Unilateral comum

#   H0 = Hipótese nula = vendas de medicamentos em 2019 >= aos anos de 2020 e 2021 (anos de pandemia)

#   Ha = Hipótese Alternativa  = vendas de medicamentos em 2019 é < os anos de 2020 e 2021 (anos de pandemia)

# 24. Definir os valores necessários para o cálculo do teste
# Sendo eles a média do H0, a média da amostra (que é a média da média dos anos de 2020 e 2021) e o desvio padrão da amostra
mediaH0 <- unlist(Brasil_indice_de_depressao[1, 6])
mediaAmostra <- unlist((Brasil_indice_de_depressao[2, 6] + Brasil_indice_de_depressao[3, 6])/2)
sAmostra <- sd(Brasil_indice_de_depressao$CAPSULAS_POR_PESSOA)

# 25. Realizar o teste e definir o valor crítico
tStudent <- unlist((mediaH0 - mediaAmostra) / (sAmostra / sqrt(nrow(Brasil_indice_de_depressao)))); tStudent
xc <- qt(0.05, 2); xc

  ## Explicação dos resultados ##

# o teste t está dentro da região crítica, logo tomamos a hipótese alternativa:
# Ha = vendas de medicamentos em 2019 é < os anos de 2020 e 2021 (anos de pandemia). Ou seja, a pandemia teve influência sobre o consumo médio.


#_____________________________________________________________________________________________________________________________#


### ANALISE DESCRITIVA DOS DADOS ###

# 1 - Explicaçao do conjunto de dados ----------------------------------------

populacao <- read_excel("C:/Users/aline/Documents/R Studio/depressao/Populacao Municipios1.xlsx")
cloridrato <- read.csv("cloridrato_duloxetina_tratado.csv")

# Rodar o c�digo acima para ter tamb�m os conjuntos de dados que foram gerados na an�lise das amostras

print(vendas_estado_populacao)
print(Brasil_indice_de_depressao)

# padronizar nome de coluna
cloridrato <- rename(cloridrato, UF = UF_VENDA)
cloridrato <- rename(cloridrato, ANO = ANO_VENDA)

# Transformar a coluna ANO de todos os datasets no tipo factor para facilitar a visualiza��o da an�lise abaixo
cloridrato$ANO <- factor(cloridrato$ANO)
populacao$ANO <- factor(populacao$ANO)

# 2 - Explicar as variáveis --------------------------------------------------
# A descri��o das colunas consta no PDF do estudo
str(cloridrato)
str(populacao)
str(vendas_estado_populacao)
str(Brasil_indice_de_depressao)

# 3 - Descrever cada variável ------------------------------------------------

# A descri��o ser� feita em cima da duas amostras utilizadas na an�lisee do c�digo acima.
# Uma considerando o ano de 2019, e outra considerando o ano de 2020 e 2021

# Algumas colunas se repetem nas bases pois as bases "vendas_estado_populacao" e "Brasil_indice_de_depressao"
# foram gerados a partir das bases "populacao" e "cloridrato"
# Tendo isso, foi feita a an�lise em cima da base "vendas_estado_populacao" e "Brasil_indice_de_depressao"
# pois elas t�m as colunas das bases "populacao" e "cloridrato" tamb�m

D_2019_vendasUF <- vendas_estado_populacao %>% filter(ANO == 2019)
D_2020_21_vendasUF <- vendas_estado_populacao %>% filter(ANO %in% c(2020, 2021))

D_2019_BrasilIndice <- Brasil_indice_de_depressao %>% filter(ANO == 2019)
D_2020_21_BrasilIndice <- Brasil_indice_de_depressao %>% filter(ANO %in% c(2020, 2021))

## Qualitativa ##

# 1 para cada UF e para cada ano
table(cloridrato$UF)
table(vendas_estado_populacao %>% select(UF))
table(populacao$UF)

# 27 resultados do mesmo valor para cada ano. No caso da base Brasil � 1 resultado para cada ano
table(cloridrato$PRINCIPIO_ATIVO)
table(vendas_estado_populacao$PRINCIPIO_ATIVO)
table(Brasil_indice_de_depressao$PRINCIPIO_ATIVO)

table(cloridrato$TIPO_UNIDADE_FARMACOTECNICA)
table(vendas_estado_populacao$TIPO_UNIDADE_FARMACOTECNICA)
table(Brasil_indice_de_depressao$TIPO_UNIDADE_FARMACOTECNICA)

table(cloridrato$ANO)
table(vendas_estado_populacao$ANO)
table(Brasil_indice_de_depressao$ANO)
table(populacao$ANO)

# Quantitativa

summary(D_2019_vendasUF$QTD_UNIDADE_FARMACOTECNICA)
summary(D_2020_21_vendasUF$QTD_UNIDADE_FARMACOTECNICA)
var(D_2019_vendasUF$QTD_UNIDADE_FARMACOTECNICA)
var(D_2020_21_vendasUF$QTD_UNIDADE_FARMACOTECNICA)
sd(D_2019_vendasUF$QTD_UNIDADE_FARMACOTECNICA)
sd(D_2020_21_vendasUF$QTD_UNIDADE_FARMACOTECNICA)

summary(D_2019_vendasUF$POPULACAO_ESTIMADA)
summary(D_2020_21_vendasUF$POPULACAO_ESTIMADA)
var(D_2019_vendasUF$POPULACAO_ESTIMADA)
var(D_2020_21_vendasUF$POPULACAO_ESTIMADA)
sd(D_2019_vendasUF$POPULACAO_ESTIMADA)
sd(D_2020_21_vendasUF$POPULACAO_ESTIMADA)

summary(D_2019_vendasUF$CAPSULAS_POR_PESSOA)
summary(D_2020_21_vendasUF$CAPSULAS_POR_PESSOA)
var(D_2019_vendasUF$CAPSULAS_POR_PESSOA)
var(D_2020_21_vendasUF$CAPSULAS_POR_PESSOA)
sd(D_2019_vendasUF$CAPSULAS_POR_PESSOA)
sd(D_2020_21_vendasUF$CAPSULAS_POR_PESSOA)

summary(D_2019_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)
summary(D_2020_21_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)
var(D_2019_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)
var(D_2020_21_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)
sd(D_2019_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)
sd(D_2020_21_BrasilIndice$QTD_UNIDADE_FARMACOTECNICA)

summary(D_2019_BrasilIndice$POPULACAO_ESTIMADA)
summary(D_2020_21_BrasilIndice$POPULACAO_ESTIMADA)
var(D_2019_BrasilIndice$POPULACAO_ESTIMADA)
var(D_2020_21_BrasilIndice$POPULACAO_ESTIMADA)
sd(D_2019_BrasilIndice$POPULACAO_ESTIMADA)
sd(D_2020_21_BrasilIndice$POPULACAO_ESTIMADA)

summary(D_2019_BrasilIndice$CAPSULAS_POR_PESSOA)
summary(D_2020_21_BrasilIndice$CAPSULAS_POR_PESSOA)
var(D_2019_BrasilIndice$CAPSULAS_POR_PESSOA)
var(D_2020_21_BrasilIndice$CAPSULAS_POR_PESSOA)
sd(D_2019_BrasilIndice$CAPSULAS_POR_PESSOA)
sd(D_2020_21_BrasilIndice$CAPSULAS_POR_PESSOA)


# 4 - Verificar as relações entre variáveis ----------------------------------
# criar um conjunto somente com as variáveis quantitativas

D_2019_BrasilIndice_num <- D_2019_BrasilIndice[, c(4, 5, 6)]
D_2020_21_BrasilIndice_num <- D_2020_21_BrasilIndice[, c(4, 5, 6)]

D_2019_vendasUF_num <- D_2019_vendasUF[, c(3, 6, 7)]
D_2020_21_vendasUF_num <- D_2020_21_vendasUF[, c(3, 6, 7)]

# fazer a matriz de correlação
cor(D_2019_BrasilIndice_num)
cor(D_2020_21_BrasilIndice_num)

cor(D_2019_vendasUF_num)
cor(D_2020_21_vendasUF_num)
