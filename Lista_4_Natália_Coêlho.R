#Lista 4 
#An�lise de Dados
#Nat�lia Co�lho de Souza Oliveira

#1
#link para reposit�rio
https://github.com/coelhonatalia/Lista_4_Natalia_Coelho.git


#2.1
#instalando o pacote
install.packages("reasxl")

#carregando o pacote
require("readxl")

#definindo o diret�rio 
setwd("C:/Users/NATHALIA/Documents/CP-Mestrado/An�lise de Dados - Davi Moreira")

#carregar base de dados 
dados_pnud <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx")


#2.2

#instalando pacote
install.packages("tidyverse")

#abrindo pacote
library(tidyverse)

#abrindo pacote
library(magrittr)

#filtrar base de dados para os casos apenas de Pernambuco no ano de 2010
pe_pnud <- dados_pnud %>% filter(UF == 26, ANO == 2010)

#carregando base de dados
load("docentes_pe_censo_escolar_2016.RData")

#carregando base de dados
load("matricula_pe_censo_escolar_2016.RData")

#filtrando por docentes que possuem mais de 18 anos e menos de 70.
docentes_pe_selecao <- docentes_pe %>% filter(NU_IDADE >= 18, NU_IDADE <= 70)

#checando os valores m�nimos e m�ximos para averiguar se realmente s� cont�m docentes igual/acima de 18 anos e igual/abaixo de 70.
summary(docentes_pe_selecao$NU_IDADE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
18.00   32.00   39.00   39.96   48.00   70.00  

#2.3
#filtrando por alunos que possuem mais de 1 ano e menos de 25.  
matriculas_pe_selecao <- matricula_pe %>% filter(NU_IDADE >= 1, NU_IDADE <= 25)

#checando os valores m�nimos e m�ximos para averiguar se realmente s� cont�m alunos igual/acima de 1 ano e igual/abaixo 25.
summary(matriculas_pe_selecao$NU_IDADE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.00    8.00   12.00   11.63   15.00   25.00 

#2.4
#carregando pacote
require(tidyr)

#criar nova base de dados para agrupar o n�mero de matr�culas por municipio
#filtrando as vari�veis ID_MATRICULA e CO_MUNICIPIO da base de dados
matricula_municipio <- matriculas_pe_selecao %>% select(ID_MATRICULA, CO_MUNICIPIO) %>% group_by(CO_MUNICIPIO) %>% summarise(Mat_Muni = n())

#filtrando as vari�veis CO_PESSOA_FISICA e CO_MUNICIPIO da base de dados
docentes_municipio <- docentes_pe_selecao %>% select(CO_PESSOA_FISICA, CO_MUNICIPIO) %>% group_by(CO_MUNICIPIO) %>% summarise(Doc_Muni = n ())

#juntando as bases filtradas em uma nova base e criando a nova vari�vel Mat_Doc que representa as matr�culas por docente
matricula_docente <- inner_join(matricula_municipio, docentes_municipio) %>% mutate(Mat_Doc = Mat_Muni / Doc_Muni)

#2.4 Estat�stica descritiva da vari�vel Mat_Doc da base "Matricula_Docente"
summary(matricula_docente [,4])
Mat_Doc     
Min.   :4.431  
1st Qu.:5.464  
Median :5.945  
Mean   :6.043  
3rd Qu.:6.584  
Max.   :9.557  

#2.5

#filtrando as vari�veis IDHM e Codmun7 da base de dados
PNUD_IDHM <- pe_pnud %>% select(IDHM, Codmun7) %>% group_by(Codmun7)

#juntando em uma �nica base a base PNUD_IDHM e Matricula_Docente, atrav�s das vari�veis de mesmo calor CO_MUNICIPIO e Codmun7
(IDHM_DOC_MAT <- inner_join(matricula_docente, PNUD_IDHM, by = c("CO_MUNICIPIO" = "Codmun7"))) 

#carregando pacote
require(dplyr)

#ordenando de forma decrescente a vari�vel Mat_Doc
IDHM_DOC_MAT %>% arrange(desc(Mat_Doc))

#Apresentado os valores das colunas da primeira linha
head(IDHM_DOC_MAT)
#Resposta
CO_MUNICIPIO Mat_Muni Doc_Muni Mat_Doc  IDHM
<dbl>    <int>    <int>   <dbl> <dbl>
  1      2615805     6986      731    9.56 0.519


#TUPANATINGA � o munic�pio correspondente ao c�digo 2615805, sendo portanto ele o com maior n�mero de matr�culas por docentes e apresenta o IDHM  de 0.519

#2.6
#correla��o linear de pearson entre as vari�veis Mat_Doc e IDHM
cor(matricula_docente$Mat_Doc, PNUD_IDHM$IDHM)
#Resposta
[1] -0.5057435

#2.7
#Salvar arquivo em R.data
save(IDHM_DOC_MAT, file ="IDHM_DOC_MAT.RData")


#3
#carregando o pacote
require(ggplot2)

#gerando o gr�fico de dispers�o 
ggplot(data = IDHM_DOC_MAT, aes(x = Mat_Doc, y = IDHM) ) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Matr�culas por Docente", y = "IDHM")


