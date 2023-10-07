#------------------------------------------------------------------------------#
#                                                                              #
#                         Introdução ao R e Aplicações                         #
#                                 R-Ladies Goiânia                             #
#                                                                              #
#                           Uma ferramenta e tanto {dplyr}                     #
#                                                                              #
#                              Jennifer Luz Lopes                              #
#                                                                              #
#------------------------------------------------------------------------------#

# Instalação dos pacotes --------------------------------------------------

# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("DT")

# Carregando os pacotes ---------------------------------------------------

library(tidyverse)# Coleção de pacotes para funcionarem em conjunto no R
library(janitor)  # Examinar e limpar dados
library(ggplot2)  # visualização gráfica
library(dplyr)    # Manipulação de dados
library(readr)    # Leitura de dados separados por vírgula
library(DT)       # Formatação de DataTables

# Importando dados --------------------------------------------------------

nhanes <- read.csv("nhanes.csv") 


# Importação via environment ----------------------------------------------



# Visualização dos dados --------------------------------------------------

# Função datatable() {Pacote DT}

datatable(nhanes)

# Limpeza dos dados -------------------------------------------------------
# Funcção rename
# Primeiro o nome da variável nova = variável original

dados <- nhanes %>% 
  dplyr::rename(genero= Gender, idade= Age, idade_dec=AgeDecade, raca= Race1,
         educacao= Education, estado_civil= MaritalStatus, renda= HHIncome,
         casa= HomeOwn,trabalho= Work, peso= Weight, altura= Height,
         imc= BMI, saude_genero= HealthGen, horas_sono= SleepHrsNight, 
         atividade_fisica= PhysActive, atividade_diaria= PhysActiveDays, 
         fumante=SmokeNow, ano= SurveyYr) 

# Visualização dos dados -------------------------------------------------------

dados %>%
  DT::datatable() 

# Inspeção dos dados ------------------------------------------------------

View(dados)
head(dados,n = 10)
dplyr::glimpse(dados)

# Removendo colunas ------------------------------------------------------------

dados <- dados %>% 
  dplyr::select (-DaysPhysHlthBad, -DaysMentHlthBad, -TVHrsDay) 

# Concatenar

dados <- dados %>% 
  dplyr::select (-c(DaysPhysHlthBad, DaysMentHlthBad, TVHrsDay))

# Removendo NAs -----------------------------------------------------------
# A função drop_na() elimina todas as linhas que contém missings no dataset.

dados <- dados %>%
  tidyr::drop_na()

###############################################################################
# Como ficaram os dados? --------------------------------------------------
 
dados %>% 
  DT::datatable()

# Função SELECT () -----------------------------------------------------------

dados %>%
  dplyr::select(genero, idade) %>% 
  DT::datatable()

# Select você também ------------------------------------------------------

# estado_civil
# raça

# Função select e contains ------------------------------------------------

dados %>%
  dplyr::select(contains("idade")) %>% 
  DT::datatable()

# Função select (starts_with e ends_with) ---------------------------------

dados %>% 
  dplyr::select(starts_with("R")) %>% 
  DT::datatable()

dados %>% 
  dplyr::select(ends_with("E")) %>% 
  DT::datatable()

# Função Select (var:var) ------------------------------------------------------



# Função Select  (-var) ----------------------------------------------------------

dados %>% 
  dplyr::select(-ID) %>% 
  DT::datatable()

# Função Select conjunto (-(var:var)) ----------------------------------------



###############################################################################
# Função MUTATE () ------------------------------------------------------------
# .before = antes
# .after = depois

# 1. Criar uma nova variável com um valor específico

dados %>% 
  dplyr::mutate(nova_var= "Americanos", .before = genero) %>% 
  DT::datatable()
  
dados %>% 
  dplyr::mutate(nova_var= "Americanos", .after= genero) %>% 
  DT::datatable()

# 2. Criar uma nova variável com base em outras variáveis

dados %>% 
  dplyr::mutate(altura_polegadas= altura/2.54, .after = altura) %>%
  DT::datatable()

# 3. Alterar uma variável existente
# round= arredondamentos

dados %>% 
  dplyr::mutate(imc = round(imc, digits = 0)) %>% 
  DT::datatable()

# Mutate você tambem ------------------------------------------------------

# .before = antes
# .after = depois

# crie uma nova variável e atribua o valor 100
# new_var depois de ano

# Função FILTER () -------------------------------------------------------
# Operadores binários 

# x < y
# x > y
# x <= y
# x >= y
# x == y
# x != y

# 1. Usamos == para selecionar todas as observações que atendem aos critérios

dados %>% 
  dplyr::filter(genero == "female") %>% 
  dplyr::select(ID, genero) %>% 
  DT::datatable()

# 2. Usamos != para selecionar todas as observações que não atendem aos critérios
  
dados %>% 
  dplyr::filter(saude_genero!= "Good") %>% 
  dplyr::select(genero, saude_genero) %>% 
  DT::datatable()
  
# 3. Podemos combinar comparações e operadores lógicos

dados %>% 
  dplyr::filter(saude_genero == "Good" | saude_genero == "Vgood" |
                  horas_sono > 7) %>%
  dplyr::select(saude_genero, horas_sono) %>% 
  DT::datatable()

# Filter() você tambem ------------------------------------------------------

# filter genero feminino e idade maior e igual que 30

# Função SUMMARIZE () --------------------------------------------------------

# Qual a média de altura por gênero?
# acrescentar mais funções (mediana, sd, var)

dados %>% 
  dplyr::group_by(genero, raca) %>% 
  dplyr::summarize(mean_altura = mean(altura))%>% 
  DT::datatable()
      
# Função GROUP-BY () ---------------------------------------------------------

dados %>% 
  dplyr::group_by(ano, genero) %>% 
  dplyr::summarise(mean_atividade_diária= mean(atividade_diaria)) %>% 
  DT::datatable()
           
(atividade <- dados %>%
    dplyr::group_by(idade, genero) %>% 
    dplyr::summarise(mean_atividade_diária= mean(atividade_diaria))%>% 
    dplyr::arrange(desc(mean_atividade_diária)) %>% 
    DT::datatable())

# Função COUNT () -----------------------------------------------------------
# acrescentar mais variáveis

dados %>% 
  dplyr::count(idade_dec, genero) %>% 
  DT::datatable()
  
# Função ARRANGE () ----------------------------------------------------------

dados %>% 
  dplyr::arrange(idade) %>% 
  DT::datatable()

dados %>% 
  dplyr::arrange(desc(idade)) %>% 
  DT::datatable()

# Criando novas tabelas ---------------------------------------------------

(altura_feminina <- dados %>%
   dplyr::filter(genero == "female") %>% 
   dplyr::mutate(altura_polegadas = altura / 2.54) %>% 
   dplyr::group_by(idade_dec, genero) %>% 
   dplyr::summarize(altura_polegadas = mean(altura_polegadas))%>% 
   DT::datatable())
  



# Função tabyl e adorn_ são do pacote janitor -----------------------------

# n() e percent()

dados %>% 
  janitor::tabyl(idade_dec) %>% 
  DT::datatable()

# Adicionando variáveis 

dados %>% 
  janitor::tabyl(idade_dec, genero) %>% 
  DT::datatable()

# Adicionando totais nas linhas e nas colunas

dados %>% 
  janitor::tabyl(idade_dec, genero) %>%
  janitor::adorn_totals(where = c("row", "col")) %>% 
  DT::datatable()

# Adicionando totais na linha

dados %>% 
  janitor::tabyl(idade_dec, genero) %>%
  janitor::adorn_totals (c("row")) %>%
  DT::datatable()

# Adicionando e Formatando a porcentagem

dados %>% 
  janitor::tabyl(idade_dec, genero) %>%
  janitor::adorn_totals(c("row", "col")) %>%
  janitor::adorn_percentages() %>% 
  janitor::adorn_pct_formatting(digits = 0) %>%
  DT::datatable()

# Acrescentando n (totais)

dados %>% 
  janitor::tabyl(idade_dec, genero) %>%
  janitor::adorn_totals(where = c("row", "col")) %>%
  janitor::adorn_percentages() %>% 
  janitor::adorn_pct_formatting(digits = 0) %>% 
  janitor::adorn_ns() %>% 
  DT::datatable()

# Exportando dados -------------------------------------------------------

readr::write_delim(dados, file = "dados.csv")

