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

library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)

# Importando dados --------------------------------------------------------

nhanes <- read.csv("nhanes.csv")

# Limpeza dos dados -------------------------------------------------------

dados <- nhanes %>% 
  rename(genero= Gender, idade= Age, idade_dec=AgeDecade, raca= Race1,
         educacao= Education, estado_civil= MaritalStatus, renda= HHIncome,
         casa= HomeOwn,trabalho= Work, peso= Weight, altura= Height,
         imc= BMI, saude_genero= HealthGen, horas_sono= SleepHrsNight, 
         atividade_fisica= PhysActive, atividade_diaria= PhysActiveDays, 
         fumante=SmokeNow, ano= SurveyYr) 

# Visualização dos dados -------------------------------------------------------

dados %>%
  datatable() 

# Inspeção dos dados ------------------------------------------------------

View(dados)
head(dados,n = 10)
glimpse(dados)

# Removendo colunas ------------------------------------------------------------

dados <- dados %>% 
  select (-DaysPhysHlthBad, -DaysMentHlthBad) 

# Removendo NAs -----------------------------------------------------------
# A função drop_na() elimina todas as linhas que contém missings no dataset.

dados <- dados %>% 
  drop_na() 

# Pegando o gancho  -------------------------------------------------------
# Função replace_na: vocês podem substituir NAs por valores especificos

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

# tidyr::drop_na(data, nome da coluna)
# todas as linhas que contém missing nessa coluna são eliminados

###############################################################################
# Função SELECT () -----------------------------------------------------------

dados %>% 
  select(genero, idade) %>% 
  datatable()

# Função select e contains ------------------------------------------------

dados %>% 
  select(contains("idade")) %>% 
  datatable()


# Função select (starts_with e ends_with) ---------------------------------

dados %>% 
  select(starts_with("R")) %>% 
  datatable()

dados %>% 
  select(ends_with("E")) %>% 
  datatable()

# Função Select (var1:var5) ------------------------------------------------------

dados %>% 
  select(genero:trabalho) %>% 
  datatable()

# Função Select  (-var) ----------------------------------------------------------

dados %>% 
  select(-ID) %>% 
  datatable()

# Função Select conjunto (-(var1:var2)) -------------------------------------------

dados %>% 
  select(-(ID:educacao)) %>% 
  datatable()

###############################################################################
# Função MUTATE () -----------------------------------------------------------

# 1. Criar uma nova variável com um valor específico

dados %>% 
  mutate(nova_var= "Americanos", .before = genero) %>% 
  datatable()
  
dados %>% 
  mutate(nova_var= "Americanos", .after= genero) %>% 
  datatable()

# 2. Criar uma nova variável com base em outras variáveis

dados %>% 
  mutate(altura_polegadas= altura/2.54, .after = altura) %>%
  datatable()
  
# 3. Alterar uma variável existente

dados %>% 
  mutate(imc = round(imc, digits = 0)) %>% 
  select(imc) %>% 
  datatable()

# Função FILTER () -------------------------------------------------------

# 1. Usamos == para selecionar todas as observações que atendem aos critérios

dados %>% 
  filter(genero == "female") %>% 
  select(ID, genero) %>% 
  datatable()

# 2. Usamos != para selecionar todas as observações que não atendem aos critérios
  
dados %>% 
  filter(saude_genero!= "Good") %>% 
  select(saude_genero) %>% 
  datatable()
  
# 3. Podemos combinar comparações e operadores lógicos

dados %>% 
  filter(saude_genero == "Good" | saude_genero == "Vgood" |
         saude_genero == "Excellent") %>%
  select(saude_genero) %>% 
  datatable()

# 4. Podemos encadear múltiplas funções.

dados %>%
  filter(genero == "male" & (saude_genero == " Good" |
  saude_genero == "Vgood" | saude_genero == "Excellent")) %>%
  select(genero,saude_genero) %>% 
  datatable()
  

# Função SUMMARIZE () --------------------------------------------------------

# Qual a média de altura por gênero

dados %>% 
  group_by(genero) %>% 
  summarize(mean_altura = mean(altura)) %>% 
  datatable()

# Vários argumentos

dados %>% 
summarize (mean_peso = mean(peso),
           mean_idade = mean(idade),
           mean_horas_sono= mean(horas_sono)) %>% 
  datatable()
      
# Função GROUP-BY () ---------------------------------------------------------

dados %>% 
  group_by(ano, genero) %>% 
    summarise(mean_atividade_diária= mean(atividade_diaria)) %>% 
  datatable()
           
(atividade <- dados %>%
  group_by(idade, genero) %>% 
  summarise(mean_atividade_diária= mean(atividade_diaria))%>% 
  arrange(desc(mean_atividade_diária)) %>% 
  datatable())

# Função COUNT () -----------------------------------------------------------

dados %>% 
  count(idade_dec) %>% 
  datatable()
  
dados %>% 
  count(idade_dec, genero, renda) %>% 
  datatable()

# Função ARRANGE () ----------------------------------------------------------

dados %>% 
  arrange(idade) %>% 
  datatable()

dados %>% 
  arrange(desc(idade)) %>% 
  datatable()

# Criando novas tabelas ---------------------------------------------------

(altura_feminina <- dados %>% 
  filter(genero == "female") %>% 
  mutate(altura_polegadas = altura / 2.54) %>% 
  group_by(idade_dec, genero) %>% 
  summarize(altura_polegadas = mean(altura_polegadas))%>% 
  datatable())
  
# Tabelas cruzadas (crosstabs)  -------------------------------------------------------

# Função tabyl e adorn_ são do pacote janitor

dados %>% 
  tabyl(idade_dec) %>% 
  datatable()

# Adicionando variáveis 

dados %>% 
  tabyl(idade_dec, genero) %>% 
  datatable()

# Adicionando totais nas linhas e nas colunas

dados %>% 
  tabyl(idade_dec, genero) %>%
  adorn_totals(where = c("row", "col")) %>% 
  datatable()

# Adicionando porcentagem

dados %>% 
  tabyl(idade_dec, genero) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages() %>% 
  datatable()

# Formatando a porcentagem

dados %>% 
  tabyl(idade_dec, genero) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0,
                     rounding = "half up") %>% 
  datatable()

# Acrescentando n (totais)

dados %>% 
  tabyl(idade_dec, genero) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0,
                       rounding = "half up") %>% 
  adorn_ns() %>% 
  datatable()

# Exportando dados -------------------------------------------------------

write_delim(dados, file = "dados.csv")

