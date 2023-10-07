#------------------------------------------------------------------------------#
#                                                                              #
#                         Introdução ao R e Aplicações                         #
#                                 R-Ladies Goiânia                             #
#                                                                              #
#                           A mágica do Pacote {ggplot2}                       #
#                                                                              #
#                              Jennifer Luz Lopes                              #
#                                                                              #
#------------------------------------------------------------------------------#

# Instalação de pacotes ---------------------------------------------------

# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("readr")
# install.packages("patchwork")
# install.packages("ggtext")
# install.packages("ggridges")
# install.packages("cowplot")
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("forcast")
# install.packages("ggthemes")
# install.packages("patchwork")
# install.packages("ggforce")
# install.packages("plotly")
# install.packages("GGally")
# install.packages("gganimate")
# install.packages("pcman")

# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(janitor)
library(readr)
library(ggtext)
library(ggridges)
library(DT)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggthemes)
library(patchwork)
library(ggforce)
library(plotly)
library(GGally)
library(gganimate)
library(pcman)
library(lemon)


# Use o pacote pcman ------------------------------------------------------

pacman::p_load(lemon, gganimate, GGally,plotly, ggforce,patchwork,ggthemes,
               forcats,viridis,hrbrthemes, DT, ggridges, ggtext, 
               readr, janitor, tidyverse)


# Argumentos --------------------------------------------------------------

# color= altera a cor de formas que não tem área (pontos e retas)
# fill=  altera a cor de formas com área (barras, caixas, densidades, áreas)
# size=  altera o tamanho de formas
# shape=  altera o tipo da forma, geralmente usada para pontos
# alpha= altera a transparência da forma


# Importando dados --------------------------------------------------------

dados <- read.delim("dados.csv", header = TRUE, sep = " ")

# Gráfico de pontos ----------------------------------------------------

ggplot() ?

ggplot(data = dados,
       mapping = aes(x = peso,
                     y = altura)) +
  geom_point() 

# Gráfico de pontos (Função geom_jitter) ----------------------------------
# fill= utilizada para 

ggplot(data = dados,
       mapping = aes(x = peso,
                     y = altura,
                     fill= genero)) +
  geom_jitter(size = 3, 
              alpha = 0.5, 
              shape = 21)

# Grafico de pontos (Coloração manual) ------------------------------------

ggplot(data = dados,
       mapping = aes(x = peso,
                     y = altura,
                     color = genero)) +
  geom_point()+
  scale_color_manual(values = c("#B048A2", "#FDE962"))

# Vocês agora!
# aes= idade,imc
# color= raca
# geom_point()

# Resposta ----------------------------------------------------------------

ggplot(data = dados, 
       mapping = aes(x = idade,
                     y= imc,
                     color= raca)) +
  geom_point()

# Gráfico de barras -------------------------------------------------------

(altura_feminina <- dados %>% 
   dplyr::filter(genero == "female") %>% 
   dplyr::mutate(altura_polegadas = altura / 2.54) %>% 
   dplyr::group_by(idade_dec, genero) %>% 
   dplyr::summarize(altura_polegadas = mean(altura_polegadas)))

ggplot(data = altura_feminina, 
       aes(x = idade_dec,
           y = altura_polegadas)) +
  geom_bar(stat = "identity") 

# Gráfico de barras (argumento = fill) ------------------------------------

# Argumento: (stat = "identity") deixa os dados como estão
# Já temos os valores de y calculados e vamos utilizá-lo diretamente

ggplot(data = altura_feminina, 
       aes(x = idade_dec,
           y = altura_polegadas,
           fill= idade_dec)) +
  geom_bar(stat = "identity")
  
# Gráfico de barras horizontal --------------------------------------------

ggplot(data = altura_feminina, 
       aes(x = idade_dec,
           y = altura_polegadas)) +
  geom_bar(stat = "identity") +  
  coord_flip()

# Histograma --------------------------------------------------------------
# Histograma simples ------------------------------------------------------

(plot0 <- ggplot(dados, aes(x= altura)) +
  geom_histogram())

# Argumentos: binwidth | fill | color | alpha | labs | tag | cap ------------

(plot1 <- ggplot(dados, aes(x= altura)) +
  geom_histogram(binwidth=5,
                 fill="#073b4c", 
                 color="#e9ecef", 
                 alpha=0.8) +
  labs(title = "Distribuição da altura",
       x= "Altura",
       y= " ",
       tag = "Plot1", 
       caption = "Jennifer Lopes") +
  theme_minimal())

plot0 + plot1

#Alteração de eixos
# scale_y_continuous(limits=c(0,145), breaks= seq(0,145, by = 50))

# Gráfico de densidade ----------------------------------------------------

(plot2 <- ggplot(dados, aes(x = altura)) +
  geom_histogram(aes(y = ..density..), 
                 fill="#073b4c", 
                 alpha=0.9) +
  geom_density() +

  stat_function(fun = dnorm,
                color = "red",
                size = 1,
                args = list(mean = mean(dados$altura),
                            sd = sd(dados$altura))) +
  
  labs(title = "Gráfico de densidade",
       x= "Altura",
       y= "Densidade",
       tag = "Plot2", 
       caption = "Jennifer Lopes") +
  theme_minimal())

# Unindo gráficos 
# library(patchwork)

(patchwork <- (plot0 + plot1/ plot2) +
    
  plot_annotation(title = "Distribuição da altura", 
                  subtitle = "Unindo gráficos com pacote {patchword}",
                  tag_levels = c("A","1")) +
    
  theme(plot.tag = element_text(size = 8, 
                                hjust = 0, 
                                vjust = 0)))


# Box Plot ----------------------------------------------------------------
# library(ggforce)
# Box plot 1--------------------------------------------------------------------

(box1 <- dados %>%
  ggplot(aes(x= raca, 
              y=altura, 
              fill=raca)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
 labs(x= "Raça", 
      y=" Altura"))

# Box plot 2--------------------------------------------------------------------
(box2 <- dados %>%
ggplot(aes(x=raca, y=altura, fill=raca)) +
  geom_boxplot()+
  
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  
  labs(title = "Altura de diferentes raças",
       x= "Raça",
       y= "Altura",
       caption = "Jennifer Lopes") +
  theme_minimal())

# Box plot 3--------------------------------------------------------------------  

(box3 <- 
dados %>%
  ggplot(aes(x=raca, y=altura, fill=raca)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    labs(x= "Raça", 
         y=" Altura") +
  coord_flip())

# Unindo box-plots --------------------------------------------------------

box1 + box2 / box3

# Gráficos interativos com pacote plotly ----------------------------------
# library(plotly)


# Gráfico interativo (p1) -------------------------------------------------
(p1 <- dados %>%
  ggplot(aes(x= saude_genero, 
              y= idade, 
              fill= genero)) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  labs(x="Estado de saúde",
       y= "Idade"))

# geom_jitter(color="black", size=0.4, alpha=0.9) +

ggplotly(p1)

# Gráfico interativo (p2) -------------------------------------------------
#library(lemon)
 
 (p2 <- ggplot(dados, aes(x= saude_genero,
                           y= idade, 
                           color =genero)) + 
      geom_point(position=position_jitter(width=0.1)) + 
     scale_color_viridis(discrete = TRUE, alpha=0.6) +
      coord_flex_cart(bottom= brackets_horisontal(),
                      left= capped_vertical('both')) +
      labs(x="Estado de saúde",
           y= "Idade", 
           title = "Estado de saude conforme a idade",
           subtitle = "Estudo realizado em homens e mulheres",
           caption = " Jennifer Lopes")+
      theme_classic() + 
      theme(panel.border=element_blank(), 
            axis.line = element_line()))

# Gráficos interativos

ggplotly(p1)
ggplotly(p2)

# Unindo Gráficos

p2+p1

# Facetas -----------------------------------------------------------------
# library(ggthemes)

# Função facet_wrap -------------------------------------------------------

# position = "dodge": requer que a variável de agrupamento seja especificada no global ou geom_*na camada


(fc1 <- ggplot(data = dados, 
       mapping = aes(x = idade_dec,
                    y = altura,
                    fill= genero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 200),
                     breaks = c(0, 50, 100, 150,195)) +
  labs(title = "Altura de homens e mulheres por faixa etária",
       x= "Idade",
       y= "Altura", 
       tag = "A") +
  theme_ipsum_tw() +
  facet_wrap(~genero))

# Função facet_grid -------------------------------------------------------

(fc2 <- ggplot(data = dados,
       mapping = aes(x = idade,
                     y = peso,
                     color = genero)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE, alpha=0.9) +
  facet_grid(rows = vars(saude_genero),
             cols = vars(genero)) +
  
  labs(title = "Estado de saúde de homens e mulheres por faixa etária e peso",
       x= "Idade",
       y= "peso", 
       tag = "B")+
  theme_light()+
   theme(element_blank()))


# Resumos estatísticos ----------------------------------------------------
# Função stat_summary -----------------------------------------------------

(re1 <- dados %>%
   ggplot( aes(x=raca, 
               y=altura, 
               fill=raca)) +
   geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "black",
    size = 3)) 

# Função Summarise  -------------------------------------------------------

(re <- dados %>%
    group_by(genero, educacao) %>% 
    summarise(total= sum(idade),
              avg= mean(idade),
              number= n(),
              sd= sd(idade))) 
datatable(re)

# total
(re2 <- ggplot(re, aes(x= educacao, y= total)) +
  geom_col())


# avg  
(re3 <- ggplot(re, aes(x= educacao, y= avg)) +
    geom_col())

# total + fill
(re4 <- ggplot(re, aes(x= educacao, y= total, fill= genero)) +
    geom_col())

# position = position_dodge()
(re5 <- ggplot(re, aes(x= educacao, y= total, fill= genero)) +
    geom_col(position = position_dodge()))

re1+ re2+ re3/ re4 + re5

# Gráfico interativo
ggplotly(re3)

# Gráficos interativos (pacote gganimate) ---------------------------------------------------

# Função= transition_states()
# Argumento: transition_length()- A duração relativa da transição. Será reciclado para corresponder ao número de fumantes nos dados
# Argumento: state_length()- A duração relativa da pausa dos fumantes. Será reciclado para corresponder ao número de fumantes nos dados
# Argumento: enter_fade()- Entrada de dados 
# Argumento: exit_shrink()- Saída de dados 

(ani <- dados %>%
    ggplot( aes(x=raca, y=altura, fill= raca)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha= 0.6) +
    
    geom_jitter(color="black", size=0.4, alpha= 0.9) +
    labs(title = "Altura de diferentes raças",
         x= "Raça",
         y= "Altura",
         caption = "Jennifer Lopes") +
    theme_minimal()+
    
    transition_states(
      fumante,
      transition_length = 2,
      state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('exponential-in-out'))

anim_save("ani.gif",ani)

# Salvando gráficos -------------------------------------------------------

(re3 <- ggplot(re2, aes(x= educacao, y= avg)) +
   geom_col())

ggsave(filename = "plotly.png",
       height = 8,
       width = 11,
       units = "in",
       dpi = 300)



