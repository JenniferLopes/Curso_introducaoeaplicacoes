

```{r}
library(ggplot2)
data("mtcars")
ggplot(mtcars) + 
  aes(mpg, disp) + 
  geom_point() +   #<<
  geom_smooth()    #<<
```

---
  ```{r highlight.output=c(1, 3)}
head(mtcars)
```
---
  
  
  
  
  ```{r message=FALSE, warning=FALSE, , out.width="100%", paged.print=FALSE, echo=FALSE, fig.align = 'center'}
knitr::include_graphics("imagem_5.jpg")
```










```{r}
library(gapminder)
library(tidyverse)
```


```{r}
gapminder %>% 
  glimpse() %>% 
  head(10) %>% # primeiros
  tail(10)     #ultimos
```


```{r}
(gapminder_port <- gapminder %>% 
    rename(pais= country,
           ano=year,
           exp_vida= lifeExp,
           renda_media=gdpPercap))
```



```{r}
(gapminder %>% 
    rename(pais= country,
           ano=year,
           exp_vida= lifeExp,
           renda_media=gdpPercap) -> gapminder_teste)
```


```{r}
gapminder_port %>% 
  sample_n(10) # amostra aleatório
```


```{r}
gapminder_port %>% 
  ggplot()+
  aes(x=log10(renda_media), y=exp_vida)+
  geom_point()+
  geom_smooth(method = "lm")
```

```{r}
gapminder_port %>% 
  select(pais, renda_media)
```

```{r}
gapminder_port %>% 
  select(-pop)
```

```{r}
gapminder_port %>% 
  select(renda_media, ano, everything()) # vem primneiro
```

```{r}
gapminder_port %>% 
  select(-pop) %>% 
  filter(continent=="Americas") %>% 
  ggplot()+
  aes(x=ano, y=renda_media)+
  geom_line()+
  facet_wrap(~pais)
```



regressão <- lm(mpg~ cyl, data = mtcars)
summary(regressão)
qqplot(regressão)


rgba(136, 57, 138, 0.3)



