#######################Análise da inflação brasileira######################

######################Buscar os dados no SIDRA e ativar o pacote sidrar para coletar os dados de forma fácil####

install.packages ("sidrar")
library(sidrar)
library(tidyverse)

######Coleta

ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

##Limpeza

dplyr::glimpse(ipca_raw)
ipca <- ipca_raw |> 
  dplyr::select("data" = "Mês (Código)",
                "ipca" = "Valor") |> 
  dplyr::mutate(data = lubridate::ym(data)) |>
  dplyr::filter(data >= "2004-01-01") |>
  dplyr::as_tibble()

####### Análise exploratória

ggplot2::ggplot(ipca)+
  ggplot2::aes(x= data, y= ipca)+
  ggplot2::geom_line()

summary(ipca)

ggplot2::ggplot(ipca)+
 ggplot2::aes(y= ipca)+
  ggplot2::geom_boxplot()
  
ggplot2::ggplot(ipca)+
  ggplot2::aes(x= ipca)+
  ggplot2::geom_histogram()

##### Taxa de desocupação (desemprego) e coleta de dados

desocupacao_raw <- sidrar:: get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")
desocupacao <- desocupacao_raw |>
  dplyr::select("data" = "Trimestre Móvel (Código)", "desocupacao" = "Valor") |>
                  dplyr::mutate(data = lubridate::ym(data)) |>
                  dplyr::as_tibble()
                
####Unir os dois dados

df_dados <- ipca |> 
  dplyr::inner_join(desocupacao, by = "data")

##Construção de gráficos lineares
df_dados |>
ggplot2::ggplot()+
  ggplot2::aes(x = data)+
  ggplot2::geom_line(aes(y = desocupacao, color = "Taxa de desocupacao"))+
  ggplot2::geom_line(aes(y = ipca, color = "IPCA"))+
  ggplot2::scale_color_manual(values = c("#282f6b", "#b22200"))

##Curva de philips
df_dados |>
  modelo_philips <- lm(ipca ~ desocupacao, data = df_dados)
summary(modelo_philips)

###Gráfico de dispersão
dispersao <- ggplot2::ggplot(df_dados, ggplot2::aes(x = desocupacao, y = ipca)) +
  ggplot2::geom_point()+
  geom_smooth(method = 'lm')
  