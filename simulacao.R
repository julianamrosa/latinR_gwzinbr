#Pacotes
library(dplyr)
library(leaflet)
library(gwzinbr)

#Importar dados dos municípios
municipios <- read.csv("C:/Users/Juliana Rosa/OneDrive/Documents/LatinR/Simulações/municipios.csv")

#Adicionar info de região
municipios <- municipios%>%
  mutate(regiao=case_when(codigo_uf %in% 10:19 ~ "Norte",
                          codigo_uf %in% 20:29 ~ "Nordeste",
                          codigo_uf %in% 30:39 ~ "Sudeste",
                          codigo_uf %in% 40:49 ~ "Sul",
                          codigo_uf %in% 50:59 ~ "Centro-Oeste"))
table(municipios$codigo_uf, municipios$regiao)

#Calcular qtd de município por região
table(municipios$regiao)

#Gerar dados aleatórios para as 4 distribuições

##Norte - Poisson
set.seed(374563874)
norte <- rpois(450, 4)
norte_zeroinf <- rep(0, 450)
hist(norte)


## Nordeste - BN
set.seed(56907956)
nordeste <- rnbinom(1794, mu=4, size=0.25)
nordeste_zeroinf <- rep(0, 1794)
hist(nordeste)

## Centro-Oeste - ZIP
set.seed(95867)
centro_oeste <- rpois(467, 4)
co_zeroinf <- rbinom(467, 1, 0.33) #parâmetro p da piz é 0.33
centro_oeste[co_zeroinf==1] <- 0 #adicionando zeros inflacionados
hist(centro_oeste)

## Sul/Sudeste - ZINB
n <- 1668+1191
set.seed(48678233)
sul_sudeste <- rnbinom(n, mu=4, size=0.25)
su_zeroinf <- rbinom(n, 1, 0.33) #parâmetro p da piz é 0.33
sul_sudeste[su_zeroinf==1] <- 0 #adicionando zeros inflacionados
hist(sul_sudeste)


#Juntar dados aleatórios com dados dos municípios
municipios <- municipios%>%
  arrange(codigo_uf)

municipios$y <- c(norte, nordeste, sul_sudeste, centro_oeste)
municipios$zero_inf <- c(norte_zeroinf, nordeste_zeroinf, su_zeroinf,
                         co_zeroinf)

hist(municipios$y)

#Criar mapa

##Escala de cores
municipios <- municipios %>% mutate(cor=case_when(
  y == 0.0 ~ "#F0F0F0",
  y > 0.0 & y <= 13.0 ~ "#B0D5E6",     
  y > 13.0 & y <= 26.0 ~ "#62ABCC",     
  y > 26.0 & y <= 39.0 ~ "#1381B2",
  y > 39.0 ~ "#006C9E"     
))

##Popup
mun_popup <- paste0("<strong>Local: </strong>", 
                    municipios$nome, 
                    "<br><strong>Valor: </strong>", 
                    municipios$y)

##Mapa
mapa_resposta <- municipios %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
  addCircleMarkers(
    color = ~cor,
    fillColor = ~cor,
    fillOpacity = 0.7,
    weight = 1,
    radius = 3,
    stroke = TRUE,
    popup = mun_popup,
    lat = ~latitude,
    lng = ~longitude
  ) %>%
  addLegend(
    labels = c("0.0", "0.0 -| 13.0", "13.0 -| 26.0",
               "26.0 -| 39.0", "> 39.0"),
    colors = c("#F0F0F0", "#B0D5E6", "#62ABCC",
               "#1381B2", "#006C9E"),
    title = "Valor"
  )

mapa_resposta

#Simular covariáveis

##x
set.seed(659202)
a = runif(1)
b = round(runif(1, 0, 20))
dp = round(runif(1, 0, 10))
x=(municipios$y-b)/a
x=x+rnorm(nrow(municipios), 0, dp)
municipios$x <- x
plot(municipios$x, municipios$y)

##xvarinf
set.seed(8564534)
dp2 = round(runif(1, 0, 20))
x_varinf=ifelse(municipios$zero_inf==1, rnorm(1, 100, 20), rnorm(1, 200, 20))
x_varinf=x_varinf+rnorm(nrow(municipios), 0, dp2)
municipios$x_varinf <- x_varinf
plot(municipios$x_varinf, municipios$zero_inf)

#Usar função gwzinbr

startTime <- Sys.time()
Golden(
  data=municipios,
  formula=y~x,
  xvarinf = "x_varinf",
  lat="latitude",
  long="longitude",
  method="adaptive_bsq",
  model = "zinb",
  bandwidth = "aic",
  distancekm = TRUE
) #cv, fixed_g
endTime <- Sys.time()
endTime-startTime

startTime <- Sys.time()
mod <- gwzinbr(data = municipios, 
               formula = y~x,
               xvarinf = "x_varinf",
               lat = "latitude", long = "longitude", method = "adaptive_bsq",
               model = "zinb", distancekm = TRUE, h=) #force
endTime <- Sys.time()
endTime-startTime

#adicionar código que exporta os resultados do modelo

#Mapas resultados gwzinbr
##3 mapas -> beta, gamma e alpha

#usar mapas mais adequados

