#Pacotes
library(dplyr)
library(leaflet)
library(gwzinbr)
library(ggplot2)

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

#Filtrando para região Centro-Oeste
dados_co <- municipios%>%
  filter(regiao=="Centro-Oeste")
table(dados_co$codigo_uf)

#Gerar dados aleatórios para as 3 distribuições


## Mato Grosso do Sul (50) - BN
set.seed(56907956)
mg_sul <- rnbinom(79, mu=4, size=0.25)
mg_sul_zeroinf <- rep(0, 79)
hist(mg_sul)

## Mato Grosso (51) - ZIP
set.seed(467448)
mg <- rpois(141, 4)
mg_zeroinf <- rbinom(141, 1, 0.43) #parâmetro p da piz é 0.43
mg[mg_zeroinf==1] <- 0 #adicionando zeros inflacionados
hist(mg)

## Goiás/DF (52 e 53) - ZINB
set.seed(578457834)
goias_df <- rnbinom(247, mu=4, size=0.25)
goias_df_zeroinf <- rbinom(247, 1, 0.33) #parâmetro p da piz é 0.33
goias_df[goias_df_zeroinf==1] <- 0 #adicionando zeros inflacionados
hist(goias_df)

#Juntar dados aleatórios com dados do centro-oeste
dados_co <- dados_co%>%
  arrange(codigo_uf)
dados_co$Y <- c(mg_sul, mg, goias_df)
dados_co$zero_inf <- c(mg_sul_zeroinf, mg_zeroinf, goias_df_zeroinf)
hist(dados_co$Y)

#Histogramas dos 3 estados
dados_co <- dados_co%>%
  mutate(estado=case_when(codigo_uf==50 ~ "Mato Grosso do Sul",
                          codigo_uf==51 ~ "Mato Grosso",
                          codigo_uf %in% 52:53 ~ "Goiás/ DF"))
ggplot(dados_co)+
  geom_histogram(aes(Y), fill="darkgray")+
  facet_grid(~estado)+
  theme_light()+
  labs(title="Distribuição da Resposta por Estado", x="Variável Resposta", y="Frequência")

#Criar mapa

##Escala de cores
dados_co <- dados_co %>% mutate(cor=case_when(
  Y == 0.0 ~ "#F0F0F0",
  Y > 0.0 & Y <= 13.0 ~ "#B0D5E6",     
  Y > 13.0 & Y <= 26.0 ~ "#62ABCC",     
  Y > 26.0 & Y <= 39.0 ~ "#1381B2",
  Y > 39.0 ~ "#006C9E"     
))

##Popup
co_popup <- paste0("<strong>Local: </strong>", 
                   dados_co$nome, 
                   "<br><strong>Valor: </strong>", 
                   dados_co$Y)

##Mapa
mapa_resposta <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addCircleMarkers(
    color = ~cor,
    fillColor = ~cor,
    fillOpacity = 0.7,
    weight = 1,
    radius = 5,
    stroke = TRUE,
    popup = co_popup,
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
X=(dados_co$Y-b)/a
X=X+rnorm(nrow(dados_co), 0, dp)
dados_co$X <- X
plot(dados_co$X, dados_co$Y)
cor(dados_co$X, dados_co$Y)

##xvarinf
set.seed(32527549)
dp2 = runif(1, 0, 1)
x_varinf=dados_co$zero_inf+rnorm(nrow(dados_co), 0, dp2)
dados_co$x_varinf <- x_varinf
plot(dados_co$x_varinf, dados_co$zero_inf)
cor(dados_co$x_varinf, dados_co$zero_inf)

#Golden

# startTime <- Sys.time()
# Golden(
#   data=dados_co,
#   formula=Y~X,
#   xvarinf = "x_varinf",
#   lat="latitude",
#   long="longitude",
#   method="adaptive_bsq",
#   model = "zinb",
#   bandwidth = "aic",
#   distancekm = TRUE
# )
# endTime <- Sys.time()
# endTime-startTime
#26 mins
#bandwidth=314

#gwzinbr

startTime <- Sys.time()
mod <- gwzinbr(data = dados_co, 
               formula = Y~X,
               xvarinf = "x_varinf",
               lat = "latitude",
               long = "longitude",
               method = "adaptive_bsq",
               model = "zinb",
               distancekm = TRUE,
               h=314)
endTime <- Sys.time()


endTime-startTime
#5 mins

#Estimativas dos parâmetros

##beta
dados_co$X_est <- round(as.numeric((mod$parameter_estimates)[,"X"]), 3)
ggplot(dados_co)+
  geom_boxplot(aes(X_est))+
  facet_grid(~codigo_uf)+
  coord_flip()
dados_co%>%
  group_by(codigo_uf)%>%
  summarise(q1=quantile(X_est, 0.25), q2=quantile(X_est, 0.50), q3=quantile(X_est, 0.75))
#<=0.031; 0.031 - 0.035; >=0.035

##alpha
dados_co$alpha <- round(as.numeric((mod$alpha_estimates)[,"alpha"]), 3)
ggplot(dados_co)+
  geom_boxplot(aes(alpha))+
  facet_grid(~codigo_uf)+
  coord_flip()
dados_co%>%
  group_by(codigo_uf)%>%
  summarise(q1=quantile(alpha, 0.25), q2=quantile(alpha, 0.50), q3=quantile(alpha, 0.75))
#<=0.115; 0.115 - 0.624; >=0.624

##lambda
dados_co$lambda <- round(as.numeric((mod$parameter_estimates)[,"Inf_x_varinf"]), 3)
ggplot(dados_co)+
  geom_boxplot(aes(lambda))+
  facet_grid(~codigo_uf)+
  coord_flip()
dados_co%>%
  group_by(codigo_uf)%>%
  summarise(q1=quantile(lambda, 0.25), q2=quantile(lambda, 0.50), q3=quantile(lambda, 0.75))
#<=2.56; 2.56 - 6.49; >=6.49

#Mapas resultados gwzinbr

##mapa betas

###Escala de cores
dados_co <- dados_co %>% mutate(beta_cor=case_when(
  X_est <= 0.031 ~ "#B0D5E6",
  X_est > 0.031 & X_est < 0.035 ~ "#62ABCC",     
  X_est >= 0.035 ~ "#1381B2",
))


###Popup
beta_popup <- paste0("<strong>Local: </strong>", 
                     dados_co$nome, 
                     "<br><strong>Beta: </strong>", 
                     dados_co$X_est)

###Mapa
mapa_beta <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addCircleMarkers(
    color = ~beta_cor,
    fillColor = ~beta_cor,
    fillOpacity = 0.7,
    weight = 1,
    radius = 5,
    stroke = TRUE,
    popup = beta_popup,
    lat = ~latitude,
    lng = ~longitude
  ) %>%
  addLegend(
    labels = c("< 0.031", "0.031 - 0.035", "> 0.035"),
    colors = c("#B0D5E6", "#62ABCC", "#1381B2"),
    title = "Estimativa do Parâmetro Beta"
  )
mapa_beta

##mapa alphas

###Escala de cores
dados_co <- dados_co %>% mutate(alpha_cor=case_when(
  alpha <= 0.115 ~ "#B0D5E6",
  alpha > 0.115 & alpha < 0.624 ~ "#62ABCC",     
  alpha >= 0.624 ~ "#1381B2",     
))

###Popup
alpha_popup <- paste0("<strong>Local: </strong>", 
                      dados_co$nome, 
                      "<br><strong>Alpha: </strong>", 
                      dados_co$alpha)

###Mapa
mapa_alpha <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addCircleMarkers(
    color = ~alpha_cor,
    fillColor = ~alpha_cor,
    fillOpacity = 0.7,
    weight = 1,
    radius = 5,
    stroke = TRUE,
    popup = alpha_popup,
    lat = ~latitude,
    lng = ~longitude
  ) %>%
  addLegend(
    labels = c("< 0.115", "0.115 - 0.624", "> 0.624"),
    colors = c("#B0D5E6", "#62ABCC", "#1381B2"),
    title = "Superdispersão"
  )
mapa_alpha

##mapa lambdas

###Escala de cores
dados_co <- dados_co %>% mutate(lambda_cor=case_when(
  lambda <= 2.56 ~ "#B0D5E6",     
  lambda > 2.56 & lambda < 6.49 ~ "#62ABCC",     
  lambda >= 6.49 ~ "#1381B2",
))

###Popup
lambda_popup <- paste0("<strong>Local: </strong>", 
                       dados_co$nome, 
                       "<br><strong>Lambda: </strong>", 
                       dados_co$lambda)

###Mapa
mapa_lambda <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addCircleMarkers(
    color = ~lambda_cor,
    fillColor = ~lambda_cor,
    fillOpacity = 0.7,
    weight = 1,
    radius = 5,
    stroke = TRUE,
    popup = lambda_popup,
    lat = ~latitude,
    lng = ~longitude
  ) %>%
  addLegend(
    labels = c("< 2.56",
               "2.56 |- 6.49",
               "> 6.49"),
    colors = c("#B0D5E6", "#62ABCC", "#1381B2"),
    title = "Inflação de Zeros"
  )
mapa_lambda
