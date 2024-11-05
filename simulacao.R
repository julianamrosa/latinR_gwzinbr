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

#Filtrando para região Centro-Oeste
dados_co <- municipios%>%
  filter(regiao=="Centro-Oeste")
table(dados_co$codigo_uf)

#Gerar dados aleatórios para as 4 distribuições

## Mato Grosso do Sul - Poisson
# set.seed(374563874)
# mg_sul <- rpois(79, 4)
# mg_sul_zeroinf <- rep(0, 79)
# hist(mg_sul)


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
# set.seed(8564534)
# dp2 = round(runif(1, 0, 20))
# x_varinf=ifelse(dados_co$zero_inf==1, rnorm(1, 100, 20), rnorm(1, 200, 20))
# #x_varinf=ifelse(dados_co$codigo_uf==51, runif(1, 100, 200), x_varinf)
# x_varinf=x_varinf+rnorm(nrow(dados_co), 0, dp2)
# dados_co$x_varinf <- x_varinf
# plot(dados_co$x_varinf, dados_co$zero_inf)
# cor(dados_co$x_varinf, dados_co$zero_inf)

set.seed(8564534)
dp2 = round(runif(1, 0, 1))
x_varinf=dados_co$zero_inf+rnorm(nrow(dados_co), 0, dp2)
dados_co$x_varinf <- x_varinf
plot(dados_co$x_varinf, dados_co$zero_inf)
cor(dados_co$x_varinf, dados_co$zero_inf)

#Usar função gwzinbr

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
# ) #cv, fixed_g
# endTime <- Sys.time()
# endTime-startTime
#26 mins
#bandwidth=440

startTime <- Sys.time()
mod <- gwzinbr(data = dados_co, 
               formula = Y~X,
               xvarinf = "x_varinf",
               lat = "latitude", long = "longitude", method = "adaptive_bsq",
               model = "zinb", distancekm = TRUE, h=440) #force
endTime <- Sys.time()
endTime-startTime
#5 mins

#Resultados do modelo
# mod$bandwidth
# mod$measures
# mod$qntls_gwr_param_estimates
# mod$descript_stats_gwr_param_estimates
# mod$t_test_gwr_param_estimates
# mod$qntls_gwr_se
# mod$descript_stats_gwr_se
# mod$qntls_gwr_zero_infl_param_estimates
# mod$descript_stats_gwr_zero_infl_param_estimates
# mod$t_test_gwr_zero_infl_param_estimates
# mod$qntls_gwr_zero_infl_se
# mod$descript_stats_gwr_zero_infl_se
# mod$global_param_estimates
# mod$analysis_max_like_zero_infl_param_estimates
# mod$analysis_max_like_gof_measures
# mod$variance_covariance_matrix
# mod$residuals
# mod$alpha_estimates
# mod$parameter_estimates

#Mapas resultados gwzinbr
##3 mapas -> beta, gamma e alpha

## mapa betas

dados_co$X_est <- round(as.numeric((mod$parameter_estimates)[,"X"]), 3)

###Definindo classes

k <- floor(1+3.3*log10(nrow(dados_co))) #número de classes
minimum <- min(dados_co$X_est)
maximum <- max(dados_co$X_est)
h <- ceiling(1000*(maximum - minimum)/k)/1000 #amplitude das classes
min_trunc <- trunc(minimum*1000)/1000
seq(min_trunc, min_trunc+k*h, h) #classes

###Escala de cores
dados_co <- dados_co %>% mutate(beta_cor=case_when(
  X_est >= 0.019 & X_est < 0.023 ~ "#F0F0F0",
  X_est >= 0.023 & X_est < 0.027 ~ "#D1E5EC",     
  X_est >= 0.027 & X_est < 0.031 ~ "#B0D5E6",     
  X_est >= 0.031 & X_est < 0.035 ~ "#8DC5D8",
  X_est >= 0.035 & X_est < 0.039 ~ "#62ABCC",
  X_est >= 0.039 & X_est < 0.043 ~ "#3B92BB",
  X_est >= 0.043 & X_est < 0.047 ~ "#1381B2",
  X_est >= 0.047 & X_est < 0.051 ~ "#0273A2",
  X_est >= 0.051 & X_est < 0.055 ~ "#006C9E"
))


###Popup
beta_popup <- paste0("<strong>Local: </strong>", 
                   dados_co$nome, 
                   "<br><strong>Valor: </strong>", 
                   dados_co$X_est)

###Mapa
mapa_beta <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
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
    labels = c("0.019 -| 0.023", "0.023 -| 0.027", "0.027 -| 0.031",
               "0.031 -| 0.035", "0.035 -| 0.039", "0.039 -| 0.043",
               "0.043 -| 0.047", "0.047 -| 0.051", "0.051 -| 0.055"),
    colors = c("#F0F0F0", "#D1E5EC", "#B0D5E6", "#8DC5D8", "#62ABCC",
               "#3B92BB", "#1381B2", "#0273A2", "#006C9E"),
    title = "Beta"
  )

mapa_beta

# mapa alphas

dados_co$alpha <- round(as.numeric((mod$alpha_estimates)[,"alpha"]), 3)

###Definindo classes

minimum <- min(dados_co$alpha)
maximum <- max(dados_co$alpha)
h <- ceiling(1000*(maximum - minimum)/k)/1000 #amplitude das classes
min_trunc <- trunc(minimum*1000)/1000
seq(min_trunc, min_trunc+k*h, h) #classes

###Escala de cores
dados_co <- dados_co %>% mutate(alpha_cor=case_when(
  alpha >= 0.000 & alpha < 0.124 ~ "#F0F0F0",
  alpha >= 0.124 & alpha < 0.248 ~ "#D1E5EC",     
  alpha >= 0.248 & alpha < 0.372 ~ "#B0D5E6",     
  alpha >= 0.372 & alpha < 0.496 ~ "#8DC5D8",
  alpha >= 0.496 & alpha < 0.620 ~ "#62ABCC",
  alpha >= 0.620 & alpha < 0.744 ~ "#3B92BB",
  alpha >= 0.744 & alpha < 0.868 ~ "#1381B2",
  alpha >= 0.868 & alpha < 0.992 ~ "#0273A2",
  alpha >= 0.992 & alpha < 1.116 ~ "#006C9E"
))
#c("#F0F0F0", "#D1E5EC", "#B0D5E6", "#8DC5D8", "#62ABCC", "#3B92BB", "#1381B2", "#0273A2", "#006C9E")

###Popup
alpha_popup <- paste0("<strong>Local: </strong>", 
                     dados_co$nome, 
                     "<br><strong>Valor: </strong>", 
                     dados_co$alpha)

###Mapa
mapa_alpha <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
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
    labels = c("0.000 -| 0.124", "0.124 -| 0.248", "0.248 -| 0.372",
               "0.372 -| 0.496", "0.496 -| 0.620", "0.620 -| 0.744",
               "0.744 -| 0.868", "0.868 -| 0.992", "0.992 -| 1.116"),
    colors = c("#F0F0F0", "#D1E5EC", "#B0D5E6", "#8DC5D8", "#62ABCC",
               "#3B92BB", "#1381B2", "#0273A2", "#006C9E"),
    title = "Alpha"
  )

mapa_alpha

# mapa lambdas

dados_co$lambda <- round(as.numeric((mod$parameter_estimates)[,"Inf_x_varinf"]), 3)

###Definindo classes

minimum <- min(dados_co$lambda)
maximum <- max(dados_co$lambda)
h <- ceiling(1000*(maximum - minimum)/k)/1000 #amplitude das classes
min_trunc <- trunc(minimum*1000)/1000
seq(min_trunc, min_trunc+k*h, h) #classes

###Escala de cores
dados_co <- dados_co %>% mutate(lambda_cor=case_when(
  lambda >= -0.415 & lambda < -0.368 ~ "#660A00",
  lambda >= -0.368 & lambda < -0.321 ~ "#8C1300",     
  lambda >= -0.321 & lambda < -0.274 ~ "#B31C00",     
  lambda >= -0.274 & lambda < -0.227 ~ "#D92500",
  lambda >= -0.227 & lambda < -0.180 ~ "#FF2E00",
  lambda >= -0.180 & lambda < -0.133 ~ "#FF5C38",
  lambda >= -0.133 & lambda < -0.086 ~ "#FF8A70",
  lambda >= -0.086 & lambda < -0.039 ~ "#FFB8A8",
  lambda >= -0.039 & lambda < 0.008 ~ "#FFE5E0"
))

#c("#FFE5E0", "#FFB8A8", "#FF8A70", "#FF5C38", "#FF2E00", "#D92500", "#B31C00", "#8C1300", "#660A00")
###Popup
lambda_popup <- paste0("<strong>Local: </strong>", 
                      dados_co$nome, 
                      "<br><strong>Valor: </strong>", 
                      dados_co$lambda)

###Mapa
mapa_lambda <- dados_co %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
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
    labels = c("-0.415 |- -0.368",
               "-0.368 |- -0.321",     
               "-0.321 |- -0.274",     
               "-0.274 |- -0.227",
               "-0.227 |- -0.180",
               "-0.180 |- -0.133",
               "-0.133 |- -0.086",
               "-0.086 |- -0.039",
               "-0.039 |-  0.008"),
    colors = c("#660A00", "#8C1300", "#B31C00", "#D92500",
               "#FF2E00", "#FF5C38", "#FF8A70", "#FFB8A8",
               "#FFE5E0"),
    title = "Lambda"
  )

mapa_lambda

#usar mapas mais adequados


