# ----- pacotes necessarios

library(tidyverse)
library(readr)
library(gwzinbr)
library(geobr)
library(sf)
library(stringr)

# ----- leitura e adaptacao

tempor <- tempfile()
download.file(url = 'https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_2024.zip', 
              destfile = tempor)
unzip(tempor, list = TRUE)
eleitorado_local_votacao_2024 <- read_csv2(unz(tempor, 'eleitorado_local_votacao_2024.csv'), locale = locale(encoding = 'ISO-8859-1'))
unlink(tempor)

eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>% 
  select(NR_TURNO,SG_UF,NM_MUNICIPIO,NR_LATITUDE,NR_LONGITUDE,
         DS_SITU_SECAO_ACESSIBILIDADE,NM_LOCAL_VOTACAO_ORIGINAL,DS_ENDERECO_LOCVT_ORIGINAL)

  # Filtrar NR_TURNO apenas para o primeiro
eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>%
  filter(NR_TURNO == 1)

  # Renomear variaveis
eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>%
  rename(y = NR_LATITUDE, x = NR_LONGITUDE)

  # Seções eleitorais sem coordenadas
secoes_sem_coordenadas <- eleitorado_local_votacao_2024 %>%
  filter(x == -1 & y == -1)

eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>%
  filter(!(x == -1 & y == -1))

  # Criar a nova variável quantitativa (0 para "Sem acessibilidade", 1 para "Com acessibilidade")
eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>%
  mutate(ACESSIBILIDADE = if_else(DS_SITU_SECAO_ACESSIBILIDADE == "Com acessibilidade", 1, 0))

is.numeric(eleitorado_local_votacao_2024$ACESSIBILIDADE)

  # Reduzir para apenas os municípios da região Norte
municipios_brasil <- read_municipality(year = 2020)
estados_norte <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
municipios_norte <- municipios_brasil %>%
  filter(abbrev_state %in% estados_norte)

municipios_norte <- municipios_norte %>%
  mutate(x = st_coordinates(st_centroid(geom))[,1],
         y = st_coordinates(st_centroid(geom))[,2])

  # Mudar formatacao dos nomes dos municipios
eleitorado_local_votacao_2024 <- eleitorado_local_votacao_2024 %>%
  mutate(NM_MUNICIPIO = str_to_lower(NM_MUNICIPIO))

municipios_norte <- municipios_norte %>%
  mutate(name_muni = str_to_lower(name_muni))

  # Selecionar região norte dos dados do eleitorado
eleitorado_norte <- eleitorado_local_votacao_2024 %>%
  filter(SG_UF %in% estados_norte)

# Agrupar por município e calcular a contagem de acessibilidade
acessibilidade_por_municipio <- eleitorado_norte %>%
  filter(ACESSIBILIDADE == 1) %>%
  group_by(NM_MUNICIPIO) %>%
  summarize(N_ACESSIBILIDADE = n()) %>%
  ungroup()

eleitorado_local_acessibilidade_NORTE <- acessibilidade_por_municipio %>%
  left_join(municipios_norte, by = c("NM_MUNICIPIO" = "name_muni")) %>%
  select(NM_MUNICIPIO, x, y, N_ACESSIBILIDADE)

library(ggplot2)
ggplot(eleitorado_local_acessibilidade_NORTE) + 
  geom_histogram(aes(N_ACESSIBILIDADE), fill = "darkgray") + 
  theme_light() + 
  labs(title = "Distribuição de Frequência das Seções Eleitorais com acessibilidade na Região Norte", 
       x = "Número de Seções Eleitorais com acessibilidade", y = "Frequência")

# ----- variaveis explicativas possíveis de anexar: quantidade total de eleitores, quantidade de eleitores com deficiencia

  # Obtendo quantidade de eleitores com deficiencia
tempor <- tempfile()
download.file(url = 'https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_deficiente/perfil_eleitor_deficiencia_2024.zip', 
              destfile = tempor)
unzip(tempor, list = TRUE)

ler_arquivo <- function(uf){
  read_csv2(unz(tempor, paste0("perfil_eleitor_deficiencia_2024_", uf, ".csv")), 
            locale = locale(encoding = 'ISO-8859-1'))
}

perfil_eleitor_deficiencia_2024_NORTE <- map_dfr(estados_norte, ler_arquivo)

perfil_eleitor_deficiencia_2024_NORTE <- perfil_eleitor_deficiencia_2024_NORTE %>%
  mutate(NM_MUNICIPIO = str_to_lower(NM_MUNICIPIO))

    # Agrupar por município e calcular a contagem de eleitorado com deficiência
eleitorado_por_deficiencia_NORTE <- perfil_eleitor_deficiencia_2024_NORTE %>%
  group_by(NM_MUNICIPIO) %>%
  summarize(N_PCD = n()) 

eleitorado_PCD_acessib_FINAL <- eleitorado_local_acessibilidade_NORTE %>%
  inner_join(eleitorado_por_deficiencia_NORTE, by = "NM_MUNICIPIO")

  # Obtendo quantidade de eleitores total 
tempor <- tempfile()
download.file(url = 'https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2024.zip', 
              destfile = tempor)
unzip(tempor, list = TRUE)
perfil_eleitorado_2024 <- read_csv2(unz(tempor, 'perfil_eleitorado_2024.csv'), locale = locale(encoding = 'ISO-8859-1'))
unlink(tempor)

perfil_eleitorado_2024 <- perfil_eleitorado_2024 %>% 
  mutate(NM_MUNICIPIO = str_to_lower(NM_MUNICIPIO))

  # Filtrar e agrupar o perfil_eleitorado_2024 para os estados da Região Norte por
    # Total de eleitores e Total de eleitores com deficiência
perfil_eleitorado_2024_NORTE <- perfil_eleitorado_2024 %>% 
  filter(SG_UF %in% estados_norte) %>% 
  group_by(NM_MUNICIPIO) %>% 
  summarize(
    TOTAL_ELEITORES = sum(QT_ELEITORES_PERFIL, na.rm = TRUE),
    TOTAL_ELEITORES_DEFICIENCIA = sum(QT_ELEITORES_DEFICIENCIA, na.rm = TRUE)
  )

  # Dataframe final

eleicoes_2024_eleitorado_PCD_acessibilidade <- eleitorado_local_acessibilidade_NORTE %>% 
  inner_join(perfil_eleitorado_2024_NORTE, by = "NM_MUNICIPIO")

eleicoes_2024_eleitorado_PCD_acessibilidade <- eleicoes_2024_eleitorado_PCD_acessibilidade %>% 
  filter(!is.na(x) & !is.na(y))

library(xlsx)
write_excel_csv(eleicoes_2024_eleitorado_PCD_acessibilidade, 
      "eleicoes_2024_eleitorado_PCD_acessibilidade.xlsx")

# ----- rodar o modelo
Golden(data = eleicoes_2024_eleitorado_PCD_acessibilidade,
       formula = N_ACESSIBILIDADE ~ TOTAL_ELEITORES + TOTAL_ELEITORES_DEFICIENCIA,
       xvarinf = NULL, long = "x", lat = "y", model = 'zinb', 
       method = "adaptive_bsq", bandwidth = 'aic', globalmin = FALSE,
       distancekm = TRUE, force = FALSE)


gwzinbr(eleicoes_2024_eleitorado_PCD_acessibilidade,
        formula = N_ACESSIBILIDADE ~ TOTAL_ELEITORES + TOTAL_ELEITORES_DEFICIENCIA,
        xvarinf = c("TOTAL_ELEITORES", "TOTAL_ELEITORES_DEFICIENCIA"),
        long = "x", lat = "y", model = 'zinb', method = "adaptive_bsq",
        distancekm = TRUE, h = 184, force = FALSE)

# ----- mapas

library(leaflet)

  # Criando escala de cores

    # Definindo classes

k <- floor(1+3.3*log10(nrow(eleicoes_2024_eleitorado_PCD_acessibilidade))) #número de classes
minimum <- min(eleicoes_2024_eleitorado_PCD_acessibilidade$N_ACESSIBILIDADE)
maximum <- max(eleicoes_2024_eleitorado_PCD_acessibilidade$N_ACESSIBILIDADE)
h <- round((maximum - minimum)/k, 1) #amplitude das classes
min_trunc <- trunc(minimum*10)/10
seq(min_trunc, min_trunc+8*h, h) #classes


eleicoes_2024_eleitorado_PCD_acessibilidade <- eleicoes_2024_eleitorado_PCD_acessibilidade  %>% 
  mutate(N_ACESSIBILIDADE_COL=case_when(
    N_ACESSIBILIDADE >= 1 & N_ACESSIBILIDADE < 192.7 ~  "#FFAAAA",
    N_ACESSIBILIDADE >= 192.7 & N_ACESSIBILIDADE < 384.4 ~ "#FF8888",
    N_ACESSIBILIDADE >= 384.4 & N_ACESSIBILIDADE < 576.1 ~ "#FF6666", 
    N_ACESSIBILIDADE >= 576.1 & N_ACESSIBILIDADE < 767.8 ~ "#FF4444",
    N_ACESSIBILIDADE >= 767.8 & N_ACESSIBILIDADE < 959.5 ~ "#FF2222",
    N_ACESSIBILIDADE >= 959.9 & N_ACESSIBILIDADE < 1151.2 ~ "#FF0000",
    N_ACESSIBILIDADE >= 1151.2 & N_ACESSIBILIDADE < 1342.9 ~ "#CC0000",
    N_ACESSIBILIDADE >= 1342.9 & N_ACESSIBILIDADE < 1534.6 ~ "#990000",
    N_ACESSIBILIDADE >= 1534.6 ~ "#3C090E"
))


  # Criando popup do mapa

state_popup <- paste0("<strong>Local: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$NM_MUNICIPIO, 
                      "<br><strong>Casos: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$N_ACESSIBILIDADE)
  
  # Mapa da variável resposta

mapa_resposta <- eleicoes_2024_eleitorado_PCD_acessibilidade %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Esri World Image") %>%  
  addCircleMarkers(
    color = ~N_ACESSIBILIDADE_COL,
    fillColor = ~N_ACESSIBILIDADE_COL,
    fillOpacity = 0.8,
    weight = 1,
    radius = 7,
    stroke = TRUE,
    popup = state_popup,
    lat = ~y,
    lng = ~x
  ) %>%
  addLegend(
    labels = c("1 |- 192.7", "192.7 |- 384.4", 
               "384.4 |- 576.1", "576.1 |- 767.8", 
               "767.8 |- 959.5", "959.5 |- 1151.2", 
               "1151.2 |- 1342.9", "1342.9 |- 1534.6", " >= 1534.6"),
    colors = c(colors = c("#FFAAAA", "#FF8888", "#FF6666",
    "#FF4444", "#FF2222", "#FF0000", "#CC0000", "#990000", "#3C090E")),
    title = "Número de seções eleitorais com acessibilidade"
  )

mapa_resposta

  # Mapa da variável explicativa - TOTAL_ELEITORES

k <- floor(1+3.3*log10(nrow(eleicoes_2024_eleitorado_PCD_acessibilidade))) #número de classes
minimum <- min(eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES)
maximum <- max(eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES)
h <- round((maximum - minimum)/k, 1) #amplitude das classes
min_trunc <- trunc(minimum*10)/10
seq(min_trunc, min_trunc+8*h, h) #classes


eleicoes_2024_eleitorado_PCD_acessibilidade <- eleicoes_2024_eleitorado_PCD_acessibilidade  %>% 
  mutate(TOTAL_ELEITORES_COL=case_when(
    TOTAL_ELEITORES >= 1793 & TOTAL_ELEITORES < 162274 ~  "#FFAAAA",
    TOTAL_ELEITORES >= 162274 & TOTAL_ELEITORES < 322755 ~ "#FF8888",
    TOTAL_ELEITORES >= 322755 & TOTAL_ELEITORES < 483236 ~ "#FF6666", 
    TOTAL_ELEITORES >= 483236 & TOTAL_ELEITORES < 643717 ~ "#FF4444",
    TOTAL_ELEITORES >= 643717 & TOTAL_ELEITORES < 804198 ~ "#FF2222",
    TOTAL_ELEITORES >= 804198 & TOTAL_ELEITORES < 964679 ~ "#FF0000",
    TOTAL_ELEITORES >= 964679 & TOTAL_ELEITORES < 1125160 ~ "#CC0000",
    TOTAL_ELEITORES >= 1125160 & TOTAL_ELEITORES < 1285641 ~ "#990000",
    TOTAL_ELEITORES >= 1285641 ~ "#3C090E"
  ))


# Criando popup do mapa

state_popup <- paste0("<strong>Local: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$NM_MUNICIPIO, 
                      "<br><strong>Casos: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES)


mapa_total_eleitores <- eleicoes_2024_eleitorado_PCD_acessibilidade %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Esri World Image") %>%  
  addCircleMarkers(
    color = ~TOTAL_ELEITORES_COL,
    fillColor = ~TOTAL_ELEITORES_COL,
    fillOpacity = 0.8,
    weight = 1,
    radius = 7,
    stroke = TRUE,
    popup = state_popup,
    lat = ~y,
    lng = ~x
  ) %>%
  addLegend(
    labels = c("1793 |- 162274", "162274 |- 322755", 
               "322755 |- 483236", "483236 |- 643717",
               "643717 |- 804198", "804198 |- 964679",
               "964679 |- 1125160", "1125160 |- 1285641", " >= 1285641"),
    colors = c(colors = c("#FFAAAA", "#FF8888", "#FF6666",
                          "#FF4444", "#FF2222", "#FF0000", "#CC0000", "#990000", "#3C090E")),
    title = "Total de eleitores por município"
  )

mapa_total_eleitores




  # Mapa da variável explicativa - TOTAL_ELEITORES_DEFICIENCIA

k <- floor(1+3.3*log10(nrow(eleicoes_2024_eleitorado_PCD_acessibilidade))) #número de classes
minimum <- min(eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES_DEFICIENCIA)
maximum <- max(eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES_DEFICIENCIA)
h <- round((maximum - minimum)/k, 1) #amplitude das classes
min_trunc <- trunc(minimum*10)/10
seq(min_trunc, min_trunc+8*h, h) #classes


eleicoes_2024_eleitorado_PCD_acessibilidade <- eleicoes_2024_eleitorado_PCD_acessibilidade  %>% 
  mutate(TOTAL_ELEITORES_DEFICIENCIA_COL=case_when(
    TOTAL_ELEITORES_DEFICIENCIA >= 7 & TOTAL_ELEITORES_DEFICIENCIA < 2217.9 ~  "#FFAAAA",
    TOTAL_ELEITORES_DEFICIENCIA >= 2217.9 & TOTAL_ELEITORES_DEFICIENCIA < 4428.8 ~ "#FF8888",
    TOTAL_ELEITORES_DEFICIENCIA >= 4428.8 & TOTAL_ELEITORES_DEFICIENCIA < 6639.7 ~ "#FF6666", 
    TOTAL_ELEITORES_DEFICIENCIA >= 6639.7 & TOTAL_ELEITORES_DEFICIENCIA < 8850.6 ~ "#FF4444",
    TOTAL_ELEITORES_DEFICIENCIA >= 8850.6 & TOTAL_ELEITORES_DEFICIENCIA < 11061.5 ~ "#FF2222",
    TOTAL_ELEITORES_DEFICIENCIA >= 11061.5 & TOTAL_ELEITORES_DEFICIENCIA < 13272.4 ~ "#FF0000",
    TOTAL_ELEITORES_DEFICIENCIA >= 13272.4  & TOTAL_ELEITORES_DEFICIENCIA < 15483.3 ~ "#CC0000",
    TOTAL_ELEITORES_DEFICIENCIA >= 15483.3 & TOTAL_ELEITORES_DEFICIENCIA < 17694.2 ~ "#990000",
    TOTAL_ELEITORES_DEFICIENCIA >= 17694.2 ~ "#3C090E"
  ))

# labels = c("7 |- 2217.9", "2217.9 |- 4428.8", 
#            "4428.8 |- 6639.7", "6639.7 |- 8850.6",
#            "8850.6 |- 11061.5", "11061.5 |- 13272.4",
#            "13272.4 |- 15483.3", "15483.3 |- 17694.2", " >= 17694.2")

# Criando popup do mapa

state_popup <- paste0("<strong>Local: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$NM_MUNICIPIO, 
                      "<br><strong>Casos: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$TOTAL_ELEITORES_DEFICIENCIA)

mapa_total_eleitores_deficiencia <- eleicoes_2024_eleitorado_PCD_acessibilidade %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Esri World Image") %>%  
  addCircleMarkers(
    color = ~TOTAL_ELEITORES_DEFICIENCIA_COL,
    fillColor = ~TOTAL_ELEITORES_DEFICIENCIA_COL,
    fillOpacity = 0.8,
    weight = 1,
    radius = 7,
    stroke = TRUE,
    popup = state_popup,
    lat = ~y,
    lng = ~x
  ) %>%
  addLegend(
    labels = c("7 |- 2217.9", "2217.9 |- 4428.8", 
               "4428.8 |- 6639.7", "6639.7 |- 8850.6",
               "8850.6 |- 11061.5", "11061.5 |- 13272.4",
               "13272.4 |- 15483.3", "15483.3 |- 17694.2", " >= 17694.2"),
    colors = c(colors = c("#FFAAAA", "#FF8888", "#FF6666",
                          "#FF4444", "#FF2222", "#FF0000", "#CC0000", "#990000", "#3C090E")),
    title = "Total de eleitores com deficiência por município"
  )

mapa_total_eleitores_deficiencia


  # Mapa parâmetro alpha

#Pegando as estimativas

eleicoes_2024_eleitorado_PCD_acessibilidade$alpha <- round(as.numeric((modelo$alpha_estimates)[,"alpha"]), 3)

#Definindo classes

minimum <- min(eleicoes_2024_eleitorado_PCD_acessibilidade$alpha)
maximum <- max(eleicoes_2024_eleitorado_PCD_acessibilidade$alpha)
h <- ceiling(1000*(maximum - minimum)/k)/1000 #amplitude das classes
min_trunc <- trunc(minimum*1000)/1000
seq(min_trunc, min_trunc+k*h, h) #classes

#Criando escala de cores

eleicoes_2024_eleitorado_PCD_acessibilidade <- eleicoes_2024_eleitorado_PCD_acessibilidade  %>% 
  mutate(alpha_col=case_when(
    alpha >= 0 & alpha < 0.183 ~  "#FFAAAA",
    alpha >= 0.183 & alpha < 0.366 ~ "#FF8888",
    alpha >= 0.366 & alpha < 0.549 ~ "#FF6666", 
    alpha >= 0.549 & alpha < 0.732 ~ "#FF4444",
    alpha >= 0.732 & alpha < 0.915 ~ "#FF2222",
    alpha >= 0.915 & alpha < 1.098 ~ "#FF0000",
    alpha >= 1.098 & alpha < 1.281 ~ "#CC0000",
    alpha >= 1.281 & alpha < 1.464 ~ "#990000",
    alpha >= 1.464 & alpha < 1.647 ~ "#551A00",
    alpha >= 1.647 ~ "#3C090E"
  ))


#Criando popup do mapa

state_popup <- paste0("<strong>Local: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$NM_MUNICIPIO, 
                      "<br><strong>Estimativa para alpha: </strong>", 
                      eleicoes_2024_eleitorado_PCD_acessibilidade$alpha)

#Mapa

#mudar escala de cores para incluir o aspecto negativo->positivo
mapa_alpha <- eleicoes_2024_eleitorado_PCD_acessibilidade %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Esri World Image") %>%  
  addCircleMarkers(
    color = ~alpha_col,
    fillColor = ~alpha_col,
    fillOpacity = 0.8,
    weight = 1,
    radius = 7,
    stroke = TRUE,
    popup = state_popup,
    lat = ~y,
    lng = ~x
  ) %>%
  addLegend(
    labels = c("0.000 |- 0.183", "0.183 |- 0.366", "0.366 |- 0.549",
               "0.549 |- 0.732", "0.732 |- 0.915", "0.915 |- 1.098",
               "1.098 |- 1.281", "1.281 |- 1.464", "1.464 |- 1.647",
               ">= 1.647"),
    colors = c("#FFAAAA", "#FF8888", "#FF6666",
    "#FF4444", "#FF2222", "#FF0000", "#CC0000", "#990000", "#551A00", "#3C090E"),
    title = "Superdispersão"
  )

mapa_alpha


  # Mapa Parâmetro Lambda

# Para TOTAL_ELEITORES

#Pegando as estimativas

eleicoes_2024_eleitorado_PCD_acessibilidade$lambda <- round(as.numeric((modelo$parameter_estimates)[,"Inf_TOTAL_ELEITORES"]), 1)


# Obs.: NÃO SAÍRAM ESTIMATIVAS PARA LAMBDA PQ O MODELO NÃO É INF. DE ZEROS. 



#Definindo classes

minimum <- min(korea_df$lambda)
maximum <- max(korea_df$lambda)
minimum
maximum

#Criando escala de cores

korea_df <- korea_df %>% mutate(lambda_col=case_when(
  lambda >= (-300) & lambda < (-150) ~ "#ef746f",     
  lambda >= (-150) & lambda < 0 ~ "#fbb4b9",     
  lambda == 0 ~ "#ffffff",     
  lambda > 0 & lambda <= 150 ~ "#9ecae1", 
  lambda > 150 & lambda <= 300 ~ "#4292c6"
))
#scale_color <- c("#ef746f", "#fdd0a2", "#ffffff", "#9ecae1", "#4292c6")

#Criando popup do mapa

state_popup <- paste0("<strong>Local: </strong>", 
                      korea_df$IDNAME, 
                      "<br><strong>Estimativa para Crowding (IZ): </strong>", 
                      korea_df$lambda)

#Mapa

#mudar escala de cores para incluir o aspecto negativo->positivo
mapa_lambda <- korea_df %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
  addCircleMarkers(
    color = ~lambda_col,
    fillColor = ~lambda_col,
    fillOpacity = 0.8,
    weight = 1,
    radius = 7,
    stroke = TRUE,
    popup = state_popup,
    lat = ~y,
    lng = ~x
  ) %>%
  addLegend(
    labels = c("-300 |- -150", "-150 |- 0", "0", "0 -| 150", "150 -| 300"),
    colors = c("#ef746f", "#fbb4b9", "#ffffff", "#9ecae1", "#4292c6"),
    title = "Estimativa para Parâmetro Crowding IZ"
  )

mapa_lambda


