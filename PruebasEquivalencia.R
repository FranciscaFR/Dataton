#El siguiente código se ha escrito para identificar qué municipios son equivalentes a nivel localidad
#Usamos una prueba t que nos indicará si el promedio de cada uno de los indicadores (im_2020, imn_2020, ice, ice_n) es diferente a nivel localidad (se descartaron algunas variables por la ausencia de información para todas las localidades)
#Este análisis es necesario para validar el análisis con datos de la ENIGH, los cuales están agregados únicamente a nivel entidad para llegar a conclusiones sobre las localidades

library(dplyr)
library(readxl)

data <- read_excel("ICE_Localidad_2020.xlsx", sheet = "Resumen", skip=1)
data <- data %>%
  mutate_at(vars(IM_2020, IMN_2020, ICE, ICE_N, ENT, MUN, LOC), as.numeric)

#Datos a nivel localidad
localidad_summ <- data %>%
  group_by(NOM_ENT, NOM_MUN, NOM_LOC, ENT, MUN, LOC) %>%
  summarise(
    im_2020_loc = mean(IM_2020, na.rm = TRUE),
    imn_2020_loc = mean(IMN_2020, na.rm = TRUE),
    ice_loc = mean(ICE, na.rm = TRUE),
    ice_n_loc = mean(ICE_N, na.rm = TRUE)
  )

#Datos a nivel municipio, se usan las variables con terminación _avg para indicar que es el promedio de todos los valores mostrados por columna por municipio.
#Es decir, captura el promedio de las localides y lo asigna al municipio
municipal_summ <- data %>%
  group_by(NOM_ENT, NOM_MUN, MUN) %>%
  summarise(
    im_2020_avg = mean(IM_2020, na.rm = TRUE),
    imn_2020_avg = mean(IMN_2020, na.rm = TRUE),
    ice_avg = mean(ICE, na.rm = TRUE),
    ice_n_avg = mean(ICE_N, na.rm = TRUE)
  ) %>%
  ungroup() 

#La siguiente tabla junta los datos individuales de localidades y municipios
loc_mun <- localidad_summ %>%
  left_join(municipal_summ, by = c("NOM_ENT", "NOM_MUN", "MUN"))
loc_mun <- loc_mun %>%
  mutate(ENT_MUN_ID = paste(ENT, MUN, sep = "_"))

resultados <- data.frame(
  NOM_ENT = character(),
  NOM_MUN = character(),
  NOM_LOC = character(),
  ENT = numeric(),
  MUN = numeric(),
  LOC = numeric(),
  im_2020_t_stat = numeric(),
  im_2020_pval = numeric(),
  imn_2020_t_stat = numeric(),
  imn_2020_pval = numeric(),
  ice_t_stat = numeric(),
  ice_pval = numeric(),
  ice_n_t_stat = numeric(),
  ice_n_pval = numeric(),
  stringsAsFactors = FALSE
)

#A continuación, se hacen pruebas t de student para identificar si los datos son estadísticamente diferentes
#En esta prueba la hipótesis nula es que no hay diferencias significativas (H0: X=Mu), queremos no rechazar
#Para nuestros fines, queremos validar que los datos desagregados por localidad son equivalentes al promedio obtenido de los mismos a nivel municipal
#Esto nos permitiría hacer inferencias sobre las características municipales y usar bases de datos desagregadas a ese nivel, no a nivel localidad o sección
for (i in 1:nrow(loc_mun)) {
  #Identificador
  actual_id <- loc_mun$ENT_MUN_ID[i]
  
  #Se queda solo con los valores de LOC para cada municipio y entidad 
  loc_vector_im_2020 <- loc_mun %>% filter(ENT_MUN_ID == actual_id) %>% pull(im_2020_loc)
  loc_vector_imn_2020 <- loc_mun %>% filter(ENT_MUN_ID == actual_id) %>% pull(imn_2020_loc)
  loc_vector_ice <- loc_mun %>% filter(ENT_MUN_ID == actual_id) %>% pull(ice_loc)
  loc_vector_ice_n <- loc_mun %>% filter(ENT_MUN_ID == actual_id) %>% pull(ice_n_loc)
  
  #Se queda solo con los valores municipales para poder comparar
  mun_valor_im_2020 <- loc_mun$im_2020_avg[i]
  mun_valor_imn_2020 <- loc_mun$imn_2020_avg[i]
  mun_valor_ice <- loc_mun$ice_avg[i]
  mun_valor_ice_n <- loc_mun$ice_n_avg[i]
  
  #T test para determinar si son estadísticamente diferentes 
  im_2020_ttest <- t.test(loc_vector_im_2020, mu = mun_valor_im_2020, var.equal = FALSE)
  imn_2020_ttest <- t.test(loc_vector_imn_2020, mu = mun_valor_imn_2020, var.equal = FALSE)
  ice_ttest <- t.test(loc_vector_ice, mu = mun_valor_ice, var.equal = FALSE)
  ice_n_ttest <- t.test(loc_vector_ice_n, mu = mun_valor_ice_n, var.equal = FALSE)
  
  resultados <- rbind(resultados, data.frame(
    NOM_ENT = loc_mun$NOM_ENT[i],
    NOM_MUN = loc_mun$NOM_MUN[i],
    NOM_LOC = loc_mun$NOM_LOC[i],
    ENT = loc_mun$ENT[i],
    MUN = loc_mun$MUN[i],
    LOC = loc_mun$LOC[i],
    im_2020_t_stat = im_2020_ttest$statistic,
    im_2020_pval = im_2020_ttest$p.value,
    imn_2020_t_stat = imn_2020_ttest$statistic,
    imn_2020_pval = imn_2020_ttest$p.value,
    ice_t_stat = ice_ttest$statistic,
    ice_pval = ice_ttest$p.value,
    ice_n_t_stat = ice_n_ttest$statistic,
    ice_n_pval = ice_n_ttest$p.value
  ))
}

#Filtramos los resultados en los que se rechace la H0, es decir, que sean signficativamente diferentes
diferentes <- resultados %>%
  filter(im_2020_pval < 0.05 | imn_2020_pval < 0.05 | ice_pval < 0.05 | ice_n_pval < 0.05)

print(diferentes)

