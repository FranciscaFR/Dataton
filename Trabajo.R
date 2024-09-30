library(stargazer)
library(readxl)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(tidyr)
install.packages("writexl")
library(writexl)

#A continuación, se generarán diferentes regresiones múltiples con las siguientes variaciones:
# 3 nivel: una a nivel sección electoral, otra a nivel municipal y otra a nivel entidad
#Esto se hará para identificar cuáles son los factores asociados a una mayor densidad de farmacias en cada nivel 

options(scipen = 999999)
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
#Cargamos datos del censo únicamente y nos quedamos con las variables de interés
censo <- read.csv("INE_SECCION_2020.csv") |>
  select(ID, ENTIDAD, MUNICIPIO, SECCION, POBTOT, PCON_DISC, PDER_SS, PAFIL_IPRI, PAFIL_OTRA) |>
  janitor::clean_names()
#Base de datos de farmacias en la última edición del DENUE (con las variables de interés)
denue_2024 = read_csv("05_2024_DENUE.csv") |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  mutate_at(vars(cve_ent, cve_mun, cve_loc), as.numeric) |>
  select(id, nom_estab, raz_social, codigo_act, nombre_act, per_ocu, cve_ent, entidad, cve_mun, municipio, cve_loc, localidad, ageb, manzana, latitud, longitud, fecha_alta)  |>
  filter(str_detect(nombre_act, "farm"))

#Comenzando con el sección electoral: Num farmacias por sección vs población total, relación hombres y mujeres, población sin derecho a la salud y población con seguros privados
#Bases de datos utilizadas: DENUE, CENSO 2020
secciones <- st_read("SECCION.shp")
municipios <- st_read("MUNICIPIO.shp")

#Cargamos unión de datos censo y secciones (conservando los datos de censo que nos importan y pasando a proporciones)
mex_secciones = left_join(secciones, censo, 
                   by = c("SECCION" = "seccion",
                          "ENTIDAD" = "entidad",
                          "MUNICIPIO" = "municipio")) |>
  mutate(area_seccion_m2 = st_area(geometry),
         prop_disc = pcon_disc/pobtot, #share personas con discapacidad
         prop_acceso_salud = pder_ss/pobtot, #share personas con acceso a la salud
         prop_salud_priv = pafil_ipri/pobtot) #share personas afilidadas a institución de salud privada 
#Localización de las farmacias
farmacias_2024 = st_as_sf(denue_2024, coords = c("longitud", "latitud"), crs = 4326) |> st_transform(st_crs(mex_secciones)) 

#Guardamos en una base de las farmacias registradas por sección
farm_limpias = st_join(mex_secciones, farmacias_2024) |> 
  select(-ID, -DISTRITO_F, -DISTRITO_L, -TIPO, -CONTROL, -GEOMETRY1_, -id.x, -id.y) |>
  filter(!is.na(nombre_act))

#Contamos farmacias por sección
farm_secc_cuenta <- farm_limpias %>%
  group_by(ENTIDAD, MUNICIPIO, SECCION) %>%
  summarise(farm_num = n(), .groups = "drop") 

combinada_secc <- farm_limpias %>%
  st_join(farm_secc_cuenta, by = c("ENTIDAD", "MUNICIPIO", "SECCION"))

#Regresión por sección electoral con todas las farmacias incluidas
reg_secc1 <- lm(farm_num ~ pobtot + prop_disc + prop_acceso_salud + prop_salud_priv + area_seccion_m2, data = combinada_secc)
reg_secc <- lm(farm_num ~ pobtot + prop_disc + prop_acceso_salud + prop_salud_priv, data = combinada_secc)
summary(reg_secc)

#Nivel municipal:
#Cargamos las bases de datos (las dos siguientes son de CONAPO)
ICE <- read_excel("ICE_Localidad_2020.xlsx", skip=1)|>
  mutate_at(vars(ENT, MUN, LOC), as.numeric)|>
  select(-CVE_LOC,-CVE_CONF, -AMBITO,-COMENTARIOS,-IMN_2020,-G_IE,-G_ICE,-ICE_N)
ICE <- ICE %>%
  mutate(GACU = case_when(
    GACU == "Muy  bajo (más de 120 min)" ~ 1,
    GACU == "Bajo (de 91 a 120 min)" ~ 2,
    GACU == "Medio (de 61 a 90 min)" ~ 3,
    GACU == "Alto (de 31 a 60 min)" ~ 4,
    GACU == "Muy alto (menos de 30 min)" ~ 5,
    TRUE ~ NA_real_  
  ))
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
ICE <- ICE %>%
  group_by(ENT, MUN) %>% 
  summarise(IM_2020 = mean(IM_2020, na.rm = TRUE), 
            IE = mean(IE, na.rm = TRUE), 
            ICE = mean(ICE, na.rm = TRUE), 
            GACU = moda(GACU),
            .groups = "drop")
#De esta base de datos usaremos IM, IE, ICE, GACU

farm_mun_cuenta <- farm_limpias %>%
  group_by(ENTIDAD, MUNICIPIO) %>%
  summarise(farm_num_mun = n(), 
            pobtot = mean(pobtot, na.rm = TRUE), 
            prop_disc = mean(prop_disc, na.rm = TRUE), 
            prop_acceso_salud = mean(prop_acceso_salud, na.rm = TRUE), 
            prop_salud_priv = mean(prop_salud_priv, na.rm = TRUE), 
            .groups = "drop")

farm_municip <- left_join(farm_mun_cuenta, ICE, by = c("ENTIDAD" = "ENT", "MUNICIPIO" = "MUN"))

reg_mun1 <- lm(farm_num_mun ~ pobtot + prop_disc + prop_acceso_salud + prop_salud_priv + IM_2020 + IE + ICE, data = farm_municip)
reg_mun <- lm(farm_num_mun ~ IM_2020 + IE + ICE, data = farm_municip)
summary(reg_mun1)
summary(reg_mun)

#Nivel entidad:
ENIGH_hogar = read_csv("ENIGH_GASTOS_HOGAR/conjunto_de_datos/conjunto_de_datos_gastoshogar_enigh2022_ns.csv")
#Usaremos solo ciertos gastos (categoría J, algunos de ellos, categoría médica)
gastos_med <- c("J004", "J009", "J010", "J020", "J021", "J022", "J023", "J024", 
                "J025", "J026", "J027", "J028", "J029", "J030", "J031", "J032", 
                "J033", "J034", "J035", "J042", "J044", "J045", "J046", "J047", 
                "J048", "J049", "J050", "J051", "J052", "J053", "J054", "J055", 
                "J056", "J057", "J058", "J059", "J060", "J061", "J063")
gasto_medico <- ENIGH_hogar %>%
  filter(clave %in% gastos_med) %>%           
  group_by(entidad) %>%
  summarise(total_gasto_med = sum(gasto, na.rm = TRUE))

#Usaremos gasto total por entidad 
gasto_total <- ENIGH_hogar %>%
  group_by(entidad) %>%
  summarise(total_gasto = sum(gasto, na.rm = TRUE)) 

ENIGH_ingresos = read_csv("ENIGH_INGRESOS/conjunto_de_datos/conjunto_de_datos_ingresos_enigh2022_ns.csv")
#Usaremos ingreso_tri
ingreso_total <- ENIGH_ingresos %>%
  group_by(entidad) %>%
  summarise(total_ingreso = sum(ing_tri, na.rm = TRUE)) 

gasto <- left_join(gasto_medico,gasto_total, by="entidad")
ENIGH <- left_join(gasto,ingreso_total, by="entidad")|>
  mutate_at(vars(entidad), as.numeric)

farm_ent_cuenta <- farm_limpias %>%
  group_by(ENTIDAD) %>%
  summarise(farm_num_ent = n(), 
            pobtot = mean(pobtot, na.rm = TRUE), 
            prop_disc = mean(prop_disc, na.rm = TRUE), 
            prop_acceso_salud = mean(prop_acceso_salud, na.rm = TRUE), 
            prop_salud_priv = mean(prop_salud_priv, na.rm = TRUE), 
            .groups = "drop")

farm_entidad <- left_join(farm_ent_cuenta, ENIGH, by = c("ENTIDAD" = "entidad"))

reg_ent1 <- lm(farm_num_ent ~ pobtot + prop_disc + prop_acceso_salud + prop_salud_priv + total_gasto_med + total_gasto + total_ingreso, data = farm_entidad)
reg_ent <- lm(farm_num_ent ~ total_gasto_med + total_gasto + total_ingreso, data = farm_entidad)
summary(reg_ent1)
summary(reg_ent)

#Junta todas las regresiones:
stargazer(reg_secc,reg_mun1,reg_mun, reg_ent1, reg_ent, title="Resultados regresiones por niveles de agregación",align=TRUE,no.space=TRUE,type="text")

############################################################################################################

#Análisis de farmacias ANTAD:
#Base de datos obtenida de la COFEPRIS de las farmacias de la ANTAD
farmacias_antad1 <- read_excel("farmacias.xlsx")|>
  separate(ent_mun, into = c("ent","sep","mun"), sep = " | ")|>
  select(-sep)

farm_fenix <- read_excel("FARMACIAS_FENIX.xlsx")|>
  separate(...7, into = c("ent","sep","mun"), sep = " | ")|>
  select(-sep)

farmacias_antad <- bind_rows(farmacias_antad1, farm_fenix)

san_pablo <- farmacias_antad %>%
  filter(grepl("FARMACIA San Pablo", ESTABLECIMIENTO, ignore.case = TRUE))
print(san_pablo, n=100) 
View(san_pablo)

benavides <- farmacias_antad %>%
  filter(grepl("FARMACIAS Benavides", ESTABLECIMIENTO, ignore.case = TRUE))
print(benavides, n=100)

guadalajara <- farmacias_antad %>%
  filter(grepl("FARMACIA Guadalajara|GUADALAJARA", ESTABLECIMIENTO, ignore.case = TRUE))
print(guadalajara, n=100)

ahorro <- farmacias_antad %>%
  filter(grepl("FARMACIA del Ahorro", ESTABLECIMIENTO, ignore.case = TRUE))
print(ahorro, n=100)
View(ahorro)

klyn <- farmacias_antad %>%
  filter(grepl("KLYNS", ESTABLECIMIENTO, ignore.case = TRUE))
print(klyn, n=100)

farmatodo <- farmacias_antad %>%
  filter(grepl("FARMATODO", ESTABLECIMIENTO, ignore.case = TRUE))
print(farmatodo, n=100)

fenix <- farmacias_antad %>%
  filter(grepl("SUPERFARMACIAS EL FENIX", GIRO, ignore.case = TRUE))
print(farmatodo, n=100)
View(fenix)

antad_interes <- bind_rows(san_pablo, benavides, guadalajara, ahorro, klyn, farmatodo, fenix) |>
  mutate_at(vars(ent, mun), as.numeric)
antad_interes <- antad_interes %>%
  filter(!is.na(ent) & mun != "")

antad_interes_completa_casi <- inner_join(antad_interes,ICE, by = c("ent"="ENT", "mun"="MUN"))
antad_interes_completa <- inner_join(antad_interes_completa_casi,farm_mun_cuenta, by = c("ent"="ENTIDAD", "mun"="MUNICIPIO"))

#ANTAD por municipio por farmacia 
antad_interes_completa_sin_fenix <- antad_interes_completa %>%
  group_by(ent, mun, ESTABLECIMIENTO, geometry) %>% 
  summarise(IM_2020 = mean(IM_2020, na.rm = TRUE), 
            IE = mean(IE, na.rm = TRUE), 
            ICE = mean(ICE, na.rm = TRUE), 
            farm_num_mun = farm_num_mun,
            pobtot = pobtot,
            prop_disc =prop_disc,
            prop_acceso_salud = prop_acceso_salud,
            prop_salud_priv = prop_salud_priv,
            .groups = "drop")

#Indicadores farmacia San Pablo
aic_san_pablo  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("FARMACIA San Pablo", ESTABLECIMIENTO, ignore.case = TRUE))
san_pablo_IM_2020 <- mean(aic_san_pablo$IM_2020)
san_pablo_IM_2020
san_pablo_IE <- mean(aic_san_pablo$IE)
san_pablo_IE
san_pablo_ICE <- mean(aic_san_pablo$ICE)
san_pablo_ICE
san_pablo_farm_num_mun <- mean(aic_san_pablo$farm_num_mun)
san_pablo_farm_num_mun
san_pablo_pobtot <- mean(aic_san_pablo$pobtot)
san_pablo_pobtot
san_pablo_prop_disc <- mean(aic_san_pablo$prop_disc)
san_pablo_prop_disc
san_pablo_acceso_salud <- mean(aic_san_pablo$prop_acceso_salud)
san_pablo_acceso_salud
san_pablo_prop_salud_priv <- mean(aic_san_pablo$prop_salud_priv)
san_pablo_prop_salud_priv

#Indicadores farmacia Benavides
aic_benavides  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("FARMACIAS Benavides", ESTABLECIMIENTO, ignore.case = TRUE))
benavides_IM_2020 <- mean(aic_benavides$IM_2020)
benavides_IM_2020
benavides_IE <- mean(aic_benavides$IE)
benavides_IE
benavides_ICE <- mean(aic_benavides$ICE)
benavides_ICE
benavides_farm_num_mun <- mean(aic_benavides$farm_num_mun)
benavides_farm_num_mun
benavides_pobtot <- mean(aic_benavides$pobtot)
benavides_pobtot
benavides_prop_disc <- mean(aic_benavides$prop_disc)
benavides_prop_disc
benavides_acceso_salud <- mean(aic_benavides$prop_acceso_salud)
benavides_acceso_salud
benavides_prop_salud_priv <- mean(aic_benavides$prop_salud_priv)
benavides_prop_salud_priv

#Indicadores farmacia Guadalajara
aic_guadalajara  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("FARMACIAS Gualajara|GUADALAJARA", ESTABLECIMIENTO, ignore.case = TRUE))
guadalajara_IM_2020 <- mean(aic_guadalajara$IM_2020)
guadalajara_IM_2020
guadalajara_IE <- mean(aic_guadalajara$IE)
guadalajara_IE
guadalajara_ICE <- mean(aic_guadalajara$ICE)
guadalajara_ICE
guadalajara_farm_num_mun <- mean(aic_guadalajara$farm_num_mun)
guadalajara_farm_num_mun
guadalajara_pobtot <- mean(aic_guadalajara$pobtot)
guadalajara_pobtot
guadalajara_prop_disc <- mean(aic_guadalajara$prop_disc)
guadalajara_prop_disc
guadalajara_acceso_salud <- mean(aic_guadalajara$prop_acceso_salud)
guadalajara_acceso_salud
guadalajara_prop_salud_priv <- mean(aic_guadalajara$prop_salud_priv)
guadalajara_prop_salud_priv

#Indicadores farmacia del Ahorro
aic_ahorro  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("FARMACIA del Ahorro", ESTABLECIMIENTO, ignore.case = TRUE))
ahorro_IM_2020 <- mean(aic_ahorro$IM_2020)
ahorro_IM_2020
ahorro_IE <- mean(aic_ahorro$IE)
ahorro_IE
ahorro_ICE <- mean(aic_ahorro$ICE)
ahorro_ICE
ahorro_farm_num_mun <- mean(aic_ahorro$farm_num_mun)
ahorro_farm_num_mun
ahorro_pobtot <- mean(aic_ahorro$pobtot)
ahorro_pobtot
ahorro_prop_disc <- mean(aic_ahorro$prop_disc)
ahorro_prop_disc
ahorro_acceso_salud <- mean(aic_ahorro$prop_acceso_salud)
ahorro_acceso_salud
ahorro_prop_salud_priv <- mean(aic_ahorro$prop_salud_priv)
ahorro_prop_salud_priv

#Indicadores farmacia Klyn
aic_klyn  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("KLYNS", ESTABLECIMIENTO, ignore.case = TRUE))
klyn_IM_2020 <- mean(aic_klyn$IM_2020)
klyn_IM_2020
klyn_IE <- mean(aic_klyn$IE)
klyn_IE
klyn_ICE <- mean(aic_klyn$ICE)
klyn_ICE
klyn_farm_num_mun <- mean(aic_klyn$farm_num_mun)
klyn_farm_num_mun
klyn_pobtot <- mean(aic_klyn$pobtot)
klyn_pobtot
klyn_prop_disc <- mean(aic_klyn$prop_disc)
klyn_prop_disc
klyn_acceso_salud <- mean(aic_klyn$prop_acceso_salud)
klyn_acceso_salud
klyn_prop_salud_priv <- mean(aic_klyn$prop_salud_priv)
klyn_prop_salud_priv

#Indicadores farmacia farmatodo
aic_farmatodo  <- antad_interes_completa_sin_fenix %>%
  filter(grepl("FARMATODO", ESTABLECIMIENTO, ignore.case = TRUE))
farmatodo_IM_2020 <- mean(aic_farmatodo$IM_2020)
farmatodo_IM_2020
farmatodo_IE <- mean(aic_farmatodo$IE)
farmatodo_IE
farmatodo_ICE <- mean(aic_farmatodo$ICE)
farmatodo_ICE
farmatodo_farm_num_mun <- mean(aic_farmatodo$farm_num_mun)
farmatodo_farm_num_mun
farmatodo_pobtot <- mean(aic_farmatodo$pobtot)
farmatodo_pobtot
farmatodo_prop_disc <- mean(aic_farmatodo$prop_disc)
farmatodo_prop_disc
farmatodo_acceso_salud <- mean(aic_farmatodo$prop_acceso_salud)
farmatodo_acceso_salud
farmatodo_prop_salud_priv <- mean(aic_farmatodo$prop_salud_priv)
farmatodo_prop_salud_priv

#Indicadores farmacia el fenix
aic_fenix  <- antad_interes_completa %>%
  filter(grepl("SUPERFARMACIAS EL FENIX", GIRO, ignore.case = TRUE))
fenix_IM_2020 <- mean(aic_fenix$IM_2020)
fenix_IM_2020
fenix_IE <- mean(aic_fenix$IE)
fenix_IE
fenix_ICE <- mean(aic_fenix$ICE)
fenix_ICE
fenix_farm_num_mun <- mean(aic_fenix$farm_num_mun)
fenix_farm_num_mun
fenix_pobtot <- mean(aic_fenix$pobtot)
fenix_pobtot
fenix_prop_disc <- mean(aic_fenix$prop_disc)
fenix_prop_disc
fenix_acceso_salud <- mean(aic_fenix$prop_acceso_salud)
fenix_acceso_salud
fenix_prop_salud_priv <- mean(aic_fenix$prop_salud_priv)
fenix_prop_salud_priv

base_sin_farmacias<- inner_join(censo, ICE, by = c("entidad"="ENT", "municipio"="MUN"))
#Filtraremos los datos con intervalos obtenidos a partir de las regresiones múltiples hechas 
# Cuando X cambia en una unidad, Y se incrementa en B1 
    # pobtot = 0.006
      # 1000 habitantes más = 6 farmacias más
    # IM_2020 = -4.222
      # 1 punto adicional en el índice de marginación = -4.222 farmacias
    # prop_acceso_salud = -54.894
      # 1% más de población con acceso = -54.894 farmacias
    # prop_salud_priv = 925.127
      # 1% más de población con salud privada = 925.127 farmacias
    # IE = -26.558
      # 0.1 punto adicional en el índice de equipamiento = -2.6558 farmacias
    # ICE = 19.051
      # 1 punto adicional en el índice de calidad del entorno = 19.051 farmacias

dicc_entidad_mun <- read_excel("~/Desktop/Dataton/dudas y diccionario entidades.xlsx")

INpoptot = (500/3)*5
INIM_2020 = (500/2111)*5
INprop_acceso_salud = (5/27447)*5
INprop_salud_priv = (10/925127)*5
INIE= (500/13279)*5
INICE=(1000/19051)*5

para_fenix_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=fenix_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=fenix_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=fenix_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=fenix_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=fenix_ICE*(1-INICE)&base_sin_farmacias$ICE<=fenix_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=fenix_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=fenix_acceso_salud*(1+INprop_acceso_salud))

contar_fenix <- para_fenix_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(numero_farmacias_por_municipio = n())

para_farmatodo_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=farmatodo_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=farmatodo_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=farmatodo_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=farmatodo_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=farmatodo_ICE*(1-INICE)&base_sin_farmacias$ICE<=farmatodo_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=farmatodo_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=farmatodo_acceso_salud*(1+INprop_acceso_salud))

contar_farmatodo <- para_farmatodo_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(farmatodo_mun_total = n())

para_san_pablo_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=san_pablo_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=san_pablo_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=san_pablo_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=san_pablo_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=san_pablo_ICE*(1-INICE)&base_sin_farmacias$ICE<=san_pablo_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=san_pablo_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=san_pablo_acceso_salud*(1+INprop_acceso_salud))

contar_san_pablo <- para_san_pablo_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(san_pablo_mun_total = n())

para_benavides_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=benavides_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=benavides_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=benavides_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=benavides_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=benavides_ICE*(1-INICE)&base_sin_farmacias$ICE<=benavides_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=benavides_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=benavides_acceso_salud*(1+INprop_acceso_salud))

contar_benavides <- para_benavides_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(benavides_mun_total = n())

para_guadalajara_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=guadalajara_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=guadalajara_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=guadalajara_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=guadalajara_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=guadalajara_ICE*(1-INICE)&base_sin_farmacias$ICE<=guadalajara_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=guadalajara_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=guadalajara_acceso_salud*(1+INprop_acceso_salud))

contar_guadalajara <- para_guadalajara_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(guadalajara_mun_total = n())

para_ahorro_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=ahorro_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=ahorro_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=ahorro_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=ahorro_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=ahorro_ICE*(1-INICE)&base_sin_farmacias$ICE<=ahorro_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=ahorro_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=ahorro_acceso_salud*(1+INprop_acceso_salud))

contar_ahorro <- para_ahorro_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(ahorro_mun_total = n())

para_klyn_filtrados <- base_sin_farmacias %>%
  filter(base_sin_farmacias$pobtot>=klyn_pobtot-(INpoptot)&base_sin_farmacias$pobtot<=klyn_pobtot+(INpoptot),
         base_sin_farmacias$IM_2020>=klyn_IM_2020*(1-INIM_2020)&base_sin_farmacias$IM_2020<=klyn_IM_2020*(1+INIM_2020),
         base_sin_farmacias$ICE>=klyn_ICE*(1-INICE)&base_sin_farmacias$ICE<=klyn_ICE*(1+INICE),
         base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot>=klyn_acceso_salud*(1-INprop_acceso_salud)&base_sin_farmacias$pder_ss/base_sin_farmacias$pobtot<=klyn_acceso_salud*(1+INprop_acceso_salud))

contar_klyn <- para_klyn_filtrados %>%
  group_by(entidad,municipio) %>%
  summarise(klyn_mun_total = n())


contar_fenix_com <- left_join(contar_fenix,dicc_entidad_mun, by = c("entidad","municipio"))
contar_farmatodo_com <- left_join(contar_farmatodo,dicc_entidad_mun, by = c("entidad","municipio"))
contar_san_pablo_com <- left_join(contar_san_pablo,dicc_entidad_mun, by = c("entidad","municipio"))
contar_benavides_com <- left_join(contar_benavides,dicc_entidad_mun, by = c("entidad","municipio"))
contar_guadalajara_com <- left_join(contar_guadalajara,dicc_entidad_mun, by = c("entidad","municipio"))
contar_ahorro_com <- left_join(contar_ahorro,dicc_entidad_mun, by = c("entidad","municipio"))
contar_klyn_com <- left_join(contar_klyn,dicc_entidad_mun, by = c("entidad","municipio"))
write_xlsx(contar_fenix_com, path = "fenix.xlsx")
write_xlsx(contar_san_pablo_com, path = "san_pablo.xlsx")
write_xlsx(contar_benavides_com, path = "benavides.xlsx")
write_xlsx(contar_guadalajara_com, path = "guadalajara.xlsx")
write_xlsx(contar_ahorro_com, path = "ahorro.xlsx")
write_xlsx(contar_klyn_com, path = "klyn.xlsx")

recomendaciones_sin_filtro <- read_excel("todas_farm.xlsx") |>
  mutate(recomenX5 = ahorro_mun_total) |>
  select(-ahorro_mun_total,-...8,-...9,-...10,-...11,-...12,-...13)

farmacias_para_rec <- farm_mun_cuenta |> select (ENTIDAD, MUNICIPIO, farm_num_mun, geometry)

recomendaciones <- full_join(farmacias_para_rec, recomendaciones_sin_filtro, 
                           by = c("ENTIDAD"="entidad", "MUNICIPIO"="municipio"))
recomendaciones <- recomendaciones %>%
  filter(!is.na(recomenX5) & recomenX5 != "")
recomendaciones <- recomendaciones %>%
  filter(!is.na(farm_num_mun) & farm_num_mun != "")
avg_farm_mun<-mean(recomendaciones$farm_num_mun)*1.3
#Establecimos el promedio de farmacias por municipio, debajo de esa media (más el 30% para alcanzar la cantidad necesaria de farmacias), sugerimos agregar farmacias según las recomendaciones
recom_filtradas <- recomendaciones %>%
  filter(farm_num_mun < avg_farm_mun)

#A continuación, se muestran los mapas 

#Mapa de México
ggplot()+
  geom_sf(data = municipios, 
          color = "gray",
          fill = "white",
          size = 0.05)+
  theme_minimal()

mex_municipios = left_join(municipios, censo, 
                           by = c("ENTIDAD" = "entidad",
                                  "MUNICIPIO" = "municipio"))

#Mapa de México con farmacias en cada punto 
ggplot()+
  geom_sf(data = mex_municipios, fill = "white", color = "gray")+
  geom_sf(data = farmacias_2024, color = "#750050", alpha = 0.05, size = 0.5)+
  theme_minimal()

#Mapa de México con las recomendaciones filtradas

ggplot(data = recom_filtradas) +
  geom_sf(aes(fill = num_farm), color = "white", size = 0.05) +
  scale_fill_gradient(low = "lightyellow", high = "darkorange", na.value = "grey90", name = "Número de farmacias") +
  facet_wrap(~ farmacia) +
  theme_minimal() +
  labs(title = "Distribución de farmacias recomendadas",
       subtitle = "Número de farmacias recomendadas por Municipio y por Farmacia")
