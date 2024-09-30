ACU <- read_excel("/Users/josemeade/Desktop/Dataton/Archivos/Base_ACU_loc.xlsx", sheet = "bd_loc", skip = 2)
head(ACU)

farmacias <- subset(denue, codigo_act %in% c(464111, 464112))
head(farmacias)

farm_loc <- farmacias %>% group_by(localidad) %>% summarise(num_farmacias = n())

base_conj <- merge(farm_loc, ACU, by.x="localidad", by.y="NOM_LOC")

mas_de_una_farm <- base_conj %>%
  filter(num_farmacias > 0 & num_farmacias<=500)
reg_tras_a <- lm(num_farmacias ~ TPVLDA, data = mas_de_una_farm)
summary(reg_tras_a)

plot(mas_de_una_farm$TPVLDA, mas_de_una_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al tiempo de traslado a a la ciudad más cercana con más de 100,000 habitantes",
     abline(reg_tras_a, col="blue", lwd=3), xlab="Minutos de traslado a la ciudad más cercana con más de 100,000 habitantes",
     ylab="Número de farmacias en la localidad")

reg_tras_b <- lm(num_farmacias  ~ TPVLDB, data = mas_de_una_farm)
summary(reg_tras_b)
plot(mas_de_una_farm$TPVLDB, mas_de_una_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al tiempo de traslado a a la ciudad más cercana con más de 25,000 habitantes",
     abline(reg_tras_b, col="blue", lwd=3), xlab="Minutos de traslado a la ciudad más cercana con más de 25,000 habitantes",
     ylab="Número de farmacias en la localidad")

reg_tras_c <- lm(num_farmacias  ~ TPVLDC, data = mas_de_una_farm)
summary(reg_tras_c)
plot(mas_de_una_farm$TPVLDC, mas_de_una_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al tiempo de traslado a a la ciudad más cercana con más de 10,000 habitantes",
     abline(reg_tras_c, col="blue", lwd=3), xlab="Minutos de traslado a la ciudad más cercana con más de 10,000 habitantes",
     ylab="Número de farmacias en la localidad")

reg_tras_cu <- lm(num_farmacias  ~ TCVCU, data = mas_de_una_farm)
summary(reg_tras_cu)
plot(mas_de_una_farm$TCVCU, mas_de_una_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al tiempo de traslado a centros urbanos",
     abline(reg_tras_cu, col="blue", lwd=3), xlab="Minutos de traslado a centros urbanos",
     ylab="Número de farmacias en la localidad")

nivel_gacu <- mas_de_una_farm %>% mutate(
  GACU_numeric = case_when(
    GACU == "Muy bajo" ~ 1,
    GACU == "Bajo" ~ 2,
    GACU == "Medio" ~ 3,
    GACU == "Alto" ~ 4,
    GACU == "Muy alto" ~ 5,
    TRUE ~ NA_real_  )) %>%
  group_by(GACU_numeric) %>%
  summarize(total_farmacias = n(), .groups = 'drop')
print(nivel_gacu)
nivel_gacu_limpio <- na.omit(nivel_gacu)
lm_gacu <- lm(total_farmacias   ~  GACU_numeric, data = nivel_gacu_limpio)
barplot(nivel_gacu_limpio$total_farmacias, names.arg = nivel_gacu_limpio$GACU_numeric, data = nivel_gacu,
        xlab = "GACU", ylab = "Farmacias", main = "Número de farmacias por cada nivel GACU",
        abline(lm_gacu, col="blue", lwd=3))
summary(lm(total_farmacias   ~  GACU_numeric, data = nivel_gacu))

reg_tras_gacu <- lm(total_farmacias  ~ GACU_numeric, data = nivel_gacu)
summary(reg_tras_gacu)
plot(nivel_gacu$GACU_numeric, nivel_gacu$num_farmacias, col="red", 
     main = "Número de farmacias con respecto a GACU",
     abline(reg_tras_gacu, col="blue", lwd=3), xlab="GACU",
     ylab="Número de farmacias en la localidad")

ICE <- read_excel("/Users/josemeade/Desktop/Dataton/Archivos/ICE_Localidad_2020.xlsx", sheet = "Resumen", skip = 4)
View(ICE)

base_conj_2 <- merge(farm_loc, ICE, by.x="localidad", by.y="NOM_LOC")
head(base_conj_2)

mas_de_una_farm_2 <- base_conj_2 %>%
  filter(num_farmacias > 0 & num_farmacias<=500) %>%
  filter(IE>1)

reg_tras_IM <- lm(num_farmacias ~ IM_2020, data = mas_de_una_farm_2)
summary(reg_tras_IM)

plot(mas_de_una_farm_2$IM_2020, mas_de_una_farm_2$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al Índice de Marginación de la localidad",
     abline(reg_tras_IM, col="blue", lwd=3), xlab="Índice de Marginación de la localidad",
     ylab="Número de farmacias en la localidad")

reg_tras_IMN <- lm(num_farmacias ~ IMN_2020, data = mas_de_una_farm_2)
summary(reg_tras_IM)

plot(mas_de_una_farm_2$IMN_2020, mas_de_una_farm_2$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al Índice de Marginación Normalizado de la localidad",
     abline(reg_tras_IMN, col="blue", lwd=3), xlab="Índice de Marginación Normalizadode la localidad",
     ylab="Número de farmacias en la localidad")

reg_tras_IM <- lm(num_farmacias ~ IM_2020, data = mas_de_una_farm_2)
summary(reg_tras_IM)

plot(mas_de_una_farm_2$IM_2020, mas_de_una_farm_2$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al Índice de Marginación de la localidad",
     abline(reg_tras_IM, col="blue", lwd=3), xlab="Índice de Marginación de la localidad",
     ylab="Número de farmacias en la localidad")

reg_tras_IE <- lm(num_farmacias ~ IE, data = mas_de_una_farm_2)
summary(reg_tras_IM)

plot(mas_de_una_farm_2$IE, mas_de_una_farm_2$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al Índice de Equipamiento de la localidad",
     abline(reg_tras_IE, col="blue", lwd=3), xlab="Índice de Equipamiento de la localidad",
     ylab="Número de farmacias en la localidad")

reg_tras_ICE <- lm(num_farmacias ~ ICE, data = mas_de_una_farm_2)
summary(reg_tras_IM)

plot(mas_de_una_farm_2$ICE, mas_de_una_farm_2$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al Índice de Calidad del Entorno de la localidad",
     abline(reg_tras_ICE, col="blue", lwd=3), xlab="Índice de Calidad del Entorno de la localidad",
     ylab="Número de farmacias en la localidad")

gasto_hogar <- read_csv("/Users/josemeade/Desktop/Dataton/Archivos/conjunto_de_datos_gastoshogar_enigh2022_ns.csv")
head(gasto_hogar)
cat_entidad <- read_csv("/Users/josemeade/Desktop/Dataton/Archivos/entidad.csv")
head(cat_entidad)

farm_ent <- farmacias %>% group_by(entidad) %>% summarise(num_farmacias = n())

gasto_por_entidad <- gasto_hogar %>%
  group_by(entidad) %>%
  summarise(total_cantidad = sum(cantidad, na.rm = TRUE)) 
print(gasto_por_entidad)

gasto_por_entidad_nom <- merge(gasto_por_entidad, cat_entidad, by = "entidad")
head(gasto_por_entidad_nom)

gasto_farm <- merge(gasto_por_entidad_nom, farm_ent, by.x = "descripcion", by.y = "entidad")

reg_gasto_h <- lm(num_farmacias  ~ total_cantidad, data = gasto_farm)
summary(reg_gasto_h)
plot(gasto_farm$total_cantidad, gasto_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al gasto por hogar total por entidad",
     abline(reg_gasto_h, col="blue", lwd=3), xlab="gasto por hogar total por entidad",
     ylab="Número de farmacias en la entidad")

gasto_persona <- read_csv("/Users/josemeade/Desktop/Dataton/Archivos/conjunto_de_datos_gastospersona_enigh2022_ns.csv")
head(gasto_persona)

gasto_por_entidad_persona <- gasto_persona %>%
  group_by(entidad) %>%
  summarise(total_cantidad = sum(cantidad, na.rm = TRUE)) 
print(gasto_por_entidad)

gasto_por_entidad_nom_per <- merge(gasto_por_entidad_persona, cat_entidad, by = "entidad")
head(gasto_por_entidad_nom_per)

gasto_farm_per <- merge(gasto_por_entidad_nom_per, farm_ent, by.x = "descripcion", by.y = "entidad")

reg_gasto_p <- lm(num_farmacias  ~ total_cantidad, data = gasto_farm_per)
summary(reg_gasto_p)
plot(gasto_farm_per$total_cantidad, gasto_farm_per$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al gasto por persona total por entidad",
     abline(reg_gasto_p, col="blue", lwd=3), xlab="gasto por persona total por entidad",
     ylab="Número de farmacias en la entidad")

ingreso <- read_csv("/Users/josemeade/Desktop/Dataton/Archivos/conjunto_de_datos_ingresos_enigh2022_ns.csv")
head(ingreso)

ingreso_entidad <- ingreso %>%
  group_by(entidad) %>%
  summarise(total_ingreso = sum(ing_tri, na.rm = TRUE)) 
print(ingreso_entidad)

ingreso_entidad_nom <- merge(ingreso_entidad, cat_entidad, by = "entidad")
head(ingreso_entidad_nom)

ingreso_farm <- merge(ingreso_entidad_nom, farm_ent, by.x = "descripcion", by.y = "entidad")
head(ingreso_farm)

reg_ing <- lm(num_farmacias  ~ total_ingreso, data = ingreso_farm)
summary(reg_ing)
plot(ingreso_farm$total_ingreso, ingreso_farm$num_farmacias, col="red", 
     main = "Número de farmacias con respecto al ingreso trimestral por entidad",
     abline(reg_ing, col="blue", lwd=3), xlab="ingreso trimestral total por entidad",
     ylab="Número de farmacias en la entidad")
