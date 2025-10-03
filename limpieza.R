#eligiendo directorio de trabajo control + shift + h
setwd("C:/Users/user/Documents/Articulos/coresistencia")

# leyendo base original  
library(readxl)
base <- read_excel("mi_base.xlsx")
View(base)
names(base)

# Cargamos librerias que vamos a necesitar en esta seccion
library(tidyverse) 
#usaremos library(dplyr) library(stringr)

# sexo --------------------------------------------------------------------


#sexo
summary(base$sexo)
str(base$sexo)
unique(base$sexo)

# Recode F=1/Femenino, M=0/Masculino con etiquetas
base <- base %>%
  mutate(
    sexo = case_when(
      sexo == "F" ~ 1,
      sexo == "M" ~ 0,
      TRUE ~ NA_real_
    ),
    sexo = factor(sexo,
                  levels = c(0, 1),
                  labels = c("Masculino", "Femenino"))
  )

base %>% 
  count(sexo)

#Si lo conviertes a número (as.numeric(sexo) - 1), recuperarás el 0/1.


# edad --------------------------------------------------------------------

summary(base$edad)
str(base$edad)#tiene 76 datos perdidos 
unique(base$edad)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.00   38.25   60.50   55.65   76.00   94.00      76
# leucocitos --------------------------------------------------------------
summary(base$leucocitos)
str(base$leucocitos)
unique(base$leucocitos)


base <- base %>%
  mutate(
    # Paso 1: estandarizar (cambiar " a " por "-", quitar espacios extras)
    leucocitos_clean = str_replace_all(leucocitos, "a", "-"),
    leucocitos_clean = str_replace_all(leucocitos_clean, "\\s+", ""),
    
    # Paso 2: tomar el valor máximo de cada rango
    max_val = as.numeric(str_extract_all(leucocitos_clean, "\\d+", simplify = TRUE)[,2]),
    
    # Paso 3: categorizar según tu criterio
    leucocitos_cat = case_when(
      max_val <= 4              ~ "0 - 4",
      max_val >= 5 & max_val <= 10  ~ "5 - 10",
      max_val >= 11 & max_val <= 25 ~ "11 - 25",
      max_val > 25              ~ ">25",
      TRUE ~ NA_character_
    ),# Lo dejamos como factor ordenado
    leucocitos_cat = factor(leucocitos_cat, 
                            levels = c("0 - 4", "5 - 10", "11 - 25", ">25")))


leuco <- base %>% 
  select(leucocitos, leucocitos_clean, leucocitos_cat, leucocitos_bin)

base %>% 
  count(leucocitos_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)



# leucocitos binarios0/1 -----------------------------------------------------

summary(base$leucocitos_cat)
str(base$leucocitos_cat)
unique(base$leucocitos_cat)


base <- base %>%
  mutate(
    leucocitos_bin = case_when(
      leucocitos_cat == "0 - 4" ~ 0,   # Normal
      TRUE ~ 1                   # Todo lo demás es Piuria
    ),
    leucocitos_bin = factor(leucocitos_bin,
                            levels = c(0, 1),
                            labels = c("Normal", "Piuria"))
  )

base %>% 
  count( leucocitos_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)



# hematies y hematies binarios 0/1 ----------------------------------------------------------------

summary(base$hematies)
str(base$hematies)
unique(base$hematies)

base <- base %>%
  mutate(
    # 1. Normalizar: cualquier cosa que no sea dígito -> "-"
    hematies_clean = str_replace_all(hematies, "[^0-9]+", "-"),
    
    # 2. Eliminar posibles guiones al inicio/final
    hematies_clean = str_replace_all(hematies_clean, "^-|-$", ""),
    
    # 3. Extraer valor máximo del rango
    max_val = as.numeric(str_extract_all(hematies_clean, "\\d+", simplify = TRUE)[,2]),
    
    # 4. Categorías de 4 grupos
    hematies_cat = case_when(
      max_val <= 4              ~ "0 - 4",
      max_val >= 5 & max_val <= 10  ~ "5 - 10",
      max_val >= 11 & max_val <= 50 ~ "11 - 50",
      max_val > 50              ~ ">50",
      TRUE ~ NA_character_
    ),
    hematies_cat = factor(hematies_cat,
                          levels = c("0 - 4", "5 - 10", "11 - 50", ">50")),
    
    # 5. Variable binaria clínica
    hematies_bin = case_when(
      max_val <= 4 ~ 0,   # Normal
      max_val > 4  ~ 1,   # Hematuria
      TRUE ~ NA_real_
    ),
    hematies_bin = factor(hematies_bin,
                          levels = c(0, 1),
                          labels = c("Normal", "Hematuria"))
  )


hema <- base %>% 
  select(hematies, hematies_clean,  max_val, hematies_cat, hematies_bin)

base %>% 
  count( hematies_cat, hematies_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# celulas epiteliales y celulas epiteliales binario   ----------------------------------------------------

summary(base$c_epi)
str(base$c_epi)
unique(base$c_epi)


base <- base %>%
  mutate(
    # 1. Normalizar: todo lo que no sea dígito -> "-"
    c_epi_clean = str_replace_all(c_epi, "[^0-9]+", "-"),
    # 2. Quitar guiones sobrantes al inicio/final
    c_epi_clean = str_replace_all(c_epi_clean, "^-|-$", ""),
    
    # 3. Extraer valor máximo del rango
    max_val = as.numeric(str_extract_all(c_epi_clean, "\\d+", simplify = TRUE)[,2]),
    
    # 4. Categorizar en 3 grupos
    c_epi_cat = case_when(
      max_val <= 5              ~ "0 - 5",
      max_val >= 6 & max_val <= 10 ~ "6 - 10",
      max_val > 10              ~ ">10",
      TRUE ~ NA_character_
    ),
    c_epi_cat = factor(c_epi_cat,
                       levels = c("0 - 5", "6 - 10", ">10")),
    
    # 5. Variable binaria clínica
    c_epi_bin = case_when(
      max_val <= 10 ~ 0,   # Muestra adecuada
      max_val > 10  ~ 1,   # Contaminada
      TRUE ~ NA_real_
    ),
    c_epi_bin = factor(c_epi_bin,
                       levels = c(0, 1),
                       labels = c("Adecuada", "Contaminada"))
  )

#verificamos con la funcion select
cepite <- base %>% 
  select(c_epi, c_epi_clean, max_val, c_epi_cat, c_epi_bin)

#contamos
base %>% 
  count( c_epi_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# bacterias y carga bacteriana --------------------------------------------------------------

summary(base$bacterias)
str(base$bacterias)
unique(base$bacterias)


base <- base %>%
  mutate(
    # 1. Nueva variable agrupada
    bacterias_carga = case_when(
      bacterias %in% c("escasas", "moderadas") ~ "Baja",
      bacterias %in% c("regular", "abundantes") ~ "Alta",
      TRUE ~ NA_character_
    ),
    bacterias_carga = factor(bacterias_carga, 
                             levels = c("Baja", "Alta")),  # nivel ref = "Baja"
    
    # 2. Ajustar la variable madre con ref "escasas"
    bacterias = factor(bacterias,
                       levels = c("escasas", "moderadas", "regular", "abundantes"))
  )

base %>% 
  count(bacterias_carga)%>% 
  mutate(porcentaje=n/sum(n)*100)





# piocitos  ---------------------------------------------------------------

summary(base$piocitos)
str(base$piocitos)
unique(base$piocitos)

base <- base %>%
  mutate(
    piocitos = if_else(is.na(piocitos), "no", piocitos),
    piocitos = factor(piocitos, levels = c("no", "si"))  # ref = "no"
  )

base %>% 
  count(piocitos)%>% 
  mutate(porcentaje=n/sum(n)*100)


# cristales ---------------------------------------------------------------

summary(base$cristales)
str(base$cristales)
unique(base$cristales)


base <- base %>%
  mutate(
    cristales_cat = case_when(
      is.na(cristales) | cristales == "0" ~ "Ausentes",
      cristales == "1" | str_detect(str_to_lower(cristales), "au") ~ "Ácido úrico",
      cristales == "2" | str_detect(str_to_lower(cristales), "biurato") ~ "Biurato NH4",
      cristales == "3" | str_detect(str_to_lower(cristales), "ft") ~ "Fosfato triple",
      cristales == "4" | str_detect(str_to_lower(cristales), "fa") ~ "Fosfatos amorfos",
      cristales == "5" | str_detect(str_to_lower(cristales), "oxc|oxca|oxaca|oxa ca|oxalato") ~ "Oxalato Ca+",
      str_detect(str_to_lower(cristales), "ua") ~ "Urat. amorfos",
      TRUE ~ "Otros"
    )
  )

crist <- base %>% 
  select(cristales, cristales_cat)

#verificantes variable creada
unique(base$cristales_cat)

base %>% 
  count(cristales_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


# filamento mucoide -------------------------------------------------------

#filamento mucoide 
summary(base$filamento_mucoide)
str(base$filamento_mucoide)
unique(base$filamento_mucoide)


base <- base %>%
  mutate(
    filamento_mucoide_bin = case_when(
      is.na(filamento_mucoide) | filamento_mucoide == "0" ~ "Ausente",
      TRUE ~ "Presente"
    )
  )

#verificantes variable creada
fm <- base %>% 
  select(filamento_mucoide, filamento_mucoide_bin)
unique(base$filamento_mucoide_bin)


base %>% 
  count(filamento_mucoide_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)




# aislamiento -------------------------------------------------------------

summary(base$aislamiento)
str(base$aislamiento)
unique(base$aislamiento)

base %>% 
  count(aislamiento)%>% 
  mutate(porcentaje=n/sum(n)*100)


# recuento_ufc_ml ---------------------------------------------------------

summary(base$recuento_ufc_ml)
str(base$recuento_ufc_ml)
unique(base$recuento_ufc_ml)

base <- base %>%
  mutate(
    # Primero paso todo a número
    recuento_num = case_when(
      recuento_ufc_ml == "<10^4" ~ 9999,
      recuento_ufc_ml == "10^4-10^5" ~ 50000,
      recuento_ufc_ml == ">10^5" ~ 100001,
      TRUE ~ suppressWarnings(as.numeric(gsub("[^0-9]", "", recuento_ufc_ml)))
    ),
    
    # Clasificación clínica
    recuento_cat = case_when(
      recuento_num < 1000 ~ "<10^3 UFC/ml",
      recuento_num >= 1000 & recuento_num < 100000 ~ "10^3–10^4 UFC/ml",
      recuento_num >= 100000 ~ "≥10^5 UFC/ml",
      TRUE ~ NA_character_
    )
  )



#verificantes variable creada
ufc <- base %>% 
  select(recuento_ufc_ml, recuento_num, recuento_cat)


unique(base$recuento_cat)


base %>% 
  count(recuento_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


#ahora hare una variable binaria 

base <- base %>%
  mutate(
    recuento_bin = case_when(
      recuento_cat == "≥10^5 UFC/ml" ~ 1,
      recuento_cat %in% c("<10^3 UFC/ml", "10^3–10^4 UFC/ml", "10^4–10^5 UFC/ml") ~ 0,
      TRUE ~ NA_real_
    ),
    recuento_bin = factor(
      recuento_bin,
      levels = c(0, 1),
      labels = c("<10^5 UFC/ml", "≥10^5 UFC/ml")
    )
  )


unique(base$recuento_bin)


base %>% 
  count(recuento_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# fenotipo ----------------------------------------------------------------

summary(base$fenotipo)
str(base$fenotipo)
unique(base$fenotipo)



base <- base %>%
  mutate(
    fenotipo1 = case_when(
      fenotipo %in% c("blee-", NA) ~ "Ninguno",
      fenotipo %in% c("blee+", "blee") ~ "BLEE",
      fenotipo == "ampc" ~ "AmpC",
      TRUE ~ fenotipo
    )
  )

fenot <- base %>% 
  select(fenotipo, fenotipo1, aislamiento) #%>% 
  #filter(aislamiento== "eco")


unique(base$fenotipo1)


base %>% 
  count(fenotipo1)%>% 
  mutate(porcentaje=n/sum(n)*100)



# nitritos ----------------------------------------------------------------
summary(base$nitritos)
str(base$nitritos)
unique(base$nitritos)


base <- base %>%
  mutate(
    nitritos = case_when(
      is.na(nitritos) ~ "negativo",
      TRUE ~ nitritos
    )
  )

base %>% 
  count(nitritos)%>% 
  mutate(porcentaje=n/sum(n)*100)


# esterasa ----------------------------------------------------------------
summary(base$esterasa)
str(base$esterasa)
unique(base$esterasa)


base <- base %>%
  mutate(
    # Reemplazo NA por "negativo"
    esterasa = case_when(
      is.na(esterasa) ~ "negativo",
      TRUE ~ esterasa
    ),
    
    # Versión binaria
    esterasa_bin = case_when(
      esterasa == "negativo" ~ 0,
      esterasa %in% c("1+", "2+", "3+") ~ 1,
      TRUE ~ NA_real_
    )
  )

  #conteo
base %>% 
  count(esterasa_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)



# cetonas -----------------------------------------------------------------

summary(base$cetonas)
str(base$cetonas)
unique(base$cetonas)



base <- base %>%
  mutate(
    # Reemplazo 0 y NA por "negativo"
    cetonas = case_when(
      cetonas == "0" | is.na(cetonas) ~ "negativo",
      TRUE ~ cetonas
    ),
    
    # Versión binaria
    cetonas_bin = case_when(
      cetonas == "negativo" ~ 0,
      cetonas %in% c("1+", "2+", "3+") ~ 1,
      TRUE ~ NA_real_))

#conteo
base %>% 
  count(cetonas_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# sangre ------------------------------------------------------------------

summary(base$sangre)
str(base$sangre)
unique(base$sangre)



base <- base %>%
  mutate(
    # Reemplazo negativo y NA por "negativo"
    sangre = case_when(
      sangre == "negativo" | is.na(sangre) ~ "negativo",
      TRUE ~ sangre
    ),
    
    # Versión binaria
    sangre_bin = case_when(
      sangre == "negativo" ~ 0,
      sangre %in% c("1+", "2+", "3+", "trazas") ~ 1,
      TRUE ~ NA_real_
    )
  )


    #conteo
base %>% 
  count(sangre_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# glucosa -----------------------------------------------------------------

summary(base$glucosa)
str(base$glucosa)
unique(base$glucosa)


base <- base %>%
  mutate(
    # Limpieza: negativo y NA a "negativo"
    glucosa = case_when(
      glucosa == "negativo" | is.na(glucosa) ~ "negativo",
      TRUE ~ glucosa
    ),
    
    # Versión numérica: paso a número, negativo = 0
    glucosa_num = case_when(
      glucosa == "negativo" ~ 0,
      TRUE ~ suppressWarnings(as.numeric(glucosa))
    ),
    
    # Versión binaria: negativo = 0, positivo = 1
    glucosa_bin = case_when(
      glucosa == "negativo" ~ 0,
      glucosa_num > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(glucosa_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)



# proteinas ---------------------------------------------------------------

summary(base$proteinas)
str(base$proteinas)
unique(base$proteinas)



base <- base %>%
  mutate(
    # Limpieza: negativo y NA -> "negativo"
    proteinas = case_when(
      proteinas == "negativo" | is.na(proteinas) ~ "negativo",
      TRUE ~ proteinas
    ),
    
    # Versión numérica: paso a número, negativo=0, trazas=15
    proteinas_num = case_when(
      proteinas == "negativo" ~ 0,
      proteinas == "trazas" ~ 15,
      TRUE ~ suppressWarnings(as.numeric(proteinas))
    ),
    
    # Versión binaria: negativo=0, trazas y números positivos=1
    proteinas_bin = case_when(
      proteinas == "negativo" ~ 0,
      proteinas_num > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )



#conteo
base %>% 
  count(proteinas_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


#***********##ANTIBIOTICOS



# amc ---------------------------------------------------------------------
summary(base$amc)
str(base$amc)
unique(base$amc)

# tengo estas variables aislamiento y amc que tienen estos valores 
# 
# unique(base$aislamiento)
# [1] "eco"     "proteus" "kecs"    "pae" 
# 
# unique(base$amc)
# [1] NA     "R"    "S"    "17"   "15"   "8"    "21"  
# [8] "11.5" "20"   "18"   "0"    "30.5" "30"   "14.5"
# [15] "23"   "6"    "10"   "22"   "25.5" "14"   "17.5"
# [22] "9.5"  "24"   "19"   "16"   "35"   "19.5" "13"  
# [29] "16.5" "32"   "25"   "18.5" "11"   "13.5" "26"  
# [36] "28"   "34"
# 
# y necesito que me crees una nueva, donde se cumpla 
# ≥ 18 y S es S
# 14–17 es I
# ≤ 13 y R es R  para  aislamiento diferente de pae, 
# 
# y donde hay NA que quede en NA


base <- base %>%
  mutate(
    # Versión numérica del halo
    amc_num = suppressWarnings(as.numeric(amc)),
    
    # Clasificación categórica
    amc_clas = case_when(
      is.na(amc) ~ NA_character_,
      amc %in% c("S", "R") ~ amc,
      aislamiento != "pae" & amc_num >= 18 ~ "S",
      aislamiento != "pae" & amc_num >= 14 & amc_num <= 17 ~ "I",
      aislamiento != "pae" & amc_num <= 13 ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Versión binaria: S = 0, I/R = 1, NA se mantiene
    amc_bin = case_when(
      is.na(amc_clas) ~ NA_real_,
      amc_clas == "S" ~ 0,
      amc_clas %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )


#revisamos

amccvista <- base %>% 
  select(amc, amc_num, amc_clas, amc_bin) 

#conteo
base %>% 
  count(amc_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)

str(base$amc_clas)
unique(base$amc_bin)







# tzp ---------------------------------------------------------------------

unique(base$tzp)
unique(base$aislamiento)

# Perfecto 🙌, para tzp tenemos dos reglas distintas según el aislamiento:
#   
#   eco, proteus, kecs (≠ pae)
# 
# ≥ 25 → S
# 
# 21–24 → SDD
# 
# ≤ 20 o "R" → R
# 
# pae
# 
# ≥ 22 → S
# 
# 18–21 → I
# 
# ≤ 17 o "R" → R
# 
# Y los NA se conservan como NA.
# Después creamos la versión binaria (tzp_bin) con la misma lógica que usamos antes: S = 0, todo lo demás (SDD, I, R) = 1, y NA se queda NA.

base <- base %>%
  mutate(
    # Convertir a número cuando sea posible
    tzp_num = suppressWarnings(as.numeric(tzp)),
    
    # Clasificación categórica
    tzp_clas = case_when(
      is.na(tzp) ~ NA_character_,
      aislamiento != "pae" & tzp %in% c("S", "R") ~ tzp,
      aislamiento != "pae" & tzp_num >= 25 ~ "S",
      aislamiento != "pae" & tzp_num >= 21 & tzp_num <= 24 ~ "SDD",
      aislamiento != "pae" & tzp_num <= 20 ~ "R",
      
      aislamiento == "pae" & tzp %in% c("S", "R") ~ tzp,
      aislamiento == "pae" & tzp_num >= 22 ~ "S",
      aislamiento == "pae" & tzp_num >= 18 & tzp_num <= 21 ~ "I",
      aislamiento == "pae" & tzp_num <= 17 ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria: S = 0, todo lo demás = 1, NA se mantiene
    tzp_bin = case_when(
      is.na(tzp_clas) ~ NA_real_,
      tzp_clas %in% c("S", "SDD") ~ 0,
      tzp_clas %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )


#revisamos

tzpvista <- base %>% 
  select(tzp, tzp_clas, tzp_bin, aislamiento) 

#conteo
base %>% 
  count(tzp_clas)%>% 
  mutate(porcentaje=n/sum(n)*100)

str(base$tzp_clas)
unique(base$tzp_bin)


# cxm ---------------------------------------------------------------------

unique(base$cxm)
unique(base$aislamiento)

# Para cxm (Cefuroxima), aplicamos la regla solo a los aislamientos diferentes de pae (en pae normalmente no se prueba este antibiótico, así que quedará en NA).
# 
# Las reglas:
#   
#   ≥ 23 o "S" → S
# 
# 15–22 → I
# 
# ≤ 14 o "R" → R
# 
# NA → NA
# 
# Después hacemos la versión binaria:
#   
#   S = 0
# 
# I o R = 1
# 
# NA se mantiene.


base <- base %>%
  mutate(
    # Convertir cxm a número cuando se pueda
    cxm_num = suppressWarnings(as.numeric(cxm)),
    
    # Clasificación categórica
    cxm_clas = case_when(
      is.na(cxm) ~ NA_character_,
      
      aislamiento != "pae" & cxm %in% c("S", "R") ~ cxm,
      aislamiento != "pae" & cxm_num >= 23 ~ "S",
      aislamiento != "pae" & cxm_num >= 15 & cxm_num <= 22 ~ "I",
      aislamiento != "pae" & cxm_num <= 14 ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria: S = 0, lo demás = 1
    cxm_bin = case_when(
      is.na(cxm_clas) ~ NA_real_,
      cxm_clas == "S" ~ 0,
      cxm_clas %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#revisamos

cxmvista <- base %>% 
  select(cxm, cxm_clas, cxm_bin, aislamiento) 

#conteo
base %>% 
  count(cxm_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)

str(base$cxm_clas)
unique(base$cxm_bin)


# ctx ---------------------------------------------------------------------
str(base$ctx)
unique(base$ctx)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm para ctx
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, ctx)

#categorizamos sio y hacemos la version binaria s=0 

base <- base %>%
  mutate(
    # Paso 1: convertir a número cuando sea posible
    ctx_num = suppressWarnings(as.numeric(ctx)),
    
    # Paso 2: clasificación categórica
    ctx_cat = case_when(
      ctx == "S" | (!is.na(ctx_num) & ctx_num >= 26) ~ "S",
      !is.na(ctx_num) & ctx_num >= 23 & ctx_num <= 25 ~ "I",
      ctx == "R" | (!is.na(ctx_num) & ctx_num <= 22) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria
    ctx_bin = case_when(
      ctx_cat == "S" ~ 0,
      ctx_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#revisamos

ctxvista <- base %>% 
  select(ctx, ctx_cat, ctx_bin, aislamiento) 

#conteo
base %>% 
  count(ctx_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


# cro ---------------------------------------------------------------------

str(base$cro)
unique(base$cro)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm para cro
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, cro)

#categorizamos y creamos la version de cro binaria S=0

base <- base %>%
  mutate(
    # Paso 1: convertir a número cuando sea posible
    cro_num = suppressWarnings(as.numeric(cro)),
    
    # Paso 2: clasificación categórica
    cro_cat = case_when(
      cro == "S" | (!is.na(cro_num) & cro_num >= 23) ~ "S",
      !is.na(cro_num) & cro_num >= 20 & cro_num <= 22 ~ "I",
      cro == "R" | (!is.na(cro_num) & cro_num <= 19) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria
    cro_bin = case_when(
      cro_cat == "S" ~ 0,
      cro_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )


#revisamos

crovista <- base %>% 
  select(cro, cro_cat, cro_bin, aislamiento) 

#conteo
base %>% 
  count(cro_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


# caz ---------------------------------------------------------------------

str(base$caz)
unique(base$caz)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm para cro
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, caz)

#categorizamos 


base <- base %>%
  mutate(
    # Paso 1: convertir valores a numéricos donde se pueda
    caz_num = suppressWarnings(as.numeric(caz)),
    
    # Paso 2: clasificación categórica con condiciones distintas para pae
    caz_cat = case_when(
      # Aislamientos ≠ pae
      aislamiento != "pae" & (caz == "S" | (!is.na(caz_num) & caz_num >= 21)) ~ "S",
      aislamiento != "pae" & (!is.na(caz_num) & caz_num >= 18 & caz_num <= 20) ~ "I",
      aislamiento != "pae" & (caz == "R" | (!is.na(caz_num) & caz_num <= 17)) ~ "R",
      
      # Aislamientos = pae
      aislamiento == "pae" & (caz == "S" | (!is.na(caz_num) & caz_num >= 18)) ~ "S",
      aislamiento == "pae" & (!is.na(caz_num) & caz_num >= 15 & caz_num <= 17) ~ "I",
      aislamiento == "pae" & (caz == "R" | (!is.na(caz_num) & caz_num <= 14)) ~ "R",
      
      # NA se conserva
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria (S = 0, I/R = 1, NA intacto)
    caz_bin = case_when(
      caz_cat == "S" ~ 0,
      caz_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#revisamos

cazvista <- base %>% 
  select(caz, caz_cat, caz_bin, aislamiento) 

#conteo
base %>% 
  count(caz_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)



# fep ---------------------------------------------------------------------
str(base$fep)
unique(base$fep)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, fep)

#categorizamos 

base <- base %>%
  mutate(
    # Paso 1: intentar convertir a numérico donde se pueda
    fep_num = suppressWarnings(as.numeric(fep)),
    
    # Paso 2: clasificación categórica con reglas distintas para pae
    fep_cat = case_when(
      # Aislamientos ≠ pae
      aislamiento != "pae" & (fep == "S" | (!is.na(fep_num) & fep_num >= 25)) ~ "S",
      aislamiento != "pae" & (!is.na(fep_num) & fep_num >= 19 & fep_num <= 24) ~ "I",
      aislamiento != "pae" & (fep == "R" | (!is.na(fep_num) & fep_num <= 18)) ~ "R",
      
      # Aislamientos = pae
      aislamiento == "pae" & (fep == "S" | (!is.na(fep_num) & fep_num >= 18)) ~ "S",
      aislamiento == "pae" & (!is.na(fep_num) & fep_num >= 15 & fep_num <= 17) ~ "I",
      aislamiento == "pae" & (fep == "R" | (!is.na(fep_num) & fep_num <= 14)) ~ "R",
      
      # NA se mantiene
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria (S = 0, I o R = 1, NA intacto)
    fep_bin = case_when(
      fep_cat == "S" ~ 0,
      fep_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )


#revisamos

fepvista <- base %>% 
  select(fep, fep_cat, fep_bin, aislamiento) 

#conteo
base %>% 
  count(fep_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)

#verificamos que pae  tenga lecturas de halos e interpretacion validos 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, fep_num, fep_cat, fep_bin)


# atm ---------------------------------------------------------------------

str(base$atm)
unique(base$atm)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, atm)

#categorizamos 

base <- base %>%
  mutate(
    # Paso 1: intentar convertir a numérico donde se pueda
    atm_num = suppressWarnings(as.numeric(atm)),
    
    # Paso 2: clasificación categórica con reglas distintas para pae y no-pae
    atm_cat = case_when(
      # Aislamientos ≠ pae
      aislamiento != "pae" & (atm == "S" | (!is.na(atm_num) & atm_num >= 21)) ~ "S",
      aislamiento != "pae" & (!is.na(atm_num) & atm_num >= 18 & atm_num <= 20) ~ "I",
      aislamiento != "pae" & (atm == "R" | (!is.na(atm_num) & atm_num <= 17)) ~ "R",
      
      # Aislamientos = pae
      aislamiento == "pae" & (atm == "S" | (!is.na(atm_num) & atm_num >= 22)) ~ "S",
      aislamiento == "pae" & (!is.na(atm_num) & atm_num >= 16 & atm_num <= 21) ~ "I",
      aislamiento == "pae" & (atm == "R" | (!is.na(atm_num) & atm_num <= 15)) ~ "R",
      
      # NA se mantiene
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria (S = 0, I o R = 1, NA intacto)
    atm_bin = case_when(
      atm_cat == "S" ~ 0,
      atm_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )


#conteo
base %>% 
  count(atm_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)

#verificamos que pae  tenga lecturas de halos e interpretacion validos 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, atm_num, atm_cat, atm_bin)


# mem ---------------------------------------------------------------------

str(base$mem)
unique(base$mem)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, mem)

#categorizamos 

base <- base %>%
  mutate(
    # Convertimos a numérico si es posible
    mem_num = suppressWarnings(as.numeric(mem)),
    
    # Nueva variable categórica S/I/R
    mem_cat = case_when(
      aislamiento != "pae" & (mem == "S" | mem_num >= 23) ~ "S",
      aislamiento != "pae" & mem_num %in% 20:22           ~ "I",
      aislamiento != "pae" & (mem == "R" | mem_num <= 19) ~ "R",
      
      aislamiento == "pae" & (mem == "S" | mem_num >= 19) ~ "S",
      aislamiento == "pae" & mem_num %in% 16:18           ~ "I",
      aislamiento == "pae" & (mem == "R" | mem_num <= 15) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    mem_bin = case_when(
      mem_cat == "S" ~ 0,
      mem_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(mem_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)

#verificamos que pae  tenga lecturas de halos e interpretacion validos 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, mem_num, mem_cat, mem_bin)


# imp ---------------------------------------------------------------------

str(base$imp)
unique(base$imp)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, imp)

#categorizamos 

base <- base %>%
  mutate(
    # Convertimos a numérico si es posible
    imp_num = suppressWarnings(as.numeric(imp)),
    
    # Nueva variable categórica S/I/R
    imp_cat = case_when(
      aislamiento != "pae" & (imp == "S" | imp_num >= 23) ~ "S",
      aislamiento != "pae" & imp_num %in% 20:22           ~ "I",
      aislamiento != "pae" & (imp == "R" | imp_num <= 19) ~ "R",
      
      aislamiento == "pae" & (imp == "S" | imp_num >= 19) ~ "S",
      aislamiento == "pae" & imp_num %in% 16:18           ~ "I",
      aislamiento == "pae" & (imp == "R" | imp_num <= 15) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    imp_bin = case_when(
      imp_cat == "S" ~ 0,
      imp_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(imp_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)

#verificamos que pae  tenga lecturas de halos e interpretacion validos 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, imp_num, imp_cat, imp_bin)

impvista <- base %>% 
  select(imp, imp_cat, imp_bin, aislamiento) 



# ert ---------------------------------------------------------------------
str(base$ert)
unique(base$ert)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, ert)

#categorizamos 

base <- base %>%
  mutate(
    # Paso 1: convertir a numérico (los "S" o "R" quedan NA)
    ert_num = suppressWarnings(as.numeric(ert)),
    
    # Paso 2: crear variable categórica
    ert_cat = case_when(
      ert == "S" | ert_num >= 22            ~ "S",
      ert_num %in% 19:21                    ~ "I",
      ert == "R" | (!is.na(ert_num) & ert_num <= 18) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Paso 3: versión binaria
    ert_bin = case_when(
      ert_cat == "S"            ~ 0,
      ert_cat %in% c("I", "R")  ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(ert_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)



ertvista <- base %>% 
  select(ert, ert_cat, ert_bin, aislamiento) 



# gen ---------------------------------------------------------------------

str(base$gen)
unique(base$gen)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, gen)

#categorizamos 

base <- base %>%
  mutate(
    # Convertir a número (los "S"/"R" quedan como NA)
    gen_num = suppressWarnings(as.numeric(gen)),
    
    # Clasificación según reglas
    gen_cat = case_when(
      aislamiento == "pae" ~ NA_character_,  # Si es pae → todo NA
      
      aislamiento != "pae" & (gen == "S" | gen_num >= 18) ~ "S",
      aislamiento != "pae" & gen_num %in% 15:17           ~ "I",
      aislamiento != "pae" & (gen == "R" | (!is.na(gen_num) & gen_num <= 14)) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    gen_bin = case_when(
      gen_cat == "S" ~ 0,
      gen_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(gen_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


#verificamos que pae no tenga lecturas de halos en mm para gen en orinas ya no se usa clsi 2025
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, gen_num, gen_cat, gen_bin)



# amk ---------------------------------------------------------------------
str(base$amk)
unique(base$amk)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, amk)

#categorizamos segun tipo de aislamiento

base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" quedan NA)
    amk_num = suppressWarnings(as.numeric(amk)),
    
    # Clasificación categórica
    amk_cat = case_when(
      # --- No PAE ---
      aislamiento != "pae" & (amk == "S" | amk_num >= 20) ~ "S",
      aislamiento != "pae" & amk_num %in% 17:19           ~ "I",
      aislamiento != "pae" & (amk == "R" | (!is.na(amk_num) & amk_num <= 16)) ~ "R",
      
      # --- PAE ---
      aislamiento == "pae" & (amk == "S" | amk_num >= 17) ~ "S",
      aislamiento == "pae" & amk_num %in% 15:16           ~ "I",
      aislamiento == "pae" & (amk == "R" | (!is.na(amk_num) & amk_num <= 14)) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    amk_bin = case_when(
      amk_cat == "S" ~ 0,
      amk_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(amk_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)


#verificamos en pae
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, amk_num, amk_cat, amk_bin)



# cip ---------------------------------------------------------------------
str(base$cip)
unique(base$cip)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, cip)

#categorizamos


base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" quedan como NA)
    cip_num = suppressWarnings(as.numeric(cip)),
    
    # Clasificación categórica
    cip_cat = case_when(
      # --- No PAE ---
      aislamiento != "pae" & (cip == "S" | cip_num >= 26) ~ "S",
      aislamiento != "pae" & cip_num %in% 22:25           ~ "I",
      aislamiento != "pae" & (cip == "R" | (!is.na(cip_num) & cip_num <= 21)) ~ "R",
      
      # --- PAE ---
      aislamiento == "pae" & (cip == "S" | cip_num >= 25) ~ "S",
      aislamiento == "pae" & cip_num %in% 19:24           ~ "I",
      aislamiento == "pae" & (cip == "R" | (!is.na(cip_num) & cip_num <= 18)) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    cip_bin = case_when(
      cip_cat == "S" ~ 0,
      cip_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(cip_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


#verificamos en pae
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, cip_num, cip_cat, cip_bin)


# lev ---------------------------------------------------------------------

str(base$lev)
unique(base$lev)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, lev)

#categorizamos 

base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" quedan NA)
    lev_num = suppressWarnings(as.numeric(lev)),
    
    # Clasificación categórica
    lev_cat = case_when(
      # --- No PAE ---
      aislamiento != "pae" & (lev == "S" | lev_num >= 21) ~ "S",
      aislamiento != "pae" & lev_num %in% 17:20           ~ "I",
      aislamiento != "pae" & (lev == "R" | (!is.na(lev_num) & lev_num <= 16)) ~ "R",
      
      # --- PAE ---
      aislamiento == "pae" & (lev == "S" | lev_num >= 22) ~ "S",
      aislamiento == "pae" & lev_num %in% 15:21           ~ "I",
      aislamiento == "pae" & (lev == "R" | (!is.na(lev_num) & lev_num <= 14)) ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    lev_bin = case_when(
      lev_cat == "S" ~ 0,
      lev_cat %in% c("I","R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(lev_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)


#verificamos en pae
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, lev_num, lev_cat, lev_bin)


# stx ---------------------------------------------------------------------

str(base$stx)
unique(base$stx)
unique(base$aislamiento)

#verificamos que pae no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, stx)

#categorizamos

base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" quedan NA)
    stx_num = suppressWarnings(as.numeric(stx)),
    
    # Clasificación categórica
    stx_cat = case_when(
      stx == "S" | stx_num >= 16 ~ "S",
      stx_num %in% 11:15         ~ "I",
      stx == "R" | (!is.na(stx_num) & stx_num <= 10) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    stx_bin = case_when(
      stx_cat == "S" ~ 0,
      stx_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(stx_cat)%>% 
  mutate(porcentaje=n/sum(n)*100)



# fos ---------------------------------------------------------------------

str(base$fos)
unique(base$fos)
unique(base$aislamiento)

#verificamos que otro aislamiento diferente de eco no tenga lecturas de halos en mm 
base %>%
  filter(aislamiento == "kecs") %>%
  select(aislamiento, fos)

#categorizamos 

base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" quedarían NA si aparecieran)
    fos_num = suppressWarnings(as.numeric(fos)),
    
    # Clasificación categórica
    fos_cat = case_when(
      aislamiento == "eco" & (fos == "S" | fos_num >= 16) ~ "S",
      aislamiento == "eco" & fos_num %in% 13:15           ~ "I",
      aislamiento == "eco" & (fos == "R" | (!is.na(fos_num) & fos_num <= 12)) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    fos_bin = case_when(
      fos_cat == "S" ~ 0,
      fos_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(fos_bin)%>% 
  mutate(porcentaje=n/sum(n)*100)

#revisamos

fosvista <- base %>% 
  select(fos, fos_num, fos_cat, fos_bin, aislamiento) %>% 
filter(aislamiento == "eco")


# nit ---------------------------------------------------------------------
str(base$nit)
unique(base$nit)
unique(base$aislamiento)

#verificamos  halos en mm en pae
base %>%
  filter(aislamiento == "pae") %>%
  select(aislamiento, nit)

#categorizamos

base <- base %>%
  mutate(
    # Convertir a numérico (los "S"/"R" se vuelven NA)
    nit_num = suppressWarnings(as.numeric(nit)),
    
    # Clasificación categórica
    nit_cat = case_when(
      aislamiento %in% c("eco", "kecs") & (nit == "S" | nit_num >= 17) ~ "S",
      aislamiento %in% c("eco", "kecs") & nit_num %in% 15:16           ~ "I",
      aislamiento %in% c("eco", "kecs") & (nit == "R" | (!is.na(nit_num) & nit_num <= 14)) ~ "R",
      TRUE ~ NA_character_
    ),
    
    # Versión binaria
    nit_bin = case_when(
      nit_cat == "S" ~ 0,
      nit_cat %in% c("I", "R") ~ 1,
      TRUE ~ NA_real_
    )
  )

#conteo
base %>% 
  count(nit_cat)%>% 
  mutate(porcentaje=n/sum(n)*100) 

#revisamos

nitvista <- base %>% 
  select(nit, nit_num, nit_cat, nit_bin, aislamiento) %>% 
  filter(aislamiento == "eco", !is.na(nit_bin))
  
