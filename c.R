library(dplyr)
setwd("C:/Users/user/Documents/Articulos/coresistencia")
library(readxl)
base <- read_excel("Libro1.xlsx")

View(base)
names(base)

#sexo
summary(base$sexo)
unique(base$sexo)

base <- base %>%
  mutate(sexo = ifelse(sexo == "0", "F",
                       ifelse(sexo == "1", "M", NA))) %>%
  filter(!is.na(sexo))

#edad
summary(base$edad)
unique(base$edad)
base$edad <- as.numeric(base$edad)

# leucocitos
summary(base$leucocitos)
unique(base$leucocitos)

base$leucocitos <- recode(base$leucocitos,
                          "0" = "0-5",
                          "1" = "5-10",
                          "2" = "10-20",
                          "3" = "20-30",
                          "4" = "30-40",
                          "5" = "40-50",
                          "6" = "50-99",
                          "7" = "80-100")

# hematies
summary(base$hematies)
unique(base$hematies)

base$hematies <- recode(base$hematies,
                        "0" = "0-5",
                        "1" = "5-10",
                        "2" = "10-20",
                        "3" = "20-30",
                        "4" = "30-40",
                        "5" = "40-50",
                        "6" = "50-99")

# c_epi

summary(base$c_epi)
unique(base$c_epi)

base$c_epi <- recode(base$c_epi,
                     "0" = "0-5",
                     "1" = "5-10",
                     "2" = "10-20",
                     "3" = "20-30")

# bacterias
summary(base$bacterias)
unique(base$bacterias)
base$bacterias <- recode(base$bacterias,
                         "0" = "escasas",
                         "1" = "moderadas",
                         "2" = "regular",
                         "3" = "abundantes")

#piocitos
unique(base$piocitos)
base$piocitos <- recode(base$piocitos,
                         "0" = "no",
                         "1" = "si")
# eliminando en NA 
base <- base[!is.na(base$piocitos), ]

# filamento_mucoide
unique(base$filamento_mucoide)

# aislamiento
unique(base$aislamiento)

base <- base %>%
  filter(!aislamiento %in% c(0,5,6)) %>%
  mutate(aislamiento = case_when(
    aislamiento == 1 ~ "eco",
    aislamiento == 4 ~ "proteus"
  ))

# recuento_ufc_ml
unique(base$recuento_ufc_ml)

base <- base %>%
  filter(recuento_ufc_ml != "0") %>%
  mutate(recuento_ufc_ml = case_when(
    recuento_ufc_ml == "1" ~ "<10^4",
    recuento_ufc_ml == "2" ~ "10^4-10^5",
    recuento_ufc_ml == "3" ~ ">10^5"
  ))


# fenotipo
unique(base$fenotipo)

base <- base %>%
  mutate(fenotipo = case_when(
    fenotipo == "1" ~ "blee+",
    fenotipo == "0" ~ "blee-"
  ))

# amc
unique(base$amc)


# nitritos
unique(base$nitritos)

base <- base %>%
  mutate(nitritos = case_when(
    nitritos == "1" ~ "positivo",
    nitritos == "0" ~ "negativo"
  ))

# esterasa
unique(base$esterasa)

base <- base %>%
  mutate(esterasa = case_when(
    esterasa == 1 ~ "1+",
    esterasa == 0 ~ "negativo"
  ))

# cetonas
unique(base$cetonas)

# sangre
unique(base$sangre)
base <- base %>%
  mutate(sangre = case_when(
    sangre == "0" ~ "negativo",
    sangre == "1" ~ "1+",
    sangre == "2" ~ "2+",
    sangre == "3" ~ "3+"
  ))

# glucosa
unique(base$glucosa)

base <- base %>%
  mutate(glucosa = case_when(
    glucosa == "0" ~ "negativo",
    glucosa == "1" ~ "50",
    glucosa == "2" ~ "150"
  ))

# proteinas
unique(base$proteinas)

base <- base %>%
  mutate(proteinas = case_when(
    proteinas == "0" ~ "negativo",
    proteinas == "2" ~ "100",
    proteinas == "3" ~ "500"
  ))


# guardar mi base tratada 

library(writexl)

# Guardar en Excel
write_xlsx(base, "mi_base.xlsx")

