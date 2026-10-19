library(readxl)
library(fpp3)
library(tidyverse)
library(here)

#PRUEBA DE VINCULACIÓN CON GITHUB 

base <- read_xlsx(here("DATA", "Datos_petroleo.xlsx"))

# convertir a data frame 
base <- as.data.frame(base)
class(base)

names(base) <- c("fecha", "precio_cierre", "apertura", "maximo",
                 "minimo", "volumen", "var_pct")

# modificar para correcta lectura 
base_t <- base |>
  mutate(
    fecha = ymd(fecha),
    multiplicador = case_when(
      str_detect(volumen, "K$") ~ 1e3,
      str_detect(volumen, "M$") ~ 1e6
    ),
    volumen = parse_number(volumen, locale = locale(decimal_mark = ",")) * multiplicador
  ) |>
  select(-multiplicador) |>
  arrange(fecha)   # esto porque el excel viene en orden descendente 

head(base_t)

# de data frame a tsibble 
base_nueva <- base_t |>
  as_tsibble(index = fecha)

head(base_nueva)

count_gaps(base_nueva)
