library(readxl)
library(fpp3)
library(tidyverse)
library(here)

path <- here("DATA", "Datos petroleo.xlsx") ##lee la base usando el paquete here

base <- read_excel(path)

#dataframe
base <- as.data.frame(base)
class(base)

names(base) <- c("fecha", "precio_cierre", "apertura", "maximo",
                 "minimo", "volumen", "var_pct")

base_t <- base |>
  mutate(
    fecha = ymd(fecha),
    # "236,38K" -> 236380 ; "1,2M" -> 1200000 ; vacío -> NA
    volumen = case_when(
      is.na(volumen) | volumen == "" ~ NA_real_,
      str_detect(volumen, "K$") ~ as.numeric(str_replace(str_remove(volumen, "K$"), ",", ".")) * 1e3,
      str_detect(volumen, "M$") ~ as.numeric(str_replace(str_remove(volumen, "M$"), ",", ".")) * 1e6,
      TRUE ~ as.numeric(str_replace(volumen, ",", "."))
    )
  )|>
  arrange(fecha)   # el Excel viene en orden descendente

head(base_t)

#convertir a tsibble
base_nueva <- base_t |>
  as_tsibble(index = fecha)|>fill_gaps()

head(base_nueva)
count_gaps(base_nueva)

