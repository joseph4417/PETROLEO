
#GRAFICAS DE LA SERIE TEMPORAL DEL PETROLEO

library(fpp3)
library(tidyverse)
library(here)

#leer archivos de la carpeta DATA 
datos <- read_csv(
  here("DATA", "Data.csv"),
  show_col_types = FALSE
) |>
  mutate(fecha = ymd(fecha)) |>
  arrange(fecha)

# Convertir de data frame a tsibble
petroleo <- datos |>
  as_tsibble(index = fecha)

class(petroleo)
head(petroleo)



#grafica de la serie temporal del precio de cierre
autoplot(petroleo, precio_cierre) +
  labs(
    title = "Evolución del precio del petróleo",
    subtitle = "Precio diario de cierre de los futuros",
    x = "Fecha",
    y = "Precio de cierre",
    caption = "Fuente: base de datos del proyecto"
  ) +
  theme_minimal()


#Comparacion del precio de cierre con apertura, maximo y minimo
petroleo |>
  pivot_longer(
    cols = c(precio_cierre, apertura, maximo, minimo),
    names_to = "variable",
    values_to = "precio"
  ) |>
  ggplot(aes(x = fecha, y = precio, color = variable)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  labs(
    title = "Comportamiento de los precios del petróleo",
    subtitle = "Comparación de apertura, cierre, máximo y mínimo",
    x = "Fecha",
    y = "Precio",
    color = "Variable"
  ) +
  theme_minimal()


# ------------------------------------------------------------
#comportamiento dentro de cada semana
gg_season(
  petroleo,
  precio_cierre,
  period = "week"
) +
  labs(
    title = "Estacionalidad semanal del precio del petróleo",
    x = "Día de la semana",
    y = "Precio de cierre"
  ) +
  theme_minimal()



#comportamiento dentro de cada año
gg_season(
  petroleo,
  precio_cierre,
  period = "year"
) +
  labs(
    title = "Patrón estacional anual del precio del petróleo",
    x = "Tiempo dentro del año",
    y = "Precio de cierre"
  ) +
  theme_minimal()


#serie mensual - ultimo precio de cierre de cada mes
petroleo_mensual <- petroleo |>
  index_by(mes = yearmonth(fecha)) |>
  summarise(
    precio_cierre = last(precio_cierre[!is.na(precio_cierre)])
  )

head(petroleo_mensual)



#grafica de la serie mensual
autoplot(petroleo_mensual, precio_cierre) +
  labs(
    title = "Precio mensual del petróleo",
    subtitle = "Último precio de cierre disponible de cada mes",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()


#grafica estacional y subseries mensuales
gg_season(petroleo_mensual, precio_cierre) +
  labs(
    title = "Estacionalidad anual del precio mensual del petróleo",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()

gg_subseries(petroleo_mensual, precio_cierre) +
  labs(
    title = "Subseries mensuales del precio del petróleo",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()



#grafica del volumen negociado
autoplot(petroleo, volumen) +
  labs(
    title = "Volumen negociado de futuros del petróleo",
    x = "Fecha",
    y = "Volumen"
  ) +
  theme_minimal()

# grafica de la variacion porcentual
petroleo |>
  ggplot(aes(x = fecha, y = var_pct)) +
  geom_line(na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Variación porcentual del precio del petróleo",
    x = "Fecha",
    y = "Variación (%)"
  ) +
  theme_minimal()

