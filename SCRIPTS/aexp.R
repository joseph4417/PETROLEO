library(fpp3)
library(tidyverse)
library(here)

#cargamos los datos a partir de la carpeta DATA del repositorio
datos <- read_csv(
  here("DATA", "Data.csv"),        
  show_col_types = FALSE
) |>
  mutate(fecha = ymd(fecha)) |>
  arrange(fecha)

# Convertimos el data frame a un objeto tsibble 
petroleo <- datos |>
  as_tsibble(index = fecha)


class(petroleo)
head(petroleo)


# Trabajaremos con el precio de cierre del petroleo como variable principal 


#Grafica de la serie temporal del precio de cierre en función de los días
#Esta gráfica es la base de datos completa del precio de cierre de acuerdo al día
autoplot(petroleo, precio_cierre) +
  labs(title = "Evolución del precio del petróleo",
    subtitle = "Precio diario de cierre de los futuros",
    x = "Fecha (diaria)",
    y = "Precio de cierre"
  ) +
  theme_minimal()

#Revisado por:  Javi Javier 
#Estacionalidad: No, la fecha no influye en el valor
#Tendencia: Si, al parecer desde 2026 se observa una tendencia al alza
#Ciclo: No
#Estacionaridad:No, Tanto la esperanza como la varianza no se observan constantes

# Grafica estacional:comportamiento dentro de cada semana
# cada linea representa una semana de observaciones
# pregunta  para quien me revise: ¿Por qué crees que los sabados y domingos no varían las series? 
#me atrevería a especular que es por que estan cerrados los mercados
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
#Revisado por:  Javi Javier 
#Estacionalidad:Si, los fines de semana se mantienen constantes pero como se toman los datos
#Tendencia: No, en esta grafica no es adecuado buscar tendencia
#Ciclo: No, no se observan ciclos
#Estacionaridad:No, tanto la esperanza como la varianza no se aprecian constantes


# Gráfica mensual: ultimo precio de cierre de cada mes
# aquí graficamos solo los valores de cierre al final de cada mes 
petroleo_mensual <- petroleo |>
  index_by(mes = yearmonth(fecha)) |>
  summarise(
    precio_cierre = last(precio_cierre[!is.na(precio_cierre)])
  )

autoplot(petroleo_mensual, precio_cierre) +
  labs(
    title = "Precio mensual del petróleo",
    subtitle = "Último precio de cierre disponible de cada mes",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()

#Revisado por:  Javi Javier 
#Estacionalidad:No
#Tendencia: Si, en general una tendencia al alza
#Ciclo: No, no se observan ciclos
#Estacionaridad:No, tanto la esperanza como la varianza no se aprecian constantes

# Grafica estacional: comportamiento dentro de cada año
# aquí graficamos una serie de tiempo por cada año
gg_season(
  petroleo,
  precio_cierre,
  period = "year"
) +
  labs(
    title = "Patrón estacional por año del precio del petróleo",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()
#Revisado por:  Javi Javier 
#Estacionalidad:No, la relación con el mes no existe
#Tendencia: No, en esta grafica no es adecuado buscar tendencia
#Ciclo: No, no se observan ciclos
#Estacionaridad:No, tanto la esperanza como la varianza no se aprecian constantes

# Gráficas de las subseries por mes 
# aquí en cada mes se tiene una subserie que representa el comportamiento del precio de cierre en ese mes a 
# traves de los años
gg_subseries(petroleo_mensual, precio_cierre) +
  labs(
    title = "Subseries mensuales del precio del petróleo",
    x = "Mes",
    y = "Precio de cierre"
  ) +
  theme_minimal()

#Revisado por:  Javi Javier 
#Estacionalidad:No, los meses no presentan una estacionalidad
#Tendencia: No, en esta grafica no es adecuado buscar tendencia
#Ciclo: No, no se observan ciclos
#Estacionaridad:No, tanto la esperanza como la varianza no se aprecian constantes

# Comparacion del precio de cierre con apertura, máximo y mínimo
# caquí graficamos una serie de tiempo para cada una de las variables disponibles en la base de datos. 
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
    subtitle = "Comparación de precios de apertura, cierre, máximo y mínimo",
    x = "Fecha (diaria)",
    y = "Precio",
    color = "Variable"
  ) +
  theme_minimal()

#Revisado por:  Javi Javier 
## las variables que se observan tienen relacion directa con el precio de cierre
# y esta grafica ya se reviso al inicio
#Estacionalidad: No, la fecha no influye en el valor
#Tendencia: Si, al parecer desde 2026 se observa una tendencia al alza
#Ciclo: No
#Estacionaridad:No, Tanto la esperanza como la varianza no se observan constantes


#Haz bien tu script
