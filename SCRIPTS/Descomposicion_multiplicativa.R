library(fpp3)   
library(slider) 


petroleo <- read.csv("DATA/Data.csv") |>
  mutate(fecha = ymd(fecha)) |> arrange (fecha)

# Convertimos el data frame a un objeto tsibble 
petroleo_ts <- petroleo |>
  as_tsibble(index = fecha)

#huecos
has_gaps(petroleo_ts)
interval(petroleo_ts)

#obtención de variable precio_cierre
petroleo_pc <- petroleo_ts |>
  select(fecha, precio_cierre)

# Gráfico de la serie original
petroleo_pc |>
  autoplot(precio_cierre) +
  labs(y = "Precio de cierre (USD)",
       x = "Fecha",
       title = "Precio de cierre diario del petróleo")


# Como se tienen datos diarios, entonces el periodo estacional  es semanal y m =7. 
# Como m es IMPAR, la tendencia-ciclo se estima con una media móvil
# centrada de orden m (7-MA): 3 observaciones antes, la actual y 3 después.

m <- 7

#MEDIAS MOVILES
petroleo_ma <- petroleo_pc |>
  mutate(
    `7-MA` = slider::slide_dbl(
      precio_cierre,
      mean,
      .before = (m - 1) / 2,   # 3
      .after  = (m - 1) / 2,   # 3
      .complete = TRUE
    )
  )

# Gráfico de la serie con media movil
petroleo_ma |>
  autoplot(precio_cierre, colour = "gray") +
  geom_line(aes(y = `7-MA`), colour = "#D55E00") +
  labs(y = "Precio de cierre (USD)",
       x = "Fecha",
       title = "Precio de cierre del petróleo y su media móvil (7-MA)")



# SERIE SIN TENDENCIA ( y_t / T_t)

# En la descomposición MULTIPLICATIVA se DIVIDE entre la tendencia
# (en la aditiva se resta: y_t - T_t).

petroleo_dec <- petroleo_ma |>
  mutate(
    tendencia = `7-MA`,
    razon     = precio_cierre / tendencia,              # y_t / T_t = S_t x R_t
    dia_semana = wday(fecha, label = TRUE, week_start = 1)  # posición estacional
  )

#grafico serie sin tendencia
petroleo_dec |>
  autoplot(razon) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray40") +
  labs(y = "Razón",
       x = "Fecha",
       title = "Serie sin tendencia: precio de cierre / tendencia")



#COMPONENTE ESTACIONAL

# Se promedian las razones y_t / T_t de cada día de la semana
# (todos los lunes, todos los martes, etc.) ignorando los NA.
# Luego se NORMALIZAN para que el índice estacional promedie 1
# (en la aditiva se ajustan para que sumen 0; en la multiplicativa
# para que su media sea 1, es decir, sumen m).

indices_estacionales <- petroleo_dec |>
  as_tibble() |>
  group_by(dia_semana) |>
  summarise(indice_bruto = mean(razon, na.rm = TRUE), .groups = "drop") |>
  mutate(estacionalidad = indice_bruto / mean(indice_bruto))

indices_estacionales   # índice estacional por día de la semana
mean(indices_estacionales$estacionalidad)   # debe ser 1

# Asignamos el índice estacional a cada observación
petroleo_dec <- petroleo_dec |>
  left_join(indices_estacionales |> select(dia_semana, estacionalidad),
            by = "dia_semana")



# COMPONENTE RESIDUAL  R_t = y_t / (T_t x S_t)


petroleo_dec <- petroleo_dec |>
  mutate(residuo = precio_cierre / (tendencia * estacionalidad))

# Tabla de resumen con los tres componentes
petroleo_dec |>
  select(fecha, precio_cierre, tendencia, estacionalidad, residuo)


# Grafico detendencia-ciclo
petroleo_dec |>
  autoplot(tendencia, colour = "#D55E00") +
  labs(y = "USD", x = "Fecha",
       title = "Componente de tendencia-ciclo (7-MA)")

# Gráfico de estacionalidad
petroleo_dec |>
  autoplot(estacionalidad, colour = "#0072B2") +
  labs(y = "Índice estacional", x = "Fecha",
       title = "Componente estacional (semanal)")


# Gráfico residuo. 
petroleo_dec |>
  autoplot(residuo, colour = "#009E73") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray40") +
  labs(y = "Residuo", x = "Fecha",
       title = "Componente residual (multiplicativo)")



# DESCOMPOSICIÓN CON classical_decomposition() Y GRÁFICO CONJUNTO

dcmp <- petroleo_pc |>
  model(
    classical_decomposition(precio_cierre ~ season(7), type = "multiplicative")
  ) |>
  components()

dcmp

# Gráfico conjunto de la descomposición
dcmp |>
  autoplot() +
  labs(title = "Descomposición clásica multiplicativa del precio de cierre del petróleo")




# VERIFICACIÓN
all.equal(petroleo_dec$tendencia,      dcmp$trend)
all.equal(petroleo_dec$estacionalidad, dcmp$seasonal)
all.equal(petroleo_dec$residuo,        dcmp$random)
