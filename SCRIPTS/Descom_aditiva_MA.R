#Autora del código: Daniela Cuzco 
library(fpp3)
library(here)
library(tidyverse)
library(readr)
library(slider)


petroleo <- read.csv(here ("DATA", "Data.csv"))

petroleo_ts <- petroleo |> 
  mutate(fecha=as_date(fecha)) |>
  as_tsibble(index=fecha)

petroleo_ts

pet_cierre <- petroleo_ts |>
  autoplot(precio_cierre) + labs(
    title = "Precio de cierre del petróleo",
    x = "Año",
    y = "Precio (Cierre)"
  ) 



#Este para tener el modelo 7-MA (semanales)
pet_med_m7 <- petroleo_ts |>
  mutate(
    Tt = slide_dbl(
      precio_cierre, 
      mean,             
      .before = 3,
      .after = 3,
      .complete = TRUE  
    )
  )

pet_med_m7 |> autoplot(precio_cierre) +

  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1.2) +
  labs(
    x = "Fecha", 
    y = "Precio (USD)", 
    title = "Precio de cierre del petróleo y su media móvil de 7 días"
  )



#Recordemos que yt_pt es el resultado de la serie sin tendencia 
#precio_cierre = yt
datos <- pet_med_m7 |>
  mutate(yt_pt= precio_cierre - Tt)
autoplot(datos, yt_pt) + labs(title = "Serie sin tendencia (yt - Tt)")


#PASO 3
comp_estacional <- datos |>
  group_by(wday(fecha)) |>
  mutate(st= mean(yt_pt, na.rm=TRUE))

autoplot(comp_estacional,st) + labs(x = "Fecha",
                                    y = "Efecto estacional (S_t)", title="Componente estacional (S_t) por día de la semana")


#PASO 4 
restante <- comp_estacional |>
  mutate(Rt = precio_cierre - Tt - st)

autoplot(restante, Rt) + labs(x= "Fecha", y = "Residuo (R_t)", title = "Componente Resto (Ruido) del precio del petróleo")


