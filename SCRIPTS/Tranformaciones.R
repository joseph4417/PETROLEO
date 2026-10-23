library(fpp3)
library(ggplot2)


base <- read.csv("C:/Users/math.epn.edu.ec/Documents/ST/Data.csv")

base <- as.data.frame(base)

names(base) <- c("fecha", "precio_cierre", "apertura", "maximo",
                 "minimo", "volumen", "var_pct")

base_t <- base |>
  mutate(
    fecha = ymd(fecha)
    )

head(base_t)
class(base_t)

#tsibble
base_nueva <- base_t |>
  as_tsibble(index = fecha)

autoplot(base_nueva, precio_cierre) +
  labs(
    title = "Precio de cierre",
    x = "Fecha",
    y = "Precio"
  ) +
  theme_minimal()

#coeficiente que hace que la varianza sea constante 
lambda <- base_nueva %>% features(precio_cierre, features = guerrero) %>%
  pull(lambda_guerrero)

base_nueva %>% autoplot(box_cox(precio_cierre, lambda)) 

lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)

dcmp <- base_nueva %>%
  model(stl = STL(precio_cierre))
components(dcmp)

components(dcmp)|> autoplot()

