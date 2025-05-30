library(dplyr)
library(lubridate)

df_raw <- read.csv("datos/basedatosabiertos_becas270824.csv") |>
  mutate(NIVEL_DETALLADO = recode(NIVEL_DETALLADO,
                                  `DOCTORado` = "DOCTORADO",
                                  `doctorado` = "DOCTORADO"))

df_sen <- df_raw |>
  #filter(PROGRAMA_GENERAL == "PROGRAMA DE BECAS SENESCYT") |>
  mutate(FECHA_INICIO_ESTUDIOS = lubridate::as_date(FECHA_INICIO_ESTUDIOS, origin = "1899-12-30"),
         FECHA_FIN_ESTUDIOS = lubridate::as_date(FECHA_FIN_ESTUDIOS, origin = "1899-12-30"),
         year_init = year(FECHA_INICIO_ESTUDIOS),
         year_fini = year(FECHA_FIN_ESTUDIOS),
         duracion = interval(FECHA_INICIO_ESTUDIOS, FECHA_FIN_ESTUDIOS)/months(1))
  


df_sen |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
#  filter(PROVINCIA_RESIDENCIA_HOMOLOGADA == "EXTERIOR") |>
  group_by(PROGRAMA_GENERAL,NIVEL_DE_ESTUDIOS, NIVEL_DETALLADO, DESTINO, year_init) |>
  summarise(nummer = n()) |>
  ggplot(aes(x = year_init, y = nummer, group = NIVEL_DETALLADO, color = NIVEL_DETALLADO))+
  geom_line()+
  facet_grid(DESTINO~PROGRAMA_GENERAL)

df_sen |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  ggplot(aes(y = NIVEL_DETALLADO,  x = duracion/12))+
  geom_boxplot()+
  facet_wrap(~DESTINO)

jums <- df_sen |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  group_by(CARRERA, NIVEL_DETALLADO, DESTINO) |>
  summarise(nummer = n()) |>
  arrange(CARRERA,-nummer)

df_sen |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  #  filter(PROVINCIA_RESIDENCIA_HOMOLOGADA == "EXTERIOR") |>
  group_by(NIVEL_DE_ESTUDIOS, NIVEL_DETALLADO, DESTINO, year_fini) |>
  summarise(nummer = n()) |>
  ggplot(aes(x = year_fini, y = nummer, group = NIVEL_DETALLADO, color = NIVEL_DETALLADO))+
  geom_line()+
  facet_wrap(~DESTINO)
