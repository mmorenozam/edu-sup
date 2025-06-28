library(haven)
library(dplyr)
library(tidyr)

tic <- read_sav("datos/bdd_CT_2015.sav")

tic2 <- read_sav("datos/bdd_CT_2013.sav")

tic2012<- tic |>
  select(id = ui.0,
         tipo = TIPO, 
         ciiu2,
         n.hombres.total_12 = II.1.a.h.12.total,
         n.hombres.total_13 = II.1.a.h.13.total,
         n.hombres.total_14 = II.1.a.h.14.total,
         n.hombres.total.becarios_12 = II.1.b.h.12.total,
         n.hombres.total.becarios_13 = II.1.b.h.13.total,
         n.hombres.total.becarios_14 = II.1.b.h.14.total,
         
         n.mujeres.total_12 = II.1.a.m.12.total,
         n.mujeres.total_13 = II.1.a.m.13.total,
         n.mujeres.total_14 = II.1.a.m.14.total,
         n.mujeres.total.becarios_12 = II.1.b.m.12.total,
         n.mujeres.total.becarios_13 = II.1.b.m.13.total,
         n.mujeres.total.becarios_14 = II.1.b.m.14.total,
         )|>
  mutate(tipo = as.numeric(tipo),
         tipo = recode(tipo,
                       `1` = "GOBIERNO",
                       `2` = "UNIVERSIDAD",
                       `3` = "ONG"),
         tipo = as.character(tipo),
         ciiu2 = as.character(ciiu2),
         ciiu2 = recode(ciiu2,
                        `A01` = "AGRICULTURA Y GANADERIA",
                        `M71` = "ARQUITECTURA E INGENIERIA",
                        `M72` = "INVESTIGACION CIENTIFICA Y DESARROLLO",
                        `O84` = "ADMINISTRACION PUBLICA Y DEFENSA",
                        `P85` = "ENSEÑANZA",
                        `Q86` = "SALUD HUMANA",
                        `R91` = "BIBLIOTECAS, ARCHIVOS Y MUSEOS",
                        `S94` = "ACTIVIDADES DE ASOCIACIONES")) |>
  pivot_longer(
    cols = starts_with("n."),
    names_to = c("sexo", "tipo_empleo", "year"),
    names_pattern = "n\\.(hombres|mujeres)\\.(total(?:\\.becarios)?)[_]?([0-9]{2})",
    values_to = "n_personas"
  ) |>
  mutate(
    year = paste0("20", year),
    tipo_empleo = recode(tipo_empleo,
                         "total" = "total_gentusa",
                         "total.becarios" = "total_becarios")
  ) |>
  group_by(id, tipo, ciiu2, year, tipo_empleo) |>
  summarise(n = sum(n_personas, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = tipo_empleo,
    values_from = n,
    values_fill = 0
  )

  
  
tic2010 <- tic2 |>
  select(id = id_empresa,
         tipo = TIPO,
         ciiu2,
         n.hombres.total_09 = III.1.a.h.09.total,
         n.hombres.total_10 = III.1.a.h.10.total,
         n.hombres.total_11 = III.1.a.h.11.total,
         n.hombres.total.becarios_09 = III.1.b.h.09.total,
         n.hombres.total.becarios_10 = III.1.b.h.10.total,
         n.hombres.total.becarios_11 = III.1.b.h.11.total,
         
         n.mujeres.total_09 = III.1.a.m.09.total,
         n.mujeres.total_10 = III.1.a.m.10.total,
         n.mujeres.total_11 = III.1.a.m.11.total,
         n.mujeres.total.becarios_09 = III.1.b.m.09.total,
         n.mujeres.total.becarios_10 = III.1.b.m.10.total, 
         n.mujeres.total.becarios_11 = III.1.b.m.11.total) |>
  mutate(ciiu2 = as.character(ciiu2),
         ciiu2 = recode(ciiu2,
                        `A01` = "AGRICULTURA Y GANADERIA",
                        `M72` = "INVESTIGACION CIENTIFICA Y DESARROLLO",
                        `M74` = "OTRAS ACTIVIDADES CIENTIFICAS Y TECNICAS",
                        `O84` = "ADMINISTRACION PUBLICA Y DEFENSA",
                        `P85` = "ENSEÑANZA",
                        `Q86` = "SALUD HUMANA",
                        `R91` = "BIBLIOTECAS, ARCHIVOS Y MUSEOS",
                        `S94` = "ACTIVIDADES DE ASOCIACIONES")) |>
  pivot_longer(
    cols = starts_with("n."),
    names_to = c("sexo", "tipo_empleo", "year"),
    names_pattern = "n\\.(hombres|mujeres)\\.(total(?:\\.becarios)?)[_]?([0-9]{2})",
    values_to = "n_personas"
  ) |>
  mutate(
    year = paste0("20", year),
    tipo_empleo = recode(tipo_empleo,
                         "total" = "total_gentusa",
                         "total.becarios" = "total_becarios")
  ) |>
  group_by(id, tipo, ciiu2, year, tipo_empleo) |>
  summarise(n = sum(n_personas, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = tipo_empleo,
    values_from = n,
    values_fill = 0
  )


tics_alles <- bind_rows(tic2010, tic2012)

saveRDS(tics_alles, "datos/tics.rds")

tics_alles |> 
  group_by(year) |> 
  summarise(investigadores = sum(total_gentusa), phdeses = sum(total_becarios)) |>
  
