library(dplyr)
library(ggplot2)

source("00_Auxiliares.R")

df_raw <- openxlsx::read.xlsx("datos/Reporte_estadistico_docentes_UEP_2015-2022.xlsx",
                              startRow = 15) |>
  mutate(NIVEL = gsub("CUARTO_NIVEL_", "CUARTO_NIVEL", NIVEL),
         GRADO = recode(GRADO,
                        `TERCER NIVEL DE GRADO` = "TERCER_NIVEL",
                        `TERCER NIVEL TECNOLÓGICO` = "TERCER_NIVEL",
                        `TERCER NIVEL TÉCNICO` = "TERCER_NIVEL")) |>
  filter(NIVEL != "NO_DEFINIDO") # solo 7 casos

df_raw <- left_join(df_raw, universities, by = c("NOMBRE_IES" = "nombre")) |>
  distinct()

colnames(df_raw)[1] <- "YEAR"

JUMS <- df_raw |>
  group_by(YEAR, RELACION_IES) |>
  summarise(tot = sum(TOTAL))


per_df <- left_join(df_raw |>
                      filter(!GRADO %in% c("DOCTOR EN FILOSOFIA O JURISPRUDENCIA",
                                           "ESPECIALISTA")) |>
                      group_by(YEAR, GRADO) |>
                      summarise(total_grado = sum(TOTAL))|>
                      ungroup(),
                    df_raw |>
                      filter(!GRADO %in% c("DOCTOR EN FILOSOFIA O JURISPRUDENCIA",
                                           "ESPECIALISTA")) |>
                      
                      group_by(YEAR) |>
                      summarise(total_docentes = sum(TOTAL)) |>
                      ungroup(), by = "YEAR") |>
  mutate(porcen = total_grado/total_docentes) |>
  group_by(YEAR) |>
  arrange(desc(GRADO)) |>
  mutate(pos_label = cumsum(porcen) - porcen / 2)

library(viridis)

ggplot(per_df, aes(x = factor(YEAR), y = porcen, fill = GRADO)) +
  geom_bar(stat = "identity", position = "fill", color = "white") +
  geom_text(aes(y = pos_label, label = scales::percent(porcen, accuracy = 0.1)),
            color = "white", size = 4.5, fontface = "bold")+
  scale_y_continuous(label = scales::percent) +
#  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.9) +
  labs(title = "Distribución porcentual de docentes según grado académico",
       x = "Año", y = "Porcentaje", fill = "Grado académico") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


per_df |>
  ggplot(aes(x = YEAR, y = porcen, color = GRADO))+
  geom_point()+
  geom_line()+
  ggrepel::geom_label_repel(aes(label = scales::percent(porcen, accuracy = 0.1)))+
  scale_y_continuous(label = scales::percent)


ggplot(per_df, aes(x = factor(YEAR), y = porcen, fill = GRADO)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribución porcentual de docentes según grado académico",
       x = "Año", y = "Porcentaje", fill = "Grado académico") +
  theme_minimal(base_size = 14)

per_df |>
  arrange(GRADO,YEAR) |>
  group_by(GRADO) |>
  mutate(pct_change = (porcen - lag(porcen))/lag(porcen)) |>
  ggplot(aes(x = YEAR, y = pct_change))+
  geom_point()+
  geom_line()+
  facet_wrap(~GRADO)+
  ggrepel::geom_label_repel(aes(label = scales::percent(pct_change, accuracy = 0.1)))+
  scale_y_continuous(label = scales::percent)



df_raw |>
  group_by(YEAR) |>
  summarise(total = sum(TOTAL)) |>
  ggplot(aes(x = YEAR, y = total)) + 
  geom_point()+
  geom_line()+
  geom_label(aes(label = total))+
  theme_bw()+
  #facet_wrap(~clase)+
  labs(x = "Año", y = "Total Docentes")

df1 <- df_raw |>
  filter(NIVEL != "NO_DEFINIDO") |>
  group_by(YEAR, NIVEL) |>
  summarise(total_docente = sum(TOTAL))
  

unique(df1$NIVEL)

df1 |>
  ggplot(aes(x = YEAR, y = total_docente, color = NIVEL))+
  geom_point()+
  geom_line()

df2 <- df_raw |>
  #filter(NIVEL == "CUARTO_NIVEL") |>
  group_by(YEAR,GRADO) |>
  summarise(total_docente = sum(TOTAL))
  
df2 |>
  ggplot(aes(x = YEAR, y = total_docente, color = GRADO))+
  geom_point()+
  geom_line()

df3 <- df_raw |>
  #filter(NIVEL == "CUARTO_NIVEL") |>
  group_by(YEAR, GRADO, clase) |>
  summarise(total_docente = sum(TOTAL))

df3 |>
  ggplot(aes(x = YEAR, y = total_docente, color = GRADO))+
  geom_point()+
  geom_line()+
  facet_wrap(~clase)

df4 <- df_raw |>
  #filter(NIVEL == "CUARTO_NIVEL") |>
  filter(RELACION_IES != "PROMETEO") |>
  group_by(YEAR, GRADO, RELACION_IES, Classification) |>
  summarise(total_docente = sum(TOTAL))

df4 |>
  ggplot(aes(x = YEAR, y = total_docente, color = GRADO))+
  geom_point()+
  geom_line()+
  facet_grid(RELACION_IES~Classification)



library(readr)

investigadores <- read_csv("datos/bdd_CT_2015.csv")


df_col <- openxlsx::read.xlsx("datos/profesores_colombia_copy.xlsx")
df_col_raw <- df_col |>
  filter(SEMESTRE == 2) |>
  select(GRADO = MÁXIMO.NIVEL.DE.FORMACIÓN.DEL.DOCENTE, dedicacion = TIEMPO.DE.DEDICACIÓN.DEL.DOCENTE,
         contrato = TIPO.DE.CONTRATO, year = AÑO, TOTAL = DOCENTES)

df_col_raw |>
  group_by(GRADO) |>
  summarise(total = sum(TOTAL))

sum(df_col_raw$TOTAL)


per_df_rl <- left_join(df_raw |>
                         filter(!GRADO %in% c("DOCTOR EN FILOSOFIA O JURISPRUDENCIA",
                                              "ESPECIALISTA")) |>
                         group_by(YEAR, GRADO, clase, RELACION_IES) |>
                         summarise(total_grado = sum(TOTAL))|>
                         ungroup(),
                       df_raw |>
                         filter(!GRADO %in% c("DOCTOR EN FILOSOFIA O JURISPRUDENCIA",
                                              "ESPECIALISTA")) |>
                         
                         group_by(YEAR, clase) |>
                         summarise(total_docentes = sum(TOTAL)) |>
                         ungroup(), by = c("YEAR", "clase")) |>
  mutate(porcen = total_grado/total_docentes) |>
  mutate(RELACION_IES = recode(RELACION_IES,
                               `CONTRATO_CON_RELACION_DE_DEPENDENCIA` = "RELACION DE DEPENDENCIA",
                               `CONTRATO_SIN_RELACION_DE_DEPENDENCIA` = "RELACION SIN DEPENDENCIA")) |>
  filter(RELACION_IES != "PROMETEO") |>
  group_by(YEAR, clase, RELACION_IES) |>
  arrange(desc(GRADO), desc(clase), desc(RELACION_IES)) |>
  mutate(pos_label = cumsum(porcen) - porcen / 2)



per_df_rl |>
  filter(clase == "Private") |>
  
  ggplot(aes(x = factor(YEAR), y = porcen, fill = GRADO)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  geom_text(aes(y = pos_label, label = scales::percent(porcen, accuracy = 0.1)),
            color = "white", size = 2, fontface = "bold")+
  scale_y_continuous(label = scales::percent) +
  facet_wrap2(vars(RELACION_IES), scales = "free_y")+
  #  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.9) +
  labs(title = "Distribución porcentual de docentes según grado académico y relación laboral: Universidades privadas",
       x = "Año", y = "%", caption = "Fuente: Base estadística de docentes UEP 2015 - 2022\nGrados excluidos: DOCTOR EN FILOSOFIA O JURISPRUDENCIA, ESPECIALISTA.\nTercer Nivel: Agregadas TERCER NIVEL DE GRADO,\nTERCER NIVEL TECNOLOGICO Y TERCER NIVEL TECNICO.") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))+
  
  guides(fill = guide_legend(nrow=1))



df_raw |>
  mutate(clase = recode(clase,
                        `Public` = "Públicas",
                        `Private` = "Privadas"),
         RELACION_IES = recode(RELACION_IES,
                               `CONTRATO_CON_RELACION_DE_DEPENDENCIA` = "RELACION DE DEPENDENCIA",
                               `CONTRATO_SIN_RELACION_DE_DEPENDENCIA` = "RELACION SIN DEPENDENCIA")) |>
  filter(!GRADO %in% c("DOCTOR EN FILOSOFIA O JURISPRUDENCIA",
                       "ESPECIALISTA")) |>
  filter(RELACION_IES != "PROMETEO") |>
  group_by(YEAR, clase, RELACION_IES, GRADO) |>
  summarise(total = sum(TOTAL))|>
  ungroup() |>
  filter(clase == "Privadas") |>
  ggplot(aes(x = YEAR, y = total, color = GRADO)) +
  geom_point()+
  geom_line(linetype = "dashed")+
  
  #geom_vline(xintercept = 2020, color = "red", linetype = "dashed")+
  ggrepel::geom_label_repel(aes(label = total), size = 1.7)+
  xlim(c(2015,2022))+
  #facet_grid2(vars(clase), vars(RELACION_IES), scales = "free_y", independent = "y")+
  theme_bw()+
  theme_bw(base_size = 8)+
  facet_wrap2(vars(RELACION_IES), scales = "free_y")+
  labs(x = "Año", title = "Total Docentes sector privado y relación laboral", y = "# Docentes", caption = "Fuente: Base estadística de docentes UEP 2015 - 2022\nGrados excluidos: DOCTOR EN FILOSOFIA O JURISPRUDENCIA, ESPECIALISTA.\nTercer Nivel: Agregadas TERCER NIVEL DE GRADO,\nTERCER NIVEL TECNOLOGICO Y TERCER NIVEL TECNICO.") +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom")+
  guides(color = guide_legend(nrow=1))
