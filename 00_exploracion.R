library(dplyr)
library(ggplot2)

df_raw <- openxlsx::read.xlsx("datos/Reporte_estadistico_docentes_UEP_2015-2022.xlsx",
                              startRow = 15) |>
  mutate(NIVEL = gsub("CUARTO_NIVEL_", "CUARTO_NIVEL", NIVEL),
         GRADO = recode(GRADO,
                        `TERCER NIVEL DE GRADO` = "TERCER_NIVEL",
                        `TERCER NIVEL TECNOLÓGICO` = "TERCER_NIVEL",
                        `TERCER NIVEL TÉCNICO` = "TERCER_NIVEL")) |>
  filter(NIVEL != "NO_DEFINIDO") # solo 7 casos

df_raw <- left_join(df_raw, universities, by = c("NOMBRE_IES" = "University"))

colnames(df_raw)[1] <- "YEAR"

df1 <- df_raw |>
  filter(NIVEL != "NO_DEFINIDO") |>
  group_by(YEAR, SEXO, NIVEL) |>
  summarise(total_docente = sum(TOTAL))
  

unique(df1$NIVEL)

df1 |>
  ggplot(aes(x = YEAR, y = total_docente, color = NIVEL))+
  geom_point()+
  geom_line()+
  facet_wrap(~SEXO)

df2 <- df_raw |>
  #filter(NIVEL == "CUARTO_NIVEL") |>
  group_by(YEAR, SEXO, GRADO) |>
  summarise(total_docente = sum(TOTAL))
  
df2 |>
  ggplot(aes(x = YEAR, y = total_docente, color = GRADO))+
  geom_point()+
  geom_line()+
  facet_wrap(~SEXO)

df3 <- df_raw |>
  #filter(NIVEL == "CUARTO_NIVEL") |>
  group_by(YEAR, GRADO, Classification) |>
  summarise(total_docente = sum(TOTAL))

df3 |>
  ggplot(aes(x = YEAR, y = total_docente, color = GRADO))+
  geom_point()+
  geom_line()+
  facet_wrap(~Classification)

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

universities <- data.frame(
  University = c(
    "ESCUELA POLITECNICA NACIONAL",
    "ESCUELA SUPERIOR POLITECNICA DE CHIMBORAZO",
    "ESCUELA SUPERIOR POLITECNICA AGROPECUARIA DE MANABI",
    "UNIVERSIDAD CENTRAL DEL ECUADOR",
    "UNIVERSIDAD DE GUAYAQUIL",
    "UNIVERSIDAD DE CUENCA",
    "UNIVERSIDAD NACIONAL DE LOJA",
    "UNIVERSIDAD TECNICA DE MANABI",
    "UNIVERSIDAD TECNICA DE AMBATO",
    "UNIVERSIDAD TECNICA DE MACHALA",
    "UNIVERSIDAD TECNICA LUIS VARGAS TORRES DE ESMERALDAS",
    "UNIVERSIDAD TECNICA DE BABAHOYO",
    "UNIVERSIDAD TECNICA ESTATAL DE QUEVEDO",
    "UNIVERSIDAD TECNICA DEL NORTE",
    "UNIVERSIDAD LAICA ELOY ALFARO DE MANABI",
    "UNIVERSIDAD ESTATAL DE BOLIVAR",
    "UNIVERSIDAD AGRARIA DEL ECUADOR",
    "UNIVERSIDAD NACIONAL DE CHIMBORAZO",
    "UNIVERSIDAD TECNICA DE COTOPAXI",
    "ESCUELA SUPERIOR POLITECNICA DEL LITORAL",
    "UNIVERSIDAD ANDINA SIMON BOLIVAR",
    "UNIVERSIDAD ESTATAL PENINSULA DE SANTA ELENA",
    "UNIVERSIDAD ESTATAL DE MILAGRO",
    "UNIVERSIDAD ESTATAL DEL SUR DE MANABI",
    "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES",
    "PONTIFICIA UNIVERSIDAD CATÓLICA DEL ECUADOR",
    "UNIVERSIDAD CATOLICA DE SANTIAGO DE GUAYAQUIL",
    "UNIVERSIDAD CATOLICA DE CUENCA",
    "UNIVERSIDAD LAICA VICENTE ROCAFUERTE DE GUAYAQUIL",
    "UNIVERSIDAD TECNICA PARTICULAR DE LOJA",
    "UNIVERSIDAD UTE",
    "UNIVERSIDAD DEL AZUAY",
    "UNIVERSIDAD POLITECNICA SALESIANA",
    "UNIVERSIDAD PARTICULAR INTERNACIONAL SEK",
    "UNIVERSIDAD PARTICULAR DE ESPECIALIDADES ESPIRITU SANTO",
    "UNIVERSIDAD SAN FRANCISCO DE QUITO",
    "UNIVERSIDAD DE LAS AMERICAS",
    "UNIVERSIDAD INTERNACIONAL DEL ECUADOR",
    "UNIVERSIDAD REGIONAL AUTÓNOMA DE LOS ANDES",
    "UNIVERSIDAD DEL PACIFICO ESCUELA DE NEGOCIOS",
    "UNIVERSIDAD TECNOLOGICA INDOAMERICA",
    "UNIVERSIDAD CASA GRANDE",
    "UNIVERSIDAD TECNOLOGICA EMPRESARIAL DE GUAYAQUIL",
    "UNIVERSIDAD TECNOLOGICA ISRAEL",
    "UNIVERSIDAD DE ESPECIALIDADES TURISTICAS",
    "UNIVERSIDAD METROPOLITANA",
    "INSTITUTO DE ALTOS ESTUDIOS NACIONALES",
    "UNIVERSIDAD ESTATAL AMAZONICA",
    "UNIVERSIDAD DE OTAVALO",
    "UNIVERSIDAD PARTICULAR SAN GREGORIO DE PORTOVIEJO",
    "UNIVERSIDAD DE LOS HEMISFERIOS",
    "UNIVERSIDAD IBEROAMERICANA DEL ECUADOR",
    "UNIVERSIDAD POLITECNICA ESTATAL DEL CARCHI",
    "UNIVERSIDAD TECNOLOGICA ECOTEC",
    "UNIVERSIDAD DE LAS FUERZAS ARMADAS",
    "UNIVERSIDAD REGIONAL AMAZONICA 'IKIAM'",
    "UNIVERSIDAD DE INVESTIGACION EXPERIMENTAL YACHAY-TECH",
    "UNIVERSIDAD DE LAS ARTES 'UARTES'",
    "UNIVERSIDAD NACIONAL DE EDUCACION 'UNAE'",
    "UNIVERSIDAD DEL RÍO",
    "UNIVERSIDAD INTERCULTURAL DE LAS NACIONALIDADES Y PUEBLOS INDIGENAS AMAWTAY WASI",
    "UNIVERSIDAD BOLIVARIANA DEL ECUADOR"
  ),
  Classification = c(
    "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", 
    "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", 
    "Public", "Public", "Public", "Public", "Public", "Private", "Private", "Private", "Private", "Private", 
    "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", 
    "Private", "Private", "Private", "Private", "Private", "Private", "Public", "Public", "Private", "Private", 
    "Private", "Private", "Public", "Private", "Public", "Public", "Public", "Public", "Public", "Private", 
    "Public", "Private"
  )
)
