library(dplyr)
library(lubridate)
library(ggplot2)

df_raw_bec <- read.csv("datos/basedatosabiertos_becas270824.csv") |>
  mutate(NIVEL_DETALLADO = recode(NIVEL_DETALLADO,
                                  `DOCTORado` = "DOCTORADO",
                                  `doctorado` = "DOCTORADO"))

df_bec <- df_raw_bec |>
  #filter(PROGRAMA_GENERAL == "PROGRAMA DE BECAS SENESCYT") |>
  mutate(FECHA_INICIO_ESTUDIOS = as_date(FECHA_INICIO_ESTUDIOS, origin = "1899-12-30"),
         FECHA_FIN_ESTUDIOS = as_date(FECHA_FIN_ESTUDIOS, origin = "1899-12-30"),
         FECHA_DE_SUSCRIPCION = as_date(as.numeric(FECHA_DE_SUSCRIPCION), origin = "1899-12-30"),
         year_susc = year(FECHA_DE_SUSCRIPCION),
         year_init = year(FECHA_INICIO_ESTUDIOS),
         year_fini = year(FECHA_FIN_ESTUDIOS),
         duracion = interval(FECHA_INICIO_ESTUDIOS, FECHA_FIN_ESTUDIOS)/months(1),
         IDENTIFICADOR_BECARIO = recode(IDENTIFICADOR_BECARIO,
                                        `ok` = "OK")) |>
  filter(IDENTIFICADOR_BECARIO == "OK")|>
  filter(!SUBPROGRAMA %in% c("LINEA DE AYUDAS ECONOMICAS") & duracion >=12) |>
  distinct()


df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO" & 
           NIVEL_DETALLADO != "POSTDOCTORADO" & 
           NIVEL_DETALLADO != "ESPECIALIZACION" &
           year_init <= 2018 &
           year_init >= 2011) |>
  #  filter(PROVINCIA_RESIDENCIA_HOMOLOGADA == "EXTERIOR") |>
  group_by(NIVEL_DETALLADO, year_init) |>
  summarise(nummer = n()) |>
  ggplot(aes(x = year_init, y = nummer, color = NIVEL_DETALLADO))+
  geom_line()+
  geom_point()+
  ggrepel::geom_label_repel(aes(label = nummer), size = 5)+
  theme_bw(base_size = 16)+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom")+
  labs(title = "# de becarios de postgrado por año de inicio de estudios", x = "Año de Inicio", y = "#",
       color = "Nivel", caption = "Fuente: Base de datos de Becarios actualizada agosto 2024 (SENESCYT)\nNíveles excluidos: POSTDOCTORADO y ESPECIALIZACION")

df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO" & 
           NIVEL_DETALLADO != "POSTDOCTORADO" & 
           NIVEL_DETALLADO != "ESPECIALIZACION" &
           year_init <= 2018 &
           year_init >= 2011) |>
  ggplot(aes(x = NIVEL_DETALLADO, y = duracion/12))+
  geom_boxplot()+
  facet_wrap(~ESTADO_BECA)+
  geom_jitter(alpha = 0.25)

### cosas varias:

repes <- df_bec |>
  group_by(CODIGO_IDENTIFICADOR) |>
  summarise(nummer = n()) |>
  filter(nummer > 1) |>
  arrange(-nummer)

repe_champ <- df_bec |>
  filter(CODIGO_IDENTIFICADOR %in% repes$CODIGO_IDENTIFICADOR) |>
  arrange(CODIGO_IDENTIFICADOR, IDENTIFICADOR_BECAS)

zeros <- df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO" & 
           NIVEL_DETALLADO != "POSTDOCTORADO" & 
           NIVEL_DETALLADO != "ESPECIALIZACION") |>
  arrange(duracion)

JUMSIS <- df_sen |> filter(PROGRAMA_GENERAL == "PROGRAMA DE BECAS SENESCYT") |> group_by(SUBPROGRAMA) |> summarise(nummer = n())

haram <- df_bec |> group_by(ESTADO_ESTUDIANTE, ESTADO_BECA) |> summarise(nummer = n())

orujos <- df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO" & 
           NIVEL_DETALLADO != "POSTDOCTORADO" & 
           NIVEL_DETALLADO != "ESPECIALIZACION" &
           year_init <= 2018 &
           year_init >= 2011 &
           ESTADO_BECA != "INCUMPLIDOS") |>
  arrange(-duracion)

programs <- df_bec |> group_by(SUBPROGRAMA, year_init)|>
  summarise(nummer = n())


df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  #  filter(PROVINCIA_RESIDENCIA_HOMOLOGADA == "EXTERIOR") |>
  group_by(NIVEL_DETALLADO, year_init) |>
  summarise(nummer = n()) |>
  ggplot(aes(x = year_init, y = nummer, color = NIVEL_DETALLADO))+
  geom_line()

df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  ggplot(aes(y = NIVEL_DETALLADO,  x = duracion/12))+
  geom_boxplot()+
  facet_wrap(~DESTINO)

jums <- df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  group_by(CARRERA, NIVEL_DETALLADO, DESTINO) |>
  summarise(nummer = n()) |>
  arrange(CARRERA,-nummer)

df_bec |>
  filter(NIVEL_DE_ESTUDIOS == "POSTGRADO") |>
  #  filter(PROVINCIA_RESIDENCIA_HOMOLOGADA == "EXTERIOR") |>
  group_by(NIVEL_DE_ESTUDIOS, NIVEL_DETALLADO, DESTINO, year_fini) |>
  summarise(nummer = n()) |>
  ggplot(aes(x = year_fini, y = nummer, group = NIVEL_DETALLADO, color = NIVEL_DETALLADO))+
  geom_line()+
  facet_wrap(~DESTINO)


oferta <- openxlsx::read.xlsx("datos/base-datos-abiertos_oferta-academica_05022025.xlsx",
                                        startRow = 14)

oferta_work <- oferta |>
  mutate(NIVEL_FORMACIÓN = recode(NIVEL_FORMACIÓN, 
                                  `EDUCACIÓN SUPERIOR DE POSGRADO O CUARTO NIVEL` = "CUARTO NIVEL O POSGRADO"),
         ESTADO = recode(ESTADO, 
                         `NO VIGENTE HABILITADA PARA REGISTRO DE TÍTULO` = "NO VIGENTE HABILITADO PARA REGISTRO DE TÍTULOS")) |>
  filter(NIVEL_FORMACIÓN == "CUARTO NIVEL O POSGRADO") |>
  mutate(nivel_detalle = case_when(grepl("DOCTORADO", NOMBRE_CARRERA) == T ~ "DOCTORADO",
                                   grepl("MAESTR", NOMBRE_CARRERA) == T ~ "MAESTRIA",
                                   grepl("ESPECIALIDAD", NOMBRE_CARRERA) == T ~ "ESPECIALIDAD",
                                   grepl("DIPLOMADO", NOMBRE_CARRERA) == T ~ "DIPLOMADO",
                                   grepl("ESPECIALIZACION", NOMBRE_CARRERA) == T ~ "ESPECIALIZACION",
                                   grepl("DIPLOMA SUPERIOR", NOMBRE_CARRERA) == T ~ "DIPLOMA SUPERIOR",
                                   T ~ "MAESTRIA"))

oferta_work |> group_by(nivel_detalle, ESTADO) |> summarise(nummer = n())  

oferta_work |> filter(nivel_detalle == "DOCTORADO") |> group_by(NIVEL_FORMACIÓN) |>
  summarise(nummer = n())

phds <- oferta_work |> filter(nivel_detalle == "DOCTORADO")

mscs <- oferta_work |> filter(nivel_detalle == "MAESTRIA")


table(mscs$ESTADO)

oferta_actual <- openxlsx::read.xlsx("datos/Oferta_académica.xlsx",
                              startRow = 16)

oferta_actual |> group_by(TIPO.DE.PROGRAMA) |> summarise(nummer = n())

phds <- oferta_actual |>
  filter(TIPO.DE.PROGRAMA == "DOCTORADO") 

xlsx::write.xlsx(phds, file = "datos/programas_phd.xlsx")

nubeluz <- phds |>
  mutate(`PROGRAMA./.CARRERA` = gsub("DOCTORADO", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub(" EN ", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("DE LAS", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("DOCTOR/A\\s*/?\\s*", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("DE LA", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("Y", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("DE LOS", "", `PROGRAMA./.CARRERA`),
         `PROGRAMA./.CARRERA` = gsub("DEL", "", `PROGRAMA./.CARRERA`))

wordcloud(words = nubeluz$`PROGRAMA./.CARRERA`, colors = brewer.pal(8, "Dark2"), min.freq = 1)



matriculados_raw <- openxlsx::read.xlsx("datos/Base_estadistica_matricula_UEP_15_23.xlsx",
                                    startRow = 15)

matriculados <- matriculados_raw |> 
  filter(NIVEL_FORMACION == "CUARTO NIVEL O POSGRADO")

matriculados <- matriculados |>
  mutate(NOMBRE_IES = recode(NOMBRE_IES,
                             `ESCUELA POLITECNICA NACIONAL` = "ESCUELA POLITÉCNICA NACIONAL",
                             `FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES` = "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO",
                             `UNIVERSIDAD ANDINA SIMON BOLIVAR` = "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR",
                             `UNIVERSIDAD SAN FRANCISCO DE QUITO` = "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ",
                             `ESCUELA SUPERIOR POLITECNICA DEL LITORAL` = "ESCUELA SUPERIOR POLITÉCNICA DEL LITORAL (ESPOL)",
                             `UNIVERSIDAD PARTICULAR DE ESPECIALIDADES ESPIRITU SANTO` = "UNIVERSIDAD DE ESPECIALIDADES ESPÍRITU SANTO - UEES",
                             `UNIVERSIDAD DE CUENCA` = "UNIVERSIDAD DE CUENCA",
                             `UNIVERSIDAD DEL AZUAY` = "UNIVERSIDAD DEL AZUAY",
                             `UNIVERSIDAD POLITECNICA SALESIANA` = "UNIVERSIDAD POLITÉCNICA SALESIANA - UPS",
                             `UNIVERSIDAD DE LAS FUERZAS ARMADAS (ESPE)` = "UNIVERSIDAD DE LAS FUERZAS ARMADAS-ESPE",
                             `UNIVERSIDAD CATOLICA DE SANTIAGO DE GUAYAQUIL` = "UNIVERSIDAD CATÓLICA DE SANTIAGO DE GUAYAQUIL (UCSG)",
                             `UNIVERSIDAD TECNICA PARTICULAR DE LOJA` = "UNIVERSIDAD TÉCNICA PARTICULAR DE LOJA",
                             `UNIVERSIDAD TECNICA DE MANABI` = "UNIVERSIDAD TÉCNICA DE MANABÍ",
                             `UNIVERSIDAD ESTATAL PENINSULA DE SANTA ELENA` = "UNIVERSIDAD ESTATAL PENÍNSULA DE SANTA ELENA - UPSE",
                             `UNIVERSIDAD NACIONAL DE EDUCACION UNAE` = "UNIVERSIDAD NACIONAL DE EDUCACIÓN",
                             `UNIVERSIDAD POLITECNICA ESTATAL DEL CARCHI` = "UNIVERSIDAD POLITÉCNICA ESTATAL DEL CARCHI",
                             `UNIVERSIDAD TECNOLOGICA EMPRESARIAL DE GUAYAQUIL` = "UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL",
                             `UNIVERSIDAD CENTRAL DEL ECUADOR` = "UNIVERSIDAD CENTRAL DEL ECUADOR",
                             `UNIVERSIDAD DE LAS AMERICAS` = "UNIVERSIDAD DE LAS AMÉRICAS"
                             )) |>
  filter(NOMBRE_IES %in% c("ESCUELA POLITÉCNICA NACIONAL", "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO", 
                           "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR",
                           "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ", "ESCUELA SUPERIOR POLITÉCNICA DEL LITORAL (ESPOL)",
                           "UNIVERSIDAD DE ESPECIALIDADES ESPÍRITU SANTO - UEES", 
                           "UNIVERSIDAD DE CUENCA", "UNIVERSIDAD DEL AZUAY", 
                           "UNIVERSIDAD POLITÉCNICA SALESIANA - UPS", "UNIVERSIDAD DE LAS FUERZAS ARMADAS-ESPE", 
                           "UNIVERSIDAD CATÓLICA DE SANTIAGO DE GUAYAQUIL (UCSG)", "UNIVERSIDAD TÉCNICA PARTICULAR DE LOJA", 
                           "UNIVERSIDAD TÉCNICA DE MANABÍ", "UNIVERSIDAD ESTATAL PENÍNSULA DE SANTA ELENA - UPSE",
                           "UNIVERSIDAD NACIONAL DE EDUCACIÓN", "UNIVERSIDAD POLITÉCNICA ESTATAL DEL CARCHI", 
                           "UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL", 
                           "UNIVERSIDAD CENTRAL DEL ECUADOR", "UNIVERSIDAD DE LAS AMÉRICAS")) 


matriculados <- matriculados |>
  filter(!grepl("MAESTRIA", NOMBRE_CARRERA) == T & 
           !grepl("DIPLOMADO", NOMBRE_CARRERA) == T &
           !grepl("ESPECIALIDAD EN ", NOMBRE_CARRERA) == T &
           !grepl("ESPECIALIZACION EN ", NOMBRE_CARRERA) == T &
           !grepl("ESPECIALIZACION  EN", NOMBRE_CARRERA) == T &
           !grepl("ESPECIALIZACION SUPERIOR EN", NOMBRE_CARRERA) == T &
           !grepl("ESPECIALIZACION MEDICA EN ", NOMBRE_CARRERA) == T)

matri_ind <- matriculados |>
  filter((grepl("HISTORIA DE LOS ANDES", NOMBRE_CARRERA) == T & 
            NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO") |
           (grepl("ECONOMIA DEL DESARROLLO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO") |
           (grepl("POLITICAS PUBLICAS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO") |
           (grepl("ESTUDIOS LATINOAMERICANOS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR") |
           (grepl("ESTUDIOS INTERNACIONALES", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO") |
           (grepl("GESTION TECNOLOGICA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("MICROBIOLOGIA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ") |
           (grepl("TECNOLOGIA DE ALIMENTOS", NOMBRE_CARRERA) == T & 
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("MATEMATICA APLICADA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("INFORMATICA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("COMPUTACIONALES APLICADAS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA SUPERIOR POLITÉCNICA DEL LITORAL (ESPOL)")|
           (grepl("INGENIERIA ELECTRICA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("CIENCIAS DE LA MECANICA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "ESCUELA POLITÉCNICA NACIONAL" )|
           (grepl("SALUD COLECTIVA AMBIENTE Y SOCIEDAD", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR")|
           (grepl("ADMINISTRACION Y GESTION DE LAS ORGANIZACIONES", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD DE ESPECIALIDADES ESPÍRITU SANTO - UEES") |
           (grepl("RECURSOS NATURALES RENOVABLES", NOMBRE_CARRERA) == T &
              (NOMBRE_IES == "UNIVERSIDAD DE CUENCA"|NOMBRE_IES == "UNIVERSIDAD DEL AZUAY")) |
           (grepl("EDUCACION", NOMBRE_CARRERA) == T &
              (NOMBRE_IES == "UNIVERSIDAD DE ESPECIALIDADES ESPÍRITU SANTO - UEES"|
                 NOMBRE_IES == "UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL"|
                 NOMBRE_IES == "UNIVERSIDAD NACIONAL DE EDUCACIÓN"|
                 NOMBRE_IES == "UNIVERSIDAD POLITÉCNICA SALESIANA - UPS"|
                 NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR")) |
           (grepl("ADMINISTRACION", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR") |
           (grepl("COMPUTACION APLICADA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD DE CUENCA") |
           (grepl("CIENCIAS COMPUTACIONALES", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD POLITÉCNICA SALESIANA - UPS") |
           (grepl("DOCTORADO EN ADMINISTRACION", NOMBRE_CARRERA) == T &
              (NOMBRE_IES == "UNIVERSIDAD DE LAS FUERZAS ARMADAS-ESPE" | NOMBRE_IES == "UNIVERSIDAD CATÓLICA DE SANTIAGO DE GUAYAQUIL (UCSG)")) |
           (grepl("SOSTENIBILIDAD TERRITORIAL", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD DE CUENCA") |
           (grepl("DOCTORADO EN DERECHO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD CATÓLICA DE SANTIAGO DE GUAYAQUIL (UCSG)") |
           (grepl("CIENCIAS DE LA COMUNICACION Y PERIODISMO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD CATÓLICA DE SANTIAGO DE GUAYAQUIL (UCSG)") |
           (grepl("DOCTORADO EN ESTUDIOS POLITICOS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO")|
           (grepl("INGENIERIA", NOMBRE_CARRERA) == T&
              NOMBRE_IES == "ESCUELA SUPERIOR POLITÉCNICA DEL LITORAL (ESPOL)")|
           (grepl("DOCTORADO EN QUIMICA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD TÉCNICA PARTICULAR DE LOJA")|
           (grepl("AGROPECUARIAS", NOMBRE_CARRERA) == T &
              (NOMBRE_IES == "UNIVERSIDAD TÉCNICA DE MANABÍ"|NOMBRE_IES == "UNIVERSIDAD ESTATAL PENÍNSULA DE SANTA ELENA - UPSE"))|
           (grepl("DOCTORADO EN COMUNICACION", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR")|
           (grepl("DOCTORADO EN POLITICAS PUBLICAS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD POLITÉCNICA ESTATAL DEL CARCHI")|
           (grepl("EN ADMINISTRACION", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD TÉCNICA DE MANABÍ")|
           (grepl("ADMINISTRACION DE EMPRESAS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL")|
           (grepl("GESTION SUSTENTABLE DE AGUA Y RIEGO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD CENTRAL DEL ECUADOR")|
           (grepl("ECOLOGIA Y SOLUCIONES GLOBALES", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ")|
           (grepl("BIOCIENCIAS APLICADAS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD DE LAS AMÉRICAS") |
           (grepl("MATERIALES AVANZADOS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ") |
           (grepl("LITERATURA LATINOAMERICANA", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR") |
           (grepl("ESTUDIOS LATINOAMERICANOS", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR") |
           (grepl("DOCTORADO EN DERECHO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "UNIVERSIDAD ANDINA SIMÓN BOLÍVAR") |
           (grepl("ECONOMIA DEL DESARROLLO", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO") |
           (grepl("DOCTORADO EN CIENCIAS SOCIALES", NOMBRE_CARRERA) == T &
              NOMBRE_IES == "FACULTAD LATINOAMERICANA DE CIENCIAS SOCIALES, FLACSO")
           ) |>
  filter(!NOMBRE_CARRERA %in% c("INGENIERIA BIOMEDICA", "INGENIERIA NAVAL", "INGENIERIA CIVIL"))


matri_ind_sum <- matri_ind |>
  select(year = AÑO, NOMBRE_IES, NOMBRE_CARRERA, CAMPO_AMPLIO, TOTAL) |>
  mutate(CAMPO_AMPLIO = case_when(NOMBRE_CARRERA == "DOCTORADO EN CIENCIA Y TECNOLOGIA DE ALIMENTOS" ~ "CIENCIAS NATURALES, MATEMATICAS Y ESTADISTICA",
                                  NOMBRE_CARRERA == "DOCTORADO EN ESTUDIOS LATINOAMERICANOS" ~ "CIENCIAS SOCIALES, PERIODISMO, INFORMACION Y DERECHO",
                                  NOMBRE_CARRERA == "DOCTORADO EN INFORMATICA" ~ "TECNOLOGIAS DE LA INFORMACION Y LA COMUNICACION (TIC)",
                                  NOMBRE_CARRERA == "DOCTORADO EN INGENIERIA ELECTRICA" ~ "INGENIERIA, INDUSTRIA Y CONSTRUCCION",
                                  NOMBRE_CARRERA == "DOCTORADO EN MATEMATICA APLICADA" ~ "CIENCIAS NATURALES, MATEMATICAS Y ESTADISTICA",
                                  NOMBRE_CARRERA == "ADMINISTRACION DE EMPRESAS" ~ "ADMINISTRACION DE EMPRESAS Y DERECHO",
                                  NOMBRE_CARRERA == "POLITICAS PUBLICAS" ~ "CIENCIAS SOCIALES, PERIODISMO E INFORMACION",
                                  T ~ CAMPO_AMPLIO)) |>
  mutate(CAMPO_AMPLIO = recode(CAMPO_AMPLIO, 
                               `ADMINISTRACION` = "ADMINISTRACIÓN",
                               `ADMINISTRACION DE EMPRESAS Y DERECHO` = "ADMINISTRACIÓN DE EMPRESAS Y DERECHO",
                               `CIENCIAS NATURALES, MATEMATICAS Y ESTADISTICA` = "CIENCIAS NATURALES, MATEMÁTICAS Y ESTADÍSTICA",
                               `CIENCIAS SOCIALES, PERIODISMO E INFORMACION` = "CIENCIAS SOCIALES, PERIODISMO E INFORMACIÓN",
                               `CIENCIAS SOCIALES, PERIODISMO, INFORMACION Y DERECHO` = "CIENCIAS SOCIALES, PERIODISMO, INFORMACIÓN Y DERECHO" ,
                               `EDUCACION` = "EDUCACIÓN",
                               `INGENIERIA, INDUSTRIA Y CONSTRUCCION` = "INGENIERÍA, INDUSTRIA Y CONSTRUCCIÓN",
                               `TECNOLOGIAS DE LA INFORMACION Y LA COMUNICACION (TIC)` = "TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)"
                               )) |>
  filter(!NOMBRE_CARRERA %in% c("EDUCACION ESPECIAL", 
                                "EDUCACION INCLUSIVA",
                                "EDUCACION INTERCULTURAL BILINGUE",
                                "EDUCACION Y NUEVAS TECNOLOGIAS DE LA INFORMACION Y LA COMUNICACION",
                                "EDUCACION, TECNOLOGIA E INNOVACION",
                                "INNOVACION EN EDUCACION",
                                "INNOVACION EN EDUCACION PARA LA ENSEÑANZA DE LAS CIENCIAS SOCIALES Y HUMANIDADES",
                                "INVESTIGACION EN EDUCACION",
                                "TECNOLOGIAS DE LA INFORMACION Y COMUNICACION PARA LA EDUCACION",
                                "GESTION DE LA CALIDAD EN EDUCACION")) |>
  filter(!NOMBRE_IES %in% c("UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL"))

matri_ind_sum |>
  filter(CAMPO_AMPLIO == "EDUCACIÓN") |> 
  group_by(NOMBRE_IES, NOMBRE_CARRERA, year) |> 
  summarise(nummer = sum(TOTAL))

matri_ind_sum |>
  filter(NOMBRE_IES == "UNIVERSIDAD TECNOLÓGICA EMPRESARIAL DE GUAYAQUIL") |> 
  group_by(NOMBRE_CARRERA, year) |> 
  summarise(nummer = sum(TOTAL))

saveRDS(matri_ind_sum, file = "datos/matriculados.rds")

library(geomtextpath)

matri_ind_sum |> 
  group_by(year, CAMPO_AMPLIO) |> 
  summarise(nummer = sum(TOTAL)) |> 
  ggplot(aes(x=year, y = nummer)) + 
  geom_line(linetype = "dashed")+
  facet_wrap(~CAMPO_AMPLIO)+
  #geom_textpath(aes(label = CAMPO_AMPLIO))+
  ggrepel::geom_label_repel(aes(label = nummer))+
  theme_bw()+
  theme(legend.position = "none")

jumchis <- matri_ind_sum |>
  group_by(NOMBRE_CARRERA, CAMPO_AMPLIO) |>
  summarise(nummer = sum(TOTAL))

sort(unique(phds$CAMPO.AMPLIO))
sort(unique(jumchis$CAMPO_AMPLIO))

noregistra <- matriculados |>
  filter(CAMPO_AMPLIO == "NO_REGISTRA")

matri_phds <- matriculados |>
  filter(grepl("DOCTOR", NOMBRE_CARRERA)==T)

table(matriculados$CAMPO_AMPLIO)

mach1 <- matriculados |>
  select(NOMBRE_IES, TIPO_FINANCIAMIENTO, NOMBRE_CARRERA, MODALIDAD, PROVINCIA_SEDE, CANTON_SEDE) |>
  distinct()


saveRDS(matriculados, file = "datos/matriculados.rds")
matriculados <- readRDS("datos/matriculados.rds")
table(matriculados$NIVEL_FORMACION)

# matching hasta donde se avance






matriculados_resumen <- matriculados |>
  group_by(AÑO, NOMBRE_IES, TIPO_FINANCIAMIENTO, NOMBRE_CARRERA, NIVEL_FORMACION, MODALIDAD,
           CAMPO_AMPLIO, CAMPO_ESPECIFICO, CAMPO_DETALLADO, TIPO_SEDE, PROVINCIA_SEDE,CANTON_SEDE) |>
  summarise(nummer = sum(TOTAL)) |>
  ungroup()|>
  filter(NIVEL_FORMACION == "CUARTO NIVEL O POSGRADO") |>
  select(year = AÑO, NOMBRE_IES, TIPO_FINANCIAMIENTO, NOMBRE_CARRERA, NIVEL_FORMACION, MODALIDAD,
         CAMPO_AMPLIO, CAMPO_ESPECIFICO, CAMPO_DETALLADO, TIPO_SEDE, PROVINCIA_SEDE,CANTON_SEDE, nummer)

matri_phds <- matriculados_resumen |>
  filter(grepl("DOCTOR", NOMBRE_CARRERA)== T)
