library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(ggh4x)

universities <- data.frame(
  nombre = c(
    "ESCUELA POLITECNICA NACIONAL", #ok
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
  clase = c(
    "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", 
    "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", "Public", 
    "Public", "Public", "Public", "Public", "Public", "Private", "Private", "Private", "Private", "Private", 
    "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", "Private", 
    "Private", "Private", "Private", "Private", "Private", "Private", "Public", "Public", "Private", "Private", 
    "Private", "Private", "Public", "Private", "Public", "Public", "Public", "Public", "Public", "Private", 
    "Public", "Private"
  )
)

# xlsx::write.xlsx(universities, file = "datos/universidades.xlsx")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

crono <- data.frame(
  milestones = c("LOES 2000", 
                 "LOES 2010", 
                 "Proyecto de Fortalecimiento", 
                 "Reforma 2018", 
                 "Reglamento Escalafón 2022", 
                 "Plazo transitoria 13ra.", 
                 "Plazo transitoria light", 
                 "Fin Becas Abiertas y Excelencia",
                 "Actualización al Proyecto de Fortalecimiento",
                 "Actualidad",
                 "Reglamento Doctoral"),
  event_type = c("LOES", "LOES", "Becas", "LOES", "LOES", "Transitoria", "Transitoria", 
                 "Becas", "Becas", "Actualidad", "LOES"),
  year = c(2000, 2010, 2011, 2018, 2022, 2017, 2023, 2019, 2021, 2025, 2024)
) |> arrange(year)

positions <- c(0.5, -0.5, 1.0, -1.0, 1.25, -1.25, 1.5, -1.5) 

directions <- c(1, -1)

line_pos <- data.frame(
  year = crono$year,
  milestones = crono$milestones,
  position = rep(positions, length.out = length(crono$year)),
  direction = rep(directions, length.out = length(crono$year))) 

crono <- left_join(crono, line_pos, by = c("milestones", "year")) |>
  mutate(text_position = (abs(position) + 0.15)*direction ,
         event_type = factor(event_type))

my_plt <- gg_color_hue(length(unique(crono$event_type)))
names(my_plt) <- unique(crono$event_type)

timeline <- function(df,anio){
  
  df$line_style <- ifelse(grepl("transitoria", df$milestones, ignore.case = TRUE), "dashed", "solid")
  
  plt <- df |>
    ggplot(aes(x = year, y = position, col = event_type, label = milestones)) +
    theme_classic()+
    geom_hline(yintercept = 0, linewidth = 1.25)+
    geom_segment(aes(y = position, yend = 0, xend = year, linetype = line_style),
                 color = "black", linewidth = 1)+
    geom_point(aes(y = position), size = 4)+
    xlim(c(1999, 2025))+
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position = "bottom"
    ) +
    geom_label(data = df, aes(x=year, y = -0.15, label = year), size = 4, vjust=0.5, angle= 45, color = "black")+
    geom_text(aes(y = text_position,  label = milestones), size = 4, hjust = 1)+
    labs(color = "", linetype = "")+
    scale_color_manual(values = my_plt) +
    scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"))+
    guides(linetype = "none")+
    geom_rect(aes(xmin = anio - 0.5, xmax = anio + 0.5, ymin = -Inf, ymax = Inf), 
              alpha = 0.05, color = "white", fill = "yellow")
  
  return(plt)
}

timeline_all <- function(df){
  
  df$line_style <- ifelse(grepl("transitoria", df$milestones, ignore.case = TRUE), "dashed", "solid")
  
  plt <- df |>
    ggplot(aes(x = year, y = position, col = event_type, label = milestones)) +
    theme_classic()+
    geom_hline(yintercept = 0, linewidth = 1.25)+
    geom_segment(aes(y = position, yend = 0, xend = year, linetype = line_style),
                 color = "black", linewidth = 1)+
    geom_point(aes(y = position), size = 4)+
    xlim(c(1999, 2025))+
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position = "bottom"
    ) +
    geom_label(data = df, aes(x=year, y = -0.15, label = year), size = 4, vjust=0.5, angle= 45, color = "black")+
    geom_text(aes(y = text_position,  label = milestones), size = 4, hjust = 1)+
    labs(color = "", linetype = "")+
    scale_color_manual(values = my_plt) +
    scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"))+
    guides(linetype = "none")
  
  return(plt)
}