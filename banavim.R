### App BANAVIM ###

# 1.- Set up ----
## a) Cargar paquetes, seleccionar idioma y fuente ----
library(shiny)
library(tidyverse)
library(lubridate) 
library(janitor)
library(stringr)
library(plotly)
library(fuzzyjoin)
library(zoo)
library(shinyWidgets)
library(scales)
library(shinydashboard)
library(extrafont)
library(sysfonts)
library(DT)
library(leaflet)
library(sf)
library(scales)

library(shiny)
library(tidyverse)
library(lubridate) 
library(janitor)
library(stringr)
library(plotly)
library(fuzzyjoin)
library(zoo)
library(shinyWidgets)
library(scales)
library(shinydashboard)
library(extrafont)
library(sysfonts)
library(DT)
library(leaflet)
library(sf)
library(googledrive)
library(googlesheets4)
library(shiny)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(RColorBrewer) 
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(shinyjs)
library(leaflet)
library(mxmaps)
library(shinyWidgets)

# Idioma #
Sys.setlocale(locale="es_ES.UTF-8")

# Fuente 
# font_paths()
# font_files()
# font_add("Nutmeg-Regular", "Nutmeg-Regular.ttf")
# font_families()

# setwd("~/Documents/GitHub/PAIMEF/BANAVIM")

## b) Leer base de datos----
# 
# ### i) Base de casos
# base_casos <- data.table::fread("Base de Datos Jalisco corte septiembre 2023 1.csv",
#                                 encoding = "UTF-8")
# # base_casos <- read.csv("Base de Datos Jalisco corte septiembre 2023 1.csv",
# #                        fileEncoding = "LATIN1"
# #                                 )
# 
# ### ii) Base de personas agresoras
# # Base de personas agresoras
# base_agre <- data.table::fread("Base de Datos Jalisco corte septiembre 2023 2.csv",
#                                encoding = "UTF-8")
# 
# 
# ### iii) Base de ordenes
# base_ordenes <- data.table::fread("Base de Datos Jalisco corte septiembre 2023 3.csv",
#                                   encoding = "UTF-8")
# 
# ### iv) Base de servicios
# base_servicios <- data.table::fread("Base de Datos Jalisco corte septiembre 2023 4.csv",
#                                     encoding = "UTF-8")
# 

data <- read_rds("data_banavim.rds")

base_agre_clean <- data$base_agre_clean
base_casos_clean <- data$base_casos_clean
base_ordenes_clean <- data$base_ordenes_clean
base_servicios_clean <- data$base_servicios_clean
### v) Base de población
poblacion <- data.table::fread("pob_mun_jalisco.csv") %>% 
  filter(variable == "pob_muj") 

### vi) Mapa
# jalisco_shape <- read_sf("Municipios/Municipios.shp") %>% 
#   clean_names() %>% 
#   filter(estado == "Jalisco")  %>% 
#   mutate(municipios = toupper(municipios)) %>% 
#   stringdist_join(., poblacion, 
#                   by= c("municipios" = "municipio"),
#                   mode='left',
#                   method = "jw", 
#                   max_dist=.08, 
#                   distance_col='dist')  %>% 
#   select(-variable, -valor)
# 
# jalisco_shape <- st_transform(jalisco_shape, 
#                               st_crs("+proj=longlat +datum=WGS84 +no_defs"))
# 
jalisco_shape <- read_rds("municipios_shp.rds")

## c) Limpiar datos ----
clean_acentos <- function(palabra){
  
  palabra <- palabra %>% 
    toupper(.) %>% 
    gsub("Ã¡", "Á", .) %>% 
    gsub("Ã³", "Ó", .) %>% 
    gsub("Ã±", "Ñ", .) %>% 
    gsub("Ãº", "Ú", .) %>% 
    gsub("Ã©", "É", .) %>% 
    gsub("Ã", "Í", .)  %>% 
    str_squish(.) %>% 
    str_replace_all(., "\\s", " ")
  
}

# Función de dropdownMenu y checkboxes
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # Contenido
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # Apariencia
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

# Limpiar base de casos
# base_casos_clean <- base_casos %>% 
#   # Limpiar nombres
#   clean_names() %>% 
#   # Seleccionar variables de interés
#   select(euv, 
#          fecha_de_recepcion,
#          dep_exp,
#          fecha_hechos,
#          municipio_hecho,
#          municipio_del_domicilio, 
#          rango_de_edades,
#          econ_a3mica, 
#          fa_sica,
#          patrimonial, 
#          psicol_a3gica, 
#          sexual, 
#          otro, 
#          descripcion_tipo_violencia,
#          modalidad_de_la_violencia,
#          va_nculo_con_victima,
#          detalle_de_va_nculo_con_victima,
#          pertenece_etnia,
#          migrante,
#          descripcion_del_lugar,
#          esta_embarazada,
#          estado_civil,
#          numero_de_hijos,
#          starts_with("vict"),
#          conocimiento_de_autoridad,
#          edad,
#          conocimiento_de_autoridad
#   ) %>% 
#   select(-victima_de_delincuencia) %>% 
#   # Limpiar acentos #
#   mutate(municipio_hecho_clean = clean_acentos(municipio_hecho)) %>% 
#   # Limpiar fechas #
#   mutate(fecha_de_recepcion = as.Date(fecha_de_recepcion, format = "%d/%m/%y")) %>% 
#   mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%y")) %>% 
#   # Unir población para ubicar municipios de Jalisco
#   stringdist_join(., poblacion, 
#                   by= c("municipio_hecho_clean" = "municipio"),
#                   mode='left',
#                   method = "jw", 
#                   max_dist=.08, 
#                   distance_col='dist') %>% 
#   mutate(municipio_hecho_clean = case_when(is.na(dist) ~ "MUNICIPIOS FUERA DE JALISCO",
#                                            !is.na(dist) ~ municipio_hecho_clean)) %>% 
#   mutate(dep_exp = str_to_title(dep_exp)) %>% 
#   mutate(dep_exp = gsub("ã‘", "ñ", dep_exp))

# Dependencias 
dependencias_base_casos <- 
  c(str_to_upper("Todas las dependencias"), 
    str_to_upper(unique(base_casos_clean$dep_exp)[order(unique(base_casos_clean$dep_exp))]))

# base_casos_clean$dep_exp[base_casos_clean$dep_exp=="Comisaria De Seguridad Pãºblica De Talpa De Allende"] <- "Comisaria De Seguridad Pública De Talpa De Allende"
# Limpiar base de personas agresoras
# base_agre_clean <- base_agre %>% 
#   # Limpiar nombres
#   clean_names() %>% 
#   # Seleccionar variables de interés
#   select(euv, 
#          edad,
#          genero,
#          escolaridad, 
#          fecha_hechos,
#          municipio,
#          genero,
#          droga_alcohol, 
#          droga_drogas_ilegales,
#          posee_algun_tipo_de_arma, 
#          portaba_dicha_arma, 
#          chacos, 
#          macanas, 
#          objeto_punzo_cortante,
#          machete,
#          proyectil,
#          arma_fuego_corta,
#          arma_fuego_larga,
#          otra_fuego_larga,
#          vinculo_con_va_ctima, 
#          lugar_hechos,
#          starts_with("prin"),
#          starts_with("fte")) %>% 
#   left_join(., base_casos_clean %>% select(euv, dep_exp) %>% unique()) %>% 
#   # Limpiar acentos #
#   mutate(municipio_hecho_clean = clean_acentos(municipio)) %>% 
#   # Limpiar fechas #
#   mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%y")) %>% 
#   # Unir población para ubicar municipios de Jalisco
#   stringdist_join(., poblacion, 
#                   by= c("municipio_hecho_clean" = "municipio"),
#                   mode='left',
#                   method = "jw", 
#                   max_dist=.08, 
#                   distance_col='dist') %>% 
#   mutate(municipio_hecho_clean = case_when(is.na(dist) ~ "MUNICIPIOS FUERA DE JALISCO",
#                                            !is.na(dist) ~ municipio_hecho_clean))
# 
# # Limpiar base de órdenes de protección
# base_ordenes_clean <- base_ordenes %>% 
#   clean_names() %>% 
#   select(euv, 
#          fecha_de_recepcion, 
#          desc_tipo_orden, 
#          municipio_evento, 
#          autoridad_emisora) %>% 
#   left_join(., base_casos_clean %>% select(euv, dep_exp) %>% unique()) %>% 
#   # Limpiar acentos #
#   mutate(municipio_hecho_clean = clean_acentos(municipio_evento)) %>% 
#   # Limpiar fechas #
#   mutate(fecha_de_recepcion = as.Date(fecha_de_recepcion, format = "%d/%m/%y")) %>% 
#   mutate(fecha_de_recepcion = as_date(fecha_de_recepcion, format = "%d/%m/%y")) %>% 
#   # Unir población para ubicar municipios de Jalisco
#   stringdist_join(., poblacion, 
#                   by= c("municipio_hecho_clean" = "municipio"),
#                   mode='left',
#                   method = "jw", 
#                   max_dist=.08, 
#                   distance_col='dist') %>% 
#   mutate(municipio_hecho_clean = case_when(is.na(dist) ~ "MUNICIPIOS FUERA DE JALISCO",
#                                            !is.na(dist) ~ municipio_hecho_clean))
# 
# # Limpiar base de casos
# base_servicios_clean <- base_servicios %>% 
#   # Limpiar nombres
#   clean_names() %>% 
#   rename(fecha_captura = fecha_cap_servicio) %>% 
#   # Seleccionar variables de interés
#   select(euv, 
#          fecha_captura,
#          mun,
#          edad_vict,
#          serviciodetalle,
#          estatus, 
#          estado_civil_vict,
#          ususervicio,
#          tiposervicio, 
#          dependenciaquebrindoservicio) %>% 
#   # Limpiar acentos #
#   mutate(municipio_hecho_clean = clean_acentos(mun)) %>% 
#   # Limpiar fechas #
#   mutate(fecha_captura = as.Date(fecha_captura, format = "%d/%m/%y")) %>%  
#   # Unir población para ubicar municipios de Jalisco
#   stringdist_join(., poblacion, 
#                   by= c("municipio_hecho_clean" = "municipio"),
#                   mode='left',
#                   method = "jw", 
#                   max_dist=.08, 
#                   distance_col='dist') %>% 
#   mutate(municipio_hecho_clean = case_when(is.na(dist) ~ "MUNICIPIOS FUERA DE JALISCO",
#                                            !is.na(dist) ~ municipio_hecho_clean)) %>% 
#   select(-variable, -dist, -valor)

# base_servicios_clean$dependenciaquebrindoservicio[base_servicios_clean$dependenciaquebrindoservicio=="COMISIÃ³N EJECUTIVA ESTATAL DE COMISIÃ³N A VÃ­CTIMAS"] <- "COMISIÓN EJECUTIVA ESTATAL DE COMISIÓN A VÍCTIMAS"

dependencias_base_servicios <- 
  str_to_upper(c("Todas las dependencias", 
    unique(base_servicios_clean$dependenciaquebrindoservicio)[
      order(unique(base_servicios_clean$dependenciaquebrindoservicio))]))

# Crear lista de municipios en base
municipios_en_base <- c(str_to_upper("Todos los municipios de Jalisco"), 
                        str_to_upper(unique(base_casos_clean$municipio_hecho_clean)[
                          order(unique(base_casos_clean$municipio_hecho_clean))]))

municipios_en_base <- municipios_en_base[c(-2)]

# municipios_en_base <- municipios_en_base[-which(grepl("Fuera ", municipios_en_base))]

municipios_en_base <- c(municipios_en_base, str_to_upper("Municipios fuera de Jalisco"))

# base_casos_clean$municipio_hecho_clean <- str_to_upper(base_casos_clean$municipio_hecho_clean)
# base_agre_clean$municipio_hecho_clean <- str_to_upper(base_agre_clean$municipio_hecho_clean)
# base_ordenes_clean$municipio_hecho_clean <- str_to_upper(base_ordenes_clean$municipio_hecho_clean)
# base_servicios_clean$municipio_hecho_clean <- str_to_upper(base_servicios_clean$municipio_hecho_clean)
# 
# base_casos_clean$municipio_hecho_clean[base_casos_clean$municipio_hecho_clean=="SANTA MARÍ­A DE LOS Í\u0081NGELES"] <- "SANTA MARÍ­A DE LOS ÁNGELES"
# base_agre_clean$municipio_hecho_clean[base_agre_clean$municipio_hecho_clean=="SANTA MARÍ­A DE LOS Í\u0081NGELES"] <- "SANTA MARÍ­A DE LOS ÁNGELES"
# base_ordenes_clean$municipio_hecho_clean[base_ordenes_clean$municipio_hecho_clean=="SANTA MARÍ­A DE LOS Í\u0081NGELES"] <- "SANTA MARÍ­A DE LOS ÁNGELES"
# base_servicios_clean$municipio_hecho_clean[base_servicios_clean$municipio_hecho_clean=="SANTA MARÍ­A DE LOS Í\u0081NGELES"] <- "SANTA MARÍ­A DE LOS ÁNGELES"

# 2.- Definir UI ----
ui <- dashboardPage(
  
  
  # Estilos
  
  
  # Título de la aplicación
  header = dashboardHeader(title = "Herramienta de análisis - BANAVIM",
                           titleWidth = 330),
  
  # # Logo parte superior derecha
  # tags$li(class = "dropdown",
  #         tags$style(".main-header {height: 50px}"),
  #         a(img(src = 'logo-SISEHM.png',
  #               title = "logo", height = "40px"),
  #           style = "padding-top:10px; padding-bottom:10px;"),
  #         class = "dropdown")),
  
  
  # Layout de barra lateral
  sidebar = dashboardSidebar(
    width = 350,
    
    # Pestañas laterales
    sidebarMenu(
      menuItem("Información general", tabName = "info_general"),
      menuItem("Evolución en el tiempo", tabName = "evolucion"),
      menuItem("Características de los casos", tabName = "variables"),
      menuItem("Características de las personas agresoras", tabName = "agresoras"),
      # menuItem("Interacción de variables", tabName = "interaccion"),
      menuItem("Órdenes de protección", tabName = "ordenes"),
      menuItem("Servicios otorgados", tabName = "servicios"))),
  
  body = dashboardBody(
    includeCSS("www/style.css"),
    
    tags$head(
      tags$style(HTML("
        
        
      .p-0 {
       padding: 0px!important;
      }
      .small-box h3 {
    font-size: 38px;
    font-weight: 700;
    margin: 0 0 10px 0;
    white-space: normal!important;
    padding: 0;
    }
    @media (min-width: 768px) {
  .d-flex {
    display: flex;
  }
    }
    .small-box{
    border-radius: 2px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
    height: calc(100% - 20px);
    }
    # .html-widget{min-width: 300px;
    # }
    .mb-2{ 
    margin-bottom:20px;
    }
    .p-2{ 
    padding: 20px;     
    }x|
    #table_muertes{overflow: scroll; 
    }   
    
  .small-box.bg-olive{
   background-color: #b06497 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-fuchsia{
   background-color: #B14C71 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon{
     background-color: #8F5199 !important; 
   color: white !important;       
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-yellow{
   background-color: #7e5691 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
   }
   
      .small-box.bg-red{
   background-color: #854858 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
      }
   
      .small-box.bg-navy{
   background-color: #702f2b !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
      }
   
      .small-box.bg-aqua {
   background-color: #488269 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
   }
   
    
                        ")),
      tags$script(HTML("window.addEventListener('message', event => {
    // IMPORTANT: check the origin of the data!
    console.log('recibi un mensaje', event);
    if (event.origin.includes('https://igualdad.jalisco.gob.mx')) {
        // The data was sent from your site.
        // Data sent with postMessage is stored in event.data:

        let height = 0;
        if (document.body) {
            if (document.body.scrollHeight) {
                height= document.body.scrollHeight;
            }
        }

        event.source.postMessage(height, event.origin);
    } 

    return;
});")),
      tags$script('
           var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
    
    add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
    busy_start_up(
      loader = spin_epic("flower", color = "#7e3794"),
      text = "Cargando",
      timeout = 1500,
      color = "#7e3794",
      background = " white"),
    
    
    tabItems(
      
      ## 2.1.- Información general (UI)----
      tabItem(tabName = "info_general", 
              
              ### a) Título----
              
              fluidRow(align = "center",
                       h1("Casos de Violencia contra las Mujeres", 
                          align = "center"),
                       h1(""),
                       h1("")),
              
              ### b) Cajas de valor----
              fluidRow(align = "center",
                       column(width = 6,
                              valueBoxOutput("total_casos", width = 18)),
                       column(width = 6, 
                              valueBoxOutput("tasa_casos", width = 18))),
              
              ### c) Filtros de fecha y depdendencia----
              
              fluidRow( 
                h3("Municipios con mayor incidencia (top 10)", align = "center"),
                
                column(width = 4,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_1",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_casos)),
                column(width = 4,
                       align = "center",
                       dateInput("fecha_hecho_inicio",
                                 "Ingresa la fecha inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                column(width = 4,
                       align = "center",
                       dateInput("fecha_hecho_final",
                                 "Ingresa la fecha final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30")))),
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output1")))), 
              
              ### d) Gráfica----
              fluidRow(align = "center",
                       h1(""),
                       h1(""),
                       # h3("Municipios con mayor incidencia (top 10)", align = "center"),
                       h1(""),
                       tags$div(
                         style = "width: 100%;",
                         tags$div(
                           style = "margin: auto; width: 120px;",
                           materialSwitch(
                             inputId = "switch_tasa",
                             label = "Análisis por tasa",
                             inline = T,
                             status = "primary",
                             width = "800px"
                           ))),
                       column(width = 12,
                              box(width = 12,
                                  align = "center",
                                  h1(""),
                                  plotlyOutput("municipios_plot"),
                                  h1(""),
                                  h6("Fuente: BANAVIM 2023; INEGI 2020.", align = "right"))),
                       column(width = 12,
                              h1(""),
                              h3("Mapa", align = "center"),
                              h1(""),
                              leafletOutput("mapa_1")),
                       column(width = 12,
                              box(width = 12,
                                  h3("Resto de municipios en Jalisco", align = "center"),
                                  DT::dataTableOutput("tabla_municipios"))))
              
      ), # Cerrar tab Item
      
      ## 2.2.- Evolución (UI)----
      tabItem(tabName = "evolucion",
              
              ### a) Título de evolución ----
              
              fluidRow(
                align = "center",
                h1(""),
                h1("Evolución de casos de violencia contra las mujeres"),
                h1(""),
                h1("")),
              
              ### b) Cajas de valor----
              
              fluidRow(align = "center",
                       column(width = 3,
                              valueBoxOutput("caja_mun_total",
                                             width = 15)),
                       column(width = 3,
                              valueBoxOutput("caja_mun_porc",
                                             width = 15)),
                       column(width = 3,
                              valueBoxOutput("caja_mun_total_d",
                                             width = 15)),
                       column(width = 3,
                              valueBoxOutput("caja_mun_porc_d",
                                             width = 15))),
              
              
              ### c) Filtros de información y periodo de análisis----
              
              fluidRow( 
                column(width = 3,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_2",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_casos)),
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_inicio_2",
                                 "Ingresa la fecha inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_final_2",
                                 "Ingresa la fecha final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30"))),
                
                column(width = 3,
                       align = "center",
                       radioButtons("tempo_analisis",
                                    "Temporalidad de análisis",
                                    choices = c("Últimos 14 días",
                                                "Últimos 60 días",
                                                "Último año"),
                                    selected = "Últimos 14 días"))),
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output2")))), 
              
              ### d) Gráfica ----
              
              fluidRow(column( width = 12,
                               box(width = 12,
                                   h1(""),
                                   plotlyOutput("evolucion_plot"),
                                   h1(""),
                                   h6("Fuente: BANAVIM 2023.", align = "right")))),
              
              fluidRow(column(width = 12,
                              box(width = 12,
                                  h3("Cambio en número de casos por municipio", align = "center"),
                                  DT::dataTableOutput("tabla_cambio_municipios"))))
              
      ), # Cerrar Tab Item
      
      ## 2.3- Características de los casos (UI)----
      tabItem(tabName = "variables",
              
              ### a) Títulos----
              fluidRow(
                align = "center",
                h1(""),
                h1("Características de los de casos de violencia contra las mujeres"),
                h1(""),
                h1("")),
              
              ### b) Cajas de valor----
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_tipo_violencia")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_modalidad_violencia")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_rango_edad"))),
              
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_vinculo")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_lugar")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_edo_civil"))),
              
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_numero_de_hijos")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_actividad_victima")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_conocimiento_a"))),
              
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_etnia")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_migrante")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_embarazada"))),
              
              
              
              ### c) Filtros de información y periodo de análisis----
              fluidRow(
                column(width = 3,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_3",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_casos)),
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_inicio_3",
                                 "Ingresa la fecha inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_final_3",
                                 "Ingresa la fecha final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30"))),
                
                column(width = 3,
                       align = "center",
                       selectInput("municipio_seleccionado", 
                                   "Ingresa el municipio:", 
                                   choices = municipios_en_base, 
                                   selected = "Todos los municipios de Jalisco"))),
              
              
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output3")))), 
              
              ### d) Primeras dos gráficas----
              
              fluidRow(align = "center",
                       box(title = h3("Tipo de violencia", align = "center"),
                           plotlyOutput("tipo_violencia_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Modalidad de violencia", align = "center"),
                           plotlyOutput("tipo_modalidad_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### d) Segundas dos gráficas----
              
              fluidRow(align = "center",
                       box(title = h3("Rango de edad", align = "center"),
                           plotlyOutput("rango_edad_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Vínculo con el perpetrador", align = "center"),
                           plotlyOutput("vinculo_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### e) Terceras dos gráficas----
              
              fluidRow(align = "center",
                       box(title = h3("Lugar de los hechos", align = "center"),
                           plotlyOutput("lugar_plot")),
                       box(title = h3("Estado civil", align = "center"),
                           plotlyOutput("estado_civil_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### e) Cuartas dos gráficas----
              
              fluidRow(align = "center",
                       box(title = h3("Número de hijos", align = "center"),
                           plotlyOutput("numero_de_hijos_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Actividades principales de la víctima", align = "center"),
                           plotlyOutput("actividades_victima_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")))
              
      ), # Cierra TabItem
      
      ## 2.4- Características de las personas agresoras (UI)----
      
      tabItem(tabName = "agresoras",
              
              ### a) Títulos----
              fluidRow(
                align = "center",
                h1(""),
                h1("Características de las personas agresoras"),
                h1(""),
                h1("")),
              
              ### b) Cajas de valor----
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_edad_agresor")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_escolaridad_agresor")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_genero_agresor"))),
              
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_actividad_agresor")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_fuente_agresor")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_drogas_agresor"))),
              
              ### c) Filtros de información y periodo de análisis----
              fluidRow(
                column(width = 3,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_4",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_casos)),
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_inicio_4",
                                 "Ingresa la fecha inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                
                column(width = 3,
                       align = "center",
                       dateInput("fecha_hecho_final_4",
                                 "Ingresa la fecha final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30"))),
                
                column(width = 3,
                       align = "center",
                       selectInput("municipio_seleccionado_2", 
                                   "Ingresa el municipio:", 
                                   choices = municipios_en_base, 
                                   selected = "Todos los municipios de Jalisco"))),
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output4")))), 
              
              ### d) Primeras seis gráficas----
              
              fluidRow(align = "center",
                       box(title = h3("Edad", align = "center"),
                           plotlyOutput("edad_agresor_plot"), #cocca
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Escolaridad", align = "center"),
                           plotlyOutput("escolaridad_agresor_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              fluidRow(align = "center",
                       box(title = h3("Género de la persona agresora", align = "center"),
                           plotlyOutput("genero_agresor_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Principal actividad de la persona agresora", align = "center"),
                           plotlyOutput("actividad_agresor_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              fluidRow(align = "center",
                       box(title = h3("Fuente de ingresos de la persona agresora", align = "center"),
                           plotlyOutput("fuente_agresor_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Uso de drogas o alcohol", align = "center"),
                           plotlyOutput("drogas_agresor_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### e) Casos en donde hubo uso de arma ----
              
              fluidRow(align = "center",
                       h1(""),
                       h1("Casos en donde hubo uso de arma"),
                       h1(""),
                       h1("")),
              
              fluidRow(algin = "center", 
                       column(width = 2), 
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_arma_agresor")),
                       column(width = 4, 
                              valueBoxOutput(width = 18, 
                                             "vb_arma_agresor_por")),
                       column(width = 4)),
              
              ### f) Gráficas armas ----
              
              fluidRow(align = "center",
                       box(title = h3("Tipo de arma", align = "center"),
                           plotlyOutput("tipo_arma_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Rango de edad", align = "center"),
                           plotlyOutput("edad_agresor_arma_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              fluidRow(align = "center",
                       box(title = h3("Lugar de los hechos", align = "center"),
                           plotlyOutput("lugar_arma_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Vínculo con la víctima", align = "center"),
                           plotlyOutput("vinculo_arma_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")))
              
              
              
      ),
      
      ## 2.5- Interacción de variables (UI)----
      
      # tabItem(tabName = "interaccion",
      #         
      #         ### a) Títulos----
      #         fluidRow(
      #           align = "center",
      #           h1(""),
      #           h1("Interacción de variables"),
      #           h1(""),
      #           h1("")),
      #         
      #         ### b) Filtros de información, periodo de análisis----
      #         fluidRow(
      #           column(width = 4,
      #                  align = "center",
      #                  dateInput("fecha_hecho_inicio_5",
      #                            "Ingresa la fecha inicial:",
      #                            language = "es", 
      #                            value = "2020-01-01")),
      #           
      #           column(width = 4,
      #                  align = "center",
      #                  dateInput("fecha_hecho_final_5",
      #                            "Ingresa la fecha final:",
      #                            language = "es", 
      #                            value = as.Date("2023-09-30"))),
      #           
      #           column(width = 4,
      #                  align = "center",
      #                  selectInput("municipio_seleccionado_3", 
      #                              "Ingresa el municipio:", 
      #                              choices = municipios_en_base, 
      #                              selected = "Todos los municipios de Jalisco"))),
      #         
      #         ### c) Explicación y filtros de variables----
      #         
      #         fluidRow(h1(""),
      #                  h4("Selecciona dos variables para generar una gráfica que muestre el comportamiento de ambas.", 
      #                     align = "center"),
      #                  h1("")),
      #         
      #         fluidRow(
      #           box(width = 6,
      #               radioButtons("eje_x",
      #                            "Variable en el eje de las X:",
      #                            c("Modalidad" = "modalidad",
      #                              "Estado civil de la víctima" = "estado_civil",
      #                              "Edad de la víctima" = "rango_de_edades",
      #                              "Actividad principal de la víctima" = "actividad",
      #                              "Número de hijos de la víctima" = "numero_de_hijos",
      #                              "Vínculo" = "vinculo",
      #                              "Tipo de violencia" = "tipo",
      #                              "Lugar de los hechos" = "lugar",
      #                              "Escolaridad del agresor" = "escolaridad", 
      #                              "Edad del agresor" = "rango_de_edades_agresor",
      #                              "Género del agresor" = "genero_del_agresor",
      #                              "Uso de sustancias" = "uso_de_sustancias",
      #                              "Actividad principal del agresor" = "actividad_del_agresor",
      #                              "Ingreso principal del agresor" = "ingresos_agresor",
      #                              "Arma blanca" = "indicador_de_arma_blanca", 
      #                              "Arma de fuego" = "indicador_de_arma_fuego",
      #                              "Conocimiento de autoridad" = "conocimiento_de_autoridad"),
      #                            selected = "modalidad")),
      #           
      #           box(width = 6,
      #               radioButtons("eje_y",
      #                            "Variable en el eje de las Y:",
      #                            c("Modalidad" = "modalidad",
      #                              "Estado civil de la víctima" = "estado_civil",
      #                              "Edad de la víctima" = "rango_de_edades",
      #                              "Actividad principal de la víctima" = "actividad",
      #                              "Número de hijos de la víctima" = "numero_de_hijos",
      #                              "Vínculo" = "vinculo",
      #                              "Tipo de violencia" = "tipo",
      #                              "Lugar de los hechos" = "lugar",
      #                              "Escolaridad del agresor" = "escolaridad", 
      #                              "Edad del agresor" = "rango_de_edades_agresor",
      #                              "Género del agresor" = "genero_del_agresor",
      #                              "Uso de sustancias" = "uso_de_sustancias",
      #                              "Actividad principal del agresor" = "actividad_del_agresor",
      #                              "Ingreso principal del agresor" = "ingresos_agresor",
      #                              "Arma blanca" = "indicador_de_arma_blanca", 
      #                              "Arma de fuego" = "indicador_de_arma_fuego",
      #                              "Conocimiento de autoridad" = "conocimiento_de_autoridad"),
      #                            selected = "rango_de_edades"))),
      #         
      #         ### d) Plot----
      #         
      #         fluidRow(
      #           
      #           column(width = 12,
      #                  box( width = 12,
      #                       plotlyOutput("plot_interaccion"),
      #                       h6("Fuente: BANAVIM 2023.", align = "right"))))),
      
      
      
      # ## 2.6- Órdenes de protección (UI)----
      
      tabItem(tabName = "ordenes",
              
              ### a) Títulos----
              fluidRow(
                align = "center",
                h1(""),
                h1("Órdenes de protección"),
                h1(""),
                h1("")),
              
              ### b) Cajas de valor----
              
              fluidRow(align = "center",
                       column(width = 6,
                              valueBoxOutput("caja_ordenes_total",
                                             width = 15)),
                       column(width = 6,
                              valueBoxOutput("caja_ordenes_porcentaje",
                                             width = 15))),
              
              ### c) Filtros de información, periodo de análisis----
              fluidRow(
                column(width = 3,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_5",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_casos)),
                column(width = 3,
                       align = "center",
                       dateInput("fecha_de_recepcion_inicial",
                                 "Ingresa la fecha de recepción inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                
                column(width = 3,
                       align = "center",
                       dateInput("fecha_de_recepcion_final",
                                 "Ingresa la fecha de recepción final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30"))),
                
                column(width = 3,
                       align = "center",
                       selectInput("municipio_seleccionado_4", 
                                   "Ingresa el municipio:", 
                                   choices = municipios_en_base, 
                                   selected = "Todos los municipios de Jalisco"))),
              
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output5")))), 
              
              ### d) Plot de evolución de órdenes----
              
              fluidRow(align = "center", 
                       column(width = 12, 
                              plotlyOutput("evolucion_ordenes_plot"),
                              h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### e) Plot de tipo de orden y entidad emisora ----
              
              fluidRow(align = "center",
                       box(title = h3("Tipo de orden", align = "center"),
                           plotlyOutput("tipo_orden_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Instancia emisora", align = "center"),
                           plotlyOutput("entidad_emisora_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")))
              
              
              
      ), # Cerrar Tab Item
      
      ## 2.7- Servicios otorgados (UI)----
      
      tabItem(tabName = "servicios",
              
              ### a) Títulos----
              
              fluidRow(align = "center",
                       h1(""),
                       h1("Servicios otorgados"),
                       h1(""),
                       h1("")),
              
              ### b) Cajas de valor ----
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_total_servicios")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_total_mujeres_servicios")),
                       
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_tipo_servicio"))),
              
              fluidRow(align = "center",
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_edad_servicio")),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_edociv_servicio")),
                       
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_dependencia_servicio"))),
              
              fluidRow(align = "center",
                       column(width = 2),
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_categoria_servicio")),
                       
                       column(width = 4,
                              valueBoxOutput(width = 18, 
                                             "vb_estatus_servicio")),
                       column(width = 2)),
              
              ### c) Filtros de información, periodo de análisis----
              fluidRow(
                column(width = 3,
                       align = "center",
                       pickerInput(
                         inputId = "depen_check_6",
                         label = "Lista de dependencias",
                         selected = c("Todas las dependencias"),
                         multiple = F,
                         width = 190,
                         choices = dependencias_base_servicios)),
                column(width = 3,
                       align = "center",
                       dateInput("fecha_de_captura_inicial",
                                 "Ingresa la fecha de captura inicial:",
                                 language = "es", 
                                 value = "2020-01-01")),
                
                column(width = 3,
                       align = "center",
                       dateInput("fecha_de_captura_final",
                                 "Ingresa la fecha de captura final:",
                                 language = "es", 
                                 value = as.Date("2023-09-30"))),
                
                column(width = 3,
                       align = "center",
                       selectInput("municipio_seleccionado_5", 
                                   "Ingresa el municipio:", 
                                   choices = municipios_en_base, 
                                   selected = "Todos los municipios de Jalisco"))),
             
              #fila de leyenda si sale algo con los filtros o no
              fluidRow(strong(h2(textOutput("output6")))), 
               ### d) Mapa ----
              
              fluidRow(column(width = 12,
                              h1(""),
                              h3("Mapa", align = "center"),
                              h1(""),
                              leafletOutput("mapa_2"))),
              
              ### e) Primera gráfica ----
              
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           h1(""),
                           h3("Evolución del número de servicios otorgados",
                              align = "center"),
                           plotlyOutput("plot_evolucion_servicios"),
                           h1(""),
                           h6("Fuente: BANAVIM 2023.", 
                              align = "right")))),
              
              ### f) Segunda y tercera gráfica ----
              
              fluidRow(align = "center",
                       box(title = h3("Tipo de servicio más común", align = "center"),
                           plotlyOutput("tipo_servicio_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Edad de mujeres atendidas", align = "center"),
                           plotlyOutput("servicios_edad_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### g) Cuarta y quinta gráfica ----
              
              fluidRow(align = "center",
                       box(title = h3("Estado civil", align = "center"),
                           plotlyOutput("servicios_edocivil_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Dependencias con más servicios", align = "center"),
                           plotlyOutput("servicios_dependencia_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right"))),
              
              ### h) Sexta y séptima gráfica ----
              
              fluidRow(align = "center",
                       # box(title = h3("Top 10 usuarios con más servicios", align = "center"),
                       #     plotlyOutput("servicios_usuarios_plot"),
                       #     h6("Fuente: BANAVIM 2023.", align = "right")),
                       box(title = h3("Categoría con más servicios", align = "center"),
                           plotlyOutput("servicios_categoria_plot"),
                           h6("Fuente: BANAVIM 2023.", align = "right")))
              
              
              
              
              
      ) # Cerrar Tab Item
      
      
    ), # Cerrar tabItems
    
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #D81B60;
                              color: #FFFFFF;
                              max-height: 60px;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #FFFFFF;
                              color: #D81B60;
                              height: 60px;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #FFFFFF;
                              color: #D81B60;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #FFFFFF;
                              }
        
                /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #FFFFF;
                              color: #E7A052;
                              font-weight: bold;
                              font-size: 15px;
        } 
        
       .skin-blue .sidebar-menu>li:hover>a {
                            color: #E7A052;
                            background: #FFFF;
                            border-left-color: #D81B60;
                            
        }
        
                /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #E7A052;
                              color: #FFF;
                              font-weight: bold;
                              font-size: 15px;
        } 
        
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle{
                              background-color: #D81B60;
                              }
        
        /* cambiar tipo de letra */
        .main-header .logo {
        font-family: "Franklin Gothic", sans-serif;
        font-color: #D81B60;
        font-weight: bold;
        font-size: 18px;
      }
        /* body */
        .content-wrapper, .right-side {
        background-color: #FFFFFF;
                                }
                              ')))
    
  ), # Cerrar Dashboardbody
  title = "BANAVIM", skin = NULL
  
) # Cerrar DasboardPage



# 3.- Definir server----
server <- function(input, output, session
                   ) {
  
  # Width
  plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})
  
  # Height
  plotHeight <- function(){
    width <- plotWidth()
    h <- ifelse(width > 425, width*0.54, width*0.75)
    return(h)}
  
  # Font
  fontbase <- 6
  
  textFunction <- function(){
    width <- plotWidth()
    textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
    return(textSize)}
  
  
  
  ### Vamos a hacer las bases artificales que van a filtrar
  #se va hacer una base por cada pestaña
  
  # Información general
  data1 <- reactive({
    base_casos_clean %>% 
      filter(if(input$depen_check_1=="TODAS LAS DEPENDENCIAS") dep_exp!="" else str_to_upper(dep_exp) %in% input$depen_check_1, 
             fecha_hechos >= input$fecha_hecho_inicio &
               fecha_hechos <= input$fecha_hecho_final
      )
  })
  
  ## 3.1.- Información general (Server) ----
  
  ### a) Mapa ----
  
  output$mapa_1 <- renderLeaflet({
    
    if(input$switch_tasa != T){
      # 
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   mapa_1p <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   mapa_1p <- base_casos_clean
      #   
      # }
      
      mapa_1p <- data1() %>% 
        # filter(fecha_hechos >= input$fecha_hecho_inicio &
        #          fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        stringdist_join(., poblacion, 
                        by= c("municipio_hecho_clean" = "municipio"),
                        mode='left',
                        method = "jw", 
                        max_dist=.08, 
                        distance_col='dist') %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        filter(!is.na(municipio)) %>% 
        select(-dist) %>% 
        replace(is.na(.), 0) %>% 
        left_join(., jalisco_shape, by = c("municipio")) %>% 
        mutate(quantile = ntile(cuenta, 5)) 
      # 
      # quantiles <- data.frame(quantile = 1:5,
      #                         upr = round(quantile(mapa_1p$cuenta, 
      #                                              probs = seq(.2, 1, by = .2))),
      #                         lwr = c(-1, 
      #                                 round(quantile(mapa_1p$cuenta, 
      #                                                probs = seq(.2, 1, by = .2))[1:4]))) %>% 
      #   mutate(lwr = lwr + 1) %>% 
      #   mutate(lwr = prettyNum(lwr, big.mark = ",")) %>% 
      #   mutate(upr = prettyNum(upr, big.mark = ",")) %>% 
      #   mutate(rango = paste0(lwr, "-", upr)) %>% 
      #   select(quantile, rango) %>% 
      #   arrange(-quantile)
      
      # mapa_1p <- mapa_1p %>% 
      #   left_join(., quantiles) %>% 
      #   mutate(rango = factor(rango, 
      #                         levels = quantiles$rango)) %>% 
      #   st_as_sf()
      
      # pal1 <-  colorFactor(palette=c("#0f0a1f", 
      #                                "#3c3065", 
      #                                "#9784d7",
      #                                "#cbc1eb",
      #                                "#ffffff"),
      #                      levels=sort(unique(mapa_1p$rango)))
      
      pal1 <- colorNumeric(palette = c("#0f0a1f", 
                                       "#3c3065", 
                                       "#9784d7",
                                       "#cbc1eb"), 
                           mapa_1p$cuenta, reverse = T
                           
      )
      
      labels_map <- sprintf(
        "<strong>%s</strong><br/>%s",
        mapa_1p$municipio_hecho_clean, 
        paste0("Número de casos: ", prettyNum(mapa_1p$cuenta, big.mark = ","))) %>%
        lapply(htmltools::HTML)
      
      mapa_1p %>% st_as_sf() %>% 
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(#data =  mapa_1p,
          color = "black",
          weight=1,
          fillOpacity = 0.8,
          fillColor  = ~pal1(cuenta),
          label=~labels_map) %>% 
        leaflet::addLegend(pal = pal1,
                           values = sort(unique(mapa_1p$cuenta)),
                           opacity = 0.75,
                           title = "Número de casos",
                           position = "bottomright")
      
    } else {
      
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   mapa_1p <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   mapa_1p <- base_casos_clean
      #   
      # }
      # 
      mapa_1p <- data1() %>% 
        filter(fecha_hechos >= input$fecha_hecho_inicio &
                 fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        stringdist_join(., poblacion, 
                        by= c("municipio_hecho_clean" = "municipio"),
                        mode='left',
                        method = "jw", 
                        max_dist=.08, 
                        distance_col='dist') %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        filter(!is.na(municipio)) %>% 
        select(-dist) %>% 
        mutate(tasa = round(cuenta/valor * 100000)) %>% 
        replace(is.na(.), 0) %>% 
        left_join(., jalisco_shape, by = c("municipio")) %>% 
        mutate(quantile = ntile(tasa, 5)) 
      
      # quantiles <- data.frame(quantile = 1:5,
      #                         upr = round(quantile(mapa_1p$tasa, 
      #                                              probs = seq(.2, 1, by = .2))),
      #                         lwr = c(-1, 
      #                                 round(quantile(mapa_1p$tasa, 
      #                                                probs = seq(.2, 1, by = .2))[1:4]))) %>% 
      #   mutate(lwr = lwr + 1) %>% 
      #   mutate(lwr = prettyNum(lwr, big.mark = ",")) %>% 
      #   mutate(upr = prettyNum(upr, big.mark = ",")) %>% 
      #   mutate(rango = paste0(lwr, "-", upr)) %>% 
      #   select(quantile, rango) %>% 
      #   arrange(-quantile)
      # 
      # mapa_1p <- mapa_1p %>% 
      #   left_join(., quantiles) %>% 
      #   mutate(rango = factor(rango, 
      #                         levels = quantiles$rango)) %>% 
      #   st_as_sf()
      # 
      # pal1 <- colorFactor(palette=c("#0f0a1f", 
      #                               "#3c3065", 
      #                               "#9784d7",
      #                               "#cbc1eb",
      #                               "#ffffff"),
      #                     levels=sort(unique(mapa_1p$rango)))
      # 
      
      pal1 <- colorNumeric(c("#0f0a1f", 
                             "#3c3065",
                             "#9784d7",
                             "#cbc1eb"), 
                           
                           mapa_1p$cuenta, reverse = T)
      
      labels_map <- sprintf(
        "<strong>%s</strong><br/>%s",
        mapa_1p$municipio_hecho_clean, 
        paste0("Tasa de casos por 100 mil mujeres: ", prettyNum(mapa_1p$tasa, big.mark = ","))) %>%
        lapply(htmltools::HTML)
      
      mapa_1p %>% st_as_sf() %>% leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(#data =  mapa_1p,
          color = "black",
          weight=1,
          fillOpacity = 0.8,
          fillColor  = ~pal1(cuenta),
          label=~labels_map) %>% 
        leaflet::addLegend(pal = pal1,
                           values = sort(unique(mapa_1p$cuenta)),
                           opacity = 0.75,
                           title = "Tasa por 100 mil mujeres",
                           position = "bottomright")
      
      
      
    }
    
  })
  
  ### b) Datos generales----
  
  output$total_casos <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_1)){
    #   
    #   total_ind <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_1)
    #   
    # } else {
    #   
    #   total_ind <- base_casos_clean
    #   
    # }
    
    total_ind <- data1() #%>% 
    # filter(fecha_hechos >= input$fecha_hecho_inicio &
    #          fecha_hechos <= input$fecha_hecho_final) 
    
    valueBox(prettyNum(nrow(total_ind), big.mark = ","), 
             "Número de casos recibidos", 
             icon = icon("file"),
             width = 18, 
             color = "maroon")
    
    
  })
  
  output$tasa_casos <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_1)){
    #   
    #   tasa_ind <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_1)
    #   
    # } else {
    #   
    #   tasa_ind <- base_casos_clean
    #   
    # }
    
    tasa_ind <- data1() #%>% 
    # filter(fecha_hechos >= input$fecha_hecho_inicio &
    #          fecha_hechos <= input$fecha_hecho_final) 
    
    valor_tasa <- nrow(tasa_ind)/4249696*100000
    
    
    valueBox(prettyNum(round(valor_tasa, 1), big.mark = ","), 
             "Casos por 100 mil mujeres", 
             icon = icon("venus"),
             width = 18, 
             color = "purple")
    
    
  })
  
  # 1.- Municipios plot
  
  output$municipios_plot <- renderPlotly({
    
    # Procesar municipios_plot (tasa)
    
    if(input$switch_tasa == T){
      
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   p1 <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   p1 <- base_casos_clean
      #   
      # }
      
      
      p1 <- data1() %>% 
        # filter(fecha_hechos >= input$fecha_hecho_inicio &
        #          fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        stringdist_join(., poblacion, 
                        by= c("municipio_hecho_clean" = "municipio"),
                        mode='left',
                        method = "jw", 
                        max_dist=.08, 
                        distance_col='dist') %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        filter(!is.na(municipio)) %>% 
        select(-dist) %>% 
        replace(is.na(.), 0) %>% 
        mutate(tasa = cuenta / valor * 100000) %>% 
        arrange(-tasa) %>% 
        filter(!is.infinite(tasa)) %>% 
        mutate(tasa = round(tasa, 1)) %>% 
        mutate(label = paste0("<b>",
                              municipio_hecho_clean,
                              "</b>",
                              "\n",
                              "Número de casos de violencia en el periodo: ", 
                              "<b>",
                              prettyNum(cuenta, big.mark = ","),
                              "</b>",
                              "\n", 
                              "Población de mujeres en el municipio: ", 
                              "<b>",
                              prettyNum(valor, big.mark = ","), 
                              "</b>",
                              "\n", 
                              "Tasa por 100,000 mujeres: ", 
                              "<b>",
                              comma(tasa, .1),
                              "</b>"))
      
      p1 <- p1 %>% 
        slice_head(n = 10) %>% 
        ggplot(aes(x=reorder(municipio_hecho_clean, -tasa),
                   y=tasa, text=label))+
        geom_col(fill = '#5F5CA8', alpha=0.8)+
        # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
        #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
        geom_text(aes(label=scales::comma(tasa)), size=4, color="black", fontface = "bold")+
        scale_y_continuous(labels = scales::comma) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
        labs(x="", y="", fill="", color="", 
             title = paste0('Top 10 municipios con mayor tasa de casos de violencia por 100,000 mujeres\n',
                            "(", 
                            format(input$fecha_hecho_inicio, "%d/%B/%Y"), 
                            " al ", 
                            format(input$fecha_hecho_final, "%d/%B/%Y"),
                            ")")
        )+
        theme_minimal()+
        theme(legend.position='none',
              text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
              strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
              plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
              plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
              plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
      
      ggplotly(p1, tooltip = "label")
        
        
        
        
        
      #   
      #   
      #   
      #   
      #   p1 %>% 
      #   mutate(municipio_hecho_clean = factor(municipio_hecho_clean,
      #                                         levels = p1$municipio_hecho_clean))
      # 
      # 
      # fig <- plot_ly(data = p1 %>% slice_head(n = 10),
      #                x = ~ reorder(
      #                  str_wrap(municipio_hecho_clean, width=6), -tasa), 
      #                y = ~ tasa,
      #                type = "bar",
      #                marker = list(color = '#5F5CA8',
      #                              line = list(color = '#5e506d',
      #                                          width = 1.5)),
      #                text = ~ label,
      #                hoverinfo = "text", 
      #                textfont = list(color = '#5F5CA8', 
      #                                size=0
      #                )
      # ) %>% 
      #   # add_trace(textfont = list(color = '#5F5CA8'))
      #   add_text(text = ~prettyNum(tasa, big.mark = ","),
      #            textposition = "top",
      #            hoverinfo="none") %>%
      #   layout(
      #     title = paste0('Top 10 municipios con mayor tasa de casos de violencia por 100,000 mujeres\n',
      #                    "(",
      #                    format(input$fecha_hecho_inicio, "%d/%b/%y"),
      #                    " al ",
      #                    format(input$fecha_hecho_final, "%d/%b/%y"),
      #                    ")"),
      #     xaxis = list(
      #       title = "Municipio",
      #       zerolinecolor = '#ffff',
      #       zerolinewidth = 2,
      #       gridcolor = '#fff'),
      #     yaxis = list(
      #       title = "Tasa",
      #       zerolinecolor = '#ffff',
      #       zerolinewidth = 2,
      #       gridcolor = '#fff'),
      #     showlegend = FALSE,
      #     layout.separators=",.",
      #     hoverlabel=list(bgcolor="white")
      #   ) 
      # 
      # fig
      
    } else { # Procesar total de casos
      
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   p1_total <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   p1_total <- base_casos_clean
      #   
      # }
      
      p1_total <- data1() %>% 
        # filter(fecha_hechos >= input$fecha_hecho_inicio &
        #          fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        arrange(-cuenta) %>% 
        mutate(label = paste0("<b>",
                              municipio_hecho_clean,
                              "</b>",
                              "\n",
                              "Número de casos de violencia en el periodo: ", 
                              "<b>",
                              prettyNum(cuenta, big.mark = ",")))
      
      p1_total <- p1_total %>% 
        slice_head(n = 10) %>% 
        ggplot(aes(x=reorder(municipio_hecho_clean, -cuenta),
                   y=cuenta, text=label))+
        geom_col(fill = '#5F5CA8', alpha=0.8)+
        # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
        #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
        geom_text(aes(label=scales::comma(cuenta), accuracy = 1), size=4, color="black", fontface = "bold")+
        scale_y_continuous(labels = scales::comma) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
        labs(x="", y="", fill="", color="", 
             title = paste0('Top 10 municipios con mayor número de casos de violencia\n',
                                                     "(", 
                                                     format(input$fecha_hecho_inicio, "%d/%B/%Y"), 
                                                     " al ", 
                                                     format(input$fecha_hecho_final, "%d/%B/%Y"),
                                                     ")")
        )+
        theme_minimal()+
        theme(legend.position='none',
              text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
              strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
              plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
              plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
              plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))

      ggplotly(p1_total, tooltip = "label")
      
        
      # 
      # p1_total %>% 
      #   mutate(municipio_hecho_clean = factor(municipio_hecho_clean,
      #                                         levels = p1_total$municipio_hecho_clean))
      # 
      # 
      # fig1_total <- plot_ly(data = p1_total %>% slice_head(n = 10),
      #                       x = ~ reorder(str_wrap(municipio_hecho_clean, width=6), -cuenta),
      #                       y = ~ cuenta,
      #                       type = "bar",
      #                       marker = list(color = '#5F5CA8',
      #                                     line = list(color = '#5e506d',
      #                                                 width = 1.5)),
      #                       text = ~ label,
      #                       hoverinfo = ~ "text", 
      #                       textfont = list(color = '#5F5CA8', 
      #                                       size=0)
      # ) %>% 
      #   add_text(text = ~prettyNum(cuenta, big.mark = ","),
      #            textposition = "top",
      #            hoverinfo="none") %>% 
      #   layout(title = paste0('Top 10 municipios con mayor número de casos de violencia\n',
      #                         "(", 
      #                         format(input$fecha_hecho_inicio, "%d/%b/%y"), 
      #                         " al ", 
      #                         format(input$fecha_hecho_final, "%d/%b/%y"),
      #                         ")"),
      #          xaxis = list(
      #            title = "Municipio",
      #            zerolinecolor = '#ffff',
      #            zerolinewidth = 2,
      #            gridcolor = '#fff'),
      #          yaxis = list(
      #            title = "Número de casos",
      #            zerolinecolor = '#ffff',
      #            zerolinewidth = 2,
      #            tickformat=",d",
      #            gridcolor = '#fff'),
      #          showlegend = FALSE,
      #          hoverlabel=list(bgcolor="white")) 
      # 
      # fig1_total
      # 
      
    }
    
  })
  
  
  # 3.- Tabla del resto de municipios
  
  output$tabla_municipios <- DT::renderDataTable({
    
    if(input$switch_tasa == T){
      
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   t1 <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   t1 <- base_casos_clean
      #   
      # }
      
      t1 <- data1() %>% 
        # filter(fecha_hechos >= input$fecha_hecho_inicio &
        #          fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        stringdist_join(., poblacion, 
                        by= c("municipio_hecho_clean" = "municipio"),
                        mode='left',
                        method = "jw", 
                        max_dist=.08, 
                        distance_col='dist') %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        filter(!is.na(municipio)) %>% 
        select(-dist) %>% 
        replace(is.na(.), 0) %>% 
        mutate(tasa = cuenta / valor * 100000) %>% 
        arrange(-tasa) %>% 
        filter(!is.infinite(tasa)) %>% 
        mutate(tasa = round(tasa, 1))
      
      t1 <- t1[c(11:125),]
      
      t1 <- t1 %>% 
        filter(!is.na(municipio_hecho_clean))
      
      t1 <- t1 %>% 
        mutate(cvo = c(11:(nrow(t1)+10))) %>% 
        select(cvo,
               municipio_hecho_clean,
               cuenta,
               valor, 
               tasa) %>% 
        mutate(valor = prettyNum(valor, big.mark = ","))
      
      names(t1) <- c("Posición", 
                     "Municipio",
                     "Número de casos en el periodo",
                     "Población de mujeres (2020)", 
                     "Tasa de casos x 100 mil mujeres")
      
      DT::datatable(t1,
                    options = list(scrollY = "300px",
                                   "pageLength" = 10,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'font-size': '85%'});",
                                     "}"),
                                   language = list(
                                     info = ' ',
                                     paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                                   columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>% 
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          fontSize = "80%",
          #color = '#008080',
          fontWeight = 'plain',
          #paddingRight = "0.5em",
          borderRightWidth = "1px",
          borderRightStyle = "solid",
          borderRightColor = "white",
          borderBottomColor = "#ffffff",
          borderBottomStyle = "solid",
          borderBottomWidth = "0.5px",
          #borderCollapse = "collapse",
          verticalAlign = "middle",
          textAlign = "center",
          wordWrap = "break-word"#,
          #backgroundColor = '#e6e6e5'
        )
      
    } else { # Analizar casos totales
      
      # if(!grepl("Todas las dependencias", input$depen_check_1)){
      #   
      #   t1_total <- base_casos_clean %>% 
      #     filter(dep_exp %in% input$depen_check_1)
      #   
      # } else {
      #   
      #   t1_total <- base_casos_clean
      #   
      # }
      
      t1_total <- data1() %>% 
        # filter(fecha_hechos >= input$fecha_hecho_inicio &
        #          fecha_hechos <= input$fecha_hecho_final) %>%
        group_by(municipio_hecho_clean) %>% 
        filter(municipio_hecho_clean !=  "") %>% 
        summarise(cuenta = n()) %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        arrange(-cuenta)
      
      t1_total <- t1_total[c(11:125),]
      
      
      t1_total <- t1_total %>% 
        filter(!is.na(municipio_hecho_clean))
      
      t1_total <- t1_total %>% 
        mutate(cvo = c(11:(nrow(t1_total)+10))) %>% 
        select(cvo,
               municipio_hecho_clean,
               cuenta)
      
      names(t1_total) <- c("Posición", 
                           "Municipio", 
                           "Número de casos en el periodo")
      
      DT::datatable(t1_total,
                    options = list(scrollY = "300px",
                                   "pageLength" = 10,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'font-size': '85%'});",
                                     "}"),
                                   language = list(
                                     info = ' ',
                                     paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                                   columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
        formatStyle(
          columns = c(1:3),
          fontFamily = "Nutmeg-Light",
          fontSize = "80%",
          #color = '#008080',
          fontWeight = 'plain',
          #paddingRight = "0.5em",
          borderRightWidth = "1px",
          borderRightStyle = "solid",
          borderRightColor = "white",
          borderBottomColor = "#ffffff",
          borderBottomStyle = "solid",
          borderBottomWidth = "0.5px",
          #borderCollapse = "collapse",
          verticalAlign = "middle",
          textAlign = "center",
          wordWrap = "break-word"#,
          #backgroundColor = '#e6e6e5'
        )
      
      
    }
    
    
  })
  
  ## 3.2.- Evolución de casos (Server) ----
  
  data2 <- reactive({
    base_casos_clean %>% 
      filter(if(input$depen_check_2=="TODAS LAS DEPENDENCIAS") dep_exp!="" else str_to_upper(dep_exp) %in% input$depen_check_2, 
             fecha_hechos >= input$fecha_hecho_inicio_2 &
               fecha_hechos <= input$fecha_hecho_final_2
      )
    
  })
  output$evolucion_plot <- renderPlotly({
    
    l <- list(
      font = list(
        family = "Nutmeg-Light",
        size = 10*textFunction(),
        color = "#000"),
      borderwidth = 0,
      x = 0.1, 
      y = 0.9)
    
    tt <- list(
      family = "Nutmeg-Light",
      size = 10*textFunction())
    # if(!grepl("Todas las dependencias", input$depen_check_2)){
    #   
    #   p2 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_2)
    #   
    # } else {
    #   
    #   p2 <- base_casos_clean
    #   
    # }
    
    p2 <- data2() #%>% 
    # filter(fecha_hechos >= input$fecha_hecho_inicio_2 &
    #          fecha_hechos <= input$fecha_hecho_final_2) 
    
    
    p2 <- p2 %>% 
      group_by(fecha_hechos) %>% 
      summarise(cuenta = n()) %>% 
      filter(!is.na(fecha_hechos))
    
    t <- list(
      family = "Nutmeg-Light",
      size = 10*textFunction())
    tt <- list(
      family = "Nutmeg-Light",
      size = 10*textFunction())
    
    p2 <- p2 %>% #COQUITA
      right_join(., data.frame(fecha_hechos = as.Date(
        as.numeric(min(p2$fecha_hechos, na.rm = T)):
          as.numeric(max(p2$fecha_hechos, na.rm = T))))) %>% 
      arrange(fecha_hechos) %>% 
      replace(is.na(.), 0) %>% 
      mutate(avg_7 = rollmean(cuenta, k = 7, fill = NA)) %>% 
      mutate(avg_15 = rollmean(cuenta, k = 15, fill = NA)) %>% 
      mutate(avg_30 = rollmean(cuenta, k = 30, fill = NA)) %>% 
      mutate(label_plot_15 = paste0(format(fecha_hechos, "%d/%B/%y"),"\nPromedio últimos 15 días: ", round(avg_15, 2)),
             label_plot_30 = paste0(format(fecha_hechos, "%d/%B/%y"),"\nPromedio últimos 30 días: ", round(avg_30, 2)))
    
    fig2 <- plot_ly(p2, 
                    x = ~ fecha_hechos,
                    y = ~ avg_30,
                    type = 'scatter',
                    mode = 'lines',
                    name = "Promedio móvil (30 días)", 
                    line = list(color = "#542344", 
                                width = 4),
                    text = ~ label_plot_30,
                    hoverinfo = "text", 
                    hoverlabel = list(bgcolor='#542344')) %>% 
      add_trace(y = ~avg_15, 
                name = 'Promedio móvil (15 días)', 
                line = list(color = '#D1A0DA',
                            width = 2, dash = 'dash'),
                opacity = 0.7,
                text = ~ label_plot_15,
                hoverinfo = "text", 
                hoverlabel = list(bgcolor='#D1A0DA'))  %>% 
      layout(title = list(text=paste0('Evolución del número de casos de violencia contra las mujeres\n',
                            "(", 
                            format(input$fecha_hecho_inicio_2, "%d/%b/%y"), 
                            " al ", 
                            format(input$fecha_hecho_final_2, "%d/%b/%y"),
                            ")"), font=t),
              xaxis = list(title = '',
                           zerolinecolor = '#ffff',zerolinewidth = 2,
                           gridcolor = '#bcbcbc',
                           tickangle=0, tickfont = list(family='Nutmeg-Light')),
              
              
              # xaxis = list(title = list(text ="Fecha de captura",
              #                           zerolinecolor = '#ffff',
              #                           zerolinewidth = 2,
              #                           gridcolor = '#bcbcbc',
              #                           font=tt)),
              yaxis = list(title = list(text ="Promedio móvil diario",
                                        zerolinecolor = '#ffff',
                                        zerolinewidth = 2,
                                        gridcolor = '#bcbcbc', font=tt),
                           tickangle=0, tickfont = list(family='Nutmeg-Light')),
             
             
             
             # xaxis = list(title = list(text ="Fecha de hechos",
             #   zerolinecolor = '#ffff',
             #   zerolinewidth = 2,
             #   gridcolor = '#bcbcbc', font=t)),
             # yaxis = list(title = list(text ="Promedio móvil diario",
             #   zerolinecolor = '#ffff',
             #   zerolinewidth = 2,
             #   gridcolor = '#bcbcbc', font=t)),
             legend = l) %>% 
      plotly::config(
        locale='es')
    
    fig2
    
    fig2 <- plot_ly(p2, 
                    x = ~ fecha_hechos,
                    y = ~ avg_30,
                    type = 'scatter',
                    mode = 'lines',
                    name = "Promedio móvil (30 días)", 
                    line = list(color = "#542344", 
                                width = 4),
                    text = ~ label_plot_30,
                    hoverinfo = "text", 
                    hoverlabel = list(bgcolor='#542344')) %>% 
      add_trace(y = ~avg_15, 
                name = 'Promedio móvil (15 días)', 
                line = list(color = '#D1A0DA',
                            width = 2, dash = 'dash'),
                opacity = 0.7,
                text = ~ label_plot_15,
                hoverinfo = "text", 
                hoverlabel = list(bgcolor='#D1A0DA'))  %>% 
      layout(title = list(text=paste0('Evolución del número de casos de violencia contra las mujeres\n',
                            "(", 
                            format(input$fecha_hecho_inicio_2, "%d/%b/%y"), 
                            " al ", 
                            format(input$fecha_hecho_final_2, "%d/%b/%y"),
                            ")"), font=t),
             # xaxis = list(title = list(text ="Fecha de hechos",
             #   zerolinecolor = '#ffff',
             #   zerolinewidth = 2,
             #   gridcolor = '#bcbcbc', font=t)),
             # yaxis = list(title = list(text ="Promedio móvil diario",
             #   zerolinecolor = '#ffff',
             #   zerolinewidth = 2,
             #   gridcolor = '#bcbcbc', font=t)),
             xaxis = list(title = '',
                          zerolinecolor = '#ffff',zerolinewidth = 2,
                          gridcolor = '#bcbcbc',
                          tickangle=0, tickfont = list(family='Nutmeg-Light')),
             
             
             # xaxis = list(title = list(text ="Fecha de captura",
             #                           zerolinecolor = '#ffff',
             #                           zerolinewidth = 2,
             #                           gridcolor = '#bcbcbc',
             #                           font=tt)),
             yaxis = list(title = list(text ="Promedio móvil diario",
                                       zerolinecolor = '#ffff',
                                       zerolinewidth = 2,
                                       gridcolor = '#bcbcbc', font=tt),
                          tickangle=0, tickfont = list(family='Nutmeg-Light')),
             
             legend = l) %>% 
      plotly::config(
        locale='es')
    
    fig2
    
    ###############################################################################

        ###############################################################################
    
    
    
  })
  
  output$tabla_cambio_municipios <- DT::renderDataTable({
    
    # if(!grepl("Todas las dependencias", input$depen_check_2)){
    #   
    #   p2_plots <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_2)
    #   
    # } else {
    #   
    #   p2_plots <- base_casos_clean
    #   
    # }
    
    # Últimos 14 días
    p2_sem <- data2() %>% 
      filter(fecha_hechos >= input$fecha_hecho_final_2-13) %>% 
      # filter(fecha_hechos <=input$fecha_hecho_final_2) %>% 
      mutate(periodo = case_when(fecha_hechos >= input$fecha_hecho_final_2-6 ~ "ultima_semana",
                                 fecha_hechos < input$fecha_hecho_final_2-6 ~ "semana_anterior")) %>% 
      group_by(municipio_hecho_clean, periodo) %>% 
      summarise(cuenta = n()) %>% 
      pivot_wider(names_from = periodo,
                  values_from = cuenta) %>% 
      filter(!is.na(municipio_hecho_clean)) %>% 
      replace(is.na(.), 0) %>% 
      mutate(cambio_semana = (ultima_semana - semana_anterior)) %>% 
      mutate(cambio_semana_por = (ultima_semana - semana_anterior)/semana_anterior) 
    
    p2_sem$cambio_semana_por[which(is.infinite(p2_sem$cambio_semana_por))] <- 1
    
    p2_sem <- p2_sem %>% 
      mutate(frase_1 = paste0(" (De ", semana_anterior, " a ", ultima_semana, " casos).")) %>% 
      mutate(frase_1 = gsub(" 1 casos).", " 1 caso).", frase_1))  %>% 
      mutate(signo = case_when(cambio_semana_por <= 0 ~ "",
                               cambio_semana_por > 0 ~ "+")) %>% 
      mutate(label_semana = paste0(signo, scales::percent(cambio_semana_por, accuracy = 1),
                                   frase_1)) %>% 
      ungroup() %>% 
      arrange(-cambio_semana) 
    
    # Últimos 60 días
    p2_mes <- data2() %>% 
      filter(fecha_hechos >= input$fecha_hecho_final_2-59) %>% 
      # filter(fecha_hechos <= input$fecha_hecho_final_2) %>% 
      mutate(periodo = case_when(fecha_hechos >= input$fecha_hecho_final_2-29 ~ "ultimo_mes",
                                 fecha_hechos < input$fecha_hecho_final_2-29 ~ "mes_anterior")) %>% 
      group_by(municipio_hecho_clean, periodo) %>% 
      summarise(cuenta = n()) %>% 
      pivot_wider(names_from = periodo,
                  values_from = cuenta) %>% 
      filter(!is.na(municipio_hecho_clean)) %>% 
      replace(is.na(.), 0) %>% 
      mutate(cambio_mes = ultimo_mes - mes_anterior) %>% 
      mutate(cambio_mes_por = (ultimo_mes - mes_anterior)/mes_anterior) 
    
    p2_mes$cambio_mes_por[which(is.infinite(p2_mes$cambio_mes_por))] <- 1
    
    p2_mes <- p2_mes %>% 
      mutate(frase_1 = paste0(" (De ", mes_anterior, " a ", ultimo_mes, " casos).")) %>% 
      mutate(frase_1 = gsub(" 1 casos).", " 1 caso).", frase_1))  %>% 
      mutate(signo = case_when(cambio_mes_por <= 0 ~ "",
                               cambio_mes_por > 0 ~ "+")) %>% 
      mutate(label_mes = paste0(signo, scales::percent(cambio_mes_por, accuracy = 1),
                                frase_1)) %>% 
      ungroup() %>% 
      arrange(-cambio_mes) 
    
    # Últimos 180 días
    p2_anios <- data2() %>% 
      filter(fecha_hechos >= input$fecha_hecho_final_2-365) %>% 
      # filter(fecha_hechos <= input$fecha_hecho_final_2) %>% 
      mutate(periodo = case_when(fecha_hechos >= input$fecha_hecho_final_2-182 ~ "ultimo_anio",
                                 fecha_hechos < input$fecha_hecho_final_2-182 ~ "anio_anterior")) %>% 
      group_by(municipio_hecho_clean, periodo) %>% 
      summarise(cuenta = n()) %>% 
      pivot_wider(names_from = periodo,
                  values_from = cuenta) %>% 
      filter(!is.na(municipio_hecho_clean)) %>% 
      replace(is.na(.), 0) %>% 
      mutate(cambio_anio = ultimo_anio - anio_anterior) %>% 
      mutate(cambio_anio_por = (ultimo_anio - anio_anterior)/anio_anterior) 
    
    p2_anios$cambio_anio_por[which(is.infinite(p2_anios$cambio_anio_por))] <- 1
    
    p2_anios <- p2_anios %>% 
      mutate(frase_1 = paste0(" (De ", anio_anterior, " a ", ultimo_anio, " casos).")) %>% 
      mutate(frase_1 = gsub(" 1 casos).", " 1 caso).", frase_1))  %>% 
      mutate(signo = case_when(cambio_anio_por <= 0 ~ "",
                               cambio_anio_por > 0 ~ "+")) %>% 
      mutate(label_anio = paste0(signo, scales::percent(cambio_anio_por, accuracy = 1),
                                 frase_1)) %>% 
      ungroup() %>% 
      arrange(-cambio_anio)
    
    output$caja_mun_total <- renderValueBox({
      
      if(input$tempo_analisis == "Últimos 14 días"){
        
        
        valor_mun_tot <- str_to_title(p2_sem$municipio_hecho_clean[1])
        
        valor_mun_tot_2 <- HTML(paste0(valor_mun_tot,
                                       br(), 
                                       "(+ ", 
                                       p2_sem$cambio_semana[1], 
                                       " casos)"))
        
      }
      
      if(input$tempo_analisis == "Últimos 60 días"){
        
        
        valor_mun_tot <- str_to_title(p2_mes$municipio_hecho_clean[1])
        
        valor_mun_tot_2 <- HTML(paste0(valor_mun_tot,
                                       br(), 
                                       "(+ ", 
                                       p2_mes$cambio_mes[1], 
                                       " casos)"))
        
      }
      
      if(input$tempo_analisis == "Último año"){
        
        
        valor_mun_tot <- str_to_title(p2_anios$municipio_hecho_clean[1])
        
        valor_mun_tot_2 <- HTML(paste0(valor_mun_tot,
                                       br(), 
                                       "(+ ", 
                                       p2_anios$cambio_anio[1], 
                                       " casos)"))
        
        
      }
      
      valueBox(tags$p(valor_mun_tot_2, style = "font-size: 62%;"),
               tags$p("Municipio con mayor aumento (total)", style = "font-size: 100%;"), 
               icon = icon("arrow-up"),
               width = 15, 
               color = "purple")
      
    })
    
    output$caja_mun_porc <- renderValueBox({
      
      if(input$tempo_analisis == "Últimos 14 días"){
        
        valor_mun_tasa <- str_to_title(p2_sem$municipio_hecho_clean[order(p2_sem$cambio_semana_por, decreasing = T)][1])
        
        valor_mun_tasa_2 <- HTML(paste0(valor_mun_tasa,
                                        br(), 
                                        "(+ ", 
                                        scales::percent(p2_sem$cambio_semana_por[order(p2_sem$cambio_semana_por, decreasing = T)][1],
                                                        accuracy = 1), 
                                        ")"))
        
      }
      
      if(input$tempo_analisis == "Últimos 60 días"){
        
        valor_mun_tasa <- str_to_title(p2_mes$municipio_hecho_clean[order(p2_mes$cambio_mes_por, decreasing = T)][1])
        
        valor_mun_tasa_2 <- HTML(paste0(valor_mun_tasa,
                                        br(), 
                                        "(+ ", 
                                        scales::percent(p2_mes$cambio_mes_por[order(p2_mes$cambio_mes_por, decreasing = T)][1],
                                                        accuracy = 1), 
                                        ")"))
        
      }
      
      if(input$tempo_analisis == "Último año"){
        
        
        valor_mun_tasa <- str_to_title(p2_anios$municipio_hecho_clean[order(p2_anios$cambio_anio_por, decreasing = T)][1])
        
        valor_mun_tasa_2 <- HTML(paste0(valor_mun_tasa,
                                        br(), 
                                        "(+ ", 
                                        scales::percent(p2_anios$cambio_anio_por[order(p2_anios$cambio_anio_por, decreasing = T)][1],
                                                        accuracy = 1), 
                                        ")"))
        
        
      }
      
      valueBox(tags$p(valor_mun_tasa_2, style = "font-size: 62%;"),
               tags$p("Municipio con mayor aumento (%)", style = "font-size: 100%;"), 
               icon = icon("percent"),
               width = 15, 
               color = "maroon")
      
    })
    
    output$caja_mun_total_d <- renderValueBox({
      
      if(input$tempo_analisis == "Últimos 14 días"){
        
        valor_mun_tot_d <- str_to_title(p2_sem$municipio_hecho_clean[nrow(p2_sem)])
        
        valor_mun_tot_2_d <- HTML(paste0(valor_mun_tot_d,
                                         br(), 
                                         "(", 
                                         p2_sem$cambio_semana[nrow(p2_sem)], 
                                         " casos)"))
        
      }
      
      if(input$tempo_analisis == "Últimos 60 días"){
        
        valor_mun_tot_d <- str_to_title(p2_mes$municipio_hecho_clean[nrow(p2_mes)])
        
        valor_mun_tot_2_d <- HTML(paste0(valor_mun_tot_d,
                                         br(), 
                                         "(", 
                                         p2_mes$cambio_mes[nrow(p2_mes)], 
                                         " casos)"))
        
      }
      
      if(input$tempo_analisis == "Último año"){
        
        valor_mun_tot_d <- str_to_title(p2_anios$municipio_hecho_clean[nrow(p2_anios)])
        
        valor_mun_tot_2_d <- HTML(paste0(valor_mun_tot_d,
                                         br(), 
                                         "(", 
                                         p2_anios$cambio_anio[nrow(p2_anios)], 
                                         " casos)"))
        
        
      }
      
      valor_mun_tot_2_d <- gsub("-1 casos)", "-1 caso)", valor_mun_tot_2_d)
      
      valueBox(tags$p(valor_mun_tot_2_d, style = "font-size: 62%;"),
               tags$p("Municipio con mayor disminución (total)", style = "font-size: 91%;"), 
               icon = icon("arrow-down"),
               width = 15, 
               color = "red") #cocca
      
    })
    
    output$caja_mun_porc_d <- renderValueBox({
      
      if(input$tempo_analisis == "Últimos 14 días"){
        
        valor_mun_tasa_d <- str_to_title(p2_sem$municipio_hecho_clean[order(p2_sem$cambio_semana_por)][1])
        
        valor_mun_tasa_2_d <- HTML(paste0(valor_mun_tasa_d,
                                          br(), 
                                          "(", 
                                          scales::percent(p2_sem$cambio_semana_por[order(p2_sem$cambio_semana_por)][1],
                                                          accuracy = 1), 
                                          ")"))
        
      }
      
      if(input$tempo_analisis == "Últimos 60 días"){
        
        valor_mun_tasa_d <- str_to_title(p2_mes$municipio_hecho_clean[order(p2_mes$cambio_mes_por)][1])
        
        valor_mun_tasa_2_d <- HTML(paste0(valor_mun_tasa_d,
                                          br(), 
                                          "(", 
                                          scales::percent(p2_mes$cambio_mes_por[order(p2_mes$cambio_mes_por)][1],
                                                          accuracy = 1), 
                                          ")"))
        
      }
      
      if(input$tempo_analisis == "Último año"){
        
        valor_mun_tasa_d <- str_to_title(p2_anios$municipio_hecho_clean[order(p2_anios$cambio_anio_por)][1])
        
        valor_mun_tasa_2_d <- HTML(paste0(valor_mun_tasa_d,
                                          br(), 
                                          "(", 
                                          scales::percent(p2_anios$cambio_anio_por[order(p2_anios$cambio_anio_por)][1],
                                                          accuracy = 1), 
                                          ")"))
        
        
      }
      
      valueBox(tags$p(valor_mun_tasa_2_d, style = "font-size: 62%;"),
               tags$p("Municipio con mayor disminución (%)", style = "font-size: 95%;"), 
               icon = icon("percent"),
               width = 15, 
               color = "yellow")
      
    })
    
    if(input$tempo_analisis == "Últimos 14 días" ){ 
      
      t2 <- p2_sem %>% relocate(ultima_semana, .before = semana_anterior) %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        mutate(cambio_semana_por = scales::percent(cambio_semana_por, accuracy = 0.1)) %>% 
        select(-frase_1, -signo, - label_semana) %>% 
        mutate(ind = case_when(cambio_semana > 0 ~ 1,
                               cambio_semana < 0 ~ -1,
                               cambio_semana == 0 ~ 0))
      
      names(t2) <- c("Municipio",
                     paste0("Del ", format(input$fecha_hecho_final_2-6, "%d de %B"),
                            " al ", format(input$fecha_hecho_final_2, "%d de %B")),
                     paste0("Del ", format(input$fecha_hecho_final_2-13, "%d de %B"),
                            " al ", format(input$fecha_hecho_final_2-7, "%d de %B")),
                     "Cambio en número", 
                     "Cambio porcentual", 
                     "ind")
      
      
    }
    
    if(input$tempo_analisis == "Últimos 60 días" ){ 
      
      t2 <- p2_mes %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        mutate(cambio_mes_por = scales::percent(cambio_mes_por, accuracy = 0.1)) %>% 
        select(-frase_1, -signo, - label_mes) %>% 
        mutate(ind = case_when(cambio_mes > 0 ~ 1,
                               cambio_mes < 0 ~ -1,
                               cambio_mes == 0 ~ 0))
      
      names(t2) <- c("Municipio",
                     paste0("Del ", format(input$fecha_hecho_final_2-29, "%d de %B"),
                            " al ", format(input$fecha_hecho_final_2, "%d de %B")),
                     paste0("Del ", format(input$fecha_hecho_final_2-59, "%d de %B"),
                            " al ", format(input$fecha_hecho_final_2-30, "%d de %B")),
                     "Cambio en número", 
                     "Cambio porcentual", 
                     "ind")
      
    }
    if(input$tempo_analisis == "Último año" ){ 
      
      t2 <- p2_anios %>% relocate(ultimo_anio, .before = anio_anterior) %>% 
        mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
        mutate(cambio_anio_por = scales::percent(cambio_anio_por, accuracy = 0.1)) %>% 
        select(-frase_1, -signo, - label_anio) %>% 
        mutate(ind = case_when(cambio_anio > 0 ~ 1,
                               cambio_anio < 0 ~ -1,
                               cambio_anio == 0 ~ 0))
      
      names(t2) <- c("Municipio",
                     paste0("Del ", format(input$fecha_hecho_final_2-182, "%d de %B del %Y"),
                            " al ", format(input$fecha_hecho_final_2, "%d de %B del %Y")),
                     
                     paste0("Del ", format(input$fecha_hecho_final_2-365, "%d de %B del %Y"),
                            " al ", format(input$fecha_hecho_final_2-183, "%d de %B del %Y")),
                     "Cambio en número",
                     "Cambio porcentual", 
                     "ind")
      
    }
    
    datatable(t2, options = list("pageLength" = 10,
                                 columnDefs = list(list(targets = 6, visible = FALSE,className = 'dt-center', targets = 1:5)),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'font-size': '75%'});",
                                   "}"),
                                 language = list(
                                   info = ' ',
                                   paginate = list(previous = 'Anterior', `next` = 'Siguiente'))
                                 # columnDefs = list(list(className = 'dt-center', targets = 1:5))
                                 )) %>% 
      formatStyle(
        "Municipio", "ind",
        backgroundColor = styleEqual(c(0, 1, -1), c('#73746D', '#6e4854', '#63917d'))) %>% 
      formatStyle(
        "Municipio", 
        fontWeight = "bold",
        color = "white") %>% 
      formatStyle(
        columns = c(1:5),
        fontFamily = "Nutmeg-Light",
        fontSize = "80%",
        #color = '#008080',
        fontWeight = 'plain',
        #paddingRight = "0.5em",
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        #borderCollapse = "collapse",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word"#,
        #backgroundColor = '#e6e6e5'
      )
    # 
    # 
    # DT::datatable(t1_total,
    #               options = list(scrollY = "300px",
    #                              "pageLength" = 10,
    #                              initComplete = JS(
    #                                "function(settings, json) {",
    #                                "$(this.api().table().header()).css({'font-size': '85%'});",
    #                                "}"),
    #                              language = list(
    #                                info = ' ',
    #                                paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
    #                              columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
    #   formatStyle(
    #     columns = c(1:3),
    #     fontFamily = "Nutmeg-Light",
    #     fontSize = "80%",
    #     #color = '#008080',
    #     fontWeight = 'plain',
    #     #paddingRight = "0.5em",
    #     borderRightWidth = "1px",
    #     borderRightStyle = "solid",
    #     borderRightColor = "white",
    #     borderBottomColor = "#ffffff",
    #     borderBottomStyle = "solid",
    #     borderBottomWidth = "0.5px",
    #     #borderCollapse = "collapse",
    #     verticalAlign = "middle",
    #     textAlign = "center",
    #     wordWrap = "break-word"#,
    #     #backgroundColor = '#e6e6e5'
    #   )
    # 
  })
  
  data3 <- reactive({
    base_casos_clean %>% 
      filter(
        if(input$depen_check_3=="TODAS LAS DEPENDENCIAS") dep_exp!="" else str_to_upper(dep_exp) %in%input$depen_check_3,
        if(input$municipio_seleccionado=="TODOS LOS MUNICIPIOS DE JALISCO") municipio_hecho_clean!="" else str_to_upper(municipio_hecho_clean) %in% str_to_upper(input$municipio_seleccionado),
        fecha_hechos >= input$fecha_hecho_inicio_3,
        fecha_hechos <= input$fecha_hecho_final_3
        
      )
  })
  
  ## 3.3.- Caracterísiticas de los casos (Server)----
  
  ### a) Tipo de violencia ----
  
  output$tipo_violencia_plot <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height  
    
    
    # p4 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p4 <- p4 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p4 <- p4
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p4 <- p4 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    
    municipio_seleccionado_tv <- input$municipio_seleccionado
    
    # } else {
    # 
    #   municipio_seleccionado_tv <- "Todos los municipios de Jalisco"
    # }
    
    p4 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>% 
      select(econ_a3mica,
             fa_sica,
             patrimonial, 
             psicol_a3gica,
             sexual,
             otro) %>% 
      pivot_longer(cols = 1:6) %>% 
      filter(value != 0) %>% 
      group_by(name) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      mutate(name_clean = case_match(name, 
                                     "econ_a3mica" ~ "Económica",
                                     "fa_sica" ~ "Física",
                                     "patrimonial" ~ "Patrimonial",
                                     "psicol_a3gica" ~ "Psicológica",
                                     "sexual" ~ "Sexual",
                                     "otro" ~ "Otro tipo")) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por este tipo: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p4 <- p4 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p4$name_clean))
    
    
    # Value Box - tipo de violencia
    
    output$vb_tipo_violencia <- renderValueBox({
      
      
      valueBox(p4$name_clean[1], 
               paste0("Tipo de violencia más común con ", 
                      scales::percent(p4$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "fuchsia")
      
      
      
    })
    
    # Plot - tipo de violencia
    
    
    fig_4 <- p4 %>%  
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#9c4068', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_tv, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_4, tooltip = "label")
      
      

    #   plot_ly(data = p4,
    #                  x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                  y = ~ cuenta,
    #                  type = "bar",
    #                  marker = list(color = '#9c4068',
    #                                line = list(color = '#9c4068',
    #                                            width = 1.5)),
    #                  text = ~ label,
    #                  hoverinfo = "text", 
    #                  textfont=list(color='#9c4068', 
    #                                size=0)) %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_tv, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Tipo de violencia",
    #            tickangle = 0), 
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    
    # fig_4
    
    
  })
  
  ### b) Tipo de modalidad ----
  
  output$tipo_modalidad_plot <- renderPlotly({
    
    # p5 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p5 <- p5 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p5 <- p5
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p5 <- p5 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_tm <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_tm <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p5 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(modalidad_de_la_violencia) %>% 
      group_by(modalidad_de_la_violencia) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      mutate(name_clean = case_match(modalidad_de_la_violencia, 
                                     "1 - Familiar" ~ "Familiar",
                                     "2 - Laboral" ~ "Laboral",
                                     "3 - Institucional" ~ "Institucional",
                                     "4 - En la comunidad" ~ "En la comunidad",
                                     "5 - Escolar/Docente" ~ "Escolar",
                                     "6 - ObstÃ©trica" ~ "Obstétrica",
                                     "8 - Digital/CibernÃ©tica" ~ "Cibernética",
                                     "9 - Feminicida" ~ "Feminicida")) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por esta modalidad: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p5 <- p5 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p5$name_clean))
    
    
    # Value Box - tipo de modalidad
    
    output$vb_modalidad_violencia <- renderValueBox({
      
      
      valueBox(p5$name_clean[1], 
               paste0("Modalidad de violencia más común con ", 
                      scales::percent(p5$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "maroon")
      
      
      
    })
    
    # Plot - modalidad de violencia
    
    fig_5 <- p5 %>%
      filter(!is.na(name_clean)) %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#40639c', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_tm, "\n (", 
                           format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                           " al ", 
                           format(input$fecha_hecho_final_3, "%d/%b/%y"),
                           ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_5, tooltip = "label")
    
      
      
      
    #   plot_ly(data = p5,
    #                  x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                  y = ~ cuenta,
    #                  type = "bar",
    #                  marker = list(color = '#40639c',
    #                                line = list(color = '#40639c',
    #                                            width = 1.5)),
    #                  text = ~ label,
    #                  textfont=list(color='#40639c', 
    #                                size=0),
    #                  hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_tm, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Tipo de modalidad",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_5
    
  })
  
  ### c) Rango de edad ----
  
  output$rango_edad_plot <- renderPlotly({
    
    # p6 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p6 <- p6 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p6 <- p6
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p6 <- p6 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_re <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_re <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p6 <- data3() %>% 
      filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
               fecha_hechos <= input$fecha_hecho_final_3) %>%
      mutate(name_clean = case_when(edad <= 11 ~ "0 a 11 años",
                                    edad >= 12 & edad <= 18 ~ "12 a 18 años",
                                    edad >= 19 & edad <= 29 ~ "19 a 29 años",
                                    edad >= 30 & edad <= 39 ~ "30 a 39 años",
                                    edad >= 40 & edad <= 49 ~ "40 a 49 años",
                                    edad >= 50 & edad <= 59 ~ "50 a 59 años",
                                    edad >= 60 ~ "60 años o más")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      replace(is.na(.), "Sin información") %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por rango de edad: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p6 <- p6 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p6$name_clean))
    
    
    # Value Box - rango de edad
    
    output$vb_rango_edad <- renderValueBox({
      
      
      valueBox(p6$name_clean[1], 
               paste0("Rango de edad más común en víctimas con ", 
                      scales::percent(p6$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "navy")
      
      
      
    })
    
    # Plot - rango de edad
    
    fig_6 <- p6 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#ba5e36', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_re, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_6, tooltip = "label")
    
      
      
    #   plot_ly(data = p6,
    #                  x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                  y = ~ cuenta,
    #                  type = "bar",
    #                  marker = list(color = '#ba5e36',
    #                                line = list(color = '#ba5e36',
    #                                            width = 1.5)),
    #                  text = ~ label,
    #                  textfont=list(color='#ba5e36', 
    #                                size=0),
    #                  hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_re, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Rango de edad",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_6
    
  })
  
  ### d) Vínculo ----
  
  output$vinculo_plot <- renderPlotly({
    
    # p7 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p7 <- p7 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p7 <- p7
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p7 <- p7 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_vn <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_vn <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p7 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(detalle_de_va_nculo_con_victima, va_nculo_con_victima) 
    
    p7$detalle_de_va_nculo_con_victima[which(p7$detalle_de_va_nculo_con_victima == "")] <- 
      p7$va_nculo_con_victima[which(p7$detalle_de_va_nculo_con_victima == "")]
    
    p7$detalle_de_va_nculo_con_victima[which(p7$detalle_de_va_nculo_con_victima == "Otro")] <- 
      p7$va_nculo_con_victima[which(p7$detalle_de_va_nculo_con_victima == "Otro")]
    
    
    p7 <- p7 %>% 
      mutate(name_clean = case_match(detalle_de_va_nculo_con_victima, 
                                     "CÃ³nyuge o pareja" ~ "Cónyuge o pareja",
                                     "Ex pareja" ~ "Expareja",
                                     "" ~ "Sin información",
                                     "Otro" ~ "Otro",
                                     "Familiar" ~ "Familiar (no especifica)",
                                     "Seleccione" ~ "Sin información",
                                     "En la comunidad" ~ "En la comunidad",
                                     "Hijo(a)" ~ "Hijo(a)",
                                     "Vecino(a)" ~ "Vecino(a)",
                                     "Hermano(a)" ~ "Hermano(a)",
                                     "Madre o padre" ~ "Madre o padre",
                                     "TÃ­o(a)" ~ "Tío(a)",
                                     "Laboral y docente" ~ "Laboral o docente",
                                     "CompaÃ±ero(a)" ~ "Compañero(a)",
                                     "Padrastro o madrastra" ~ "Padrastro o madrastra",
                                     "Novio(a)" ~ "Novio(a)",
                                     "Primo(a)" ~ "Primo(a)",
                                     "Jefe(a) o patrÃ³n(a)" ~ "Jefe(a) o patrón(a)",
                                     "Sobrino(a)" ~ "Sobrino(a)",
                                     "Institucional" ~ "Institucional",
                                     "Abuelo(a)" ~ "Abuelo(a)",
                                     "Suegro(a)" ~ "Suegro(a)",
                                     "Concubina" ~ "Concubino(a)",
                                     "Nieto(a)" ~ "Nieto(a)",
                                     "Profesor(a)" ~ "Profesor(a)",
                                     "Servidor pÃºblico" ~ "Servidor Público")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por vínculo: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p7 <- p7 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p7$name_clean))
    
    
    # Value Box - vínculo
    
    output$vb_vinculo <- renderValueBox({
      
      
      valueBox(p7$name_clean[-which(p7$name_clean== "Sin información")][1], 
               paste0("Vínculo más común entre víctima y persona agresora con ", 
                      scales::percent(p7$porcentaje[-which(p7$name_clean== "Sin información")][1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "maroon")
      
      
      
    })
    
    # Plot - vínculo
    
    fig_7 <- p7 %>% 
      slice_head(n = 8) %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#824470', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_vn, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_7, tooltip = "label")
    
    
      
    # 
    #   plot_ly(data = p7 %>% slice_head(n = 8),
    #                  x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                  y = ~ cuenta,
    #                  type = "bar",
    #                  marker = list(color = '#824470',
    #                                line = list(color = '#824470',
    #                                            width = 1.5)),
    #                  text = ~ label,
    #                  textfont=list(color='#824470',
    #                                size=0),
    #                  hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_vn, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Vínculo",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_7
    # 
  })
  
  
  ### e) Lugar ----
  
  output$lugar_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p8 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p8 <- base_casos_clean
    #   
    # }
    # 
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p8 <- p8 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    #   municipio_seleccionado_lug <- input$municipio_seleccionado
    #   
    # } else {
    #   
    municipio_seleccionado_lug <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p8 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(descripcion_del_lugar) 
    
    
    p8 <- p8 %>% 
      mutate(name_clean = case_match(descripcion_del_lugar, 
                                     "Casa habitaciÃ³n" ~ "Casa - habitación",
                                     "Mercado" ~ "Mercado",
                                     "" ~ "Sin información",
                                     "Otro" ~ "Otro",
                                     "Negocio" ~ "Negocio",
                                     "Escuela o colegio" ~ "Escuela",
                                     "JardÃ­n o parque" ~ "Parque",
                                     "Empresa" ~ "Empresa",
                                     "Centro comercial" ~ "Centro comercial",
                                     "Centro comunitario" ~ "Centro comunitario",
                                     "Explanada" ~ "Explanada",
                                     "Centro cultural" ~ "Centro cultural",
                                     "Particular" ~ "Particular",
                                     "Centro deportivo" ~ "Centro deportivo",
                                     "Estacionamiento" ~ "Estacionamiento",
                                     "Aeropuerto" ~ "Aeropuerto",
                                     "Templo religioso" ~ "Templo religioso",
                                     "Instituciones gubernamentales" ~ "Instituciones gubernamentales",
                                     "Central de autobuses" ~ "Central de autobuses",
                                     "Centro recreativo" ~ "Centro recreativo",
                                     "AutobÃºs" ~ "Autobús")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por lugar: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p8 <- p8 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p8$name_clean))
    
    
    # Value Box - lugar
    
    output$vb_lugar <- renderValueBox({
      
      
      valueBox(p8$name_clean[-which(p8$name_clean== "Sin información")][1], 
               paste0("Lugar de los hechos más común con ", 
                      scales::percent(p8$porcentaje[-which(p8$name_clean== "Sin información")][1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "aqua")
      
      
      
    })
    
    # Plot - lugar
    
    fig_8 <- p8 %>% 
      slice_head(n = 8) %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#447042', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_lug, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_8, tooltip = "label")
    
      

    #   
    # 
    # plot_ly(data = p8 %>% slice_head(n = 8),
    #                  x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                  y = ~ cuenta,
    #                  type = "bar",
    #                  marker = list(color = '#447042',
    #                                line = list(color = '#447042',
    #                                            width = 1.5)),
    #                  text = ~ label,
    #                  textfont= list(color='#447042', size=0),
    #                  hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_lug, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Vínculo",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_8
    
  })
  
  
  ### f) Estado civil ----
  
  output$estado_civil_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p9 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p9 <- base_casos_clean
    #   
    # }
    # 
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p9 <- p9 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_ec <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_ec <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p9 <- data3() %>% 
      filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
               fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(estado_civil) 
    
    
    p9 <- p9 %>% 
      mutate(name_clean = case_match(estado_civil, 
                                     "Soltera" ~ "Soltera",
                                     "No identificada" ~ "Sin información",
                                     "Casada" ~ "Casada",
                                     "Desconocido" ~ "Sin información",
                                     "Divorciada" ~ "Divorciada",
                                     "UniÃ³n libre" ~ "Unión libre",
                                     "Concubinato" ~ "Concubinato",
                                     "Viuda" ~ "Viuda",
                                     "Separada" ~ "Separada")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por estado civil: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p9 <- p9 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p9$name_clean))
    
    
    # Value Box - estado civil
    
    output$vb_edo_civil <- renderValueBox({
      
      
      valueBox(p9$name_clean[-which(p9$name_clean== "Sin información")][1], 
               paste0("Estado civil de la víctima más común con ", 
                      scales::percent(p9$porcentaje[-which(p9$name_clean== "Sin información")][1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "purple")
      
      
      
    })
    
    # Plot - edo civil
    
    fig_9 <-p9 %>% 
      slice_head(n = 8) %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#7356b0', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ec, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_9, tooltip = "label")
    
      
      
      # plot_ly(data = p9 %>% slice_head(n = 8),
      #                x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
      #                y = ~ cuenta,
      #                type = "bar",
      #                marker = list(color = '#7356b0',
      #                              line = list(color = '#7356b0',
      #                                          width = 1.5)),
      #                text = ~ label,
      #                textfont= list(color='#7356b0', size=0),
      #                hoverinfo = "text") %>% 
      # add_text(text = ~ prettyNum(cuenta, big.mark = ","),
      #          textposition = "top",
      #          hoverinfo="none") %>% 
      # layout(title = paste0(municipio_seleccionado_ec, " (", 
      #                       format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
      #                       " al ", 
      #                       format(input$fecha_hecho_final_3, "%d/%b/%y"),
      #                       ")"),
      #        xaxis = list(
      #          title = "Estado civil",
      #          tickangle = 0),
      #        yaxis = list(
      #          title = "Número de casos"),
      #        showlegend = FALSE,
      #        layout.separators=",.",
      #        hoverlabel=list(bgcolor="white")) 
    
    
    # fig_9
    
  })
  
  ### f) Migrante, indígenas y mujeres embarazadas ----
  
  # Value Box - Migrante
  
  output$vb_migrante <- renderValueBox({
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p10 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p10 <- base_casos_clean
    #   
    # }
    # 
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p10 <- p10 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    # } 
    # 
    p10 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(migrante, pertenece_etnia, esta_embarazada)
    
    
    valueBox(length(p10$migrante[grepl("SÃ­", p10$migrante, ignore.case = T)]), 
             "Casos de mujeres migrantes recibidos", 
             icon = NULL,
             width = 18, 
             color = "fuchsia")
    
  })
  
  # Value Box - Etnia
  
  output$vb_etnia <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p11 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p11 <- base_casos_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p11 <- p11 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    # } 
    # 
    p11 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(migrante, pertenece_etnia, esta_embarazada)
    
    valueBox(length(p11$pertenece_etnia[grepl("SÃ­", p11$pertenece_etnia, ignore.case = T)]), 
             "Casos de mujeres indígenas recibidos", 
             icon = NULL,
             width = 18, 
             color = "purple")
    
    
  })
  
  # Value Box - embarazada
  
  output$vb_embarazada <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p12 <- base_casos_clean %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p12 <- base_casos_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p12 <- p12 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    # } 
    # 
    p12 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(migrante, pertenece_etnia, esta_embarazada)
    
    
    
    valueBox(length(p12$esta_embarazada[grepl("Si", p12$esta_embarazada, ignore.case = T)]), 
             "Casos de mujeres embarazadas recibidos", 
             icon = NULL,
             width = 18, 
             color = "navy")
    
    
    
    
  })
  
  
  ### g) Número de hijos ----
  
  output$numero_de_hijos_plot <- renderPlotly({
    
    # p21 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p21 <- p21 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p21 <- p21
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p21 <- p21 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_nh <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_nh <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p21 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(numero_de_hijos) 
    
    
    p21 <- p21 %>% 
      mutate(name_clean = case_when(numero_de_hijos <= 0  ~ "Sin hijos",
                                    numero_de_hijos >0 & numero_de_hijos < 5 ~
                                      paste0(as.character(numero_de_hijos), " hijos"),
                                    numero_de_hijos >= 5 ~ "5 hijos o más")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(name_clean = gsub("1 hijos", "1 hijo", name_clean)) %>% 
      replace(is.na(.), "Sin información") %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por número de hijos: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p21 <- p21 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p21$name_clean))
    
    
    # Value Box - número de hijos
    
    output$vb_numero_de_hijos <- renderValueBox({
      
      
      valueBox(p21$name_clean[1], 
               paste0("Número de hijos de la víctima más común con ", 
                      scales::percent(p21$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "red")
      
      
      
    })
    
    # Plot - número de hijos
    
    fig_21 <- p21 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#ab673a', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_nh, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_21, tooltip = "label")
    
      
      
      
    #   
    #   
    #   plot_ly(data = p21,
    #                   x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#ab673a',
    #                                 line = list(color = '#ab673a',
    #                                             width = 1.5)),
    #                   textfont= list(color='#ab673a', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_nh, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Número de hijos",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_21
    
  })
  
  ### h) Actividades de la víctima ----
  
  output$actividades_victima_plot <- renderPlotly({
    
    # p22 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p22 <- p22 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p22 <- p22
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p22 <- p22 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    municipio_seleccionado_actv <- input$municipio_seleccionado
    #   
    # } else {
    #   
    #   municipio_seleccionado_actv <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p22 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(starts_with("vict")) %>% 
      pivot_longer(cols = 1:9) %>% 
      filter(value != 0) %>% 
      select(-value)
    
    p22 <- p22 %>% 
      mutate(name_clean = case_match(name,
                                     "vict_trabaja_f_hogar" ~ "Trabaja fuera",
                                     "vict_se_desconoce" ~ "Se desconoce",
                                     "vict_trabaja_hogar" ~ "Trabaja en el hogar",
                                     "vict_jubilada_pensionada" ~ "Jubilada / pensionada",
                                     "vict_estudia" ~ "Estudia",
                                     "vict_otro" ~ "Otra actividad",
                                     "vict_pensionada" ~ "Jubilada / pensionada",
                                     "vict_act_ilicita" ~ "Actividad ilícita")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por actividad principal: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p22 <- p22 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p22$name_clean))
    
    
    # Value Box - actividad principal
    
    output$vb_actividad_victima <- renderValueBox({
      
      
      valueBox(p22$name_clean[-which(p22$name_clean=="Se desconoce")][1], 
               paste0("Actividad de la víctima más común con ", 
                      scales::percent(p22$porcentaje[-which(p22$name_clean=="Se desconoce")][1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "maroon")
      
      
      
    })
    
    # Plot - número de hijos
    
    fig_22 <-p22 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#9c5286', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_actv, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_22, tooltip = "label")
    
      
    #   plot_ly(data = p22,
    #                   x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#9c5286',
    #                                 line = list(color = '#9c5286',
    #                                             width = 1.5)),
    #                   text = ~ label,
    #                   textfont=list(color='#9c5286', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_actv, " (", 
    #                         format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_3, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Actividad de la víctima",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_22
    # 
  })
  
  ### i) Value Box - conocimiento autoridad ----
  
  output$vb_conocimiento_a <- renderValueBox({
    
    # p23 <- base_casos_clean
    # 
    # if(!grepl("Todas las dependencias", input$depen_check_3)){
    #   
    #   p23 <- p23 %>% 
    #     filter(dep_exp %in% input$depen_check_3)
    #   
    # } else {
    #   
    #   p23 <- p23
    #   
    # }
    # 
    # if(input$municipio_seleccionado != "Todos los municipios de Jalisco"){
    #   
    #   p23 <- p23 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado))
    #   
    # } 
    # 
    p23 <- data3() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_3 &
      #          fecha_hechos <= input$fecha_hecho_final_3) %>%
      select(conocimiento_de_autoridad) %>% 
      group_by(conocimiento_de_autoridad) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      filter(conocimiento_de_autoridad == "Si")
    
    valueBox(paste0("En ", scales::percent(p23$porcentaje, accuracy = 1)), 
             "de los casos hay conocimiento de los hechos por parte de las autoridades", 
             icon = NULL,
             width = 18, 
             color = "yellow")
    
    
  })
  
  
  ## 3.4.- Características de las personas agresoras (Server)----
  data4 <- reactive({
    base_agre_clean %>% 
      filter(
        if(input$depen_check_4=="TODAS LAS DEPENDENCIAS") dep_exp!="" else str_to_upper(dep_exp) %in% input$depen_check_4,
        if(input$municipio_seleccionado_2=="TODOS LOS MUNICIPIOS DE JALISCO") municipio_hecho_clean!="" else str_to_upper(municipio_hecho_clean) %in% input$municipio_seleccionado_2,
        fecha_hechos >= input$fecha_hecho_inicio_4,
        fecha_hechos <= input$fecha_hecho_final_4
      )
  })
  ### a) Edad----
  
  output$edad_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p13 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p13 <- base_agre_clean
    #   
    # }
    # 
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p13 <- p13 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_e_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_e_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    
    p13 <- data4() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
      #          fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(edad) 
    
    p13 <- p13 %>% 
      mutate(name_clean = case_when(     edad >=  0 & edad <= 11 ~ "0 a 11 años",
                                         edad >= 12 & edad <= 18 ~ "12 a 18 años",
                                         edad >= 19 & edad <= 29 ~ "19 a 29 años",
                                         edad >= 30 & edad <= 39 ~ "30 a 39 años",
                                         edad >= 40 & edad <= 49 ~ "40 a 49 años",
                                         edad >= 50 & edad <= 59 ~ "50 a 59 años",
                                         edad >= 60 ~ "60 años o más")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      replace(is.na(.), "Sin información") %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por rango de edad: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p13 <- p13 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p13$name_clean))
    
    
    # Value Box - edad agresor
    
    output$vb_edad_agresor <- renderValueBox({
      
      if("Sin información" %in% unique(p13$name_clean)){
        
        valor_edad_agresor <- 
          p13$name_clean[-which(p13$name_clean== "Sin información")][1]
        
        por_edad_agresor <- 
          scales::percent(p13$porcentaje[-which(p13$name_clean== "Sin información")][1], accuracy = 1)
        
      } else {
        
        valor_edad_agresor <- 
          p13$name_clean[1]
        
        por_edad_agresor <- 
          scales::percent(p13$porcentaje[1], accuracy = 1)
        
      }
      
      
      valueBox(valor_edad_agresor, 
               paste0("Rango de edad más común de la persona agresora con ", 
                      por_edad_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "purple")
      
      
      
    })
    
    # Plot - rango edad agresor
    
    fig_13 <- p13 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#58569c', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_e_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_13, tooltip = "label")
    
    
      # plot_ly(data = p13,
      #                 x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
      #                 
      #                 y = ~ cuenta,
      #                 type = "bar",
      #                 marker = list(color = '#58569c',
      #                               line = list(color = '#58569c',
      #                                           width = 1.5)),
      #                 textfont=list(color='#58569c', size=0),
      #                 text = ~ label,
      #                 hoverinfo = "text") %>% 
      # add_text(text = ~ prettyNum(cuenta, big.mark = ","),
      #          textposition = "top",
      #          hoverinfo="none") %>% 
      # layout(title = paste0(municipio_seleccionado_e_ag, " (", 
      #                       format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
      #                       " al ", 
      #                       format(input$fecha_hecho_final_4, "%d/%b/%y"),
      #                       ")"),
      #        xaxis = list(
      #          title = "Rango de edad",
      #          tickangle = 0),
      #        yaxis = list(
      #          title = "Número de casos"),
      #        showlegend = FALSE,
      #        layout.separators=",.",
      #        hoverlabel=list(bgcolor="white")) 
      # 
    # 
    # fig_13
    
  })
  
  ### b) Escolaridad----
  
  output$escolaridad_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p14 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p14 <- base_agre_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p14 <- p14 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_es_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_es_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p14 <- data4() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
      #          fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(escolaridad) 
    
    p14 <- p14 %>% 
      mutate(name_clean = case_match(escolaridad, 
                                     "No identificado" ~ "Sin información",
                                     "Primaria" ~ "Primaria",
                                     "Ninguna" ~ "Ninguna",
                                     "Secundaria" ~ "Secundaria",
                                     "Preparatoria" ~ "Preparatoria",
                                     "Seleccione" ~ "Sin información",
                                     "Carrera tÃ©cnica comercial" ~ "Carrera Técnica",
                                     "Estudios que no requieren validÃ©z oficial" ~ "Sin información",
                                     "MaestrÃ­a" ~ "Posgrado",
                                     "Posgrado" ~ "Posgrado",
                                     "Doctorado" ~ "Doctorado",
                                     "Preescolar" ~ "Ninguna", 
                                     "Licenciatura" ~ "Licenciatura", 
                                     "" ~ "Sin información"))  %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por escolaridad: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p14 <- p14 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p14$name_clean))
    
    
    # Value Box - escolaridad
    
    output$vb_escolaridad_agresor <- renderValueBox({
      
      if("Sin información" %in% unique(p14$name_clean)){
        
        valor_escolaridad_agresor <- 
          p14$name_clean[-which(p14$name_clean== "Sin información")][1]
        
        por_escolaridad_agresor <- 
          scales::percent(p14$porcentaje[-which(p14$name_clean== "Sin información")][1], accuracy = 1)
        
      } else {
        
        valor_escolaridad_agresor <- 
          p14$name_clean[1]
        
        por_escolaridad_agresor <- 
          scales::percent(p14$porcentaje[1], accuracy = 1)
        
      }
      
      
      valueBox(valor_escolaridad_agresor, 
               paste0("Escolaridad reportada de la persona agresora más común con ", 
                      por_escolaridad_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "red")
      
      
      
    })
    
    # Plot - escolaridad agresor
    
    fig_14 <- p14 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#3d9970', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_es_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_14, tooltip = "label")
    
      
      
    #   
    #   plot_ly(data = p14,
    #                   x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#3d9970',
    #                                 line = list(color = '#3d9970',
    #                                             width = 1.5)),
    #                   textfont=list(color='#3d9970', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_es_ag, " (", 
    #                         format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_4, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Escolaridad",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_14
    # 
  })
  
  ### c) Género del agresor----
  
  output$genero_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p24 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p24 <- base_agre_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p24 <- p24 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_gen_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_gen_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p24 <- data4() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
      #          fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(genero) 
    
    p24 <- p24 %>% 
      mutate(name_clean = case_match(genero, 
                                     "Hombre" ~ "Hombre",
                                     "" ~ "Sin información",
                                     "Mujer" ~ "Mujer",
                                     "Seleccione" ~ "Sin información",
                                     "Otro (Especifique)" ~ "Sin información"))  %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por género de la persona agresora: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p24 <- p24 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p24$name_clean))
    
    
    # Value Box - escolaridad
    
    output$vb_genero_agresor <- renderValueBox({
      
      if("Sin información" %in% unique(p24$name_clean)){
        
        valor_genero_agresor <- 
          p24$name_clean[-which(p24$name_clean== "Sin información")][1]
        
        por_genero_agresor <- 
          scales::percent(p24$porcentaje[-which(p24$name_clean== "Sin información")][1], accuracy = 1)
        
      } else {
        
        valor_genero_agresor <- 
          p24$name_clean[1]
        
        por_genero_agresor <- 
          scales::percent(p24$porcentaje[1], accuracy = 1)
        
      }
      
      
      valueBox(valor_genero_agresor, 
               paste0("Género de la persona agresora más común con ", 
                      por_genero_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "yellow")
      
      
      
    })
    
    # Plot - genero agresor
    
    fig_24 <-p24 %>%
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#bf5841', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_gen_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_24, tooltip = "label")
    
      
      
      
    #   plot_ly(data = p24,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#bf5841',
    #                                 line = list(color = '#bf5841',
    #                                             width = 1.5)),
    #                   textfont= list(color='#bf5841', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_gen_ag, " (", 
    #                         format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_4, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Género",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_24
    
  })
  
  ### d) Actividad del agresor----
  
  output$actividad_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p25 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p25 <- base_agre_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p25 <- p25 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_act_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_act_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p25 <- data4() %>% 
      # filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
      #          fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(starts_with("prin")) 
    
    p25 <- p25 %>% 
      pivot_longer(cols = 1:10) %>% 
      filter(value != 0) %>% 
      mutate(name_clean = case_match(name, 
                                     "prin_act_ag_se_desc" ~ "Sin información",
                                     "prin_act_ag_trab_f_hogar" ~ "Fuera del hogar",
                                     "prin_act_ag_otro" ~ "Otro",
                                     "prin_act_ag_ning" ~ "Ninguna",
                                     "prin_act_ag_act_ilicita" ~ "Actividad ilícita",
                                     "prin_act_ag_estudia" ~ "Estudia",
                                     "prin_act_ag_trab_hogar" ~ "Trabaja en el hogar",
                                     "prin_act_ag_jub_pens" ~ "Jubilado o pensionado",
                                     "prin_act_ag_pens" ~ "Jubilado o pensionado"
      ))  %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por actividad de la persona agresora: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p25 <- p25 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p25$name_clean))
    
    
    # Value Box - actividad agresor
    
    output$vb_actividad_agresor <- renderValueBox({
      
      if("Sin información" %in% unique(p25$name_clean)){
        
        valor_actividad_agresor <- 
          p25$name_clean[-which(p25$name_clean== "Sin información")][1]
        
        por_actividad_agresor <- 
          scales::percent(p25$porcentaje[-which(p25$name_clean== "Sin información")][1], accuracy = 1)
        
      } else {
        
        valor_actividad_agresor <- 
          p25$name_clean[1]
        
        por_actividad_agresor <- 
          scales::percent(p25$porcentaje[1], accuracy = 1)
        
      }
      
      
      valueBox(valor_actividad_agresor, 
               paste0("Actividad principal de la persona agresora más común con ", 
                      por_actividad_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "maroon")
      
      
      
    })
    
    # Plot - actividad agresor
    
    fig_25 <- p25 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#9e4767', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_act_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_25, tooltip = "label")
    
      
      
      
    #   plot_ly(data = p25,
    #                   x = ~ reorder(str_wrap(name_clean, width = 6), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#9e4767',
    #                                 line = list(color = '#9e4767',
    #                                             width = 1.5)),
    #                   textfont=list(color='#9e4767', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_act_ag, " (", 
    #                         format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_4, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Actividad principal",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_25
    # 
  })
  
  ### e) Fuente de ingresos del agresor----
  
  output$fuente_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p26 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p26 <- base_agre_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p26 <- p26 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_fte_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_fte_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    
    p26 <- data4() %>% 
      filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
               fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(starts_with("fte")) 
    
    p26 <- p26 %>% 
      pivot_longer(cols = 1:9) %>% 
      filter(value != 0) %>% 
      mutate(name_clean = case_match(name, 
                                     "fte_ingresos_ag_trab_formal" ~ "Trabajo formal",
                                     "fte_ingresos_ag_remesas" ~ "Remesas",
                                     "fte_ingresos_ag_trab_informal" ~ "Trabajo informal",
                                     "fte_ingresos_ag_otro" ~ "Otra",
                                     "fte_ingresos_ag_ahorros" ~ "Ahorros",
                                     "fte_ingresos_ag_herencia" ~ "Herencia",
                                     "fte_ingresos_ag_rentas" ~ "Rentas",
                                     "fte_ingresos_ag_pensiones" ~ "Pensiones"
      ))  %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por fuente de ingresos de la persona agresora: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p26 <- p26 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p26$name_clean))
    
    
    # Value Box - fuente de ingresos agresor
    
    output$vb_fuente_agresor <- renderValueBox({
      
      if("Sin información" %in% unique(p26$name_clean)){
        
        valor_fuente_agresor <- 
          p26$name_clean[-which(p26$name_clean== "Sin información")][1]
        
        por_fuente_agresor <- 
          scales::percent(p26$porcentaje[-which(p26$name_clean== "Sin información")][1], accuracy = 1)
        
      } else {
        
        valor_fuente_agresor <- 
          p26$name_clean[1]
        
        por_fuente_agresor <- 
          scales::percent(p26$porcentaje[1], accuracy = 1)
        
      }
      
      
      valueBox(valor_fuente_agresor, 
               paste0("Fuente de ingresos de la persona agresora más común con ", 
                      por_fuente_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "aqua")
      
      
      
    })
    
    # Plot - actividad agresor
    
    fig_26 <- p26 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#a6568b', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_fte_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_26, tooltip = "label")
    
      
    #   plot_ly(data = p26,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#a6568b',
    #                                 line = list(color = '#a6568b',
    #                                             width = 1.5)
    #                   ),
    #                   text = ~ label,
    #                   textfont=list(color='#a6568b', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_fte_ag, " (", 
    #                         format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
    #                         " al ",
    #                         format(input$fecha_hecho_final_4, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Fuente de ingreso",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_26
    # 
  })
  
  ### f) Uso de drogas o alcohol del agresor----
  
  output$drogas_agresor_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p27 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p27 <- base_agre_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p27 <- p27 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_drog_ag <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_drog_ag <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p27 <- data4() %>% 
      filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
               fecha_hechos <= input$fecha_hecho_final_4) %>%
      select(droga_alcohol) 
    
    p27 <- p27 %>% 
      mutate(name_clean = case_match(droga_alcohol, 
                                     1 ~ "Sí drogas / alcohol",
                                     0 ~ "No drogas / alcohol"
      ))  %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por situación: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p27 <- p27 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p27$name_clean))
    
    
    # Value Box - fuente de ingresos agresor
    
    output$vb_drogas_agresor <- renderValueBox({
      
      valor_drogas_agresor <- 
        p27$name_clean[1]
      
      por_drogas_agresor <- 
        scales::percent(p27$porcentaje[1], accuracy = 1)
      
      valueBox(valor_drogas_agresor, 
               paste0("Tipo de situación más común con ", 
                      por_drogas_agresor,
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "fuchsia")
      
    })
    
    # Plot - drogas agresor
    
    fig_27 <- p27 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#4a8594', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_drog_ag, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_27, tooltip = "label")
    
      
      
    #   plot_ly(data = p27,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#4a8594',
    #                                 line = list(color = '#4a8594',
    #                                             width = 1.5)),
    #                   textfont=list(color='#4a8594', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_drog_ag, " (", 
    #                         format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_hecho_final_4, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Uso de drogas",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de casos"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_27
    
  })
  
  
  ### g) Armas ----
  
  output$vb_arma_agresor <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_4)){
    #   
    #   p15 <- base_agre_clean %>% 
    #     filter(dep_exp %in% input$depen_check_4)
    #   
    # } else {
    #   
    #   p15 <- base_agre_clean
    #   
    # }
    # 
    # p15 <-  p15 %>% 
    #   filter(fecha_hechos >= input$fecha_hecho_inicio_4 &
    #            fecha_hechos <= input$fecha_hecho_final_4) %>%
    #   filter(posee_algun_tipo_de_arma == "SI")
    # 
    # if(input$municipio_seleccionado_2 != "Todos los municipios de Jalisco"){
    #   
    #   p15 <- p15 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_2))
    #   
    municipio_seleccionado_arma <- input$municipio_seleccionado_2
    #   
    # } else {
    #   
    #   municipio_seleccionado_arma <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    #### i) Plot tipo de arma ----
    
    output$tipo_arma_plot <- renderPlotly({
      
      p15_1 <- data4() %>% 
        select(chacos, 
               macanas,
               objeto_punzo_cortante,
               machete,
               proyectil,
               arma_fuego_corta,
               arma_fuego_larga,
               otra_fuego_larga) %>% 
        pivot_longer(cols = 1:8) %>% 
        filter(value != 0) %>% 
        mutate(name_clean = case_match(name, 
                                       "chacos" ~ "Chacos o macanas", 
                                       "macanas" ~ "Chacos o macanas", 
                                       "objeto_punzo_cortante" ~ "Objeto punzocortante", 
                                       "machete" ~ "Machete", 
                                       "proyectil" ~ "Proyectil o arma de fuego", 
                                       "arma_fuego_corta" ~ "Proyectil o arma de fuego", 
                                       "arma_fuego_larga" ~ "Proyectil o arma de fuego", 
                                       "otra_fuego_larga" ~ "Proyectil o arma de fuego"
        )) %>% 
        group_by(name_clean) %>% 
        summarise(cuenta = n()) %>% 
        ungroup() %>% 
        mutate(total = sum(cuenta)) %>% 
        mutate(porcentaje = cuenta/total) %>% 
        mutate(label = paste0("<b>",
                              name_clean,
                              "</b>",
                              "\n",
                              "Porcentaje: ", 
                              "<b>",
                              scales::percent(porcentaje, accuracy = 0.1), 
                              "</b>"))
      
      fig_15 <- plot_ly(data = p15_1, 
                        values = ~ porcentaje, 
                        labels = ~ label, 
                        parents = c(""),
                        type = "treemap",
                        hoverinfo= ~"label") %>% 
        layout(title = paste0(municipio_seleccionado_arma, 
                              " (", 
                              format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_4, "%d/%b/%y"),
                              ")"))
      
      fig_15
      
    })
    
    
    
    #### ii) Rango de edad arma ----
    
    output$edad_agresor_arma_plot <- renderPlotly({
      
      p15_2 <- data4() %>% 
        select(edad) %>% 
        mutate(name_clean = case_when(     edad >=  0 & edad <= 11 ~ "0 a 11 años",
                                           edad >= 12 & edad <= 18 ~ "12 a 18 años",
                                           edad >= 19 & edad <= 29 ~ "19 a 29 años",
                                           edad >= 30 & edad <= 39 ~ "30 a 39 años",
                                           edad >= 40 & edad <= 49 ~ "40 a 49 años",
                                           edad >= 50 & edad <= 59 ~ "50 a 59 años",
                                           edad >= 60 ~ "60 años o más")) %>% 
        group_by(name_clean) %>% 
        summarise(cuenta = n()) %>% 
        ungroup() %>% 
        mutate(total = sum(cuenta)) %>% 
        mutate(porcentaje = cuenta/total) %>% 
        mutate(label = paste0("<b>",
                              name_clean,
                              "</b>",
                              "\n",
                              "Porcentaje: ", 
                              "<b>",
                              scales::percent(porcentaje, accuracy = 0.1), 
                              "</b>"))
      
      fig_16 <- plot_ly(data = p15_2, 
                        values = ~ porcentaje, 
                        labels = ~ label, 
                        parents = c(""),
                        type = "treemap",
                        hoverinfo= ~"label") %>% 
        layout(title = paste0(municipio_seleccionado_arma, 
                              " (", 
                              format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_4, "%d/%b/%y"),
                              ")"))
      
      fig_16
      
    })
    
    
    #### iii) Lugar arma----
    
    output$lugar_arma_plot <- renderPlotly({
      
      p15_3 <- data4() %>% 
        select(lugar_hechos)  %>% 
        mutate(name_clean = case_match(lugar_hechos, 
                                       "Espacio particular" ~ "Espacio particular",
                                       "Transporte privado" ~ "Transporte privado",
                                       "Espacio pÃºblico" ~ "Espacio público")) %>% 
        group_by(name_clean) %>% 
        summarise(cuenta = n()) %>% 
        ungroup() %>% 
        mutate(total = sum(cuenta)) %>% 
        mutate(porcentaje = cuenta/total) %>% 
        mutate(label = paste0("<b>",
                              name_clean,
                              "</b>",
                              "\n",
                              "Porcentaje: ", 
                              "<b>",
                              scales::percent(porcentaje, accuracy = 0.1), 
                              "</b>"))
      
      fig_17 <-  plot_ly(data = p15_3,
                         values = ~ porcentaje, 
                         labels = ~ label, 
                         parents = c(""),
                         type = "treemap",
                         hoverinfo= ~"label")  %>% 
        layout(title = paste0(municipio_seleccionado_arma, 
                              " (", 
                              format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_4, "%d/%b/%y"),
                              ")"))
      
      fig_17
      
    })
    
    
    #### iv) Vínculo arma----
    
    output$vinculo_arma_plot <- renderPlotly({
      
      p15_4 <- data4() %>% 
        select(vinculo_con_va_ctima)  %>% 
        mutate(name_clean = case_match(vinculo_con_va_ctima, 
                                       "Otro" ~ "Otro",
                                       "CÃ³nyuge o pareja" ~ "Cónyuge, pareja o novio(a)",
                                       "Seleccione" ~ "Sin información",
                                       "" ~ "Sin información",
                                       "Ex pareja" ~ "Ex pareja",
                                       "Hijo(a)" ~ "Familiar",
                                       "Padrastro o madrastra" ~ "Familiar",
                                       "Novio(a)" ~ "Cónyuge, pareja o novio(a)",
                                       "Primo(a)" ~ "Familiar",
                                       "Concubina" ~ "Cónyuge, pareja o novio(a)",
                                       "Vecino(a)" ~ "Vecino(a)",
                                       "Sobrino(a)" ~ "Familiar",
                                       "Hermano(a)" ~ "Familiar",
                                       "Madre o padre" ~ "Familiar",
                                       "CompaÃ±ero(a)" ~ "Laboral",
                                       "Jefe(a) o patrÃ³n(a)" ~ "Laboral",
                                       "TÃ­o(a)" ~ "Familiar",
                                       "Nieto(a)" ~ "Familiar")) %>% 
        
        group_by(name_clean) %>% 
        summarise(cuenta = n()) %>% 
        ungroup() %>% 
        mutate(total = sum(cuenta)) %>% 
        mutate(porcentaje = cuenta/total) %>% 
        mutate(label = paste0("<b>",
                              name_clean,
                              "</b>",
                              "\n",
                              "Porcentaje: ", 
                              "<b>",
                              scales::percent(porcentaje, accuracy = 0.1), 
                              "</b>"))
      
      fig_18 <- plot_ly(data = p15_4,
                        values = ~ porcentaje, 
                        labels = ~ label, 
                        parents = c(""),
                        type = "treemap",
                        hoverinfo= ~"label")   %>% 
        layout(title = paste0(municipio_seleccionado_arma, 
                              " (", 
                              format(input$fecha_hecho_inicio_4, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_4, "%d/%b/%y"),
                              ")"))
      
      fig_18
      
    })
    
    output$vb_arma_agresor_por <- renderValueBox({
      
      por_armas <- nrow(data4())/nrow(base_agre_clean)
      
      valueBox(scales::percent(por_armas, accuracy = 1), 
               "Porcentaje del total de casos en los que se reportó que la persona agresora poseía algún arma", 
               icon = NULL,
               width = 18, 
               color = "purple")
      
      
      
    })
    
    
    
    #### v) Value box arma---- 
    
    valueBox(prettyNum(nrow(data4()), big.mark = ","), 
             "Casos en los que se reportó que la persona agresora poseía algún tipo de arma", 
             icon = NULL,
             width = 18, 
             color = "purple")
    
  })
  
  
  
  ## 3.5.- Interacción de variables (Server)----
  
  ### a) Plot general----
  
  output$plot_interaccion <- renderPlotly({
    
    #### i) Cuando hay dos variables iguales----
    
    if(input$eje_x == input$eje_y){
      
      fig_19 <- plot_ly(data = data.frame(x =2, y = 2),
                        x = ~ x, 
                        y = ~ y,
                        marker = list(color ="white")) %>% 
        add_annotations(x = 2,
                        y = 2,
                        text = "Error: Por favor elige dos variables diferentes.",
                        showarrow = F) %>% 
        layout(xaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
      
      fig_19
      
    }
    
    #### ii) Cuando hay dos variables diferentes que no son de tiempo----
    
    if(input$eje_x != input$eje_y & input$eje_x != "fecha_hechos"){
      
      p16_v <- base_casos_clean %>% 
        select(euv, 
               econ_a_mica, 
               fa_sica,
               patrimonial, 
               psicol_a_gica, 
               sexual, 
               otro) %>% 
        pivot_longer(cols = 2:7) %>% 
        filter(value != 0) %>% 
        mutate(modalidad = case_match(name, 
                                      "econ_a3mica" ~ "Económica",
                                      "fa_sica" ~ "Física",
                                      "patrimonial" ~ "Patrimonial",
                                      "psicol_a3gica" ~ "Psicológica",
                                      "sexual" ~ "Sexual",
                                      "otro" ~ "Otro tipo")) %>% 
        select(-value, -name)
      
      p16_ac <- base_casos_clean %>% 
        select(euv, starts_with("vict")) %>% 
        pivot_longer(cols = 2:10) %>% 
        filter(value != 0) %>% 
        select(-value)
      
      p16_ac <- p16_ac %>% 
        mutate(actividad = case_match(name,
                                      "vict_trabaja_f_hogar" ~ "Trabaja fuera",
                                      "vict_se_desconoce" ~ "Se desconoce",
                                      "vict_trabaja_hogar" ~ "Trabaja en el hogar",
                                      "vict_jubilada_pensionada" ~ "Jubilada/pensionada",
                                      "vict_estudia" ~ "Estudia",
                                      "vict_otro" ~ "Otra actividad",
                                      "vict_pensionada" ~ "Jubilada/pensionada",
                                      "vict_act_ilicita" ~ "Actividad ilícita"))
      
      p16_ac_ag <- base_agre_clean %>% 
        select(euv, starts_with("fte")) %>% 
        pivot_longer(cols = 2:10) %>% 
        filter(value != 0) %>% 
        mutate(ingresos_agresor = case_match(name, 
                                             "fte_ingresos_ag_trab_formal" ~ "Trabajo formal",
                                             "fte_ingresos_ag_remesas" ~ "Remesas",
                                             "fte_ingresos_ag_trab_informal" ~ "Trabajo informal",
                                             "fte_ingresos_ag_otro" ~ "Otra",
                                             "fte_ingresos_ag_ahorros" ~ "Ahorros",
                                             "fte_ingresos_ag_herencia" ~ "Herencia",
                                             "fte_ingresos_ag_rentas" ~ "Rentas",
                                             "fte_ingresos_ag_pensiones" ~ "Pensiones"
        )) %>% 
        select(-name, - value)
      
      p16_ing_ag <- base_agre_clean %>% 
        select(euv, starts_with("prin")) %>% 
        pivot_longer(cols = 2:11) %>% 
        filter(value != 0) %>% 
        mutate(actividad_del_agresor = case_match(name, 
                                                  "prin_act_ag_se_desc" ~ "Sin información",
                                                  "prin_act_ag_trab_f_hogar" ~ "Fuera del hogar",
                                                  "prin_act_ag_otro" ~ "Otro",
                                                  "prin_act_ag_ning" ~ "Ninguna",
                                                  "prin_act_ag_act_ilicita" ~ "Actividad ilícita",
                                                  "prin_act_ag_estudia" ~ "Estudia",
                                                  "prin_act_ag_trab_hogar" ~ "Trabaja en el hogar",
                                                  "prin_act_ag_jub_pens" ~ "Jubilado o pensionado",
                                                  "prin_act_ag_pens" ~ "Jubilado o pensionado"
        )) %>% 
        select(-name, - value)
      
      
      
      p16 <- base_casos_clean %>% 
        select(euv, 
               fecha_hechos, 
               rango_de_edades, 
               modalidad_de_la_violencia,
               estado_civil,
               descripcion_del_lugar, 
               detalle_de_va_nculo_con_victima, 
               va_nculo_con_victima, 
               municipio_hecho_clean, 
               edad, 
               numero_de_hijos, 
               conocimiento_de_autoridad)
      
      p16$detalle_de_va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "")] <- 
        p16$va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "")]
      
      p16$detalle_de_va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "Otro")] <- 
        p16$va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "Otro")]
      
      
      
      p16_ag <- base_agre_clean %>% 
        select(euv, 
               edad, 
               escolaridad, 
               chacos, 
               macanas, 
               objeto_punzo_cortante,
               machete,
               proyectil,
               arma_fuego_corta,
               arma_fuego_larga,
               otra_fuego_larga, 
               genero,
               droga_alcohol) %>% 
        mutate(genero_del_agresor = case_match(genero, 
                                               "Hombre" ~ "Hombre",
                                               "" ~ "Sin información",
                                               "Mujer" ~ "Mujer",
                                               "Seleccione" ~ "Sin información",
                                               "Otro (Especifique)" ~ "Sin información")) %>% 
        mutate(uso_de_sustancias = case_match(droga_alcohol, 
                                              1 ~ "Sí drogas/alcohol",
                                              0 ~ "No drogas/alcohol")) %>% 
        mutate(indicador_de_arma_blanca = case_when(chacos == 1 |
                                                      macanas == 1 |
                                                      objeto_punzo_cortante == 1|
                                                      machete == 1  ~ "Usó arma blanca")) %>%
        mutate(indicador_de_arma_fuego = case_when(proyectil == 1 |
                                                     arma_fuego_corta == 1 |
                                                     arma_fuego_larga == 1|
                                                     otra_fuego_larga == 1  ~ "Usó arma de fuego")) %>% 
        replace(is.na(.), "No usó arma") %>% 
        select(euv, 
               edad, 
               escolaridad, 
               indicador_de_arma_blanca, 
               indicador_de_arma_fuego, 
               genero_del_agresor, 
               uso_de_sustancias) %>% 
        mutate(rango_de_edades_agresor =case_when(     edad >=  0 & edad <= 11 ~ "0 a 11 años",
                                                       edad >= 12 & edad <= 18 ~ "12 a 18 años",
                                                       edad >= 19 & edad <= 29 ~ "19 a 29 años",
                                                       edad >= 30 & edad <= 39 ~ "30 a 39 años",
                                                       edad >= 40 & edad <= 49 ~ "40 a 49 años",
                                                       edad >= 50 & edad <= 59 ~ "50 a 59 años",
                                                       edad >= 60 ~ "60 años o más")) %>% 
        mutate(escolaridad = case_match(escolaridad, 
                                        "No identificado" ~ "Sin información",
                                        "Primaria" ~ "Primaria",
                                        "Ninguna" ~ "Ninguna",
                                        "Secundaria" ~ "Secundaria",
                                        "Preparatoria" ~ "Preparatoria",
                                        "Seleccione" ~ "Sin información",
                                        "Carrera tÃ©cnica comercial" ~ "Carrera Técnica",
                                        "Estudios que no requieren validÃ©z oficial" ~ "Sin información",
                                        "MaestrÃ­a" ~ "Posgrado",
                                        "Posgrado" ~ "Posgrado",
                                        "Doctorado" ~ "Doctorado",
                                        "Preescolar" ~ "Ninguna", 
                                        "Licenciatura" ~ "Licenciatura", 
                                        "" ~ "Sin información")) %>% 
        right_join(., p16_ac_ag, multiple = "all", by = "euv") %>% 
        right_join(., p16_ing_ag, multiple = "all", by = "euv") 
      
      p16 <- p16 %>% 
        mutate(detalle_de_va_nculo_con_victima = case_match(detalle_de_va_nculo_con_victima, 
                                                            "CÃ³nyuge o pareja" ~ "Cónyuge o pareja",
                                                            "Ex pareja" ~ "Expareja",
                                                            "" ~ "Sin información",
                                                            "Otro" ~ "Otro",
                                                            "Familiar" ~ "Familiar (no especifica)",
                                                            "Seleccione" ~ "Sin información",
                                                            "En la comunidad" ~ "En la comunidad",
                                                            "Hijo(a)" ~ "Hijo(a)",
                                                            "Vecino(a)" ~ "Vecino(a)",
                                                            "Hermano(a)" ~ "Hermano(a)",
                                                            "Madre o padre" ~ "Madre o padre",
                                                            "TÃ­o(a)" ~ "Tío(a)",
                                                            "Laboral y docente" ~ "Laboral o docente",
                                                            "CompaÃ±ero(a)" ~ "Compañero(a)",
                                                            "Padrastro o madrastra" ~ "Padrastro o madrastra",
                                                            "Novio(a)" ~ "Novio(a)",
                                                            "Primo(a)" ~ "Primo(a)",
                                                            "Jefe(a) o patrÃ³n(a)" ~ "Jefe(a) o patrón(a)",
                                                            "Sobrino(a)" ~ "Sobrino(a)",
                                                            "Institucional" ~ "Institucional",
                                                            "Abuelo(a)" ~ "Abuelo(a)",
                                                            "Suegro(a)" ~ "Suegro(a)",
                                                            "Concubina" ~ "Concubino(a)",
                                                            "Nieto(a)" ~ "Nieto(a)",
                                                            "Profesor(a)" ~ "Profesor(a)",
                                                            "Servidor pÃºblico" ~ "Servidor Público")) %>% 
        mutate(rango_de_edades = case_when(edad <= 11 ~ "0 a 11 años",
                                           edad >= 12 & edad <= 18 ~ "12 a 18 años",
                                           edad >= 19 & edad <= 29 ~ "19 a 29 años",
                                           edad >= 30 & edad <= 39 ~ "30 a 39 años",
                                           edad >= 40 & edad <= 49 ~ "40 a 49 años",
                                           edad >= 50 & edad <= 59 ~ "50 a 59 años",
                                           edad >= 60 ~ "60 años o más")) %>% 
        mutate(descripcion_del_lugar = case_match(descripcion_del_lugar, 
                                                  "Casa habitaciÃ³n" ~ "Casa-habitación",
                                                  "Mercado" ~ "Mercado",
                                                  "" ~ "Sin información",
                                                  "Otro" ~ "Otro",
                                                  "Negocio" ~ "Negocio",
                                                  "Escuela o colegio" ~ "Escuela",
                                                  "JardÃ­n o parque" ~ "Parque",
                                                  "Empresa" ~ "Empresa",
                                                  "Centro comercial" ~ "Centro comercial",
                                                  "Centro comunitario" ~ "Centro comunitario",
                                                  "Explanada" ~ "Explanada",
                                                  "Centro cultural" ~ "Centro cultural",
                                                  "Particular" ~ "Particular",
                                                  "Centro deportivo" ~ "Centro deportivo",
                                                  "Estacionamiento" ~ "Estacionamiento",
                                                  "Aeropuerto" ~ "Aeropuerto",
                                                  "Templo religioso" ~ "Templo religioso",
                                                  "Instituciones gubernamentales" ~ "Instituciones gubernamentales",
                                                  "Central de autobuses" ~ "Central de autobuses",
                                                  "Centro recreativo" ~ "Centro recreativo",
                                                  "AutobÃºs" ~ "Autobús")) %>% 
        mutate(numero_de_hijos = case_when(numero_de_hijos <= 0  ~ "Sin hijos",
                                           numero_de_hijos >0 & numero_de_hijos < 5 ~
                                             paste0(as.character(numero_de_hijos), " hijos"),
                                           numero_de_hijos >= 5 ~ "5 hijos o más")) %>% 
        mutate(numero_de_hijos = gsub("1 hijos", "1 hijo", numero_de_hijos)) %>% 
        mutate(modalidad_de_la_violencia = case_match(modalidad_de_la_violencia, 
                                                      "1 - Familiar" ~ "Familiar",
                                                      "2 - Laboral" ~ "Laboral",
                                                      "3 - Institucional" ~ "Institucional",
                                                      "4 - En la comunidad" ~ "En la comunidad",
                                                      "5 - Escolar/Docente" ~ "Escolar",
                                                      "6 - ObstÃ©trica" ~ "Obstétrica",
                                                      "8 - Digital/CibernÃ©tica" ~ "Cibernética",
                                                      "9 - Feminicida" ~ "Feminicida")) %>% 
        mutate(estado_civil = case_match(estado_civil, 
                                         "Soltera" ~ "Soltera",
                                         "No identificada" ~ "Sin información",
                                         "Casada" ~ "Casada",
                                         "Desconocido" ~ "Sin información",
                                         "Divorciada" ~ "Divorciada",
                                         "UniÃ³n libre" ~ "Unión libre",
                                         "Concubinato" ~ "Concubinato",
                                         "Viuda" ~ "Viuda",
                                         "Separada" ~ "Separada")) %>% 
        right_join(., p16_v, multiple = "all", by = "euv") %>% 
        right_join(., p16_ac, multiple = "all", by = "euv") %>% 
        right_join(., p16_ag, multiple = "all", by = "euv") %>% 
        rename(tipo = modalidad, 
               modalidad = modalidad_de_la_violencia,
               lugar = descripcion_del_lugar, 
               vinculo = detalle_de_va_nculo_con_victima) %>% 
        filter(fecha_hechos >= input$fecha_hecho_inicio_5 &
                 fecha_hechos <= input$fecha_hecho_final_5) 
      
      
      if(input$municipio_seleccionado_3 != "Todos los municipios de Jalisco"){
        
        p16 <- p16 %>% 
          filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_3))
        
        municipio_seleccionado_interac <- input$municipio_seleccionado_3
        
      } else {
        
        municipio_seleccionado_interac <- "Todos los municipios de Jalisco"
        
      }
      
      p16_aux <- p16 %>% 
        select(euv, input$eje_x, input$eje_y) %>% 
        unique() %>% 
        group_by_(input$eje_x, input$eje_y) %>% 
        summarise(cuenta = n()) 
      
      
      names(p16_aux) <- c("x", "y", "cuenta")
      
      p16_aux <- p16_aux %>% 
        mutate(x = factor(x, levels = unique(p16_aux$x[order(p16_aux$cuenta, decreasing = T)]))) %>% 
        mutate(y = factor(y, levels = unique(p16_aux$y[order(p16_aux$cuenta, decreasing = T)])))
      
      p16_aux <- p16_aux %>% 
        ungroup() %>% 
        group_by(x) %>% 
        mutate(total = sum(cuenta)) %>% 
        mutate(porcentaje = scales::percent(cuenta/total, accuracy = 0.1)) %>% 
        mutate(label = paste0("<b>",
                              str_to_sentence(gsub("_", " ", input$eje_x)), 
                              ":\n", 
                              x, 
                              "</b>",
                              "\n",
                              porcentaje,
                              " de los casos son de ", 
                              str_to_lower(gsub("_", " ", input$eje_y)), 
                              " ",
                              str_to_lower(y)))
      
      
      fig_19 <- plot_ly(data = p16_aux, 
                        x = ~ x,
                        y = ~ cuenta, 
                        color = ~ y,
                        text = ~ label,
                        hoverinfo = "text") %>% 
        layout(barmode = "stack", 
               xaxis = list(
                 title = str_to_sentence(gsub("_", " ", input$eje_x))),
               yaxis = list(
                 title = paste0("Número de casos por ", 
                                gsub("_", " ", tolower(input$eje_y)))),
               title = paste0(municipio_seleccionado_interac, 
                              " (", 
                              format(input$fecha_hecho_inicio_5, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_5, "%d/%b/%y"),
                              ")"))
      
    }
    
    #### iii) Cuando hay una variable de tiempo----
    
    if(input$eje_x == "fecha_hechos"){
      
      p16_v <- base_casos_clean %>% 
        select(euv, 
               econ_a_mica, 
               fa_sica,
               patrimonial, 
               psicol_a_gica, 
               sexual, 
               otro) %>% 
        pivot_longer(cols = 2:7) %>% 
        filter(value != 0) %>% 
        mutate(modalidad = case_match(name, 
                                      "econ_a3mica" ~ "Económica",
                                      "fa_sica" ~ "Física",
                                      "patrimonial" ~ "Patrimonial",
                                      "psicol_a3gica" ~ "Psicológica",
                                      "sexual" ~ "Sexual",
                                      "otro" ~ "Otro tipo")) %>% 
        select(-value, -name)
      
      p16 <- base_casos_clean %>% 
        select(euv, 
               fecha_hechos, 
               rango_de_edades, 
               modalidad_de_la_violencia,
               estado_civil,
               descripcion_del_lugar, 
               detalle_de_va_nculo_con_victima, 
               va_nculo_con_victima, 
               municipio_hecho_clean)
      
      p16$detalle_de_va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "")] <- 
        p16$va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "")]
      
      p16$detalle_de_va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "Otro")] <- 
        p16$va_nculo_con_victima[which(p16$detalle_de_va_nculo_con_victima == "Otro")]
      
      p16_ag <- base_agre_clean %>% 
        select(euv, 
               edad, 
               escolaridad, 
               chacos, 
               macanas, 
               objeto_punzo_cortante,
               machete,
               proyectil,
               arma_fuego_corta,
               arma_fuego_larga,
               otra_fuego_larga) %>% 
        mutate(indicador_de_arma_blanca = case_when(chacos == 1 |
                                                      macanas == 1 |
                                                      objeto_punzo_cortante == 1|
                                                      machete == 1  ~ "Usó arma blanca")) %>%
        mutate(indicador_de_arma_fuego = case_when(proyectil == 1 |
                                                     arma_fuego_corta == 1 |
                                                     arma_fuego_larga == 1|
                                                     otra_fuego_larga == 1  ~ "Usó arma de fuego")) %>% 
        replace(is.na(.), "No usó arma") %>% 
        select(euv, 
               escolaridad, 
               indicador_de_arma_blanca, 
               indicador_de_arma_fuego) %>% 
        mutate(escolaridad = case_match(escolaridad, 
                                        "No identificado" ~ "Sin información",
                                        "Primaria" ~ "Primaria",
                                        "Ninguna" ~ "Ninguna",
                                        "Secundaria" ~ "Secundaria",
                                        "Preparatoria" ~ "Preparatoria",
                                        "Seleccione" ~ "Sin información",
                                        "Carrera tÃ©cnica comercial" ~ "Carrera Técnica",
                                        "Estudios que no requieren validÃ©z oficial" ~ "Sin información",
                                        "MaestrÃ­a" ~ "Posgrado",
                                        "Posgrado" ~ "Posgrado",
                                        "Doctorado" ~ "Doctorado",
                                        "Preescolar" ~ "Ninguna", 
                                        "Licenciatura" ~ "Licenciatura", 
                                        "" ~ "Sin información")) 
      
      p16 <- p16  %>% 
        mutate(mes_anio = as_date(paste0(format(fecha_hechos, "%Y-%m"), "-01")))  %>% 
        mutate(detalle_de_va_nculo_con_victima = case_match(detalle_de_va_nculo_con_victima, 
                                                            "CÃ³nyuge o pareja" ~ "Cónyuge o pareja",
                                                            "Ex pareja" ~ "Expareja",
                                                            "" ~ "Sin información",
                                                            "Otro" ~ "Otro",
                                                            "Familiar" ~ "Familiar (no especifica)",
                                                            "Seleccione" ~ "Sin información",
                                                            "En la comunidad" ~ "En la comunidad",
                                                            "Hijo(a)" ~ "Hijo(a)",
                                                            "Vecino(a)" ~ "Vecino(a)",
                                                            "Hermano(a)" ~ "Hermano(a)",
                                                            "Madre o padre" ~ "Madre o padre",
                                                            "TÃ­o(a)" ~ "Tío(a)",
                                                            "Laboral y docente" ~ "Laboral o docente",
                                                            "CompaÃ±ero(a)" ~ "Compañero(a)",
                                                            "Padrastro o madrastra" ~ "Padrastro o madrastra",
                                                            "Novio(a)" ~ "Novio(a)",
                                                            "Primo(a)" ~ "Primo(a)",
                                                            "Jefe(a) o patrÃ³n(a)" ~ "Jefe(a) o patrón(a)",
                                                            "Sobrino(a)" ~ "Sobrino(a)",
                                                            "Institucional" ~ "Institucional",
                                                            "Abuelo(a)" ~ "Abuelo(a)",
                                                            "Suegro(a)" ~ "Suegro(a)",
                                                            "Concubina" ~ "Concubino(a)",
                                                            "Nieto(a)" ~ "Nieto(a)",
                                                            "Profesor(a)" ~ "Profesor(a)",
                                                            "Servidor pÃºblico" ~ "Servidor Público")) %>% 
        mutate(rango_de_edades = case_match(rango_de_edades, 
                                            "0-11" ~ "0 a 11 años",
                                            "43435" ~ "12 a 18 años",
                                            "19-40" ~ "19 a 40 años",
                                            "41-60" ~ "41 a 60 años",
                                            "61-" ~ "61 años o más",
                                            "SinEdad" ~ "Sin información")) %>% 
        mutate(descripcion_del_lugar = case_match(descripcion_del_lugar, 
                                                  "Casa habitaciÃ³n" ~ "Casa-habitación",
                                                  "Mercado" ~ "Mercado",
                                                  "" ~ "Sin información",
                                                  "Otro" ~ "Otro",
                                                  "Negocio" ~ "Negocio",
                                                  "Escuela o colegio" ~ "Escuela",
                                                  "JardÃ­n o parque" ~ "Parque",
                                                  "Empresa" ~ "Empresa",
                                                  "Centro comercial" ~ "Centro comercial",
                                                  "Centro comunitario" ~ "Centro comunitario",
                                                  "Explanada" ~ "Explanada",
                                                  "Centro cultural" ~ "Centro cultural",
                                                  "Particular" ~ "Particular",
                                                  "Centro deportivo" ~ "Centro deportivo",
                                                  "Estacionamiento" ~ "Estacionamiento",
                                                  "Aeropuerto" ~ "Aeropuerto",
                                                  "Templo religioso" ~ "Templo religioso",
                                                  "Instituciones gubernamentales" ~ "Instituciones gubernamentales",
                                                  "Central de autobuses" ~ "Central de autobuses",
                                                  "Centro recreativo" ~ "Centro recreativo",
                                                  "AutobÃºs" ~ "Autobús")) %>% 
        mutate(modalidad_de_la_violencia = case_match(modalidad_de_la_violencia, 
                                                      "1 - Familiar" ~ "Familiar",
                                                      "2 - Laboral" ~ "Laboral",
                                                      "3 - Institucional" ~ "Institucional",
                                                      "4 - En la comunidad" ~ "En la comunidad",
                                                      "5 - Escolar/Docente" ~ "Escolar",
                                                      "6 - ObstÃ©trica" ~ "Obstétrica",
                                                      "8 - Digital/CibernÃ©tica" ~ "Cibernética",
                                                      "9 - Feminicida" ~ "Feminicida")) %>% 
        mutate(estado_civil = case_match(estado_civil, 
                                         "Soltera" ~ "Soltera",
                                         "No identificada" ~ "Sin información",
                                         "Casada" ~ "Casada",
                                         "Desconocido" ~ "Sin información",
                                         "Divorciada" ~ "Divorciada",
                                         "UniÃ³n libre" ~ "Unión libre",
                                         "Concubinato" ~ "Concubinato",
                                         "Viuda" ~ "Viuda",
                                         "Separada" ~ "Separada")) %>% 
        right_join(., p16_v, multiple = "all", by = "euv") %>% 
        right_join(., p16_ag, multiple = "all", by = "euv") %>% 
        rename(tipo = modalidad, 
               modalidad = modalidad_de_la_violencia,
               lugar = descripcion_del_lugar, 
               vinculo = detalle_de_va_nculo_con_victima) %>% 
        filter(fecha_hechos >= input$fecha_hecho_inicio_5 &
                 fecha_hechos <= input$fecha_hecho_final_5) 
      
      if(input$municipio_seleccionado_3 != "Todos los municipios de Jalisco"){
        
        p16 <- p16 %>% 
          filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_3))
        
        municipio_seleccionado_interac <- input$municipio_seleccionado_3
        
      } else {
        
        municipio_seleccionado_interac <- "Todos los municipios de Jalisco"
        
      }
      
      p16_aux <- p16 %>% 
        select(euv, mes_anio, input$eje_y) %>% 
        unique() 
      
      p16_aux <- p16_aux %>% 
        group_by_("mes_anio", input$eje_y) %>% 
        summarise(cuenta = n())  %>% 
        filter(!is.na(mes_anio)) 
      
      names(p16_aux) <- c("x", "y", "cuenta")
      
      p16_aux <- p16_aux %>% 
        mutate(label = paste0("<b>",
                              st_to_sentence(format(x,"%b %y")), 
                              ":\n", 
                              "</b>",
                              "\n",
                              prettyNum(cuenta, big.mark = ","),
                              " de los casos son de ", 
                              str_to_lower(gsub("_", " ", input$eje_y)), 
                              " ",
                              str_to_lower(y)))
      
      fig_19 <-  plot_ly(data = p16_aux, 
                         x = ~ x,
                         y = ~ cuenta, 
                         color = ~ y,
                         text = ~label, 
                         hoverinfo = "text", 
                         type = "bar") %>% 
        layout(barmode = "stack",
               yaxis = list(title = "Número de casos"), 
               xaxis = list(title = "Mes"), 
               title = paste0(municipio_seleccionado_interac, 
                              " (", 
                              format(input$fecha_hecho_inicio_5, "%d/%b/%y"), 
                              " al ", 
                              format(input$fecha_hecho_final_5, "%d/%b/%y"),
                              ")")) %>% 
        plotly::config(
          locale='es')
      
      fig_19
      
      
    }
    
    fig_19
    
  })
  
  
  ## 3.6.- Órdenes de protección (server)----
  data5 <- reactive({
    base_ordenes_clean %>% 
      filter(
        if(input$depen_check_5=="TODAS LAS DEPENDENCIAS") dep_exp!="" else str_to_upper(dep_exp) %in% input$depen_check_5,
        if(input$municipio_seleccionado_4=="TODOS LOS MUNICIPIOS DE JALISCO") municipio_hecho_clean!="" else str_to_upper(municipio_hecho_clean) %in% str_to_upper(input$municipio_seleccionado_4),
        fecha_de_recepcion >= input$fecha_de_recepcion_inicial,
        fecha_de_recepcion <= input$fecha_de_recepcion_final
      )
    
  })
  
  ### a) Caja órdenes de protección (total) ----
  
  output$caja_ordenes_total <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_5)){
    #   
    #   p17 <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    # } else {
    #   
    #   p17 <- base_ordenes_clean
    #   
    # }
    # 
    p17 <- data5() #%>% 
    #   filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #            fecha_de_recepcion <= input$fecha_de_recepcion_final) 
    # 
    # 
    # if(input$municipio_seleccionado_4 != "Todos los municipios de Jalisco"){
    #   
    #   p17 <- p17 %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    # }
    
    valueBox(prettyNum(nrow(p17), big.mark = ","), 
             "Órdenes de protección registradas", 
             icon = icon("scale-balanced"),
             width = 18, 
             color = "fuchsia")
    
    
  })
  
  
  ### b) Caja órdenes de protección (porcentaje) ----
  
  output$caja_ordenes_porcentaje <- renderValueBox({
    
    # if(!grepl("Todas las dependencias", input$depen_check_5)){
    #   
    #   p17_2 <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    #   p17_aux <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    # } else {
    
    p17_2 <- data5()
    
    p17_aux <- base_casos_clean %>% 
      filter(if(input$depen_check_5=="Todas las dependencias") dep_exp!="" else dep_exp %in% input$depen_check_5,
             if(input$municipio_seleccionado_4=="Todos los municipios de Jalisco") municipio_hecho_clean!="" else municipio_hecho_clean %in% input$municipio_seleccionado_4,
             fecha_de_recepcion >= input$fecha_de_recepcion_inicial,
             fecha_de_recepcion <= input$fecha_de_recepcion_final
      )
    
    # }
    
    
    
    # p17_2 <- p17_2 %>% 
    #   filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #            fecha_de_recepcion <= input$fecha_de_recepcion_final) 
    # 
    # p17_aux <- p17_aux %>% 
    #   filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #            fecha_de_recepcion <= input$fecha_de_recepcion_final) 
    
    # if(input$municipio_seleccionado_4 != "Todos los municipios de Jalisco"){
    #   
    #   p17_2 <- p17_2 %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    #   p17_aux <- p17_aux %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    # }
    # 
    
    valueBox(scales::percent(nrow(p17_2)/nrow(p17_aux), accuracy = 0.1), 
             "Porcentaje de órdenes de protección por casos registrados", 
             icon = icon("gavel"),
             width = 18, 
             color = "maroon")
    
    
  })
  
  
  ### c) Plot de evolución----
  
  output$evolucion_ordenes_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_5)){
    #   
    #   p18 <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    # } else {
    #   
    #   p18 <- base_ordenes_clean
    #   
    # }
    # 
    # p18 <- data5() #%>% 
    # filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #          fecha_de_recepcion <= input$fecha_de_recepcion_final)
    
    # if(input$municipio_seleccionado_4 != "Todos los municipios de Jalisco"){
    #   
    #   p18 <- p18 %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    municipio_ordenes <- str_to_title(input$municipio_seleccionado_4)
    #   
    # } else {
    #   
    #   municipio_ordenes <- "Todos los municipios de Jalisco"
    #   
    #   
    # }
    
    
    p18 <- data5() %>% 
      mutate(mes_anio = as_date(paste0(format(fecha_de_recepcion, "%Y-%m"),"-01"))) %>% 
      group_by(mes_anio) %>% 
      summarise(cuenta = n()) %>% 
      mutate(label = paste0("<b>",
                            str_to_title(format(mes_anio,"%b %y")), 
                            ":\n", 
                            "</b>",
                            "\n",
                            prettyNum(cuenta, big.mark = ",")))
    
    fig_20 <-  p18 %>% 
      ggplot(aes(x=mes_anio,
                 y=cuenta, text=label))+
      geom_col(fill = '#58569c', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=2, color="black", fontface = "bold", angle=90)+
      scale_y_continuous(labels = scales::comma) +
      # scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_ordenes, "\n (",format(input$fecha_hecho_inicio_3, "%d/%b/%y"), " al ", format(input$fecha_hecho_final_3, "%d/%b/%y"),")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_20, tooltip = "label")
    
    

    # plot_ly(data = p18, 
    #                    x = ~ mes_anio,
    #                    y = ~ cuenta, 
    #                    text = ~label, 
    #                    marker = list(color = "#58569c"), #cocca
    #                    hoverinfo = "text", 
    #                    type = "bar") %>% 
    #   layout(yaxis = list(title = "Número de casos"),
    #          xaxis = list(title = "Mes"),
    #          title = paste0(municipio_ordenes,
    #                         " (",
    #                         format(input$fecha_de_recepcion_inicial, "%d/%b/%y"),
    #                         " al ",
    #                         format(input$fecha_de_recepcion_final, "%d/%b/%y"),
    #                         ")")) %>%
    #   plotly::config(
    #     locale='es')
    # 
    # fig_20
    # 
    
  })
  
  
  
  
  ### d) Tipo orden plot----
  
  output$tipo_orden_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_5)){
    #   
    #   p19 <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    # } else {
    #   
    #   p19 <- base_ordenes_clean
    #   
    # }
    # 
    # 
    # 
    # p19 <- p19 %>% 
    #   filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #            fecha_de_recepcion <= input$fecha_de_recepcion_final)
    # 
    # if(input$municipio_seleccionado_4 != "Todos los municipios de Jalisco"){
    #   
    #   p19 <- p19 %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    municipio_ordenes <- str_to_title(input$municipio_seleccionado_4)
    #   
    # } else {
    #   
    #   municipio_ordenes <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    
    p19 <- data5() %>% 
      group_by(desc_tipo_orden) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      mutate(label = scales::percent(cuenta/total))
    
    
    fig_21 <-  plot_ly(data = p19, 
                       values = ~ porcentaje, 
                       labels = ~ desc_tipo_orden,
                       parents = c(""),
                       type = "treemap",
                       hoverinfo = ~ "label")  %>% 
      layout(title = paste0(municipio_ordenes,
                            " (",
                            format(input$fecha_de_recepcion_inicial, "%d/%b/%y"),
                            " al ",
                            format(input$fecha_de_recepcion_final, "%d/%b/%y"),
                            ")"))
    
    
    fig_21
    
    
  })
  
  ### e) Entidad emisora----
  
  output$entidad_emisora_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_5)){
    #   
    #   p20 <- base_ordenes_clean %>% 
    #     filter(dep_exp %in% input$depen_check_5)
    #   
    # } else {
    #   
    #   p20 <- base_ordenes_clean
    #   
    # }
    # 
    # p20 <- p20 %>% 
    #   filter(fecha_de_recepcion >= input$fecha_de_recepcion_inicial &
    #            fecha_de_recepcion <= input$fecha_de_recepcion_final)
    # 
    # if(input$municipio_seleccionado_4 != "Todos los municipios de Jalisco"){
    #   
    #   p20 <- p20 %>%
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_4))
    #   
    municipio_ordenes <- str_to_title(input$municipio_seleccionado_4)
    #   
    # } else {
    #   
    #   municipio_ordenes <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    
    p20 <- data5() %>% 
      mutate(auto_emisora_clean = case_when(
        grepl("MINISTERIO|^MP|M\\.P|PUBLICO|M\\. P|P$|M PCO|INISTERI|MO$|AGENCIA|MINES|M \\.P\\.|M,P,|P\\.$|PM$|M-P-", 
              autoridad_emisora) ~ "Ministerio Público",
        grepl("CENTRO D|CENTRO INTE|CENTRO PARA", autoridad_emisora) ~ "Centro de Justicia para la Mujer",
        grepl("FISCAL|FGE|FE ", autoridad_emisora) ~ "Fiscalía",
        grepl("INSTITUTO DE LA MUJES", autoridad_emisora) ~ "Instituto de las Mujeres",
        grepl("JUZGADO|JUZ |JUEZ", autoridad_emisora) ~ "Juzgado"))
    
    
    p20 <- p20 %>% 
      group_by(auto_emisora_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      mutate(label = scales::percent(cuenta/total))
    
    
    fig_22 <-  plot_ly(data = p20, 
                       values = ~ porcentaje, 
                       labels = ~ auto_emisora_clean,
                       parents = c(""),
                       type = "treemap",
                       hoverinfo = ~ "label")  %>% 
      layout(title = paste0(municipio_ordenes,
                            " (",
                            format(input$fecha_de_recepcion_inicial, "%d/%b/%y"),
                            " al ",
                            format(input$fecha_de_recepcion_final, "%d/%b/%y"),
                            ")"))
    
    
    fig_22
    
    
    
  })
  
  ## 3.7.- Servicios otorgados ----
  data6 <- reactive({
    base_servicios_clean %>% 
      filter(
        if(input$depen_check_6=="TODAS LAS DEPENDENCIAS") dependenciaquebrindoservicio!="" else str_to_upper(dependenciaquebrindoservicio) %in% str_to_upper(input$depen_check_6),
        if(input$municipio_seleccionado_5=="TODOS LOS MUNICIPIOS DE JALISCO") municipio_hecho_clean!="" else str_to_upper(municipio_hecho_clean) %in% str_to_upper(input$municipio_seleccionado_5),
        fecha_captura >= input$fecha_de_captura_inicial, 
        fecha_captura <= input$fecha_de_captura_final
      )
    
  })
  ### a) Mapa----
  
  output$mapa_2 <- renderLeaflet({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   mapa_2p <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   mapa_2p <- base_servicios_clean
    #   
    # }
    # 
    mapa_2p <- data6() %>% 
      # filter(fecha_captura >= input$fecha_de_captura_inicial &
      #          fecha_captura <= input$fecha_de_captura_final) %>%
      group_by(municipio_hecho_clean) %>% 
      filter(municipio_hecho_clean !=  "") %>% 
      summarise(cuenta = n()) %>% 
      stringdist_join(., poblacion, 
                      by= c("municipio_hecho_clean" = "municipio"),
                      mode='left',
                      method = "jw", 
                      max_dist=.08, 
                      distance_col='dist') %>% 
      mutate(municipio_hecho_clean = str_to_title(municipio_hecho_clean)) %>% 
      filter(!is.na(municipio)) %>% 
      select(-dist) %>% 
      # replace(is.na(.), 0) %>% 
      right_join(., jalisco_shape, by = c("municipio")) %>% 
      ungroup() %>%  drop_na(cuenta) 
    
    # mapa_2p$cuenta[which(is.na(mapa_2p$cuenta))] <- 0
    
    pal2 <- colorNumeric(c("#0f0a1f", 
                           "#3c3065",
                           "#9784d7",
                           "#cbc1eb"),
                         mapa_2p$cuenta, reverse = T)
    
    # mapa_2p <- mapa_2p  %>% 
    #   mutate(quantile = ntile(cuenta, 5)) 
    # 
    # quantiles_2 <- data.frame(quantile = 1:5,
    #                           upr = round(quantile(mapa_2p$cuenta, 
    #                                                probs = seq(.2, 1, by = .2))),
    #                           lwr = c(-1, 
    #                                   round(quantile(mapa_2p$cuenta, 
    #                                                  probs = seq(.2, 1, by = .2))[1:4]))) %>% 
    #   mutate(lwr = lwr + 1) %>% 
    #   mutate(lwr = prettyNum(lwr, big.mark = ",")) %>% 
    #   mutate(upr = prettyNum(upr, big.mark = ",")) %>% 
    #   mutate(rango = paste0(lwr, "-", upr)) %>% 
    #   select(quantile, rango) %>% 
    #   arrange(-quantile)
    # 
    # mapa_2p <- mapa_2p %>% 
    #   left_join(., quantiles_2) %>% 
    #   mutate(rango = factor(rango, 
    #                         levels = quantiles_2$rango)) %>% 
    #   st_as_sf()
    # 
    # pal2 <- colorFactor(palette=c("#0f0a1f", 
    #                               "#3c3065", 
    #                               "#9784d7",
    #                               "#cbc1eb",
    #                               "#ffffff"),
    #                     levels=sort(unique(mapa_2p$rango)))
    # 
    labels_map_2 <- sprintf(
      "<strong>%s</strong><br/>%s",
      mapa_2p$municipio_hecho_clean, 
      paste0("Número de servicios: ", prettyNum(mapa_2p$cuenta, big.mark = ","))) %>%
      lapply(htmltools::HTML)
    
    mapa_2p %>% st_as_sf() %>% leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(#data =  mapa_2p,
        color = "black",
        weight=1,
        fillOpacity = 0.8,
        # fillColor  = ~pal2(rango),
        fillColor  = ~pal2(cuenta),
        label=~labels_map_2)  %>% 
      leaflet::addLegend(pal = pal2,
                         values = sort(unique(mapa_2p$cuenta)),
                         opacity = 0.75,
                         title = "Número de servicios",
                         position = "bottomright")
    
  })
  
  ### b) Plot de evolución y vb total de servicios y mujeres atendidas ----
  
  output$plot_evolucion_servicios <- renderPlotly({
  
      
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p28 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p28 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p28 <- p28 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_serv_ev <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_serv_ev <- "Todos los municipios de Jalisco"
    # }
    
    p28 <- data6() %>% 
      # filter(fecha_captura >= input$fecha_de_captura_inicial &
      #          fecha_captura <= input$fecha_de_captura_final) %>%
      select(euv, 
             fecha_captura) %>% 
      mutate(ind_mujeres = gsub("-.*", "", euv))
    
    # Value box - número de servicios
    output$vb_total_servicios <- renderValueBox({
      
      valueBox(prettyNum(nrow(p28), big.mark = ","), 
               paste0("Total del número de servicios otorgados en el periodo señalado."), 
               icon = NULL,
               width = 18, 
               color = "yellow") #jati 1
      
      
    })
    
    # Value box - número de mujeres
    output$vb_total_mujeres_servicios <- renderValueBox({
      
      
      valueBox(prettyNum(length(unique(p28$ind_mujeres)), big.mark = ","), 
               paste0("Total de mujeres atendidas en el periodo señalado."), 
               icon = NULL,
               width = 18, 
               color = "fuchsia") #jati 2
      
      
      
    })
    
    # Plot - evolución de servicios
    
    p28_aux <- p28 %>% 
      group_by(fecha_captura) %>% 
      summarise(cuenta = n())
    
    p28_aux <- p28_aux %>% 
      right_join(., data.frame(fecha_captura = as.Date(
        as.numeric(min(p28_aux$fecha_captura, na.rm = T)):
          as.numeric(max(p28_aux$fecha_captura, na.rm = T))))) %>% 
      arrange(fecha_captura) %>% 
      replace(is.na(.), 0) %>% 
      mutate(avg_15 = rollmean(cuenta, k = 15, fill = NA)) %>% 
      mutate(avg_30 = rollmean(cuenta, k = 30, fill = NA)) %>% 
      mutate(label_plot_15 = paste0(format(fecha_captura, "%d/%B/%y"),"\nPromedio últimos 15 días: ", round(avg_15, 2)),
             label_plot_30 = paste0(format(fecha_captura, "%d/%B/%y"),"\nPromedio últimos 30 días: ", round(avg_30, 2)))
    
    # ------------------------------------------
    
    
    l <- list(
      font = list(
        family = "Nutmeg-Light",
        size = 10*textFunction(),
        color = "#000"),
      borderwidth = 0,
      x = 0.1, 
      y = 0.9)
    
    tt <- list(
      family = "Nutmeg-Light",
      size = 10*textFunction())
    
    
    fig_28 <- plot_ly(p28_aux, #cocca
                      x = ~ fecha_captura,
                      y = ~ avg_30,
                      font=tt,
                      type = 'scatter',
                      mode = 'lines',
                      name = "Promedio móvil (30 días)", 
                      line = list(color = "#514885", 
                                  width = 4),
                      text = ~ label_plot_30,
                      hoverinfo = "text", 
                      hoverlabel = list(bgcolor='#514885')) %>% 
      add_trace(y = ~avg_15, 
                name = 'Promedio móvil (15 días)', 
                line = list(color = '#d34736',
                            width = 2, dash = 'dash'),
                opacity = 0.7,
                text = ~ label_plot_15,
                hoverinfo = "text", 
                hoverlabel = list(bgcolor='#d34736'))  %>% 
      layout(title= list(text = paste0(municipio_seleccionado_serv_ev,
                                      " ",
                                      "(", 
                                      format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
                                      " al ", 
                                      format(input$fecha_de_captura_final, "%d/%b/%y"),
                                      ")"), font=tt),
             xaxis = list(title = '',
                          zerolinecolor = '#ffff',zerolinewidth = 2,
                           gridcolor = '#bcbcbc',
                          tickangle=0, tickfont = list(family='Nutmeg-Light')),
    
            
             # xaxis = list(title = list(text ="Fecha de captura",
             #                           zerolinecolor = '#ffff',
             #                           zerolinewidth = 2,
             #                           gridcolor = '#bcbcbc',
             #                           font=tt)),
             yaxis = list(title = list(text ="Promedio móvil diario",
                                       zerolinecolor = '#ffff',
                                       zerolinewidth = 2,
                                       gridcolor = '#bcbcbc', font=tt),
             tickangle=0, tickfont = list(family='Nutmeg-Light')),
             legend = l) %>%  #list(x = 0.1, y = 0.9)) %>% 
      plotly::config(
        locale='es')
    
    
    fig_28
    
    
  })
  
  ### c) Tipo de servicio más común atendidas ----
  
  output$tipo_servicio_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p29 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p29 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p29 <- p29 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_tip <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_tip <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p29 <- data6() %>% 
      # filter(fecha_captura >= input$fecha_de_captura_inicial &
      #          fecha_captura <= input$fecha_de_captura_final) %>%
      select(serviciodetalle) 
    
    p29 <- p29 %>% 
      mutate(name_clean = gsub("Ã¡", "á", serviciodetalle)) %>% 
      mutate(name_clean = gsub("Ã³", "ó", name_clean)) %>% 
      mutate(name_clean = gsub("Ã©", "é", name_clean)) %>% 
      mutate(name_clean = gsub("Ã", "í", name_clean)) %>% 
      mutate(name_clean = gsub("Ãº", "ú", name_clean)) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      slice_head(n = 5) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por tipo de servicio: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p29 <- p29 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p29$name_clean))
    
    
    # Value Box - edad agresor
    
    output$vb_tipo_servicio <- renderValueBox({
      
      valueBox(p29$name_clean[1], 
               paste0("Tipo de servicio otorgado más común con ", 
                      scales::percent(p29$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "purple")
      
    })
    
    # Plot - rango edad agresor
 
    
    
    fig_29 <- p29 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#58569c', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=2.5, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_tip, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_29, tooltip = "label")
    
      
    #   plot_ly(data = p29,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#58569c',
    #                                 line = list(color = '#58569c',
    #                                             width = 1.5)),
    #                   text = ~ label,
    #                   textfont=list(color='#58569c', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = list(text=paste0("Top 5 servicios en\n", 
    #                         municipio_seleccionado_ser_tip, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"), font=t),
    #          xaxis = list(title = list(text ="Tipo de servicio",
    #            tickangle = 0), font=t),
    #          yaxis = list(title = list(text ="Número de servicios"), font=t),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_29
    # 
  })
  
  
  ### d) Rango de edad de personas atendidas ----
  
  output$servicios_edad_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p30 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p30 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p30 <- p30 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_edad <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_edad <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p30 <- data6() %>% 
      # filter(fecha_captura >= input$fecha_de_captura_inicial &
      #          fecha_captura <= input$fecha_de_captura_final) %>%
      select(edad_vict) 
    
    p30 <- p30 %>% 
      mutate(name_clean = case_when(edad_vict <= 11 ~ "0 a 11 años",
                                    edad_vict >= 12 & edad_vict <= 18 ~ "12 a 18 años",
                                    edad_vict >= 19 & edad_vict <= 29 ~ "19 a 29 años",
                                    edad_vict >= 30 & edad_vict <= 39 ~ "30 a 39 años",
                                    edad_vict >= 40 & edad_vict <= 49 ~ "40 a 49 años",
                                    edad_vict >= 50 & edad_vict <= 59 ~ "50 a 59 años",
                                    edad_vict >= 60 ~ "60 años o más")) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por edad: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p30 <- p30 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p30$name_clean))
    
    
    # Value Box - edad persona atendida
    
    output$vb_edad_servicio <- renderValueBox({
      
      valueBox(p30$name_clean[1], 
               paste0("Rango de edad más común de las mujeres atendidas con ", 
                      scales::percent(p30$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "olive")
      
    })
    
    # Plot - rango edad persona atendida
    
    fig_30 <- p30 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#5d9970', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=2.5, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_edad, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_30, tooltip = "label")
      
      
    #   plot_ly(data = p30,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#5d9970',
    #                                 line = list(color = '#5d9970',
    #                                             width = 1.5)),
    #                   text = ~ label,
    #                   textfont=list(color='#5d9970', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_ser_edad, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Rango de edad",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de servicios"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_30
    
  })
  
  ### e) Estado civil más común atendidas ----
  
  output$servicios_edocivil_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p31 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p31 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p31 <- p31 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_edociv <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_edociv <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p31 <- data6() %>% 
      filter(fecha_captura >= input$fecha_de_captura_inicial &
               fecha_captura <= input$fecha_de_captura_final) %>%
      select(estado_civil_vict) 
    
    p31 <- p31 %>% 
      mutate(name_clean = gsub("Ã³", "ó", estado_civil_vict)) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por estado civil: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p31 <- p31 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p31$name_clean))
    
    
    # Value Box - estado civil persona atendida
    
    output$vb_edociv_servicio <- renderValueBox({
      
      valueBox(p31$name_clean[1], 
               paste0("Estado civil más común de las mujeres atendidas con ", 
                      scales::percent(p31$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "aqua") #jati 5
      
    })
    
    # Plot - estado civil edad persona atendida
    
    fig_31 <- p31 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#8a4960', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=2.5, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_edociv, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_31, tooltip = "label")
    
      
    #   plot_ly(data = p31,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#8a4960',
    #                                 line = list(color = '#8a4960',
    #                                             width = 1.5)),
    #                   textfont=list(color='#8a4960', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_ser_edociv, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Estado civil",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de servicios"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_31
    
  })
  
  ### f) Dependencia más común atendidas ----
  
  output$servicios_dependencia_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p32 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p32 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p32 <- p32 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_depen <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_depen <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p32 <- data6() %>% 
      filter(fecha_captura >= input$fecha_de_captura_inicial &
               fecha_captura <= input$fecha_de_captura_final) %>%
      select(dependenciaquebrindoservicio) 
    
    p32 <- p32 %>% 
      mutate(name_clean = case_when(grepl("PAIMEF", dependenciaquebrindoservicio) ~ "PAIMEF-Unidad Regional de Atención Integral",
                                    grepl("CENTROS DE JUS|CENTRO DE JUST", dependenciaquebrindoservicio) ~ "Centro de Justicia",
                                    grepl("DIVISION ESPE", dependenciaquebrindoservicio) ~ "División Especializada en Atención a la Violencia Contra las Mujeres en Razón de Género",
                                    grepl("FISCALIA REGIONAL", dependenciaquebrindoservicio) ~ "Fiscalía Regional",
                                    grepl("COMISARIA DE SE", dependenciaquebrindoservicio) ~ "Comisaría de Seguridad Pública"
      )) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      replace(is.na(.), "Otra") %>% 
      mutate(name_clean = str_wrap(name_clean, width = 25)) %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos por dependencia: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p32 <- p32 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p32$name_clean))
    
    
    # Value Box - estado civil persona atendida
    
    output$vb_dependencia_servicio <- renderValueBox({
      
      valueBox(p32$name_clean[nrow(p32)],
               paste0("Dependencia más común en brindar algún servicio con ", 
                      scales::percent(p32$porcentaje[nrow(p32)], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "red") #jati 6
      
    })
    
    # Plot - dependencia más común
    
    fig_32 <- p32 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#46648f', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_depen, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_32, tooltip = "label")
    
      
    #   
    #   plot_ly(data = p32,
    #                   x = ~ reorder(str_wrap(name_clean, width = 8), cuenta),
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#46648f',
    #                                 line = list(color = '#46648f',
    #                                             width = 0)),
    #                   text = ~ label,
    #                   textfont=list(color='#46648f', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>%
    #   layout(title = paste0(municipio_seleccionado_ser_depen, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Número de servicios",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Dependencia"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_32
    
  })
  
  ### g) Usuario con mas servicios ----
  
  output$servicios_usuarios_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p33 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p33 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p33 <- p33 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_usu <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_usu <- "Todos los municipios de Jalisco"
    #   
    # }
    # 
    p33 <- data6() %>% 
      filter(fecha_captura >= input$fecha_de_captura_inicial &
               fecha_captura <= input$fecha_de_captura_final) %>%
      select(ususervicio) 
    
    p33 <- p33 %>% 
      mutate(name_clean = ususervicio) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      slice_head(n = 10) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos atendidos por usuario: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p33 <- p33 %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p33$name_clean))
    
    
    # Value Box - estado civil persona atendida
    
    # output$vb_usuario_servicio <- renderValueBox({
    #   
    #   valueBox(p33$name_clean[1],
    #            paste0("Usuario más común en brindar algún servicio con ", 
    #                   scales::percent(p33$porcentaje[1], accuracy = 1),
    #                   " de los casos."), 
    #            icon = NULL,
    #            width = 18, 
    #            color = "maroon")
    #   
    # })
    
    # Plot - dependencia más común
    
    fig_33 <- p33 %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#d81b60', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_usu, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_33, tooltip = "label")
    
    # plot_ly(data = p33,
    #                   x = ~ name_clean, 
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#d81b60',
    #                                 line = list(color = '#d81b60',
    #                                             width = 1.5)),
    #                   textfont=list(color='#d81b60', size=0),
    #                   text = ~ label,
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_ser_usu, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Usuario",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número de servicio"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    
    # fig_33
    
  })
  
  ### h) Categoría de servicios ----
  
  output$servicios_categoria_plot <- renderPlotly({
    
    # if(!grepl("Todas las dependencias", input$depen_check_6)){
    #   
    #   p34 <- base_servicios_clean %>% 
    #     filter(dependenciaquebrindoservicio %in% input$depen_check_6)
    #   
    # } else {
    #   
    #   p34 <- base_servicios_clean
    #   
    # }
    # 
    # if(input$municipio_seleccionado_5 != "Todos los municipios de Jalisco"){
    #   
    #   p34 <- p34 %>% 
    #     filter(municipio_hecho_clean == toupper(input$municipio_seleccionado_5))
    #   
    municipio_seleccionado_ser_cate <- input$municipio_seleccionado_5
    #   
    # } else {
    #   
    #   municipio_seleccionado_ser_cate <- "Todos los municipios de Jalisco"
    #   
    # }
    
    p34 <- data6() %>% 
      filter(fecha_captura >= input$fecha_de_captura_inicial &
               fecha_captura <= input$fecha_de_captura_final) %>%
      select(tiposervicio, estatus) 
    
    p34_aux <- p34 %>% 
      filter(!grepl("Seleccione", tiposervicio)) %>% 
      mutate(name_clean = tiposervicio) %>% 
      group_by(name_clean) %>% 
      summarise(cuenta = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(cuenta)) %>% 
      mutate(porcentaje = cuenta/total) %>% 
      arrange(-cuenta) %>% 
      mutate(label = paste0("<b>",
                            name_clean,
                            "</b>",
                            "\n",
                            "Número de casos atendidos por categoría de servicio: ", 
                            "<b>",
                            prettyNum(cuenta, big.mark = ","),
                            "</b>",
                            "\n", 
                            "Porcentaje que representa: ", 
                            "<b>",
                            scales::percent(porcentaje, accuracy = 0.1), 
                            "</b>"))
    
    p34_aux <- p34_aux %>% 
      mutate(name_clean = factor(name_clean, 
                                 levels = p34_aux$name_clean))
    
    
    # Value Box - categoría de servicio
    
    output$vb_categoria_servicio <- renderValueBox({
      
      valueBox(p34_aux$name_clean[1],
               paste0("Categoría de servicio brindado más común con ", 
                      scales::percent(p34_aux$porcentaje[1], accuracy = 1),
                      " de los casos."), 
               icon = NULL,
               width = 18, 
               color = "purple") #Jati 7
      
    })
    
    # Value Box - estado civil persona atendida
    
    output$vb_estatus_servicio <- renderValueBox({
      
      valueBox(scales::percent(length(which(p34$estatus=="Concluido"))/nrow(p34), accuracy = 1),
               "Porcentaje de los casos que fue registrado como concluido.", 
               icon = NULL,
               width = 18, 
               color = "yellow") # jati 8
      
    })
    
    # Plot - dependencia más común
    
    fig_34 <- p34_aux %>% 
      ggplot(aes(x=reorder(name_clean, cuenta),
                 y=cuenta, text=label))+
      geom_col(fill = '#bf8037', alpha=0.8)+
      # geom_label(aes(x = name_clean, y = cuenta, label=cuenta, size=13),
      #            size=3, alpha=1, colour="#0f3776",   label.size = 0.25,  fontface = "bold")+
      geom_text(aes(label=scales::comma(cuenta)), size=4, color="black", fontface = "bold")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
      labs(x="", y="", fill="", color="", 
           title = paste0(municipio_seleccionado_ser_cate, "\n (", 
                          format(input$fecha_hecho_inicio_3, "%d/%b/%y"), 
                          " al ", 
                          format(input$fecha_hecho_final_3, "%d/%b/%y"),
                          ")"))+
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(family="Nutmeg-Light", size = 8*textFunction()),
            strip.text.x = element_text(size = 7*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 7L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 7L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 7L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))
    
    ggplotly(fig_34, tooltip = "label")
      
      
      
    #   plot_ly(data = p34_aux,
    #                   x = ~ name_clean, 
    #                   y = ~ cuenta,
    #                   type = "bar",
    #                   marker = list(color = '#bf8037',
    #                                 line = list(color = '#bf8037',
    #                                             width = 1.5)),
    #                   text = ~ label,
    #                   textfont=list(color='#bf8037', size=0),
    #                   hoverinfo = "text") %>% 
    #   add_text(text = ~ prettyNum(cuenta, big.mark = ","),
    #            textposition = "top",
    #            hoverinfo="none") %>% 
    #   layout(title = paste0(municipio_seleccionado_ser_cate, " (", 
    #                         format(input$fecha_de_captura_inicial, "%d/%b/%y"), 
    #                         " al ", 
    #                         format(input$fecha_de_captura_final, "%d/%b/%y"),
    #                         ")"),
    #          xaxis = list(
    #            title = "Categoría de servicio",
    #            tickangle = 0),
    #          yaxis = list(
    #            title = "Número"),
    #          showlegend = FALSE,
    #          layout.separators=",.",
    #          hoverlabel=list(bgcolor="white")) 
    # 
    # 
    # fig_34
    
  })
  
  output$output1 <- renderText({
    if(nrow(data1())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  
  output$output2 <- renderText({
    if(nrow(data2())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  
  output$output3 <- renderText({
    if(nrow(data3())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  
  output$output4 <- renderText({
    if(nrow(data4())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  
  output$output5 <- renderText({
    if(nrow(data5())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  
  output$output6 <- renderText({
    if(nrow(data6())>0) {
      ""
    } else {
      "Los filtros seleccionados no arrojan ningún valor"
    }
    
  })
  


  
  
  
  
  
  
}

# Ejecturas aplicación


shinyApp(ui = ui, server = server)