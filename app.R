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
library(openxlsx)
library(shinyWidgets)





font_paths()
font_files()

#font_add("Nutmeg-Light", "Nutmeg-Light.ttf")
font_add("Nutmeg-Regular", "Nutmeg-Regular.ttf")

font_families()



nb.cols <-13
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


nb.cols.129 <-9
mycolors129 <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols.129)


nb.cols_2 <- 20
bupu <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols_2)


nb.cols_2 <- 20
bupu_2 <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols_2)




# drive_find(n_max = 6)
# 1
# 
# drive_download("codigo_violeta_bases.xlsx",overwrite = TRUE)
# drive_download("2.reportes_llamadas_2021_2022.xlsx",overwrite = TRUE)
# drive_download("1.reportes_llamadas_2019_2020.xlsx",overwrite = TRUE)
# drive_download("medidas_ordenes",overwrite = TRUE)
# drive_download("atenciones.xlsx",overwrite = TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #
#funciones

mes_nombre <- function(fecha=fecha){
  fecha <- as_date(fecha)
  tabla_mes <- tibble(num_mes=seq(1,12,1), 
                      mes=c("Enero", "Febrero", "Marzo", "Abril", 
                            "Mayo", "Junio", "Julio", "Agosto", 
                            "Septiembre", "Octubre", "Noviembre", 
                            "Diciembre"))
  d <- tibble(fecha=fecha) %>%
    mutate(num_mes=month(fecha)) %>% 
    left_join(tabla_mes)
  return(d$mes)
}
# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")


# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv") 




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


#reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv") %>% 
  mutate(
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")




violencia_familiar_diario<- read.csv("violencia_familiar_diario.csv") %>% 
  mutate(
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre"))) %>% 
  select(año, mes, zona, sexo, registro, month)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Órdenes y medidas

library(readxl)

medidas_ordenes_estatal <- read_excel("medidas_ordenes.xlsx", sheet = "estatal")
medidas_ordenes_municipal <- read_excel("medidas_ordenes.xlsx", sheet = "municipal") %>% 
  mutate(
    # mes=case_when(
    #   mes=="enero" ~ "Enero",
    #   mes=="febrero" ~ "Febrero",
    #   mes=="marzo" ~ "Marzo",
    #   mes=="abril" ~ "Abril",
    #   mes=="mayo" ~ "Mayo",
    #   mes=="junio" ~ "Junio",
    #   mes=="julio" ~ "Julio",
    #   mes=="agosto" ~ "Agosto",
    #   mes=="septiembre" ~ "septiembre",
    #   mes=="octubre" ~ "Octubre",
    #   mes=="noviembre" ~ "Noviembre",
    #   mes=="diciembre" ~ "Diciembre"),
    mes=factor(mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
#metemos la base de aborto
aborto <- readxl::read_excel("AbortoLimpio.xlsx") %>% 
  clean_names()
aborto <- aborto %>% 
  mutate(metodo_planificacion_familiar=case_when(
    metodo_planificacion_familiar=="Pend" ~ "PEND", 
    metodo_planificacion_familiar %in% c("DIU", "OTB", 
                                         "PEND") ~ metodo_planificacion_familiar,
    T ~ str_to_title(metodo_planificacion_familiar)
  ), 
  hospital=case_when(
    hospital=="HGO" ~ "Hospital General de Occidente", 
    hospital=="HMIELM" ~ "Hospital Materno Infantil ELM", 
    hospital=="HMIESnMF"~"Hospital Materno Infantil San Martín de las Flores", 
    T ~ hospital
  ), 
  rango_edad=case_when(
    edad<15 ~ "Menor a 15 años", 
    edad>=15 & edad<18~ "15 a 17", 
    edad>=18 & edad<30 ~ "+18",
    # edad>=30 & edad<45 ~ "30 a 44", 
    edad>=45 ~ "+45", T ~ "Desconocido"), 
    rango_edad=factor(rango_edad, 
                      levels=c("Menor a 15 años", 
                               "15 a 17", "+18", 
                               # "30 a 44", "+45", 
                               "Desconocido"))
    
    
  ) %>% 
  replace_na(list(procedimiento="Sin información", 
                  metodo_planificacion_familiar="Sin información", 
                  hospital="Sin información", 
                  rango_sgd="Sin información")) %>% 
  mutate(fecha=as_date(fecha))


aborto_historico <- tibble(ao=c(
  2016:2021
), Total=c(4,12,9,10, 13,10))
aborto_historico2 <- aborto %>% 
  group_by(ao=year(fecha)) %>% 
  summarise(Total=n())

total_aborto <- bind_rows(aborto_historico, aborto_historico2)

aborto_causal <- aborto %>% tabyl(causal) %>% arrange(desc(n))

coord_hospital <- tibble(
  hospital=c("Cd Guzmán", "Colotlán",
            "Hospital General de Occidente", 
            "Hospital Materno Infantil ELM", 
            "Hospital Materno Infantil San Martín de las Flores", 
            "Pto Vallarta", 
            "Tepatitlán"), 
  x=c(-103.47863, -103.27124, -103.36893, -103.35142, 
      -103.27774, -105.21027, -102.77926), 
  y=c(19.69772, 22.09913, 20.71708, 20.66281, 
      20.59739, 20.66897, 20.81140)
)

# mapa
#df_mxmunicipio_2020<-data("df_mxmunicipio_2020") 

data("df_mxmunicipio_2020")


medidas_ordenes_municipal %>% 
  #filter(año==2019) %>%
  group_by(año, municipio) %>% 
  summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
            ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>% 
  pivot_longer(cols=c("ordenes","medidas"),
               names_to = "tipo",
               values_to = "total") ->medidas_y_ordenes


medidas_y_ordenes %>%  filter(tipo=="medidas", año==2019)->medidas_2019
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2020)->medidas_2020
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2021)->medidas_2021
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2022)->medidas_2022
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2023)->medidas_2023


# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

medidas_2019 <-merge(df_mxmunicipio_2020, medidas_2019, by.x="municipio_name", by.y="municipio")
medidas_2020 <-merge(df_mxmunicipio_2020, medidas_2020, by.x="municipio_name", by.y="municipio")
medidas_2021 <-merge(df_mxmunicipio_2020, medidas_2021, by.x="municipio_name", by.y="municipio")
medidas_2022 <-merge(df_mxmunicipio_2020, medidas_2022, by.x="municipio_name", by.y="municipio")
medidas_2023 <-merge(df_mxmunicipio_2020, medidas_2023, by.x="municipio_name", by.y="municipio")

medidas_2019$value<- medidas_2019$total
medidas_2020$value<- medidas_2020$total
medidas_2021$value<- medidas_2021$total
medidas_2022$value<- medidas_2022$total
medidas_2023$value<- medidas_2023$total


medidas_2019<- medidas_2019 %>% filter(state_name=="Jalisco")
medidas_2020<- medidas_2020 %>% filter(state_name=="Jalisco")
medidas_2021<- medidas_2021 %>% filter(state_name=="Jalisco")
medidas_2022<- medidas_2022 %>% filter(state_name=="Jalisco")
medidas_2023<- medidas_2023 %>% filter(state_name=="Jalisco")

#_______________________________________________________________________________#

medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2019)->ordenes_2019
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2020)->ordenes_2020
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2021)->ordenes_2021
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2022)->ordenes_2022
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2023)->ordenes_2023


# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

ordenes_2019 <-merge(df_mxmunicipio_2020, ordenes_2019, by.x="municipio_name", by.y="municipio")
ordenes_2020 <-merge(df_mxmunicipio_2020, ordenes_2020, by.x="municipio_name", by.y="municipio")
ordenes_2021 <-merge(df_mxmunicipio_2020, ordenes_2021, by.x="municipio_name", by.y="municipio")
ordenes_2022 <-merge(df_mxmunicipio_2020, ordenes_2022, by.x="municipio_name", by.y="municipio")
ordenes_2023 <-merge(df_mxmunicipio_2020, ordenes_2023, by.x="municipio_name", by.y="municipio")

ordenes_2019$value<- ordenes_2019$total
ordenes_2020$value<- ordenes_2020$total
ordenes_2021$value<- ordenes_2021$total
ordenes_2022$value<- ordenes_2022$total
ordenes_2023$value<- ordenes_2023$total

ordenes_2019<- ordenes_2019 %>% filter(state_name=="Jalisco")
ordenes_2020<- ordenes_2020 %>% filter(state_name=="Jalisco")
ordenes_2021<- ordenes_2021 %>% filter(state_name=="Jalisco")
ordenes_2022<- ordenes_2022 %>% filter(state_name=="Jalisco")
ordenes_2023<- ordenes_2023 %>% filter(state_name=="Jalisco")









# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `Región Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
#                    "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")
# 
# `Región Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
#                          "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
#                          "Unión de San Antonio", "Villa Hidalgo")
# 
# `Región Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
#                        "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
#                        "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")
# 
# `Región Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
#                      "Poncitlán", "Tototlán", "Zapotlán del Rey")
# 
# 
# `Región Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
#                      "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
#                      "Tuxcueca","Valle de Juárez")
# 
# 
# `Región Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
#                  "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
#                  "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")
# 
# `Región Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
#                              "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
#                              "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")
# 
# 
# `Región Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán",
#                        "La Huerta", "Tomatlán", "Villa Purificación")
# 
# 
# 
# `Región Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
#                                      "Mascota", "Mixtlán", "Puerto Vallarta",
#                                      "San Sebastián del Oeste", "Talpa de Allende")
# 
# 
# 
# `Región Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
#                     "Etzatlán", "Hostotipaquillo", "Magdalena",
#                     "San Juanito de Escobedo", "San Marcos", "Tala",
#                     "Tequila", "Teuchitlán")
# 
# 
# `Región Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
#                      "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
#                      "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
#                      "Villa Corona", "Zacoalco de Torres")
# 
# 
# `Región Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
#                     "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
#                     "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan",
#                     "Zapotlanejo")
# 
# 
# 
# 
# #Carpetas
# Regiones<-read.csv("IDM_NM_oct22.csv", encoding="latin1", check.names = T) %>%
#   filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
#                                   "Violación simple", "Violación equiparada", "Feminicidio",
#                                   "Violencia familiar",
#                                   "Violencia de género en todas sus modalidades distinta a la violencia familiar"),
#          Entidad=="Jalisco") %>%
#   mutate(Región = case_when(
#     Municipio %in% `Región Norte` ~ "Región Norte",
#     Municipio %in% `Región Altos Norte` ~ "Región Altos Norte",
#     Municipio %in% `Región Altos Sur` ~ "Región Altos Sur",
#     Municipio %in% `Región Ciénega` ~ "Región Ciénega",
#     Municipio %in% `Región Sureste` ~ "Región Sureste",
#     Municipio %in% `Región Sur` ~ "Región Sur",
#     Municipio %in% `Región Sierra de Amula` ~ "Región Sierra de Amula",
#     Municipio %in% `Región Costa Sur` ~ "Región Costa Sur",
#     Municipio %in% `Región Costa-Sierra Occidental` ~ "Región Costa-Sierra Occidental",
#     Municipio %in% `Región Valles` ~ "Región Valles",
#     Municipio %in% `Región Lagunas` ~ "Región Lagunas",
#     Municipio %in% `Región Centro` ~ "Región Centro")) %>%
#   group_by(Año, Región, Subtipo.de.delito) %>%
#   summarise(ene=sum(Enero, na.rm = T),
#             feb=sum(Febrero, na.rm = T),
#             mar=sum(Marzo, na.rm = T),
#             abr=sum(Abril, na.rm = T),
#             may=sum(Mayo, na.rm = T),
#             jun=sum(Junio, na.rm = T),
#             jul=sum(Julio, na.rm = T),
#             ago=sum(Agosto, na.rm = T),
#             sep=sum(Septiembre, na.rm = T),
#             oct=sum(Octubre, na.rm = T),
#             nov=sum(Noviembre, na.rm = T),
#             dic=sum(Diciembre, na.rm = T),
#             Total=sum(ene+ feb+ mar+ abr+
#                         may+ jun + jul+ ago+
#                         sep+ oct+ nov+ dic))
# 
# write.csv(Regiones, "Regiones.csv")

Regiones<-read.csv("Regiones.csv", encoding="latin-1", check.names = T)

Regiones %>% 
  pivot_longer(cols = ene:dic,
               names_to = "mes",
               values_to = "total") %>% 
  mutate(mes=case_when(
    mes=="ene"~1,
    mes=="feb"~2,
    mes=="mar"~3,
    mes=="abr"~4,
    mes=="may"~5,
    mes=="jun"~6,
    mes=="jul"~7,
    mes=="ago"~8,
    mes=="sep"~9,
    mes=="oct"~10,
    mes=="nov"~11,
    mes=="dic"~12),
    
    
    month=case_when(
      mes==1~"Enero",
      mes==2~"Febrero",
      mes==3~"Marzo",
      mes==4~"Abril",
      mes==5~"Mayo",
      mes==6~"Junio",
      mes==7~"Julio",
      mes==8~"Agosto",
      mes==9~"Septiembre",
      mes==10~"Octubre",
      mes==11~"Noviembre",
      mes==12~"Diciembre")
    ,
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo",
                          "Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre",
                          "Octubre", "Noviembre", "Diciembre"))
  ) %>%
  mutate(Periodo = ymd(paste0(Año, "-", mes, "-01"))) %>% 
  mutate(text = paste("Año: ", Año,
                      "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                      "\nTotal de carpetas: ", scales::comma(Total), sep="")) %>% 
  filter(Periodo <= "2023-06-01")->Regiones #actualizar

#Regiones$Periodo<-substr(Regiones$Periodo, start = 1, stop = 7)


# - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - -

municipios_2<-read.csv("municipios.csv")

# municipios_2<-read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   mutate(Subtipo.de.delito=case_when(
#     Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
#     Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
#     Subtipo.de.delito=="Violación equiparada"~"Violación",
#     Subtipo.de.delito=="Violación simple"~ "Violación",
#     Subtipo.de.delito=="Feminicidio"~"Feminicidio",
#     Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
#     Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
#   filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
#                                   "Hostigamiento sexual",
#                                   "Violación", "Violencia familiar"))

municipios_2<-municipios_2 %>%
  group_by(Año, Subtipo.de.delito, Municipio) %>%
  summarise(ene=sum(ene, na.rm = T),
            feb=sum(feb, na.rm = T),
            mar=sum(mar, na.rm = T),
            abr=sum(abr, na.rm = T),
            may=sum(may, na.rm = T),
            jun=sum(jun, na.rm = T),
            jul=sum(jul, na.rm = T),
            ago=sum(ago, na.rm = T),
            sep=sum(sep, na.rm = T),
            oct=sum(oct, na.rm = T),
            nov=sum(nov, na.rm = T),
            dic=sum(dic, na.rm = T),
            Total=sum(ene+ feb+ mar+ abr+
                        may+ jun + jul+ ago+
                        sep+ oct+ nov+ dic))




# read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   filter(Entidad=="Jalisco",
#          Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
#                                   "Violación simple", "Violación equiparada", "Feminicidio",
#                                   "Violencia familiar",
#                                   "Violencia de género en todas sus modalidades distinta a la violencia familiar")) %>%
#   group_by(Año, Subtipo.de.delito) %>% summarise(ene=sum(Enero, na.rm = T),
#                                                  feb=sum(Febrero, na.rm = T),
#                                                  mar=sum(Marzo, na.rm = T),
#                                                  abr=sum(Abril, na.rm = T),
#                                                  may=sum(Mayo, na.rm = T),
#                                                  jun=sum(Junio, na.rm = T),
#                                                  jul=sum(Julio, na.rm = T),
#                                                  ago=sum(Agosto, na.rm = T),
#                                                  sep=sum(Septiembre, na.rm = T),
#                                                  oct=sum(Octubre, na.rm = T),
#                                                  nov=sum(Noviembre, na.rm = T),
#                                                  dic=sum(Diciembre, na.rm = T),
#                                                  Total=sum(ene+ feb+ mar+ abr+
#                                                              may+ jun + jul+ ago+
#                                                              sep+ oct+ nov+ dic))-> Estatal_total
# entidad<- c("Estado de Jalisco")
# cbind(entidad, Estatal_total)->Estatal_total
# names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
# rbind(Estatal_total, municipios_2 )->municipios
# write.csv(municipios, "municipios.csv")

municipios<-read.csv("municipios.csv",check.names = T, encoding = "latin-1") %>% 
  # mutate(Subtipo.de.delito=case_when(
  #   Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
  #   Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
  #   Subtipo.de.delito=="Violación equiparada"~"Violación",
  #   Subtipo.de.delito=="Violación simple"~ "Violación",
  #   Subtipo.de.delito=="Feminicidio"~"Feminicidio",
  #   Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
  #   Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
  # filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
  #                                 "Hostigamiento sexual",
  #                                 "Violación", "Violencia familiar")) %>% 
pivot_longer(cols = ene:dic,
             names_to = "mes",
             values_to = "total") %>% 
  mutate(mes=case_when(
    mes=="ene"~1,
    mes=="feb"~2,
    mes=="mar"~3,
    mes=="abr"~4,
    mes=="may"~5,
    mes=="jun"~6,
    mes=="jul"~7,
    mes=="ago"~8,
    mes=="sep"~9,
    mes=="oct"~10,
    mes=="nov"~11,
    mes=="dic"~12),
    
    
    month=case_when(
      mes==1~"Enero",
      mes==2~"Febrero",
      mes==3~"Marzo",
      mes==4~"Abril",
      mes==5~"Mayo",
      mes==6~"Junio",
      mes==7~"Julio",
      mes==8~"Agosto",
      mes==9~"Septiembre",
      mes==10~"Octubre",
      mes==11~"Noviembre",
      mes==12~"Diciembre")
    ,
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo",
                          "Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre",
                          "Octubre", "Noviembre", "Diciembre"))
  ) %>%
  mutate(Periodo = ymd(paste0(Año, "-", mes, "-01"))) %>% 
  # mutate(text = paste("Año: ", Año,
  #                     "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
  #                     "\nTotal de carpetas: ", scales::comma(total), sep="")) %>% 
  filter(Periodo <= "2023-06-01") #actualizar



################################################################################
################################################################################

# victimas <- read.csv("IDVFC_NM_oct22.csv", encoding="latin1", check.names = T) %>%
#   filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
#          Sexo=="Mujer", Entidad=="Jalisco")
# write.csv(victimas, "victimas.csv")


victimas<-read.csv("victimas.csv",check.names = T, encoding = "latin-1")

################################################################################

atenciones <- read_excel("atenciones_shiny.xlsx") %>% 
  pivot_longer(
    cols=Psicológica:Digital,
    names_to = "tipo", 
    values_to = "total") %>% 
  suppressWarnings()

# atenciones$fecha   <-as.POSIXct(atenciones$Fecha) 
# atenciones$fecha   <-as.Date(atenciones$Fecha,format="%Y-%m-%d")
# atenciones$mes     <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%Y-%m")
# atenciones$año     <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%Y")
# atenciones$month   <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%B")
# 

atenciones <- atenciones %>% 
  mutate(Month=case_when(
    Mes=="Enero" ~ 1,
    Mes=="Febrero" ~ 2,
    Mes=="Marzo" ~ 3,
    Mes=="Abril" ~ 4,
    Mes=="Mayo" ~ 5,
    Mes=="Junio" ~ 6,
    Mes=="Julio" ~ 7,
    Mes=="Agosto" ~ 8,
    Mes=="Septiembre" ~ 9,
    Mes=="Octubre" ~ 10,
    Mes=="Noviembre" ~ 11,
    Mes=="Diciembre" ~ 12),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    Fecha = ymd(paste0(Año, "-", Month, "-01")))


datos_victimas<-read.csv("bases_publicas/incidencia_jalisco_victimas.csv",check.names = T, encoding = "latin-1")
datos_incidencia_mun<-read_excel("bases_publicas/incidencia_municipal.xlsx")
datos_incidencia_regional<-read_excel("bases_publicas/incidencia_regional.xlsx")
medidas_y_ordenes <- read_excel("bases_publicas/medidas_ordenes.xlsx")
llamadas_911<-read_excel("bases_publicas/reportes_llamadas_911.xlsx")
violencia_familiar <- read_excel("bases_publicas/violencia_familiar_diario.xlsx")
base_atenciones <- read_excel("bases_publicas/base_atenciones.xlsx")





################################################################################


ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
      class = 'p-0',
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
    
  .small-box.bg-fuchsia {
   background-color: #b06497 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-purple {
   background-color: #B14C71 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon {
     background-color: #8F5199 !important; 
   color: white !important;       
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-light-blue {
   background-color: #5d3d6c !important; 
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
      
      navbarPage(title = "DATOS ABIERTOS", 
                 header=
                   busy_start_up(
                     loader = spin_epic("flower", color = "#8F5199"),
                     text = "Cargando",
                     timeout = 1500,
                     color = "#8F5199",
                     background = " white"),
                 useShinydashboard(),
                 
                 navbarMenu(title = "CÓDIGO VIOLETA", #icon = icon("dot-circle"),
                            tabPanel(title = "Reportes 911", class="p-2",
                                     #tabsetPanel(
                                     box(width=12,  
                                         div(class="row d-flex", #Replicar
                                             valueBox("En promedio", "se atienden 6,697 llamadas al mes relacionadas a violencias por razón de género y 223  al día.",icon=icon("chart-area"),color="fuchsia"),
                                             valueBox("Máximo histórico", "marzo es el mes con mayor registro de llamadas: 8,986 y 8,714 en 2021 y 2022 respectivamente.", icon=icon("equals"), color="purple"),
                                             valueBox("354,963 llamadas al 911", "por razón de género desde 2019 a mayo 2023, siendo el principal motivo (44%) por violencia familiar.", icon=icon("wave-square"), color="maroon")),
                                         
                                         sidebarLayout(
                                           sidebarPanel("Seleccione algunas características", class=".mb-2",
                                                        selectInput(
                                                          inputId = "llamadas_año",
                                                          label = "Año",
                                                          choices = unique(sort(reportes_llamadas$año)),
                                                          multiple = T
                                                        ),
                                                        selectInput(
                                                          inputId = "llamadas_month",
                                                          label = "Mes",
                                                          choices = unique(sort(reportes_llamadas$month)),
                                                          multiple = TRUE
                                                        ),
                                                        selectInput(
                                                          inputId = "llamadas_clasificacion",
                                                          label = "Tipo de violencia",
                                                          choices = unique(sort(reportes_llamadas$clasificación)),
                                                          multiple = TRUE
                                                        ),
                                                        selectInput(
                                                          inputId = "llamadas_municipio",
                                                          label = "Municipio",
                                                          choices = unique(sort(reportes_llamadas$municipio)),
                                                          multiple = T,
                                                        ),  
                                                        downloadButton("downloadData_llamadas", "Descarga (.csv)")
                                           ),
                                           
                                           mainPanel(h3("Total de llamadas al 911 por razón de género", align="center"),
                                                     plotlyOutput("grafico_llamadas",  
                                                                  width = "auto", height = "auto"),
                                                     h6("Fuente: Datos proporcionados por Escudo Urbano C5."),
                                                     h6("Los registros del 'Reporte al 911' son las llamadas que se contabilizan por motivo de violencia contra las mujeres.
                                                  Se clasifican las violencias en tres: 1) violencia contra las mujeres, 2) violencia de pareja y 3) violencia familiar."))
                                         ))),
                            
                            tabPanel(title = "Violencia familiar", class="p-2",
                                     #tabsetPanel(
                                     box(
                                       width=12,  
                                       div(class="row d-flex", #Replicar
                                           valueBox("En promedio", "se atienden 1,883 denuncias por violencia familiar al mes, 60 al día.",icon=icon("chart-area"),color="fuchsia"), 
                                           valueBox("9 de cada 10", "denuncias por violencia familiar fueron realizadas por mujeres.", icon=icon("equals"), color="purple"),
                                           valueBox("71,584 denuncias","registradas en el periodo de marzo de 2020 a marzo 2023.", icon=icon("wave-square"), color="maroon"))), #En 2021 el total fue de 12,783 y en 2020 de 10,003
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características", class=".mb-2",
                                                    selectInput(
                                                      inputId = "violencia_familiar_año",
                                                      label = "Año",
                                                      choices = unique(sort(violencia_familiar_diario$año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "violencia_familiar_month",
                                                      label = "Mes",
                                                      choices = unique(sort(violencia_familiar_diario$month)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "violencia_familiar_sexo",
                                                      label = "Sexo",
                                                      choices = unique(sort(violencia_familiar_diario$sexo)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "violencia_familiar_zona",
                                                      label = "Zona",
                                                      choices = unique(sort(violencia_familiar_diario$zona)),
                                                      multiple = F,
                                                      selected = "Estado de Jalisco"
                                                    ),
                                                    downloadButton("download_violencia_familiar", "Descarga (.csv)")
                                       ),
                                       mainPanel(h3("Total de denuncias por violencia familiar", align="center"),
                                                 plotlyOutput("grafico_violencia_familiar",  height = "auto", width = "auto"),
                                                 h6("Fuente: Elaborado con datos de la Fiscalía Estatal.")))
                                     #)
                            ),
                            
                            # - - - - - - - - -- - - - - - - - - - - - - - - - - - 
                            tabPanel(title = "Medidas de protección", class="p-2",
                                     #fluidRow(width=12,  
                                     #h3(align="center", "Medidas de protección trabajadas"),
                                     #box(width=12,
                                     #tabsetPanel(
                                     box(
                                       width=12, 
                                       div(class="row d-flex", #Replicar
                                           
                                           valueBox("2023", "De enero a junio se otorgaron 16,568, 23% más con respecto al mismo período del año 2022",icon=icon("chart-area"),color="fuchsia", width = 3),
                                           valueBox("2022", "Se otorgaron 27,376 medidas de protección. Un aumento del 26% en comparación con el año anterior", icon=icon("equals"), color="purple", width = 3),
                                           valueBox("2021", "Se otorgaron 21,644 medidas de protección. Un aumento del 24% en comparación con el año anterior", icon=icon("wave-square"), color="maroon", width = 3),
                                           valueBox("2020", "Se otorgaron 17,473 medidas de protección. Una disminución del 4% con respecto al año anterior.", icon=icon("signal"), color="light-blue", width = 3))),
                                     
                                     tabsetPanel(
                                       tabPanel("Total de medidas",  class="mb-2",
                                                #h2(align="center", "Medidas de protección trabajadas")
                                                
                                                sidebarPanel("Seleccione algunas características", class=".mb-2",
                                                             selectInput(
                                                               inputId = "medidas_año",
                                                               label = "Año",
                                                               choices = unique(sort(medidas_ordenes_municipal$año)),
                                                               multiple = TRUE                                                               
                                                             ),                                                               
                                                             selectInput(
                                                               inputId = "medidas_mes",
                                                               label = "Mes",
                                                               choices = unique(sort(medidas_ordenes_municipal$mes)),
                                                               multiple = TRUE
                                                             ),
                                                             selectInput(
                                                               inputId = "medidas_municipio",
                                                               label = "Municipio",
                                                               choices = unique(sort(medidas_ordenes_municipal$municipio)),
                                                               multiple = TRUE
                                                             ),
                                                             downloadButton("downloadData_medidas", "Descarga (.csv)")
                                                ),
                                                mainPanel(h3("Total de medidas de protección emitidas", align="center"),
                                                          plotlyOutput("grafico_medidas", height = "auto", width = "auto"),
                                                          h6("Fuente: Elaborado con datos de la Fiscalía Estatal a junio 2023."))),
                                       tabPanel("Mapa de medidas de protección", class="p-2",
                                                column(12, align="center",
                                                       h2(""),
                                                       
                                                       #h2("Total de medidas trabajadas en el estado de Jalisco, 2019 a 2022"),
                                                       #h6("Datos de la Fiscalía del Estado"),
                                                       selectInput("mapa_medidas", "Seleccione el año" ,
                                                                   choices = c("Año 2019", "Año 2020", 
                                                                               "Año 2021", "Año 2022", "Año 2023"),
                                                                   selected = "Año 2023",  multiple = FALSE, 
                                                                   selectize = TRUE),
                                                       # h3(text=paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas ,
                                                       #           '<br>','<sup>',
                                                       #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                                                       fluidRow(
                                                       splitLayout(cellWidths = c("50%", "50%"),
                                                                   dataTableOutput("table_medidas"),
                                                                   plotlyOutput("mapa_1",height = "auto", width = "auto")
                                                       
                                                       
                                                       )),
                                                       
                                                       h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                                                       h5("*La etiqueta del mapa 'value' hace referencia al valor total de medidas de protección.", 
                                                          align="left", face="italic")))
                                       #)
                                       #)
                                     )),
                            
                            #--------------------------------------------
                            
                            tabPanel(title = "Órdenes de protección", class="p-2",
                                     #fluidRow(width=12,  
                                     #h3(align="center", "Órdenes de protección emitidas"),
                                     #box(width=12,
                                     #tabsetPanel(
                                     box(
                                       width=12,  
                                       div(class="row d-flex", #Replicar
                                           valueBox("2023", "De enero a junio de 2023, se otorgaron 170 órdenes de medidas, 42% más con respecto al mismo período del año 2022.",icon=icon("chart-area"),color="fuchsia", width = 3),
                                           valueBox("2022", "Se otorgaron 245 órdenes de protección, una reducción del 32% con respecto al año anterior.", icon=icon("equals"), color="purple", width = 3),
                                           valueBox("2021", "Se otorgaron 359 órdenes de protección, un aumento del 81% con respecto al año anterior.", icon=icon("wave-square"), color="maroon", width = 3),
                                           valueBox("2020", "Se otorgaron 198 órdenes de protección, un aumento del 607% con respecto al año anterior.", icon=icon("signal"), color="light-blue", width = 3))),
                                     
                                     tabsetPanel(
                                       tabPanel("Total de órdenes",  class="mb-2",
                                                #box(width=12,
                                                # tabsetPanel(
                                                #   #h2(align="center", "ordenes de protección trabajadas")
                                                # ),
                                                sidebarPanel("Seleccione algunas características", class=".mb-2",
                                                             selectInput(
                                                               inputId = "ordenes_año",
                                                               label = "Año",
                                                               choices = unique(sort(medidas_ordenes_municipal$año)),
                                                               multiple = TRUE),                                                               
                                                             selectInput(
                                                               inputId = "ordenes_mes",
                                                               label = "Mes",
                                                               choices = unique(sort(medidas_ordenes_municipal$mes)),
                                                               multiple = TRUE
                                                             ),
                                                             selectInput(
                                                               inputId = "ordenes_municipio",
                                                               label = "Municipio",
                                                               choices = unique(sort(medidas_ordenes_municipal$municipio)),
                                                               multiple = TRUE
                                                             ),
                                                             downloadButton("downloadData_ordenes", "Descarga (.csv)")),
                                                mainPanel(h3("Total de órdenes de protección emitidas", align="center"),
                                                          plotlyOutput("grafico_ordenes", height = "auto", width = "auto"),
                                                          h6("Fuente: Elaborado con datos de la Fiscalía Estatal a junio 2023."))),
                                       tabPanel("Mapa de órdenes de protección", class="p-2",
                                                #tags$br(),
                                                column(12, align="center",
                                                       h2(""),
                                                       
                                                       #h2("Total de ordenes trabajadas en el estado de Jalisco, 2019 a 2022"),
                                                       #h6("Datos de la Fiscalía del Estado"),
                                                       selectInput("mapa_ordenes", "Seleccione el año" ,
                                                                   choices = c("Año 2019", "Año 2020", 
                                                                               "Año 2021", "Año 2022", "Año 2023"),
                                                                   selected = "Año 2023",  multiple = FALSE, 
                                                                   selectize = TRUE),
                                                       # h3(text=paste0("Total de ordenes de protección otorgadas: ", input$mapa_ordenes ,
                                                       #           '<br>','<sup>',
                                                       #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                                                       
                                                       fluidRow(
                                                         splitLayout(cellWidths = c("50%", "50%"),
                                                                     dataTableOutput("table_ordenes"),
                                                                     plotlyOutput("mapa_2",height = "auto", width = "auto")
                                                                     )),
                                                                     
                                                       h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                                                       h5("*La etiqueta del mapa 'value' hace referencia al valor total de órdenes de protección.", 
                                                          align="left", face="italic")))
                                       #)
                                     )),
                            
                            tabPanel(title = "Muertes violentas", class="p-2",
                                     #fluidRow(width = 12, 
                                     #h2("Muertes violentas de mujeres"),
                                     #box(width = 12,
                                     #tabsetPanel(
                                     box(
                                       width=12,  
                                       div(class="row d-flex", #Replicar
                                           valueBox("2023 (mayo)", "Se contabilizan 86 muertes violentas de mujeres, 73 homicidios dolosos y 13 feminicidios.",icon=icon("chart-area"),color="fuchsia", width = 4), #Actualizar
                                           valueBox("Máximo histórico", "2019 es el año con mayor número de muertes violentas con 285: homicidios dolosos 218 y feminicidios 67.", icon=icon("equals"), color="purple", width = 4),
                                           valueBox("Crecimiento anual", "La mayor variación anual se presenta en 2018 al aumentar en 70.5% con respecto a 2017.", icon=icon("wave-square"), color="maroon", width = 4))),
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características", class=".mb-2",
                                                    
                                                    
                                                    selectInput(
                                                      inputId = "victimas_año",
                                                      label = "Año",
                                                      choices = unique(sort(victimas$Año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "victimas_edad",
                                                      label = "Rango de edad",
                                                      choices = unique(sort(victimas$Rango.de.edad)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "victimas_delito",
                                                      label = "Delito",
                                                      choices = unique(sort(victimas$Subtipo.de.delito)),
                                                      multiple = TRUE
                                                    ),  
                                                    downloadButton("downloadData_victimas", "Descarga (.csv)")),
                                       mainPanel(h3("Total de muertes violentas de mujeres", align="center"),
                                                 plotlyOutput("grafico_victimas",  height = "auto", width = "auto"),
                                                 dataTableOutput("table_muertes"),
                                                 h6("Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, SESNSP"))))#)
                            
                            # box(width=12,
                            #     mainPanel(dataTableOutput("table_muertes"))
                            # )
                 ),
                 
                 
                 #############################################################################################
                 #############################################################################################
                 
                 navbarMenu(
                   title = "Incidencia delictiva",
                   #icon = icon("dot-circle"),
                   tabPanel(title = "Municipal",
                            
                            tabPanel("Total por municipio",
                                     box(
                                       width=12, 
                                       div(class="row d-flex", #Replicar
                                           valueBox("Junio 2023", "se registran 2,226 carpetas iniciadas 
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia"),#actualizar cada día 20
                                           valueBox("De 2015 a 2016", "se presenta la variación anual más grande del histórico con 37%, al pasar de 10,704 a 14,634.", icon=icon("equals"), color="purple"),#actualizar cada día 20
                                           valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 86%, 
                                                seguido de abuso sexual (22%) y violación con 2%.", icon=icon("wave-square"), color="maroon"))),#actualizar cada día 20
                                     
                                     sidebarLayout(
                                       sidebarPanel("\nSeleccione algunas características",
                                                    selectInput(
                                                      inputId = "municipal_año",
                                                      label = "Año",
                                                      choices = sort(unique(municipios$Año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_mes",
                                                      label = "Mes",
                                                      choices = sort(unique(municipios$month)),
                                                      multiple = T,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_delito",
                                                      label = "Delito",
                                                      choices = sort(unique(municipios$Subtipo.de.delito)),
                                                      multiple = T,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_municipio",
                                                      label = "Municipio",
                                                      choices = sort(unique(municipios$Municipio)),
                                                      multiple = F,
                                                      selected = "Estado de Jalisco"
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),
                                                    
                                                    downloadButton("downloadData_municipal", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(h4(align="center","Total de carpetas de investigación"),
                                                 plotlyOutput("grafico_municipal_periodo"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a junio de 2023"),#actualizar cada día 20
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."),
                                                 br(),
                                                 h4(align="center","Total de carpetas de investigación"),
                                                 plotlyOutput("grafico_municipal"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a junio de 2023"),#actualizar cada día 20
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."))
                                     ))),
                   
                   
                   #############################################################################################
                   
                   tabPanel(title = "Comparativa Regional",
                            tabPanel("Incidencia delictiva por Regiones",
                                     box(
                                       width=12, 
                                       div(class="row d-flex", #Replicar
                                           valueBox("Junio 2023", "se registran 2,226 carpetas iniciadas 
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia"),#actualizar cada día 20
                                           valueBox("De 2015 a 2016", "se presenta la variación anual más grande del histórico con 37%, al pasar de 10,704 a 14,634.", icon=icon("equals"), color="purple"),#actualizar cada día 20
                                           valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 86%, 
                                                seguido de abuso sexual (22%) y violación con 2%.", icon=icon("wave-square"), color="maroon"))),#actualizar cada día 20
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    selectInput(
                                                      inputId = "Regional_Año",
                                                      multiple = T,
                                                      label = "Año",
                                                      choices = sort(unique(Regiones$Año))
                                                    ),
                                                    selectInput(
                                                      inputId = "Regional_Mes",
                                                      label = "Mes",
                                                      multiple = T,
                                                      choices = sort(unique(Regiones$month))
                                                    ),
                                                    selectInput(
                                                      inputId = "Regional_delito",
                                                      label = "Delito",
                                                      choices = sort(unique(Regiones$Subtipo.de.delito)),
                                                      multiple = T,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "Regional_Región",
                                                      label = "Región",
                                                      choices = sort(unique(Regiones$Región)),
                                                      multiple = F,
                                                      selected = c("Región Altos Norte")
                                                    ),
                                                    downloadButton("downloadData_regional", "\nDescarga (.csv)")
                                       ),
                                       
                                       
                                       mainPanel(h4(align="center","Total de carpetas de investigación"),
                                                 plotlyOutput("grafico_Regional"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a junio de 2023"),
                                                 h4(align="center","Total de carpetas de investigación"),
                                                 plotlyOutput("regional_anual",height = "auto", width = "auto"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a junio de 2023"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."))
                                     )))
                 ),
                 
                 
                 navbarMenu(
                   title = "Unidades de atención",
                   #icon = icon("dot-circle"),
                   tabPanel(title = "Total de atenciones",
                            tabPanel("Total por servicios brindados en el año 2022.",
                                     box(
                                       width=12, 
                                       div(class="row d-flex", #Replicar
                                           valueBox("Mayo 2023", "se han brindado 2,412 servicios por violencia en razón de género en el mes y durante el año 2022 fueron 23,514.", icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox("En promedio", "se realizan 80 servicios por violencia al día en lo que va de 2023.",icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox("El 51% de los servicios", "se realizaron en la Unidad Metropolitana de Atención Integral (UMEA), seguido de Zapotlán el Grande con 11%.", icon=icon("equals"), color="maroon", width = 4))),
                                     
                                     #h3(align="center", "Total de atenciones realizadas en las módulos de atención."),
                                     #h5(align="center", "Los datos son proporcionados mensualmente por el registro interno a cada usuaria que solicita atención en cualquiera de las unidades de atención."),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    selectInput(
                                                      inputId = "atencion_año",
                                                      label = "Año",
                                                      choices = sort(unique(atenciones$Año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_mes",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$Mes)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo",
                                                      label = "Tipo de violencia por la que se atiende",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                                    ),
                                                    
                                                    downloadButton("downloadData_atenciones_1", "Descarga (.csv)")),
                                       mainPanel(h3(align="center","Total de servicios brindados por violencias"),
                                                 plotlyOutput("total_atenciones_grafico"),
                                                 h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital."))))),
                   
                   tabPanel(title = "Comportamiento anual por tipo de violencia",
                            tabPanel("Total por atenciones brindadas en el año 2022.",
                                     box(
                                       width=12,  
                                       div(class="row d-flex", #Replicar
                                           valueBox("Mayo 2023", "la violencia más atendida es violencia psicológica con 33%, seguido de violencia económica con 20%.", icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox("En el año 2023", "las violencias más atendidas son: violencia psicológica (61%), económica (34%) y física (33%).",icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox("Aumentó en 51%", "el total de servicios brindados en el mes de mayo 2023 con respecto el año anterior, al pasar  de 1,596 (2022) a 2,412 (2023).", icon=icon("equals"), color="maroon", width = 4))),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    selectInput(
                                                      inputId = "atencion_año2",
                                                      label = "Año",
                                                      choices = sort(unique(atenciones$Año)),
                                                      multiple = F,
                                                      selected = 2023
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_mes2",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$Mes)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo2",
                                                      label = "Tipo de violencia",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad2",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                                    ),
                                                    
                                                    downloadButton("downloadData_atenciones_2", "\nDescarga (.csv)")),
                                       mainPanel(h3(align="center","Total de servicios brindados por tipo de violencia"),
                                                 plotlyOutput("atenciones_mensuales"),
                                                 h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital."))))),
                   #                 )))),
                   
                   tabPanel(title = "Atenciones por tipo de violencia",
                            tabPanel("Total de atenciones brindadas en el año por tipo de violencia.",
                                     box(
                                       width=12, 
                                       div(class="row d-flex", #Replicar
                                           valueBox("Mayo 2023", "la violencia más atendida es violencia psicológica con 33%, seguido de violencia económica con 20%.", icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox("En el año 2023", "las violencias más atendidas son: violencia psicológica (61%), económica (34%) y física (33%).",icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox("Aumentó en 51%", "el total de servicios brindados en el mes de mayo 2023 con respecto el año anterior, al pasar  de 1,596 (2022) a 2,412 (2023).", icon=icon("equals"), color="maroon", width = 4))),
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    selectInput(
                                                      inputId = "atencion_año3",
                                                      label = "Año",
                                                      choices = sort(unique(atenciones$Año)),
                                                      multiple = F,
                                                      selected = 2023
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_mes3",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$Mes)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo3",
                                                      label = "Tipo de violencia",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad3",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                                    ),
                                                    
                                                    downloadButton("downloadData_atenciones_3", "\nDescarga (.csv)")),
                                       mainPanel(h3(align="center","Total de servicios brindados por tipo de violencia"),
                                                 plotlyOutput("atenciones_tipo"),
                                                 h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital."))))),
                 ),
                 navbarMenu(title = "Interrupción legal del embarazo", 
                            tabPanel("Serie histórica", class="p-2", 
                                     box(
                                       width=12,
                                       div(class="row d-flex", #Replicar
                                           valueBox(
                                             value = "En el histórico",
                                             subtitle = paste0("De ", min(total_aborto$ao), " a ", max(total_aborto$ao), " (",
                                                               mes_nombre(floor_date(max(aborto$fecha), "month")), ") se han atendido ",
                                                               comma(sum(total_aborto$Total)), " interrupciones del embarazo"
                                             ),
                                             icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox(
                                             value = paste0("En el año ", max(total_aborto$ao)),
                                             subtitle =  paste0("Se registran ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)])), 
                                                                " iles mientras que en ",  max(total_aborto$ao)-1,
                                             " se registraron ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)-1]))
                                                                
                                             ),icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox(
                                             value = "La causal principal es",
                                             subtitle =  paste0(aborto_causal$causal[1], " con ", percent(aborto_causal$percent[1], .1), 
                                             " (año ",  max(total_aborto$ao), ")", " mientras que ", 
                                             aborto_causal$causal[2], " tiene ", percent(aborto_causal$percent[2], .1)
                                             ), icon=icon("equals"), color="maroon", width = 4))
                                       ), 
                                     tabsetPanel(
                                       tabPanel(title = "Mensual",
                                                sidebarLayout(
                                                  sidebarPanel("Seleccione algunas características",
                                                               dateRangeInput(
                                                                 inputId = "date_aborto",
                                                                 label = "Rango de fechas",
                                                                 start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                                 min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                                 max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                                                 end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es",
                                                                 separator = "-"
                                                               ),
                                                               selectInput(
                                                                 inputId = "causal_aborto",
                                                                 label = "Causales",
                                                                 choices = sort(unique(aborto$causal)),
                                                                 multiple = T
                                                               ),
                                                               selectInput(
                                                                 inputId = "hospital_aborto",
                                                                 label = "Hospital",
                                                                 choices = sort(unique(aborto$hospital)),
                                                                 multiple = T
                                                               ),
                                                               selectInput(
                                                                 inputId = "redad_aborto",
                                                                 label = "Rango de edad",
                                                                 choices = c("Menor a 15 años", "15 a 17", "+18",
                                                                             "Desconocido"),
                                                                 multiple = T,
                                                               ),
                                                               
                                                               downloadButton("downloadData_aborto", "\nDescarga (.csv)")
                                                  ),
                                                  mainPanel(h3(align="center","Total de interrupciones legales del embarazo"),
                                                            plotlyOutput("aborto_ts"),
                                                            h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")
                                                            # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                            #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                            # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                            #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                                  )
                                                )
                                       ),
                                       
                                       tabPanel(title = "Anual",
                                                # sidebarLayout(
                                                #   sidebarPanel("Seleccione algunas características",
                                                #                dateRangeInput(
                                                #                  inputId = "date_aborto",
                                                #                  label = "Rango de fechas",
                                                #                  start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                #                  min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                #                  max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                                #                  end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es",
                                                #                  separator = "-"
                                                #                ),
                                                #                selectInput(
                                                #                  inputId = "causal_aborto",
                                                #                  label = "Causales",
                                                #                  choices = sort(unique(aborto$causal)),
                                                #                  multiple = T
                                                #                ),
                                                #                selectInput(
                                                #                  inputId = "hospital_aborto",
                                                #                  label = "Hospital",
                                                #                  choices = sort(unique(aborto$hospital)),
                                                #                  multiple = T
                                                #                ),
                                                #                selectInput(
                                                #                  inputId = "redad_aborto",
                                                #                  label = "Rango de edad",
                                                #                  choices = c("Menor a 15 años", "15 a 17", "+18",
                                                #                              "Desconocido"),
                                                #                  multiple = T,
                                                #                ),
                                                #
                                                #                downloadButton("downloadData_aborto", "\nDescarga (.csv)")
                                                #   ),
                                                  mainPanel(h3(align="center","Total de interrupciones legales del embarazo"), width = 12,
                                                            plotlyOutput("aborto_ts_anual"),
                                                            h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")
                                                            # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                            #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                            # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                            #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                                  )
                                                )
                                       

                                       )
                                       


                                     ),
                                     # sidebarLayout(
                                     #   sidebarPanel("Seleccione algunas características",
                                     #                dateRangeInput(
                                     #                  inputId = "date_aborto",
                                     #                  label = "Rango de fechas",
                                     #                  start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                     #                  min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                     #                  max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                     #                  end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es", 
                                     #                  separator = "-"
                                     #                ),
                                     #                selectInput(
                                     #                  inputId = "causal_aborto",
                                     #                  label = "Causales",
                                     #                  choices = sort(unique(aborto$causal)),
                                     #                  multiple = T
                                     #                ),
                                     #                selectInput(
                                     #                  inputId = "hospital_aborto",
                                     #                  label = "Hospital",
                                     #                  choices = sort(unique(aborto$hospital)),
                                     #                  multiple = T
                                     #                ),
                                     #                selectInput(
                                     #                  inputId = "redad_aborto",
                                     #                  label = "Rango de edad",
                                     #                  choices = c("Menor a 15 años", "15 a 17", "+18", 
                                     #                              "Desconocido"),
                                     #                  multiple = T,
                                     #                ),
                                     #                
                                     #                downloadButton("downloadData_aborto", "\nDescarga (.csv)")
                                     #   ),
                                     #   mainPanel(h3(align="center","Total de interrupciones legales del embarazo"),
                                     #             plotlyOutput("aborto_ts"),
                                     #             h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")
                                     #             # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                     #             #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                     #             # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                     #             #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                     #   )
                                     # )
                                     
                            # ),
                            tabPanel("Procedimiento", class="p-2", 
                                     box(
                                       width=12,
                                       div(class="row d-flex", #Replicar
                                           valueBox(
                                             value = "En el histórico",
                                             subtitle = paste0("De ", min(total_aborto$ao), " a ", max(total_aborto$ao), " (",
                                                               mes_nombre(floor_date(max(aborto$fecha), "month")), ") se han atendido ",
                                                               comma(sum(total_aborto$Total)), " interrupciones del embarazo"
                                             ),
                                             icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox(
                                             value = paste0("En el año ", max(total_aborto$ao)),
                                             subtitle =  paste0("Se registran ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)])), 
                                                                " iles mientras que en ",  max(total_aborto$ao)-1,
                                                                " se registraron ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)-1]))
                                                                
                                             ),icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox(
                                             value = "La causal principal es",
                                             subtitle =  paste0(aborto_causal$causal[1], " con ", percent(aborto_causal$percent[1], .1), 
                                                                " (año ",  max(total_aborto$ao), ")", " mientras que ", 
                                                                aborto_causal$causal[2], " tiene ", percent(aborto_causal$percent[2], .1)
                                             ), icon=icon("equals"), color="maroon", width = 4))
                                     ),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    dateRangeInput(
                                                      inputId = "date_aborto2",
                                                      label = "Rango de fechas",
                                                      start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                                      end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es", 
                                                      separator = "-"
                                                    ),
                                                    selectInput(
                                                      inputId = "causal_aborto2",
                                                      label = "Causales",
                                                      choices = sort(unique(aborto$causal)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "hospital_aborto2",
                                                      label = "Hospital",
                                                      choices = sort(unique(aborto$hospital)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "redad_aborto2",
                                                      label = "Rango de edad",
                                                      choices = c("Menor a 15 años", "15 a 17", "+18", 
                                                                  "Desconocido"),
                                                      multiple = T,
                                                    ),
                                                    
                                                    downloadButton("downloadData_aborto2", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(h3(align="center","Total de interrupciones legales del embarazo"),
                                                 plotlyOutput("aborto_procedimiento"),
                                                 h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")                                                 # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                 #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                 #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                       )
                                     )
                                     ), 
                            tabPanel("Semanas de gestación", class="p-2", 
                                     box(
                                       width=12,
                                       div(class="row d-flex", #Replicar
                                           valueBox(
                                             value = "En el histórico",
                                             subtitle = paste0("De ", min(total_aborto$ao), " a ", max(total_aborto$ao), " (",
                                                               mes_nombre(floor_date(max(aborto$fecha), "month")), ") se han atendido ",
                                                               comma(sum(total_aborto$Total)), " interrupciones del embarazo"
                                             ),
                                             icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox(
                                             value = paste0("En el año ", max(total_aborto$ao)),
                                             subtitle =  paste0("Se registran ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)])), 
                                                                " iles mientras que en ",  max(total_aborto$ao)-1,
                                                                " se registraron ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)-1]))
                                                                
                                             ),icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox(
                                             value = "La causal principal es",
                                             subtitle =  paste0(aborto_causal$causal[1], " con ", percent(aborto_causal$percent[1], .1), 
                                                                " (año ",  max(total_aborto$ao), ")", " mientras que ", 
                                                                aborto_causal$causal[2], " tiene ", percent(aborto_causal$percent[2], .1)
                                             ), icon=icon("equals"), color="maroon", width = 4))
                                     ), 
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    dateRangeInput(
                                                      inputId = "date_aborto4",
                                                      label = "Rango de fechas",
                                                      start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                                      end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es", 
                                                      separator = "-"
                                                    ),
                                                    selectInput(
                                                      inputId = "causal_aborto4",
                                                      label = "Causales",
                                                      choices = sort(unique(aborto$causal)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "hospital_aborto4",
                                                      label = "Hospital",
                                                      choices = sort(unique(aborto$hospital)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "redad_aborto4",
                                                      label = "Rango de edad",
                                                      choices = c("Menor a 15 años", "15 a 17", "+18", 
                                                                  "Desconocido"),
                                                      multiple = T,
                                                    ),
                                                    
                                                    downloadButton("downloadData_aborto4", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(h3(align="center","Total de interrupciones legales del embarazo"),
                                                 plotlyOutput("aborto_gestacion"),
                                                 h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")
                                                 
                                                 # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                 #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                 #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                       )
                                     )
                            ),
                            tabPanel("Hospitales", class="p-2", 
                                     box(
                                       width=12,
                                       div(class="row d-flex", #Replicar
                                           valueBox(
                                             value = "En el histórico",
                                             subtitle = paste0("De ", min(total_aborto$ao), " a ", max(total_aborto$ao), " (",
                                                               mes_nombre(floor_date(max(aborto$fecha), "month")), ") se han atendido ",
                                                               comma(sum(total_aborto$Total)), " interrupciones del embarazo"
                                             ),
                                             icon=icon("wave-square"), color="fuchsia", width = 4),
                                           valueBox(
                                             value = paste0("En el año ", max(total_aborto$ao)),
                                             subtitle =  paste0("Se registran ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)])), 
                                                                " iles mientras que en ",  max(total_aborto$ao)-1,
                                                                " se registraron ", comma(sum(total_aborto$Total[total_aborto$ao==max(total_aborto$ao)-1]))
                                                                
                                             ),icon=icon("chart-area"),color="purple", width = 4),
                                           valueBox(
                                             value = "La causal principal es",
                                             subtitle =  paste0(aborto_causal$causal[1], " con ", percent(aborto_causal$percent[1], .1), 
                                                                " (año ",  max(total_aborto$ao), ")", " mientras que ", 
                                                                aborto_causal$causal[2], " tiene ", percent(aborto_causal$percent[2], .1)
                                             ), icon=icon("equals"), color="maroon", width = 4))
                                     ),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    dateRangeInput(
                                                      inputId = "date_aborto3",
                                                      label = "Rango de fechas",
                                                      start = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      min = floor_date(min(aborto$fecha, na.rm = T), "month"),
                                                      max = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1,
                                                      end = ceiling_date(max(aborto$fecha, na.rm = T), "month")-1, language = "es", 
                                                      separator = "-"
                                                    ),
                                                    selectInput(
                                                      inputId = "causal_aborto3",
                                                      label = "Causales",
                                                      choices = sort(unique(aborto$causal)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "hospital_aborto3",
                                                      label = "Hospital",
                                                      choices = sort(unique(aborto$hospital)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "redad_aborto3",
                                                      label = "Rango de edad",
                                                      choices = c("Menor a 15 años", "15 a 17", "+18", 
                                                                  "Desconocido"),
                                                      multiple = T,
                                                    ),
                                                    
                                                    downloadButton("downloadData_aborto3", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(h3(align="center","Total de interrupciones legales del embarazo"),
                                                 plotlyOutput("aborto_hospitales"),
                                                 h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud"),
                                                 leafletOutput("aborto_map"), 
                                                 h6("Fuente: Datos proporcionados de O.P.D. Servicios de Salud")
                                                 
                                                 # h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                 #    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 # h6("Los datos reflejan el total de los servicios brindados por violencias, las cuales son: 1) psicológica,
                                                 #    2) física, 3) patrimonial, 4) económica, 5) sexual y 6) digital.")
                                       )
                                     )
                                     )
                            
                   
                 ),
                 tabPanel(title = "Descarga masiva",
                          includeHTML("carrusel.html"), 
                          includeCSS("carrusel.css", height = "auto", width = "auto"), br(),
                          # div(class = "footer", hr(),
                          # h6(img(src = "https://w7.pngwing.com/pngs/646/324/png-transparent-github-computer-icons-github-logo-monochrome-head-thumbnail.png", width = "25", height="25"),
                          #    "Descarga el código abierto de la plataforma en lenguaje R",
                          #    (a(target="blank",href="https://github.com/Igualdad-Jalisco/App-CodigoVioleta","aquí."))))
                          
                          
                          
                 )))
    
    
  ))
server <- function(input, output, session) {
  
  # Width
  plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})
  
  # Height
  plotHeight <- function(){
    width <- plotWidth()
    h <- ifelse(width > 425, width*0.54, width*0.75)
    return(h)}
  
  # Font
  fontbase <- 8
  
  textFunction <- function(){
    width <- plotWidth()
    textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
    return(textSize)}
  
  
  
  
  
  output$llamadas_año <- renderUI({
    selectInput("llamadas_año",
                label =  "Seleccione el año",
                choices = sort(unique(reportes_llamadas$año)),
                multiple = T)
  })
  
  output$llamadas_month<- renderUI({
    selectInput("llamadas_month",
                label =  "Seleccione el mes",
                choices = sort(unique(reportes_llamadas$month)),
                multiple = T)
  })
  
  
  output$llamadas_clasificacion <- renderUI({
    selectInput("llamadas_clasificacion",
                label =  "Selecciona el tipo de violencia",
                choices = sort(unique(reportes_llamadas$clasificación)),
                multiple = T)
  })
  
  
  output$llamadas_municipio <- renderUI({
    selectInput("llamadas_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(reportes_llamadas$municipio)),
                multiple = T)
  })
  
  
  #base reactiva para slide 3
  reportes_llamadas_reactive <- reactive({
    
    reportes_llamadas %>%
      filter(
        if(!is.null(input$llamadas_año))                       año %in% input$llamadas_año             else año != "",
        if(!is.null(input$slider))                             año %in% input$slider                   else año != "",
        if(!is.null(input$slider2))                             año %in% input$slider2                 else año != "",
        if(!is.null(input$llamadas_month))                       month %in% input$llamadas_month       else month != "",
        if(!is.null(input$llamadas_clasificacion))   clasificación %in% input$llamadas_clasificacion   else clasificación != "",
        if(!is.null(input$llamadas_municipio))           municipio %in% input$llamadas_municipio       else municipio != ""
      )
    
  })
  
  
  output$downloadData_llamadas <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(llamadas_911, file, row.names = F)
    })
  
  
  output$grafico_llamadas <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height  
    
    reportes_llamadas_reactive() %>% 
      # reportes_llamadas %>% 
      group_by(año, mes, clasificación) %>% 
      summarise(total=n(),.groups = "drop") %>% 
      mutate(text = paste("Total de llamadas: ", scales::comma(total), 
                          "\nPeriodo: ", mes,
                          "\nTipo de violencia: ", clasificación, sep="")) %>% 
      ggplot() +
      aes(x = as.factor(mes), y = total, text=text,
          group = 1, fill = clasificación, color = clasificación) +
      geom_point(size=1.7)+
      geom_line(size=1) +
      scale_color_manual(
        values = c(
          `Violencia contra las mujeres` = "#D581B9",
          `Violencia de pareja` = "#B14C71",
          `Violencia familiar` = "#8F5199"))+      
      
      scale_fill_manual(
        values = c(
          `Violencia contra las mujeres` = "#D581B9",
          `Violencia de pareja` = "#B14C71",
          `Violencia familiar` = "#8F5199"))+      
      scale_y_continuous(labels = scales::comma) +
      labs(title= "",
           #paste0("Municipio ",reportes_llamadas_reactive()$municipio[1]),
           x="", y="Total de llamadas", fill="", color="")+
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-12","2022-03","2022-06","2022-09",
                                  "2023-03"))+ #Actualizando conforme se agreguen trimestres
      theme_minimal()+
      theme(text=element_text(size=11,  family="Nutmeg-Light"),
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            legend.text = element_text(lineheight = .8, family="Nutmeg-Light"), 
            legend.key.width = unit(.2, "cm"),
            legend.key.height = unit(.5, "cm"),
            
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90, family="Nutmeg-Light"),
            #legend.text = element_text(size = 10L, hjust = 0.5, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10*textFunction(), family="Nutmeg-Light"))->grafico_llamadas
    
    ggplotly(grafico_llamadas, tooltip = "text") %>%
      layout(
        #title = list(text = paste0("Total de llamadas al 911 por razón de género \n",
        #                                   #reportes_llamadas_reactive()$municipio[1],
        #                                   '<br>',
        #                                   '<sup>')),
        margin = list(b=0,t=0),
        xaxis = list(side = "bottom"),
        legend = list(orientation = "h", x = 0.1, y = -0.3,
                      side="bottom"))
    
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  
  output$violencia_familiar_año <- renderUI({
    selectInput("violencia_familiar_año",
                label =  "Seleccione el año",
                choices = sort(unique(violencia_familiar_diario$año)),
                multiple = T)
  })
  
  
  output$violencia_familiar_month <- renderUI({
    selectInput("violencia_familiar_month",
                label =  "Seleccione el mes",
                choices = sort(unique(violencia_familiar_diario$month)),
                multiple = T)
  })
  
  output$violencia_familiar_sexo <- renderUI({
    selectInput("violencia_familiar_sexo",
                label =  "Selecciona el sexo",
                choices = sort(unique(violencia_familiar_diario$sexo)),
                multiple = T)
  })
  
  
  output$violencia_familiar_zona <- renderUI({
    selectInput("violencia_familiar_zona",
                label =  "Selecciona la zona",
                choices = sort(unique(violencia_familiar_diario$zona)),
                multiple = T)
  })
  
  
  #base reactiva para slide 3
  violencia_familiar_diario_reactive <- reactive({
    
    violencia_familiar_diario %>%
      filter(
        if(!is.null(input$violencia_familiar_año))             año %in% input$violencia_familiar_año          else año != "",
        if(!is.null(input$violencia_familiar_month))         month %in% input$violencia_familiar_month        else month != "",
        if(!is.null(input$violencia_familiar_zona))           zona %in% input$violencia_familiar_zona         else zona != "",
        if(!is.null(input$violencia_familiar_sexo))           sexo %in% input$violencia_familiar_sexo         else sexo != ""
      ) 
    
    
  })
  
  output$download_violencia_familiar <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(violencia_familiar, file, row.names = F)
      
    })
  
  # Width
  plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})
  
  # Height
  plotHeight <- function(){
    width <- plotWidth()
    h <- ifelse(width > 425, width*0.54, width*0.75)
    return(h)}
  
  # Font
  fontbase <- 8
  
  textFunction <- function(){
    width <- plotWidth()
    textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
    return(textSize)}
  
  
  output$grafico_violencia_familiar <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    violencia_familiar_diario_reactive() %>% 
      mutate(
        month=factor(
          month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                         "Septiembre", "Octubre","Noviembre", "Diciembre"))) %>%
      group_by(mes,sexo) %>% 
      summarise(registro=sum(registro),.groups = "drop") %>% 
      mutate(text = paste("Total de denuncias: ", scales::comma(registro), 
                          "\nPeríodo: ", mes,
                          "\nSexo del denunciante: ", sexo, sep="")) %>% 
      ggplot()+
      aes(x = as.factor(mes), y = registro, group=sexo, text=text) +
      geom_point(aes(fill=sexo, color = sexo), size=3)+
      geom_line (aes(fill=sexo, color = sexo), size=1) +
      scale_color_manual(values = c(
        Mujeres = "#B14C71",
        Hombres = "#8F5199"))+
      scale_fill_manual(values = c(
        Mujeres = "#B14C71",
        Hombres = "#8F5199"))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-03","2022-06","2022-09","2022-12", 
                                  "2023-03"))+
      labs(x="", y="Total de denuncias", fill="Sexo", color="Sexo")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Nutmeg-Light"),
            #plot.margin = margin(1, 1, 1, 1, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90, family="Nutmeg-Light"),
            plot.title = element_text(face="bold",size = 20L, hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10*textFunction(), family="Nutmeg-Light"))->grafico_violencia_familiar_1
    
    
    # #grafico_violencia_familiar
    ggplotly(grafico_violencia_familiar_1, tooltip = "text") %>%
      layout(title = list(text = paste0(#"Total de denuncias por violencia familiar \n",
        violencia_familiar_diario_reactive()$zona,
        '<br>',
        '<sup>')),
        margin = list(b=0,t=30),
        xaxis = list(side = "bottom"),
        legend = list(orientation = "h", x = 0.0, y = -0.5,
                      side="bottom"))
    
  })  
  
  
  
  
  
  
  medidas_reactive <- reactive({
    
    medidas_ordenes_municipal %>%
      filter(
        if(!is.null(input$medidas_año))             año %in% input$medidas_año               else año != "",
        if(!is.null(input$medidas_mes))             mes %in% input$medidas_mes               else mes != "",
        if(!is.null(input$medidas_municipio))       municipio %in% input$medidas_municipio   else municipio != ""
      )
    
  })
  
  
  
  
  output$downloadData_medidas <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(medidas_y_ordenes, file, row.names = F)
    }
  )
  
  
  output$medidasr_año <- renderUI({
    selectInput("medidas_año",
                label =  "Seleccione el año",
                choices = sort(unique(medidas_ordenes_municipal$año)),
                multiple = T)
  })
  
  output$medidasr_mes <- renderUI({
    selectInput("medidasr_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(medidas_ordenes_municipal$mes)),
                multiple = T)
  })
  
  
  output$medidas_municipio <- renderUI({
    selectInput("medidas_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(medidas_ordenes_municipal$municipio)),
                multiple = T)
  })
  
  
  
  # Width
  plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})
  
  # Height
  plotHeight <- function(){
    width <- plotWidth()
    h <- ifelse(width > 425, width*0.54, width*0.75)
    return(h)}
  
  # Font
  fontbase <- 8
  
  textFunction <- function(){
    width <- plotWidth()
    textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
    return(textSize)}
  
  
  output$grafico_medidas <- renderPlotly ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height  
    
    medidas_reactive() %>% 
      group_by(año, mes) %>% 
      summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
                ordenes=sum(ordenes_aceptadas + ordenes_rechazadas),.groups = "drop") %>% 
      pivot_longer(cols=c("ordenes","medidas"),
                   names_to = "tipo",
                   values_to = "total")%>% 
      mutate(fecha=case_when(
        mes=="Enero"~ "01", 
        mes=="Febrero"~"02", 
        mes=="Marzo"~"03", 
        mes=="Abril"~"04", 
        mes=="Mayo"~"05", 
        mes=="Junio"~"06",
        mes=="Julio"~"07", 
        mes=="Agosto"~"08", 
        mes=="Septiembre"~"09", 
        mes=="Octubre"~"10", 
        mes=="Noviembre"~"11", 
        mes=="Diciembre"~"12"),
        fecha=paste0(año,"-", fecha)) %>%  
      filter(tipo=="medidas") %>% 
      mutate(text = paste("Total de medidas: ", scales::comma(total), 
                          "\nPeríodo: ", fecha, sep="")) %>% 
      ggplot() +
      aes(x =fecha, y = total, color="#de1065", text=text) +
      #geom_col()+
      geom_point(color="#CB337F", size=3, alpha=0.7) + 
      geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+
      #geom_line(size=1)+
      # scale_fill_manual(
      #   values = c(#ordenes = "#B14C71"#,
      #     medidas ="#B14C71" #"#8F5199"
      #   ))+
      #   scale_color_manual(
      #     values = c(#ordenes = "#B14C71"#,
      #       medidas ="#B14C71" #"#8F5199"
      #     ))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-12","2022-03","2022-06", "2022-09",
                                            "2023-03","2023-06"))+
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(#title = paste("Total de medidas emitidas"),
        #violencia_familiar_diario_reactive()$municipio[1]),
        x="", y="", fill="Tipo", color="Tipo")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Nutmeg-Light"),
            #plot.margin = margin(1, 1, 1, 1, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))->gr_medidas
    
    ggplotly(gr_medidas, tooltip = "text") 
    # %>%
    #   layout(#legend = list(orientation = "h", x = 0.1, y = -0.3),
    #     margin = list(b=0,t=0),
    #     # title = paste("Total de medidas de protección emitidas \n",
    #     #               #medidas_reactive()$municipio,
    #     #               '</sup>',
    #     #               '<br>')
    #     )
  })  
  
  
  
  
  output$mapa_1 <- renderPlotly ({
    
    
    if (input$mapa_medidas == "Año 2019") {
      
      
      mxmunicipio_choropleth(medidas_2019, num_colors = 1,
                             zoom = subset(medidas_2019, state_name %in% 
                                             c("Jalisco"))$region,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#CB337F",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.5, 'cm'),
              legend.key.width= unit(.5, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2020") {
      
      mxmunicipio_choropleth(medidas_2020, num_colors = 1,
                             zoom = subset(medidas_2020, state_name %in% 
                                             c("Jalisco"))$region ,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#CB337F",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.5, 'cm'),
              legend.key.width= unit(.5, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2021") {
      
      mxmunicipio_choropleth(medidas_2021, num_colors = 1,
                             zoom = subset(medidas_2021, state_name %in% 
                                             c("Jalisco"))$region,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#CB337F",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2022") {
      
      mxmunicipio_choropleth(medidas_2022, num_colors = 1,
                             zoom = subset(medidas_2022, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x=" ", y=" ", title = paste0("Total de medidas de protección otorgadas: \n",'<br>','<sup>', input$mapa_medidas)) +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#CB337F",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.5, 'cm'),
              legend.key.width= unit(.5, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }  
    
    
    if (input$mapa_medidas == "Año 2023") {
      
      mxmunicipio_choropleth(medidas_2023, num_colors = 1,
                             zoom = subset(medidas_2023, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x=" ", y=" ", title = paste0("Total de medidas de protección otorgadas: \n",'<br>','<sup>', input$mapa_medidas)) +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#CB337F",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.5, 'cm'),
              legend.key.width= unit(.5, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }  
    
    ggplotly(mapa_1) %>%
      layout(title = list(text = paste0("Total de medidas de protección emitidas: \n", 
                                        input$mapa_medidas,
                                        '<br>',
                                        '<sup>')),
             
             margin = list(b=0, t=30), annotations =
               list(x =.67, y = -.27,
                    text = "",
                    #text = "   Datos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
    # layout(
    #    title = list(text = paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas 
    #                               )),
    #   margin = list(b=0,t=55))
    
    # add_annotations(
    #   yref="paper", 
    #   xref="paper", 
    #   y=0, 
    #   x=0, 
    #   text=paste0("Total de medidas de protección otorgadas: ", '</sup>',
    #               input$mapa_medidas),#"My Title", 
    #   showarrow=F, 
    #   font=list(size=13)
    # ) %>% 
    #   layout(title=FALSE)
    
  })  
  
  
  # ---------------------------------------------------------------------------
  
  output$table_medidas <- renderDataTable ({
  
      
      if (input$mapa_medidas == "Año 2019") {

        medidas_2019 %>% 
          select(año, municipio_name, total) %>%
          #filter(!total==0) %>% 
          arrange(-total) %>% 
          datatable(
            
            filter = 'top',
            colnames = c('Año',
                         'Municipio', 
                         'Total'), 
            
            extensions = 'Buttons',
            options = list(
              #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              language = list(
                info = ' ',
                paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#CB337F', 'color': '#fff', align:'center'});","}"),
              
              dom = "tip",#'Blfrtip',
              buttons = c('copy', 'excel', 'print'),
              lengthMenu = list(c(10,20,55,100, "All"),
                                c(10,20,55,100, "All")),
              columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
          
          formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
          
          #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
          #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
          formatStyle(
            columns = c(1:5),
            fontFamily = "Nutmeg-Light",
            #fontSize = "13px",
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
          ) ->datatable_1     
        }

      if (input$mapa_medidas == "Año 2020") {

        medidas_2020 %>%
          select(año, municipio_name, total) %>%
          #filter(!total==0) %>% 
          arrange(-total) %>% 
          datatable(
            
            filter = 'top',
            colnames = c('Año',
                         'Municipio', 
                         'Total'), 
            
            extensions = 'Buttons',
            options = list(
              #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              language = list(
                info = ' ',
                paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#CB337F', 'color': '#fff', align:'center'});","}"),
              
              dom = "tip",#'Blfrtip',
              buttons = c('copy', 'excel', 'print'),
              lengthMenu = list(c(10,20,55,100, "All"),
                                c(10,20,55,100, "All")),
              columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
          
          formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
          
          #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
          #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
          formatStyle(
            columns = c(1:5),
            fontFamily = "Nutmeg-Light",
            #fontSize = "13px",
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
          )->datatable_1 
        }

      if (input$mapa_medidas == "Año 2021") {

        medidas_2021 %>%
          select(año, municipio_name, total) %>%
          #filter(!total==0) %>% 
          arrange(-total) %>% 
          datatable(
            
            filter = 'top',
            colnames = c('Año',
                         'Municipio', 
                         'Total'), 
            
            extensions = 'Buttons',
            options = list(
              #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              language = list(
                info = ' ',
                paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#CB337F', 'color': '#fff', align:'center'});","}"),
              
              dom = "tip",#'Blfrtip',
              buttons = c('copy', 'excel', 'print'),
              lengthMenu = list(c(10,20,55,100, "All"),
                                c(10,20,55,100, "All")),
              columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
          
          formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
          
          #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
          #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
          formatStyle(
            columns = c(1:5),
            fontFamily = "Nutmeg-Light",
            #fontSize = "13px",
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
          )->datatable_1
      }

      if (input$mapa_medidas == "Año 2022") {

        medidas_2022 %>%
          select(año, municipio_name, total) %>%
            #filter(!total==0) %>% 
              arrange(-total) %>% 
              datatable(
                
                filter = 'top',
                colnames = c('Año',
                             'Municipio', 
                             'Total'), 
                
                extensions = 'Buttons',
                options = list(
                  #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  language = list(
                    info = ' ',
                    paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#CB337F', 'color': '#fff', align:'center'});","}"),
                  
                  dom = "tip",#'Blfrtip',
                  buttons = c('copy', 'excel', 'print'),
                  lengthMenu = list(c(10,20,55,100, "All"),
                                    c(10,20,55,100, "All")),
                  columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
              
              formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
              
              #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
              #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
              formatStyle(
                columns = c(1:5),
                fontFamily = "Nutmeg-Light",
                #fontSize = "13px",
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
              )->datatable_1
      }


      if (input$mapa_medidas == "Año 2023") {

        medidas_2023 %>%
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>%
        arrange(-total) %>%
        datatable(

          filter = 'top',
          colnames = c('Año',
                       'Municipio',
                       'Total'),

          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#CB337F', 'color': '#fff', align:'center'});","}"),

            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%

          formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>%

          #formatStyle('entidad', target = "row", fontWeight = "bold") %>%
          #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>%
          formatStyle(
            columns = c(1:5),
            fontFamily = "Nutmeg-Light",
            #fontSize = "13px",
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
          )->datatable_1

      }
      
datatable_1
    
    
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  
  
  
  
  ###############################################################################
  
  output$ordenesr_año <- renderUI({
    selectInput("ordenes_año",
                label =  "Seleccione el año",
                choices = sort(unique(medidas_ordenes_municipal$año)),
                multiple = T)
  })
  
  output$ordenesr_mes <- renderUI({
    selectInput("ordenesr_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(medidas_ordenes_municipal$mes)),
                multiple = T)
  })
  
  
  output$ordenes_municipio <- renderUI({
    selectInput("ordenes_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(medidas_ordenes_municipal$municipio)),
                multiple = T)
  })
  
  
  
  ordenes_reactive <- reactive({
    
    medidas_ordenes_municipal %>%
      filter(
        if(!is.null(input$ordenes_año))             año %in% input$ordenes_año               else año != "",
        if(!is.null(input$ordenes_mes))             mes %in% input$ordenes_mes               else mes != "",
        if(!is.null(input$ordenes_municipio))       municipio %in% input$ordenes_municipio   else municipio != ""
      )
    
  })
  
  
  output$downloadData_ordenes <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(medidas_y_ordenes, file, row.names = F)
    }
  )
  
  
  output$grafico_ordenes <- renderPlotly ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    ordenes_reactive() %>% 
      #medidas_ordenes_municipal %>% 
      group_by(año, mes) %>% 
      summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
                ordenes=sum(ordenes_aceptadas + ordenes_rechazadas),.groups = "drop") %>% 
      pivot_longer(cols=c("ordenes","medidas"),
                   names_to = "tipo",
                   values_to = "total")%>% 
      mutate(fecha=case_when(
        mes=="Enero"~ "01", 
        mes=="Febrero"~"02", 
        mes=="Marzo"~"03", 
        mes=="Abril"~"04", 
        mes=="Mayo"~"05", 
        mes=="Junio"~"06",
        mes=="Julio"~"07", 
        mes=="Agosto"~"08", 
        mes=="Septiembre"~"09", 
        mes=="Octubre"~"10", 
        mes=="Noviembre"~"11", 
        mes=="Diciembre"~"12"),
        fecha=paste0(año,"-", fecha)) %>%  
      filter(tipo=="ordenes") %>% 
      mutate(text = paste("Total de ordenes: ", scales::comma(total), 
                          "\nPeríodo: ", fecha, sep="")) %>% 
      ggplot() +
      aes(x =fecha, y = total, color="8F5199", text=text) +
      #geom_col()+
      geom_point(color="#8F5199", size=3, alpha=0.6) + 
      geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+#, color="#b24dd1")+
      #geom_line(size=1)+
      # scale_fill_manual(
      #   values = c(#ordenes = "#B14C71"#,
      #     medidas ="#B14C71" #"#8F5199"
      #   ))+
      #   scale_color_manual(
      #     values = c(#ordenes = "#B14C71"#,
      #       medidas ="#B14C71" #"#8F5199"
      #     ))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-12","2022-03","2022-06","2022-09",
                                            "2023-03", "2023-06"))+  
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(x="", y="Total", fill="Tipo", color="Tipo")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Nutmeg-Light"),
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9*textFunction()))->gr_ordenes
    
    ggplotly(gr_ordenes, tooltip = "text") 
    # %>%
    #   layout(#legend = list(orientation = "h", x = 0.1, y = -0.3),
    #     margin = list(b=0,t=70),
    #     title = paste("Total de órdenes de protección emitidas \n",
    #                   #medidas_reactive()$municipio,
    #                   '</sup>',
    #                   '<br>'))
  })  
  
  
  
  
  output$mapa_2 <- renderPlotly ({
    
    
    if (input$mapa_ordenes == "Año 2019") {
      
      mxmunicipio_choropleth(ordenes_2019, num_colors = 1,
                             zoom = subset(ordenes_2019, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#8F5199",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2020") {
      
      mxmunicipio_choropleth(ordenes_2020, num_colors = 1,
                             zoom = subset(ordenes_2020, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#8F5199",
          guide = "colourbar",
          label=comma)+        
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2021") {
      
      mxmunicipio_choropleth(ordenes_2021, num_colors = 1,
                             zoom = subset(ordenes_2021, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#8F5199",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2022") {
      
      mxmunicipio_choropleth(ordenes_2022, num_colors = 1,
                             zoom = subset(ordenes_2022, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x=" ", y=" ", 
             # title = paste0("Total de medidas de protección otorgadas: \n",'<br>','<sup>', 
             #                                                        input$mapa_medidas)
        ) +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#8F5199",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
      
    }
    
    if (input$mapa_ordenes == "Año 2023") {
      
      mxmunicipio_choropleth(ordenes_2023, num_colors = 1,
                             zoom = subset(ordenes_2023, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#8F5199",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Nutmeg-Light"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    
    
    ggplotly(mapa_2) %>%
      layout(title = list(text = paste0("Total de órdenes de protección emitidas: \n", 
                                        input$mapa_ordenes,
                                        '<br>',
                                        '<sup>')),
             
             margin = list(b=0, t=30), annotations =
               list(x =.67, y = -.27,
                    text = "",
                    #text = "   Datos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
    # layout(
    #    title = list(text = paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas 
    #                               )),
    #   margin = list(b=0,t=55))
    
    # add_annotations(
    #   yref="paper", 
    #   xref="paper", 
    #   y=0, 
    #   x=0, 
    #   text=paste0("Total de medidas de protección otorgadas: ", '</sup>',
    #               input$mapa_medidas),#"My Title", 
    #   showarrow=F, 
    #   font=list(size=13)
    # ) %>% 
    #   layout(title=FALSE)
    
  })  
  
  
  
  output$table_ordenes<- renderDataTable ({
    
    
    if (input$mapa_ordenes == "Año 2019") {
      
      ordenes_2019 %>% 
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>% 
        arrange(-total) %>% 
        datatable(
          
          filter = 'top',
          colnames = c('Año',
                       'Municipio', 
                       'Total'), 
          
          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
            
            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
        
        formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
        
        #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
        #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          #fontSize = "13px",
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
        ) ->datatable_2     
    }
    
    if (input$mapa_ordenes == "Año 2020") {
      
      ordenes_2020 %>%
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>% 
        arrange(-total) %>% 
        datatable(
          
          filter = 'top',
          colnames = c('Año',
                       'Municipio', 
                       'Total'), 
          
          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
            
            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
        
        formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
        
        #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
        #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          #fontSize = "13px",
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
        )->datatable_2
    }
    
    if (input$mapa_ordenes == "Año 2021") {
      
      ordenes_2021 %>%
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>% 
        arrange(-total) %>% 
        datatable(
          
          filter = 'top',
          colnames = c('Año',
                       'Municipio', 
                       'Total'), 
          
          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
            
            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
        
        formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
        
        #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
        #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          #fontSize = "13px",
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
        )->datatable_2
    }
    
    if (input$mapa_ordenes == "Año 2022") {
      
      ordenes_2022 %>%
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>% 
        arrange(-total) %>% 
        datatable(
          
          filter = 'top',
          colnames = c('Año',
                       'Municipio', 
                       'Total'), 
          
          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
            
            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
        
        formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
        
        #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
        #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          #fontSize = "13px",
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
        )->datatable_2
    }
    
    
    if (input$mapa_ordenes == "Año 2023") {
      
      ordenes_2023 %>%
        select(año, municipio_name, total) %>%
        #filter(!total==0) %>%
        arrange(-total) %>%
        datatable(
          
          filter = 'top',
          colnames = c('Año',
                       'Municipio',
                       'Total'),
          
          extensions = 'Buttons',
          options = list(
            #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
            language = list(
              info = ' ',
              paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
            
            dom = "tip",#'Blfrtip',
            buttons = c('copy', 'excel', 'print'),
            lengthMenu = list(c(10,20,55,100, "All"),
                              c(10,20,55,100, "All")),
            columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%
        
        formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>%
        
        #formatStyle('entidad', target = "row", fontWeight = "bold") %>%
        #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>%
        formatStyle(
          columns = c(1:5),
          fontFamily = "Nutmeg-Light",
          #fontSize = "13px",
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
        )->datatable_2
      
    }
    
    datatable_2
    
    
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  

  
  # ---------------------------------------------------------------------------
  
  output$table_muertes <- renderDataTable ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    victimas_reactive() %>% 
      #victimas %>% 
      group_by(Año, Subtipo.de.delito, Sexo) %>% 
      filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
             Sexo=="Mujer", Entidad=="Jalisco") %>% 
      summarise(ene=sum(Enero, na.rm = T),
                feb=sum(Febrero, na.rm = T),
                mar=sum(Marzo, na.rm = T),
                abr=sum(Abril, na.rm = T),
                may=sum(Mayo, na.rm = T),
                jun=sum(Junio, na.rm = T),
                jul=sum(Julio, na.rm = T),
                ago=sum(Agosto, na.rm = T),
                sep=sum(Septiembre, na.rm = T),
                oct=sum(Octubre, na.rm = T),
                nov=sum(Noviembre, na.rm = T),
                dic=sum(Diciembre, na.rm = T),
                value=sum(ene+feb+mar+abr+ 
                            may+jun+jul+ago+
                            sep+oct+nov+dic)) %>% 
      select(Año, Subtipo.de.delito, value) %>% 
      pivot_wider(names_from = "Subtipo.de.delito",
                  values_from = "value") %>% 
      summarise(
        feminicidio=Feminicidio,
        homicidio=`Homicidio doloso`,
        total=sum(Feminicidio + `Homicidio doloso`), .groups = "drop") %>% 
      #mutate(variación = total - lag(total)) %>% 
      mutate(variación = scales::percent((total - lag(total))/lag(total),0.1)) %>% 
      
      #select(Año, total, variación) %>% 
      arrange(-Año) %>% 
      datatable(
        
        filter = 'top',
        colnames = c('Año', 
                     'Feminicidios','Homicidios', 
                     'Total','Variación'), 
        
        extensions = 'Buttons',
        options = list(
          #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          language = list(
            info = ' ',
            paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#8F5199', 'color': '#fff', align:'center'});","}"),
          
          dom = "tip",#'Blfrtip',
          buttons = c('copy', 'excel', 'print'),
          lengthMenu = list(c(5,1,5,10, "All"),
                            c(5,1,5,10, "All")),
          columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>% 
      
      formatCurrency('total',currency = "", interval = 5, mark = ",", digits = 0) %>% 
      
      #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
      #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
      formatStyle(
        columns = c(1:5),
        fontFamily = "Nutmeg-Light",
        #fontSize = "13px",
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
    
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  output$victimas_año <- renderUI({
    selectInput("victimas_año",
                label =  "Seleccione el año",
                choices = sort(unique(victimas$Año)),
                multiple = T)
  })
  
  
  output$victimas_edad <- renderUI({
    selectInput("victimas_edad",
                label =  "Selecciona el municipio",
                choices = sort(unique(victimas$Rango.de.edad)),
                multiple = T)
  })
  
  
  output$victimas_delito <- renderUI({
    selectInput("victimas_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(victimas$Subtipo.de.delito)),
                multiple = T)
  })
  
  #base reactiva para slide 3
  victimas_reactive <- reactive({
    
    victimas %>%
      filter(
        if(!is.null(input$victimas_año))                    Año %in% input$victimas_año          else Año != "",
        if(!is.null(input$victimas_edad))        Rango.de.edad %in% input$victimas_edad   else Rango.de.edad != "",
        if(!is.null(input$victimas_delito))   Subtipo.de.delito %in% input$victimas_delito       else Subtipo.de.delito != ""
        
        
      )
    
  })
  
  output$downloadData_victimas <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(datos_victimas, file, row.names = F)
    }
  )
  
  output$grafico_victimas <- renderPlotly ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    victimas_reactive() %>% 
      # victimas %>% 
      group_by(Año, Subtipo.de.delito, Sexo) %>% 
      filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
             Sexo=="Mujer", Entidad=="Jalisco") %>% 
      summarise(ene=sum(Enero, na.rm = T),
                feb=sum(Febrero, na.rm = T),
                mar=sum(Marzo, na.rm = T),
                abr=sum(Abril, na.rm = T),
                may=sum(Mayo, na.rm = T),
                jun=sum(Junio, na.rm = T),
                jul=sum(Julio, na.rm = T),
                ago=sum(Agosto, na.rm = T),
                sep=sum(Septiembre, na.rm = T),
                oct=sum(Octubre, na.rm = T),
                nov=sum(Noviembre, na.rm = T),
                dic=sum(Diciembre, na.rm = T),
                value=sum(ene+feb+mar+abr+ 
                            may+jun+jul+ago+
                            sep+oct+nov+dic),.groups = "drop") %>% 
      select(Año, Subtipo.de.delito, value) %>% 
      mutate(text = paste("Total de víctimas del delito: ", scales::comma(value), 
                          "\nAño: ", Año,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>% 
      ggplot(aes(x=as.factor(Año), y=value, fill=Subtipo.de.delito, text=text))+
      geom_col(position="dodge2") +
      scale_fill_manual(values =
                          c(Feminicidio = "#B14C71",
                            `Homicidio doloso` = "#8F5199"))+
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="", fill="", color="")+
      theme_minimal()+
      theme(text=element_text(size=13,  family="Nutmeg-Light"),
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=2*textFunction()))->grafico_victimas
    
    ggplotly(grafico_victimas, tooltip = "text") %>%
      layout(
        xaxis = list(side = "bottom"),
        legend = list(orientation = "h", x = 0.1, y = -0.2,
                      side="bottom"),
        legend = list(orientation = "v", x = 0.1, y = -0.3),
        margin = list(b=0,t=0))
    #          title = paste0("Total de muertes violentas de mujeres", #victimas_reactive()$Rango.de.edad, 
    #                         '</sup>',
    #                         '<br>'))
    # 
    
  })  
  
  
  
  
  ######################################################################3
  
  output$Regional_Periodo <- renderUI({
    selectInput("Periodo",
                label =  "Seleccione tipo de Periodo",
                choices = sort(unique(Regiones$Periodo)))
  })
  
  
  output$Regional_delito <- renderUI({
    selectInput("Delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(Regiones$Subtipo.de.delito)))
  })
  
  output$Regional_Región <- renderUI({
    selectInput("Región",
                label =  "Seleccione alguna Región",
                choices = sort(unique(Regiones$Región)))
  })
  
  
  output$Regional_Año<- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(Regiones$Año)))
  })
  
  output$Regional_Mes <- renderUI({
    selectInput("Región",
                label =  "Seleccione algún mes",
                choices = sort(unique(Regiones$month)))
  })
  
  Regiones_data <- reactive({
    
    Regiones %>%
      filter(if(!is.null(input$Regional_Mes))                  month %in% input$Regional_Mes     else month != "",
             if(!is.null(input$Regional_Año))                  Año %in% input$Regional_Año     else Año != "",
             if(!is.null(input$Regional_delito)) Subtipo.de.delito %in% input$Regional_delito  else Subtipo.de.delito != "",
             if(!is.null(input$Regional_Región))            Región %in% input$Regional_Región  else Región != "")  
  })
  
  
  
  output$downloadData_regional <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(datos_incidencia_regional, file, row.names = F)
    })
  
  
  
  output$grafico_Regional<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    Regiones_data() %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(total), 
                          # "\nAño: ", Año,
                          "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nDelito: ", Subtipo.de.delito,
                          "\nRegión: ", Región, sep="")) %>% 
      ggplot()+ 
      aes(x=Periodo, y=total,
          fill=Subtipo.de.delito, colour = Subtipo.de.delito, group = Subtipo.de.delito,
          text=text)+
      geom_line(aes(x=Periodo, y=total),size=1) +
      geom_point(aes(x=Periodo, y=total), size=2.5)+
      labs(x="", y="Total", fill="", color=""
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      ) +
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values = mycolors)+
      scale_y_continuous(labels = scales::comma) + 
      # scale_x_discrete(breaks = c("2015-12","2015-03","2015-06","2015-09",
      #                             "2016-12","2016-03","2016-06","2016-09",
      #                             "2017-12","2017-03","2017-06","2017-09",
      #                             "2018-12","2018-03","2018-06","2018-09",
      #                             "2019-12","2019-03","2019-06","2019-09",
      #                             "2020-12","2020-03","2020-06","2020-09",
      #                             "2021-12","2021-03","2021-06","2021-09",
      #                             "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de carpetas iniciadas")+
      theme_minimal()+
      theme(text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11*textFunction()))->Regional
    
    ggplotly(Regional, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2),
             margin = list(b=0,t=30), 
             title = paste0( Regiones_data()$Región[1])
             # title = paste0(Regiones_data()$Subtipo.de.delito[1])
      )
    
    
    
  })
  
  # output$test <- renderText({
  #   head(Regiones_data()$Periodo)
  # })
  # 
  
  output$regional_anual<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    Regiones_data() %>% 
      #Regiones %>%   
      group_by(Año, Región, Subtipo.de.delito) %>% 
      summarise(Total=sum(total, na.rm = T)) %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(Total), 
                          "\nAño: ", Año,
                          "\nDelito: ", Subtipo.de.delito,
                          "\nRegión: ", Región, sep="")) %>% 
      ggplot() +
      aes(x =as.factor(Año), y = Total, fill= Subtipo.de.delito, text=text) +
      geom_col(position = "dodge")+
      labs(x="", y="Total de carpetas", fill="", color=""
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      )+
      scale_fill_manual(values = mycolors) +
      scale_y_continuous(labels = scales::comma) + 
      theme_minimal()+
      theme(text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11*textFunction()))->Regional_anual
    
    ggplotly(Regional_anual, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.1),
             margin = list(b=0,t=30),
             title = paste0(Regiones_data()$Región[1])
             # title = paste0(Regiones_data()$Subtipo.de.delito[1])
      )
    
    
    
  })
  
  
  ############################################################
  #                     MUNICIPAL
  ############################################################
  output$municipal_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(municipios$Año)))
    
  })
  output$municipal_mes <- renderUI({
    selectInput("Mes",
                label =  "Seleccione uno o varios mes",
                choices = sort(unique(municipios$month)))
    
  })
  output$municipal_delito <- renderUI({
    selectInput("municipal_delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(municipios$Subtipo.de.delito)))
  })
  output$municipal_municipio <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione uno o varios municipios",
                choices = sort(unique(municipios$Municipio)))
    
  })
  
  #######################################################################33
  
  
  
  municipios_data <- reactive({
    
    municipios %>%
      filter(if(!is.null(input$municipal_año))                    Año %in% input$municipal_año        else Año != "",
             if(!is.null(input$municipal_mes))                    month %in% input$municipal_mes        else month != "",
             if(!is.null(input$municipal_delito))   Subtipo.de.delito %in% input$municipal_delito     else Subtipo.de.delito != "",
             if(!is.null(input$municipal_municipio))         Municipio %in% input$municipal_municipio  else Municipio != "")
    
  })
  
  
  output$downloadData_municipal <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(datos_incidencia_mun, file, row.names = F)
    })
  
  output$grafico_municipal<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    municipios_data() %>% 
      # municipios %>% 
      #  filter(Subtipo.de.delito=="Violación") %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(Total), 
                          "\nAño: ", Año,
                          "\nMunicipio: ", Municipio,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>% 
      ggplot() +
      aes(x =as.factor(Año), y = Total, fill= Subtipo.de.delito, text=text) +
      geom_col(position = "dodge")+
      # geom_line(aes(x=Año, y=Total, colour=Municipio),size=1) +
      # geom_point(aes(x=Año, y=Total, colour=Municipio), size=3)+
      labs(x="", y="Total de carpetas", fill="", color="")+
      #scale_color_brewer(palette = "mycolors")+
      #scale_fill_brewer(palette = "mycolors")+      
      scale_fill_manual(values = mycolors129) +
      #scale_color_manual(values = mycolors129)+
      scale_y_continuous(labels = scales::comma) +
      
      theme_minimal()+
      theme_minimal()+
      theme(text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11*textFunction()))->municipal
    
    ggplotly(municipal, tooltip = "text") %>% 
      layout(title = paste0(#"Total de carpetas de investigación por delito \n" , 
        municipios_data()$Municipio[1]),
        legend = list(orientation = "h", x = 0.1, y = -0.1),
        margin = list(b=0,t=30))
    
    
  })
  
  
  
  output$grafico_municipal_periodo<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    
    
    municipios_data() %>%
      # municipios %>% 
      # filter(Subtipo.de.delito=="Violación") %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(total), 
                          "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nMunicipio: ", Municipio,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>% 
      ggplot()+ 
      aes(x=Periodo, y=total,
          fill=Subtipo.de.delito, colour = Subtipo.de.delito, 
          group = Subtipo.de.delito,
          text=text)+
      geom_line(aes(x=Periodo, y=total),size=1) +
      geom_point(aes(x=Periodo, y=total), size=2.5)+
      labs(x="", y="Total de carpetas"
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      ) +
      scale_fill_manual(values = mycolors129) +
      scale_color_manual(values = mycolors129)+
      scale_y_continuous(labels = scales::comma) + 
      # scale_x_discrete(breaks = c("2015-12","2015-03","2015-06","2015-09",
      #                             "2016-12","2016-03","2016-06","2016-09",
      #                             "2017-12","2017-03","2017-06","2017-09",
      #                             "2018-12","2018-03","2018-06","2018-09",
      #                             "2019-12","2019-03","2019-06","2019-09",
      #                             "2020-12","2020-03","2020-06","2020-09",
      #                             "2021-12","2021-03","2021-06","2021-09",
      #                             "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de carpetas iniciadas", fill="", color="")+
      theme_minimal()+
      theme(text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 12L, hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11*textFunction()))->municipal_periodo
    
    ggplotly(municipal_periodo, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.05, y = -0.2),
             margin = list(b=0,t=30), 
             title = paste0(#"Total de carpetas de investigación por delito \n" , 
               municipios_data()$Municipio[1])
      )
    
    
    
  })
  
  
  ################################################################################
  #                              ATENCIONES
  ################################################################################
  
  
  output$atencion_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$Año)))
  })
  
  output$atencion_mes <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año))           Año %in% input$atencion_año      else Año != "",
             if(!is.null(input$atencion_mes))           Mes %in% input$atencion_mes      else Mes != "",
             if(!is.null(input$atencion_tipo))         tipo %in% input$atencion_tipo     else tipo != "",
             if(!is.null(input$atencion_unidad))     Unidad %in% input$atencion_unidad   else Unidad != ""
      )
  })
  
  output$downloadData_atenciones_1 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(base_atenciones, file, row.names = F)
    })
  
  output$total_atenciones_grafico <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    atenciones_reactive() %>% 
      
      #    atenciones %>% 
      group_by(Fecha, Año) %>% 
      summarise(total=n())%>% 
      mutate(text = paste("Total de servicios brindados: ", scales::comma(total), 
                          "\nAño: ", Año,
                          "\nPeríodo: ", format(as_date(Fecha), "%B de %Y"), sep="")) %>% 
      ggplot() +
      aes(x =Fecha, y = total, text=text) +
      #geom_col()+
      geom_point(color="#CB337F", size=6, alpha=0.6) + 
      geom_segment(aes(x=Fecha, xend=Fecha, y=0, yend=total, color="#B14C71"))+
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Total de servicios brindados"),
           #violencia_familiar_diario_reactive()$municipio[1]),
           x="", y="", color="", fill="")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))->grafico_total_atenciones
    
    ggplotly(grafico_total_atenciones, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.05, y = -0.3),
             margin = list(b=0,t=0)#,
             #title = paste("Total de servicios brindados por violencias \n" 
             #,atenciones_reactive()$Unidad
             #)
      )
    
    
    
  }) 
  
  
  
  
  
  
  
  output$atencion_año2 <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$Año)))
  })
  
  output$atencion_mes2 <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo2 <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad2 <- renderUI({
    selectInput("Unidad",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive2 <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año2))           Año %in% input$atencion_año2      else Año != "",
             if(!is.null(input$atencion_mes2))           Mes %in% input$atencion_mes2      else Mes != "",
             if(!is.null(input$atencion_tipo2))         tipo %in% input$atencion_tipo2     else tipo != "",
             if(!is.null(input$atencion_unidad2))     Unidad %in% input$atencion_unidad2   else Unidad != ""
      )
  })
  
  
  output$downloadData_atenciones_2 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(base_atenciones, file, row.names = F)
    })
  
  
  
  
  
  output$atenciones_mensuales<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    atenciones_reactive2() %>% 
      
      filter(total >=0) %>% 
      group_by(tipo, Año)%>% 
      mutate(total= as.numeric(total)) %>% 
      summarise(total=sum(total, na.rm = T),.groups = "drop")%>% 
      mutate(text = paste("\nAño: ", Año,
                          "\nTotal de servicios: ", scales::comma(total), 
                          "\nTipo de violencia: ", tipo, sep="")) %>% 
      ggplot()+
      aes(x = reorder(tipo, total), y = total, fill=tipo, text=text) +
      geom_col() +
      scale_fill_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+  
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="")+
      coord_flip() +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11*textFunction(),  family="Nutmeg-Light"),
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12*textFunction(), face = "bold", angle=90),
            plot.tag = element_text(size = 15L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.title = element_text(size = 15L*textFunction(), hjust = 0.5, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9*textFunction()))->grafico_anteciones_mensuales
    
    ggplotly(grafico_anteciones_mensuales, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)#,
             # title = paste0("Total de servicios brindados por tipo de violencia",
             #                #,atenciones_reactive()$Unidad, 
             #                '</sup>',
             #                '<br>')
      )
    
  })
  
  
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  
  
  
  output$atencion_año3 <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$Año)))
  })
  
  output$atencion_mes3 <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo3 <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad3 <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive3 <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año3))           Año %in% input$atencion_año3      else Año != "",
             if(!is.null(input$atencion_mes3))           Mes %in% input$atencion_mes3      else Mes != "",
             if(!is.null(input$atencion_tipo3))         tipo %in% input$atencion_tipo3     else tipo != "",
             if(!is.null(input$atencion_unidad3))     Unidad %in% input$atencion_unidad3   else Unidad != ""
      )
  })
  
  
  output$downloadData_atenciones_3 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(base_atenciones, file, row.names = F)
    })
  
  output$downloadData_aborto <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos_ile.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(aborto, file, row.names = F)
    
  })
  
  output$downloadData_aborto2 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos_ile.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(aborto, file, row.names = F)
      
    })
  
  output$downloadData_aborto3 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos_ile.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(aborto, file, row.names = F)
      
    })
  
  output$downloadData_aborto4 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos_ile.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(aborto, file, row.names = F)
      
    })
  
  
  
  output$atenciones_tipo<- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    atenciones_reactive3() %>% 
      
      #atenciones %>% 
      filter(total >=0) %>% 
      group_by(Fecha,Año, tipo)%>% 
      mutate(total= as.numeric(total)) %>% 
      summarise(total=sum(total, na.rm = T),.groups = "drop")%>% 
      mutate(tipo=factor(tipo,
                         levels=c("Psicológica", "Económica", "Física", "Patrimonial", "Sexual", "Digital"))) %>% 
      filter(total !=0) %>% 
      mutate(text = paste("Total de atenciones: ", scales::comma(total), 
                          "\nTipo de violencia: ", tipo,
                          "\nAño: ", Año,
                          "\nPeríodo: ", format(as_date(Fecha), "%B de %Y"),  
                          sep="")) %>% 
      ggplot()+
      aes(x=Fecha, y=tipo, 
          size=total, text=text)+  
      geom_point(mapping=aes(colour=tipo))+
      geom_text(aes(label=total, accuracy = 1), size=2.5, color="ghostwhite")+
      scale_y_discrete(limits = rev , 
                       labels = function(x) str_wrap(x, width = 15)) + 
      scale_size_continuous(range = c(2,10)) +
      scale_fill_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+
      scale_color_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+
      labs(title="", 
           x="", y="")+
      theme_minimal()+
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"), 
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction()))->grafico_anteciones_tipo
    
    ggplotly(grafico_anteciones_tipo, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n", 
             #                #atenciones_reactive3()$tipo, 
             #                '</sup>',
             #                '<br>')
      )
    
    
    
  })
  
  data_aborto1 <- reactive({
    aborto %>% 
      filter(fecha>=min(input$date_aborto), fecha<=max(input$date_aborto), 
             if(is.null(input$causal_aborto)) causal!="" else causal %in% input$causal_aborto,
             if(is.null(input$hospital_aborto)) hospital!="" else hospital %in% input$hospital_aborto,
             if(is.null(input$redad_aborto)) rango_edad!="" else rango_edad %in% input$redad_aborto
             
             ) 
  
  })
  
  data_aborto2 <- reactive({
    aborto %>% 
      filter(fecha>=min(input$date_aborto2), fecha<=max(input$date_aborto2), 
             if(is.null(input$causal_aborto2)) causal!="" else causal %in% input$causal_aborto2,
             if(is.null(input$hospital_aborto2)) hospital!="" else hospital %in% input$hospital_aborto2,
             if(is.null(input$redad_aborto2)) rango_edad!="" else rango_edad %in% input$redad_aborto2
             
      ) 
    
  })
  
  data_aborto3 <- reactive({
    aborto %>% 
      filter(fecha>=min(input$date_aborto3), fecha<=max(input$date_aborto3), 
             if(is.null(input$causal_aborto3)) causal!="" else causal %in% input$causal_aborto3,
             if(is.null(input$hospital_aborto3)) hospital!="" else hospital %in% input$hospital_aborto3,
             if(is.null(input$redad_aborto3)) rango_edad!="" else rango_edad %in% input$redad_aborto3
             
      ) 
    
  })
  
  data_aborto4 <- reactive({
    aborto %>%
      filter(fecha>=min(input$date_aborto4), fecha<=max(input$date_aborto4),
             if(is.null(input$causal_aborto4)) causal!="" else causal %in% input$causal_aborto4,
             if(is.null(input$hospital_aborto4)) hospital!="" else hospital %in% input$hospital_aborto4,
             if(is.null(input$redad_aborto4)) rango_edad!="" else rango_edad %in% input$redad_aborto4

      )

  })

  output$aborto_ts <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    plot <- data_aborto1() %>%
      group_by(fecha=floor_date(fecha, "month")) %>%
      summarise(Total=n()) %>% ungroup() %>%
      complete(fecha=seq.Date(as_date(floor_date(min(input$date_aborto), "month")),
                              as_date(floor_date(max(input$date_aborto), "month")), "1 month"),
               fill=list(Total=0)
               ) %>%
      # mutate(text=paste0("Total: ", comma(Total))) %>%
      ggplot(aes(x=fecha, y=Total,
                 )) +
      geom_point(size=2, color="#6737ab", alpha=.7) +
      geom_line(size=1.2, color="#6737ab") + theme_minimal() +
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction()))

    ggplotly(plot
             ) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n", 
             #                #atenciones_reactive3()$tipo, 
             #                '</sup>',
             #                '<br>')
      )
    
  })
  
  output$aborto_procedimiento <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    plot <- data_aborto2() %>%
      group_by(procedimiento) %>%
      summarise(Total=n()) %>% ungroup() %>%
      # mutate(text=paste0("Total: ", comma(Total))) %>%
      ggplot(aes(x=reorder(procedimiento, -Total), y=Total,
                 text=paste0("Procemiento: ", procedimiento, 
                                  "<br>Total: ", Total)
      )) +
      geom_col(size=2, fill="#6737ab") + theme_minimal() +
      labs(x="Procedimiento") +
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction()))
    
    ggplotly(plot, tooltip = "text"
    ) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n", 
             #                #atenciones_reactive3()$tipo, 
             #                '</sup>',
             #                '<br>')
      )
    
  })
  
  
  output$aborto_hospitales <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    plot <- data_aborto3() %>%
      group_by(hospital) %>%
      summarise(Total=n()) %>% ungroup() %>%
      # mutate(text=paste0("Total: ", comma(Total))) %>%
      ggplot(aes(x=reorder(hospital, Total), y=Total, 
                 text=paste0("Hospital: ", hospital, 
                             "<br>Total: ", Total)
      )) +
      geom_col(size=2, fill="#D581B9") + theme_minimal() +
      labs(x="Hospital") +
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction())) +
      coord_flip()
    
    ggplotly(plot, tooltip = "text"
    ) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n", 
             #                #atenciones_reactive3()$tipo, 
             #                '</sup>',
             #                '<br>')
      )
    
  })
  
  output$aborto_map <- renderLeaflet({
    
    awesome <- makeAwesomeIcon(
      icon = "plus",
      # iconColor = "black",
      markerColor = "purple",
      library = "fa"
    )
    
    data_aborto3() %>%
      filter(!hospital %in% c("Sin información", "Otro",
                             "Otra Instancia particular")) %>% 
      group_by(hospital) %>%
      summarise(Total=n()) %>% 
      full_join(coord_hospital, "hospital") %>% 
      replace_na(list(Total=0)) %>% 
      sf::st_as_sf(coords=c("x", "y")) %>% 
      leaflet() %>% addTiles() %>% 
      addAwesomeMarkers(popup = ~paste0(
        "Hospital: ", hospital, "</br>", 
        "Total: ", Total
      ), icon=awesome)
    
  })
  
  output$aborto_ts_anual <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    plot <- total_aborto %>%
      # mutate(text=paste0("Total: ", comma(Total))) %>%
      ggplot(aes(x=ao, y=Total, text=paste0("Año: ", ao, 
                                            "<br>Total : ", Total)
      )) +
      geom_col(size=2, fill="purple") + theme_minimal() +
      # geom_line(size=1.2, color="purple") +
      labs(x="Año") +
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction())) 
    
    ggplotly(plot, tooltip = "text"
    ) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n", 
             #                #atenciones_reactive3()$tipo, 
             #                '</sup>',
             #                '<br>')
      )
    
  })
  
  output$aborto_gestacion <- renderPlotly({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height

    plot <- data_aborto4() %>%
      # mutate(text=paste0("Total: ", comma(Total))) %>%
      group_by(rango_sgd) %>% summarise(Total=n()) %>% 
      ggplot(aes(x=reorder(rango_sgd, -Total), y=Total,
                 text=paste0("Semanas de gestación: ", rango_sgd, 
                             "<br>Total: ", Total)
      )) +
      geom_col(size=2, fill="#D581B9") + theme_minimal() +
      labs(x="Semanas de gestación") +
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14*textFunction()),
            legend.text = element_text(size=12*textFunction()),
            text=element_text(size=12*textFunction(), family="Nutmeg-Light"),
            plot.title = element_text(size = 18L*textFunction(), hjust = 0, family="Nutmeg-Light"),
            plot.caption = element_text(size = 12L*textFunction(), hjust = 0),
            strip.text.x = element_text(size = 11*textFunction(), color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10*textFunction()))

    ggplotly(plot, tooltip = "text"
    ) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=0)
             # title = paste0("Total de servicios brindados por tipo de violencia\n",
             #                #atenciones_reactive3()$tipo,
             #                '</sup>',
             #                '<br>')
      )

  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
