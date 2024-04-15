library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)

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
library(shiny)
library(shiny.router)

nb.cols <- 25
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


# Indicador 1: ----------------------------------------------------------------#
indicador_1 <- read_excel("indicadores_ijcf.xlsx",sheet = "Ind 1")%>% suppressWarnings()

indicador_1$Fecha <- format(as.Date(indicador_1$Fecha, format = "%d/%m/%Y"))
#indicador_1$Mes <- format(as.Date(indicador_1$Fecha, format="%Y-%m-%d"), "%B")
indicador_1$Periodo <- format(as.Date(indicador_1$Fecha, format="%Y-%m-%d"), "%Y-%m")


indicador_1 %>%
  filter(`Muerte violenta`=="Violenta") %>%
  mutate(Mes = case_when(
    Mes == 1 ~ "Enero",
    Mes == 2 ~ "Febrero",
    Mes == 3 ~ "Marzo",
    Mes == 4 ~ "Abril",
    Mes == 5 ~ "Mayo",
    Mes == 6 ~ "Junio",
    Mes == 7 ~ "Julio",
    Mes == 8 ~ "Agosto",
    Mes == 9 ~ "Septiembre",
    Mes == 10 ~ "Octubre",
    Mes == 11 ~ "Noviembre",
    Mes == 12 ~ "Diciembre",
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")))->indicador_1

indicador_1 %>% 
  mutate(`Total de muertes violentas`=case_when(
    `Muerte violenta`=="Violenta"~1, T~0)) %>% 
  pivot_longer(cols = 7:12,
               names_to = "Servicios forenses",
               values_to = "Aplicado")->indicador_1


id_violenta <- indicador_1 %>% 
  filter(!duplicated(Folio)) %>% pull(Folio)

# Indicador 2: ----------------------------------------------------------------#

indicador_2 <- read_excel("indicadores_ijcf.xlsx",sheet = "Ind 2")%>% suppressWarnings

indicador_2$Fecha   <- format(as.Date(indicador_2$Fecha, format = "%d/%m/%Y"))
#indicador_2$Mes     <- format(as.Date(indicador_2$Fecha, format="%Y-%m-%d"), "%B")
indicador_2$Periodo <- format(as.Date(indicador_2$Fecha, format="%Y-%m-%d"), "%Y-%m")

indicador_2 %>%
  filter(
    Folio %in% id_violenta,
    !Año=="NA") %>%
  mutate(Mes = case_when(
    Mes == 1 ~ "Enero",
    Mes == 2 ~ "Febrero",
    Mes == 3 ~ "Marzo",
    Mes == 4 ~ "Abril",
    Mes == 5 ~ "Mayo",
    Mes == 6 ~ "Junio",
    Mes == 7 ~ "Julio",
    Mes == 8 ~ "Agosto",
    Mes == 9 ~ "Septiembre",
    Mes == 10 ~ "Octubre",
    Mes == 11 ~ "Noviembre",
    Mes == 12 ~ "Diciembre",
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  `Total de muertes violentas`= c(1))->indicador_2





# Indicador 3: ----------------------------------------------------------------#
indicador_3 <- read_excel("indicadores_ijcf.xlsx",sheet = "Ind 3")%>% suppressWarnings()

indicador_3$Fecha   <- format(as.Date(indicador_3$Fecha, format = "%d/%m/%Y"))
# indicador_3$Mes     <- format(as.Date(indicador_3$Fecha, format="%Y-%m-%d"), "%B")
indicador_3$Periodo <- format(as.Date(indicador_3$Fecha, format="%Y-%m-%d"), "%Y-%m")

indicador_3 %>% 
  filter(!Año=="NA",
         !`Procesamiento del lugar de los hechos (levantamiento de cadáver, indicios y fotografía)` %in% c("Imp.", "Inf.", NA, "NA"),
         !`Toma de huellas decadactilares` %in% c("Imp.", "Inf.", NA, "NA"),
         Folio %in% id_violenta) %>% 
  
  mutate(
    Mes = case_when(
      Mes == 1 ~ "Enero",
      Mes == 2 ~ "Febrero",
      Mes == 3 ~ "Marzo",
      Mes == 4 ~ "Abril",
      Mes == 5 ~ "Mayo",
      Mes == 6 ~ "Junio",
      Mes == 7 ~ "Julio",
      Mes == 8 ~ "Agosto",
      Mes == 9 ~ "Septiembre",
      Mes == 10 ~ "Octubre",
      Mes == 11 ~ "Noviembre",
      Mes == 12 ~ "Diciembre",
    ),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    `Procesamiento del lugar de los hechos (levantamiento de cadáver, indicios y fotografía)` = as.numeric(`Procesamiento del lugar de los hechos (levantamiento de cadáver, indicios y fotografía)`),
    `Toma de huellas decadactilares` = as.numeric(`Toma de huellas decadactilares`),
    `Total de muertes violentas`= c(1)) ->indicador_3

indicador_3 <- indicador_3 %>% 
  filter(Folio %in% id_violenta)

# Indicador 4: ----------------------------------------------------------------#
indicador_4 <- read_excel("indicadores_ijcf.xlsx",sheet = "Ind 4")%>% suppressWarnings()


indicador_4$Fecha   <- format(as.Date(indicador_4$Fecha, format = "%d/%m/%Y"))
# indicador_4$Mes     <- format(as.Date(indicador_4$Fecha, format="%Y-%m-%d"), "%B")
indicador_4$Periodo <- format(as.Date(indicador_4$Fecha, format="%Y-%m-%d"), "%Y-%m")

indicador_4 %>%
  filter(Folio %in% id_violenta) %>% 
  filter(!Año=="NA") %>%
  mutate(Mes = case_when(
    Mes == 1 ~ "Enero",
    Mes == 2 ~ "Febrero",
    Mes == 3 ~ "Marzo",
    Mes == 4 ~ "Abril",
    Mes == 5 ~ "Mayo",
    Mes == 6 ~ "Junio",
    Mes == 7 ~ "Julio",
    Mes == 8 ~ "Agosto",
    Mes == 9 ~ "Septiembre",
    Mes == 10 ~ "Octubre",
    Mes == 11 ~ "Noviembre",
    Mes == 12 ~ "Diciembre"),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    `Total de muertes violentas`= c(1))->indicador_4






# Indicador 6: ----------------------------------------------------------------#
indicador_6 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 6")%>% suppressWarnings()

indicador_6 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_6


indicador_6$Fecha   <- format(as.Date(indicador_6$Periodo, format = "%d-%m-%Y"))
indicador_6$Fecha   <- format(as.Date(indicador_6$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_6$Fecha   <- as.Date(indicador_6$Periodo, format = "%d-%m-%Y")



indicador_6 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Mujeres víctimas de violencia de género`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
            `Medidas de protección aceptadas`=sum(`Medida de protección aceptada`),
            `Medidas de protección rechazadas`=sum(`Medida de protección rechazada`),
            `Órdenes de protección aceptadas`=sum(`Orden de protección aceptada`),
            `Órdenes de protección rechazadas`=sum(`Orden de protección rechazada`))->estatal_6

indicador_6 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Mujeres víctimas de violencia de género`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
            `Medidas de protección aceptadas`=sum(`Medida de protección aceptada`),
            `Medidas de protección rechazadas`=sum(`Medida de protección rechazada`),
            `Órdenes de protección aceptadas`=sum(`Orden de protección aceptada`),
            `Órdenes de protección rechazadas`=sum(`Orden de protección rechazada`))->municipal_6


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_6)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_6)->indicador_6


# Indicador 7: ----------------------------------------------------------------#
indicador_7 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 7")%>% suppressWarnings()

indicador_7 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_7


indicador_7$Fecha   <- format(as.Date(indicador_7$Periodo, format = "%d-%m-%Y"))
indicador_7$Fecha   <- format(as.Date(indicador_7$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_7$Fecha   <- as.Date(indicador_7$Periodo, format = "%d-%m-%Y")



indicador_7 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total de mujeres víctimas de violencia de género atendidas`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
            `Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`),
            `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`),
            `Indicador`=scales::percent(sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal` + `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`)/`Total de mujeres víctimas de violencia de género atendidas`))->estatal_7

indicador_7 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total de mujeres víctimas de violencia de género atendidas`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
            `Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`),
            `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`),
            `Indicador`=scales::percent(sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal` + `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`)
                                        /`Total de mujeres víctimas de violencia de género atendidas`)) ->municipal_7


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_7)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_7)->indicador_7

# Indicador 9: ----------------------------------------------------------------#
indicador_9 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 9")%>% suppressWarnings()

indicador_9 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_9


indicador_9$Fecha   <- format(as.Date(indicador_9$Periodo, format = "%d-%m-%Y"))
indicador_9$Fecha   <- format(as.Date(indicador_9$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_9$Fecha   <- as.Date(indicador_9$Periodo, format = "%d-%m-%Y")



indicador_9 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total de medidas de protección emitidas por violencia por razón de género vigentes`=sum(`Total de medidas de protección emitidas por violencia por razón de género vigentes`),
            `Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`=sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`),
            `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`),
            `Indicador`=scales::percent(sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`+ `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)/
                                          `Total de medidas de protección emitidas por violencia por razón de género vigentes`, 0.1))->estatal_9

indicador_9 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total de medidas de protección emitidas por violencia por razón de género vigentes`=sum(`Total de medidas de protección emitidas por violencia por razón de género vigentes`),
            `Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`=sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`),
            `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`),
            `Indicador`=scales::percent(sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`+ `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)/
                                          `Total de medidas de protección emitidas por violencia por razón de género vigentes`, 0.1))->municipal_9


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_9)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_9)->indicador_9


# Indicador 10: ----------------------------------------------------------------#
indicador_10 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 10")%>% suppressWarnings()

indicador_10 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_10


indicador_10$Fecha   <- format(as.Date(indicador_10$Periodo, format = "%d-%m-%Y"))
indicador_10$Fecha   <- format(as.Date(indicador_10$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_10$Fecha   <- as.Date(indicador_10$Periodo, format = "%d-%m-%Y")



indicador_10 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total de órdenes de protección emitidas por violencia por razón de género`=sum(`Total de órdenes de protección emitidas por violencia por razón de género`),
            
            `Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`),
            
            `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=
              sum(`Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)) ->estatal_10

indicador_10 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total de órdenes de protección emitidas por violencia por razón de género`=sum(`Total de órdenes de protección emitidas por violencia por razón de género`),
            
            `Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`),
            
            `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=
              sum(`Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)) ->municipal_10


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_10)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_10)->indicador_10


# Indicador 11: ----------------------------------------------------------------#
indicador_11 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 11")%>% suppressWarnings()

indicador_11 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_11


indicador_11$Fecha   <- format(as.Date(indicador_11$Periodo, format = "%d-%m-%Y"))
indicador_11$Fecha   <- format(as.Date(indicador_11$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_11$Fecha   <- as.Date(indicador_11$Periodo, format = "%d-%m-%Y")



indicador_11 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Mujeres con medidas de protección vigentes que han recibido seguimiento`=sum(`Mujeres con medidas de protección vigentes que han recibido seguimiento`),
            
            `Mujeres con órdenes de protección vigentes que han recibido seguimiento`=sum(`Mujeres con órdenes de protección vigentes que han recibido seguimiento`),
            
            `Total de mujeres con medidas de protección vigentes`=sum(`Total de mujeres con medidas de protección vigentes`),
            
            `Total de mujeres con órdenes de protección vigentes`=sum(`Total de mujeres con órdenes de protección vigentes`))->estatal_11

indicador_11 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Mujeres con medidas de protección vigentes que han recibido seguimiento`=sum(`Mujeres con medidas de protección vigentes que han recibido seguimiento`),
            
            `Mujeres con órdenes de protección vigentes que han recibido seguimiento`=sum(`Mujeres con órdenes de protección vigentes que han recibido seguimiento`),
            
            `Total de mujeres con medidas de protección vigentes`=sum(`Total de mujeres con medidas de protección vigentes`),
            
            `Total de mujeres con órdenes de protección vigentes`=sum(`Total de mujeres con órdenes de protección vigentes`))->municipal_11


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_11)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_11)->indicador_11

# Indicador 12: ----------------------------------------------------------------#
indicador_12 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 12")%>% suppressWarnings()

indicador_12 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_12


indicador_12$Fecha   <- format(as.Date(indicador_12$Periodo, format = "%d-%m-%Y"))
indicador_12$Fecha   <- format(as.Date(indicador_12$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_12$Fecha   <- as.Date(indicador_12$Periodo, format = "%d-%m-%Y")



indicador_12 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total de medidas de protección emitidas vigentes`=sum(`Total de medidas de protección emitidas vigentes`),
            `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`),
            
            `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
            
            `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`))->estatal_12

indicador_12 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total de medidas de protección emitidas vigentes`=sum(`Total de medidas de protección emitidas vigentes`),
            `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`),
            
            `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
            
            `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`))->municipal_12


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_12)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_12)->indicador_12





# Indicador 13: ----------------------------------------------------------------#
indicador_13 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 13")%>% suppressWarnings()

indicador_13 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_13


indicador_13$Fecha   <- format(as.Date(indicador_13$Periodo, format = "%d-%m-%Y"))
indicador_13$Fecha   <- format(as.Date(indicador_13$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_13$Fecha   <- as.Date(indicador_13$Periodo, format = "%d-%m-%Y")



indicador_13 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total medidas de protección emitidas vigentes`=sum(`Total medidas de protección emitidas vigentes`),
            `Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
            `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
            `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
            `Indicador`=scales::percent(sum((`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`+ `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`)/(`Total medidas de protección emitidas vigentes` +`Total de órdenes de protección emitidas vigentes`)-1)*-1, 0.1))->estatal_13

indicador_13 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total medidas de protección emitidas vigentes`=sum(`Total medidas de protección emitidas vigentes`),
            `Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
            `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
            `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
            `Indicador`=scales::percent(sum((`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`+ `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`)/(`Total medidas de protección emitidas vigentes` +`Total de órdenes de protección emitidas vigentes`)-1)*-1, 0.1))->municipal_13


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_13)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_13)->indicador_13


# Indicador 16: ----------------------------------------------------------------#
indicador_16 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 16")%>% suppressWarnings()

indicador_16 %>%
  mutate(Fecha=case_when(
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
    # Trimestre = case_when(
    #     Mes == "Enero" ~ "ene - mar",
    #     Mes == "Febrero" ~ "ene - mar",
    #     Mes == "Marzo" ~ "ene - mar",
    #     Mes == "Abril" ~ "abr - jun",
    #     Mes == "Mayo" ~ "abr - jun",
    #     Mes == "Junio" ~ "abr - jun",
    #     Mes == "Julio" ~ "jul - sep",
    #     Mes == "Agosto" ~ "jul - sep",
    #     Mes == "Septiembre" ~ "jul - sep",
    #     Mes == "Octubre" ~ "oct - dic",
    #     Mes == "Noviembre" ~ "oct - dic",
    #     Mes == "Diciembre" ~ "oct - dic"),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    #  Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
    `Rango de edad`=factor(`Rango de edad`, levels = c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                                                       "36 a 45 años", "46 a 59 años", "60 en adelante", "No especifica")),
    Periodo = ymd(paste0(Año, "-", Fecha, "-01"))#,
    #Trimestre = paste0(Año, " ", Trimestre)
  ) ->indicador_16


# indicador_16$Fecha   <- format(as.Date(indicador_16$Periodo, format = "%d-%m-%Y"))
# indicador_16$Fecha   <- format(as.Date(indicador_16$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_16$Fecha<- as.character(indicador_16$Fecha)


# Indicador 17: ----------------------------------------------------------------#
indicador_17 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 17")%>% suppressWarnings()

# indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`<-as.Date(indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`,format="%d/%m/%y")
# indicador_17$Mes     <-format(as.Date(indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`, format="%Y-%m-%d"), "%B")
# indicador_17$Month     <-format(as.Date(indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`, format="%Y-%m-%d"), "%m")

# indicador_17$Periodo <-format(as.Date(indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`, format="%Y-%m-%d"), "%Y-%m-%d")

#indicador_17$Periodo <- indicador_17$`Fecha de referidas por fiscalía (dd-mm-aa)`

indicador_17 %>% 
  filter(Año >=2019) %>% 
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
  Rango= case_when(
    `Edad (0-99)` <= 2 ~ "0 a 2 años",
    `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
    `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
    `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
    `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
    `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
    `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
    `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
    `Edad (0-99)` >= 60  ~ "60 en adelante"),
  Rango=factor(Rango,
               levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                        "36 a 45 años", "46 a 59 años", "60 en adelante")))->indicador_17


indicador_17$`Tipo: (abuso sexual infantil / violación)`[indicador_17$`Tipo: (abuso sexual infantil / violación)`=="ABUSO SEXUAL INTANTIL"]<- "Abuso sexual infantil"
indicador_17$`Tipo: (abuso sexual infantil / violación)`[indicador_17$`Tipo: (abuso sexual infantil / violación)`=="VIOLACIÓN"]<- "Violación"      
# indicador_17$`Tipo: (abuso sexual infantil / violación)`[indicador_17$`Tipo: (abuso sexual infantil / violación)`=="VIOLENCIA SEXUAL"]<- "Violencia sexual"


# indicador_17$Fecha   <- format(as.Date(indicador_17$Periodo, format = "%d-%m-%Y"))
# indicador_17$Fecha   <- format(as.Date(indicador_17$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_17$Fecha   <- as.Date(indicador_17$Periodo, format = "%d-%m-%Y")
# 
# indicador_17$Fecha<-as.character(indicador_17$Periodo)


# Indicador 18: ----------------------------------------------------------------#
indicador_18 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 18")%>% suppressWarnings()

# 
# indicador_18$`Fecha (dd-mm-aa)`<-as.Date(indicador_18$`Fecha (dd-mm-aa)`,format="%d/%m/%Y")
# indicador_18$Año<-format(as.Date(indicador_18$`Fecha (dd-mm-aa)` , format="%d/%m/%Y"), "%Y")
# indicador_18$Mes<-format(as.Date(indicador_18$`Fecha (dd-mm-aa)` , format="%d/%m/%Y"), "%B")
# indicador_18$Month  <-format(as.Date(indicador_18$`Fecha (dd-mm-aa)`, format="%Y-%m-%d"), "%m")



indicador_18 %>% 
  # mutate(Fecha=case_when(
  #   Mes=="Enero" ~ 1,
  #   Mes=="Febrero" ~ 2,
  #   Mes=="Marzo" ~ 3,
  #   Mes=="Abril" ~ 4,
  #   Mes=="Mayo" ~ 5,
  #   Mes=="Junio" ~ 6,
  #   Mes=="Julio" ~ 7,
  #   Mes=="Agosto" ~ 8,
  #   Mes=="Septiembre" ~ 9,
  #   Mes=="Octubre" ~ 10,
#   Mes=="Noviembre" ~ 11,
#   Mes=="Diciembre" ~ 12,
#   
# ),
# Mes=factor(Mes,
#            levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                     "Septiembre", "Octubre","Noviembre", "Diciembre")),
# Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
mutate(
  Rango= case_when(
    `Edad (0-99)` <= 2 ~ "0 a 2 años",
    `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
    `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
    `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
    `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
    `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
    `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
    `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
    `Edad (0-99)` >= 60  ~ "60 en adelante"),
  Rango=factor(Rango,
               levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                        "36 a 45 años", "46 a 59 años", "60 en adelante")))->indicador_18


# indicador_18$Fecha<-as.character(indicador_18$Periodo)

indicador_18$`Causal: (violacion/ salud/ riesgo)`[indicador_18$`Causal: (violacion/ salud/ riesgo)`=="Violación"]<- "Violación"
indicador_18$`Causal: (violacion/ salud/ riesgo)`[indicador_18$`Causal: (violacion/ salud/ riesgo)`=="Salud|SALUD|salud"]<- "Violación"


# indicador_18$Fecha   <- format(as.Date(indicador_18$Periodo, format = "%d-%m-%Y"))
# indicador_18$Fecha   <- format(as.Date(indicador_18$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_18$Fecha   <- as.Date(indicador_18$Periodo, format = "%d-%m-%Y")



# Indicador 19: ----------------------------------------------------------------#
indicador_19 <- read_excel("indicadores_salud.xlsx",sheet = "Ind 19")%>% suppressWarnings()

# indicador_19$Fecha   <- format(as.Date(indicador_19$`Fecha (dd-mm-aa)` , format = "%d/%m/%Y"))
# indicador_19$Año     <- format(as.Date(indicador_19$`Fecha (dd-mm-aa)`, format="%Y-%m-%d"), "%Y")
# indicador_19$Mes     <- format(as.Date(indicador_19$`Fecha (dd-mm-aa)`, format="%Y-%m-%d"), "%B")
# indicador_19$Month   <-format(as.Date(indicador_19$`Fecha (dd-mm-aa)`, format="%Y-%m-%d"), "%m")
# 

#indicador_19$Periodo <- format(as.Date(indicador_19$`Fecha (dd-mm-aa)`, format="%Y-%m-%d"), "%Y-%m")

indicador_19 %>%
  # filter(Año >= 2021) %>%
  mutate(
    # Fecha=case_when(
    #   Mes=="Enero" ~ 1,
    #   Mes=="Febrero" ~ 2,
    #   Mes=="Marzo" ~ 3,
    #   Mes=="Abril" ~ 4,
    #   Mes=="Mayo" ~ 5,
    #   Mes=="Junio" ~ 6,
    #   Mes=="Julio" ~ 7,
    #   Mes=="Agosto" ~ 8,
    #   Mes=="Septiembre" ~ 9,
    #   Mes=="Octubre" ~ 10,
    #   Mes=="Noviembre" ~ 11,
    #   Mes=="Diciembre" ~ 12),
    # Mes=factor(Mes,
    #            levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
    #                     "Septiembre", "Octubre","Noviembre", "Diciembre")),
    # Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    `Causal: (salud/riesgo)`= case_when(
      `Causal: (salud/riesgo)`== "Salud" ~ "Daño a la salud",
      `Causal: (salud/riesgo)`== "Causal no legal" ~ "Causal no legal",
      `Causal: (salud/riesgo)`== "Violación" ~ "Causal de violación"),
    
    # 
    # `Causal: (salud/riesgo)`== "4.Riesgo a la salud" ~ "Daño a la salud",
    # `Causal: (salud/riesgo)`== "5.Riesgo de muerte" ~ "Peligro de muerte",
    # `Causal: (salud/riesgo)`== "1. Violación Sexual (IVE)" ~ "Causal de violación",
    # `Causal: (salud/riesgo)`== "4.       Grave daño salud"  ~ "Daño a la salud",
    # `Causal: (salud/riesgo)`== "5.       Peligro de muerte" ~ "Peligro de muerte",
    # 
    # `Causal: (salud/riesgo)`== "4.       Grave daño salud"  ~ "Daño a la salud",
    # `Causal: (salud/riesgo)`== "5.       Peligro de muerte" ~ "Peligro de muerte"),
    Rango= case_when(
      `Edad (0-99)` <= 2 ~ "0 a 2 años",
      `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
      `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
      `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
      `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
      `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
      `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
      `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
      `Edad (0-99)` >= 60  ~ "60 en adelante"),
    Rango=factor(Rango,
                 levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                          "36 a 45 años", "46 a 59 años", "60 en adelante")),
    `¿Se realizó el procedimiento? (sí/no)`=case_when(
      `¿Se realizó el procedimiento? (sí/no)`== "NA"~0,
      `¿Se realizó el procedimiento? (sí/no)`== "SI"~1))->indicador_19




# Indicador 20: ----------------------------------------------------------------#
indicador_20 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 20")%>% suppressWarnings()

indicador_20 %>% 
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    
    `Notificadas al mp: (si/no)` = case_when(
      `Notificadas al mp: (si/no)`== "SI" ~ 1,
      `Notificadas al mp: (si/no)`== "NO" ~ 0,
      T~0),
    `Tipo de violencia: (violencia familiar / sexual)`=case_when(
      str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "psic|PSIC|Psic") ~ "Violencia psicológica", 
      str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "eco|ECO|Eco") ~ "Violencia económica",
      str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "fami|FAMI|Fami") ~ "Violencia familiar",
      str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "física|Física|FÍSICA|FISICA|Fisica|fisica") ~ "Violencia física",
      str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "sex|SEX|Sex") ~ "Violencia sexual",
      T~"Otro tipo"))-> indicador_20

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="SE IGNORA"] <- "NO ESPECIFICADO"

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`== NA] <- "NO ESPECIFICADO"

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="NA"] <- "NO ESPECIFICADO"

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="LUGAR No especificado"] <- "NO ESPECIFICADO"

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="GRANJA"] <- "NO ESPECIFICADO"

indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)` <- toupper(indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)



indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="LUGAR NO ESPECIFICADO"] <- "NO ESPECIFICADO"
indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`[indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="OTRO LUGAR"] <- "NO ESPECIFICADO"


# Indicador 21: ----------------------------------------------------------------#
indicador_21 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 21")%>% suppressWarnings()

indicador_21 %>% 
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)` = case_when(
      `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`== "SI" ~ 1,
      `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`== "NO" ~ 0,
      T~0))->indicador_21



# Indicador 22: ----------------------------------------------------------------#
indicador_22 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 22")%>% suppressWarnings()


indicador_22 %>% 
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_22

indicador_22$Formación <- toupper(indicador_22$Formación)


# Indicador 23: ----------------------------------------------------------------#
indicador_23 <- read_excel("indicadores_salud.xlsx", sheet = "Ind 23")%>% suppressWarnings()


indicador_23 %>% 
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
  mutate(`Objetor de conciencia: (SI/NO)` =case_when(
    `Objetor de conciencia: (SI/NO)`=="SI"~0,
    `Objetor de conciencia: (SI/NO)`=="NO"~1))->indicador_23

indicador_23$Formación <- toupper(indicador_23$Formación)



# Indicador 24: ----------------------------------------------------------------#
indicador_24 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 24")%>% suppressWarnings()

indicador_24 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
  mutate(Actualizan=case_when(
    Actualizan=="Si"~1,
    Actualizan=="No"~0)) %>% 
  
  mutate(Instancia=case_when(
    Instancia==NA ~ 0,
    Instancia==NA_character_~ 0,
    T~1))->indicador_24



indicador_24$Fecha   <- format(as.Date(indicador_24$Periodo, format = "%d-%m-%Y"))
indicador_24$Fecha   <- format(as.Date(indicador_24$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_24$Fecha   <- as.Date(indicador_24$Periodo, format = "%d-%m-%Y")



indicador_24 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(Actualizan=sum(Actualizan),
            Instancia=sum(Instancia),
            `Indicador`=scales::percent(sum(Actualizan/Instancia), 0.1)) ->estatal_24

indicador_24 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(Actualizan=sum(Actualizan),
            Instancia=sum(Instancia),
            `Indicador`=scales::percent(sum(Actualizan/Instancia), 0.1)) ->municipal_24


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_24)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_24)->indicador_24


# Indicador 25: ----------------------------------------------------------------#
indicador_25 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 25")%>% suppressWarnings()

indicador_25 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) ->indicador_25



indicador_25$Fecha   <- format(as.Date(indicador_25$Periodo, format = "%d-%m-%Y"))
indicador_25$Fecha   <- format(as.Date(indicador_25$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_25$Fecha   <- as.Date(indicador_25$Periodo, format = "%d-%m-%Y")



# Indicador 26: ----------------------------------------------------------------#
indicador_26 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 26")%>% suppressWarnings()

indicador_26 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
  mutate(Actualización=case_when(
    Actualización=="Si"~1,
    Actualización=="No"~0))->indicador_26



indicador_26$Fecha   <- format(as.Date(indicador_26$Periodo, format = "%d-%m-%Y"))
indicador_26$Fecha   <- format(as.Date(indicador_26$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_26$Fecha   <- as.Date(indicador_26$Periodo, format = "%d-%m-%Y")


# Indicador 27: ----------------------------------------------------------------#
indicador_27 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 27")%>% suppressWarnings()

indicador_27 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_27



indicador_27$Fecha   <- format(as.Date(indicador_27$Periodo, format = "%d-%m-%Y"))
indicador_27$Fecha   <- format(as.Date(indicador_27$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_27$Fecha   <- as.Date(indicador_27$Periodo, format = "%d-%m-%Y")



# Indicador 28: ----------------------------------------------------------------#
indicador_28 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 28")%>% suppressWarnings()

indicador_28 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  `Mujeres atendidas en el CJM`=as.numeric(`Mujeres atendidas en el CJM`),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_28



indicador_28$Fecha   <- format(as.Date(indicador_28$Periodo, format = "%d-%m-%Y"))
indicador_28$Fecha   <- format(as.Date(indicador_28$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_28$Fecha   <- as.Date(indicador_28$Periodo, format = "%d-%m-%Y")





# Indicador 29: ----------------------------------------------------------------#
indicador_29 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 29")%>% suppressWarnings()

indicador_29 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12
    # Mes=="Enero a Junio"~1,
    # Mes=="Julio a Diciembre"~6,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre", "Enero a Junio", "Julio a Diciembre"
             )),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_29



indicador_29$Fecha   <- format(as.Date(indicador_29$Periodo, format = "%d-%m-%Y"))
indicador_29$Fecha   <- format(as.Date(indicador_29$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_29$Fecha   <- as.Date(indicador_29$Periodo, format = "%d-%m-%Y")



indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de Contra la Dignidad"] <- "Delitos contra la dignidad"
indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nCORRUPCIÓN DE MENORES"] <- "Corrupción de menores"
indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de CORRUPCIÓN DE MENORES"] <- "Corrupción de menores"

indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nPROSTITUCIÓN INFANTIL"] <- "Prostitución infantil"
indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de PROSTITUCIÓN INFANTIL"] <- "Prostitución infantil"

indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nULTRAJES A LA MORAL"] <- "Ultrajes a la moral"
indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de ULTRAJES A LA MORAL"] <- "Ultrajes a la moral"


indicador_29$Delito[indicador_29$Delito=="Desobediencia o Resistencia de Particulares"] <- "Desobediencia o resistencia de particulares"
indicador_29$Delito[indicador_29$Delito=="Homicidio (muerte violenta de una mujer)"] <- "Muertes violentas (aún en investigación)"
indicador_29$Delito[indicador_29$Delito=="Instigación o ayuda al suicidio feminicida"] <- "Suicidio feminicida"
indicador_29$Delito[indicador_29$Delito=="NAM bajo los protocolos de perspectiva y violencia de género"] <- "Por el delito que resulte"
# indicador_29$Delito[indicador_29$Delito==""] <- ""

# unique(sort(indicador_29$Delito)) %>% as.tibble() %>%  View()


# indicador_29 %>% 
#   group_by(Año,Mes, Periodo, Carpeta, Delito) %>% 
#   summarise(`Total de casos por violencia por razón de género denunciados`=sum(Registro, na.rm=T))-> estatal_29
# 
# indicador_29 %>% 
#   group_by(Año,Mes, Periodo, Delito, Carpeta, Municipio) %>% 
#   filter(Carpeta=="Judicializada") %>% 
#   summarise(`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Registro, na.rm=T))->municipal_29


# entidad<- c("Estado de Jalisco")
# cbind(entidad, estatal_29)->Estatal_total
# names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
# 
# 
# merge(Estatal_total, municipal_29,
#       by.x = "Periodo",
#       by.y = "Periodo") %>%
#   select(Periodo, Año.x, Mes.x, Municipio.x, Delito.x,Carpeta.x, `Total de casos por violencia por razón de género denunciados`,
#          `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`)->tabla_29_3
# 
# names(tabla_29_3)[names(tabla_29_3) == "Año.x"] <- "Año"
# names(tabla_29_3)[names(tabla_29_3) == "Mes.x"] <- "Mes"
# names(tabla_29_3)[names(tabla_29_3) == "Municipio.x"] <- "Municipio"
# names(tabla_29_3)[names(tabla_29_3) == "Delito.x"] <- "Delito"
# names(tabla_29_3)[names(tabla_29_3) == "Carpeta.x"] <- "Carpeta"
# 
# 
# 
# tabla_29_3->indicador_29
# 


# Indicador 30: ----------------------------------------------------------------#
indicador_30 <- read_excel("indicadores_fiscalia.xlsx", 
                           sheet = "Ind 30", col_types = c("numeric", 
                                                           "text", "text", "text", "date", "numeric", 
                                                           "text", "text", "numeric"))
indicador_30 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
  mutate(`Año de sentencia`=case_when(
    `Año de sentencia`==2023~1,
    `Año de sentencia`==2022~1,
    `Año de sentencia`==2021~1,
    `Año de sentencia`==2020~1, 
    T~0))->indicador_30



indicador_30$Fecha   <- format(as.Date(indicador_30$Periodo, format = "%d-%m-%Y"))
indicador_30$Fecha   <- format(as.Date(indicador_30$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_30$Fecha   <- as.Date(indicador_30$Periodo, format = "%d-%m-%Y")



# Indicador 32: ----------------------------------------------------------------#
indicador_32 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 32")%>% suppressWarnings()

indicador_32 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_32



indicador_32$Fecha   <- format(as.Date(indicador_32$Periodo, format = "%d-%m-%Y"))
indicador_32$Fecha   <- format(as.Date(indicador_32$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_32$Fecha   <- as.Date(indicador_32$Periodo, format = "%d-%m-%Y")



indicador_32 %>% 
  group_by(Año, Mes, Fecha) %>% 
  summarise(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(
    `0 a 2 años`+
      `3 a 5 años`+
      `6 a 12 años`+
      `13 a 17 años`+
      `18 a 25 años`+
      `26 a 35 años`+
      `36 a 45 años`+
      `46 a 59 años`+
      `60  años en adelante`+
      `Sin datos`, na.rm = T),
    `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
    `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
    `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T))->estatal_32

indicador_32 %>% 
  group_by(Año, Mes, Fecha, Municipio) %>% 
  summarise(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(
    `0 a 2 años`+
      `3 a 5 años`+
      `6 a 12 años`+
      `13 a 17 años`+
      `18 a 25 años`+
      `26 a 35 años`+
      `36 a 45 años`+
      `46 a 59 años`+
      `60  años en adelante`+
      `Sin datos`, na.rm = T),
    `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
    `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
    `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T))->municipal_32


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_32)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_32)->indicador_32



# Indicador 33: ----------------------------------------------------------------#
indicador_33 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 33")%>% suppressWarnings()

indicador_33 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_33



indicador_33$Fecha   <- format(as.Date(indicador_33$Periodo, format = "%d-%m-%Y"))
indicador_33$Fecha   <- format(as.Date(indicador_33$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_33$Fecha   <- as.Date(indicador_33$Periodo, format = "%d-%m-%Y")


# Indicador 34: ----------------------------------------------------------------#
indicador_34 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 34")%>% suppressWarnings()

indicador_34 %>%
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    Trimestre = case_when(
      Mes == "Enero" ~ "ene - mar",
      Mes == "Febrero" ~ "ene - mar",
      Mes == "Marzo" ~ "ene - mar",
      Mes == "Abril" ~ "abr - jun",
      Mes == "Mayo" ~ "abr - jun",
      Mes == "Junio" ~ "abr - jun",
      Mes == "Julio" ~ "jul - sep",
      Mes == "Agosto" ~ "jul - sep",
      Mes == "Septiembre" ~ "jul - sep",
      Mes == "Octubre" ~ "oct - dic",
      Mes == "Noviembre" ~ "oct - dic",
      Mes == "Diciembre" ~ "oct - dic"),
    Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
    Trimestre = paste0(Año, " ", Trimestre))->indicador_34



# Indicador 35: ----------------------------------------------------------------#
indicador_35 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 35")%>% suppressWarnings()

indicador_35 %>%
  mutate(Fecha=case_when(
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
    Mes=="Diciembre" ~ 12,
    
  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_35



indicador_35$Fecha   <- format(as.Date(indicador_35$Periodo, format = "%d-%m-%Y"))
indicador_35$Fecha   <- format(as.Date(indicador_35$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_35$Fecha   <- as.Date(indicador_35$Periodo, format = "%d-%m-%Y")

# Indicador 36: ----------------------------------------------------------------#
indicador_36 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 36")%>% suppressWarnings()

indicador_36 %>%
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    Trimestre = case_when(
      Mes == "Enero" ~ "ene - mar",
      Mes == "Febrero" ~ "ene - mar",
      Mes == "Marzo" ~ "ene - mar",
      Mes == "Abril" ~ "abr - jun",
      Mes == "Mayo" ~ "abr - jun",
      Mes == "Junio" ~ "abr - jun",
      Mes == "Julio" ~ "jul - sep",
      Mes == "Agosto" ~ "jul - sep",
      Mes == "Septiembre" ~ "jul - sep",
      Mes == "Octubre" ~ "oct - dic",
      Mes == "Noviembre" ~ "oct - dic",
      Mes == "Diciembre" ~ "oct - dic"),
    Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
    Trimestre = paste0(Año, " ", Trimestre),
    Trimestre=factor(Trimestre, levels = c("2020 ene - mar", "2020 abr - jun", "2020 jul - sep", "2020 oct - dic",
                                           "2021 ene - mar", "2021 abr - jun", "2021 jul - sep", "2021 oct - dic",
                                           "2022 ene - mar", "2022 abr - jun", "2022 jul - sep", "2022 oct - dic")),
    `Rango de edad`=factor(`Rango de edad`,
                           levels=c("0 a 2 años", "3 a 5 años", "6  a 12 años","13 a 17 años", "Sin datos")))->indicador_36


# Indicador 37: ----------------------------------------------------------------#
indicador_37 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 37")%>% suppressWarnings()

indicador_37 %>%
  mutate(
    Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_37


# Indicador 38: ----------------------------------------------------------------#
indicador_38 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 38")%>% suppressWarnings()

indicador_38 %>%
  mutate(Mes=factor(Mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre","Octubre", "Noviembre", "Diciembre")),
         Trimestre = case_when(
           Mes == "Enero" ~ "ene - mar",
           Mes == "Febrero" ~ "ene - mar",
           Mes == "Marzo" ~ "ene - mar",
           Mes == "Abril" ~ "abr - jun",
           Mes == "Mayo" ~ "abr - jun",
           Mes == "Junio" ~ "abr - jun",
           Mes == "Julio" ~ "jul - sep",
           Mes == "Agosto" ~ "jul - sep",
           Mes == "Septiembre" ~ "jul - sep",
           Mes == "Octubre" ~ "oct - dic",
           Mes == "Noviembre" ~ "oct - dic",
           Mes == "Diciembre" ~ "oct - dic"),
         Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
         Periodo = ymd(paste0(Año, "-", Mes, "-01")),
         Trimestre = paste0(Año, " ", Trimestre),
         Trimestre=factor(Trimestre, levels = c("2020 ene - mar", "2020 abr - jun", "2020 jul - sep", "2020 oct - dic",
                                                "2021 ene - mar", "2021 abr - jun", "2021 jul - sep", "2021 oct - dic",
                                                "2022 ene - mar", "2022 abr - jun", "2022 jul - sep", "2022 oct - dic")),
         `Rango de edad`=factor(`Rango de edad`,
                                levels=c("0 a 2 años", "3 a 5 años", "6  a 12 años","13 a 17 años",
                                         "18 a 25 años", "26 a 35", "36 a 45 años", "46 a 59 años", "60 en adelante", "Sin datos")))->indicador_38


# Indicador 39: ----------------------------------------------------------------#
indicador_39 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 39")%>% suppressWarnings()

indicador_39 %>%
  mutate(Mes=factor(Mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre","Octubre", "Noviembre", "Diciembre")),
         Trimestre = case_when(
           Mes == "Enero" ~ "ene - mar",
           Mes == "Febrero" ~ "ene - mar",
           Mes == "Marzo" ~ "ene - mar",
           Mes == "Abril" ~ "abr - jun",
           Mes == "Mayo" ~ "abr - jun",
           Mes == "Junio" ~ "abr - jun",
           Mes == "Julio" ~ "jul - sep",
           Mes == "Agosto" ~ "jul - sep",
           Mes == "Septiembre" ~ "jul - sep",
           Mes == "Octubre" ~ "oct - dic",
           Mes == "Noviembre" ~ "oct - dic",
           Mes == "Diciembre" ~ "oct - dic"),
         Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
         Periodo = ymd(paste0(Año, "-", Mes, "-01")),
         Trimestre = paste0(Año, " ", Trimestre),
         Trimestre=factor(Trimestre, levels = c("2020 ene - mar", "2020 abr - jun", "2020 jul - sep", "2020 oct - dic",
                                                "2021 ene - mar", "2021 abr - jun", "2021 jul - sep", "2021 oct - dic",
                                                "2022 ene - mar", "2022 abr - jun", "2022 jul - sep", "2022 oct - dic")),
         `Rango de edad`=factor(`Rango de edad`,
                                levels=c("0 a 2 años", "3 a 5 años", "6  a 12 años","13 a 17 años",
                                         "18 a 25 años", "26 a 35", "36 a 45 años", "46 a 59 años", "60 en adelante", "Sin datos")))->indicador_39




# Indicador 40: ----------------------------------------------------------------#
indicador_40 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 40")%>% suppressWarnings()

indicador_40 %>%
  mutate(Fecha=case_when(
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
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_40



indicador_40$Fecha   <- format(as.Date(indicador_40$Periodo, format = "%d-%m-%Y"))
indicador_40$Fecha   <- format(as.Date(indicador_40$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_40$Fecha   <- as.Date(indicador_40$Periodo, format = "%d-%m-%Y")


# home_page <- div(
#   titlePanel("Home page"),
#   p("This is the home page!"),
#   uiOutput("power_of_input")
# )

header_img <- div(
  # img(src="https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png", height="45px"),
  div(
    class = "my-title",
    h4('Title'), h5('Subtitle'),
    tags$style(".my-title :is(h4, h5){color: white; font-weight: bold;}")
  ),
  style = "display: flex;"
)



# UI - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
      class = 'p-2',
      tags$head(
        tags$style(HTML("
  .nav.navbar-nav .form-group.shiny-input-container {margin-bottom: 0; height: 38px;}
  .nav.navbar-nav .form-group.shiny-input-container > label {display: inline;}                      
                        
  .p{                    
  font-family: Nutmeg-Regular;
  color: #ffffff;
  }
  

  .dropdown-menu > .active > a {background-color:#baafc4;}


  .navbar-static-top {
   font-family: Nutmeg-Regular;
   background-color:#AA86CA;
   
   }
                                       
   .navbar > .active {
   color: white;
   font-family: Nutmeg-Regular;
   background-color:#AA86CA;!important;},
   
   .navbar-default .navbar-brand{
    color: white;
    font-family: Nutmeg-Regular;
    background-color:#AA86CA;
   }
  
  .navbar-nav li a:hover, .navbar-nav > .active > a {
    color: white;
    font-family: Nutmeg-Regular;
    background-color:#C5B6CA!important;}

      
  .p-2 {
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
    navbarPage(#header,
      # title = "Indicadores de la AVGM", #div(img(src='https://github.com/nancymanzo/Alerta-de-Violencia-de-Genero-contras-Mujeres-AVGM/blob/main/Logo%20AVGM.png?raw=true',
                      # style="margin-top: -20px;
                      #          padding-left:-20px;
                      #          padding-bottom:0px",
                      # height = 60, width=50)),
      #theme = "journal",
      header= busy_start_up(
        loader = spin_epic("flower", color = "#8F5199"),
        text = "Cargando",
        timeout = 1500,
        color = "#8F5199",
        background = " white"),
      useShinydashboard(),
      
# Inicio -----------------------------------------------------------------------

      tabPanel(icon = icon("home"),
               title=p(" Indicadores AVGM", style ="font-weight:bold"), id="Inicio",
               uiOutput('box'),
               actionLink("ling_indicador_1", "Indicador 1: Porcentaje de servicios forenses en casos de muertes violentas de mujeres, provistos conforme a la debida diligencia y perspectiva de género"), hr(),
               actionLink("link_indicadores", "Volver al inicio")),

# Servicios forenses------------------------------------------------------------

      navbarMenu(title = p("Servicios forenses", style ="font-weight:bold"), 
                 tabPanel(
                   id="Ind_1", 
                   title=h6("Indicador 1: Porcentaje de servicios forenses en casos de muertes violentas de mujeres, provistos conforme a la debida diligencia y perspectiva de género", style="white-space: pre-line;", class="p-2"),
                          h3(align="center","Indicador 1:", style="color:black"),
                          h4(p(align="center", "Porcentaje de servicios forenses en casos de muertes violentas de mujeres, provistos conforme a la debida diligencia y perspectiva de género.")),
                          box(width=12, div(class="row d-flex", #Replicar
                                            valueBox("59.9%", "Indicador 2023", icon=icon("equals"),color="light-blue", width = 3), # actualizar
                                            valueBox("61.8%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                            valueBox("55.7%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                            valueBox("57.7%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3)), # actualizar
                              br(), br(), br(),
                              fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                              fluidRow(column(4,selectInput(
                                inputId = "ind_1_año",
                                label = "Seleccione el año",
                                choices = unique(sort(indicador_1$Año)),
                                multiple = T 
                                 )),
                                column(4, align="center",selectInput(
                                  inputId = "ind_1_mes",
                                  label = "Seleccione el mes",
                                  choices = unique(sort(indicador_1$Mes)),
                                  multiple = TRUE
                                 )),
                                column(4, align="center",selectInput(
                                  inputId = "ind_1_servicio",
                                  label = "Selecciona el tipo de acción forense",
                                  choices = unique(sort(indicador_1$`Servicios forenses`)),
                                  multiple = TRUE
                                 ))),
                              fluidRow(
                                # splitLayout(cellWidths = c("40%", "60%"),
                                #             dataTableOutput("t_1"),
                                #             plotlyOutput("gr1"))),
                                column(4,dataTableOutput("t_1", height = "auto", width = "auto")),
                                column(8, plotlyOutput("gr1",  height = "auto", width = "auto"))),
                              fluidRow(column(12, offset = 1, h6("Fuente: Datos proporcionados por IJCF.")), br()
                                       ))),
                 
                 tabPanel(title=h6("Indicador 2: Porcentaje de dictámenes psicosociales en que familiares, víctimas indirectas y/o personas conocidas proveen información para el desarrollo del dictamen", style="white-space: pre-line;", class="p-2"),
                          h3(align="center","Indicador 2:", style="color:black"),
                          h4(p(align="center", "Porcentaje de dictámenes psicosociales en que familiares, víctimas indirectas y/o personas conocidas proveen información para el desarrollo del dictamen.")),
                          box(width=12, div(class="row d-flex", #Replicar
                                            valueBox("67%", "Indicador 2023", icon=icon("equals"),color="light-blue", width = 3), # actualizar
                                            valueBox("62%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                            valueBox("65%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                            valueBox("66%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3)), # actualizar
                              br(), br(), br(),
                              fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                              fluidRow(column(6, align="center", offset = 0, selectInput(
                                         inputId = "ind_2_año",
                                         label = "Seleccione el año",
                                         choices = unique(sort(indicador_2$Año)),
                                         multiple = T
                                       )),
                                column(6, align="center", offset = 0,selectInput(
                                  inputId = "ind_2_mes",
                                  label = "Seleccione el mes",
                                  choices = unique(sort(indicador_2$Mes)),
                                  multiple = TRUE
                                  ))),
                              fluidRow(
                                column(6,dataTableOutput("t_2", height = "auto", width = "auto")),
                                column(6,plotlyOutput("gr2",  height = "auto", width = "auto"))),
                              fluidRow(column(12, offset = 1, h6("Fuente: Datos proporcionados por IJCF.")), br()
                                       ))),
                 
                 tabPanel(title=h6("Indicador 3: Porcentaje de peritajes en servicios forenses con perspectiva de género aplicados conforme a instrumentos de operación del IJCF", style="white-space: pre-line;", class="p-2"),
                          h3(align="center","Indicador 3:", style="color:black"),
                          h4(p(align="center", "Porcentaje de peritajes en servicios forenses con perspectiva de género aplicados conforme a instrumentos de operación del IJCF.")),
                          box(width=12,  div(class="row d-flex", #Replicar
                                             valueBox("42.2%", "Indicador 2023", icon=icon("equals"),color="light-blue", width = 3), # actualizar
                                             valueBox("47.1%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                             valueBox("46.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                             valueBox("46.9%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3)), # actualizar
                              br(), br(), br(),
                              fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                              fluidRow(column(6, align="center", offset = 0,
                                              selectInput(
                                                inputId = "ind_3_año",
                                                 label = "Seleccione el año",
                                                 choices = unique(sort(indicador_3$Año)),
                                                 multiple = T
                                                )),
                                       column(6, align="center", offset = 0, selectInput(
                                         inputId = "ind_3_mes",
                                         label = "Seleccione el mes",
                                         choices = unique(sort(indicador_3$Mes)),
                                        multiple = TRUE))),
                              fluidRow(column(4,dataTableOutput("t_3", height = "1200px", width = "auto")),
                                       column(8, offset = 0, plotlyOutput("gr3",  height = "1200px", width = "auto"))),
                              fluidRow(column(12, offset = 1, h6("Fuente: Datos proporcionados por IJCF.")), br()
                                       ))),
                 
                 tabPanel(title=h6("Indicador 4: Porcentaje de dictámenes de muertes violentas de mujeres en los que se presenta acreditación técnica-científica con razones de género conforme al Protocolo de Actuación con PEG para la Investigación del Delito de Feminicidio", style="white-space: pre-line;", class="p-2"),
                          h3(align="center","Indicador 4:", style="color:black"), 
                          h4(p(align="center", "Porcentaje de dictámenes de muertes violentas de mujeres en los que se presenta acreditación técnica-científica con razones de género conforme al Protocolo de Actuación con PEG para la Investigación del Delito de Feminicidio.")),
                          box(width=12, div(class="row d-flex", #Replicar
                                            valueBox("24.4%", "Indicador 2023", icon=icon("equals"),color="light-blue", width = 3), # actualizar
                                            valueBox("30.8", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                            valueBox("34.8%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                            valueBox("37%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3)), # actualizar
                              br(), br(), br(),
                              fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                              fluidRow(column(6, align="center", offset = 0, 
                                              selectInput(
                                                inputId = "ind_4_año",
                                                label = "Seleccione el año",
                                                choices = unique(sort(indicador_4$Año)),
                                                multiple = T)),
                                       column(6, align="center", offset = 0,selectInput(
                                         inputId = "ind_4_mes",
                                         label = "Seleccione el mes",
                                         choices = unique(sort(indicador_4$Mes)),
                                         multiple = TRUE))),
                              fluidRow(
                                column(5,dataTableOutput("t_4", height = "auto", width = "auto")),
                                column(7,plotlyOutput("gr4",  height = "700px", width = "auto"))),
                              fluidRow(column(8, offset = 1, h6("Fuente: Datos proporcionados por IJCF.")), br()
                                       ))), 
                 
                 
                 tabPanel(title=h6("PENDIENTE: Indicador 5: Porcentaje del personal de la Unidad Multidisciplinaria* adecuadamente capacitado sobre el Protocolo de Actuación con Perspectiva de Género para la Investigación del Delito de Feminicidio.", style="break-spaces: pre-line;", class="p-2"))),
      
# Órdenes y medidas -----------------------------------------------------------

      navbarMenu(title= p('Órdenes y medidas', style ="font-weight:bold"),
                 tabPanel(title=h6("Indicador 6: Porcentaje de mujeres víctimas de violencia por razones de género atendidas y canalizadas para otorgamiento de orden de protección y/o medidas de protección", style="white-space: pre-line;", class="p-2"),
                          h3(align="center","Indicador 6:", style="color:black"),
                          h4(p(align="center", h5("Porcentaje de mujeres víctimas de violencia por razones de género atendidas y canalizadas para otorgamiento de orden de protección y/o medidas de protección." , style="white-space: pre-line")),
                          box(width=12, div(class="row d-flex", #Replicar
                                            valueBox("76.6%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                            valueBox("60.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                            valueBox("55.3%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
                                            valueBox("61.0%","Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3)), # actualizar
                              br(), br(), br(), 
                              fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                              fluidRow(column(4,
                                              selectInput(
                                                inputId = "ind_6_año",
                                                label = "Seleccione el año",
                                                choices = unique(sort(indicador_6$Año)),
                                                multiple = T)),
                                       column(4,selectInput(
                                         inputId = "ind_6_mes",
                                         label = "Seleccione el mes",
                                         choices = unique(sort(indicador_6$Mes)),
                                         multiple = TRUE)),
                                       column(4,selectInput(
                                         inputId = "ind_1_servicio",
                                         label = "Selecciona el tipo de acción forense",
                                         choices = unique(sort(indicador_6$`Servicios forenses`)),
                                         multiple = TRUE
                                        ))),
                              fluidRow(column(5,dataTableOutput("t_6", height = "auto", width = "auto")),
                                       column(7,plotlyOutput("gr6",  height = "600px", width = "auto"))), 
                              fluidRow(column(8, offset = 1, h6("Fuente: Datos proporcionados por IJCF.")), br())))),
                 
                 tabPanel(title=h6("Indicador 7: Porcentaje de mujeres víctimas de violencia por razones de género que solicitaron y obtuvieron orden y/o medida de protección." , style="break-spaces: pre-line", class="p-2"),
                          h3(align="center","Indicador 7:", style="color:black"),
                          h4(p(align="center", "Porcentaje de mujeres víctimas de violencia por razones de género que solicitaron y obtuvieron orden y/o medida de protección.")),
                          box(width=12,
                              div(class="row d-flex", #Replicar
                                  valueBox("25%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("37%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
      valueBox("50%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
      valueBox("62%","Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3)), # actualizar
      br(), br(), br(),
      fluidRow(column(12, offset = 5,"Seleccione algunas características")),
      fluidRow(
        column(4,
               selectInput(
                 inputId = "ind_7_año",
                 label = "Seleccione el año",
                 choices = unique(sort(indicador_7$Año)),
                 multiple = T)
        ),
        column(4,selectInput(
          inputId = "ind_7_mes",
          label = "Seleccione el mes",
          choices = unique(sort(indicador_7$Mes)),
          multiple = TRUE
        )),
        column(4,selectInput(
          inputId = "ind_7_municipio",
          label = "Selecciona el municipio",
          choices = unique(sort(indicador_7$Municipio)),
          multiple = FALSE,
          selected = "Estado de Jalisco"
        ))
      ),
      fluidRow(
        column(4,dataTableOutput("t_7", height = "auto", width = "auto")),
        column(8, offset = 0,
               plotlyOutput("gr7",  height = "600px", width = "auto"))),
      fluidRow(column(8, offset = 1,
                      h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
               ))),
      
      tabPanel(title=h6("PÉNDIENTE: Indicador 8: Porcentaje de personal capacitado en la adecuada aplicación del Modelo de Atención, Otorgamiento y Seguimiento de Órdenes de Protección.", style="break-spaces: pre-line;width: 75%;", class="p-2")),
      
      
      tabPanel(title=h6("Indicador 9: Porcentaje de medidas de protección otorgadas que fueron trabajadas y/o notificadas efectiva y personalmente a la persona agresora en relación al total", style="break-spaces: pre-line", class="p-2"),
               h3(align="center","Indicador 9:", style="color:black"),
               h4(p(align="center", "Porcentaje de medidas de protección otorgadas que fueron trabajadas y/o notificadas efectiva y personalmente a la persona agresora en relación al total.")),
               box(width=12,
                   div(class="row d-flex", #Replicar
                       valueBox("86.9%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("101.8%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("101.1%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,selectInput(
             inputId = "ind_9_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_9$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_9_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_9$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_9_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_9$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    ))),
  fluidRow(
    column(4,dataTableOutput("t_9", height = "auto", width = "auto")),
    column(8, offset=0, 
           plotlyOutput("gr9",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."))))), br(),

tabPanel(title=h6("Indicador 10: Porcentaje de órdenes de protección otorgadas que fueron trabajadas y/o notificadas efectiva y personalmente a la persona agresora en relación al total." , style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 10:", style="color:black"),
         h4(p(align="center", "Porcentaje de órdenes de protección otorgadas que fueron trabajadas y/o notificadas efectiva y personalmente a la persona agresora en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("85.8%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("92.5%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("102.0%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  # sidebarLayout(
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_10_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_10$Año)),
             multiple = T
           )
    ),
    column(4,selectInput(
      inputId = "ind_10_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_10$Mes)),
      multiple = TRUE
    )
    ),
    column(4,selectInput(
      inputId = "ind_10_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_10$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    )
    )#,
    #downloadButton("downloadData_ind_1", "Descarga (.csv)")
  ),
  
  fluidRow(
    # div(class="display dataTable no-footer",
    column(4,dataTableOutput("t_10", height = "auto", width = "auto")),
    # h6("Fuente: Datos proporcionados por IJCF."),br(),
    column(8, offset=0, 
           plotlyOutput("gr10",  height = "auto", width = "auto"))
  ),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
  
))),

tabPanel(title=h6("Indicador 11: Porcentaje de mujeres que han recibido seguimiento después de otorgada su orden y/o medida de protección en relación al total."
                  , style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 11:", style="color:black"),
         h4(p(align="center", "Porcentaje de mujeres que han recibido seguimiento después de otorgada su orden y/o medida de protección en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("97.3%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("95.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("103.2%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_11_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_11$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_11_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_11$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_11_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_11$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    ))),
  fluidRow(
    column(5,dataTableOutput("t_11", height = "auto", width = "auto")),
    column(7,plotlyOutput("gr11",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
           ))),

tabPanel(title=h6("Indicador 12: Porcentaje de Carpetas de Investigación iniciadas contra personas agresoras derivados de incumplimiento de órdenes y/o medidas de protección." 
                  , style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 12:", style="color:black"),

         h4(p(align="center", "Porcentaje de Carpetas de Investigación iniciadas contra personas agresoras derivados de incumplimiento de órdenes y/o medidas de protección.")),

box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("7.8%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("0.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("1.8%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  # sidebarLayout(
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_12_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_12$Año)),
             multiple = T
           )
    ),
    column(4,selectInput(
      inputId = "ind_12_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_12$Mes)),
      multiple = TRUE
    )
    ),
    column(4,selectInput(
      inputId = "ind_12_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_12$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    ))),
  
  fluidRow(
    # div(class="display dataTable no-footer",
    column(4,dataTableOutput("t_12", height = "auto", width = "auto")),
    column(7,offset=1,
           plotlyOutput("gr12",  height = "600px", width = "auto"))
  ),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
))),

tabPanel(title=h6("Indicador 13: Porcentaje de casos en los que la orden y/o medida de protección resultó ser adecuada y efectiva para la víctima en relación al total", 
                  style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 13:", style="color:black"),
         
         h4(p(align="center", "Porcentaje de casos en los que la orden y/o medida de protección resultó ser adecuada y efectiva para la víctima en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("99.9%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("99.7%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("95.1", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_13_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_13$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_13_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_13$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_13_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_13$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    ))),
  fluidRow(
    column(5,dataTableOutput("t_13", height = "auto", width = "auto")),
    column(7,plotlyOutput("gr13",  height = "700px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
  ))),
    tabPanel(title=h6("PENDIENTE: Indicador 14: Porcentaje de mujeres víctimas de violencia con órdenes de protección que perciben que han recuperado su sensación de seguridad frente a las personas agresoras", style="break-spaces: pre-line", class="p-2")),
    
    tabPanel(title=h6("PENDIENTE: Indicador 15: Porcentaje de mujeres víctimas de violencia con medida de protección que perciben que han recuperado su sensación de seguridad frente a las personas agresoras", style="break-spaces: pre-line", class="p-2")
)),
# ILE IVE ----------------------------------------------------------------------

navbarMenu(title = p("ILE / IVE", style ="font-weight:bold"),
tabPanel(title = h6("Indicador 16: Porcentaje de mujeres denunciantes de violación o abuso sexual infantil que son remitidas para atención integral de la salud conforme a la NOM 046 en relación al total", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 16:", style="color:black"),
         h4(p(align="center", "Porcentaje de mujeres denunciantes de violación o abuso sexual infantil que son remitidas para atención integral de la salud conforme a la NOM 046 en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("37.9%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("42.7%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("33.3%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_16_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_16$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_16_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_16$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_16_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_16$`Rango de edad`)),
      multiple = TRUE
    ))),
  fluidRow(
    fluidRow(column(11,dataTableOutput("t_16", height = "400px", width = "auto"))),
    fluidRow(
      plotlyOutput("gr16",  height = "1000px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por IJCF.")), br()
))),

tabPanel(title=h6("Indicador 17: Porcentaje de mujeres atendidas por violación y abuso sexual infantil en el Sector Salud referidas por la Fiscalía del Estado en relación al total", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 17:", style="color:black"),
         h4(p(align="center", "Porcentaje de mujeres atendidas por violación y abuso sexual infantil en el Sector Salud referidas por la Fiscalía del Estado en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("46.4%", "Atenciones por violación en 2023", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("100%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
      valueBox("46.4%", "Atenciones por violación en 2022",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
      valueBox("100%", "Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_17_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_17$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_17_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_17$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_17_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_17$`Rango de edad`)),
      multiple = TRUE
    ))),
  fluidRow(
    fluidRow(column(11,dataTableOutput("t_17", height = "400px", width = "auto"))),
    fluidRow(
      plotlyOutput("gr17",  height = "450px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()))),

tabPanel(title = h6("Indicador 18: Porcentaje de mujeres solicitantes de IVE por violación que reciben el procedimiento", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 18:", style="color:black"),
         h4(p(align="center", "Porcentaje de mujeres atendidas por violación y abuso sexual infantil en el Sector Salud referidas por la Fiscalía del Estado en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("46.4%", "Atenciones por violación en 2023", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("100%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
      valueBox("46.4%", "Atenciones por violación en 2022",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
      valueBox("100%", "Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_18_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_18$Año)),
             multiple = T
           )),
    column(4,selectInput(
      inputId = "ind_18_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_18$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_18_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_18$Rango)),
      multiple = TRUE
    ))),
  fluidRow(
    column(4,dataTableOutput("t_18", height = "auto", width = "auto")),
    column(6, offset = 2,
           plotlyOutput("gr18",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
           ))),
tabPanel(title = h6("Indicador 19: Porcentaje de mujeres que reciben el procedimiento de interrupción legal del embarazo conforme al Programa Estatal para la Interrupción Legal del Embarazo (Programa ILE) en los Servicios de Salud del Estado de Jalisco", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 19:", style="color:black"),
         h4(p(align="center", "Porcentaje de mujeres atendidas por violación y abuso sexual infantil en el Sector Salud referidas por la Fiscalía del Estado en relación al total.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("99.1%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 6), # actualizar
      valueBox("0%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 6)), # actualizar
  
  br(), br(), br(),
  # sidebarLayout(
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_19_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_19$Año)),
             multiple = T
           )
    ),
    column(4,selectInput(
      inputId = "ind_19_causal",
      label = "Selecciona la causal",
      choices = unique(sort(indicador_19$`Causal: (salud/riesgo)`)),
      multiple = TRUE
    )
    ),
    column(4,selectInput(
      inputId = "ind_19_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_19$Rango)),
      multiple = TRUE
    ))),
  fluidRow(
    column(6,dataTableOutput("t_19", height = "auto", width = "auto")),
    column(6,plotlyOutput("gr19",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por IJCF.")), br()
           ))),

tabPanel(title =h6("Indicador 20: Porcentaje de casos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía",
                   style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 20:", style="color:black"),
         h4(p(align="center", "Porcentaje de casos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía.")),
         box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("79.8%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("83.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 2), # actualizar
      valueBox("40.2%", "Indicador 2020", icon=icon("wave-square"),color="maroon", width = 2), # actualizar
      valueBox("73.5%", "Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 2), # actualizar
      valueBox("75.6%", "Indicador 2018", icon=icon("wave-square"),color="red", width = 3)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_20_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_20$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_20_causal",
      label = "Selecciona la causal",
      choices = unique(sort(indicador_20$`Causal: (salud/riesgo)`)),
      multiple = TRUE)
    ),
    column(4,selectInput(
      inputId = "ind_20_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_20$Rango)),
      multiple = TRUE
    ))),
  
  fluidRow(
    fluidRow(column(11,dataTableOutput("t_20", height = "400px", width = "auto"))),
    fluidRow(
      plotlyOutput("gr20",  height = "700px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
           ))),

tabPanel(title = h6("Indicador 21: Porcentaje de establecimientos estatales proveedores de servicios de salud en condiciones óptimas para realizar un procedimiento ILE/IVE",
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 21:", style="color:black"),
         h4(p(align="center", "Porcentaje de casos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía.")),
         box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("79.8%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("83.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 2), # actualizar
      valueBox("40.2%", "Indicador 2020", icon=icon("wave-square"),color="maroon", width = 2), # actualizar
      valueBox("73.5%", "Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 2), # actualizar
      valueBox("75.6%", "Indicador 2018", icon=icon("wave-square"),color="red", width = 3)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4, offset = 2, 
           selectInput(
             inputId = "ind_21_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_21$Año)),
             multiple = T
           )),
    column(4,offset = 2,selectInput(
      inputId = "ind_21_establecimiento",
      label = "Seleccione el hospital",
      choices = unique(sort(indicador_21$Establecimiento)),
      multiple = TRUE
    ))),
  fluidRow(
    column(12,dataTableOutput("t_21", height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
           ))),

tabPanel(title = h6("Indicador 22: Porcentaje del personal de salud relacionado al procedimiento ILE/IVE, capacitado en el Programa ILE y NOM 046", 
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 22:", style="color:black"),
         h4(p(align="center", "Porcentaje del personal de salud relacionado al procedimiento ILE/IVE, capacitado en el Programa ILE y NOM 046.")),
         box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("100%", "Indicador 2020", icon=icon("equals"),color="fuchsia", width = 6), # actualizar
      valueBox("100%", "Indicador 2019", icon=icon("wave-square"),color="purple", width = 6)),# actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4, offset = 2,
           selectInput(
             inputId = "ind_22_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_22$Año)),
             multiple = T)
    ),
    column(4, offset = 2, selectInput(
      inputId = "ind_22_formación",
      label = "Seleccione la formación del personal",
      choices = unique(sort(indicador_22$Formación)),
      multiple = TRUE))),
  fluidRow(
    fluidRow(column(11,dataTableOutput("t_22", height = "400px", width = "auto"))),
    fluidRow(
      plotlyOutput("gr22",  height = "700px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
           ))),

tabPanel(title = h6("Indicador 23: Porcentaje de personal médico debidamente capacitado en el procedimiento ILE/IVE",
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 23:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de personal médico debidamente capacitado en el procedimiento ILE/IVE.")),
         box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("78.4%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("6.1%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("0%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_22_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_22$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_23_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_23$Mes)),
      multiple = TRUE)
    ),
    column(4,selectInput(
      inputId = "ind_23_formación",
      label = "Selecciona la formación del personal medicx",
      choices = unique(sort(indicador_23$Formación)),
      multiple = TRUE)
    )),
  fluidRow(
    column(4,dataTableOutput("t_23", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr23",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
  )))
),

# Baedavim ---------------------------------------------------------------------

navbarMenu(title = p("BAEDAVIM", style ="font-weight:bold"),
tabPanel(h6("Indicador 24: Porcentaje de municipios que actualizan la información sobre atención a mujeres víctimas de violencia con respecto al total", 
            style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 24:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de municipios que actualizan la información sobre atención a mujeres víctimas de violencia con respecto al total.")),
         box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("7.4%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("6.4%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("5.0%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_24_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_24$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_24_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_24$Mes)),
      multiple = TRUE)
    ),
    column(4,selectInput(
      inputId = "ind_24_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_24$Municipio)),
      multiple = FALSE,
      selected = "Estado de Jalisco"
    ))),
  
  fluidRow(
    column(4,dataTableOutput("t_24", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr24",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")), br()
))
),
tabPanel(h6("Indicador 25: Porcentaje del personal responsable de la actualización del banco de datos adecuadamente capacitado con relación al total", 
            style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 25:", style="color:black"),
         h4(p(align="center", "Porcentaje del personal responsable de la actualización del banco de datos adecuadamente capacitado con relación al total.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("255.9%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("187.4%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("50%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_25_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_25$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_25_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_25$Mes)),
      multiple = TRUE
    )
    ),
    column(4,selectInput(
      inputId = "ind_25_nivel",
      label = "Selecciona el nivel",
      choices = unique(sort(indicador_25$Nivel)),
      multiple = TRUE
    ))),
  
  fluidRow(
    column(4,dataTableOutput("t_25", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr25",  height = "auto", width = "auto"))
  ),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()

           ))),

tabPanel(h6("Indicador 26: Porcentaje de instancias estatales responsables de alimentar banco de datos que actualizan en tiempo y forma en relación al total de instancias obligadas", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 26:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de instancias estatales responsables de alimentar banco de datos que actualizan en tiempo y forma en relación al total de instancias obligadas.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("60.7%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("69.0%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("85.7%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_26_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_26$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_26_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_26$Mes)),
      multiple = TRUE
    ))),
  
  fluidRow(
    column(4,dataTableOutput("t_26", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr26",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
           )))),
  
# Delitos por razón de género---------------------------------------------------

navbarMenu(title = p("Delitos por razón de género", style ="font-weight:bold"),
           tabPanel(h6("Indicador 27: Número de opiniones técnicas que ha emitido la Dirección de Análisis y Contexto sobre los delitos de feminicidio y desaparición de niñas, adolescentes y mujeres",style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 27:", style="color:black"),
         h4(p(align="center",
              "Número de opiniones técnicas que ha emitido la Dirección de Análisis y Contexto sobre los delitos de feminicidio y desaparición de niñas, adolescentes y mujeres.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("974", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("523", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("392","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4, offset = 2, 
           selectInput(
             inputId = "ind_27_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_27$Año)),
             multiple = T
           )
    ),
    column(4,offset = 2, selectInput(
      inputId = "ind_27_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_27$Mes)),
      multiple = TRUE)
    )),
  
  fluidRow(
    column(4,dataTableOutput("t_27", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr27",  height = "auto", width = "auto"))
  ),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
  ))
),
tabPanel(h6("Indicador 28: Porcentaje de mujeres víctimas de violación y violencia familiar que son atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres, en relación al total de mujeres atendidas en los Centros de Justicia para las Mujeres", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 28:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de mujeres víctimas de violación y violencia familiar que son atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres, en relación al total de mujeres atendidas en los Centros de Justicia para las Mujeres.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("83.0%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("50.8%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("92.4%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  # sidebarLayout(
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_28_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_28$Año)),
             multiple = T
           )
    ),
    column(4,selectInput(
      inputId = "ind_28_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_28$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_28_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_28$Municipio)),
      multiple = TRUE
    ))),
  
  fluidRow(
    column(4,dataTableOutput("t_28", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr28",  height = "600px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
))),
tabPanel(h6("Indicador 29: Porcentaje de casos de violencia contra las mujeres por razones de género judicializados", 
            style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 29:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de casos de violencia contra las mujeres por razones de género judicializados.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("20.2%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("16.1%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("11.0%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(3,
     selectInput(
       inputId = "ind_29_año",
       label = "Seleccione el año",
       choices = unique(sort(indicador_29$Año)),
       multiple = T
     )),
    column(3,selectInput(
      inputId = "ind_29_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_29$Mes)),
      multiple = TRUE
    )),
    column(3,selectInput(
      inputId = "ind_29_municipio",
      label = "Selecciona el municipio",
      choices = unique(sort(indicador_29$Municipio)),
      multiple = TRUE
    )),
    column(3,selectInput(
      inputId = "ind_29_delito",
      label = "Selecciona el delito",
      choices = unique(sort(indicador_29$Delito)),
      multiple = TRUE
    ))),
  fluidRow(
    column(4,dataTableOutput("t_29", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr29",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
))),

tabPanel(h6("Indicador 30: Porcentaje de sentencias por el delito de feminicidio en relación a los casos vinculados a proceso", 
            style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 30:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de sentencias por el delito de feminicidio en relación a los casos vinculados a proceso.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("29.4%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("23.4%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("30.6%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,  
           selectInput(
             inputId = "ind_30_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_30$Año)),
             multiple = T
           )),
    column(4,selectInput(
      inputId = "ind_30_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_30$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_30_condena",
      label = "Selecciona el tipo de condena",
      choices = unique(sort(indicador_30$`Tipo de sentencia (absolutoria, condenatoria y en proceso)`)),
      multiple = TRUE
    ))),
  fluidRow(
    column(4,dataTableOutput("t_30", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr30",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
)))),

# Protocolo alba----------------------------------------------------------------
navbarMenu(title = p("Protocolo Alba", style ="font-weight:bold"),
tabPanel(title=h6("PENDIENTE: Indicador 31: Porcentaje de niñas, adolescentes y mujeres denunciadas como desaparecidas que son localizadas", style="break-spaces: pre-line", class="p-2"),
tabPanel(title = h6("Indicador 32: Número de denuncias de niñas, adolescentes y mujeres desaparecidas", style="break-spaces: pre-line", class="p-2"),
         tabItem(tabName = "Ind32",
                 fluidRow(width=10,
                          h3(align="center","Indicador 32:", style="color:black"),
                          h4(p(align="center",
                               "Número de denuncias de niñas, adolescentes y mujeres desaparecidas.")),
                          box(width=12,
                       div(class="row d-flex", #Replicar
                       valueBox("645", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
                       valueBox("942", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
                       valueBox("1,065", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
                   
                   br(), br(), br(),
                   fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                   fluidRow(
                     column(4,
                            selectInput(
                              inputId = "ind_32_año",
                              label = "Seleccione el año",
                              choices = unique(sort(indicador_32$Año)),
                              multiple = T
                            )
                     ),
                     column(4,selectInput(
                       inputId = "ind_32_mes",
                       label = "Seleccione el mes",
                       choices = unique(sort(indicador_32$Mes)),
                       multiple = TRUE
                     )
                     ),
                     column(4,selectInput(
                       inputId = "ind_32_municipio",
                       label = "Selecciona el municipio",
                       choices = unique(sort(indicador_32$Municipio)),
                       multiple = FALSE,
                       selected = "Estado de Jalisco"
                     ))),
                   
                   fluidRow(
                     column(4,dataTableOutput("t_32", height = "auto", width = "auto")),
                     column(6,  offset = 2, 
                            plotlyOutput("gr32",  height = "700px", width = "auto"))),
                   fluidRow(column(8, offset = 1,
                                   h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
                            )))))),

tabPanel(title = h6("Indicador 33: Porcentaje de cédulas únicas de difusión que son remitidas al Comité Técnico de Colaboración", 
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 33:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de cédulas únicas de difusión que son remitidas al Comité Técnico de Colaboración.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("10.5%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("93.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("66.7%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_33_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_33$Año)),
             multiple = T
           )),
    column(4,selectInput(
      inputId = "ind_33_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_33$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_33_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_33$Edad)),
      multiple = TRUE))),
  
  fluidRow(
    column(4,dataTableOutput("t_33", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr33",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()))),

tabPanel(title = h6("Indicador 34: Porcentaje de casos de búsqueda y localización realizados de forma inmediata y diferenciada en relación al total de casos", 
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 34:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de casos de búsqueda y localización realizados de forma inmediata y diferenciada en relación al total de casos.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("51.9%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("55.6%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("55.2%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
     selectInput(
       inputId = "ind_34_año",
       label = "Seleccione el año",
       choices = unique(sort(indicador_34$Año)),
       multiple = T
     )),
    column(4,selectInput(
      inputId = "ind_34_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_34$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_34_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_34$`Rango de edad`)),
      multiple = TRUE
    ))),
  fluidRow(
    column(4,dataTableOutput("t_34", height = "1000px", width = "auto")),
    column(7, offset=1, 
           plotlyOutput("gr34",  height = "1000px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
           ))),

tabPanel(title = h6("Indicador 35: Porcentaje de Cédulas de Difusión activas en relación al total de Cédulas de Difusión emitidas", 
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 35:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de Cédulas de Difusión activas en relación al total de Cédulas de Difusión emitidas.")),
box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("31.6%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
      valueBox("33.3%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
      valueBox("36.4%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
      valueBox("29.2%","Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3)), # actualizar
  
  br(), br(), br(),
  # sidebarLayout(
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4, offset = 2,
           selectInput(
             inputId = "ind_35_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_35$Año)),
             multiple = T
           )),
    column(4,offset = 2,selectInput(
      inputId = "ind_35_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_35$Mes)),
      multiple = TRUE
    ))),
  
  fluidRow(
    column(4,dataTableOutput("t_35", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr35",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
  ))
),
tabPanel(title = h6("Indicador 36: Porcentaje de casos de investigación de desaparición de niñas y adolescentes en los que se activa reporte Amber", 
                    style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 36:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de casos de investigación de desaparición de niñas y adolescentes en los que se activa reporte Amber.")),
 box(
  width=12,
  div(class="row d-flex", #Replicar
      valueBox("100%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("100%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("100%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4, 
           selectInput(
             inputId = "ind_36_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_36$Año)),
             multiple = T
           )),
    column(4, selectInput(
      inputId = "ind_36_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_36$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_36_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_36$`Rango de edad`)),
      multiple = TRUE
    ))),

  fluidRow(
    column(4,dataTableOutput("t_36", height = "auto", width = "auto")),
    column(8,plotlyOutput("gr36",  height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
))
),
tabPanel(title = h6("Indicador 37: Porcentaje de personal capacitado en el Protocolo Alba", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 37:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de personal capacitado en el Protocolo Alba.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("100%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("96.4%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("76.7%","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_37_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_37$Año)),
             multiple = T
    )),
    column(4,selectInput(
      inputId = "ind_37_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_37$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_37_personal",
      label = "Seleccione la función del personal",
      choices = unique(sort(indicador_37$Personal)),
      multiple = TRUE
    ))),
  fluidRow(column(10,dataTableOutput("t_37", height = "auto", width = "auto"))),
  fluidRow(column(8, offset = 1, h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
           ))),

tabPanel(title = h6("Indicador 38: Porcentaje de casos resueltos en etapa de investigación", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 38:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de casos resueltos en etapa de investigación.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("92.7%%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("80.6%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("81.2","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
  column(4,
   selectInput(
   inputId = "ind_38_año",
   label = "Seleccione el año",
   choices = unique(sort(indicador_38$Año)),
   multiple = T
   )),
    column(4,selectInput(
      inputId = "ind_38_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_38$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_38_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_38$`Rango de edad`)),
      multiple = TRUE))),
  fluidRow(
    column(4,dataTableOutput("t_38", height = "800px", width = "auto")),
    column(8,plotlyOutput("gr38",  height = "600px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
))),


tabPanel(title = h6("Indicador 39: Porcentaje de casos de desaparición que reciben seguimiento", style="break-spaces: pre-line", class="p-2"),
         h3(align="center","Indicador 39:", style="color:black"),
         h4(p(align="center",
              "Porcentaje de casos de desaparición que reciben seguimiento.")),
  box(width=12,
  div(class="row d-flex", #Replicar
      valueBox("100%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
      valueBox("100%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
      valueBox("100%", "Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4)), # actualizar
  br(), br(), br(),
  fluidRow(column(12, offset = 5,"Seleccione algunas características")),
  fluidRow(
    column(4,
           selectInput(
             inputId = "ind_39_año",
             label = "Seleccione el año",
             choices = unique(sort(indicador_39$Año)),
             multiple = T)
    ),
    column(4,selectInput(
      inputId = "ind_39_mes",
      label = "Seleccione el mes",
      choices = unique(sort(indicador_39$Mes)),
      multiple = TRUE
    )),
    column(4,selectInput(
      inputId = "ind_39_edad",
      label = "Selecciona el rango de edad",
      choices = unique(sort(indicador_39$`Rango de edad`)),
      multiple = TRUE
    ))),
  fluidRow(
    column(4,dataTableOutput("t_39", height = "800px", width = "auto")),
    column(8,plotlyOutput("gr39",  height = "600px", width = "auto"))),
  fluidRow(column(8, offset = 1,
                  h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()
  )))),

# Generales---------------------------------------------------------------------
  navbarMenu(title = p("Generales", style ="font-weight:bold"),
             tabPanel(title = h6("Indicador 40: Porcentaje de muertes violentas de mujeres registradas, en relación al año anterior",
                                 style="break-spaces: pre-line", class="p-2"),
                      h3(align="center","Indicador 40:", style="color:black"),
                      h4(p(align="center", "Porcentaje de muertes violentas de mujeres registradas, en relación al año anterior.")),
                      box(width=12, div(class="row d-flex", #Replicar
                                        valueBox("-14.1%", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
                                        valueBox("39.8%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
                                        valueBox("-10.7%", "Indicador 2020", icon=icon("wave-square"),color="maroon", width = 4)), # actualizar
                          br(), br(), br(),
                          fluidRow(column(12, offset = 5,"Seleccione algunas características")),
                          fluidRow(column(4, offset = 2,
                                          selectInput(
                                            inputId = "ind_40_año",
                                            label = "Seleccione el año",
                                            choices = unique(sort(indicador_39$Año)),
                                            multiple = T)),
                                   column(4, offset = 2,selectInput(
                                     inputId = "ind_40_mes",
                                     label = "Seleccione el mes",
                                     choices = unique(sort(indicador_39$Mes)),
                                     multiple = TRUE
                                     ))),
                          fluidRow(column(4,dataTableOutput("t_40", height = "1200px", width = "auto")),
                                   column(6, offset=2, plotlyOutput("gr40",  height = "1000px", width = "auto"))),
                          fluidRow(column(8, offset = 1, h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())
             )),
             tabPanel(title = h6("PENDIENTE: Indicador 41: Porcentaje de casos judicializados como feminicidios juzgados con perspectiva de género"),
                      style="break-spaces: pre-line", class="p-2"),
             tabPanel(title=h6("PENDIENTE:Indicador 42: Tasa de variación de recursos estatales destinados a la alerta de violencia de género contra las mujeres, en relación al año anterior", style="break-spaces: pre-line", class="p-2"))
)
))))#jati


# Server - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {

  observeEvent(input$ling_indicador_1, {
    newvalue <- "Ind_1"
    updateTabItems(session, "Ind_1", newvalue)
  })
  observeEvent(input$link_indicadores, {
    newvalue <- "Indicadores AVGM"
    updateTabsetPanel(session, "Inicio", newvalue)
  })
  
  output$mtcars_table <- DT::renderDataTable({mtcars
    })

  output$box = renderUI({
    div(id = 'boxID', # give the box an ID
        box(
          title = "Box to link to tab2",
          status = "primary",
          width = 3
        )
    )
  })

  # update tab when box is clicked
  observe({
    onclick('boxID',
            updateTabItems(session, inputId = 'navbarID', selected = 'Tab 2'))
  })
  
  output$filecontainer <- renderUI({
    tags$iframe(src="//rstudio-pubs-static.s3.amazonaws.com/1083902_d0b2a08d8b944627aed0720cb7a08291.html",
                style="border: 0px solid white; width: 900px; height: 750px;" )#height = 600, width = 1200)
  })
  
  
  # Indicador 1: -----------------------------------------------------------------  
  
  output$ind_1_año <- renderUI({
    selectInput("ind_1_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_1$Año)),
                multiple = T)
  })
  
  output$ind_1_mes<- renderUI({
    selectInput("ind_1_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_1$Mes)),
                multiple = T)
  })
  
  
  output$ind_1_servicio <- renderUI({
    selectInput("ind_1_servicio",
                label =  "Selecciona el tipo de servicio forense",
                choices = sort(unique(indicador_1$`Servicios forenses`)),
                multiple = T)
  })
  
  
  ind_1_reactive <- reactive({
    
    indicador_1 %>%
      filter(
        if(!is.null(input$ind_1_año))                        Año %in% input$ind_1_año             else Año != "",
        if(!is.null(input$ind_1_mes))                        Mes %in% input$ind_1_mes             else Mes != "",
        if(!is.null(input$ind_1_servicio))  `Servicios forenses` %in% input$ind_1_servicio         else `Servicios forenses` != ""
      )
    
  })
  
  
  
  output$gr1 <-renderPlotly ({
    
    ind_1_reactive() %>% 
      group_by(`Servicios forenses`, Periodo) %>% 
      summarise(`Total de muertes violentas de mujeres`=sum(`Total de muertes violentas`),
                `Total de acciones con debida diligencia`= n(),
                `Total de acciones con debida diligencia aplicados con PEG`= sum(Aplicado, na.rm=T)) %>% 
      mutate(text = paste(#"Año: ", Año,
        "\nPeriodo: ",  format(as_date(Periodo), "%b de %y"),
        "\nTotal: ", scales::comma(`Total de acciones con debida diligencia aplicados con PEG`), 
        "\nMuertes violentas: ", `Total de muertes violentas de mujeres`,
        sep="")) %>%
      ggplot() +
      aes(x = Periodo, y = `Total de acciones con debida diligencia aplicados con PEG`, 
          colour = `Servicios forenses`, group=`Servicios forenses`, text=text) +
      geom_line(size = 1.5) + 
      geom_point(size = 2)+
      
      geom_point(aes(x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"), shape = "asterisk", size = 2.5)+
      geom_line(aes( x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"),  linetype = "dashed", size = 1.5)+
      labs(x="", y="", title = "Indicador 1",
           color = " ") +
      theme_minimal()+   
      #facet_wrap(vars(AÑO))+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Servicio realizado dentro de la 1era hora` = "#c91682",
                   `Oportuna recolección de pruebas` = "#7e3794",
                   `Oportuna preservación de pruebas` = "#d98cbc",
                   `Oportuna identificación y entrevista a testigos (dictamen femi.)`= "#b58cd9",
                   `Recopilación de evidencia a través de videos`= "#a544ff",
                   `Asistencia de primer respondiente (policía), asistencia de policía de investigación y asistencia de peritos`= "#597DFF", 
                   `Total de muertes violentas de mujeres`= "#b02a45"))+
      
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr1
    
    
    
    ggplotly(gr1, tooltip = "text") %>% 
      layout(title = "Indicador 1",
             legend = list(orientation = 'h',
                           x = 0, y = -1),
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_1 <- renderDataTable ({
    
    ind_1_reactive() %>% 
      filter(`Servicios forenses` %in% c("Oportuna recolección de pruebas",
                                         "Oportuna recolección de pruebas")) %>%
      group_by(Folio, Año) %>% 
      summarise(
        `Total de muertes violentas de mujeres`=sum(`Total de muertes violentas`, na.rm = T), #aquí se debe dividir entre la cantidad de elecciones en la acción forense
        `Total de acciones con debida diligencia aplicados con PEG`= sum(Aplicado, na.rm=T),
        `Total de acciones con debida diligencia`= n()
        
      ) %>% mutate(`Total de muertes violentas de mujeres`=`Total de muertes violentas de mujeres`/`Total de acciones con debida diligencia`) %>% 
      group_by(Año) %>% 
      summarise(
        `Total de muertes violentas de mujeres`=sum(`Total de muertes violentas de mujeres`, na.rm = T),
        `Total de acciones con debida diligencia aplicados con PEG`= sum(`Total de acciones con debida diligencia aplicados con PEG`, na.rm=T),
        `Total de acciones con debida diligencia`= sum(`Total de acciones con debida diligencia`)
      ) ->tabla_1
    # group_by(Año) %>% 
    # summarise(
    #   `Total de muertes violentas de mujeres`=sum(`Total de muertes violentas`/6), #aquí se debe dividir entre la cantidad de elecciones en la acción forense
    #   `Total de acciones con debida diligencia aplicados con PEG`= sum(Aplicado, na.rm=T),
    #   `Total de acciones con debida diligencia`= n(),
    #   
    #   Indicador=scales::percent(sum((`Total de acciones con debida diligencia aplicados con PEG`)/`Total de acciones con debida diligencia`), 0.1))->tabla_1
    # 
    tabla_1 %>% datatable(filter="top", extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'excel', 'print'),
                                         lengthMenu = list(c(6,10,20, -1),
                                                           c(6,10,20,"Todo"))))
  })  
  
  
  # Indicador 2: -----------------------------------------------------------------  
  
  
  output$ind_2_año <- renderUI({
    selectInput("ind_2_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_1$Año)),
                multiple = T)
  })
  
  output$ind_2_mes<- renderUI({
    selectInput("ind_2_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_2$Mes)),
                multiple = T)
  })
  
  # 
  # output$ind_2_dictamen <- renderUI({
  #   selectInput("ind_2_dictamen",
  #               label =  "Selecciona el tipo de dictamen",
  #               choices = sort(unique(indicador_2$`Dictamen psicosocial (servicio)`)),
  #               multiple = T)
  # })
  # 
  # 
  # output$ind_2_identificación <- renderUI({
  #   selectInput("ind_2_identificación",
  #               label =  "Selecciona",
  #               choices = sort(unique(indicador_2$`Oportuna identificación y entrevista a testigos (dictamen femi.)`)),
  #               multiple = T)
  # })
  
  
  ind_2_reactive <- reactive({
    
    indicador_2 %>% 
      filter(
        if(!is.null(input$ind_2_año))                        Año %in% input$ind_2_año             else Año != "",
        if(!is.null(input$ind_2_mes))                        Mes %in% input$ind_2_mes             else Mes != ""#,
        # if(!is.null(input$ind_2_dictamen))            `Dictamen psicosocial (servicio)` %in% input$ind_2_dictamen                                           else `Dictamen psicosocial (servicio)` != "",
        # if(!is.null(input$ind_2_identificación))      `Oportuna identificación y entrevista a testigos (dictamen femi.)` %in% input$ind_2_identificación    else `Oportuna identificación y entrevista a testigos (dictamen femi.)` != ""
        
      )
    
  })
  
  
  
  output$gr2 <-renderPlotly ({
    
    
    ind_2_reactive() %>% 
      # indicador_2 %>% 
      group_by(Año, Mes, Periodo) %>% 
      summarise(`Total de muertes violentas de mujeres`=n(),
                `Total de dictámenes psicosociales`= sum(`Dictamen psicosocial (servicio)`, na.rm = t)) %>% 
      pivot_longer(cols = 5,
                   names_to = "Dictámenes forenses",
                   values_to = "Total de dictámenes") %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nMuertes violentas: ", `Total de muertes violentas de mujeres`,
                          "\nTotal: ", scales::comma(`Total de dictámenes`), sep="")) %>%
      ggplot() +
      aes(x = Periodo, y = `Total de dictámenes`, 
          colour = `Dictámenes forenses`, group=`Dictámenes forenses`, text=text) +
      geom_line(size = 1.5) + 
      geom_point(size = 2) +
      geom_point(aes(x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"), shape = "asterisk", size = 4.5)+
      geom_line(aes( x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"),  linetype = "dashed", size = 1.5)+
      labs(x="", y="", title = "Indicador 2",
           color = "") +
      theme_minimal()+   
      #facet_wrap(vars(año))+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Total de dictámenes psicosociales` = "#c91682",
                   `Total de muertes violentas de mujeres`= "#7e3794")) +
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr2
    
    
    ggplotly(gr2, tooltip = "text") %>% 
      layout(title = "Indicador 2",
             legend = list(orientation = 'v', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_2 <- renderDataTable ({
    
    ind_2_reactive() %>% 
      group_by(Año) %>% 
      summarise(`Total de muertes violentas de mujeres`=n(),
                `Total de dictámenes psicosociales`= sum(`Dictamen psicosocial (servicio)`, na.rm = t),
                # `Indicador`=scales::percent(sum((`Total de dictámenes psicosociales`)/`Total de muertes violentas de mujeres`), 0.1)
      )->tabla_2
    
    
    
    tabla_2 %>% datatable(filter="top", extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'excel', 'print'),
                                         lengthMenu = list(c(6,10,20, -1),
                                                           c(6,10,20,"Todo"))))
    
    
  })  
  
  # Indicador 3: -----------------------------------------------------------------  
  
  output$ind_3_año <- renderUI({
    selectInput("ind_3_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_3$Año)),
                multiple = T)
  })
  
  output$ind_3_mes<- renderUI({
    selectInput("ind_3_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_3$Mes)),
                multiple = T)
  })
  
  
  output$ind_3_peritaje <- renderUI({
    selectInput("ind_3_peritaje",
                label =  "Selecciona el tipo de peritaje",
                choices = sort(unique(indicador_3$Peritaje)),
                multiple = T)
  })
  
  
  ind_3_reactive <- reactive({
    
    indicador_3 %>%
      filter(
        if(!is.null(input$ind_3_año))            Año %in% input$ind_3_año         else Año != "",
        if(!is.null(input$ind_3_mes))            Mes %in% input$ind_3_mes         else Mes != ""#,
        # if(!is.null(input$ind_3_peritaje))  Peritaje %in% input$ind_3_peritaje    else Peritaje != ""
      )
    
  })
  
  
  
  output$gr3 <-renderPlotly ({
    
    # ind_3_reactive() %>% 
    indicador_3 %>% 
      pivot_longer(cols = 6:30,
                   names_to = "Peritaje",
                   values_to = "Total de peritajes")%>%
      filter(`Total de peritajes`>0) %>% 
      group_by(Año, #Mes, Periodo, 
               Peritaje) %>% 
      summarise(`Peritajes a los cuales se aplicó la perspectiva de género`= n()) %>% 
      mutate(Peritaje=str_wrap(Peritaje, 28),
             text = paste("Año: ", Año,
                          # "\nMes", Mes,
                          "\nTotal: ", scales::comma(`Peritajes a los cuales se aplicó la perspectiva de género`), sep="")) %>%
      ggplot() +
      aes(x = Año, y = `Peritajes a los cuales se aplicó la perspectiva de género`, 
          colour = `Peritaje`, group=`Peritaje`, text=text) +
      geom_line(size = 1) + 
      geom_point(size = 1.5) +
      labs(x="", y="", title = "Indicador 3",
           color = "Peritajes a los cuales se aplicó la perspectiva de género") +
      # guides(colour=guide_legend(ncol=3))+
      
      theme_minimal()+   
      facet_wrap(.~Peritaje, scales = "free_y")+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values=mycolors)+
      theme(legend.position = "bottom")+
      theme(
        text=element_text(size=12,  family="Nutmeg-Light"),
        plot.title = element_text(family="Nutmeg-Light"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Nutmeg-Light"), 
        legend.position = "none")->gr3
    
    
    ggplotly(gr3, tooltip = "text") %>% 
      layout(title = "Indicador 3",
             #legend = list(orientation = 'v', x = 0, y = -.1), 
             
             legend = list(orientation = "h", x = 0, y=-.50, font = list(size = 10, bgcolor = 'rgb(251)')),
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_3 <- renderDataTable ({
    
    ind_3_reactive() %>% 
      # indicador_3 %>%
      # pivot_longer(cols = 6:30,
      #              names_to = "Peritaje",
      #              vindicador_3 %>%
      # pivot_longer(cols = 6:30,
      #              names_to = "Peritaje",
      #              values_to = "Total de peritajes")alues_to = "Total de peritajes")%>%
      group_by(Año) %>%
      summarise(#`total`=n(),
        `Total de muertes violentas de mujeres`=n(),
        # `Total de peritajes realizados en IJCF por violencia de género`=sum(`Total de peritajes`, na.rm = T),
        `Totalidad de peritajes de servicios forenses`=`Total de muertes violentas de mujeres`*25,
        `Peritajes a los cuales se aplicó la perspectiva de género`= sum(suma_dictamenes),
        # `Indicador`=scales::percent(sum((`Peritajes a los cuales se aplicó la perspectiva de género`)/`Totalidad de peritajes de servicios forenses`, na.rm = T), 0.1)
      ) ->tabla_3
    
    tabla_3%>% datatable(filter="top", extensions = 'Buttons',
                         options = list(dom = 'Blfrtip',
                                        buttons = c('copy', 'excel', 'print'),
                                        lengthMenu = list(c(6,10,20, -1),
                                                          c(6,10,20,"Todo")))) %>% 
      formatCurrency('Totalidad de peritajes de servicios forenses',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Peritajes a los cuales se aplicó la perspectiva de género',currency = "", interval = 3, mark = ",", digits = 0)
    
    
  })  
  
  
  # Indicador 4: -----------------------------------------------------------------  
  
  output$ind_4_año <- renderUI({
    selectInput("ind_4_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_4$Año)),
                multiple = T)
  })
  
  output$ind_4_mes<- renderUI({
    selectInput("ind_4_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_4$Mes)),
                multiple = T)
  })
  
  
  # output$ind_1_servicio <- renderUI({
  #   selectInput("ind_1_servicio",
  #               label =  "Selecciona el tipo de servicio forense",
  #               choices = sort(unique(indicador_1$`Servicios forenses`)),
  #               multiple = T)
  # })
  # 
  
  ind_4_reactive <- reactive({
    
    indicador_4 %>%
      filter(
        if(!is.null(input$ind_4_año))                        Año %in% input$ind_4_año             else Año != "",
        if(!is.null(input$ind_4_mes))                        Mes %in% input$ind_4_mes             else Mes != ""
        # ,
        # if(!is.null(input$ind_1_servicio))  `Servicios forenses` %in% input$ind_1_servicio         else `Servicios forenses` != ""
      )
    
  })
  
  
  
  output$gr4 <-renderPlotly ({
    
    
    ind_4_reactive() %>% 
      pivot_longer(cols = 6:8,
                   names_to = "Dictámenes",
                   values_to = "Total de dictámenes") %>% 
      group_by(Año, Mes, Periodo, Dictámenes) %>% 
      summarise(
        `Total de muertes violentas de mujeres`=sum(`Total de muertes violentas`),
        `Dictámenes con acreditación técnica`= sum(`Total de dictámenes`, na.rm=t)) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nMuertes violentas: ", `Total de muertes violentas de mujeres`,
                          
                          "\nTotal: ", scales::comma(`Dictámenes con acreditación técnica`), sep="")) %>%
      ggplot() +
      aes(x = Periodo, y = `Dictámenes con acreditación técnica`, 
          colour = `Dictámenes`, group=Dictámenes, text=text) +
      geom_line(size = 1.5) + 
      geom_point(size = 2) +
      geom_point(aes(x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"), shape = "asterisk", size = 4.5)+
      geom_line(aes( x=Periodo, y=`Total de muertes violentas de mujeres`, color="Total de muertes violentas de mujeres"),  linetype = "dashed", size = 1.5)+
      labs(x="", y="", title = "Indicador 4",
           color = "Dictámenes con acreditación técnica") +
      theme_minimal()+   
      facet_wrap(.~Dictámenes, scales = "free_y", ncol = 1)+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Genética mínimo 18 muestras` = "#c91682",
                   `Grafoscopía y documentoscopía` = "#7e3794",
                   `Balística` = "#d98cbc",
                   `Química`= "#b58cd9",
                   `Doc. cuestionario`= "#a544ff",
                   `Criminalística`= "#597dff",
                   `Total de muertes violentas de mujeres`= "chocolate"))+
      
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "Nutmeg-Light"))->gr4
    
    ggplotly(gr4, tooltip = "text") %>% 
      layout(title = "Indicador 4",
             legend = list(orientation = 'h', 
                           x = 0, y = -.3), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
  })
  
  output$t_4 <- renderDataTable ({
    
    ind_4_reactive() %>% 
      pivot_longer(cols = 6:8,
                   names_to = "Dictámenes",
                   values_to = "Total de peritajes") %>% 
      group_by(Año) %>%
      summarise(
        `Total de muertes violentas de mujeres`=sum(`Total de muertes violentas`/3),
        `Total de peritajes realizados`= n(),
        `Total de peritajes realizados conforme a lineamientos del IJCF`= sum(`Total de peritajes`, na.rm=t),
        # `Indicador`=scales::percent(sum((`Total de peritajes realizados conforme a lineamientos del IJCF`)/`Total de peritajes realizados`), 0.1)
      )->tabla_4
    
    
    
    
    
    tabla_4%>% datatable(filter="top", extensions = 'Buttons',
                         options = list(dom = 'Blfrtip',
                                        buttons = c('copy', 'excel', 'print'),
                                        lengthMenu = list(c(6,10,20, -1),
                                                          c(6,10,20,"Todo")))) 
    
  })
  
  
  
  # Indicador 6: -----------------------------------------------------------------  
  
  output$ind_6_año <- renderUI({
    selectInput("ind_6_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_6$Año)),
                multiple = T)
  })
  
  output$ind_6_mes<- renderUI({
    selectInput("ind_6_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_6$Mes)),
                multiple = T)
  })
  
  
  output$ind_6_municipio <- renderUI({
    selectInput("ind_6_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_6$Municipio)),
                multiple = T)
  })
  
  
  ind_6_reactive <- reactive({
    
    indicador_6 %>%
      filter(
        if(!is.null(input$ind_6_año))               Año %in% input$ind_6_año       else Año != "",
        if(!is.null(input$ind_6_mes))               Mes %in% input$ind_6_mes       else Mes != "",
        if(!is.null(input$ind_6_municipio))   Municipio %in% input$ind_6_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr6 <-renderPlotly ({
    
    ind_6_reactive() %>%
      #indicador_6 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Medidas de protección aceptadas`=sum(`Medidas de protección aceptadas`),
                `Medidas de protección rechazadas`=sum(`Medidas de protección rechazadas`),
                `Órdenes de protección aceptadas`=sum(`Órdenes de protección aceptadas`),
                `Órdenes de protección rechazadas`=sum(`Órdenes de protección rechazadas`)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:7) %>%
      mutate(text = paste("Año: ", Año,
                          "\nMes: ", Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total,
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 6",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y", ncol = 1)+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Medidas de protección aceptadas` = "#B58CD9",
          `Medidas de protección rechazadas` = "#D98CBC",
          `Órdenes de protección aceptadas` = "#7E3794",
          `Órdenes de protección rechazadas` = "#C91682"))+
      theme(legend.position = "bottom")+
      theme(#axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.position = "none", 
        text=element_text(size=12,  family="Nutmeg-Light"),
        plot.title = element_text(family="Nutmeg-Light"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr6
    
    
    
    ggplotly(gr6, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 6: ",ind_6_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_6 <- renderDataTable ({
    
    ind_6_reactive() %>%
      #indicador_6 %>% 
      group_by(Año) %>% 
      summarise(`Mujeres víctimas de violencia de género`=sum(`Mujeres víctimas de violencia de género`, na.rm=T),
                `Órdenes de protección`=sum(`Órdenes de protección aceptadas` + `Órdenes de protección rechazadas`),
                `Medidas de protección`=sum(`Medidas de protección aceptadas` +`Medidas de protección rechazadas`),
                # `Indicador`=scales::percent(sum((`Órdenes de protección`+`Medidas de protección`)/`Mujeres víctimas de violencia de género`), 0.1)
      ) ->tabla_6
    
    
    tabla_6 %>% datatable(filter="top", extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'excel', 'print'),
                                         lengthMenu = list(c(6,10,20, -1),
                                                           c(6,10,20,"Todo")))) %>% 
      formatCurrency('Mujeres víctimas de violencia de género',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Medidas de protección',currency = "", interval = 3, mark = ",", digits = 0)
    
    
    
  })  
  
  
  
  # Indicador 7: -----------------------------------------------------------------  
  
  output$ind_7_año <- renderUI({
    selectInput("ind_7_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_7$Año)),
                multiple = T)
  })
  
  output$ind_7_mes<- renderUI({
    selectInput("ind_7_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_7$Mes)),
                multiple = T)
  })
  
  
  output$ind_7_municipio <- renderUI({
    selectInput("ind_7_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_7$Municipio)),
                multiple = T)
  })
  
  
  ind_7_reactive <- reactive({
    
    indicador_7 %>%
      filter(
        if(!is.null(input$ind_7_año))               Año %in% input$ind_7_año       else Año != "",
        if(!is.null(input$ind_7_mes))               Mes %in% input$ind_7_mes       else Mes != "",
        if(!is.null(input$ind_7_municipio))   Municipio %in% input$ind_7_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr7 <-renderPlotly ({
    
    ind_7_reactive() %>%
      # indicador_7 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Total de mujeres víctimas de violencia de género atendidas`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
                `Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`),
                `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`))%>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:6) %>% 
      mutate(Clasificación=str_wrap(Clasificación, 60), 
             text = paste("Año: ", Año,
                          "\nMes", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(linewidth = 1.2) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 7",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
      scale_color_manual(
        values = c(
          `Total de mujeres víctimas de violencia de género atendidas` = "#d98cbc",
          `Total de mujeres víctimas de violencia de género que\nsolicitaron una medida de protección sin tener una\ncanalización formal` = "#c91682",
          `Total de mujeres víctimas de violencia de género que\nsolicitaron una orden de protección sin tener una\ncanalización formal` = "#7e3794"))+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr7
    
    
    ggplotly(gr7, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 7: ",ind_7_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_7 <- renderDataTable ({
    
    ind_7_reactive() %>%
      # indicador_7 %>% 
      group_by(Año) %>% 
      summarise(`Total de mujeres víctimas de violencia de género atendidas`=sum(`Total de mujeres víctimas de violencia de género atendidas`),
                `Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal`),
                `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`=sum(`Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`),
                # `Indicador`=scales::percent(sum(`Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal` + `Total de mujeres víctimas de violencia de género que solicitaron una orden de protección sin tener una canalización formal`)/`Total de mujeres víctimas de violencia de género atendidas`)
      ) -> tabla_7
    
    
    
    tabla_7 %>% datatable(filter="top", extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'excel', 'print'),
                                         lengthMenu = list(c(6,10,20, -1),
                                                           c(6,10,20,"Todo")))) %>% 
      formatCurrency('Total de mujeres víctimas de violencia de género atendidas',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Total de mujeres víctimas de violencia de género que solicitaron una medida de protección sin tener una canalización formal',currency = "", interval = 3, mark = ",", digits = 0)
    
  })  
  
  
  # Indicador 9: -----------------------------------------------------------------  
  
  output$ind_9_año <- renderUI({
    selectInput("ind_9_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_9$Año)),
                multiple = T)
  })
  
  output$ind_9_mes<- renderUI({
    selectInput("ind_9_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_9$Mes)),
                multiple = T)
  })
  
  
  output$ind_9_municipio <- renderUI({
    selectInput("ind_9_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_9$Municipio)),
                multiple = T)
  })
  
  
  ind_9_reactive <- reactive({
    
    indicador_9 %>%
      filter(
        if(!is.null(input$ind_9_año))               Año %in% input$ind_9_año       else Año != "",
        if(!is.null(input$ind_9_mes))               Mes %in% input$ind_9_mes       else Mes != "",
        if(!is.null(input$ind_9_municipio))   Municipio %in% input$ind_9_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr9 <-renderPlotly ({
    
    ind_9_reactive() %>%
      # indicador_9 %>%   
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Total de medidas de protección emitidas por violencia por razón de género vigentes`=sum(`Total de medidas de protección emitidas por violencia por razón de género vigentes`),
                
                `Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`=sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`),
                
                `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de medidas de protección emitidas por violencia por razón de género vigentes",
                          "Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas", 
                          "Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ", Fecha,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      
      #t9 %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(linewidth = 1.2) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 9",
           color = "Clasificación") +
      theme_minimal()+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Total de medidas de protección emitidas por violencia por razón de género vigentes` = "#d98cbc",
          `Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas` = "#c91682",
          `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora` = "#7e3794"))+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr9
    
    
    
    ggplotly(gr9, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 9: ",ind_9_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_9 <- renderDataTable ({
    
    ind_9_reactive() %>%
      #indicador_9 %>% 
      group_by(Año) %>% 
      summarise(`Total de medidas de protección emitidas por violencia por razón de género vigentes`=sum(`Total de medidas de protección emitidas por violencia por razón de género vigentes`),
                `Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`=sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`),
                `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`),
                # `Indicador`=scales::percent(sum(`Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas`+ `Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)/`Total de medidas de protección emitidas por violencia por razón de género vigentes`, 0.1)
      )-> tabla_9
    
    
    tabla_9 %>% datatable(filter="top", extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'excel', 'print'),
                                         lengthMenu = list(c(6,10,20, -1),
                                                           c(6,10,20,"Todo")))) %>% 
      formatCurrency('Total de medidas de protección emitidas por violencia por razón de género vigentes',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Medidas de protección emitidas por violencia por razón de género vigentes que fueron trabajadas',currency = "", interval = 3, mark = ",", digits = 0) %>%   
      formatCurrency('Medidas de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora',currency = "", interval = 3, mark = ",", digits = 0)
    
  })  
  
  
  
  # Indicador 10: -----------------------------------------------------------------  
  
  output$ind_10_año <- renderUI({
    selectInput("ind_10_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_10$Año)),
                multiple = T)
  })
  
  output$ind_10_mes<- renderUI({
    selectInput("ind_10_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_10$Mes)),
                multiple = T)
  })
  
  
  output$ind_10_municipio <- renderUI({
    selectInput("ind_10_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_10$Municipio)),
                multiple = T)
  })
  
  
  ind_10_reactive <- reactive({
    
    indicador_10 %>%
      filter(
        if(!is.null(input$ind_10_año))               Año %in% input$ind_10_año       else Año != "",
        if(!is.null(input$ind_10_mes))               Mes %in% input$ind_10_mes       else Mes != "",
        if(!is.null(input$ind_10_municipio))   Municipio %in% input$ind_10_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr10 <-renderPlotly ({
    
    ind_10_reactive() %>%
      # indicador_10 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Total de órdenes de protección emitidas por violencia por razón de género`=sum(`Total de órdenes de protección emitidas por violencia por razón de género`),
                
                `Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`),
                
                `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de órdenes de protección emitidas por violencia por razón de género",
                          "Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas",
                          "Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes:" , Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, colour = Clasificación, group= Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 10",
           color = " Clasificación ") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Total de órdenes de protección emitidas por violencia por razón de género` = "#d98cbc",
          `Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas` = "#c91682",
          `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora` = "#7e3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr10
    
    ggplotly(gr10, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 10: ",ind_10_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_10 <- renderDataTable ({
    
    ind_10_reactive() %>%
      # indicador_10 %>% 
      group_by(Año) %>% 
      summarise(`Total de órdenes de protección emitidas por violencia por razón de género`=sum(`Total de órdenes de protección emitidas por violencia por razón de género`),
                
                `Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`),
                
                `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`=sum(`Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`),
                
                # `Indicador`=scales::percent(sum(`Órdenes de protección emitidas por violencia por razón de género que fueron trabajadas`+ `Órdenes de protección emitidas por violencia por razón de género que fueron notificadas efectiva y personalmente a la persona agresora`)/`Total de órdenes de protección emitidas por violencia por razón de género`, 0.1)
      ) -> tabla_10
    
    
    tabla_10 %>% datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo"))))
    
  })  
  
  
  # Indicador 11: -----------------------------------------------------------------  
  
  output$ind_11_año <- renderUI({
    selectInput("ind_11_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_11$Año)),
                multiple = T)
  })
  
  output$ind_11_mes<- renderUI({
    selectInput("ind_11_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_11$Mes)),
                multiple = T)
  })
  
  
  output$ind_11_municipio <- renderUI({
    selectInput("ind_11_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_11$Municipio)),
                multiple = T)
  })
  
  
  ind_11_reactive <- reactive({
    
    indicador_11 %>%
      filter(
        if(!is.null(input$ind_11_año))               Año %in% input$ind_11_año       else Año != "",
        if(!is.null(input$ind_11_mes))               Mes %in% input$ind_11_mes       else Mes != "",
        if(!is.null(input$ind_11_municipio))   Municipio %in% input$ind_11_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr11 <-renderPlotly ({
    
    ind_11_reactive() %>%
      #indicador_11 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Mujeres con medidas de protección vigentes que han recibido seguimiento`=sum(`Mujeres con medidas de protección vigentes que han recibido seguimiento`),
                `Mujeres con órdenes de protección vigentes que han recibido seguimiento`=sum(`Mujeres con órdenes de protección vigentes que han recibido seguimiento`),
                `Total de mujeres con medidas de protección vigentes`=sum(`Total de mujeres con medidas de protección vigentes`),
                `Total de mujeres con órdenes de protección vigentes`=sum(`Total de mujeres con órdenes de protección vigentes`)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Mujeres con medidas de protección vigentes que han recibido seguimiento",
                          "Mujeres con órdenes de protección vigentes que han recibido seguimiento",
                          "Total de mujeres con medidas de protección vigentes",
                          "Total de mujeres con órdenes de protección vigentes")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x =Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 11",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y", ncol = 1)+ 
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Mujeres con medidas de protección vigentes que han recibido seguimiento` = "#b58cd9",
          `Mujeres con órdenes de protección vigentes que han recibido seguimiento` = "#d98cbc",
          `Total de mujeres con medidas de protección vigentes` = "#7e3794",
          `Total de mujeres con órdenes de protección vigentes` = "#c91682"))+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr11
    
    
    
    ggplotly(gr11, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 11: ",ind_11_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_11 <- renderDataTable ({
    
    ind_11_reactive() %>%
      #indicador_11 %>% 
      group_by(Año) %>% 
      summarise(`Mujeres con medidas de protección vigentes que han recibido seguimiento`=sum(`Mujeres con medidas de protección vigentes que han recibido seguimiento`),
                
                `Mujeres con órdenes de protección vigentes que han recibido seguimiento`=sum(`Mujeres con órdenes de protección vigentes que han recibido seguimiento`),
                
                `Total de mujeres con medidas de protección vigentes`=sum(`Total de mujeres con medidas de protección vigentes`),
                
                `Total de mujeres con órdenes de protección vigentes`=sum(`Total de mujeres con órdenes de protección vigentes`),
                
                # `Indicador`=scales::percent(sum((`Mujeres con medidas de protección vigentes que han recibido seguimiento`+ `Mujeres con órdenes de protección vigentes que han recibido seguimiento`)/(`Total de mujeres con medidas de protección vigentes` +`Total de mujeres con órdenes de protección vigentes`)), 0.1)
      ) -> tabla_11
    
    
    tabla_11 %>% datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo")))) %>% 
      formatCurrency('Mujeres con medidas de protección vigentes que han recibido seguimiento',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Total de mujeres con medidas de protección vigentes',currency = "", interval = 3, mark = ",", digits = 0)
    
    
    
    
  })  
  
  
  # Indicador 12: -----------------------------------------------------------------  
  
  output$ind_12_año <- renderUI({
    selectInput("ind_12_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_12$Año)),
                multiple = T)
  })
  
  output$ind_12_mes<- renderUI({
    selectInput("ind_12_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_12$Mes)),
                multiple = T)
  })
  
  
  output$ind_12_municipio <- renderUI({
    selectInput("ind_12_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_12$Municipio)),
                multiple = T)
  })
  
  
  ind_12_reactive <- reactive({
    
    indicador_12 %>%
      filter(
        if(!is.null(input$ind_12_año))               Año %in% input$ind_12_año       else Año != "",
        if(!is.null(input$ind_12_mes))               Mes %in% input$ind_12_mes       else Mes != "",
        if(!is.null(input$ind_12_municipio))   Municipio %in% input$ind_12_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr12 <-renderPlotly ({
    
    ind_12_reactive() %>%
      # indicador_12 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Total de medidas de protección emitidas vigentes`=sum(`Total de medidas de protección emitidas vigentes`),
                
                `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`),
                
                `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
                
                `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de medidas de protección emitidas vigentes",
                          "Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección",
                          "Total de órdenes de protección emitidas vigentes",
                          "Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección")) %>% 
      mutate(Clasificación=str_wrap(Clasificación, 55), 
             text = paste("Año: ", Año,
                          "\nMes: ",Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 12",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y", ncol=1)+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección` = "#b58cd9",
          `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección` = "#d98cbc",
          `Total de medidas de protección emitidas vigentes` = "#7e3794",
          `Total de órdenes de protección emitidas vigentes` = "#c91682"))+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr12
    
    
    
    ggplotly(gr12, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 12: ",ind_12_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_12 <- renderDataTable ({
    
    ind_12_reactive() %>%
      # indicador_12 %>%
      group_by(Año) %>% 
      summarise(`Total de medidas de protección emitidas vigentes`=sum(`Total de medidas de protección emitidas vigentes`),
                `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`),
                
                `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
                
                `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`=sum(`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`),
                
                # `Indicador`=scales::percent(sum((`Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de medidas de protección`+ `Total de carpetas de investigación iniciadas contra personas agresoras derivados del incumplimiento de órdenes de protección`)/(`Total de medidas de protección emitidas vigentes` +`Total de órdenes de protección emitidas vigentes`)), 0.1)
      ) -> tabla_12
    
    
    
    tabla_12 %>% datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo")),
                                          autoWidth = TRUE, 
                                          columnDefs = list(list(width = '50px', targets = "_all")))) %>% 
      formatCurrency('Total de medidas de protección emitidas vigentes',currency = "", interval = 3, mark = ",", digits = 0)
    
  })  
  
  
  
  # Indicador 13: -----------------------------------------------------------------  
  
  output$ind_13_año <- renderUI({
    selectInput("ind_13_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_13$Año)),
                multiple = T)
  })
  
  output$ind_13_mes<- renderUI({
    selectInput("ind_13_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_13$Mes)),
                multiple = T)
  })
  
  
  output$ind_13_municipio <- renderUI({
    selectInput("ind_13_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_13$Municipio)),
                multiple = T)
  })
  
  
  ind_13_reactive <- reactive({
    
    indicador_13 %>%
      filter(
        if(!is.null(input$ind_13_año))               Año %in% input$ind_13_año       else Año != "",
        if(!is.null(input$ind_13_mes))               Mes %in% input$ind_13_mes       else Mes != "",
        if(!is.null(input$ind_13_municipio))   Municipio %in% input$ind_13_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr13 <-renderPlotly ({
    
    ind_13_reactive() %>%
      #indicador_13 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(`Total medidas de protección emitidas vigentes`=sum(`Total medidas de protección emitidas vigentes`),
                `Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
                `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
                `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
                # `Indicador`=scales::percent(sum((`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`+ `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`)/(`Total medidas de protección emitidas vigentes` +`Total de órdenes de protección emitidas vigentes`)-1)*-1, 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total medidas de protección emitidas vigentes",
                          "Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)",
                          "Total de órdenes de protección emitidas vigentes",
                          "Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)")) %>% 
      mutate(Clasificación=str_wrap(Clasificación, 40), 
             text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 13",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y", ncol = 1)+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)` = "#b58cd9",
          `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)` = "#d98cbc",
          `Total de órdenes de protección emitidas vigentes` = "#7e3794",
          `Total medidas de protección emitidas vigentes` = "#c91682"))+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr13
    
    
    
    ggplotly(gr13, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 13: ",ind_13_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_13 <- renderDataTable ({
    
    ind_13_reactive() %>%
      #indicador_13 %>% 
      group_by(Año) %>% 
      summarise(`Total medidas de protección emitidas vigentes`=sum(`Total medidas de protección emitidas vigentes`),
                `Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
                `Total de órdenes de protección emitidas vigentes`=sum(`Total de órdenes de protección emitidas vigentes`),
                `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`=sum(`Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`),
                # `Indicador`=scales::percent(sum((`Casos en los que la medida de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`+ `Casos en los que la orden de protección no resultó ser adecuada y efectiva para la víctima (casos de reincidencia)`)/(`Total medidas de protección emitidas vigentes` +`Total de órdenes de protección emitidas vigentes`)-1)*-1, 0.1)
      ) -> tabla_13
    
    
    tabla_13 %>% datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo")))) %>% 
      formatCurrency('Total medidas de protección emitidas vigentes',currency = "", interval = 3, mark = ",", digits = 0)
    
    
    
  })  
  
  
  
  # # Indicador 16: -----------------------------------------------------------------  
  
  output$ind_16_año <- renderUI({
    selectInput("ind_16_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_16$Año)),
                multiple = T)
  })
  
  output$ind_16_mes<- renderUI({
    selectInput("ind_16_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_16$Mes)),
                multiple = T)
  })
  
  
  output$ind_16_edad <- renderUI({
    selectInput("ind_16_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_16$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_16_reactive <- reactive({
    
    indicador_16 %>%
      filter(
        if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
        if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
        if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr16 <-renderPlotly ({
    
    ind_16_reactive() %>%
      #indicador_16 %>%
      group_by(Año, Mes, Periodo, `Rango de edad`) %>%
      summarise(`Total de NA que denuncian ASI`=sum(`Total de mujeres que denuncian abuso sexual infantil`),
                
                `Total de NA canalizadas a Salud para atención integral por ASI`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                
                `Total de mujeres que denuncian violación`=sum(`Total de mujeres que denuncian violación`),
                
                `Total de mujeres canalizadas a salud para atención integral por violación`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por violación`)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de NA que denuncian ASI",
                          "Total de mujeres que denuncian violación",
                          "Total de NA canalizadas a Salud para atención integral por ASI",
                          "Total de mujeres canalizadas a salud para atención integral por violación")) %>%
      
      mutate(text = paste("Año: ", Año,
                          "\nMes: ", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>%
      filter(!Total==0) %>%
      ggplot() +
      aes(x = Periodo, y =`Rango de edad`, size=Total,
          fill = Total, group=Clasificación, text=text) +
      # geom_line(size = 1.5) + geom_point()+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      labs(x="", y="", #title = "Indicador 16\n\n\n",
           color = "Clasificación") +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=2, color="ghostwhite")+
      scale_size_continuous(range = c(3,15))+
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      facet_wrap(~ Clasificación, ncol = 1,
                 labeller = label_wrap_gen(width = 50, multi_line = TRUE)) +
      theme_minimal()+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light",  hjust=.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))  -> gr16
    
    
    
    ggplotly(gr16, tooltip = "text") %>%
      layout(#title = list(text = paste0(" Indicador 16: "#,ind_16_reactive()$`Rango de edad`
        # )),
        legend = list(orientation = 'v',  x = 0, y = -1),
        xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
    #
    # ggplotly(gr16, tooltip = "text") %>%
    #   layout(title = "Indicador 16",
    #          #legend = list(orientation = 'v', x = 0, y = -.1),
    #
    #          legend = list(orientation = "h", x = 0, y=-.50, font = list(size = 10, bgcolor = 'rgb(251)')),
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_16 <- renderDataTable ({
    
    ind_16_reactive() %>%
      #indicador_16 %>%
      rename(`Total de niñas que denuncian abuso sexual infantil`=`Total de mujeres que denuncian abuso sexual infantil`, 
             `Total de adolescentes canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`=`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`) %>% 
      group_by(Año) %>%
      summarise(`Total de NA que denuncian ASI`=sum(`Total de niñas que denuncian abuso sexual infantil`),
                
                `Total de NA canalizadas a Salud para atención integral por ASI`=sum(`Total de adolescentes canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                
                `Total de mujeres que denuncian violación`=sum(`Total de mujeres que denuncian violación`),
                
                `Total de mujeres canalizadas a salud para atención integral por violación`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por violación`),
                
                `% Abuso sexual infantil`=scales::percent(sum((`Total de NA canalizadas a Salud para atención integral por ASI`)/(`Total de NA que denuncian ASI`)), 0.1),
                `% Violación`=scales::percent(sum((`Total de mujeres canalizadas a salud para atención integral por violación`)/(`Total de mujeres que denuncian violación`)), 0.1)) -> tabla_16
    
    
    tabla_16 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
    
    
  })
  
  
  
  # Indicador 17: -----------------------------------------------------------------
  
  output$ind_17_año <- renderUI({
    selectInput("ind_17_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_17$Año)),
                multiple = T)
  })
  
  output$ind_17_mes<- renderUI({
    selectInput("ind_17_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_17$Month)),
                multiple = T)
  })
  
  
  output$ind_17_edad <- renderUI({
    selectInput("ind_17_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_17$Rango)),
                multiple = T)
  })
  
  
  ind_17_reactive <- reactive({
    
    indicador_17 %>%
      filter(
        if(!is.null(input$ind_17_año))     Año %in% input$ind_17_año    else Año != "",
        if(!is.null(input$ind_17_mes))     Mes %in% input$ind_17_mes    else Mes != "",
        if(!is.null(input$ind_17_edad))  Rango %in% input$ind_17_edad   else Rango != ""
      )
    
  })
  
  ind_16_reactive <- reactive({
    
    indicador_16 %>%
      filter(
        if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
        if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
        if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
      )
    
  })
  
  output$gr17 <-renderPlotly ({
    
    ind_17_reactive() %>%
      # indicador_17 %>%
      # mutate(Rango= case_when(
      #   `Edad (0-99)` <= 2 ~ "0 a 2 años",
      #   `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
      #   `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
      #   `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
      #   `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
      #   `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
      #   `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
      #   `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
      #   `Edad (0-99)` >= 60  ~ "60 en adelante"),
    #   Rango=factor(Rango,
    #                levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
    #                         "36 a 45 años", "46 a 59 años", "60 en adelante"))) %>%
    group_by(Año, Mes, Periodo, `Tipo: (abuso sexual infantil / violación)`, Rango) %>%
      summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>%
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`), sep=""))%>%
      ggplot() +
      aes(x = Periodo, y =Rango, text=text,
          size =`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`,
          colour = `Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`)+
      geom_point(mapping=aes(colour=`Tipo: (abuso sexual infantil / violación)`, group=`Tipo: (abuso sexual infantil / violación)`))+
      #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
      geom_text(aes(label=comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite")+
      # scale_y_discrete(limits = rev ,
      #                  labels = function(x) str_wrap(x, width = 25)) +
      scale_size_continuous(range = c(2,5))+
      scale_color_manual(
        values = c(
          `Abuso sexual infantil` = "#7e3794",
          `Violación` = "#c91682"
          #`Violencia sexual` = "#7e3794"
        ))+
      
      #facet_grid(.~ Año, space = 'free_x', scales = 'free_x', switch = 'x') +
      labs(x="", y="", title = "Indicador 17", fill="Delito", colour="Delito")+
      theme_minimal()+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr17
    
    
    
    ggplotly(gr17, tooltip = "text") %>%
      layout(title = list(text = paste0("Indicador 17: "#,ind_17_reactive()$Rango
      )
      ),
      legend = list(orientation = 'v',  x = 0, y = -1),
      xaxis = list(side = "bottom"),legend = list(side="bottom"))
    #
    # ggplotly(gr17, tooltip = "text") %>%
    #   layout(title = "Indicador 17",
    #          #legend = list(orientation = 'v', x = 0, y = -.1),
    #
    #          legend = list(orientation = "h", x = 0, y=-.50, font = list(size = 10, bgcolor = 'rgb(251)')),
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_17 <- DT::renderDataTable ({
    
    ind_16_reactive <- reactive({
      
      indicador_16 %>%
        filter(
          if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
          # if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
          if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
        )
      
    })
    
    
    ind_16_reactive() %>% 
      group_by(Año) %>%
      summarise(`Referidas por abuso sexual`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                `Referidas por violación`=sum(`Total de mujeres que denuncian violación`)) -> total_referida
    
    
    
    
    ind_17_reactive() %>%
      # indicador_17 %>%
      filter(!Año== 2019) %>% 
      group_by(Año,`Tipo: (abuso sexual infantil / violación)`) %>%
      summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>% 
      pivot_wider(names_from = "Tipo: (abuso sexual infantil / violación)", 
                  values_from = "Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud") %>% 
      cbind(total_referida) %>% 
      select(1:3,5:6) %>% 
      mutate(`% de atención por abuso sexual`= scales::percent(`Abuso sexual infantil`/`Referidas por abuso sexual`),
             `% de atención por violación`= scales::percent(`Violación`/`Referidas por violación`)) %>% 
      select(`Referidas por abuso sexual`,`Referidas por violación`, `Abuso sexual infantil`,`Violación`, `% de atención por abuso sexual`,`% de atención por violación`)->tabla_17
    
    
    
    
    tabla_17%>%  datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo"))))
  })
  
  
  # Indicador 18: -----------------------------------------------------------------
  
  output$ind_18_año <- renderUI({
    selectInput("ind_18_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_18$Año)),
                multiple = T)
  })
  
  # output$ind_18_mes<- renderUI({
  #   selectInput("ind_18_mes",
  #               label =  "Seleccione el mes",
  #               choices = sort(unique(indicador_18$Mes)),
  #               multiple = T)
  # })
  
  
  output$ind_18_edad <- renderUI({
    selectInput("ind_18_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_18$Rango)),
                multiple = T)
  })
  
  
  ind_18_reactive <- reactive({
    
    indicador_18 %>%
      filter(
        if(!is.null(input$ind_18_año))     Año %in% input$ind_18_año    else Año != "",
        # if(!is.null(input$ind_18_mes))     Mes %in% input$ind_18_mes    else Mes != "",
        if(!is.null(input$ind_18_edad))  Rango %in% input$ind_18_edad   else Rango != ""
      )
    
  })
  
  
  
  output$gr18 <-renderPlotly ({
    
    ind_18_reactive() %>%
      # indicador_18 %>% 
      filter(!is.na(Año)) %>% 
      group_by(Año, Rango, `Causal: (violacion/ salud/ riesgo)`) %>%
      summarise(`Total de mujeres víctimas de violación que recibieron el procedimiento`= n()) %>%
      mutate(text = paste("Año: ", Año,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`), sep="")) %>%
      ggplot() +
      aes(x = Año, y =Rango,
          size =`Total de mujeres víctimas de violación que recibieron el procedimiento`,
          colour = `Total de mujeres víctimas de violación que recibieron el procedimiento`, text=text) +
      geom_point(mapping=aes(colour=`Total de mujeres víctimas de violación que recibieron el procedimiento`, group=`Causal: (violacion/ salud/ riesgo)`))+
      theme(panel.grid.major = element_line(colour = "grey"))+
      #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
      geom_text(aes(label=comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=4, color="ghostwhite")+
      # scale_x_discrete(breaks = c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
      #                             "Septiembre", "Octubre","Noviembre", "Diciembre"))+
      # scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 25)) +
      scale_size_continuous(range = c(5,15))+
      # scale_color_manual(
      #       values = c(
      #         `Violación` = "#c91682"))+
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_colour_gradient(low = "#dba9c8", high = "#7e3794") +
      
      #facet_grid(.~ Año, space = 'free_x', scales = 'free_x', switch = 'x') +
      labs(x="", y="", title = "Indicador 18", fill="", colour="")+
      theme_minimal()+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr18
    
    
    
    ggplotly(gr18, tooltip = "text") %>%
      layout(title = list(text = paste0(" Indicador 18: "
                                        #              ,ind_18_reactive()$Rango)
      )),
      legend = list(orientation = 'v',  x = 0, y = -1),
      xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_18 <- DT::renderDataTable ({
    
    ind_18_reactive() %>%
      # indicador_18 %>%
      filter(Año %in% c(2022,2023)) %>% 
      # pivot_longer(cols = 6:30,
      #             names_to = "peritaje",
      #             values_to = "total de peritajes")%>%
      group_by(Año) %>%
      summarise(`Total de mujeres víctimas de violación que recibieron el procedimiento`= n(),
                `Total de canalización por Fiscalía`= sum(`Canalizadas por fiscalía: (si/no)`, na.rm = T),
                `Total de solicitudes directas`=sum(`Solicitud del procedimiento de manera directa a dependencias de salud: (si/no)`, na.rm = T)) ->tabla_18
    
    
    tabla_18%>%  datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo"))))
  })
  
  
  # Indicador 19: -----------------------------------------------------------------  
  
  output$ind_19_año <- renderUI({
    selectInput("ind_19_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_19$Año)),
                multiple = T)
  })
  
  # output$ind_19_mes<- renderUI({
  #   selectInput("ind_19_mes",
  #               label =  "Seleccione el mes",
  #               choices = sort(unique(indicador_19$Mes)),
  #               multiple = T)
  # })
  
  output$ind_19_edad <- renderUI({
    selectInput("ind_19_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_19$Rango)),
                multiple = T)
  })
  output$ind_19_causal <- renderUI({
    selectInput("ind_19_causal",
                label =  "Selecciona la causal",
                choices = sort(unique(indicador_19$`Causal: (salud/riesgo)`)),
                multiple = T)
  })
  
  
  ind_19_reactive <- reactive({
    
    indicador_19 %>%
      filter(
        if(!is.null(input$ind_19_año))                           Año %in% input$ind_19_año     else Año != "",
        # if(!is.null(input$ind_19_mes))                           Mes %in% input$ind_19_mes     else Mes != "",
        if(!is.null(input$ind_19_causal))   `Causal: (salud/riesgo)` %in% input$ind_19_causal  else `Causal: (salud/riesgo)` != "",
        if(!is.null(input$ind_19_edad))                        Rango %in% input$ind_19_edad    else Rango != ""
      )
    
  })
  
  
  
  output$gr19 <-renderPlotly ({
    
    ind_19_reactive() %>% 
      # indicador_19 %>%
      filter(!is.na(Año),
             !is.na(`Causal: (salud/riesgo)`)) %>% 
      group_by(Año, Rango, `Causal: (salud/riesgo)`) %>% 
      summarise(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`= n()) %>% 
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ", Mes,
                          "\nCausal: ", `Causal: (salud/riesgo)`,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`), sep="")) %>%
      # ggplot() +
      # aes(x = Periodo, size =Rango,
      #     weight =`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
      #     colour = `Causal: (salud/riesgo)`, group=`Causal: (salud/riesgo)`, text=text) +
      #     geom_point(mapping=aes(colour=`Causal: (salud/riesgo)`, group=`Causal: (salud/riesgo)`))+
      ggplot() +
      aes(x =as.factor(Año) , y = Rango, text=text,
          fill = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
          colour = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
          size=`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`) +
      #geom_raster()+
      geom_point()+
      # geom_tile(color = "white",
      #           lwd = 1,
      #           linetype = 1) +
      geom_text(aes(label=comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=90)+
      facet_grid(.~ `Causal: (salud/riesgo)`, space = 'free_x', scales = 'free_x', switch = 'x') +
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_color_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_size_continuous(range = c(4,9))+
      
      
      #scale_x_discrete(limits = rev,labels = function(x) str_wrap(x, width = 5)) + 
      
      labs(x="", y="", title = "Indicador 19", fill="", colour="")+
      theme_minimal()+
      theme(legend.position = "none")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr19
    
    
    
    
    ggplotly(gr19, tooltip = "text") %>%
      layout(title = list(text = paste0("Indicador 19: "#,ind_16_reactive()$`Rango de edad`
      )),
      legend = list(orientation = 'v',  x = 0, y = -1),
      xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
    # 
    # ggplotly(gr16, tooltip = "text") %>% 
    #   layout(title = "Indicador 16",
    #          #legend = list(orientation = 'v', x = 0, y = -.1), 
    #          
    #          legend = list(orientation = "h", x = 0, y=-.50, font = list(size = 10, bgcolor = 'rgb(251)')),
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_19 <- DT::renderDataTable ({
    
    ind_19_reactive() %>% 
      # indicador_19 %>% 
      group_by(Año) %>%
      summarise(`Total de mujeres que solicitaron el procedimiento de interrupción legal del embarazo`= n(),
                `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`= sum(`¿Se realizó el procedimiento? (sí/no)`),
                # `Indicador`=scales::percent((`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`)/(`Total de mujeres que solicitaron el procedimiento de interrupción legal del embarazo`), 0.1)
      )->tabla_19
    
    
    tabla_19%>%  datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo"))))
    
    
  })  
  
  # Indicador 20: -----------------------------------------------------------------  
  
  output$ind_20_año <- renderUI({
    selectInput("ind_20_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_20$Año)),
                multiple = T)
  })
  
  output$ind_20_tipo<- renderUI({
    selectInput("ind_20_tipo",
                label =  "Seleccione el tipo de violencia",
                choices = sort(unique(indicador_20$`Tipo de violencia: (violencia familiar / sexual)`)),
                multiple = T)
  })
  
  # output$ind_20_modalidad <- renderUI({
  #   selectInput("ind_20_modalidad",
  #               label =  "Selecciona la modalidad de violencia",
  #               choices = sort(unique(indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)),
  #               multiple = T)
  # })
  
  
  ind_20_reactive <- reactive({
    
    indicador_20 %>%
      filter(
        if(!is.null(input$ind_20_año))               Año %in% input$ind_20_año      else Año != "",
        if(!is.null(input$ind_20_tipo))       `Tipo de violencia: (violencia familiar / sexual)` %in% input$ind_20_tipo     else `Tipo de violencia: (violencia familiar / sexual)` != "",
        if(!is.null(input$ind_20_modalidad))  `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)` %in% input$ind_20_modalidad  else`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)` != ""
      )
    
  })
  
  
  
  output$gr20 <-renderPlotly ({
    
    ind_20_reactive() %>% 
      # indicador_20 %>% 
      filter(!is.na(`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)) %>% 
      # mutate(`Tipo de violencia: (violencia familiar / sexual)`= case_when( 
      #   
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA FISICA"  ~ "Violencia física",
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA FÍSICA"  ~ "Violencia física",
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA FíSICA"  ~ "Violencia física",
      #   
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA PSICOLÓGICA"  ~ "Violencia psicológica",
      #   
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA SEXUAL"  ~ "Violencia sexual",
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA VIOLENCIA SEXUAL"  ~ "Violencia sexual",
      #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA ECONÓMICA / PATRIMONIAL"  ~ "Violencia económica / patrimonial",
    #   
    #   `Tipo de violencia: (violencia familiar / sexual)`== "ABANDONO Y/O NEGLIGENCIA"  ~ "Abandono y/o negligencia",
    #   `Tipo de violencia: (violencia familiar / sexual)`== "ABANDO/NEGLIGENCIA"  ~ "Abandono y/o negligencia",
    #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA PSICOLOGICA"  ~ "Violencia psicológica",
    #   
    #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA ECONÓMICA / PATRIMONIAL"  ~ "Violencia económica / patrimonial",
    #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA ECONOMICA/PATRIMONIAL"  ~ "Violencia económica / patrimonial",
    #   `Tipo de violencia: (violencia familiar / sexual)`== "VIOLENCIA ECONÓMICA / PATRIMONIAL"  ~ "Violencia económica / patrimonial")) %>%
    # 
    # filter(`Tipo de violencia: (violencia familiar / sexual)` %in% c("Violencia física", "Violencia psicológica", 
    #                                                                  "Violencia sexual", "Violencia económica / patrimonial",
    #                                                                  "Abandono y/o negligencia" )) %>% 
    group_by(Año, `Tipo de violencia: (violencia familiar / sexual)`, 
             `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`) %>% 
      summarise(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`= sum(`Notificadas al mp: (si/no)`)) %>%  
      filter(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`>=1) %>% 
      mutate(text = paste("Año: ", Año,
                          #"Periodo: ",  format(as_date(PERIODO), "%B de %Y"),
                          "\nModalidad de violencia: ", `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`,
                          "\nTipo de violencia: ", `Tipo de violencia: (violencia familiar / sexual)`,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`), sep=""))%>%
      ggplot() +
      aes(x = as.factor(Año), y = `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`,
          size =`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`,
          colour = `Tipo de violencia: (violencia familiar / sexual)`, group=`Tipo de violencia: (violencia familiar / sexual)`, text=text) +
      geom_point(mapping=aes(colour=`Tipo de violencia: (violencia familiar / sexual)`, group=`Tipo de violencia: (violencia familiar / sexual)`))+
      theme(panel.grid.major = element_line(colour = "grey"))+
      #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +  
      geom_text(aes(label=comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite")+
      # scale_x_discrete(breaks = c("enero", "febrero", "marzo","abril", "mayo", "junio","julio", "agosto",
      #                            "septiembre", "octubre","noviembre", "diciembre"))+
      scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 17)) + 
      
      scale_size_continuous(range = c(4,9))+
      scale_color_manual(
        values = c(
          `Violencia física` = "#7e3794",
          `Violencia psicológica` = "#c91682",
          `Violencia sexual`= "#26827f",
          `Violencia económica`= "#82263d",
          `Violencia familiar`= "#a544ff",
          `Otro tipo`= "#597dff")) +
      facet_grid(.~ `Tipo de violencia: (violencia familiar / sexual)`, 
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
      labs(x="", y="", title = "indicador 20")+
      theme_minimal()+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            strip.text = element_text(size = 9, family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.y = element_text(size=5, family="Nutmeg-Light"))-> gr20
    
    
    
    
    ggplotly(gr20, tooltip = "text") %>% 
      layout(title = "Indicador 20",
             legend = list(orientation = 'v', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom")) %>% 
      layout(margin = list(b=-5,t=140), 
             annotations =
               list(
                 x = .7, y = -3,  
                 text = "",
                 #                 text = "Datos de cerodesabasto.org",
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=6,
                 font=list(size=10, color="#9443FF")))
    
    
  })
  
  output$t_20 <- DT::renderDataTable ({
    
    ind_20_reactive() %>% 
      # indicador_20 %>% 
      group_by(Año) %>%
      summarise(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`= n(),
                `Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía`= sum(`Notificadas al mp: (si/no)`),
                # `Indicador`=scales::percent((`Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía`)/(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`), 0.1)
      )->tabla_20
    
    
    tabla_20%>%  datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo")))) %>% 
      formatCurrency('Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud',currency = "", interval = 3, mark = ",", digits = 0) %>%          formatCurrency('Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía',currency = "", interval = 3, mark = ",", digits = 0)
    
    
  })  
  
  
  
  # Indicador 21: -----------------------------------------------------------------  
  
  output$ind_21_año <- renderUI({
    selectInput("ind_21_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_21$Año)),
                multiple = T)
  })
  
  output$ind_21_establecimiento<- renderUI({
    selectInput("ind_21_establecimiento",
                label =  "Seleccione el hospital",
                choices = sort(unique(indicador_21$Establecimiento)),
                multiple = T)
  })
  
  ind_21_reactive <- reactive({
    
    indicador_21 %>%
      filter(
        if(!is.null(input$ind_21_año))                          Año %in% input$ind_21_año              else Año != "",
        if(!is.null(input$ind_21_establecimiento))  Establecimiento %in% input$ind_21_establecimiento  else Establecimiento != ""
      )
    
  })
  
  
  
  
  output$t_21 <- DT::renderDataTable ({
    
    ind_21_reactive() %>% 
      # indicador_21 %>% 
      group_by(Año) %>%
      summarise(`Total de unidades en condiciones óptimas para realizar ILE/IVE` = sum(`Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`),
                `Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile`= n(),
                # `Indicador`=scales::percent((`Total de unidades en condiciones óptimas para realizar ILE/IVE`)/(`Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile`), 0.1)
      )->tabla_21
    
    tabla_21%>% datatable(filter="top", options = list(pageLength = 6))
    
  })  
  
  
  
  
  # Indicador 22: -----------------------------------------------------------------  
  
  output$ind_22_año <- renderUI({
    selectInput("ind_22_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_22$Año)),
                multiple = T)
  })
  
  output$ind_22_formación<- renderUI({
    selectInput("ind_22_formación",
                label =  "Seleccione la formación del personal",
                choices = sort(unique(indicador_22$Formación)),
                multiple = T)
  })
  
  ind_22_reactive <- reactive({
    
    indicador_22 %>%
      filter(
        if(!is.null(input$ind_22_año))              Año %in% input$ind_22_año         else Año != "",
        if(!is.null(input$ind_22_formación))  Formación %in% input$ind_22_formación   else Formación != ""
      )
    
  })
  
  output$gr22 <-renderPlotly ({
    
    ind_22_reactive() %>% 
      # indicador_22 %>% 
      # group_by(Periodo, Formación) %>% 
      group_by(Formación) %>% 
      summarise(`Total de personal de salud que atiende ILE/IVE capacitado`= n()) %>% 
      mutate(text = paste(#"año: ", periodo,
        # "Periodo: ",  format(as_date(Periodo), "%b de %y"),
        "\nFormación del personal: ", Formación,
        "\nTotal: ", scales::comma(`Total de personal de salud que atiende ILE/IVE capacitado`), sep=""))%>%
      ggplot() +
      aes(x = reorder(Formación, -`Total de personal de salud que atiende ILE/IVE capacitado`),
          y =`Total de personal de salud que atiende ILE/IVE capacitado`,
          colour = Formación, group=Formación, text=text) +
      geom_line(size = 1.5) + 
      geom_point(size = 2) +
      # scale_fill_manual(values = mycolors) +
      # scale_color_manual(values=mycolors)+      
      # scale_x_discrete(breaks = c("enero", "febrero", "marzo","abril", "mayo", "junio","julio", "agosto",
      #                            "septiembre", "octubre","noviembre", "diciembre"))+
      #scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 25)) + 
      # scale_color_manual(
      #     values = c(
      #       `daño a la salud` = "#d98cbc",
      #       `peligro de muerte` = "#c91682",
      #       `causal de violación` = "#7e3794"))+
      labs(x="", y="", title = "Indicador 22")+
      theme_minimal()+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            strip.text = element_text(size = 9, family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.y = element_text(size=7, family="Nutmeg-Light"))->gr22
    
    
    
    ggplotly(gr22, tooltip = "text") %>% 
      layout(title = "Indicador 22",
             legend = list(orientation = 'h', 
                           x = 0, y = -1.4), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
  })
  
  
  output$t_22 <- DT::renderDataTable ({
    
    ind_22_reactive() %>% 
      # indicador_22 %>% 
      group_by(Año) %>%
      summarise(`Total de personal de salud que atiende ILE/IVE capacitado`= n(),
                `Total de personal de salud que atiende ILE/IVE`= n(),
                # `Indicador`=scales::percent((`Total de personal de salud que atiende ILE/IVE capacitado`)/(`Total de personal de salud que atiende ILE/IVE`), 0.1)
      )->tabla_22
    
    
    tabla_22%>% datatable(filter="top", options = list(pageLength = 6))
    
  })  
  
  
  
  # Indicador 23: -----------------------------------------------------------------  
  
  output$ind_23_año <- renderUI({
    selectInput("ind_23_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_23$Año)),
                multiple = T)
  })
  
  output$ind_23_mes<- renderUI({
    selectInput("ind_23_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_23$Mes)),
                multiple = T)
  })
  
  
  output$ind_23_formación <- renderUI({
    selectInput("ind_23_formación",
                label =  "Selecciona la formación del personal medicx",
                choices = sort(unique(indicador_23$Formación)),
                multiple = T)
  })
  
  
  ind_23_reactive <- reactive({
    
    indicador_23 %>%
      filter(
        if(!is.null(input$ind_23_año))               Año %in% input$ind_23_año       else Año != "",
        if(!is.null(input$ind_23_mes))               Mes %in% input$ind_23_mes       else Mes != "",
        if(!is.null(input$ind_23_formación))   Formación %in% input$ind_23_formación else Formación != ""
      )
    
  })
  
  
  
  output$gr23 <-renderPlotly ({
    
    ind_23_reactive() %>%
      #indicador_23 %>% 
      group_by(Año, Mes, Periodo, Función) %>% 
      summarise(`Personal médico no objetor de conciencia`=sum(`Objetor de conciencia: (SI/NO)`, na.rm = T),
                `Total de personal médico debidamente capacitado`= n()) %>% 
      mutate(Función=case_when(
        Función=="ADSCRITO"~"Medicx adscritx",
        Función=="Medico Adscrito "~"Médicx Adscritx",
        Función=="Enfermería"~"Enfermera",
        T~"Otro")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nFunción del personal : ", Función, sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = `Personal médico no objetor de conciencia`, 
          colour = Función, group=Función, 
          size=`Personal médico no objetor de conciencia`, text=text) +
      #  geom_line(size = 1.5) + 
      geom_point()+
      geom_text(aes(label=comma(`Personal médico no objetor de conciencia`, accuracy = 1)), size=3, color="ghostwhite")+
      scale_size_continuous(range = c(3,10))+
      labs(x="", y="", title = "Indicador 23") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Medicx adscritx` = "#D98CBC",
          `Enfermera` = "#7E3794",
          `Otro` = "#C91682"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr23
    
    
    
    ggplotly(gr23, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 23 ")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_23 <- renderDataTable ({
    
    ind_23_reactive() %>%
      #indicador_23 %>%
      group_by(Año) %>% 
      summarise(`Personal médico no objetor de conciencia`=sum(`Objetor de conciencia: (SI/NO)`, na.rm = T),
                `Total de personal médico debidamente capacitado`= n(),
                # `Indicador`=scales::percent(sum(`Personal médico no objetor de conciencia`/`Total de personal médico debidamente capacitado`), 0.1)
      ) -> tabla_23
    
    
    tabla_23 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
  })  
  
  
  
  # Indicador 24: -----------------------------------------------------------------  
  
  output$ind_24_año <- renderUI({
    selectInput("ind_24_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_24$Año)),
                multiple = T)
  })
  
  output$ind_24_mes<- renderUI({
    selectInput("ind_24_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_24$Mes)),
                multiple = T)
  })
  
  
  output$ind_24_municipio <- renderUI({
    selectInput("ind_24_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_24$Municipio)),
                multiple = T)
  })
  
  
  ind_24_reactive <- reactive({
    
    indicador_24 %>%
      filter(
        if(!is.null(input$ind_24_año))               Año %in% input$ind_24_año       else Año != "",
        if(!is.null(input$ind_24_mes))               Mes %in% input$ind_24_mes       else Mes != "",
        if(!is.null(input$ind_24_municipio))   Municipio %in% input$ind_24_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr24 <-renderPlotly ({
    
    ind_24_reactive() %>%
      #indicador_24 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(Actualizan=sum(Actualizan),
                Instancia=sum(Instancia),
                `Indicador`=scales::percent(sum(Actualizan/Instancia), 0.1)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Actualizan",
                          "Instancia")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 24",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y") +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Actualizan` = "#C91682",
                   `Instancia` = "#7E3794"))+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr24
    
    
    
    ggplotly(gr24, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 24: ",ind_24_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_24 <- renderDataTable ({
    
    ind_24_reactive() %>%
      #indicador_24 %>% 
      group_by(Año) %>% 
      summarise(Actualizan=sum(Actualizan, na.rm=T),
                Instancia=sum(Instancia, na.rm=T),
                # `Indicador`=scales::percent(sum(Actualizan/Instancia), 0.1)
      ) -> tabla_24
    
    
    tabla_24 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
    
  })  
  
  
  
  # Indicador 25: -----------------------------------------------------------------  
  
  output$ind_25_año <- renderUI({
    selectInput("ind_25_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_25$Año)),
                multiple = T)
  })
  
  output$ind_25_mes<- renderUI({
    selectInput("ind_25_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_25$Mes)),
                multiple = T)
  })
  
  
  output$ind_25_nivel <- renderUI({
    selectInput("ind_25_nivel",
                label =  "Selecciona el nivel",
                choices = sort(unique(indicador_25$Nivel)),
                multiple = T)
  })
  
  
  ind_25_reactive <- reactive({
    
    indicador_25 %>%
      filter(
        if(!is.null(input$ind_25_año))       Año %in% input$ind_24_año      else Año != "",
        if(!is.null(input$ind_25_mes))       Mes %in% input$ind_24_mes      else Mes != "",
        if(!is.null(input$ind_25_nivel))   Nivel %in% input$ind_25_nivel    else Nivel != ""
      )
    
  })
  
  
  
  output$gr25 <-renderPlotly ({
    
    ind_25_reactive() %>%
      #indicador_25 %>% 
      group_by(Año, Mes, Periodo) %>% 
      summarise(`Personal responsable de la actualización de datos`=sum(`Personal responsable de la actualización de datos`),
                
                `Personas debidamente capacitadas de alimentar BANAVIM`=sum(`Personas debidamente capacitadas de alimentar BANAVIM`),
                
                # `Indicador`=scales::percent(sum(`Personas debidamente capacitadas de alimentar BANAVIM`/`Personal responsable de la actualización de datos`), 0.1)
      )%>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Personal responsable de la actualización de datos",
                          "Personas debidamente capacitadas de alimentar BANAVIM")) %>%
      
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 25",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Personal responsable de la actualización de datos` = "#C91682",
                   `Personas debidamente capacitadas de alimentar BANAVIM` = "#7E3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr25
    
    
    ggplotly(gr25, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 25:")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_25 <- renderDataTable ({
    
    ind_25_reactive() %>%
      # indicador_25 %>% 
      group_by(Año) %>% 
      summarise(`Personal responsable de la actualización de datos`=sum(`Personal responsable de la actualización de datos`),
                
                `Personas debidamente capacitadas de alimentar BANAVIM`=sum(`Personas debidamente capacitadas de alimentar BANAVIM`),
                
                # `Indicador`=scales::percent(sum(`Personas debidamente capacitadas de alimentar BANAVIM`/`Personal responsable de la actualización de datos`), 0.1)
      ) -> tabla_25 
    
    tabla_25 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) 
    
    
  })  
  
  
  
  
  
  # Indicador 26: -----------------------------------------------------------------  
  
  output$ind_26_año <- renderUI({
    selectInput("ind_26_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_26$Año)),
                multiple = T)
  })
  
  output$ind_26_mes<- renderUI({
    selectInput("ind_26_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_26$Mes)),
                multiple = T)
  })
  
  
  
  ind_26_reactive <- reactive({
    
    indicador_26 %>%
      filter(
        if(!is.null(input$ind_26_año))       Año %in% input$ind_26_año      else Año != "",
        if(!is.null(input$ind_26_mes))       Mes %in% input$ind_26_mes      else Mes != ""
      )
    
  })
  
  
  
  output$gr26 <-renderPlotly ({
    
    ind_26_reactive() %>%
      #indicador_26 %>% 
      group_by(Año, Mes, Periodo) %>% 
      summarise(Actualización=sum(Actualización),
                `Indicador`=scales::percent(sum(Actualización/7), 0.1)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Actualización")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 26",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Actualización` = "#C91682"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr26
    
    
    ggplotly(gr26, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 26:")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_26 <- renderDataTable ({
    
    ind_26_reactive() %>%
      # indicador_26 %>% 
      group_by(Año) %>% 
      summarise(`Instancias que sí actualización`=sum(Actualización, na.rm=T),
                `Total de instancias estatales responsables`=n(),
                # `Indicador`=scales::percent(sum(Actualización/`Total de instancias estatales responsables`), 0.1, na.rm=T)
      ) -> tabla_26
    
    
    tabla_26 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))    
    
  })  
  
  
  
  # Indicador 27: -----------------------------------------------------------------  
  
  output$ind_27_año <- renderUI({
    selectInput("ind_27_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_27$Año)),
                multiple = T)
  })
  
  output$ind_27_mes<- renderUI({
    selectInput("ind_27_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_27$Mes)),
                multiple = T)
  })
  
  
  output$ind_27_delito <- renderUI({
    selectInput("ind_27_municipio",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_27$Delito)),
                multiple = T)
  })
  
  
  ind_27_reactive <- reactive({
    
    indicador_27 %>%
      filter(
        if(!is.null(input$ind_27_año))         Año %in% input$ind_27_año     else Año != "",
        if(!is.null(input$ind_27_mes))         Mes %in% input$ind_27_mes     else Mes != "",
        if(!is.null(input$ind_27_delito))   Delito %in% input$ind_27_delito  else Delito != ""
      )
    
  })
  
  
  
  output$gr27 <-renderPlotly ({
    
    ind_27_reactive() %>%
      #indicador_27 %>% 
      group_by(Año, #Mes,Periodo,
               Delito) %>% 
      summarise(`Número de casos analizados`=sum(`Número de casos analizados`),
                `Total opiniones técnicas`=sum(`Total opiniones técnicas`),
                # `Indicador`=scales::percent(sum(`Número de casos analizados`/`Total opiniones técnicas`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Número de casos analizados",
                          "Total opiniones técnicas")) %>% 
      mutate(text = paste("Año: ", Año,
                          #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nTotal : ", scales::comma(Total), sep="")) %>% 
      filter(!Total ==0) %>% 
      ggplot() +
      aes(x = Año , y = Total, fill = Delito,
          colour = Delito, group=Delito,
          text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      
      labs(x="", y="", title = "Indicador 27",
           color = "Delito",  fill = "Delito") +
      facet_grid(.~ Clasificación, 
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      # scale_fill_manual(
      #   values = c(`Número de casos analizados` = "#C91682",
      #              `Total opiniones técnicas` = "#7E3794"))+
      # scale_colour_manual(
      #   values = c(`Número de casos analizados` = "#C91682",
      #              `Total opiniones técnicas` = "#7E3794"))+
      scale_fill_manual(
        values = c("#C91682","#7E3794"))+
      scale_colour_manual(
        values = c("#C91682","#7E3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr27
    
    
    
    ggplotly(gr27, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 27: ")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_27 <- renderDataTable ({
    
    ind_27_reactive() %>%
      # indicador_27 %>% 
      group_by(Año, Delito) %>% 
      summarise(`Opiniones técnicas`=sum(`Total opiniones técnicas`),
                `Casos analizados`=sum(`Número de casos analizados`)) %>% 
      pivot_wider(names_from = "Delito",
                  values_from = c(`Casos analizados`, `Opiniones técnicas`)) %>% 
      select(Año, `Casos analizados_Desaparición`,   `Opiniones técnicas_Desaparición`, `Casos analizados_Feminicidio`, `Opiniones técnicas_Feminicidio`)-> tabla_27
    
    names(tabla_27)[names(tabla_27) == "Casos analizados_Desaparición"] <- "Casos analizados desaparición"
    names(tabla_27)[names(tabla_27) == "Opiniones técnicas_Desaparición"] <- "Opiniones técnicas desaparición"
    
    names(tabla_27)[names(tabla_27) == "Casos analizados_Feminicidio"] <- "Casos analizados feminicidio"
    names(tabla_27)[names(tabla_27) == "Opiniones técnicas_Feminicidio"] <- "Opiniones técnicas feminicidio"
    
    
    tabla_27 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) 
    
  })  
  
  
  
  
  # Indicador 28: -----------------------------------------------------------------  
  
  output$ind_28_año <- renderUI({
    selectInput("ind_28_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_28$Año)),
                multiple = T)
  })
  
  output$ind_28_mes<- renderUI({
    selectInput("ind_28_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_28$Mes)),
                multiple = T)
  })
  
  
  output$ind_28_municipio <- renderUI({
    selectInput("ind_28_municipio",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_28$Municipio)),
                multiple = T)
  })
  
  
  ind_28_reactive <- reactive({
    
    indicador_28 %>%
      filter(
        if(!is.null(input$ind_28_año))               Año %in% input$ind_28_año        else Año != "",
        if(!is.null(input$ind_28_mes))               Mes %in% input$ind_28_mes        else Mes != "",
        if(!is.null(input$ind_28_municipio))   Municipio %in% input$ind_28_municipio  else Municipio != ""
      )
    
  })
  
  
  
  output$gr28 <-renderPlotly ({
    
    ind_28_reactive() %>%
      # indicador_28 %>% 
      group_by(Año, Municipio) %>% 
      summarise(`Mujeres que realizan una denuncia por el delito de violacion`=sum(`Mujeres que realizan una denuncia por el delito de violacion`, na.rm=T),
                `Mujeres atendidas en el CJM`=sum(`Mujeres atendidas en el CJM`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violencia familiar`=sum(`Mujeres que realizan una denuncia por el delito de violencia familiar`, na.rm=T),
                # `Indicador`=scales::percent(sum((`Mujeres que realizan una denuncia por el delito de violacion`+ `Mujeres que realizan una denuncia por el delito de violencia familiar`)/`Mujeres atendidas en el CJM`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Mujeres atendidas en el CJM",
                          "Mujeres que realizan una denuncia por el delito de violacion",
                          "Mujeres que realizan una denuncia por el delito de violencia familiar")) %>% 
      mutate(text = paste("Año: ", Año,
                          #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Año, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 28",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(.~Municipio, scales = "free") +
      scale_color_manual(
        values = c(
          `Mujeres atendidas en el CJM` = "#D98CBC",
          `Mujeres que realizan una denuncia por el delito de violacion` = "#C91682",
          `Mujeres que realizan una denuncia por el delito de violencia familiar` = "#7E3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr28
    
    
    
    ggplotly(gr28, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 28: "#, ind_28_reactive$Municipio)
      )),
      legend = list(orientation = 'v',  x = 0, y = -1), 
      xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_28 <- renderDataTable ({
    
    ind_28_reactive() %>%
      # indicador_28 %>% 
      group_by(Año) %>% 
      summarise(`Mujeres atendidas en el CJM`=sum(`Mujeres atendidas en el CJM`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violacion`=sum(`Mujeres que realizan una denuncia por el delito de violacion`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violencia familiar`=sum(`Mujeres que realizan una denuncia por el delito de violencia familiar`, na.rm=T),
                # `Indicador`=scales::percent(sum((`Mujeres que realizan una denuncia por el delito de violacion`+ `Mujeres que realizan una denuncia por el delito de violencia familiar`)/`Mujeres atendidas en el CJM`), 0.1, na.rm=T)
      ) -> tabla_28
    
    
    
    
    tabla_28 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) %>% 
      formatCurrency('Mujeres atendidas en el CJM',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Mujeres que realizan una denuncia por el delito de violencia familiar',currency = "", interval = 3, mark = ",", digits = 0)
    
    
  })  
  
  # Indicador 29: -----------------------------------------------------------------  
  
  output$ind_29_año <- renderUI({
    selectInput("ind_29_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_29$Año)),
                multiple = T)
  })
  
  output$ind_29_mes<- renderUI({
    selectInput("ind_29_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_29$Mes)),
                multiple = T)
  })
  
  output$ind_29_delito <- renderUI({
    selectInput("ind_29_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_29$Delito)),
                multiple = T)
  })
  
  output$ind_29_municipio <- renderUI({
    selectInput("ind_29_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_29$Municipio)),
                multiple = T)
  })
  
  
  # output$ind_29_carpeta <- renderUI({
  #   selectInput("ind_29_carpeta",
  #               label =  "Selecciona el tipo de carpeta",
  #               choices = sort(unique(indicador_29$Carpeta)),
  #               multiple = T)
  # })
  
  
  ind_29_reactive <- reactive({
    
    indicador_29 %>%
      filter(
        if(!is.null(input$ind_29_año))               Año %in% input$ind_29_año       else Año != "",
        if(!is.null(input$ind_29_mes))               Mes %in% input$ind_29_mes       else Mes != "",
        if(!is.null(input$ind_29_municipio))   Municipio %in% input$ind_29_municipio else Municipio != "",
        if(!is.null(input$ind_29_delito))         Delito %in% input$ind_29_delito    else Delito != "",
        if(!is.null(input$ind_29_carpeta))       Carpeta %in% input$ind_29_carpeta    else Carpeta != ""
        
        
      )
    
  })
  
  
  
  output$gr29 <-renderPlotly ({
    
    ind_29_reactive() %>%
      # indicador_29 %>% 
      filter(Carpeta=="Judicializada") %>% 
      group_by(Año, #Mes, 
               # Periodo 
               , Delito
      ) %>% 
      summarise(`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Registro, na.rm=T)) %>%   
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ", Mes,
                          "\n`Casos denunciados que llegan a la etapa de judicialización` : ",  `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`  ,
                          "\nDelito : ",  Delito, 
                          sep="")) %>%
      ggplot() +
      aes(x = Año, y = `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`, 
          colour = Delito, group=Delito, 
          text=text) +
      geom_line(linewidth = 1.2) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 29",
           color = "Delito") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()+   
      theme(legend.position = "left") +
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr29
    
    
    
    ggplotly(gr29, tooltip = "text") %>% 
      layout(title = list(text = paste0("Indicador 29: "#,ind_29_reactive()$Municipio
      )),
      legend = list(orientation = 'v',  x = 0, y = -8), 
      xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_29 <- renderDataTable ({
    
    ind_29_reactive() %>%
      # indicador_29 %>% 
      mutate(row = row_number()) %>%
      pivot_wider(names_from = "Carpeta",
                  values_from = "Registro") %>% 
      mutate(Iniciada=as.numeric(Iniciada),
             Judicializada=as.numeric(Judicializada),
             Total=Iniciada+Judicializada) %>% 
      group_by(Año) %>%
      summarise(Iniciada=sum(Iniciada, na.rm=T),
                Judicializada=sum(Judicializada, na.rm=T),
                `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Judicializada, na.rm = T),
                `Total de casos por violencia por razón de género denunciados`=sum(Iniciada + Judicializada),
                # `Indicador`=scales::percent(sum((`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`)/
                #                                    `Total de casos por violencia por razón de género denunciados`), 0.1)
      ) %>% 
      select(!c(Iniciada, Judicializada))->tabla_29
    
    
    
    tabla_29 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) %>% 
      formatCurrency('Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Total de casos por violencia por razón de género denunciados',currency = "", interval = 3, mark = ",", digits = 0)
    
    
  })  
  
  
  
  
  # Indicador 30: -----------------------------------------------------------------  
  
  output$ind_30_año <- renderUI({
    selectInput("ind_30_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_30$Año)),
                multiple = T)
  })
  
  output$ind_30_mes<- renderUI({
    selectInput("ind_30_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_30$Mes)),
                multiple = T)
  })
  
  
  output$ind_30_condena <- renderUI({
    selectInput("ind_30_condena",
                label =  "Selecciona el tipo de condena",
                choices = sort(unique(indicador_30$`Tipo de sentencia (absolutoria, condenatoria y en proceso)`)),
                multiple = T)
  })
  
  
  ind_30_reactive <- reactive({
    
    indicador_30 %>%
      filter(
        if(!is.null(input$ind_30_año))               Año %in% input$ind_30_año       else Año != "",
        if(!is.null(input$ind_30_mes))               Mes %in% input$ind_30_mes       else Mes != "",
        if(!is.null(input$ind_30_condena))  `Tipo de sentencia (absolutoria, condenatoria y en proceso)` %in% input$ind_30_condena    else `Tipo de sentencia (absolutoria, condenatoria y en proceso)` != ""
        
      )
    
  })
  
  
  
  output$gr30 <-renderPlotly ({
    
    ind_30_reactive() %>%
      #indicador_30 %>% 
      group_by(Año, Mes, Periodo, `Tipo de sentencia (absolutoria, condenatoria y en proceso)`) %>% 
      summarise(`Total de sentencias`=n(),
                `Indicador`=scales::percent(sum(`Total de sentencias`/n()))) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de sentencias"))%>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = Total, 
          colour = `Tipo de sentencia (absolutoria, condenatoria y en proceso)`, group=`Tipo de sentencia (absolutoria, condenatoria y en proceso)`, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 30",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Total de sentencia` = "#C91682"))+
      # facet_grid(.~ Delito, 
      #           space = 'free_x', scales = 'free_x', switch = 'x',
      #           labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
      scale_color_manual(
        values = c(
          `En proceso` = "#b58cd9",
          `Condenatoria` = "#d98cbc",
          `Absolutoria` = "#7e3794",
          `Juicio` = "#c91682"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr30
    
    
    
    ggplotly(gr30, tooltip = "text") %>% 
      layout(title = list(text = paste0("Indicador 30: ")),
             legend = list(orientation = 'h',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_30 <- renderDataTable ({
    
    # ind_30_reactive() %>%
    indicador_30 %>%
      group_by(Año) %>% 
      summarise(`Total de sentencias`=sum(`Año de sentencia`, na.rm = T),           
                `Total de casos vinculados a procesos`=n(),
                # `Indicador`=scales::percent(sum(`Total de sentencias`/n()), 0.1)
      ) -> tabla_30
    
    
    tabla_30 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) 
    
    
  })  
  
  
  
  
  
  
  
  # Indicador 32: -----------------------------------------------------------------  
  
  output$ind_32_año <- renderUI({
    selectInput("ind_32_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_32$Año)),
                multiple = T)
  })
  
  output$ind_32_mes<- renderUI({
    selectInput("ind_32_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_32$Mes)),
                multiple = T)
  })
  
  
  output$ind_32_municipio <- renderUI({
    selectInput("ind_32_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_32$Municipio)),
                multiple = T)
  })
  
  
  ind_32_reactive <- reactive({
    
    indicador_32 %>%
      filter(
        if(!is.null(input$ind_32_año))               Año %in% input$ind_32_año       else Año != "",
        if(!is.null(input$ind_32_mes))               Mes %in% input$ind_32_mes       else Mes != "",
        if(!is.null(input$ind_32_municipio))   Municipio %in% input$ind_32_municipio else Municipio != ""
      )
    
  })
  
  
  
  output$gr32 <-renderPlotly ({
    
    ind_32_reactive() %>%
      # indicador_32 %>% 
      group_by(Año, Mes, Fecha) %>% 
      summarise(
        `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`, na.rm = T),
        `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
        `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
        `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:7) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Fecha, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 32",
           color = "Clasificación") +
      theme_minimal()+   
      facet_wrap(.~Clasificación, scales = "free_y", ncol=1) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas` = "#b58cd9",
          `Número de cédulas de Alerta Amber emitidas` = "#d98cbc",
          `Número de cédulas de Protocolo Alba emitidas`= "#7e3794",
          `Número de informes de factor de riesgo elaborados` = "#c91682"))+
      theme(legend.position = "none") +
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr32
    
    
    
    ggplotly(gr32, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 32: ",ind_32_reactive()$Municipio)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_32 <- renderDataTable ({
    
    ind_32_reactive() %>%
      #indicador_32 %>% 
      group_by(Año) %>% 
      summarise(
        `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`, na.rm = T),
        `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
        `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
        `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T)) -> tabla_32
    
    
    
    tabla_32 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) 
    
    
  })  
  
  
  
  # Indicador 33: -----------------------------------------------------------------  
  
  output$ind_33_año <- renderUI({
    selectInput("ind_33_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_33$Año)),
                multiple = T)
  })
  
  output$ind_33_mes<- renderUI({
    selectInput("ind_33_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_33$Mes)),
                multiple = T)
  })
  
  
  output$ind_33_edad <- renderUI({
    selectInput("ind_33_municipio",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_33$Edad)),
                multiple = T)
  })
  
  
  ind_33_reactive <- reactive({
    
    indicador_33 %>%
      filter(
        if(!is.null(input$ind_33_año))     Año %in% input$ind_33_año    else Año != "",
        if(!is.null(input$ind_33_mes))     Mes %in% input$ind_33_mes    else Mes != "",
        if(!is.null(input$ind_33_edad))   Edad %in% input$ind_33_edad   else Edad != ""
      )
    
  })
  
  
  
  output$gr33 <-renderPlotly ({
    
    ind_33_reactive() %>%
      #indicador_33 %>% 
      group_by(Año,Mes, Fecha, Edad) %>% 
      summarise(`Total de cédulas únicas de difusión emitidas`=sum(`Total de cédulas únicas de difusión emitidas`),
                `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`=sum(`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=5:6) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>%
      filter(!Total==0) %>% 
      
      ggplot() +
      aes(x = Fecha, y = Edad,
          size =Total,
          colour = Clasificación, group=Clasificación, text=text) +
      geom_point()+ 
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite")+# ggplot() +
      scale_size_continuous(range = c(5,10))+
      facet_grid(.~ Clasificación, 
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 70, multi_line = TRUE)
      ) + 
      labs(x="", y="", title = "Indicador 33",
           color = "", fill="") +
      theme_minimal()+   
      #scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Total de cédulas únicas de difusión emitidas` = "#C91682",
                   `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración` = "#7E3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr33
    
    
    
    ggplotly(gr33, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 33: ",ind_33_reactive()$Edad)),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_33 <- renderDataTable ({
    
    ind_33_reactive() %>%
      #indicador_33 %>% 
      group_by(Año) %>% 
      summarise(`Total de cédulas únicas de difusión emitidas`=sum(`Total de cédulas únicas de difusión emitidas`),
                `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`=sum(`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`),
                # `Indicador`=scales::percent(sum((`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`)/`Total de cédulas únicas de difusión emitidas`), 0.1)
      ) -> tabla_33
    
    
    
    tabla_33 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")))) 
    
    
  })  
  
  
  
  
  # Indicador 34: -----------------------------------------------------------------  
  
  output$ind_34_año <- renderUI({
    selectInput("ind_34_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_34$Año)),
                multiple = T)
  })
  
  output$ind_34_mes<- renderUI({
    selectInput("ind_34_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_34$Mes)),
                multiple = T)
  })
  
  
  output$ind_34_edad <- renderUI({
    selectInput("ind_34_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_34$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_34_reactive <- reactive({
    
    indicador_34 %>%
      filter(
        if(!is.null(input$ind_34_año))     Año %in% input$ind_34_año               else Año != "",
        if(!is.null(input$ind_34_mes))     Mes %in% input$ind_34_mes               else Mes != "",
        if(!is.null(input$ind_34_edad))   `Rango de edad` %in% input$ind_34_edad   else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr34 <-renderPlotly ({
    
    ind_34_reactive() %>%
      #indicador_34 %>% 
      group_by(Año, Trimestre, `Rango de edad`) %>% 
      summarise(`Número de casos de localización`=sum(`Número de casos de localización`),
                `Número de casos de búsqueda`=sum(`Número de casos de búsqueda`),
                `Número de casos de aplicación a cabalidad del protocolo alba`=sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`),
                `Indicador`=scales::percent(sum((`Número de casos de localización`)/`Número de casos de aplicación a cabalidad del Protocolo Alba`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:6) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  Trimestre, "%B de %Y"),
             "\nTotal: ", scales::comma(Total), sep="") %>% 
      filter(!Total==0) %>% 
      # ggplot() +
      #  aes(x = Periodo, y = `Rango de edad`, group=Clasificación, #size=Total, 
      #      colour = Clasificación, group=Clasificación, text=text) +
      #  geom_line(size = 1.5) + 
      #  geom_point()+
      #  geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
      #                 size=3, color="ghostwhite")+
      
      ggplot() +
      aes(x =Trimestre , y = `Rango de edad`, text=text,
          fill = Total) +
      #geom_raster()+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=2, color="ghostwhite", angle=90)+
      labs(x="", y="", title = "Indicador 34",
           color = "Clasificación") +
      facet_grid(.~ Clasificación,
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 40, multi_line = TRUE)
      ) +
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      
      #scale_x_discrete(limits = rev,labels = function(x) str_wrap(x, width = 3)) + 
      # scale_fill_manual(
      #   values = c(
      #     `Número de casos de localización` = "#d98cbc",
      #     `Número de casos de búsqueda` = "#c91682",
      #     `Número de casos de aplicación a cabalidad del protocolo alba` = "#7e3794"))+
      theme_minimal()+   
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr34
    
    
    
    ggplotly(gr34, tooltip = "text") %>% 
      layout(title = "Indicador 34",
             legend = list(orientation = 'h', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom")) %>% 
      layout(margin = list(b=-5,t=100), 
             annotations =
               list(
                 x = .7, y = -3,  
                 text = "",
                 #                 text = "Datos de cerodesabasto.org",
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=6,
                 font=list(size=10, color="#9443FF")))
    
    
  })
  
  output$t_34 <- renderDataTable ({
    
    ind_34_reactive() %>%
      #indicador_33 %>% 
      group_by(Año) %>% 
      summarise(`Número de casos de localización`=sum(`Número de casos de localización`),
                `Número de casos de búsqueda`=sum(`Número de casos de búsqueda`),
                `Número de casos de aplicación a cabalidad del protocolo alba`=sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`),
                # `Indicador`=scales::percent(sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`/(`Número de casos de localización`+ `Número de casos de búsqueda`)), 0.1)
      ) -> tabla_34
    
    tabla_34 %>% datatable(filter="top", extensions = 'Buttons',
                           options = list(dom = 'Blfrtip',
                                          buttons = c('copy', 'excel', 'print'),
                                          lengthMenu = list(c(6,10,20, -1),
                                                            c(6,10,20,"Todo")))) 
    
  })  
  
  
  # Indicador 35: -----------------------------------------------------------------  
  
  output$ind_35_año <- renderUI({
    selectInput("ind_35_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_35$Año)),
                multiple = T)
  })
  
  output$ind_35_mes<- renderUI({
    selectInput("ind_35_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_35$Mes)),
                multiple = T)
  })
  
  
  ind_35_reactive <- reactive({
    
    indicador_35 %>%
      filter(
        if(!is.null(input$ind_35_año))     Año %in% input$ind_35_año    else Año != "",
        if(!is.null(input$ind_35_mes))     Mes %in% input$ind_35_mes    else Mes != ""
      )
    
  })
  
  
  
  output$gr35 <-renderPlotly ({
    
    ind_35_reactive() %>%
      #indicador_35 %>% 
      group_by(Año, Mes, Periodo) %>% 
      summarise(`Cédulas de difusión activas`=sum(`Cédulas de difusión activas`),
                `Total de cédulas de difusión emitidas`=sum(`Total de cédulas de difusión emitidas`),
                # `Indicador`=scales::percent(sum((`Cédulas de difusión activas`)/`Total de cédulas de difusión emitidas`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Cédulas de difusión activas",
                          "Total de cédulas de difusión emitidas")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo , y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 35",
           color = "Clasificación") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Cédulas de difusión activas` = "#C91682",
                   `Total de cédulas de difusión emitidas` = "#7E3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr35
    
    
    
    ggplotly(gr35, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 35: ")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_35 <- renderDataTable ({
    
    ind_35_reactive() %>%
      #indicador_35 %>% 
      group_by(Año) %>% 
      summarise(`Cédulas de difusión activas`=sum(`Cédulas de difusión activas`),
                `Total de cédulas de difusión emitidas`=sum(`Total de cédulas de difusión emitidas`),
                # `Indicador`=scales::percent(sum((`Cédulas de difusión activas`)/`Total de cédulas de difusión emitidas`), 0.1)
      ) -> tabla_35
    
    
    tabla_35 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
  })  
  
  
  
  # Indicador 36: -----------------------------------------------------------------  
  
  output$ind_36_año <- renderUI({
    selectInput("ind_36_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_36$Año)),
                multiple = T)
  })
  
  output$ind_36_mes<- renderUI({
    selectInput("ind_36_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_36$Mes)),
                multiple = T)
  })
  
  
  output$ind_36_edad <- renderUI({
    selectInput("ind_36_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_36$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_36_reactive <- reactive({
    
    indicador_36 %>%
      filter(
        if(!is.null(input$ind_36_año))     Año %in% input$ind_36_año               else Año != "",
        if(!is.null(input$ind_36_mes))     Mes %in% input$ind_36_mes               else Mes != "",
        if(!is.null(input$ind_36_edad))   `Rango de edad` %in% input$ind_36_edad   else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr36 <-renderPlotly ({
    
    ind_36_reactive() %>%
      #indicador_36 %>% 
      group_by(Trimestre, `Rango de edad`) %>% 
      summarise(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`=sum(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`),
                `Total de casos de desaparición de niñas y adolescentes`=sum(`Total de casos de desaparición de niñas y adolescentes`),
                # `Indicador`=scales::percent(sum((`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`)/`Total de casos de desaparición de niñas y adolescentes`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=3:4) %>% 
      mutate(text = paste("\nPeriodo: ",  Trimestre,
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      filter(!Total==0) %>% 
      ggplot() +
      aes(x =Trimestre , y = `Rango de edad`, text=text,
          fill = Total) +
      #geom_raster()+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=90)+
      labs(x="", y="", title = "Indicador 36",
           color = "Clasificación") +
      facet_grid(.~ Clasificación,
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 70, multi_line = TRUE)
      ) +
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      
      #scale_y_continuous(labels = scales::comma) +
      # scale_fill_manual(
      #   values = c(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber` = "#C91682",
      #              `Total de casos de desaparición de niñas y adolescentes` = "#7E3794"))+
      theme_minimal()+   
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr36
    
    
    
    ggplotly(gr36, tooltip = "text") %>% 
      # xaxis = list(side = "bottom"),legend = list(side="bottom"))
      layout(title = "Indicador 36",
             legend = list(orientation = 'h', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom")) %>% 
      layout(margin = list(b=-5,t=120), 
             annotations =
               list(
                 x = .7, y = -4,  
                 text = "",
                 #                 text = "Datos de cerodesabasto.org",
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=6,
                 font=list(size=10, color="#9443FF")))
    
    
  })
  
  output$t_36 <- renderDataTable ({
    
    ind_36_reactive() %>%
      #indicador_33 %>% 
      group_by(Año) %>% 
      summarise(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`=sum(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`),
                `Total de casos de desaparición de niñas y adolescentes`=sum(`Total de casos de desaparición de niñas y adolescentes`),
                # `Indicador`=scales::percent(sum((`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`)/`Total de casos de desaparición de niñas y adolescentes`), 0.1)
      ) -> tabla_36 
    
    tabla_36 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
  })  
  
  
  
  # Indicador 37: -----------------------------------------------------------------  
  
  output$ind_37_año <- renderUI({
    selectInput("ind_37_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_37$Año)),
                multiple = T)
  })
  
  output$ind_37_mes<- renderUI({
    selectInput("ind_37_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_37$Mes)),
                multiple = T)
  })
  
  output$ind_37_personal<- renderUI({
    selectInput("ind_37_personal",
                label =  "Seleccione la función del personal",
                choices = sort(unique(indicador_37$Personal)),
                multiple = T)
  })
  
  
  ind_37_reactive <- reactive({
    
    indicador_37 %>%
      filter(
        if(!is.null(input$ind_35_año))             Año %in% input$ind_35_año         else Año != "",
        if(!is.null(input$ind_35_mes))             Mes %in% input$ind_35_mes         else Mes != "",
        if(!is.null(input$ind_37_personal))   Personal %in% input$ind_37_personal    else Personal != ""
        
      )
    
  })
  
  
  
  output$t_37 <- renderDataTable ({
    
    ind_37_reactive() %>%
      # indicador_37 %>% 
      group_by(Año, Personal) %>% 
      summarise(`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`=sum(`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`),
                `Total personal de la fiscalía`=sum(`Total personal de la fiscalía`),
                # `Indicador`=scales::percent(sum((`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`)/`Total personal de la fiscalía`), 0.1)
      ) -> tabla_37 
    tabla_37 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
  })  
  
  # Indicador 38: -----------------------------------------------------------------  
  
  output$ind_38_año <- renderUI({
    selectInput("ind_38_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_38$Año)),
                multiple = T)
  })
  
  output$ind_38_mes<- renderUI({
    selectInput("ind_38_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_38$Mes)),
                multiple = T)
  })
  
  
  output$ind_38_edad <- renderUI({
    selectInput("ind_38_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_38$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_38_reactive <- reactive({
    
    indicador_38 %>%
      filter(
        if(!is.null(input$ind_38_año))     Año %in% input$ind_38_año               else Año != "",
        if(!is.null(input$ind_38_mes))     Mes %in% input$ind_38_mes               else Mes != "",
        if(!is.null(input$ind_38_edad))   `Rango de edad` %in% input$ind_38_edad   else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr38 <-renderPlotly ({
    
    ind_38_reactive() %>%
      #indicador_38 %>% 
      group_by(Trimestre, `Rango de edad`) %>% 
      summarise(`Casos de desaparición denunciados`=sum(`Casos de desaparición denunciados`),
                `Total de casos resueltos en etapa de investigación`=sum(`Total de casos resueltos en etapa de investigación`),
                `Indicador`=scales::percent(sum((`Total de casos resueltos en etapa de investigación`)/`Casos de desaparición denunciados`), 0.1))%>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Casos de desaparición denunciados",
                          "Total de casos resueltos en etapa de investigación")) %>%  
      mutate(text = paste("Periodo: ",  Trimestre,
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      filter(!Total==0) %>% 
      ggplot() +
      aes(x = Trimestre, y = `Rango de edad`, size=Total,
          fill = Total, group=Clasificación, text=text) +
      # geom_line(size = 1.5) + 
      # geom_point(size = 2)+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=90)+
      labs(x="", y="", title = "Indicador 38", fill = "Clasificación") +
      facet_grid(.~ Clasificación,
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 70, multi_line = TRUE))+
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      
      theme_minimal()+   
      # scale_y_continuous(labels = scales::comma) +
      # scale_fill_manual(
      #   values = c(`Casos de desaparición denunciados` = "#c91682",
      #              `Total de casos resueltos en etapa de investigación` = "#7e3794"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr38
    
    
    ggplotly(gr38,tooltip = "text")%>% 
      # layout(title = "Indicador 38",
      # legend = list(orientation = 'v', 
      #               x = 0, y = -.3), 
      # xaxis = list(side = "bottom"),legend = list(side="bottom"))
      layout(title = "Indicador 38",
             legend = list(orientation = 'v', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom")) %>% 
      layout(margin = list(b=-5,t=120), 
             annotations =
               list(
                 x = .7, y = -3,  
                 text = "",
                 #                 text = "Datos de cerodesabasto.org",
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=6,
                 font=list(size=10, color="#9443FF")))
    
    
    
  })
  
  output$t_38 <- renderDataTable ({
    
    ind_38_reactive() %>%
      #indicador_38 %>% 
      group_by(Año) %>% 
      summarise(`Casos de desaparición denunciados`=sum(`Casos de desaparición denunciados`),
                `Total de casos resueltos en etapa de investigación`=sum(`Total de casos resueltos en etapa de investigación`),
                # `Indicador`=scales::percent(sum((`Total de casos resueltos en etapa de investigación`)/`Casos de desaparición denunciados`), 0.1)
      ) -> tabla_38
    
    tabla_38 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
  })  
  
  
  
  
  # Indicador 39: -----------------------------------------------------------------  
  
  output$ind_39_año <- renderUI({
    selectInput("ind_39_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_39$Año)),
                multiple = T)
  })
  
  output$ind_39_mes<- renderUI({
    selectInput("ind_39_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_39$Mes)),
                multiple = T)
  })
  
  
  output$ind_39_edad <- renderUI({
    selectInput("ind_39_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_39$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_39_reactive <- reactive({
    
    indicador_39 %>%
      filter(
        if(!is.null(input$ind_39_año))     Año %in% input$ind_39_año               else Año != "",
        if(!is.null(input$ind_39_mes))     Mes %in% input$ind_39_mes               else Mes != "",
        if(!is.null(input$ind_39_edad))   `Rango de edad` %in% input$ind_39_edad   else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr39 <-renderPlotly ({
    
    ind_39_reactive() %>%
      #indicador_39 %>% 
      group_by(Trimestre, `Rango de edad`) %>% 
      summarise(`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`=sum(`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`),
                `Total de casos de desaparición denunciados`=sum(`Total de casos de desaparición denunciados`),
                `Indicador`=scales::percent(sum((`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`)/`Total de casos de desaparición denunciados`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Casos de desaparición que reciben seguimiento por parte de Protocolo Alba",
                          "Total de casos de desaparición denunciados")) %>% 
      mutate(text = paste("Trimestre: ", Trimestre,
                          "\ntotal: ", scales::comma(Total), sep="")) %>% 
      filter(!Total==0) %>% 
      ggplot() +
      
      aes(x = Trimestre, y = `Rango de edad`, size=Total,
          fill = Total, group=Clasificación, text=text) +
      # geom_line(size = 1.5) + 
      # geom_point(size = 2)+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=90)+
      labs(x="", y="", title = "Indicador 39", fill = "Clasificación") +
      facet_grid(.~ Clasificación,
                 space = 'free_x', scales = 'free_x', switch = 'x',
                 labeller = label_wrap_gen(width = 50, multi_line = TRUE))+
      # scale_fill_manual(
      #   values = c(`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba` = "#C91682",
      #              `Total de casos de desaparición denunciados` = "#7E3794")) +
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      
      theme_minimal()+   
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr39
    
    
    ggplotly(gr39, tooltip = "text")%>% 
      # layout(title = "Indicador 39",
      # legend = list(orientation = 'v', 
      #               x = 0, y = -.4), 
      # xaxis = list(side = "bottom"),legend = list(side="bottom"))
      layout(title = "Indicador 39",
             legend = list(orientation = 'h', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom")) %>% 
      layout(margin = list(b=-5,t=120), 
             annotations =
               list(
                 x = .7, y = -3,  
                 text = "",
                 #                 text = "Datos de cerodesabasto.org",
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=6,
                 font=list(size=10, color="#9443FF")))
    
    
    
    
  })
  
  output$t_39 <- renderDataTable ({
    
    ind_39_reactive() %>%
      #indicador_39 %>% 
      group_by(Año) %>% 
      summarise(`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`=sum(`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`),
                `Total de casos de desaparición denunciados`=sum(`Total de casos de desaparición denunciados`),
                # `Indicador`=scales::percent(sum((`Casos de desaparición que reciben seguimiento por parte de Protocolo Alba`)/`Total de casos de desaparición denunciados`), 0.1)
      ) -> tabla_39
    
    tabla_39 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
    
  })  
  
  
  
  # Indicador 40: -----------------------------------------------------------------  
  
  output$ind_40_año <- renderUI({
    selectInput("ind_40_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_40$Año)),
                multiple = T)
  })
  
  output$ind_40_mes<- renderUI({
    selectInput("ind_40_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_40$Mes)),
                multiple = T)
  })
  
  
  
  
  ind_40_reactive <- reactive({
    
    indicador_40 %>%
      filter(
        if(!is.null(input$ind_40_año))     Año %in% input$ind_40_año               else Año != "",
        if(!is.null(input$ind_40_mes))     Mes %in% input$ind_40_mes               else Mes != ""
      )
    
  })
  
  
  
  output$gr40 <-renderPlotly ({
    
    ind_40_reactive() %>%
      #indicador_40 %>% 
      group_by(Año, Mes, Periodo) %>% 
      summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                `Homicidio doloso`=sum(`Homicidio doloso`),
                `Accidente`=sum(`Accidente`),
                `Feminicidio`=sum(`Feminicidio`),
                `Suicidio`=sum(`Suicidio`),
                `El que resulte`=sum(`El que resulte`),
                `Indicador`=sum(`Homicidio culposo`+ `Homicidio doloso`+
                                  `Accidente`+ `Feminicidio`+
                                  `Suicidio`+ `El que resulte`)) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Homicidio culposo",
                          "Homicidio doloso",
                          "Accidente",
                          "Feminicidio",
                          "Suicidio",
                          "El que resulte")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  format(as_date(Periodo), "%b de %y"),
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = Total, 
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", title = "Indicador 40",
           color = "Clasificación") +
      facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma, limits = c(0,30)) +
      scale_color_manual(
        values = c(`Homicidio culposo` = "#c91682",
                   `Homicidio doloso` = "#7e3794",
                   `Accidente` = "#597dff",
                   `Feminicidio`= "#b58cd9",
                   `Suicidio`= "#a544ff",
                   `El que resulte`= "#d98cbc")) +
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr40
    
    
    ggplotly(gr40,tooltip = "text")%>% 
      layout(title = "Indicador 40",
             legend = list(orientation = 'h', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
    
    
  })
  
  output$t_40 <- renderDataTable ({
    
    ind_40_reactive() %>%
      #indicador_40 %>% 
      group_by(Año) %>% 
      summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                `Homicidio doloso`=sum(`Homicidio doloso`),
                `Accidente`=sum(`Accidente`),
                `Feminicidio`=sum(`Feminicidio`),
                `Suicidio`=sum(`Suicidio`),
                `El que resulte`=sum(`El que resulte`),
                Total=sum(`Homicidio culposo`+ `Homicidio doloso`+
                            `Accidente`+ `Feminicidio`+
                            `Suicidio`+ `El que resulte`)) -> tabla_40# %>% 
    # mutate(`Indicador` = scales::percent((Total - lag(Total))/lag(Total),0.1))-> tabla_40
    
    
    
    tabla_40 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
    
    
  })  
  
  # output$t_40_1 <- renderDataTable ({
  #   
  #   ind_40_reactive() %>%
  #     #indicador_40 %>% 
  #     group_by(Año) %>% 
  #     summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
  #               `Homicidio doloso`=sum(`Homicidio doloso`),
  #               `Accidente`=sum(`Accidente`),
  #               `Feminicidio`=sum(`Feminicidio`),
  #               `Suicidio`=sum(`Suicidio`),
  #               `El que resulte`=sum(`El que resulte`),
  #               Total=sum(`Homicidio culposo`+ `Homicidio doloso`+
  #                           `Accidente`+ `Feminicidio`+
  #                           `Suicidio`+ `El que resulte`)) -> tabla_40 #%>% 
  #     # mutate(`Indicador` = scales::percent((Total - lag(Total))/lag(Total),0.1))-> tabla_40
  #   
  #   
  #   
  #   tabla_40 %>%  datatable(filter="top", extensions = 'Buttons',
  #                           options = list(dom = 'Blfrtip',
  #                                          buttons = c('copy', 'excel', 'print'),
  #                                          lengthMenu = list(c(6,10,20, -1),
  #                                                            c(6,10,20,"Todo"))))
  #   
  #   
  # })  
  
}

shinyApp(ui, server)



# library(shiny)
# # library(dplyr)
# library(data.table)
# 
# DT <- copy(mtcars)
# setDT(DT)
# 
# ui <- fluidPage(
#   titlePanel("mtcars"),
#   mainPanel(
#     tableOutput("table")
#   )
# )
# 
# server <- function(input, output) {
#   
#   output$table <- renderTable({
#     DT[, inputId := paste0("gear_input_", seq_len(.N))][, gear_links := as.character(actionLink(inputId = inputId, label = inputId, onclick = sprintf("Shiny.setInputValue(id = 'gear_click', value = %s);", gear))), by = inputId][, inputId := NULL]
#   }, sanitize.text.function = function(x){x})
#   
#   observeEvent(input$gear_click, {
#     showModal(modalDialog(
#       title = "Gear filter",
#       tableOutput("filtered_table"),
#       size = "xl"
#     ))
#   })
#   
#   output$filtered_table <- renderTable({
#     req(input$gear_click)
#     DT[gear == input$gear_click][, c("gear_links", "vs") := NULL]
#   })
#   
# }
# 
# shinyApp(ui = ui, server = server)