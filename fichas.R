#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(janitor)
library(scales)

# Define UI for application that draws a histogram

datos <- readxl::read_excel("vinculaciones.xlsx") %>% 
  rename(carpetas=incidencia)

#####
#ui
ui <- fluidPage(
  useShinyjs(),
  useShinydashboard(),
  title = "FGJCDMX - Encuesta de satisfaccion", lang = "es", 
  navbarPage(
    "Fichas generales de delitos", id="menu", collapsible = FALSE
  ),
  sidebarLayout(
    sidebarPanel(align="left", width = 2,
                 fluidRow(
                   uiOutput("tipo")
                 ),
                 fluidRow(
                   uiOutput("delitos")
                 ),
    ),
    mainPanel(width = 10, id= "main", style='padding-right:50px;',
              fluidRow(
                column(width = 4, infoBoxOutput("incidencia", width="100%")), 
                column(width = 4, 
                       infoBoxOutput("flagrancias", width="100%")),
                column(width = 4, 
                       infoBoxOutput("ordenes", width="100%"))
                
              ),
              fluidRow(
                column(width = 4, infoBoxOutput("vinculaciones", width="100%")), 
                column(width = 4, infoBoxOutput("sentencias", width="100%"))
              ),
              fluidRow(style='padding-left:20px;',
                       uiOutput("tabla")
              )
    )
  )
)


#####
#server
server <- function(input, output) {

  output$tipo <- renderUI({
    radioButtons(inputId = "tipos", label = "Tipo de carpetas", 
                 choices = unique(datos$tipo), 
                 selected = "alto impacto")})
  
  output$delitos <- renderUI({
    selectInput(inputId = "crimes", label = "Delitos", multiple = F,
                #choices= unique(datos$delito)[datos$tipo==input$tipos],
                choices= sort(unique(datos$delito[datos$tipo %in% input$tipos])), 
                selected = "Alto impacto (ADIP)"
    )
  })
  
  delay(250,   base <- reactive({
    
    datos %>% filter(tipo %in% input$tipos, 
                     delito %in% input$crimes) %>% 
      gather(indicador, Total, carpetas:sentencias) %>% 
      spread(año, Total, fill=0) %>% 
      mutate(indicador=factor(indicador, levels = c("carpetas", "flagrancias", 
                                                    "ordenes", "vinculaciones", "sentencias"))) %>% 
      arrange(indicador)
    
    
  }))
  
  delay(500,   output$tabla <- function(){
    
    req(input$tipos)
    req(input$crimes)
    
    if(input$tipos=="alto impacto"){
      
      base() %>% select(-c(delito,tipo)) %>% 
        adorn_totals(where = "col") %>% 
        mutate_at(.vars = vars(matches(c("20", "total"))), comma) %>% 
        kable(booktabs = T, linesep = "", format = "html", escape = F) %>%
        kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed",
                                                            "bordered", "responsive"), 
                      font_size = 17) %>% 
        row_spec(row=0, bold = T, align = "center",
                 background="#fe7f2d", color = "ghostwhite") %>%
        # row_spec(row=6, bold = T, align = "center",
        #          background="#233d4d", color = "ghostwhite") %>%
        column_spec(1:5, width = "8em") %>% 
        #column_spec(1, width = "14em") %>% 
        column_spec(length(names(base()))-1, bold = T)
    } else if(input$tipos=="bajo impacto"){
      base() %>% select(-c(delito,tipo)) %>% 
        adorn_totals(where = "col") %>% 
        mutate_at(.vars = vars(matches(c("20", "total"))), comma) %>% 
        kable(booktabs = T, linesep = "", format = "html", escape = F) %>%
        kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed",
                                                            "bordered", "responsive"), 
                      font_size = 17) %>% 
        row_spec(row=0, bold = T, align = "center",
                 background="#233d4d", color = "ghostwhite") %>%
        # row_spec(row=6, bold = T, align = "center",
        #          background="#233d4d", color = "ghostwhite") %>%
        column_spec(1:5, width = "8em") %>% 
        #column_spec(1, width = "14em") %>% 
        column_spec(length(names(base()))-1, bold = T)
      
    }  else {
      base() %>% select(-c(delito,tipo)) %>% 
        adorn_totals(where = "col") %>% 
        mutate_at(.vars = vars(matches(c("20", "total"))), comma) %>% 
        kable(booktabs = T, linesep = "", format = "html", escape = F) %>%
        kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed",
                                                            "bordered", "responsive"), 
                      font_size = 17) %>% 
        row_spec(row=0, bold = T, align = "center",
                 background="purple", color = "ghostwhite") %>%
        # row_spec(row=6, bold = T, align = "center",
        #          background="#233d4d", color = "ghostwhite") %>%
        column_spec(1:5, width = "8em") %>% 
        #column_spec(1, width = "14em") %>% 
        column_spec(length(names(base()))-1, bold = T)
    }
  }
  )
  
  
  delay(1000, output$incidencia <- renderInfoBox({
    nombres <- base() %>% filter(indicador=="carpetas") %>%
      names() 
    
    existe <- grepl("2019", nombres)
    
    #isTruthy(existe)
    if(!isTruthy(existe)){
      inicio <- base() %>%
        filter(indicador=="carpetas")
      
      
      n <- as.numeric(inicio[1,length(names(inicio))]/inicio[1,4]-1)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      
      n <- percent(n, .01)
      
      
    } else {
      n <- base() %>% filter(indicador=="carpetas") %>%
        mutate(porcentaje=`2022`/`2019`-1) %>%
        pull(porcentaje)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
    }
    
    if(is.na(n) | n=="Inf"){
      infoBox(title = HTML("Incidencia %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           "Sin dato","</p>"),
              color = "light-blue",
              fill = TRUE
      )
    } else {
      infoBox(title = HTML("Incidencia %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           n,"</p>"),
              color = ifelse(n>=0, "green", "red"),
              fill = TRUE
      )
      
    }
    
    
    
  })
  )
  
  
  
  
  delay(1000,
        
        
        output$flagrancias <- renderInfoBox({
          nombres <- base() %>% filter(indicador=="flagrancias") %>%
            names()
          existe <- grepl("2019", nombres)
          
          if(!isTruthy(existe)){
            inicio <- base() %>%
              filter(indicador=="flagrancias")
            
            
            n <- as.numeric(inicio[1,length(names(inicio))]/inicio[1,4]-1)
            
            # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
            n <- percent(n, .01)
            
            
          } else {
            n <- base() %>% filter(indicador=="flagrancias") %>%
              mutate(porcentaje=`2022`/`2019`-1) %>%
              pull(porcentaje)
            
            # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
            n <- percent(n, .01)
          }
          
          if(is.na(n) | n=="Inf"){
            infoBox(title = HTML("Flagrancias %<br>"),
                    value = HTML("<p style='font-size:30px'>",
                                 "Sin dato","</p>"),
                    color = "light-blue",
                    fill = TRUE
            )
          } else {
            infoBox(title = HTML("Flagrancias %<br>"),
                    value = HTML("<p style='font-size:30px'>",
                                 n,"</p>"),
                    color = ifelse(n>=0, "green", "red"),
                    fill = TRUE
            )
            
          }
        })
  )
  
  delay(1000, output$ordenes <- renderInfoBox({
    nombres <- base() %>% filter(indicador=="ordenes") %>%
      names()
    existe <- grepl("2019", nombres)
    
    if(!isTruthy(existe)){
      inicio <- base() %>%
        filter(indicador=="ordenes")
      
      
      n <- as.numeric(inicio[1,length(names(inicio))]/inicio[1,4]-1)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
      
      
    } else {
      n <- base() %>% filter(indicador=="ordenes") %>%
        mutate(porcentaje=`2022`/`2019`-1) %>%
        pull(porcentaje)
      
      #paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      
      n <- percent(n, .01)
      
    }
    
    
    
    if(is.na(n) | n=="Inf"){
      infoBox(title = HTML("OAC %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           "Sin dato","</p>"),
              color = "light-blue",
              fill = TRUE
      )
    } else {
      infoBox(title = HTML("OAC %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           n,"</p>"),
              color = ifelse(n>=0, "green", "red"),
              fill = TRUE
      )
      
    }
    
  })
  )
  
  delay(1000, output$vinculaciones <- renderInfoBox({
    nombres <- base() %>% filter(indicador=="vinculaciones") %>%
      names()
    existe <- grepl("2019", nombres)
    
    if(!isTruthy(existe)){
      inicio <- base() %>%
        filter(indicador=="vinculaciones")
      
      
      n <- as.numeric(inicio[1,length(names(inicio))]/inicio[1,4]-1)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
      
      
    } else {
      n <- base() %>% filter(indicador=="vinculaciones") %>%
        mutate(porcentaje=`2022`/`2019`-1) %>%
        pull(porcentaje)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
    }
    
    if(is.na(n)| n=="Inf"){
      infoBox(title = HTML("Vinculaciones %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           "Sin dato","</p>"),
              color = "light-blue",
              fill = TRUE
      )
    } else {
      infoBox(title = HTML("Vinculaciones %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           n,"</p>"),
              color = ifelse(n>=0, "green", "red"),
              fill = TRUE
      )
      
    }
    
  }))
  
  delay(1000,  output$sentencias <- renderInfoBox({
    nombres <- base() %>% filter(indicador=="sentencias") %>%
      names()
    existe <- grepl("2019", nombres)
    
    if(!isTruthy(existe)){
      inicio <- base() %>%
        filter(indicador=="sentencias")
      
      
      n <- as.numeric(inicio[1,length(names(inicio))]/inicio[1,4]-1)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
      
      
    } else {
      n <- base() %>% filter(indicador=="sentencias") %>%
        mutate(porcentaje=`2022`/`2019`-1) %>%
        pull(porcentaje)
      
      # paste0(ifelse(n>0, "Aumento de ", "Disminución de "), percent(n, .01))
      n <- percent(n, .01)
    }
    
    if(is.na(n) | n=="Inf"){
      infoBox(title = HTML("Sentencias %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           "Sin dato","</p>"),
              color = "light-blue",
              fill = TRUE
      )
    } else {
      infoBox(title = HTML("Sentencias %<br>"),
              value = HTML("<p style='font-size:30px'>",
                           n,"</p>"),
              color = ifelse(n>=0, "green", "red"),
              fill = TRUE
      )
      
    }
    
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
